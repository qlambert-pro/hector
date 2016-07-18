int card_share_mode(struct rtsx_chip *chip, int card)
{
	int retval;
	u8 mask, value;

	if (CHECK_PID(chip, 0x5208)) {
		mask = CARD_SHARE_MASK;
		if (card == SD_CARD)
			value = CARD_SHARE_48_SD;
		else {
			rtsx_trace(chip);
			return STATUS_FAIL;
		}

	} else if (CHECK_PID(chip, 0x5288)) {
		mask = 0x03;
		if (card == SD_CARD)
			value = CARD_SHARE_BAROSSA_SD;
		else {
			rtsx_trace(chip);
			return STATUS_FAIL;
		}

	} else {
		rtsx_trace(chip);
		return STATUS_FAIL;
	}

	retval = rtsx_write_register(chip, CARD_SHARE_MODE, mask, value);
	if (retval) {
		rtsx_trace(chip);
		return retval;
	}

	return STATUS_SUCCESS;
}

static int dgnc_tty_write(struct tty_struct *tty,
			  const unsigned char *buf, int count)
{
	struct channel_t *ch = NULL;
	struct un_t *un = NULL;
	int bufcount = 0, n = 0;
	unsigned long flags;
	ushort head;
	ushort tail;
	ushort tmask;
	uint remain;

	if (!tty || !dgnc_TmpWriteBuf)
		return 0;

	un = tty->driver_data;
	if (!un || un->magic != DGNC_UNIT_MAGIC)
		return 0;

	ch = un->un_ch;
	if (!ch || ch->magic != DGNC_CHANNEL_MAGIC)
		return 0;

	if (!count)
		return 0;

	/*
	 * Store original amount of characters passed in.
	 * This helps to figure out if we should ask the FEP
	 * to send us an event when it has more space available.
	 */

	spin_lock_irqsave(&ch->ch_lock, flags);

	/* Get our space available for the channel from the board */
	tmask = WQUEUEMASK;
	head = (ch->ch_w_head) & tmask;
	tail = (ch->ch_w_tail) & tmask;

	bufcount = tail - head - 1;
	if (bufcount < 0)
		bufcount += WQUEUESIZE;

	/*
	 * Limit printer output to maxcps overall, with bursts allowed
	 * up to bufsize characters.
	 */
	bufcount = dgnc_maxcps_room(tty, bufcount);

	/*
	 * Take minimum of what the user wants to send, and the
	 * space available in the FEP buffer.
	 */
	count = min(count, bufcount);

	/*
	 * Bail if no space left.
	 */
	if (count <= 0)
		goto exit_retry;

	/*
	 * Output the printer ON string, if we are in terminal mode, but
	 * need to be in printer mode.
	 */
	if ((un->un_type == DGNC_PRINT) && !(ch->ch_flags & CH_PRON)) {
		dgnc_wmove(ch, ch->ch_digi.digi_onstr,
			   (int)ch->ch_digi.digi_onlen);
		head = (ch->ch_w_head) & tmask;
		ch->ch_flags |= CH_PRON;
	}

	/*
	 * On the other hand, output the printer OFF string, if we are
	 * currently in printer mode, but need to output to the terminal.
	 */
	if ((un->un_type != DGNC_PRINT) && (ch->ch_flags & CH_PRON)) {
		dgnc_wmove(ch, ch->ch_digi.digi_offstr,
			   (int)ch->ch_digi.digi_offlen);
		head = (ch->ch_w_head) & tmask;
		ch->ch_flags &= ~CH_PRON;
	}

	n = count;

	/*
	 * If the write wraps over the top of the circular buffer,
	 * move the portion up to the wrap point, and reset the
	 * pointers to the bottom.
	 */
	remain = WQUEUESIZE - head;

	if (n >= remain) {
		n -= remain;
		memcpy(ch->ch_wqueue + head, buf, remain);
		head = 0;
		buf += remain;
	}

	if (n > 0) {
		/*
		 * Move rest of data.
		 */
		remain = n;
		memcpy(ch->ch_wqueue + head, buf, remain);
		head += remain;
	}

	if (count) {
		head &= tmask;
		ch->ch_w_head = head;
	}

	/* Update printer buffer empty time. */
	if ((un->un_type == DGNC_PRINT) && (ch->ch_digi.digi_maxcps > 0) &&
	    (ch->ch_digi.digi_bufsize > 0)) {
		ch->ch_cpstime += (HZ * count) / ch->ch_digi.digi_maxcps;
	}

	spin_unlock_irqrestore(&ch->ch_lock, flags);

	if (count) {
		/*
		 * Channel lock is grabbed and then released
		 * inside this routine.
		 */
		ch->ch_bd->bd_ops->copy_data_from_queue_to_uart(ch);
	}

	return count;

exit_retry:

	spin_unlock_irqrestore(&ch->ch_lock, flags);
	return 0;
}

struct crypto_priv {
	void __iomem *reg;
	void __iomem *sram;
	int irq;
	struct task_struct *queue_th;

	/* the lock protects queue and eng_st */
	spinlock_t lock;
	struct crypto_queue queue;
	enum engine_status eng_st;
	struct ablkcipher_request *cur_req;
	struct req_progress p;
	int max_req_size;
	int sram_size;
};

static int mv_probe(struct platform_device *pdev)
{
	struct crypto_priv *cp;
	struct resource *res;
	int irq;
	int ret;

	if (cpg) {
		printk(KERN_ERR "Second crypto dev?\n");
		return -EEXIST;
	}

	res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "regs");
	if (!res)
		return -ENXIO;

	cp = kzalloc(sizeof(*cp), GFP_KERNEL);
	if (!cp)
		return -ENOMEM;

	spin_lock_init(&cp->lock);
	crypto_init_queue(&cp->queue, 50);
	cp->reg = ioremap(res->start, res->end - res->start + 1);
	if (!cp->reg) {
		ret = -ENOMEM;
		goto err;
	}

	res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "sram");
	if (!res) {
		ret = -ENXIO;
		goto err_unmap_reg;
	}
	cp->sram_size = res->end - res->start + 1;
	cp->max_req_size = cp->sram_size - SRAM_CFG_SPACE;
	cp->sram = ioremap(res->start, cp->sram_size);
	if (!cp->sram) {
		ret = -ENOMEM;
		goto err_unmap_reg;
	}

	irq = platform_get_irq(pdev, 0);
	if (irq < 0 || irq == NO_IRQ) {
		ret = irq;
		goto err_unmap_sram;
	}
	cp->irq = irq;

	platform_set_drvdata(pdev, cp);
	cpg = cp;

	cp->queue_th = kthread_run(queue_manag, cp, "mv_crypto");
	if (IS_ERR(cp->queue_th)) {
		ret = PTR_ERR(cp->queue_th);
		goto err_thread;
	}

	ret = request_irq(irq, crypto_int, IRQF_DISABLED, dev_name(&pdev->dev),
			cp);
	if (ret)
		goto err_unmap_sram;

	writel(SEC_INT_ACCEL0_DONE, cpg->reg + SEC_ACCEL_INT_MASK);
	writel(SEC_CFG_STOP_DIG_ERR, cpg->reg + SEC_ACCEL_CFG);

	ret = crypto_register_alg(&mv_aes_alg_ecb);
	if (ret)
		goto err_reg;

	ret = crypto_register_alg(&mv_aes_alg_cbc);
	if (ret)
		goto err_unreg_ecb;
	return 0;
err_unreg_ecb:
	crypto_unregister_alg(&mv_aes_alg_ecb);
err_thread:
	free_irq(irq, cp);
err_reg:
	kthread_stop(cp->queue_th);
err_unmap_sram:
	iounmap(cp->sram);
err_unmap_reg:
	iounmap(cp->reg);
err:
	kfree(cp);
	cpg = NULL;
	platform_set_drvdata(pdev, NULL);
	return ret;
}
