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

struct sh_rtc {
	void __iomem		*regbase;
	unsigned long		regsize;
	struct resource		*res;
	int			alarm_irq;
	int			periodic_irq;
	int			carry_irq;
	struct clk		*clk;
	struct rtc_device	*rtc_dev;
	spinlock_t		lock;
	unsigned long		capabilities;	/* See asm/rtc.h for cap bits */
	unsigned short		periodic_freq;
};


static int __init sh_rtc_probe(struct platform_device *pdev)
{
	struct sh_rtc *rtc;
	struct resource *res;
	struct rtc_time r;
	char clk_name[6];
	int clk_id, ret;

	rtc = kzalloc(sizeof(struct sh_rtc), GFP_KERNEL);
	if (unlikely(!rtc))
		return -ENOMEM;

	spin_lock_init(&rtc->lock);

	/* get periodic/carry/alarm irqs */
	ret = platform_get_irq(pdev, 0);
	if (unlikely(ret <= 0)) {
		ret = -ENOENT;
		dev_err(&pdev->dev, "No IRQ resource\n");
		goto err_badres;
	}

	rtc->periodic_irq = ret;
	rtc->carry_irq = platform_get_irq(pdev, 1);
	rtc->alarm_irq = platform_get_irq(pdev, 2);

	res = platform_get_resource(pdev, IORESOURCE_IO, 0);
	if (unlikely(res == NULL)) {
		ret = -ENOENT;
		dev_err(&pdev->dev, "No IO resource\n");
		goto err_badres;
	}

	rtc->regsize = resource_size(res);

	rtc->res = request_mem_region(res->start, rtc->regsize, pdev->name);
	if (unlikely(!rtc->res)) {
		ret = -EBUSY;
		goto err_badres;
	}

	rtc->regbase = ioremap_nocache(rtc->res->start, rtc->regsize);
	if (unlikely(!rtc->regbase)) {
		ret = -EINVAL;
		goto err_badmap;
	}

	clk_id = pdev->id;
	/* With a single device, the clock id is still "rtc0" */
	if (clk_id < 0)
		clk_id = 0;

	snprintf(clk_name, sizeof(clk_name), "rtc%d", clk_id);

	rtc->clk = clk_get(&pdev->dev, clk_name);
	if (IS_ERR(rtc->clk)) {
		/*
		 * No error handling for rtc->clk intentionally, not all
		 * platforms will have a unique clock for the RTC, and
		 * the clk API can handle the struct clk pointer being
		 * NULL.
		 */
		rtc->clk = NULL;
	}

	clk_enable(rtc->clk);

	rtc->capabilities = RTC_DEF_CAPABILITIES;
	if (pdev->dev.platform_data) {
		struct sh_rtc_platform_info *pinfo = pdev->dev.platform_data;

		/*
		 * Some CPUs have special capabilities in addition to the
		 * default set. Add those in here.
		 */
		rtc->capabilities |= pinfo->capabilities;
	}

	if (rtc->carry_irq <= 0) {
		/* register shared periodic/carry/alarm irq */
		ret = request_irq(rtc->periodic_irq, sh_rtc_shared,
				  IRQF_DISABLED, "sh-rtc", rtc);
		if (unlikely(ret)) {
			dev_err(&pdev->dev,
				"request IRQ failed with %d, IRQ %d\n", ret,
				rtc->periodic_irq);
			goto err_unmap;
		}
	} else {
		/* register periodic/carry/alarm irqs */
		ret = request_irq(rtc->periodic_irq, sh_rtc_periodic,
				  IRQF_DISABLED, "sh-rtc period", rtc);
		if (unlikely(ret)) {
			dev_err(&pdev->dev,
				"request period IRQ failed with %d, IRQ %d\n",
				ret, rtc->periodic_irq);
			goto err_unmap;
		}

		ret = request_irq(rtc->carry_irq, sh_rtc_interrupt,
				  IRQF_DISABLED, "sh-rtc carry", rtc);
		if (unlikely(ret)) {
			dev_err(&pdev->dev,
				"request carry IRQ failed with %d, IRQ %d\n",
				ret, rtc->carry_irq);
			free_irq(rtc->periodic_irq, rtc);
			goto err_unmap;
		}

		ret = request_irq(rtc->alarm_irq, sh_rtc_alarm,
				  IRQF_DISABLED, "sh-rtc alarm", rtc);
		if (unlikely(ret)) {
			dev_err(&pdev->dev,
				"request alarm IRQ failed with %d, IRQ %d\n",
				ret, rtc->alarm_irq);
			free_irq(rtc->carry_irq, rtc);
			free_irq(rtc->periodic_irq, rtc);
			goto err_unmap;
		}
	}

	platform_set_drvdata(pdev, rtc);

	/* everything disabled by default */
	sh_rtc_irq_set_freq(&pdev->dev, 0);
	sh_rtc_irq_set_state(&pdev->dev, 0);
	sh_rtc_setaie(&pdev->dev, 0);
	sh_rtc_setcie(&pdev->dev, 0);

	rtc->rtc_dev = rtc_device_register("sh", &pdev->dev,
					   &sh_rtc_ops, THIS_MODULE);
	if (IS_ERR(rtc->rtc_dev)) {
		ret = PTR_ERR(rtc->rtc_dev);
		free_irq(rtc->periodic_irq, rtc);
		free_irq(rtc->carry_irq, rtc);
		free_irq(rtc->alarm_irq, rtc);
		goto err_unmap;
	}

	rtc->rtc_dev->max_user_freq = 256;

	/* reset rtc to epoch 0 if time is invalid */
	if (rtc_read_time(rtc->rtc_dev, &r) < 0) {
		rtc_time_to_tm(0, &r);
		rtc_set_time(rtc->rtc_dev, &r);
	}

	device_init_wakeup(&pdev->dev, 1);
	return 0;

err_unmap:
	clk_disable(rtc->clk);
	clk_put(rtc->clk);
	iounmap(rtc->regbase);
err_badmap:
	release_resource(rtc->res);
err_badres:
	kfree(rtc);

	return ret;
}
