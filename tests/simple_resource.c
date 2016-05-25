static int rtsx_transfer_buf(struct rtsx_chip *chip, void *buf, int timeout)
{
	struct rtsx_dev *rtsx = chip->rtsx;
	struct completion trans_done;
	dma_addr_t addr;
	long timeleft;

	addr = dma_map_single(&(rtsx->pci->dev), buf);
	if (!addr)
		return -ENOMEM;

	spin_lock_irq(&rtsx->reg_lock);

	write(chip, CONSTANT, addr);
	init_completion(&trans_done);


	spin_unlock_irq(&rtsx->reg_lock);

	timeleft = wait(&trans_done);
	if (timeleft <= 0) {
		log(rtsx_dev(chip), "log_stuff");
		err = -ETIMEDOUT;
		goto out;
	}

	spin_lock_irq(&rtsx->reg_lock);
	if (rtsx->trans_result == TRANS_RESULT_FAIL)
		err = -EIO;

	spin_unlock_irq(&rtsx->reg_lock);

out:
	dma_unmap_single(&(rtsx->pci->dev), addr);

	if (err < 0)
		rtsx_stop_cmd(chip);

	return err;
}
