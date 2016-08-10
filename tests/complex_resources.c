int mg_get_ICV(struct scsi_cmnd *srb, struct rtsx_chip *chip)
{
	struct ms_info *ms_card = &chip->ms_card;
	int retval;
	int bufflen;
	unsigned int lun = SCSI_LUN(srb);
	u8 *buf = NULL;

	ms_cleanup_work(chip);

	retval = ms_switch_clock(chip);
	if (retval != STATUS_SUCCESS) {
		rtsx_trace(chip);
		return STATUS_FAIL;
	}

	buf = kmalloc(1028, GFP_KERNEL);
	if (!buf) {
		rtsx_trace(chip);
		return STATUS_ERROR;
	}

	buf[0] = 0x04;
	buf[1] = 0x02;
	buf[2] = 0x00;
	buf[3] = 0x00;

	retval = mg_send_ex_cmd(chip, MG_GET_IBD, ms_card->mg_entry_num);
	if (retval != STATUS_SUCCESS) {
		set_sense_type(chip, lun, SENSE_TYPE_MEDIA_UNRECOVER_READ_ERR);
		rtsx_trace(chip);
		goto GetICVFinish;
	}

	retval = ms_transfer_data(chip, MS_TM_AUTO_READ, PRO_READ_LONG_DATA,
				2, WAIT_INT, 0, 0, buf + 4, 1024);
	if (retval != STATUS_SUCCESS) {
		set_sense_type(chip, lun, SENSE_TYPE_MEDIA_UNRECOVER_READ_ERR);
		rtsx_clear_ms_error(chip);
		rtsx_trace(chip);
		goto GetICVFinish;
	}
	if (check_ms_err(chip)) {
		set_sense_type(chip, lun, SENSE_TYPE_MEDIA_UNRECOVER_READ_ERR);
		rtsx_clear_ms_error(chip);
		rtsx_trace(chip);
		return STATUS_FAIL;
	}

	bufflen = min_t(int, 1028, scsi_bufflen(srb));
	rtsx_stor_set_xfer_buf(buf, bufflen, srb);

GetICVFinish:
	kfree(buf);
	return retval;
}
