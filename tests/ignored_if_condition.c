int f1(struct file *file)
{
	struct inode *inode = file_inode(file);
	TW_Device_Extension *tw_dev = twl_device_extension_list[iminor(inode)];

	if (mutex_lock_interruptible(&tw_dev->ioctl_lock)) {
		retval = -EINTR;
		goto out;
	}

	retval = do_stuff1();
	if (retval)
		goto out2;

	retval = do_stuff2();
	if (retval) {
		retval = -EINVAL;
		goto out2;
	}

out2:
	mutex_unlock(&tw_dev->ioctl_lock);
out:
	return retval;
}
