int f1()
{
	struct s1 *a = get();
	int result = 0;

	lock(a);
	if (clear_dirty(a)) {
		ready(a);
	} else if (a->state == NOSPACE) {
		result = -ERROR;
	} else {
		BUG();
	}

	unlock(a);
	return result;
}

int f2()
{
	int i;
	int retval;
	int var;

	for (i = 0; i < 3; i++) {
		retval = do_stuff();
		if (retval != STATUS_SUCCESS) {
			if (var != STATUS_SUCCESS) {
				release1();
			}
			release2();
			break;
		} else {
			set_stuff();
		}
	}

	return retval;
}


int f3()
{
	int err;
	int var;
	int var2;

	err = allocate();
	if (err)
		return err;

	if (var != 0) {
		do_stuff1();
	} else {
		do_stuff2();
	}

	switch (var2) {
	case 1:
		do_stuff3();
		break;
	default:
		goto failed_fb;
	}
	set();

	return 0;

failed_fb:
	release();

	return -E;
}


int f4()
{
	do {
		do_stuff();

		break;
	} while (1);

	return true;
}
