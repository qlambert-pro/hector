int f1() {
	int err = -ENOMEM;
	int * var = g();

	if (var == ERR_PTR) {
		release();
	}
	else {
		do_stuff();
		err = 0;
	}

	return err;
}

int f2() {
	int err = do_stuff();
	if (err)
		release();

	return err;
}

int f3() {
	int err = do_stuff();
	if (err < 0) {
		release();
		if (err == -E1)
			release1();
		if (err == -E2)
			release2();
	}

	return err;
}

int f4() {
	int err = do_stuff();
	if (err < 0) {
		if (err == -E1)
			goto good;
		else
			release();
	}

	return err;

good:
	return 0;
}

int f5(int a, int b) {
	for (;;) {
		a = b;
		b = a;
	}

	return a;
}
