int f1() {
	int err = do_stuff();
	if (err) {
		release();

		while (!is_finished()) {
			release_element();
		}
	}

	return err;
}
