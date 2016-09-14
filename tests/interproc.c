int f1(int * res, int ignored) {
	if (reasons())
		free(res);
		return -EFAULT;

	return 0;
}

int f2() {
	int i, j, ret;
	int * res;
	res = malloc(size * sizeof(*res));

	ret = do_stuff();
	if (ret < 0) {
		ret = f1(res);

		if (ret < 0) {
			log();
		}

		return -EINVAL;
	}

	ret = do_final_stuff();
	if (ret < 0) {
		return -EINVAL;
	}

	return res;
}
