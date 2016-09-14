int **f1(int size) {
	int i, j, ret;
	int ** res;
	res = malloc(size * sizeof(*res));

	for (i = 0; i < size; i++) {
		res[i] = malloc(size * sizeof(**res));
	}

	ret = do_stuff();
	if (ret < 0) {
		for (i = 0; i < size; i++) {
			free(res[i]);
		}
		free(res);
		return -EINVAL;
	}

	ret = do_more_stuff();
	if (ret < 0) {
		for (j = 0; j < size; j++) {
			free(res[j]);
		}
		free(res);
		return -EINVAL;
	}

	ret = do_final_stuff();
	if (ret < 0) {
		return -EINVAL;
	}

	return res;
}
