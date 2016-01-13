int f1() {
	int * a = NULL;
	int b = 35;

	struct foo * bar = allocate_resource();

	a = g();

	b = f();

	if (ERR_PTR == a) {
		printk("stuff that went wrong");
		release_resource(bar);
		return a;
	}

	if (12 < b) {
		initialise_resource(bar);
	}

	return 0;
}
