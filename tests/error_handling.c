int f1() {
	int a = 42;
	int b = 35;

	struct foo * bar = allocate_resource();

	a = f();

	b = f();

	if (ERR_PTR == g()) {
		printk("stuff that went wrong");
		release_resource(bar);
		return a;
	}

	if (a < b) {
		initialise_resource(bar);
	}

	return 0;
}
