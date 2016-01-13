int f1() {
	int a = -42;
	int b = -35;

	if ( a = b )
		return a;

	return 1;
}

int f2() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( !(b < 0) )
		return b;

	return 0;
}

int f3() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( a == ERR_PTR )
		return b;

	return 0;
}

int f4() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( ERR_PTR == g() )
		return a;

	return 0;
}

int f5() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( f() == g() )
		return a;

	return 0;
}

int f6() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( a < 0 )
		return a;
	return 1;
}

int f7() {
	int rc = allocate_and_init();
	if (rc != 0)
		do_stuff();
	return rc;
}
