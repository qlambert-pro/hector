int f() {
	int a = -42;
	int b = -35;

	if ( a = b )
		return a;

	return 1;
}

int g() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( a < 0 )
		return a;

	if ( b < 0 )
		return b;

	return;
}
