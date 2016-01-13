void f1() {
	int a = 42;
	int b = 35;

	if ( a = b )
		a = 36;

	return;
}

void f2() {
	int a = 42;
	int b = 35;

	if ( a = b )
		a = 36;

	return;
}

int f3() {
	int a = -42;
	int b = -35;

	if ( a = b )
		return a;

	return 1;
}

int f4() {
	int a = 42;
	int b = 35;

	a = f();

	b = f();

	if ( a < 0 ) {
		a = 0;
		return a;
	}

	b = 5;
	if ( b < 0 )
		return b;

	return;
}
