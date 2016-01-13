void f() {
	int a = 1;
	int b = -2;
	int c;

	{
		c = -3;
		a = -4;
	}

	if ( a = b )
		a = 5;

	return;
}
