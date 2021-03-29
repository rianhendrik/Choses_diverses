proc iml;


/*Question 1's objective function*/
start q1_func(x);
	return sin(x);
finish;

h = 0.00001; *the step size;
x = 5;  *the point that we are evaluating question 1's derivatives at;

/*The Newton's difference quotient deriver function*/
start deriver(func, n, x, h);
	res_k = 0;
	do k = 0 to n;
		funky = "z =" + func + "(x+k#h);";
		call execute(funky);
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#z);
	end;
	return((1/h##n)#res_k);
finish;

/*First derivative of sin(x) with respect to x*/
der11 = deriver('q1_func', 1, x, h);
/*Second derivative of sin(x) with respect to x*/
der12 = deriver('q1_func', 2, x, h); 
print der12 der11;

/* Question 2 */

/*If derivative is w.r.t x*/
start deriverx(func, n, x, y, h);
	res_k = 0;
	do k = 0 to n;
		funky = "z =" + func + "(x+k#h, y);";
		call execute(funky);
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#z);
	end;
	return((1/h##n)#res_k);
finish;
/*If derivative is w.r.t y*/
start derivery(func, n, x, y, h);
	res_k = 0;
	do k = 0 to n;
		funky = "z =" + func + "(x, y+k#h);";
		call execute(funky);
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#z);
	end;
	return((1/h##n)#res_k);
finish;


/*Question 2's objective function*/
start q2a_func(x, y);
	return 3#x#y+x##2+y##2;
finish;
x = 5; y = 3;

q2a = deriverx('q2a_func', 1, x, y, h);
print q2a;
q2b = derivery('q2a_func', 1, x, y, h);
print q2b;

start q2c_func(x, y);
	return 3#y + 2#x;
finish;

q2c = derivery('q2c_func', 1, x, y, h);
print q2c;

q2d = deriverx('q2a_func', 2, x, y, h);
print q2d;

/*The end*/
