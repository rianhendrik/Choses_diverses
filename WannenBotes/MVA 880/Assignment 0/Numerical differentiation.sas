/* Question 4: Numerical differentiation */

proc iml;

h = 0.00001;
x = 5;
der11 = (sin(x+h) - sin(x))/h;
der11_test = cos(x);
*der12 = ();
print der11 der11_test;

start deriver(n, x, h);
	res_k = 0;
	do k = 0 to n;
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#sin(x+k#h));
	end;
	return((1/h##n)#res_k);
finish;




t = deriver(2, 5, h); print t;

/* Question 2 */
/* First derivative wrt x */
start deriver(n, x, y, h);
	res_k = 0;
	do k = 0 to n;
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#(3#y#(x+k#h)+(x+k#h)##2));
	end;
	return((1/h##n)#res_k);
finish;

test1 = deriver(2, 5, 3, 0.01, 'func'); print test1;
/* First derivative wrt y */
start deriver(n, x, y, h);
	res_k = 0;
	do k = 0 to n;
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#(3#x#(y+k#h)+(y+k#h)##2));
	end;
	return((1/h##n)#res_k);
finish;

test2 = deriver(1, 5, 3, h); print test2;


/* Second derivative wrt x, then y */
start deriver(n, x, y, h);
	res_k = 0;
	do k = 0 to n;
		res_k = res_k+(((-1)##(k+n))#comb(n,k)#(3#(y+h#k)));
	end;
	return((1/h##n)#res_k);
finish;

test3= deriver(1, 5, 3, h); print test3;




/* First derivative wrt y then second derivative wrt x */









		