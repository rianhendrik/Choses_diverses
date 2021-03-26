/* Question 3 */
proc iml;
start f(x);
	pi = constant('pi'); mu1=10; mu2=30; sigma1=2; sigma2=8; pi2=2/3; 
	return 0.5#pi2#(1/(sigma1#sqrt(2#pi)))*exp(-0.5#((x-mu1)/sigma1)##2)+pi2#(1/(sigma2#sqrt(2#pi)))*exp(-0.5#((x-mu2)/sigma2)##2);
finish;

test = f(15);
print test;

call quad(R, "f", -100||100);
print R;

/* Visualising this density*/
xs = t(do(-50,50,0.5));
res = J(nrow(xs), 1, .);
do i = 1 to nrow(xs);
	x = xs[i];
	res[i] = f(x);
end;

run Series(xs, res);

/* Question 3b: Calculating F(40) */

numba = 40;
call quad(R, 'f', -1000||numba);
print R;

/* Question 3c: Calculating p20 */

xs = t(do(10,50,0.001));
last_v = 0;
R = 0; *Remember to initalise your value of R!!!;
do i=1 to nrow(xs) while(R<0.2);
	xi = xs[i];
	call quad(R, 'f', -1000||xi);
	print i xi R;
	last_v = last_v//xi;	
end;

last = nrow(last_v); p20 = last_v[last];
print last p20;
	

call quad(R, 'f', -1000||p20);
print R;
