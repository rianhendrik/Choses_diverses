/* Monte Carlo integration of Bivariate standard normal. */

proc iml;

start biv_norm(x1, x2);
	p = 0.5; pi = constant("pi");
   return (1/(2#pi#sqrt(1-p##2)))#exp(-(x1##2-2#p#x1#x2+x2##2)/2#(1-p##2));
finish;

/* Plotting marginal dist of xi, just for interest's sake */
xs = t(do(-5,5,0.1));
res = J(nrow(xs), 1, .);
do i = 1 to nrow(xs);
	x = xs[i];
	res[i] = biv_norm(0, x);
end;
*run series(xs, res);

c = max(res);

lb = -5; ub = 5;
nsim = 1000000;
accept = {0 0};
do i = 1 to nsim;
	x1 = lb + (ub-lb)*rand("Uniform");
	x2 = lb + (ub-lb)*rand("Uniform");
	xs = x1 || x2;
	fx12 = biv_norm(x1, x2);
	y = rand("Uniform")*c;
	if y<fx12 then accept = accept//xs;
end;

area = (nrow(accept)-1)/nsim;
print area;
