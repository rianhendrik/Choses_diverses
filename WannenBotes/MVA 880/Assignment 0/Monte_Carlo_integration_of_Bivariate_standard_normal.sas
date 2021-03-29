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

/*start Define2DGrid(XMin, XMax, Nx, YMin, YMax, Ny);*/
/*   XDiv = do(XMin, XMax, (XMax-XMin)/(Nx-1));*/
/*   YDiv = do(YMin, YMax, (YMax-YMin)/(Ny-1));*/
/*   X = repeat(XDiv, Ny);*/
/*   X = shape(X, Nx*Ny);*/
/*   Y = repeat(YDiv, Nx);*/
/*   Y = shape(T(Y), Nx*Ny);*/
/*   return ( X || Y );*/
/*finish;*/
/**/
/*grid = Define2Dgrid(-5, 5, 50, -5, 5, 50);*/
/*resses = J(nrow(grid), 1, .);*/
/*do i = 1 to nrow(grid);*/
/*	resses[i] = biv_norm(grid[i,1], grid[i,2]);*/
/*end;*/
/**/
/*c1 = max(resses);*/
/*print c1;*/

c2 = max(res);
print c2;

lb = -5; ub = 5;
volume = 10#10#c2;
print volume;
nsim = 100000;
accept = {0 0};
do i = 1 to nsim;
	x1 = lb + (ub-lb)*rand("Uniform");
	x2 = lb + (ub-lb)*rand("Uniform");
	xs = x1 || x2;
	fx12 = biv_norm(x1, x2);
	y = rand("Uniform")*c2;
	if y<fx12 then accept = accept//xs;
end;

prop = (nrow(accept)-1)/nsim;
print prop;
area = prop#volume;
print area;




