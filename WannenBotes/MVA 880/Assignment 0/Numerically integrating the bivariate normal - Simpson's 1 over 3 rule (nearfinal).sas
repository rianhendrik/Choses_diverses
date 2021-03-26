/* Numerically integrating the bivairate normal (Simpson's 1/3 rule). */

proc iml;

start biv_norm(x1, x2);
	p = 0.5; pi = constant("pi");
   return (1/(2#pi#sqrt(1-p##2)))#exp(-(x1##2-2#p#x1#x2+x2##2)/2#(1-p##2));
finish;


test = biv_norm(0,0);
print(test);


/* Plotting marginal dist of xi, just for interest's sake */
/* xs = t(do(-5,5,0.1)); */
/* res = J(nrow(xs), 1, .); */
/* do i = 1 to nrow(xs); */
/* 	x = xs[i]; */
/* 	res[i] = biv_norm(0, x); */
/* end; */
/*  */
/* run Series(xs, res); */

/* Simpson's numerical integration */
h = 0.01; *Step size;
A = 10; *Large number in Gaussian space;
x1_p = -10; x2_p = -10;
n = 1 + int((A-x1_p)/h) - int(0.5+0.5*(int(A-x1_p)/h));
m = 1 + int((A-x2_p)/h) - int(0.5+0.5*(int(A-x2_p)/h));
del1=0; del2=0; del3=0; del4=0;
res=0;
do j=1 to 2#m+1;
	do i=1 to 2#n+1;
		ai = x1_p+(i-1)#h; bj = x2_p+(j-1)#h;
		if i=1 then del1=1; if i=(2#n+1) then del2=1; if j=1 then del3=1; if j=(2#m+1) then del4=1;       
		wij = 4-mod(i,2) - mod(j,2) - del1 - del2 - del3 - del4; *I think there is something wrong with weights.
		*print wij i j ,, ai bj;
		res = res + (2##wij)#biv_norm(ai,bj);
	end;
end;
print res;

res_fin = (h##2/9)*res;
print res_fin;






