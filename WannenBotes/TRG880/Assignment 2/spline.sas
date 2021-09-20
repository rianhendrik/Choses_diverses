ods html close; ods html;


proc iml;
use sasuser.ass2trg;
read all into xy;
xx = xy[,1]; y = xy[,2];
n = nrow(xy);
call scatter(xx, y);

e1 = 479.628; e2 = 389.628;

t1 = (xx - e1)##3;
do i = 1 to n;
	if t1[i] < 0 then t1[i] = 0;
end;
t2 = (xx - e2)##3;
do i = 1 to n;
	if t2[i] < 0 then t2[i] = 0;
end;


x = J(n, 1, 1)||xx||xx##2||xx##3||t1||t2;

sq = x`*x;
test = inv(sq);
print test;
print x;
print sq;

*Checking rank of design matrix;
/*call svd(U, Q, V, x);*/
/**/
/*print Q[L="SingVals"];*/
/**/
/*tol = max(dimension(A)) * constant("maceps") * (max(Q));*/
/*rankSVD = sum(Q > tol);*/
/*print tol rankSVD;*/



thetas = inv(x`*x)*x`*y;
yhat = X*thetas;
e = y - yhat;
sse = e`*e;
print thetas;
print sse;

*We want to select e1 and e2 (our knots) so that the sse is minimsed.;


start reg;
on_error = "if error then do;  thetas = .;  resume; end;";
call push(on_error);   /* PUSH code that will be executed if an error occurs */
thetas = inv(x`*x)*x`*y;
yhat = X*thetas;
e = y - yhat;
sse = e`*e;
finish reg;

e1s = do(min(xx), max(xx), 10);
e2s = do(min(xx), max(xx), 10);
*e1s = {300 400 500};
*e2s = {400 500 560};

sses = J(ncol(e1s)#ncol(e1s), 3, .);
counter = 0;
do i = 1 to ncol(e1s);
	do j = 1 to ncol(e2s);
		counter = counter + 1;
		e1 = e1s[,i]; e2 = e2s[,j];

		t1 = (xx - e1)##3;
		do ii = 1 to n;
			if t1[ii] < 0 then t1[ii] = 0;
		end;
		t2 = (xx - e2)##3;
		do ii = 1 to n;
			if t2[ii] < 0 then t2[ii] = 0;
		end;

		x = J(n, 1, 1)||xx||xx##2||xx##3||t1||t2;
		
		deter = det(x`*x);

		if deter ^=0 then call reg;
		
		print counter,, e1 e2,, deter;

		sses[counter,1] = sse;
		sses[counter,2] = e1;
		sses[counter,3] = e2;

	end;
end;


opti_es = sses[>:<,1];
print sses;
print opti_es;

*SSE over grid search;
dom = do(1, nrow(sses), 1);
call series(dom, sses[,1]);

knot_estimates = sses[opti_es,];
print knot_estimates;

e1_hat = knot_estimates[2];
e2_hat = knot_estimates[3];

/*Final optimal model*/

e1 = e1_hat; e2 = e2_hat;

t1 = (xx - e1)##3;
do ii = 1 to n;
	if t1[ii] < 0 then t1[ii] = 0;
end;
t2 = (xx - e2)##3;
do ii = 1 to n;
	if t2[ii] < 0 then t2[ii] = 0;
end;

x = J(n, 1, 1)||xx||xx##2||xx##3||t1||t2;
print x;

call reg;

call series(xx, yhat);


*Using bootstrap to determine 95% confidence intervals for e1 and e2 - 1000 bootstrap resamples -> 1000 estimates of e1 and e2 ;


start bs;
u = sample(1:n,n);
ybs = y[u,];
xbs = xx[u,];
finish bs;


start reg;
thetas = inv(x`*x)*x`*y;
yhat = X*thetas;
e = y - yhat;
sse = e`*e;
finish reg;



e1_hats = J(nsim, 1, .);
e2_hats = J(nsim, 1, .);

nsim = 1000;
b=1;
do b = 1 to nsim;

	call bs;

	xx = xbs;

	e1s = do(min(xx), max(xx), 10);
	e2s = do(min(xx), max(xx), 10);



	sses = J(ncol(e1s)#ncol(e1s), 3, .);
	counter = 0;
	do i = 1 to ncol(e1s);
		do j = 1 to ncol(e2s);
			counter = counter + 1;
			e1 = e1s[,i]; e2 = e2s[,j];

			t1 = (xx - e1)##3;
			do ii = 1 to n;
				if t1[ii] < 0 then t1[ii] = 0;
			end;
			t2 = (xx - e2)##3;
			do ii = 1 to n;
				if t2[ii] < 0 then t2[ii] = 0;
			end;

			x = J(n, 1, 1)||xx||xx##2||xx##3||t1||t2;
			
			deter = det(x`*x);

			if deter ^=0 then call reg;

			sses[counter,1] = sse;
			sses[counter,2] = e1;
			sses[counter,3] = e2;

		end;
	end;

	e1_hats[b] = knot_estimates[2];
	e2_hats[b] = knot_estimates[3];

end;

print e1_hats e2hats;










