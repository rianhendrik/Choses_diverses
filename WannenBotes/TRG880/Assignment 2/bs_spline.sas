*Using bootstrap to determine 95% confidence intervals for e1 and e2 - 1000 bootstrap resamples -> 1000 estimates of e1 and e2 ;

ods html close; ods html;


proc iml;
use sasuser.ass2trg;
read all into xy;
x = xy[,1]; y = xy[,2];
n = nrow(xy);




start bs;
u = sample(1:n,n);
ybs = y[u,];
xbs = x[u,];
finish bs;


start reg2;
thetas = inv(xx`*xx)*xx`*ybs;
yhat = xx*thetas;
e = ybs - yhat;
sse = e`*e;
tss = ssq(y - J(n, 1, mean(y)));
R2 = 1 - (sse/tss);
sses[counter,1] = sse;
sses[counter,2] = R2;
sses[counter,3] = e1;
sses[counter,4] = e2;
finish reg2;

nsim = 1000;

e1_hats = J(nsim, 1, .);
e2_hats = J(nsim, 1, .);
r2s = J(nsim, 1, .);


do b = 1 to nsim;

	call bs;
    
	e1s = do(min(xbs), max(xbs), 10);
	e2s = do(min(xbs), max(xbs), 10);



	sses = J(ncol(e1s)#ncol(e1s), 4, .);
	counter = 0;

	do i = 1 to ncol(e1s);
		do j = 1 to ncol(e2s);
			counter = counter + 1;
			e1 = e1s[,i]; e2 = e2s[,j];

			t1 = (xbs - e1)##3;
			do ii = 1 to n;
				if t1[ii] < 0 then t1[ii] = 0;
			end;
			t2 = (xbs - e2)##3;
			do ii = 1 to n;
				if t2[ii] < 0 then t2[ii] = 0;
			end;

			xx = J(n, 1, 1)||xbs||xbs##2||xbs##3||t1||t2;
		
			deter = det(xx`*xx);

			if deter ^=0 then call reg2;

/*			sses[counter,1] = sse;*/
/*		    sses[counter,2] = e1;*/
/*			sses[counter,3] = e2;*/


		end;
	end;

	opti_es = sses[>:<,1];

	knot_estimates = sses[opti_es,];

	epsilons = knot_estimates[3:4];
	e1 = min(epsilons);
	e2 = max(epsilons);
	
	r2s[b] = knot_estimates[2];
	e1_hats[b] = e1;
	e2_hats[b] = e2;

end;

print e1_hats e2_hats;

alpha = 0.05;

call sort(e1_hats);
ll_e1 = e1_hats[round((alpha/2)*nsim)];
ul_e1 = e1_hats[round((1-alpha/2)*nsim)];

call sort(e2_hats);
ll_e2 = e2_hats[round((alpha/2)*nsim)]; 
ul_e2 = e2_hats[round((1-alpha/2)*nsim)];

call sort(r2s);
ll_r2 = r2s[round((alpha/2)*nsim)];
ul_r2 = r2s[round((1-alpha/2)*nsim)];


print "95% confidence interval for e1 is" ll_e1 ul_e1;
print "95% confidence interval for e2 is" ll_e2 ul_e2;
print "95% confidence interval for r2 is" ll_r2 ul_r2;

