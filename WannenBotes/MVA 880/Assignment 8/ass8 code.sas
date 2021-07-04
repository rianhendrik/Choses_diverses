ods output close;
ods html;

libname MVA880 "C:\Users\rianh\Dropbox\MVA880\MVA_data";




proc iml;
use mva880.digitq3;
read all into data;

/*i = 597;*/
/*digit_i = shape(data[i,], 16, 16);*/
/*call HeatmapCont(digit_i);*/



pixel_r = 16;
pixel_c = 16;
D = pixel_r#pixel_c; *number  of pixels per image; 
k = 3;
n = nrow(data);

*random initial means - K x D means.;


call randseed(123);         /* set random number seed */
means = j(D, K, .);                /* allocate */
call randgen(means, "Uniform", 0.25, 0.75); /* u ~ U(0,1) */

means_std = means/means[+,];
test = means_std[+,];
*print means_std test;

means = means_std; *making sure that we are using the standardised means;

*Visualising the means - we want to do this after each EM iteration to see how the means are changing for eack component to reflect the digits they represent;
/*digit_i = shape(means[,3], 16, 16);*/
/*call HeatmapCont(digit_i);*/

*random initial pi values - K = 3, therefore three pi's.;
pis = J(k, 1, 1/k);

/*Univariate Bernoulli pmf;*/
start ber(x, mu);
   return( (mu##x)#(1 - mu)##(1-x) );  /* elementwis multiplication */
finish;

/*We need to find the joint distribution of the D Bernoullis in each image;*/
/*Thus, we need to multiply D Bernoulli pdfs together;*/
/*Now, each image represents a Multivariate Bernoulli random variable - consisting of D independent Bernoulli random variables*/
start vec_mult(probs); *a function to find the product of a ROW vector; 
	product = probs[1]*probs[2]; *probs is a row vector of D probabilities from the D Bernouli pmf's - outputed by the ber() function;
	do i = 3 to ncol(probs);
		product = product*probs[i];
	end;
	return(product);
finish;


tol = 1;

*iter = 0;
loglik = {0, 1};
q = 2;

*do while ( (loglik[q] - loglik[q-1]) <= tol );
do iter = 1 to 30;



	q = q + 1; *for log likelihood tracker;
	*iter = iter + 1; 

	gamma_ik = J(n, k, .);
	do i = 1 to n; * we want to find gamma zik for the ith digit in the kth component ;	
		denoms = J(k, 1, .); *to store the values that will be summed to form the denominator to compute gamma(zik) ; 
		data_i = data[i,]; *we are considering the ith digit, the outcome of each of the D Bernoullis, as a row vector ;

		do mv_prob = 1 to k; *calculating k mv_probs (multivariate probabilities), therefore iterating k times ;
			probs = ber(data_i, (means[,mv_prob])`); *probability of observing the D pixels in digit_i under the "old" D means for the kth component;
			mv_prob_k = vec_mult(probs); *probability of observing digit_i (as a MVBernoulli) under the "old" means for the kth component;
			if mv_prob_k = 0 then mv_prob_k = 0.000001; *This is to prevent a zero value;
			denoms[mv_prob] = pis[mv_prob]#mv_prob_k; *the kth element of the denominator, all these will be summed together for the complete denom ;
		end;
		*Calculating gammas (expected values of zik);
		do iterg = 1 to k;
	 		gamma_ik[i,iterg] = denoms[iterg]/sum(denoms); *the values in denoms have already been multiplied by their respective pis's;	
		end;
	end;

	/*COMPLETE LOG LIKELIHOOD STEP*/
	*calculating the value of the log-likelihood function;
	lik_image_i = J(n, k, .);
/*	do i = 1 to n;*/
/*		do j = 1 to k;*/
/*			q1 = (data[i,])`#log(means[,k]) + (1-(data[i,])`)#log(1-means[,k]);	*/
/*			q2 = gamma_ik[i,k] # (log(pis[k]) + sum(q1));*/
/*			lik_image_i[i,j] = q2;*/
/*		end;	*/
/*	end;*/

	do i = 1 to n;
		do j = 1 to k;
			q1 = (data[i,])`#log(means[,k]) + (1-(data[i,])`)#log(1-means[,k]);	
			q2 = (log(pis[k]) + sum(q1));
			lik_image_i[i,j] = q2;
		end;	
	end;


/*	do i = 1 to n;*/
/*		do j = 1 to k;*/
/*			q1 = vec_mult(ber(data[i,], (means[,j])`));*/
/*			if q1 = 0 then q1 = 0.000001;*/
/*			q2 = pis[j]#q1;*/
/*			lik_image_i[i,j] = q2;*/
/*		end;*/
/*	end;*/
	*newest_loglik = sum(log(lik_image_i[,+]));
	newest_loglik = sum(lik_image_i[,+]); *old (possibly incorrect loglik function);
	loglik = loglik//newest_loglik;


	/*MAXIMISATION STEP*/
	/*UPDATING PARAMETERS*/

	*new effective sample sizes;
	nks = J(k, 1, .); *calculating our effective sample size;
	do i  = 1 to k;
		nks[i] = gamma_ik[+,i];
	end;

	*new mean (parameter) vectors;
	means = J(D, k, .);
	do i = 1 to k;
		means[,i] = (data`*gamma_ik[,i])/nks[i]; 
	end;
	do i = 1 to D;
		do j = 1 to k;
			if means[i,j] = 0 then means[i,j] = 0.00001; *To prevent a mean of 0, so that we do not have a 0##0 in the bernoulli pmf, resulting ber() crash;
		end;
	end;


	*new pi's';
	pis = J(k, 1, .);
	do i  = 1 to k;
		pis[i] = nks[i]/n;
	end;

	print iter;

	do comp_means = 1 to k;
		print comp_means;
		means_k = shape(means[,comp_means], 16, 16);
		call HeatmapCont(means_k);
	end;

end;

print loglik;

mean = mean(loglik); print mean;
iterr = do(1, nrow(loglik), 1);
title "Likelihood plot over iterations";
call series(iterr, loglik) other = "refline -276172.6 / axis = y";












/*do iter = 280 to 340;*/
/*	class_t = max(gamma_ik[iter,]);*/
/*	class = loc(gamma_ik[iter,] = class_t);*/
/*	if class = 1 then number = 0;*/
/*	if class = 2 then number = 6;*/
/*	if class = 3 then number = 8;*/
/**/
/*	print class number;*/
/**/
/*	digit_i = shape(data[iter,], 16, 16);*/
/*	call HeatmapCont(digit_i);*/
/*end;*/
/**/
/*quit;*/
