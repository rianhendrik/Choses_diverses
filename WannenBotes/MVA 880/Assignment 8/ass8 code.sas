ods output close;
ods html;

libname MVA880 "C:\Users\rianh\Dropbox\MVA880\MVA_data";

proc iml;
use mva880.digitq3;
read all into data;
i = 36;
digit_i = shape(data[i,], 16, 16);
call HeatmapCont(digit_i);


pixel_r = 16;
pixel_c = 16;
D = pixel_r#pixel_c; *number  of pixels per image; 
k = 3;
n = nrow(data);

*random initial means - K x D means.;


call randseed(123);         /* set random number seed */
means = j(D, K, .);                /* allocate */
call randgen(means, "Uniform", 0.25, 0.75); /* u ~ U(0,1) */
print means;


means_std = means/means[+,];
test = means_std[+,];
print means_std test;


means = means_std;

*print means;

*Visualising the means - we want to do this after each EM iteration to see how the means are changing for eack component to reflect the digits they represent;
digit_i = shape(means[,1], 16, 16);
call HeatmapCont(digit_i);

/*mu1 = means[,1];*/
/*mu2 = means[,2];*/
/*mu3 = means[,3];*/

*random initial pi values - K = 3, therefore three pi's.;

pi1 = 1/3;
pi2 = 1/3;
pi3 = 1/3;

pis = pi1//pi2//pi3;
print pis;


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

*test;
/*mu_1 = (means_std[,1])`;*/
/*dig1 = data[1,];*/
/*probs = ber(dig1, mu_1);*/
/*denom_k =  vec_mult(probs);*/
/*t = denom_k#100;*/
/*print probs,, denom_k,, t;*/

/*EXPECTATION STEP*/
/*Here, we want to find the probability that the ith digit belongs to the kth component*/



*print iter;

/*UNDERSTANDING WHY SOME zik numerators are 0;*/

*My solution to this problem is to set these 0 values to a very small value close to 0.

/*dc = data[34,]`;*/
/*mu1 = means[,1];*/
/*print dc mu1;*/
/**/
/*test = ber(dc`, mu1`);*/
/*print test;*/
/**/
/*test2 = vec_mult(test);*/
/*print test2;*/
/**/
/*start vec_mult; *a subroutine to find the product of a ROW vector; */
/*	product = probs[1]*probs[2]; *probs is a row vector of D probabilities from the D Bernouli pmf's - outputed by the ber() function;*/
/*	print product;*/
/*	do i = 3 to ncol(probs);*/
/*		product = product*probs[i];*/
/*		print i product;*/
/*	end;*/
/*finish;*/
/**/
/*probs = test;*/
/**/
/*call vec_mult;*/;


do iter = 1 to 30;
print iter;



zik = J(n, k, .);
do comp_k = 1 to k; * iterate through each component ;
	do i = 1 to n; * we want to find gamma zik for the ith digit in the kth component ;	
		denoms = J(k, 1, .); *to store the values that will be summed to form the denominator to compute gamma(zik) ; 
		data_i = data[i,]; *we are considering the ith digit, the outcome of each of the D Bernoullis, as a row vector ;
		prob_comp_k = ber(data_i, (means[,comp_k])`); 
		prob_k_mv = vec_mult(prob_comp_k); *prob_k_mv is the prob we will use in the numerator of the gamma(zik) calculation; 
		do mv_prob = 1 to k; *calculating k mv_probs (multivariate probabilities), therefore iterating k times ;
			probs = ber(data_i, (means[,mv_prob])`); *we are finding the probabilities of each the D Bernoulli outcomes, given the "old" means for the kth component (digit) ;
			mv_prob_k = vec_mult(probs);
			if mv_prob_k = 0 then mv_prob_k = 0.000001; *This is to prevent a zero in the denominator;
			denoms[mv_prob] = pis[mv_prob]#mv_prob_k; *the kth element of the denominator, all these will be summed together for the complete denom ;
		end;
		if prob_k_mv = 0 then prob_k_mv = 0.000001;
		zik_numerator = pis[comp_k]#prob_k_mv;
		*if zik_numerator = 0 then print comp_k i,, prob_k_mv;
		zik_denominator = sum(denoms);
	zik[i, comp_k] = zik_numerator/zik_denominator;
	*print zik_numerator,, zik_denominator;
	end;
end;

*print zik;

/*MAXIMISATION STEP*/

nks = J(k, 1, .); *calculating our effective sample size;
do i  = 1 to k;
	nks[i] = zik[+,i];
end;

*print nks;


mu1 = (data`*zik[,1])/nks[1];
zeros1 = loc(mu1=0);
mu1[zeros1] = 0.001;

mu2 = (data`*zik[,2])/nks[2];
zeros2 = loc(mu2=0);
mu2[zeros2] = 0.001;

mu3 = (data`*zik[,3])/nks[3];
zeros3 = loc(mu3=0);
mu3[zeros3] = 0.001;





pi1 = nks[1]/n;
pi2 = nks[2]/n;
pi3 = nks[3]/n;

pis = pi1//pi2//pi3;

digit_i = shape(mu1, 16, 16);
call HeatmapCont(digit_i);

digit_i = shape(mu2, 16, 16);
call HeatmapCont(digit_i);

digit_i = shape(mu3, 16, 16);
call HeatmapCont(digit_i);


means = mu1||mu2||mu3;

/*data1 = data[1,];*/
/*mean1 = means[,1];*/
/*print data1 mean1;*/
/**/
/*test = ber(data1, mean1`);*/
/*print test;*/
/**/
/**/
/*print pis means;*/

end;





quit;
