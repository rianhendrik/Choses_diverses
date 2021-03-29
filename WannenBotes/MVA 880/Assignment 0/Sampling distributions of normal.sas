proc iml;


*Question 1a

call randseed(1);
N = 6000;
Mean = {10};
Cov = {9};
sample_size = 30;
sample_n = 200;
pi = constant("pi");

*Generating a N*p (p = 1) vector of random N(10,9) variables;
x = RandNormal( N, Mean, Cov ); 
*Reshaping vector to obtain 200 samples of size 30 each;
samples = shape(x, sample_size, sample_n);
*Obtaining the sample mean of each sample;
sample_means = t(samples[+,]/sample_size);
*Here we compute the mean of our sampling distribution;
mean_of_sampling_dist = mean(sample_means); print mean_of_sampling_dist Mean;
std_of_sampling_dist = std(sample_means); sigma = 3/sqrt(sample_size); print std_of_sampling_dist sigma;
*We see that the sampling distribution of X_bar has an expected value of +-10.
*If we increase our sample size n, we will obtain values closer to 10, which is
the true (theoretical) expected value of the sampling distribution of X_bar.
By the central limit theorom however, a sample size of 30 is enough. However, we do not
need the central limit theorom here, because the parent distribution from which we are 
sampling IS a normal distribution! In fact,
even even if the population is binomial, and min(np, n(1-p))> 5, we do not need a sample size
of 30 or larger for the CLT to apply.;

*Question 1b;
call sort(x); *Very important to sort data in ascending order first!;
bins = 50;
bw = x[N] - x[1];
i_s = bw/bins ; *interval size;
CDF_prob_e = J(bins, 1, .); support = J(bins, 1, .);
curr_bin = x[1] + i_s;
*For theoretical CDF;
CDF_prob_t = J(bins, 1, .);
start Theo_pdf(x);
   return((1/(3#sqrt(2#3.14)))#exp(-0.5#((x - 10)/3)##2)); *Numerically integrating the PDF of N(10,9);
finish;
*The loop;
do i = 1 to bins;
	*computing theoretical CDF values;
	call quad(R, "Theo_pdf", x[1]||curr_bin);
	CDF_prob_t[i] = R;
	curr_bin = curr_bin + i_s;
	vec_loc = loc(x < curr_bin);
	CDF_prob_e[i] = ncol(vec_loc)/N;
	support[i] = curr_bin;
end;

print CDF_prob_e CDF_prob_t support;


title "Emperical CDF from 6000 samples of a N(10,9) parent distribution";
*run Series(support, CDF_prob_e);
g = t(repeat({¨Theo¨, ¨Emp¨}, 1, bins));
CDF_probs = CDF_prob_t || CDF_prob_e;
x = support || support;
run Series(x, CDF_probs) group = g;


/* It is known that the 95% confidence interval for xbar is (8.92648, 11.0735).  */

xbars = sample_means;
print xbars;
moe = quantile('normal', 0.975, 0, 1)*std(xbars);
ub = mean+moe; lb = mean-moe;

higher = loc(xbars>ub);
lower = loc(xbars<lb);
oob = ncol(higher)+ncol(lower);
pooob = oob/nrow(xbars);
print pooob;

	
	



