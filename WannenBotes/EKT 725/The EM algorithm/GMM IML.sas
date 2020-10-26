data em3;
infile "C:\Users\rianh\OneDrive - University of Pretoria\Documents\Rian 2020\Semester 2\EKT 725\Assignment 5\Data\m3.txt";
input x;
run;


proc iml;
use em3;
read all into x;


/*kmeans results from R*/
mu1 = 152.9469;
mu2 = 94.40457;
mu3 = 220.0142;

sigma1 = 14.8809;
sigma2 = 12.09309;
sigma3 = 19.76735;

pi1 = 0.4866667;
pi2 = 0.1;
pi3 = 0.4133333;


q = 0;

q = q // sum(log(pi1) + log(pdf('normal', x, mu1, sigma1))) + sum(log(pi2) + sum(pdf('normal', x, mu2, sigma2))) + sum(log(pi3) + log(pdf('normal', x, mu3, sigma3)));

print q;


k = 2;

print q;

tol = 0.00001;

do while (abs(q[k] - q[k - 1]) >= tol);
	
/*	E Step*/
	comp1 = pi1*pdf('normal', x, mu1, sigma1);
	comp2 = pi2*pdf('normal', x, mu2, sigma2);
	comp3 = pi3*pdf('normal', x, mu3, sigma3);
	comp_sum = comp1 + comp2 + comp3;

	gamma1 = comp1/comp_sum;
	gamma2 = comp2/comp_sum;
	gamma3 = comp3/comp_sum;
	
/*	M Step*/
	pi1 = sum(gamma1)/nrow(x);
	pi2 = sum(gamma2)/nrow(x);
	pi3 = sum(gamma3)/nrow(x);
	
	mu1 = sum(gamma1#x)/sum(gamma1);
	mu2 = sum(gamma2#x)/sum(gamma2);
	mu3 = sum(gamma3#x)/sum(gamma3);

	sigma1 = sqrt(sum(gamma1#(x - mu1)##2/sum(gamma1)));
	sigma2 = sqrt(sum(gamma2#(x - mu2)##2/sum(gamma2)));
	sigma3 = sqrt(sum(gamma3#(x - mu3)##2/sum(gamma3)));
	
	k = k + 1;
	q = q // sum(log(comp_sum));

end;

print k,, mu1 mu2 mu3,, sigma1 sigma2 sigma3;



/*QUESTION 2*/

libname ekt "C:\Users\rianh\OneDrive - University of Pretoria\Documents\Rian 2020\Semester 2\EKT 720\data";

proc iml;
use ekt.mix4;
read all into x;

truncated = 0;
do i = 1 to nrow(x);
	if x[i] < 5000 then truncated = truncated // x[i];
end;

mu_t = mean(truncated);

print mu_t;


mu = 1233.420;

sigma = 1233.420;

pi = 1;


q = 0;

q = q // sum(log(pi) + log(pdf('normal', x, mu, sigma)));

print q;


k = 2;

tol = 0.00001;

do while (abs(q[k] - q[k - 1]) >= tol);
	
/*	E Step*/
	comp = pi*pdf('normal', x, mu, sigma);
	comp_sum = comp;

	gamma = comp/comp_sum;
	
/*	M Step*/
	pi = sum(gamma)/nrow(x);
	
	mu = sum(gamma#x)/sum(gamma);

	sigma = sqrt(sum(gamma#(x - mu)##2/sum(gamma)));

	
	k = k + 1;
	q = q // sum(log(comp_sum));

end;

print k,, mu,, sigma;
