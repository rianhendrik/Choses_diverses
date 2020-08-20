options nocenter ;
ods html close;
ods html;
data a;
input income1 income2 house stand double prop;
joint_income = income1 + income2;
ratio = house/stand;
datalines;
  521.502    118.348    735.779     920.53      0     1215.86
   14.116    457.801    413.522     690.15      0      917.67
  308.237    205.341    567.238     903.11      1     1201.16
  449.589    157.470    496.226     659.05      1     1099.73
   47.286    555.871    414.292     769.92      0     1077.25
   12.702    400.744    283.223     539.62      1      973.28
  303.539    360.630    671.121     934.58      0     1206.01
  325.548    369.610    284.473     707.85      0     1071.03
  328.079     17.192    492.552     699.23      0      834.60
  479.735     34.212    767.408    1097.32      0     1102.11
   70.381    319.148    373.140     760.19      0      774.12
  232.232    255.517    238.515     577.39      0      828.64
   56.125    326.705    589.865     930.42      0     1020.10
  510.569     36.773    461.059     920.65      0     1044.39
   15.890    353.851    345.385     655.05      1      932.65
  298.906    126.398    531.592    1093.24      0     1036.68
  280.401    105.089    497.296     727.87      0      867.00
  188.411    419.229    383.097     903.32      0     1114.49
   11.004    462.602    351.969     575.44      0      833.46
  408.952    119.757    650.882     950.26      0     1044.39
  114.999    253.868    439.853     849.32      0      831.60
  200.932    141.234    400.907     571.64      0      773.70
  276.907    350.366    554.191     948.33      1     1273.24
  271.076    109.235    734.862     970.72      1     1124.66
  357.141    324.151    507.147     686.02      0     1146.86
   74.029    403.535    372.881     520.79      0      836.79
  112.752    195.755    550.987    1048.71      0     1023.95
  189.496    273.100    400.458     550.31      0      834.02
  283.516    395.697    445.404     600.35      0     1064.84
  255.701    154.743    535.123    1078.51      0     1075.30
  ;
run ;

data b;
set a;
keep prop joint_income stand ratio double;
run;


*ASSIGNMENT 3*;


proc iml;

start reg ;
k=ncol(x) ;
bh=inv(x`*x)*x`*y ;
res=y-x*bh ;
ess=res`*res ;
tss=(y-y[:,])[##,] ;
rss=tss-ess ;
df_m = ncol(x)-1 ;
df_e = n-ncol(x) ;
df_t = n-1 ;
ms_m = rss/df_m ;
ms_e = ess/df_e ;
f=ms_m/ms_e ;
p_f = 1-probf(f,df_m,df_e) ;

rmse = sqrt(ms_e) ;
meany = y[:,] ;
cv=rmse/meany*100 ;

r2=rss/tss ;
ar2 = 1 - ((1-r2)*((n-1)/(n-k))) ;

seb=sqrt(vecdiag(ms_e*inv(x`*x))) ;
t=bh/seb ;
p_t = 2*(1-probt(abs(t),df_e)) ;

cll = bh - tinv(0.975,df_e)*seb ;
clu = bh + tinv(0.975,df_e)*seb ;

finish reg ;

*3a - estimating the original, full model with all observations;

use b;
read all into x_and_y;

n = nrow(x_and_y);
one = J(n, 1, 1);
y1 = x_and_y[,3];
x_1 = one;
x_2 = x_and_y[,4];
x_3 = x_and_y[,1];
x_4 = x_and_y[,5];
x_5 = x_and_y[,2];
x1 = x_1||x_2||x_3||x_4||x_5;

x = x1;
y = y1;

call reg;
print "Initial regression"  bh r2 cll clu;

*3b-c: 'a leave on out approach to regression';
*Easy loop explained in class;
obs = (1:n) ;

beta1 = J(1, 30) ;
beta2 = J(1, 30) ;
beta3 = J(1, 30) ;
beta4 = J(1, 30) ;
beta5 = J(1, 30) ;

do i = 1 to n ; 
	obs_s = loc(obs^=i) ;

	y = y1[obs_s,] ;
	x = x1[obs_s,] ;

	call reg ;
	b1 = bh[1] ;
	b2 = bh[2] ;
	b3 = bh[3] ;
	b4 = bh[4] ;
	b5 = bh[5] ;
	beta1[i] = b1 ;
	beta2[i] = b2 ;
	beta3[i] = b3 ;
	beta4[i] = b4 ;
	beta5[i] = b5 ;
end ;

create beta_table var {beta1 beta2 beta3 beta4 beta5} ;
append;
close beta_table;

*3d: Taking a single bootstrap sample from n, and running a regression (random seed set as 0);

one_sample = sample(obs, n);
y = y1[one_sample,];
x = x1[one_sample,];

call reg;
print 'betas from single bootstrap sample' bh;

*3e: Bootstrapping, and running regressions on 100 bootstrapped samples.

*The loop for bootstrap sampling, and storing the betas obtained from regressions on tehse samples;
beta1 = J(1, 100) ;
beta2 = J(1, 100) ;
beta3 = J(1, 100) ;
beta4 = J(1, 100) ;
beta5 = J(1, 100) ;

samples = 100;
n = 30;

do i = 1 to samples;
	s = sample(obs, 30);
	y = y1[s,];
	x = x1[s,];
	call reg;
	b1 = bh[1] ;
	b2 = bh[2] ;
	b3 = bh[3] ;
	b4 = bh[4] ;
	b5 = bh[5] ;
	beta1[i] = b1 ;
	beta2[i] = b2 ;
	beta3[i] = b3 ;
	beta4[i] = b4 ;
	beta5[i] = b5 ;
end;

beta1_1 = beta1[,1:25];
beta2_1 = beta2[,1:25];
beta3_1 = beta3[,1:25];
beta4_1 = beta4[,1:25];
beta5_1 = beta5[,1:25];

beta1_2 = beta1[,26:50];
beta2_2 = beta2[,26:50];
beta3_2 = beta3[,26:50];
beta4_2 = beta4[,26:50];
beta5_2 = beta5[,26:50];

beta1_3 = beta1[,51:75];
beta2_3 = beta2[,51:75];
beta3_3 = beta3[,51:75];
beta4_3 = beta4[,51:75];
beta5_3 = beta5[,51:75];

beta1_4 = beta1[,76:100];
beta2_4 = beta2[,76:100];
beta3_4 = beta3[,76:100];
beta4_4 = beta4[,76:100];
beta5_4 = beta5[,76:100];

create beta_sample_100_1 var {beta1_1 beta2_1 beta3_1 beta4_1 beta5_1};
append;
close beta_sample_100_1;

create beta_sample_100_2 var {beta1_2 beta2_2 beta3_2 beta4_2 beta5_2};
append;
close beta_sample_100_2;

create beta_sample_100_3 var {beta1_3 beta2_3 beta3_3 beta4_3 beta5_3};
append;
close beta_sample_100_3;

create beta_sample_100_4 var {beta1_4 beta2_4 beta3_4 beta4_4 beta5_4};
append;
close beta_sample_100_4;



create beta_sample_100 var {beta1 beta2 beta3 beta4 beta5} ;
append;
close beta_sample_100;


 

proc print data = beta_table; *Obtaining the betas table to be provided as results from 'leave one out' regressions.;
run;

proc print data = beta_sample_100_1; *Obtaining the betas table to be proveded as results from the regressions run on the first 25 of the 100 bootstrap samples;
run;

proc print data = beta_sample_100_2;
run;

proc print data = beta_sample_100_3;
run;

proc print data = beta_sample_100_4;
run;

*Further exploratory analysis performed in proc univariate on betas to substantiate comments on these observed betas;

proc univariate data = beta_table;
run;

proc univariate data = beta_sample_100;
run;






/*proc univariate data = beta_table normal;*/
/*histogram;*/
/*run;*/
/**/
/*proc univariate data = beta_sample_100 normal;*/
/*histogram;*/
/*run;*/



*Other loops which could be considered, but which are more complex than the one used;
*3b - using a 'leave one out' approach to regression.;
*Different loops that we could use:;
*do ii = 1 to nrow(x_and_y) ;
  *if ii=1 then do ;
    *y = y1[2:nrow(y1),] ;
    *x = x1[2:nrow(x1),] ;
  *end ;
  *if (ii>1)  then do ;
   *if  (ii < nrow(x_and_y)) then do ;
   *y = y1[1:ii-1,] // 
    *   y1[ii+1:nrow(y1),] ;
   *x = x1[1:ii-1,] // 
  *     x1[ii+1:nrow(x1),] ;
  * end;
  *end ;
 * if ii=nrow(x_and_y) then do ;
 *   y = y1[1:nrow(y1)-1,] ;
 *   x = x1[1:nrow(x1)-1,] ;
 * end ;
*call reg; *Will I have to include a 'call reg' in each loop? I believe so;
*print bh;
