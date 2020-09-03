


data npr;
set sasuser.nps2018;
run;

/*proc gplot data = npr;*/
/*	plot y*x;*/
/*run;*/


proc iml;
use npr;
read all into x_y;
n = nrow(x_y);

xx = x_y[, 1];
yy = x_y[, 2];



/*LET SPAN = 0.1*/
s = 0.1;
m = s*n;


start wls;
p = ncol(x);
b = inv(x`#w`*x)*x`#w`*y;
yhat_i = b[1] + x0*b[2];
yhats = x*b;
x_bar = sum(x)/m;
num = (x0 - x_bar)**2;
x_cent = x - x_bar;
den = ssq(x_cent);
res = y - yhats;
MSE = ssq(res)/(n - p);
root_MSE = sqrt(MSE);
var_yhat = MSE*((1/m) + num/den);
stde_yhat = sqrt(var_yhat);
t_crit = tinv(0.975, m - p);
moe_m = t_crit*stde_yhat;
moe_i = t_crit*(root_MSE + stde_yhat);
finish wls;

start wls_d;
p = ncol(x);
b = inv(x`*w*x)*x`*w`*y;
yhat_i = b[1] + x0*b[2];
yhats = x*b;
x_bar = sum(x)/m;
num = (x0 - x_bar)**2;
x_cent = x - x_bar;
den = ssq(x_cent);
res = y - yhats;
MSE = ssq(res)/(n - p);
var_yhat = MSE*((1/m) + num/den);
stde_yhat = sqrt(var_yhat);
t_crit = tinv(0.975, m - p);
moe = t_crit*stde_yhat;
finish wls_d;


/*TEST FOR WLS SUBROUTINE*/

/*x0 = 300;*/
/*x = J(n, 1, 1)||xx;*/
/*y = yy;*/
/*w = J(n, 1, 1);*/
/*w = diag(w);*/

/*call wls;*/
/*call wls_d;*/






t0 = time();
y_hat = J(n, 1, .);
moes_m = J(n, 1, .);
moes_i = J(n, 1, .);
do i = 1 to n;    *This loop is to index through each x value in the dataset;
	x0 = x_y[i, 1];    *Selects our ith focal value;
	x_c = abs(x_y[, 1] - x0);
	D = x_y||x_c;
	call sort(D, {3});
	x_w = D[1:m, 1];   *These are the nearest neighbour xs which fall in the window (spread to the left and right of the focal point).;
	x_w_sort = x_w;    *Sorting the x_ws in order to calculate  h, the half range of the window.;
	call sort(x_w_sort);
	x_c_i = D[1:m, 3];
	h = (x_w_sort[m] - x_w_sort[1])/2;
	Wt_x = J(m, 1, .);
	do k = 1 to m;
		Wt_x[k] = (1 - (x_c_i[k]/h)**3)**3;
		if (x_c_i[k]/h) < 1 then Wt_x[k] = Wt_x[k];
		if (x_c_i[k]/h) >= 1 then Wt_x[k] = 0;
	end;
	w = Wt_x;
	x = J(m, 1, 1)||x_w;
	y = D[1:m, 2];
	call wls;
	y_hat[i] = yhat_i;
	moes_m[i] = moe_m;
	moes_i[i] = moe_i;
end;
T1 = time() - t0;

print T1;

/*DIAGONAL WEIGHT MATRIX*/

/*t0 = time();*/
/*y_hat = J(n, 1, .);*/
/*moes = J(n, 1, .);*/
/*do i = 1 to 1000;    *This loop is to index through each x value in the dataset;*/
/*	x0 = x_y[i, 1];    *Selects our ith focal value;*/
/*	x_c = abs(x_y[, 1] - x0);*/
/*	D = x_y||x_c;*/
/*	call sort(D, {3});*/
/*	x_w = D[1:m, 1];   *These are the nearest neighbour xs which fall in the window (spread to the left and right of the focal point).;*/
/*	x_w_sort = x_w;    *Sorting the x_ws in order to calculate  h, the half range of the window.;*/
/*	call sort(x_w_sort);*/
/*	x_c_i = D[1:m, 3];*/
/*	h = (x_w_sort[m] - x_w_sort[1])/2;*/
/*	Wt_x = J(m, 1, .);*/
/*	do k = 1 to m;*/
/*		Wt_x[k] = (1 - (x_c_i[k]/h)**3)**3;*/
/*		if (x_c_i[k]/h) < 1 then Wt_x[k] = Wt_x[k];*/
/*		if (x_c_i[k]/h) >= 1 then Wt_x[k] = 0;*/
/*	end;*/
/*	w = Wt_x;*/
/*	w = diag(w);*/
/*	x = J(m, 1, 1)||x_w;*/
/*	y = D[1:m, 2];*/
/*	call wls_d;*/
/*	y_hat[i] = yhat_i;*/
/*	moes[i] = moe;*/
/*end;*/
/*T2 = time() - t0;*/
/**/
/*print T2;*/

upper_m_l = y_hat + moes_m;
lower_m_l = y_hat - moes_m;

upper_i_l = y_hat + moes_i;
lower_i_l = y_hat - moes_i;

data = x_y||y_hat||lower_m_l||upper_m_l||lower_i_l||upper_i_l;

print data;

create nonparreg from data[colname = {'x', 'y', 'yhat', 'lower_m', 'upper_m'}];
append from data;
close nonparreg;

proc print data = nonparreg;
run;

goptions reset = all;

proc gplot data = nonparreg;
	plot yhat*x;
	plot y*x;
run;

proc export data=nonparreg dbms=xlsx

outfile="C:\Users\rianh\OneDrive - University of Pretoria\Documents\Rian 2020\Semester 2\EKT 720\Assignments\Assignment 4\nonpar.xlsx"

replace;

run;











	
