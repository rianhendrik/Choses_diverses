


proc iml;
use sasuser.mode2;
read all into x;

call Scatter(x[, 1], x[, 2]);

start kNN_mode_clus(data, k_density, k_mode_seek);

/*PLOT THE DATA*/
p = ncol(data);
n = nrow(data);
if p = 1 then call Histogram(data);
if p = 2 then call Scatter(data[, 1], data[, 2]);

/*ESTIMATING THE DENSITY*/
n = nrow(data);
serials = (1:n)`;
x_s = data||serials;
d = distance(data);
fx_h = J(n, 1, .);
do i = 1 to n;
	d_i = d[, i];
	call sort(d_i);
	fx_h[i] = 1/d_i[k_density];
end;

/*PLOT f(x)_hat*/
plot_data = data||fx_h;
call sort(plot_data, {1});
if p = 1 then call Series (plot_data[, 1], plot_data[, 2]);

/*GRADIENT ASCENT*/
modes = J(n, 1, .);
do i = 1 to n;
	mat = d[, i]||fx_h||serials;
	call sort(mat, {1});
	maxi = 0;
	window = mat[1:k_mode_seek,];
	loc_max_f = window[<:>, 2];
	mode_x = mat[loc_max_f, 3];
	do while (mode_x ^= maxi);
		maxi = mode_x; *This is the index of the local max x value;
		mat2 = d[, maxi]||fx_h||serials;
		call sort(mat2, {1});
		window2 = mat2[1:k_mode_seek,];
		loc_max_f2 = window2[<:>, 2];
		mode_x = mat2[loc_max_f2, 3];
	end;
	modes[i] = mode_x;
end;


/*GATHERING THE RESULTS*/
basins = data||modes;
call sort(basins, {1});	
the_modes = (unique(data[modes,]))`;
nclus = nrow(the_modes);
mode_densities = ((fx_h[the_modes,]));
call sort(mode_densities, {1}, 1);
print nclus;
print basins fx_h;
print the_modes mode_densities;
output = data || fx_h;

cn = {"x1", "fx_h"};
if p = 2 then cn = {"x1", "x2", "fx_h"};
create plot_data from output[colname = cn]; /** create data set **/
append from output;       /** write data in vectors **/
close;

finish;

run kNN_mode_clus(x, 200, 200);

proc sort data = plot_data out = new;
	by x1;
run;

proc sgplot data=new;
      series x=x1 y=fx_h;
	  series x=x2 y=fx_h;
run;

proc sort data = new;
	by x2;
run;

proc sgplot data = new;
	series x2;
run;

proc print data = new;
run;
 




proc g3d data = new
	annotate = new;
	plot x1*x2 = fx_h;
run;
