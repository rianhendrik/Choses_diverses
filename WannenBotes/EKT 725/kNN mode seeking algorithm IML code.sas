


proc iml;
use sasuser.mode1;
read all into x;
use sasuser.mode2;
read all into x2;

******************************************************************************************************;
/*WHAT THIS SUBROUTINE DOES - README!!!*/
/**/
/*This subroutine outputs a dataset with the clustering solution.*/
/*It gives the local modes, as well as their kNN densities. */
/*This dataset can be used to produce visual representations of the final clustering solutions.*/
/**/
/*In order for the subroutine to work, provide the following:*/
/*1. Give as inputs the matrix of input values (column vectors of xs);*/
/*2. Provide the algorithm witht he desired value(s) of K*/
/*3. Specify whether plots should be created, and printing of results should be made*/
*******************************************************************************************************;


start kNN_mode_clus(data, k_density, k_mode_seek, plots, prints);

/*PLOT THE DATA*/
p = ncol(data);
n = nrow(data);
if plots = 1 then if p = 1 then call Histogram(data);
if plots = 1 then if p = 2 then call Scatter(data[, 1], data[, 2]);

/*ESTIMATING THE DENSITY*/
n = nrow(data);
serials = (1:n)`;
d = distance(data);
fx_h = J(n, 1, .);
do i = 1 to n;
	d_i = d[, i];
	call sort(d_i);
	fx_h[i] = 1/d_i[k_density];
end;

/*PLOT f(x)_hat*/
plot_data = data||fx_h||serials;
call sort(plot_data, {1}); /*Sorting the x values in increasing order for a plot */
if plots = 1 then if p = 1 then call Series (plot_data[, 1], plot_data[, 2]);

/*GRADIENT ASCENT*/
modes = J(n, 1, .); /*Each x value will have a corresponding serial (index) number of the mode to which it belongs*/
fx_h_modes = J(n, 1, .); /*We also want the density values at these modes, to plot them over the density estimate*/
do i = 1 to n;
	mat = d[, i]||fx_h||serials;
	call sort(mat, {1});
	maxi = 0;
	window = mat[1:k_mode_seek, ];
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
	*true_x_serials = plot_data[, 3]; 
	loc_of_max = loc(plot_data[, p + 2] = mode_x); *This is the true location (index) of the mode;
	fx_h_modes[i] = plot_data[loc_of_max, p + 1]; *This should thus be the correct density based on the right location;
end;


/*GATHERING THE RESULTS*/
basins = data||modes;
call sort(basins, {1});	
the_modes = (unique(data[modes,]))`;
nclus = nrow(the_modes);
mode_densities = (unique(fx_h_modes))`;
call sort(mode_densities, {1}, 1);
if prints = 1 then print nclus;
if prints = 1 then print basins fx_h;
if prints = 1 then print the_modes mode_densities;
x1  = data[, 1];
if p = 2 then x2 = data[, 2];

/*ALTERNATIVE MEANS TO OUTPUT THE DATA*/
*output = data || fx_h || the_modes || mode_densities;
*return(nclus);
*cn = {"x1", "fx_h", "the_modes", "mode_densities"};
*if p = 2 then cn = {"x1", "x2", "fx_h", "the_modes", "mode_densities"};
*create plot_data from output[colname = cn]; /** create data set **/
*append from output;       /** write data in vectors **/
/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/**/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/;

if p = 1 then create plot_data var {x1, fx_h, modes, the_modes, mode_densities};
if p = 2 then create plot_data var {x1, x2, fx_h, modes, the_modes, mode_densities};
append;
close;

finish;

call kNN_mode_clus(x, 600, 600, 1, 1);


/*ods listing gpath = "C:\Users\rianh\OneDrive - University of Pretoria\Documents\Rian 2020\Semester 2\EKT 725\Assignment 3\LateX Document" */
/*image_dpi=300;*/

proc print data = plot_data;
run;


/*FINAL PLOTS*/

/*UNIVARIATE CASE PLOT*/
proc sort data = plot_data out = new_x1;
	by x1;
run;

proc sgplot data=new_x1;
      series x=x1 y=fx_h;
	  scatter x = the_modes y = mode_densities / markerattrs =  (symbol = circlefilled color = red);
run;


/*BIVARIATE CASE PLOTS*/

proc g3d data = new_x2
	annotate = new_x2;
	scatter x1*x2 = fx_h / shape="pillar";
run;


proc sgplot data = plot_data;
	scatter x = x1 y = x2 / group = modes;
run;

