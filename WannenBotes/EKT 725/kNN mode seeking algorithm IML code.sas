proc iml;
use sasuser.mode1;
read all into x;
use sasuser.mode2;
read all into x2;

/*SPECIFYING PATH FOR PLOTS, AND PLOT QUALITY*/
ods listing gpath = "C:\Users\rianh\OneDrive - University of Pretoria\Documents\Rian 2020\Semester 2\EKT 725\Assignment 3\LateX Document" 
image_dpi=300;


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

/*DEFINE THE SUBROUTINE BELOW -->*/

start kNN_mode_clus;        

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
		maxi = mode_x; *This is the serial of the local max x value;
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


*This is very important, Here we find the true x values and the densities corresponding to the serials of the local maxima;
unique_modes = unique(modes)`;
indices = J(nrow(unique_modes), 1, .);
do i = 1 to nrow(unique_modes);
	indices[i] = loc(plot_data[, p + 2] = unique_modes[i]);
end;

/*UNIVARIATE CASE*/
if p = 1 then x1_modes = plot_data[indices, 1];
if p = 1 then densities_of_modes = plot_data[indices, 2];

/*BIVARIATE CASE*/
if p = 2 then x1_modes = plot_data[indices, 1];
if p = 2 then x2_modes = plot_data[indices, 2];
if p = 2 then densities_of_modes = plot_data[indices, 3];

/*THE VARIABLES*/
x1 = data[, 1];
if p = 2 then x2 = data[, 2];

/*CREATING A SAS DATASET FOR PLOTS*/
if p = 1 then create solution var {x1, fx_h, x1_modes, densities_of_modes, modes};
if p = 2 then create solution var {x1, x2, fx_h, x1_modes, x2_modes, densities_of_modes, modes};
append;

finish;


*;***CHANGE INPUTS HERE***;
*;  data = x2;           *;
*;  k_density = 200;     *;
*;  k_mode_seek = 200;   *; 
*;  plots = 1;           *;
*;  prints = 0;          *; 
*;************************;

/*CALL THE SUBROUTINE HERE -->*/

call kNN_mode_clus;

quit;


/*SORTING THE DATA FOR PLOTS*/
proc sort data = solution out = solution_sorted;
	by x1;
run;


***************************;
/*UNIVARIATE RESULT PLOTS*/
***************************;

/*The modes plotted on the density*/
proc sgplot data = solution_sorted;
	series x = x1 y = fx_h;
	scatter x = x1_modes y = densities_of_modes / markerattrs =  (symbol = circlefilled color = red);
run;

/*The clustering solution plot*/
proc sgplot data = solution_sorted;
	scatter x = x1 y = fx_h / group = modes;
run;


***************************;
/*BIVARIATE RESULT PLOTS*/ 
***************************;

/*Plot of density estimates against the first x variable, with modes marked in red*/
proc sgplot data = solution_sorted;
	series x = x1 y = fx_h;
	scatter x = x1_modes y = densities_of_modes / markerattrs =  (symbol = circlefilled color = red);
run;

/*Plot of density estimates against the second x variable, with modes marked in red*/
proc sgplot data = solution_sorted;
	scatter x = x1 y = fx_h;
	scatter x = x2_modes y = densities_of_modes / markerattrs =  (symbol = circlefilled color = red)
run;

/*Plot of bivaraite clustering solution*/
proc sgplot data = solution_sorted;
	scatter x = x1 y = x2 / group = modes;
run;

/*3-Dimensional density plot for bivaraite case*/
proc g3d data = solution_sorted;
	scatter x1*x2 = fx_h / shape="pillar";
run;





