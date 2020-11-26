quit;
dm 'odsresults;clear';
options ls=72 nodate pageno=1 nocenter; 



data data;
set ???;
run;

/*proc sgplot data = data;*/
/*	scatter x = x1 y = x2;*/
/*run;*/

proc iml;
use sasuser.fivegaussians; *change this for test;
read all into data;

/*c1 = J(600, 1, 1);*/
/*c2 = J(632, 1, 2);*/
/*c3 = J(568, 1, 3);*/
/**/
/*clusts = c1 // c2 // c3;*/
/**/
/*run Scatter(data[, 1], data[, 2]) group = clusts;*/

/*THE kNN SUBROUTINE*/

start kNN_mode_clus;        


/*ESTIMATING THE DENSITY*/
n = nrow(data);
p = ncol(data);
*serials = (1:n)`; *since we are sampling, these serials must not reset the actual serials attached to data points in the sample.;
knn_serials = (1:n)`;
d = distance(data);
fx_h = J(n, 1, .);
do e = 1 to n;
	d_i = d[, e];
	call sort(d_i);
	fx_h[e] = 1/d_i[k];
end;

control_data = data||knn_serials||fx_h;



/*GRADIENT ASCENT*/
modes_knn = J(n, 1, .); /*Each x value will have a corresponding serial (index) number of the mode to which it belongs*/
modes_true = J(n, 1, .);
fx_h_modes = J(n, 1, .); /*We also want the density values at these modes, to plot them over the density estimate*/
do e = 1 to n;
	mat = d[, e]||fx_h||knn_serials||data[, p];
	call sort(mat, {1});
	maxi = 0;
	window = mat[1:k, ];
	loc_max_f = window[<:>, 2];
	mode_x_knn = mat[loc_max_f, 3];
	true_mode_x = mat[loc_max_f, 4];
	do while (mode_x_knn ^= maxi);
		maxi = mode_x_knn; *This is the serial of the local max x value;
		mat2 = d[, maxi]||fx_h||knn_serials||data[, p];
		call sort(mat2, {1});
		window2 = mat2[1:k, ];
		loc_max_f2 = window2[<:>, 2];
		mode_x_knn = mat2[loc_max_f2, 3];
		true_mode_x = mat2[loc_max_f2, 4];
	end;
	modes_knn[e] = mode_x_knn; *modes according to the knn_serials;
	modes_true[e] = true_mode_x;
	fx_h_modes[e] = mat2[loc_max_f2, 2];
end;
results = data[, p]||modes_true||data[, 1:p - 1];

finish;

funky = data; *Whatever my dataset is in the test, I will change its name to funky here.;

nsim = 100;
ks = do(50, 90, 1)`; 
prob = 0.6; n_full = nrow(funky);
funky = funky||(1:n_full)`; *Adding unique serial numbers to each of the xs;
I = J(n_full, n_full, 0); S = J(n_full, n_full, 0);

do iter = 1 to nsim;
	prob = 0.6;
	n_full = 501;
	k = sample(ks, 1)`;
	obs = (1:n_full)`;
	samp_obs = sample(obs, prob*n_full, 'WOR')`;
	data = funky[samp_obs, ]; *Note that funky now consists of the data, and serials;
	call kNN_mode_clus;
	do ii = 1 to nrow(results);
		do j = 1 to nrow(results);
		I[results[ii, 1], results[j, 1]] = I[results[ii, 1], results[j, 1]] + 1;
		if results[ii, 2] = results[j, 2] then S[results[ii, 1], results[j, 1]] = S[results[ii, 1], results[j, 1]] + 1;
		end;
	end;
end;

C = S/I;
dissim = 1 - C;
test = dissim[, 1];
print test;

create dissim_data from dissim;
append from dissim;

data a (type = distance);
set dissim_data;
run;

proc cluster data = a method = average outtree = b ;
run;

proc tree noprint ncl = 5 out = out;
run;

proc iml;
use out; read all into final_solution;
use sasuser.funky; read all into data;
plot_d = data || final_solution;

run Scatter(plot_d[, 1], plot_d[, 2]) group = plot_d[, 3];






/*Toy Example*/
/**/
/*m = {1 2, 2 2, 4 1}; *First column represents the observations in the sample, and the second column represents the observation which is the mode of the cluster each of the observations belong to.;*/
/*I = J(5, 5, 0);*/
/*S = J(5, 5, 0);*/
/*print I S;*/
/*do ii = 1 to nrow(m);*/
/*	do j = 1 to nrow(m);*/
/*	I[m[ii, 1], m[j, 1]] = I[m[ii, 1], m[j, 1]] + 1;*/
/*	if m[ii, 2] = m[j, 2] then S[m[ii, 1], m[j, 1]] = S[m[ii, 1], m[j, 1]] + 1;*/
/*	end;*/
/*end;*/
/**/
/*print I S;*/

