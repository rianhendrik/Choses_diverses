
dm 'odsresults;clear';
options ls=72 nodate pageno=1 nocenter; 



libname MVA880 "C:\Users\rianh\Dropbox\MVA880\MVA880_lib" ;

ods listing gpath = "C:\Users\rianh\Dropbox\MVA880\Assignment 5\LateX document"
image_dpi=300;



proc iml;
use mva880.mode2;
read all into data;
data = data[,2:3];
orig_data = data;





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

k = 200; 
call kNN_mode_clus;
funky = data; *Whatever my dataset is in the test, I will change its name to funky here.;

nsim = 100;
ks = do(50, 300, 1)`; 
prob = 0.6; n_full = nrow(funky);
funky = funky||(1:n_full)`; *Adding unique serial numbers to each of the xs;
I = J(n_full, n_full, 0); S = J(n_full, n_full, 0);

do iter = 1 to nsim;
	*prob = 0.6;
	*n_full = 501;
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


*subscript getting subroutine - compliments of Rick Wicklin;
start ind2sub(p, ind );
   idx = colvec(ind);
   n = nrow(idx);
   col = 1 + mod(idx-1, p);
   row = 1 + (idx-col) / p;
   return ( row || col );
finish;

*diagonal assigning subroutine - compliments of Rick Wicklin;
start SetDiag(A, v); *A is the matrix, and v is the value you would like the diagonals to assume;
   diagIdx = do(1,nrow(A)*ncol(A), ncol(A)+1);
   A[diagIdx] = v;             /* set diagonal elements */	
finish;


dmat = orig_data;

clusts = (1:nrow(dmat))`||J(nrow(dmat), nrow(dmat)-1, 0); *Initial cluster allocation. Each observation is in its own cluster;
nclusts = nrow(clusts);
dist = distance(dmat);
dim = 2;

print dmat; print clusts;

obss = {0 0}; *two-tuple, becuase we are working with a bivariate dataset.;
print = 0;


dist = dissim; *our distance matrix for hierarchical is the dissimilarity matrix;
k = 2; *number of clusters;
do while(nclusts>k); *I want to end up with a k cluster solution. Therefore, do until nclusts = k;


call SetDiag(dist, 90000); *set diagonal to a large distance, else the diagonoals of 0 will also be the min;
ind = dist[>:<]; *The diagonal of this distance matrix must be larger than the max in distance matrix, else dist[>:<] will return a diagonal.;
obs = ind2sub(ncol(dist), ind); *obs - cluster result: the result of this iteration of clustering. obs == the two obs clustered in this iteration;
if print = 1 then print obs; *gives subscript (column row) of the minimum (matrix symmetric so order does not matter);

obss = obss//obs;
*remove one of those observations from the distance matrix, as it is no longer needed (it is part of a cluster with another observation(s);

join = dmat[obs[1],]//dmat[obs[2],]; *These two observations are joining each other in a cluster;

dim = nrow(dist);
ln = 90909090; *ln - large number;
if max(obs) = dim then do;
	dist = dist[1:max(obs)-1,];
	dist = dist[,1:max(obs)-1];
end;

else do;
	top = dist[1:max(obs)-1,]; *All rows before the row (observation) that we are removing;
	bot = dist[max(obs)+1:dim,]; *All rows after the row (observation) that we are removing;
	dist = top//bot;
	left = dist[,1:max(obs)-1];
	right = dist[,max(obs)+1:dim];
	dist = left||right;
end;

dist[min(obs), min(obs)] = ln;
ndist = nrow(dist); pdist = ncol(dist);
if print = 1 then print ndist pdist;


ind1 = loc(clusts[max(obs),]>0);*find out indices where max(res) is not zero;
ind2 = loc(clusts[min(obs),]=0);
sn = ncol(ind1); *sn - space needed;
ind2 = ind2[1:sn]; *we only need the first 1:sn to be replaced by the observations from the new observation/cluster;
clusts[min(obs), ind2] = clusts[max(obs), ind1]; *the replacement;
if max(obs) = nrow(clusts) then do;
	clusts = clusts[1:max(obs)-1,];
end;
else do;
	top = clusts[1:max(obs)-1,]; *All rows before the row (observation) we are removing;
	bot = clusts[max(obs)+1:nclusts,]; *All rows after the row (observation) we are removing;
	clusts = top//bot;
end;

nclusts = nrow(clusts);

if print = 1 then print clusts;

end;
	

not_zero1 = loc(clusts[1,] > 0);
not_zero2 = loc(clusts[2,] > 0);
clus1_indx = clusts[1,not_zero1];
clus2_indx = clusts[2,not_zero2];

clus1_data = orig_data[clus1_indx,];
clus1_data = clus1_data||J(nrow(clus1_data), 1, 1);
clus2_data = orig_data[clus2_indx,];
clus2_data = clus2_data||J(nrow(clus2_data), 1, 2);
clus_solution = clus1_data//clus2_data;
*print clus_solution;

call  Scatter(clus_solution[,1], clus_solution[,2]) group = clus_solution[,3];






create dissim_data from dissim;
append from dissim;

data a (type = distance);
set dissim_data;
run;

proc cluster data = a method = average outtree = b ;
run;

proc tree noprint ncl = 2 out = out;
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


