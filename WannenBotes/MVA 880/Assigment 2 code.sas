quit ;
dm 'odsresults;clear';
options ls=72 nodate pageno=1 nocenter; 

libname MVA "C:\Users\rianh\Dropbox\MVA880\MVA_data";

data q1; set MVA.q1; run;
data q2; set MVA.q2; run;

/*proc print data =  q1; run;*/
/*proc print data = q2; run;*/


proc iml;
use q1;
read all into points1;
use q2;
read all into points2;

start kmeans_clus;

	n = nrow(points);
	d = ncol(points);
	conv = 0;
	count = 0;
	obs = 1:n; *sample the index of the observations for the initial values;
	if random = 'obs' then indices = sample(obs, k, 'WOR'); if random = 'obs' then inits = points[indices,];



	overall_mean = J(1, d, .); *here d denotes dimensions of the dataset;
	do i = 1 to d;
		overall_mean[,i] = points[+,i]/n;
	end;

*Total deviation;
	td_data = overall_mean // points;
	td_dists = distance(td_data)[, 1]; *We are only interested in the first column, the distance between the centroid and all the individual obervations;
	T = td_dists`*td_dists; *This is the sum of squared distances of each observation from the overall mean; 


*Assigning each observation to a centroid;
	Fs = 0; R2s = 0;
	do iter = 1 to 100 while (conv = 0);
		count = count + 1;
		clust_data = inits // points;
		clust_dist = distance(clust_data);
		int = clust_dist[, 1:k]; *int - interest, the part of the distance matrix we are interested in;
		resp = J(nrow(int), 1, .);
		do ii = i to nrow(int); *start at k, we are not clustering the k centroids.;
			focus = int[ii, ]; resp[ii] = focus[>:<]; *resp - responsibilities;
		end;
		resp_obs = resp[k + 1:nrow(resp),];

		new_centroids = J(k, d, .); wcd_k = J(k, 1, .);
		*print iter;
		do j = 1 to k;
			cluster_j = loc(resp_obs = j);
			len = ncol(cluster_j); *print len;
			points_j = points[cluster_j,];
			wcd_data = inits[j, ] // points_j; wcd_dist = distance(wcd_data)[, 1]; *We are onlt interested in the distane of each cluster centroid to the observations in its cluster;
			wcd_k[j] = wcd_dist`*wcd_dist;
			new_centroids[j, ] = points_j[+,]/nrow(points_j);
		end;
		W = sum(wcd_k); B = T - W; tt = W + B;
		c = k;
		r2 = B/T; F = (B/(k - 1))/(W/(n - k)); 
		R2s = R2s//r2; Fs = Fs//F;

		if new_centroids = inits then conv = 1; 
								 else conv = 0;
		if pr = 1 then print iter inits new_centroids ,, W B T tt ,, r2 F;
		inits = new_centroids;
	end;

/*	plot_data = points || resp_obs;*/
/*	run Scatter (plot_data[, 1], plot_data[, 2]) group = plot_data[, 3];*/
finish;


/*QUESTION 1a*/

/*points = points1*/
/*n = nrow(points);*/
/*d = ncol(points); *number of variables in dataset;*/
/*k = 4; *number of clusters we want;*/
/*random = 'no'; *random inital points selected from observations;*/
/*inits = {10 30, 20 40, 40 40}; *predetermined inital values 2D;*/
/*conv = 0; count = 0; pr = 0; *conv must be 0, and will become 1 if the centroids converge. Then, the while loop will stop.;*/
/*call kmeans_clus;*/
/*print f r2;*/


/*QUESTION 1c*/

/*k = 2;*/
/*random = 'obs';*/
/*nsim = 10000;*/
/*Fvec = J(nsim, 1, .); R2vec = J(nsim, 1, .); iterations = J(nsim, 1, .);*/
/*do q = 1 to nsim;*/
/*	call kmeans_clus;*/
/*	Fvec[q] = F; R2vec[q] = r2; iterations[q] = iter;*/
/*end;*/
/**/
/*print Fvec R2vec iterations;*/
/**/
/*mean_iterations = mean(iterations);*/
/*mean_f = mean(Fvec); mean_r2 = mean(R2vec);*/
/*print mean_iterations,, mean_f mean_r2;*/
/**/
/*create iteration_dat var {iterations}; append; close iteration_dat; /* write data */*/
/*submit;*/
/*  proc univariate data = iteration_dat;*/
/*	histogram / midpoints = (3 to 15 by 1) odstitle = "Distribution of the number of iterations before convergence";*/
/*  run;*/
/*endsubmit;*/

/*QUESTION 2*/;

points = points2;
pr=0;
k = 3;
random = 'obs';
nsim = 1000;
Fvec = J(nsim, 1, .); R2vec = J(nsim, 1, .); iterations = J(nsim, 1, .);
do q = 1 to nsim;
	call kmeans_clus;
	Fvec[q] = F; R2vec[q] = r2; iterations[q] = iter;
end;

print Fvec R2vec iterations;

mean_iterations = mean(iterations);
mean_f = mean(Fvec); mean_r2 = mean(R2vec);
print mean_iterations,, mean_f mean_r2;

create iteration_dat var {iterations}; append; close iteration_dat; /* write data */
submit;
  proc univariate data = iteration_dat;
	histogram / midpoints = (3 to 15 by 1) odstitle = "Distribution of the number of iterations before convergence";
  run;
endsubmit;

plt = points || resp_obs;
run Scatter(plt[, 1], plt[, 2]) group = plt[, 3];

/*CODE SPECIFIC TO QUESTION 1*/
/*f_crits = quantile("F", {0.025, 0.975}, 2, 297); print f_crits;*/
/*iteration = do(1, 7, 1)`;*/
/*fs = fs[2:8]; r2s = r2s[2:8];*/
/*print fs r2s iteration;*/

run series(iteration, fs); run series(iteration, r2s);

/*SOME GMM CALCULATIONS*/

/*a = points || resp_obs; c1i = loc(resp_obs = 1); c2i = loc(resp_obs = 2); c3i = loc(resp_obs = 3);*/
/*c1 = a[c1i, 1:d]; c2 = a[c2i, 1:d]; c3 = a[c3i, 1:d];*/
/*mu1 = mean(c1); mu2 = mean(c2); mu3 = mean(c3); test = var(c1);*/
/*sig1 = sqrt(var(c1)); sig2 = sqrt(var(c2)); sig3 = sqrt(var(c3));*/
/*pi1 = nrow(c1)/n; pi2 = nrow(c2)/n; pi3 = nrow(c3)/n;*/
/*print mu1 sig1 pi1,, mu2 sig2 pi2 ,, mu3 sig3 pi3;*/

 




nsim = 100;
Ws = J(nsim, 1, .); Fs = J(nsim, 1, .);
do k = 2 to nsim;
	pr = 0;
	n = nrow(points);
	d = ncol(points); *number of variables in dataset;
	random = 'obs';
	conv = 0; count = 0;
	call kmeans_clus;
	Ws[k] = W;
	Fs[k] = F;
end;

/*The elbow plot*/
k = (1:nsim);
run Series(k, Ws);
run Series(k, Fs);
