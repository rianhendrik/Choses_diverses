
ods html close;
ods html;

data q1;
set sasuser.q1;
run;

data q2;
set sasuser.q2;
run;


proc iml; 

/*DEFINING THE K-MEANS SUBROUTINE*/
start Kmeans(points, k, inits, evaluate);
random = 0;
if inits = 0 then random = 1;
n = nrow(points);
d = ncol(points);
if d > 2 then print 'This dataset is of a dimension higher than 2. Meaningful plots can thus not be generated.';
if d = 2 then print 'Two dimenional plot of the data';
if d = 2 then run Scatter(points[, 1], points[, 2]);

conv = 1;
obs = 1:n;
if random = 1 then indices = sample(obs, k, 'WOR');
if random = 1 then inits = points[indices,];
print 'The initial K-means';
print inits;
new_centroids = inits;
repeat = 0;
overall_mean = J(1, d, .);
do i = 1 to d;
	overall_mean[,i] = points[+,i]/n;
end;

ods html close;
ods html;
data q1;
set sasuser.q1;
run;

data q2;
set sasuser.q2;
run;


proc iml; 

/*DEFINING THE K-MEANS SUBROUTINE*/
start Kmeans(points, k, inits, evaluate);
random = 0;
if inits = 0 then random = 1;
n = nrow(points);
d = ncol(points);
if d > 2 then print 'This dataset is of a dimension higher than 2. Meaningful plots can thus not be generated.';
if d = 2 then print 'Two dimenional plot of the data';
if d = 2 then run Scatter(points[, 1], points[, 2]);

conv = 1;
obs = 1:n;
if random = 1 then indices = sample(obs, k, 'WOR');
if random = 1 then inits = points[indices,];
print 'The initial K-means';
print inits;
new_centroids = inits;
iterations = 0;
overall_mean = J(1, d, .);
do i = 1 to d;
	overall_mean[,i] = points[+,i]/n;
end;

R2 = J(n, 1, .);
Fs = J(n, 1, .);

/*COMPUTING THE TOTAL DEVIATION*/
t_dists = J(1, n, .);
if d = 2 then do i = 1 to n;
	t_dists[i] = ((points[i, 1] - overall_mean[1, 1])**2 + (points[i, 2] - overall_mean[1, 2])**2)**0.5;
end;
if d = 3 then do i = 1 to n;
	t_dists[i] = ((points[i, 1] - overall_mean[1, 1])**2 + (points[i, 2] - overall_mean[1, 2])**2 + (points[i, 3] - overll_mean[1, 3]**2)**0.5;
end;
if d = 4 then do i = 1 to n;
	t_dists[i] = ((points[i, 1] - overall_mean[1, 1])**2 + (points[i, 2] - overall_mean[1, 2])**2 + (points[i, 3] - overall_mean[1, 3])**2 + (points[i, 4] - overall_mean[1, 4])**2)**0.5;
end;

T = sum(t_dists);

do while (conv = 1);
iterations = iterations + 1;
inits = new_centroids;

/*ASSIGNMENT STEP:*/

groups = J(n, 1, .);
if d = 2 then do i = 1 to n;
	dist = J(k, 1, .);
		do j = 1 to k;
			dist[j] = ((points[i, 1] - inits[j, 1])**2 + (points[i, 2] - inits[j, 2])**2)**0.5;
		end;	
	mini = min(dist);
	groups[i] = loc(dist = mini);
end;

if d = 3 then do i = 1 to n;
	dist = J(k, 1, .);
		do j = 1 to k;
			dist[j] = ((points[i, 1] - inits[j, 1])**2 + (points[i, 2] - inits[j, 2])**2 + (points[i, 3] - inits[j, 3])**2)**0.5;
		end;
	mini = min(dist);
	groups[i] = loc(dist = mini);
end;	
				

if d = 4 then do i = 1 to n;
	dist = J(k, 1, .);
		do j = 1 to k;
			dist[j] = ((points[i, 1] - inits[j, 1])**2 + (points[i, 2] - inits[j, 2])**2 + (points[i, 3] - inits[j, 3])**2 + (points[i,4] - inits[j, 4])**2)**0.5;
		end;
	mini = min(dist);
	groups[i] = loc(dist = mini);
end;	

if d = 5 then do i = 1 to n;
	dist = J(k, 1, .);
		do j = 1 to k;
			dist[j] = ((points[i, 1] - inits[j, 1])**2 + (points[i, 2] - inits[j, 2])**2 + (points[i, 3] - inits[j, 3])**2 + (points[i,4] - inits[j, 4])**2 + (points[i, 5] - inits[j, 5])**2)**0.5;
		end;
	mini = min(dist);
	groups[i] = loc(dist = mini);
end;	

/*COMPUTING THE WITHIN CLUSTER VARIATON*/
wcd_i = J(1, k, .);
if d = 2 then do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	dists = J(1, nrow(points_i));
	do j = 1 to nrow(points_i);
		dists[i] = ((points_i[i, 1] - inits[k, 1])**2 + (points_i[i, 2] - inits[k, 2])**2)**0.5;
	end;
	wcd_i[i] = sum(dists);
end;

if d = 3 then do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	dists = J(1, nrow(points_i));
	do j = 1 to nrow(points_i);
		dists[i] = ((points_i[i, 1] - inits[k, 1])**2 + (points_i[i, 2] - inits[k, 2])**2 + (points_i[i, 3] - inits[k, 3])**2)**0.5;
	end;
	wcd_i[i] = sum(dists);
end;

if d = 4 then do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	dists = J(1, nrow(points_i));
	do j = 1 to nrow(points_i);
		dists[i] = ((points_i[i, 1] - inits[k, 1])**2 + (points_i[i, 2] - inits[k, 2])**2 + (points_i[i, 3] - inits[k, 3])**2 + (points_i[i, 4] - inits[k, 4])**2)**0.5;
	end;
	wcd_i[i] = sum(dists);
end;

if d = 5 then do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	dists = J(1, nrow(points_i));
	do j = 1 to nrow(points_i);
		dists[i] = ((points_i[i, 1] - inits[k, 1])**2 + (points_i[i, 2] - inits[k, 2])**2 + (points_i[i, 3] - inits[k, 3])**2 + (points_i[i, 4] - inits[k, 4])**2 + (points_i[i, 5] - inits[k, 5])**2)**0.5;
	end;
	wcd_i[i] = sum(dists);
end;

W = sum(wcd_i);
B = T - W;
R2[iterations] = (B/T);
Fs[iterations] = (B/(k-1))/(W/(n - k));
									
/*UPDATE STEP:*/
	
new_centroids = J(k, d, .);
do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	new_centroid_i = points_i[+,]/nrow(points_i);
	new_centroids[i,] = new_centroid_i;
end;

if new_centroids = inits then conv = 0; 
						 else conv = 1;
end;

grouped = points || groups;
print 'The final K-means';
print new_centroids;
if d = 2 then print 'Two dimensional plot of the final results after convergence';
if d = 2 then run Scatter(grouped[, 1], grouped[, 2]) group = grouped[, 3];
print 'The number of steps taken to reach convergence';
print iterations;
iter = 1:iterations;
/*print R2;*/
R2s = R2[iter,];
Fs = Fs[iter,];
R2 = iter`||R2s;
F = iter`||Fs;
if evaluate = 'true' then print 'The R2s for each successive iteration, computed by the ratio of betwee cluster variation, and total variation';
if evaluate = 'true' then print R2;
if evaluate = 'true' then print 'The pseudo-F test statistics for each succesive iteration';
if evaluate = 'true' then print F;
finish;



use q1;
read all into points;


/*/*/*/*NOTE*/*/*/*/
/*If you want to have the algorithm select n random points from the data as initial centroids, set the second last argument into the subroutine as 0.*/
/*Else insert a matrix of n points that you would like to specify as your starting points.*/
/*If you want to see the R2 values for each successive iteration of the K-means algortihm, set 'evaluation' to 'true'.*/

/*QUESTION 1a*/;
initial_points = {10 30, 20 40, 40 40};
call Kmeans(points, 3, initial_points, 'true');

/*QUESTION 1b*/
call Kmeans(points, 3, 0, 'true');

/*/*Loading the new, 4 dimensional dataset*/*/
use q2;
read all into points;

/*QUESTION 2a-b*/
/*R2 and pseudo-F values are computed by setting evaluation = 'true' */
call Kmeans(points, 4, 0, 'true');

/*QUESTION 2c*/
call Kmeans(points, 3, 0);









