
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
repeat = 0;
overall_mean = J(1, d, .);
do i = 1 to d;
	overall_mean[,i] = points[+,i]/n;
end;
print overall_mean;


/*c_sqs = J(1, n, .);*/
/*do i = 1 to n;*/
/*	c_sqs[i] = (points[i,] - overall_mean)**2;*/
/*end;	*/
/*T = sum(c_sqs);*/
print T;
/*T = ssq(points - overall_mean);*/


do while (conv = 1);
repeat = repeat + 1;
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

/*EVALUATION STEP*/
Ws = J(1, k, .);
do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	Ws[i] = ssq(points_i - inits[k]);
end;
W = sum(Ws);
print Ws;
print W;
print T;
			
											
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
print repeat;

finish;



use q1;
read all into points;

/*/*/*/*NOTE*/*/*/*/
/*If you want to have the algorithm select n random points from the data, set the last argument into the subroutine as 0.*/
/*Else insert a matrix of n points that you would like to specify as your starting points.*/

/*QUESTION 1a*/;
initial_points = {10 30, 20 40, 40 40};
call Kmeans(points, 3, initial_points, 'true');

/*QUESTION 1b*/
call Kmeans(points, 3, 0);

use q2;
read all into points;

/*QUESTION 2a*/
call Kmeans(points, 4, 0);

/*QUESTION 2b*/
*Write something that performs the checks;

/*QUESTION 2c*/
call Kmeans(points, 3, 0);







