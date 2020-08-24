/*K-means algorithm from first principles;*/

ods html close;
ods html;
data q1;
set sasuser.q1;
run;

proc iml;
use q1;
read all into points;

/*Visualising the data*/
run Scatter(points[, 1], points[, 2]);

inits = {10 30, 20 40, 40 40};

inits = new_centroids;
print inits;
/*ASSIGNMENT:*/
groups = J(n, 1, .);
print k;
do i = 1 to n;
	dist = J(k, 1, .);
	do j = 1 to k;
		dist[j] = ((points[i, 1] - inits[j, 1])**2 + (points[i, 2] - inits[j, 2])**2)**0.5;
	end;	
	mini = min(dist);
	groups[i] = loc(dist = mini);
end;

inits = new_cluster;

/*Grouping and plotting to see first iteration results*/
grouped = points || groups;
run Scatter(grouped[, 1], grouped[, 2]) group = grouped[, 3];

new_centroids = J(k, 2, .);
do i = 1 to k;
	cluster_i = loc(groups = i);
	points_i = points[cluster_i,];
	new_centroid_i = points_i[+,]/nrow(points_i);
	new_centroids[i,] = new_centroid_i;
end;


/*somx = J(1, k, .);*/
/*do i = 1 to k;*/
/*	if grouped[, 3] = i then somx[, i] = sum(grouped[,1]);*/
/*end;*/
/**/
/*print som;*/


/*RECALCULATE CENTROIDS*/
/*do i = 3 to k+2;*/
/*	g = points[,3 = i];*/


/*INITIATION*/
/*Selecting the first three centroids*/

n = nrow(points);
k = 3;
/*inits = J(k, 2, .);*/
/*call randseed(1);*/
/*do i = 1 to k;*/
/*	init_x1 = rand("Uniform")*max(points[, 1]);*/
/*	init_x2 = rand("Uniform")*max(points[, 2]);*/
/*	inits[i, 1] = init_x1;*/
/*	inits[i, 2] = init_x2;*/
/*end;*/

/*print inits;*/
