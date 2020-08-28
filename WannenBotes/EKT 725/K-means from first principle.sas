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

n = nrow(points);
k  = 2;
/*inits = {10 30, 20 40, 40 40};*/

conv = 1;
d = ncol(points); /*where d denotes dimensions*/
random = 1;
obs = 1:n;
/*start clusterer;*/

	
if random = 1 then indices = sample(obs, k, 'WOR');
inits = points[indices,];
print inits;
new_centroids = inits;
repeat = 0;
do while (conv = 1);
repeat = repeat + 1;
inits = new_centroids;


	/*ASSIGNMENT:*/

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
/*end clusterer;*/

print new_centroids;


/*Displaying final result*/
grouped = points || groups;
run Scatter(grouped[, 1], grouped[, 2]) group = grouped[, 3];
print repeat;

quit;




