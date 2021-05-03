ods html close; /* close previous */
ods html; /* open new */

*Looking at SAS's built in function's clustering solution;

/* proc cluster data = sasuser.x method = single rsquare pseudo;*/
/* run;*/

proc iml;
use sasuser.cluster;
read all into dmat;
use sasuser.x;
read all into dmatx;

/*x = dmatx[,1]; y = dmatx[, 2];*/
/*call scatter(x, y); */


*PRELIMS;

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

*dmat = dmatx; *************************************************************************;

clusts = (1:nrow(dmat))`||J(nrow(dmat), nrow(dmat)-1, 0);
nclusts = nrow(clusts);
dist = distance(dmat);
dim = 2;

obss = {0 0};
print = 1;

print dmat;

k = 3; *number of clusters;
do while(nclusts>k); *I want to end up with a three cluster solution. Therefore, do until nclusts = 3;


call SetDiag(dist, 90000); *set diagonal to a large distance, else the diagonoals of 0 will also be the min;
ind = dist[>:<]; *The diagonal of this distance matrix must be larger than the max in distance matrix, else dist[>:<] will return a diagonal.;
obs = ind2sub(ncol(dist), ind); *obs - cluster result: the result of this iteration of clustering. obs == the two obs clustered in this iteration;
if print = 1 then print obs; *gives subscript (column row) of the minimum (matrix symmetric so order does not matter);

obss = obss//obs;
*remove one of those observations from the distance matrix, as it is no longer needed (it is part of a cluster with another observation(s);

join = dmat[obs[1],]//dmat[obs[2],]; *These two obsrvations are joining each other in a cluster;

dim = nrow(dist);
ln = 90909090; *ln - large number;
if max(obs) = dim then do;
	dist = dist[1:max(obs)-1,];
	dist = dist[,1:max(obs)-1];
end;

else do;
	top = dist[1:max(obs)-1,]; *All rows before the row (observation) we are removing;
	bot = dist[max(obs)+1:dim,]; *All rows after the row (observation) we are removing;
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

print clusts;
print obss;
print dist;	

*Calculating total sum of squares;
om = dmat[+,]/nrow(dmat); *overall mean of data;
dist_m_total = om//dmat;
dtotal = distance(dist_m_total)[2:nrow(dist_m_total),1];
tss = dtotal`*dtotal;
print tss;






wcsi = J(1, nrow(clusts)); *within cluster sum of squares for ith cluster;

do j = 1 to nrow(clusts);
	cl = clusts[j,];
	zs = loc(cl = 0)[1,1]; 
	cl = cl[1:zs-1]; 
	obis = J(nrow(cl), ncol(dmat), 0);
	do iter = 1 to nrow(cl);
		indi = cl[iter];
		obis[iter,] = dmat[indi,];
		cm = obis[+,]/nrow(cl); *cluster mean;
	end;
	dist_m = cm//obis; print dist_m;
	d = distance(dist_m)[2:nrow(dist_m),1];
	t = d`*d; 
	wcsi[1,j] = d`*d;
end;
wcss = sum(wcsi);
bcss = tss - wcss;
c = k-1;
n = nrow(dmat);
F = ((bcss)/c)/((wcss)/(n-c));
R2 = bcss/tss;
print R2 F;


obs1 = loc(dmat=c1); print dmat obs1;
*separate clusters;
*do i = 1 to k;
	*loc

zeros_ic = loc(;*zeros_ic = zeros in cluster matrix;


/*Toy Example of how the clustering is working*/

/*obs  = {1, 2, 3, 4, 5};*/
/*zeros = J(nrow(obs), nrow(obs)-1, 0);*/
/*clusts = obs||zeros;*/
/*nclusts = nrow(clusts);*/
/*print clusts;*/
/**/
/**the first two obsevations to be clustered are 1 and 2. Thus, replace the first...;*/
/**...zero value in row 1 with the observation in row 2. Then, delete row 2.;*/
/*res = {1,2}; *res - result, 1 and two are clustered;*/
/*ind1 = loc(clusts[max(res),]>0);*find out indices where max(res) is not zero;*/
/*ind2 = loc(clusts[min(res),]=0);*/
/*print ind2;*/
/*sn = ncol(ind1); *sn - space needed;*/
/*ind2 = ind2[1:sn]; *we only need the first 1:sn to be replaced by the observations from the new observation/cluster;*/
/*print ind1 ind2;*/
/*clusts[min(res), ind2] = clusts[max(res), ind1]; *the replacement;*/
/*top = clusts[1:max(res)-1,]; *All rows before the row (observation) we are removing;*/
/*bot = clusts[max(res)+1:nclusts,]; *All rows after the row (observation) we are removing;*/
/*clusts = top//bot;*/
/*print clusts;*/

	

	

