data q1dat_ass3;
input x1 x2 x3 x4 x5 x6;
datalines;
1 0 1 0	1 0
1 0 1 0	1 1
1 0 1 0	0 0
0 0 1 0	1 0
1 0 1 1	1 1
0 1 0 1	0 1
0 1 0 1	0 0
0 1 0 1	1 0
0 1 0 0	0 1
1 1 0 1	0 1
0 0 0 0	0 0
1 0 0 0	0 0
0 0 0 0	0 1
0 0 0 1	0 0
0 0 1 0	0 0
;
run;

proc iml;
use q1dat_ass3;
read all into dmat;

start si; *si - similarity index;
	p = ncol(dmat);
	n = nrow(dmat);
	I1 = J(1, p, 1);
	sokal_michener = J(n, n, .);
	russel_rao = J(n, n, .);
	jaccard = J(n, n, .);
	do i = 1 to n;
		do j = 1 to n;
			CP = dmat[i,]*t(dmat[j,]);
			CA = (I1 - dmat[i,])*(I1 - dmat[j,])`;*p - dmat[i,]*t(I1) - I1*t(dmat[j,]) + CP;
			AP = n-sum(dmat[i,]) - CA;
			PA = n-sum(dmat[j,]) - CA;
			sokal_michener[i,j] = (CP + CA)/p;
			russel_rao[i,j] = CP/p;
			jaccard[i,j] = CP/(CP + PA + AP);
		end;
	end;
	dist = distance(dmat);
	dist2 = dist##2; 
	avg_euclid2 = dist2/p; *calculated to show that 1-Sokal-Michener is the same as average squared Euclidian distance;
finish;



*printig the three requested similarity indices;
call si;
print "Three similarity indices (SIs)",,
	  "Sokal-Michener SI:" sokal_michener[label=none],,
	  "Russel-Rao SI:" russel_rao[label=none],,
	  "Jaccard SI:" jaccard[label=none];
	  
*Emperically showing that 1-Sokal-Michener is the same as average squared Euclidian distance;
dissim_sokal_michener = 1-sokal_michener;
print "Emperically illustrating that 1 - Sokar-Michener index is the same as average squared Euclidian distance",,
	  "1-Sokal-Michener: A dissimilarity matrix.",,
	  dissim_sokal_michener[label=none],,
	  "Average squared Euclidian distance matrix:",,
	  avg_euclid2;
	  
	  

	

