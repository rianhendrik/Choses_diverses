libname ekt "C:\Users\rianh\OneDrive - University of Pretoria\Documents\Rian 2020\Semester 2\EKT 720\data" ;

symbol1 value=dot 
        height=2 i=none;
symbol2 value=dot 
        height=2 i=none;
symbol3 value=dot 
        height=2 i=none;


/*QUESTION 2A*/
proc gplot data=ekt.yx ;
plot x2*x1=group ;
title;
run ;


/*QUESTION 2B*/

/*Obtaining 80% of the data as a training set*/

proc surveyselect data=ekt.yx
   method = srs n = 200 out = training;
run;

proc iml;
use training;
read all  into xy;
n = nrow(xy); 





*x=(x-x[:,])/std(x);

*Ensure that the response variable is in the first row of your data matrix);
start kNN(data, k);

p = ncol(data);
n = nrow(data);
y = data[,1];
level = max(y);
x = data[,2:p];
dist = distance(x);
yhat = J(n, 1, .);
do i = 1 to n;
	dists_i = dist[, i];
	choice_mat = y||dists_i;
	call sort(choice_mat, {2});
	knns = choice_mat[2:k, 1];
	counts_i = J(level, 1, .);
	do j = 1 to nrow(counts_1);
		y_j = knns[y_j];
		counts_i[y_j] = counts_i[y_j] + 1;
	end;
	maximum_s = counts_i[<:>]; *If I use this to find the index of the maximum, it will choose the first maximum if there is a tie.;
	yhat[i] = maximum_s;
	*maxis = max(counts_i); *THIS CODE IS TO ENTER A CRITERIA, IE, a specific X-value etc.;
	*maximum_s = loc(counts_i = maxis);
	*if ncol(maximum_s) = 1 then yhat[i] = maximum_s;
	*if ncol(maximum_s) > 1 then;
end;
evaluate = y - yhat;
correct = loc(evaluate = 0);
return(ncol(correct)/nrow(evaluate));
finish;

prop =  kNN(xy, 4);
print prop;


