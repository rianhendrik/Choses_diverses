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

proc surveyselect data=ekt.yx seed = 817151001 
   method = srs n = 200 outall out = sampled;
run;


proc iml;
use sampled;
read all  into data;
xy = data[, 2:4];
selected = data[, 1];
n = nrow(xy);

/*DEFINING OUR TRAINING AND TEST DATASETS*/
training_index = loc(selected = 1);
training = xy[training_index,];
test_index = loc(selected = 0);
test = xy[test_index,];

n_train = nrow(training); 
n_test = nrow(test);

/*THE SUBROUTINE*/
*Ensure that the response variable is in the first row of your data matrix);

start kNN(error_prop, data, k);

p = ncol(data);
n = nrow(data);
y = data[,1];
level = max(y);
x = data[,2:p];
dist = distance(x);
yhat = J(n, 1, .);
do i = 1 to n;
	dists_i = dist[, i];
	choice_mat = y||dists_i||x;
	call sort(choice_mat, {2});
	if k > 1 then knns = choice_mat[2:k+1, 1];
	if k = 1 then knns = choice_mat[2:k, 1];
	if k = 0 then knns = choice_mat[1, 1];
	counts_i = J(level, 1, 0);
	do j = 1 to nrow(knns);
		y_j = knns[j];
		counts_i[y_j,] = counts_i[y_j,] + 1;
	end;
	maximum_s = counts_i[<:>]; *If I use this to find the index of the maximum, it will choose the first maximum if there is a tie.;
	yhat[i] = maximum_s;
	*maxis = max(counts_i); *THIS CODE IS TO ENTER A CRITERIA, IE, a specific X-value etc.;
	*maximum_s = loc(counts_i = maxis);
	*if ncol(maximum_s) = 1 then yhat[i] = maximum_s;
	*if ncol(maximum_s) > 1 then;
end;

error = abs(y - yhat);
correct = (loc(error = 0))`;
error_prop = (nrow(correct)/nrow(error))*100;

finish;

print training;


/*ERROR ON TRANING DATA*/
nk_train = n_train - 1;
errors_train = J(nk_train, 1, .);
do k = 1 to nk_train;
	run kNN(res, training, k);
	errors_train[k,] = (100 - res);
end;

/*ERROR ON TEST DATA*/
nk_test = n_test - 1;
errors_test = J(nk_test, 1, .);
do k = 1 to nk_test;
	run kNN(res, test, k);
	errors_test[k,] = (100 - res);
end;

ks = (1:n)`;
ks_50 = ks[1:nk_test];
errors_train_50 = errors_train[1:nk_test];

outset_first50 = ks_50||errors_train_50||errors_test;

create train from outset_first50[colname={'K' 'error_prop_train' 'error_prop_test'}];
append from outset_first50;                                
close;  




proc sgplot data=train;
  series x=K y=error_prop_train;
  series x=K y=error_prop_test;
run;
