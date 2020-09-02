


data npr;
set sasuser.nps2018;
run;

proc iml;
use npr;
read all into x_y;
n = nrow(x_y);
print(x_y);



/*LET SPAN = 0.1*/
s = 0.1;
m = s*n;



y_hat = J(n, 1, .);
do i = 1 to 1;    *This loop is to index through each x value in the dataset;
	x0 = x_y[i, 1];    *Selects our ith focal value;
	x_c = abs(x_y[, 1] - x0);
	D = x_y||x_c;
	call sort(D, {3});
	dists = J(n, 1, .);
	Wt_x = J(m, 1, .);
	x_w = D[1:m, 2];   *These are the nearest neighbour xs which fall in the window (spread to the left and right of the focal point).;
	x_w_sort = x_w;    *Sorting the x_ws in order to calculate  h, the half range of the window.;
	call sort(x_w_sort);
	x_weights = D[1:m, 3];
	h = (x_w_sort[m] - x_w_sort[1])/2;
	do k = 1 to m;
		Wt_x[k] = (1 - (x_weights[k]/h)**3)**3;
		if (x_weights[k]/h) < 1 then Wt_x[k] = Wt_x[k];
		if (x_weights[k]/h) >= 1 then Wt_x[k] = 0;
	end;
end;

print Wt_x;










/*print y_hat;*/





	
