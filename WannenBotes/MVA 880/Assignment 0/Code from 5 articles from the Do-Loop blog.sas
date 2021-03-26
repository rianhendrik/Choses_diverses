/*The 'Do Loop' blog post 1: The CUSUM-LAG trick in SAS/IML*/

proc iml;
   size = colvec(s);              
   endIdx = cusum(size);          
   beginIdx = 1 + lag(endIdx);    
   beginIdx[1] = 1;               
   return ( beginIdx || endIdx );
finish;
 
size = {2, 3, 2, 3};              
idx = ByGroupIndices(size);
print idx[c={"Begin" "End"}];
/**/
/*This code was sourced from https://blogs.sas.com/content/iml/2015/10/30/cusum-lag-group-sizes.html*/
quit;

/*The 'Do Loop' blog post 2: The UNIQUE-LOC trick: A real treat!*/

proc iml;
use sashelp.class; 
   read all var {sex} into C;    
   read all var {height} into x; 
close;
 
u = unique(C);    
s = j(1, ncol(u)); 
do i = 1 to ncol(u);   
   idx = loc(C=u[i]);  
   s[i] = mean(x[idx]);
end;
print s[colname=u]; 

/*This code was sourced from https://blogs.sas.com/content/iml/2011/11/01/the-unique-loc-trick-a-real-treat.html*/
quit;

/*The 'Do Loop' blog post 3: Passing values from PROC IML into SAS procedures*/


proc iml;  
pctl = {2.5 10 50 90 97.5};           
s = rowcat( char(pctl)+" " );         
call symputx("PctList", s);           
quit;

*having created a macro of this percentile list in IML, you can pass it into a SAS procedure, like so:;
proc univariate data=sashelp.cars;
   var MPG_City;
   output out=pctl pctlpre=p pctlpts=&PctList; /* use SAS/IML values here */
run;

*you can also use the sumbit statment to pass and IML list into SAS procedure that is expecting such a list,
without having to leave IML, or creating a macro variable;

proc iml;  
pctl = {2.5 10 50 90 97.5};

submit  pctl;
   proc univariate data=sashelp.cars ;
      var MPG_City;
      output out=pctl pctlpre=p pctlpts=&pctl;  
   run;
endsubmit;

/*This code was sourced from https://blogs.sas.com/content/iml/2013/06/03/passing-values-into-procedures.html;*/


/*The 'Do Loop' blog post 4: Feature generation and correlations among features in machine learning*/

%let d = 7;
%let xMean = 0;
data Poly;
call streaminit(54321);
array x[&d];
do i = 1 to 1000;
   x[1] = rand("Normal", &xMean);  /* x1 ~ U(-3, 3] */
   do j = 2 to &d;
      x[j] = x[j-1] * x[1];        /* x[i] = x1**i, i = 2..7 */
   end;
   y = 2 - 1.105*x1 - 0.2*x2 + 0.5*x3 + rand("Normal");  /* response is cubic function of x1 */
   output;
end;
drop i j;
run;
 
proc corr data=Poly nosimple noprob;
   var y;
   with x:;
run;

proc corr data=Poly nosimple noprob plots(MAXPOINTS=NONE)=matrix(NVAR=ALL);
   var x:;
run;



