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
