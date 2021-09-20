proc print data = sasuser.ass2trg;
run;


proc nlin data = sasuser.ass2trg method=marquardt hougaard;
	parms theta2 = 160.97283
		  theta2 = -0.524031
		  theta4 = 0.0005495	
		  theta5 = 0.0011692
		  theta6 = -0.001398
		  epsilon2 = 200 to 600 by 0.1
		  epsilon1 = 200 to 600 by 0.1;
	model y = theta2*x + theta3#x##2 + theta4#x##3 + theta5#(x - epsilon1)##3 + theta6#(x - epsilon2)##3;
run;



