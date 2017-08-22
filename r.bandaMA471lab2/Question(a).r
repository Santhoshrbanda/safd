main <- function(){
	
	data = read.table('data.txt',header = TRUE);
	C_rtn = data[,2];
	SP_rtn = data[,3];
	
	C_rtn_mean = mean(C_rtn);
	SP_rtn_mean = mean(SP_rtn);
	C_rtn_var = var(C_rtn);
	SP_rtn_var = var(SP_rtn);
	
	C_rtn_Kurtosis = mean((C_rtn-C_rtn_mean)^4)/(C_rtn_var^2);
	SP_rtn_Kurtosis = mean((SP_rtn-SP_rtn_mean)^4)/(SP_rtn_var^2);
	
	print(C_rtn_Kurtosis);
	print(SP_rtn_Kurtosis);
}

main();
