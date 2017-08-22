sample = generate_weibull(20,2,1);

generate_weibull <- function(n,shape,scale){
	
	rand = (-1*log(runif(n)))^(1/shape) / scale;
	return(rand);
}

density_weibull <- function(x,shape,scale)
{
	density = shape*(scale^shape)*(x^(shape-1))*exp(-1*((x*scale)^shape));
	return(density);
}

likelihood <- function(param){
	
	a = param[1]; #shape
	b = param[2]; #scale
	likelihood = 1;
	sample = generate_weibull(20,a,b);
	for(i in 1:20)
	{
		likelihood = likelihood*(density_weibull(sample[i],a,b));
	}
	return(log(likelihood));
}

main <- function(){
	
	library(numDeriv);
	H <- hessian(likelihood, x = c(2,1), method="complex", method.args=list());
	r = eigen(H,only.values=TRUE);
	eig = r$values;
	print(eig);
	
	unique = TRUE;
	
	for(i in 1:2)
	{
		if(Re(eig[i])>=0){
			unique = FALSE;
			break;
		}
	}
	if(unique){
		print("There is a unique maximum");
	}
	else{
		print("There is no unique maximum");
	}
}

main();

