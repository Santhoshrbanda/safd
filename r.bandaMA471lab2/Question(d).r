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
	for(i in 1:20)
	{
		likelihood = likelihood*(density_weibull(sample[i],a,b));
	}
	return(log(likelihood));
}

norm_vec <- function(x){
	return(sqrt(sum(x^2)));
}

findInitialChoice <- function(){
	#library(rgl);
	shape = 1:0.01:100;
	scale = 0.5:0.01:100;
	z = outer(shape,scale,likelihood)
	print(z);

	#contour(shape,scale,z,nlevels = 10)
	persp(shape,scale,z,expand = 0.5, col = "lightblue",ticktype="detailed")

	
}



findMLE <- function(TOL){
	library(numDeriv);
	library(MASS);
	x0 = c(2.2,1.0);
	oldParam = x0;
	H = hessian(likelihood, x = oldParam, method="complex", method.args=list());
	gradient = grad(likelihood, x = oldParam, method="complex", method.args=list()); 
	newParam = oldParam - (solve(H) %*% gradient);
	
	while(norm_vec(gradient)>=TOL){
		oldParam = newParam;
		H = hessian(likelihood, x = oldParam, method="complex", method.args=list());
		gradient = grad(likelihood, x = oldParam, method="complex", method.args=list()); 
		newParam = oldParam - (solve(H) %*% gradient);
	}
	
	return(newParam);
}

mle = findMLE(0.0001);
print(mle);

#findInitialChoice();

