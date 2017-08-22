library(EMCluster)
b = function(init_p,init_u1,init_u2,init_s1,init_s2,TOL){


	n = 200;
	x = rnorm(n,0,1);
	y = rnorm(n,0,5);
	u = runif(n,0,1);
	mix = 1:n;
	for(i in 1:n){
		if(u[i]<0.4){
			mix[i] = x[i];
		}else{
			mix[i] = y[i];
		}
	}

	p_old = 0.1;
	u1_old = 0.3;
	s1_old = 2.1;
	u2_old = 0.1;
	s2_old = 0.3;
	TOL = 10^(-6);

	oldParam = c(p_old,u1_old,s1_old,u2_old,s2_old);

	numer = p_old * dnorm(mix,u1_old,s1_old);
	denom = numer + ((1-p_old)*dnorm(mix,u2_old,s2_old));
	m = numer/denom;
	#print(m);


	p_new = sum(m)/n;
	u1_new = sum(m*(mix))/sum(m);
	u2_new = sum((1-m)*(mix))/sum(1-m);
	s1_new =  sum( m * ((mix) - u1_new)^2 )/sum(m);
	s1_new = sqrt(s1_new);
	s2_new =  sum( (1-m) * ((mix) - u2_new)^2 )/sum(1-m);
	s2_new = sqrt(s2_new);

	newParam = c(p_new,u1_new,s1_new,u2_new,s2_new);


	while(sqrt(sum((newParam-oldParam)*(newParam-oldParam))) >= TOL){
		oldParam = newParam;
		p_old = p_new;
		u1_old = u1_new;
		s1_old = s1_new;
		u2_old = u2_new;
		s2_old = s2_new;

		numer = p_old * dnorm(mix,u1_old,s1_old);
		denom = numer + ((1-p_old)*dnorm(mix,u2_old,s2_old));
		m = numer/denom;
		#print(m);

		if(sum(m)==0){
			break;
		}

		p_new = sum(m)/n;
		u1_new = sum(m*(mix))/sum(m);
		u2_new = sum((1-m)*(mix))/sum(1-m);
		s1_new =  sum( m * ((mix) - u1_new) * ((mix) - u1_new))/sum(m);
		s1_new = sqrt(s1_new);
		s2_new =  sum( (1-m) * ((mix) - u2_new) * ((mix) - u2_new))/sum(1-m);
		s2_new = sqrt(s2_new);
		newParam = c(p_new,u1_new,s1_new,u2_new,s2_new);

	}

	return(c(p_new,u1_new,s1_new,u2_new,s2_new));
}

param = b(0.1,0.3,2.1,0.1,0.3,10^(-6));

print(param);
