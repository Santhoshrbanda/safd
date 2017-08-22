DoubleExponential_density <- function(x,mean,b){
  
  density = exp(-1*abs(x-mean)/b)/(2*b);
  return(density);
}

Mixture_density <- function(x,mean1,std1,mean2,std2,p){
  
  density = p*dnorm(x,mean1,std1) + (1-p)*dnorm(x,mean2,std2);
  return(density);
}

main <- function(){

  data = read.table("d-csp0108.txt", stringsAsFactors = F, header = TRUE)
  C <- data[,2];
  SP <- data[,3];
  x1 = seq(min(C),max(C), length = 1000);
  x2 = seq(min(SP),max(SP), length = 1000);
  
  #Cauchy
  png("Plots/C_rtn_hist_cauchy.png");
  hist(C,1000);
  par(new = TRUE);
  hx = dcauchy(x1,mean(C),0.015,log = FALSE);
  plot(x1,hx,type = 'l');
  dev.off();
  
  png("Plots/SP_rtn_hist_cauchy.png");
  hist(SP,1000);
  par(new = TRUE);
  hx = dcauchy(x2,mean(SP),0.01,log = FALSE);
  plot(x2,hx,type = 'l');
  dev.off();
  
  #DoubleExponential
  png("Plots/C_rtn_hist_doubleexp.png");
  hist(C,1000);
  par(new = TRUE);
  hx = DoubleExponential_density(x1,mean(C),0.025);
  plot(x1,hx,type = 'l');
  dev.off();
  
  png("Plots/SP_rtn_hist_doubleexp.png");
  hist(SP,1000);
  par(new = TRUE);
  hx = DoubleExponential_density(x2,mean(SP),0.015);
  plot(x2,hx,type = 'l');
  dev.off();
  
  #Mixture
  png("Plots/C_rtn_hist_mixture1.png");
  hist(C,1000);
  par(new = TRUE);
  hx = Mixture_density(x1,mean(C),0.025,mean(C),0.02,0.9);
  plot(x1,hx,type = 'l');
  dev.off();
  
  png("Plots/SP_rtn_hist_mixture.png");
  hist(SP,1000);
  par(new = TRUE);
  hx = Mixture_density(x2,mean(SP),0.01,mean(SP),0.02,0.8);
  plot(x2,hx,type = 'l');
  dev.off();
  
}

main();
