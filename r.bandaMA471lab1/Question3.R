main <- function(){
  
  data = read.table("d-csp0108.txt", stringsAsFactors = F, header = TRUE)
  C <- data[,2];
  SP <- data[,3];
  x1 = seq(min(C),max(C), length = 1000);
  x2 = seq(min(SP),max(SP), length = 1000);
  percentages = seq(0,1,length = 100);
  n = length(C);
  sample_quantiles_C = quantile(C,percentages);
  sample_quantiles_SP = quantile(SP,percentages);
  
  
  #Cauchy
  png("Plots/C_rtn_qq_cauchy.png");
  x = rcauchy(n,mean(C),0.015);
  quantiles = quantile(x,percentages);
  plot(sample_quantiles_C,quantiles);
  dev.off();
  
  png("Plots/SP_rtn_qq_cauchy.png");
  x = rcauchy(n,mean(SP),0.01);
  quantiles = quantile(x,percentages);
  plot(sample_quantiles_SP,quantiles);
  dev.off();
  
  
  #Mixture
  png("Plots/C_rtn_qq_mixture.png");
  yn <- rbinom(n, 1, .9)
  x = rnorm(n,mean(C)+yn*mean(C),0.01+yn*0.02);
  quantiles = quantile(x,percentages);
  plot(sample_quantiles_C,quantiles);
  dev.off();
  
  png("Plots/SP_rtn_qq_mixture.png");
  yn <- rbinom(n, 1, .8)
  x = rnorm(n,mean(C)+yn*mean(C),0.025+yn*0.02);
  quantiles = quantile(x,percentages);
  plot(sample_quantiles_SP,quantiles);
  dev.off();
  
  #Normal
  png("Plots/C_rtn_qq_normal.png");
  x = rnorm(n,mean(C),sqrt(var(C)));
  quantiles = quantile(x,percentages);
  plot(sample_quantiles_C,quantiles);
  dev.off();
  
  png("Plots/SP_rtn_qq_normal.png");
  x = rnorm(n,mean(SP),sqrt(var(SP)));
  quantiles = quantile(x,percentages);
  plot(sample_quantiles_SP,quantiles);
  dev.off();
}

main();