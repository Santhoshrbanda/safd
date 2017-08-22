# samp <- read.table("d-csp0108.txt", stringsAsFactors = F, header = TRUE)
# hist(samp$C, breaks = 250)
# hist(samp$SP, breaks = 250)
# 
# x <- seq(min(samp$C), max(samp$C), length = length(samp$C))
# hx<- dnorm(x)

data = read.table("d-csp0108.txt", stringsAsFactors = F, header = TRUE)
  t <- data[,1];
  C <- data[,2];
  SP <- data[,3];
  
  # Scatter Plots
  
  png("Plots/C_rtn_scatter.png");
  plot(t,C);
  dev.off();
  
  png("Plots/SP_rtn_scatter.png");
  plot(t,SP);
  dev.off();
  
  
  # Plotting the histograms and normal distributions on histograms
  
  png("Plots/C_rtn_hist.png");
  hist(C,1000);
  par(new = TRUE);
  x = seq(min(C),max(C),length = 100);
  mean = mean(C);
  sd = sqrt(var(C));
  hx = dnorm(x,mean,sd);
  plot(x,hx,type = 'l');
  dev.off();
  
  png("Plots/SP_rtn_hist.png");
  hist(SP,1000);
  par(new = TRUE);
  x = seq(min(SP),max(SP),length = 1000);
  mean = mean(SP);
  sd = sqrt(var(SP));
  hx = dnorm(x,mean,sd);
  plot(x,hx,type = 'l');
  dev.off();