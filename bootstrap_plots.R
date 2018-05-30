dat <- read.table('Broadband_Speed_Test.csv', header=T, sep=',')
#setwd("/Users/Trevor/Downloads/STAT_403/Project")

# Bootstrap mean of download speeds on prices 25-50
d = dat[dat$cost_of_service=="25_50","actual_download"]
n = length(d)
B = 10000
Mean_Download_25_50 = rep(NA, B) 
for (i in 1:B) {
  w = sample(n,n,replace=TRUE)
  Mean_Download_25_50[i] = mean(d[w])
}
# 95% Confidence Interval
m = mean(Mean_Download_25_50)
lower = mean(Mean_Download_25_50)-qnorm(.975)*sqrt(var(Mean_Download_25_50))
upper = mean(Mean_Download_25_50)+qnorm(.975)*sqrt(var(Mean_Download_25_50))

# Bootstrap mean of download speeds on prices 50-75
d2 = dat[dat$cost_of_service=="50_75","actual_download"]
n2 = length(d2)
Mean_Download_50_75 = rep(NA, B) 
for (i in 1:B) {
  w = sample(n2,n2,replace=TRUE)
  Mean_Download_50_75[i] = mean(d2[w])
}
# 95% Confidence Interval
m2 = mean(Mean_Download_50_75)
lower2 = mean(Mean_Download_50_75)-qnorm(.975)*sqrt(var(Mean_Download_50_75))
upper2 = mean(Mean_Download_50_75)+qnorm(.975)*sqrt(var(Mean_Download_50_75))

# Bootstrap mean of download speeds on prices 75-100
d3 = dat[dat$cost_of_service=="75_100","actual_download"]
n3 = length(d3)
Mean_Download_75_100 = rep(NA, B) 
for (i in 1:B) {
  w = sample(n3,n3,replace=TRUE)
  Mean_Download_75_100[i] = mean(d3[w])
}
# 95% Confidence Interval
m3 = mean(Mean_Download_75_100)
lower3 = mean(Mean_Download_75_100)-qnorm(.975)*sqrt(var(Mean_Download_75_100))
upper3= mean(Mean_Download_75_100)+qnorm(.975)*sqrt(var(Mean_Download_75_100))

# Bootstrap mean of download speeds on prices 100 or above
d4 = dat[dat$cost_of_service=="100_or_above" & !is.na(dat$actual_download), "actual_download"]
n4 = length(d4)
Mean_Download_100_or_above = rep(NA, B) 
for (i in 1:B) {
  w = sample(n4,n4,replace=TRUE)
  Mean_Download_100_or_above[i] = mean(d4[w])
}
# 95% Confidence Interval
m4 = mean(Mean_Download_100_or_above)
lower4 = mean(Mean_Download_100_or_above)-qnorm(.975)*sqrt(var(Mean_Download_100_or_above))
upper4 = mean(Mean_Download_100_or_above)+qnorm(.975)*sqrt(var(Mean_Download_100_or_above))

# Histograms of distributions by price
hist(Mean_Download_25_50, col=rgb(1,0,0,.5), xlim = c(15,100), 
     main="Mean Download Speed Distributions By Price Range", xlab="Mb/s")
hist(Mean_Download_50_75, col=rgb(0,1,0,.5), add=TRUE)
hist(Mean_Download_75_100, col=rgb(0,0,1,.5), add=TRUE)
hist(Mean_Download_100_or_above, col=rgb(.5,0,0,.5), add=TRUE)

# mark means
abline(v=m, lwd=2, col="black")
abline(v=m2, lwd=2, col="black")
abline(v=m3, lwd=2, col="black")
abline(v=m4, lwd=2, col="black")

# mark lower bounds
abline(v=lower, lwd=2, col="orange")
abline(v=lower2, lwd=2, col="orange")
abline(v=lower3, lwd=2, col="orange")
abline(v=lower4, lwd=2, col="orange")

# mark upper bounds
abline(v=upper, lwd=2, col="antiquewhite3")
abline(v=upper2, lwd=2, col="antiquewhite3")
abline(v=upper3, lwd=2, col="antiquewhite3")
abline(v=upper4, lwd=2, col="antiquewhite3")

#############################################
#                                           #
#   Same thing but with upload speeds       #
#                                           #
#                                           #
#############################################

d = dat[dat$cost_of_service=="25_50","actual_upload"]
n = length(d)
B = 10000
Mean_Upload_25_50 = rep(NA, B) 
for (i in 1:B) {
  w = sample(n,n,replace=TRUE)
  Mean_Upload_25_50[i] = mean(d[w])
}
# 95% Confidence Interval
m = mean(Mean_Upload_25_50)
lower = mean(Mean_Upload_25_50)-qnorm(.975)*sqrt(var(Mean_Upload_25_50))
upper = mean(Mean_Upload_25_50)+qnorm(.975)*sqrt(var(Mean_Upload_25_50))

# Bootstrap mean of Upload speeds on prices 50-75
d2 = dat[dat$cost_of_service=="50_75","actual_upload"]
n2 = length(d2)
Mean_Upload_50_75 = rep(NA, B) 
for (i in 1:B) {
  w = sample(n2,n2,replace=TRUE)
  Mean_Upload_50_75[i] = mean(d2[w])
}
# 95% Confidence Interval
m2 = mean(Mean_Upload_50_75)
lower2 = mean(Mean_Upload_50_75)-qnorm(.975)*sqrt(var(Mean_Upload_50_75))
upper2 = mean(Mean_Upload_50_75)+qnorm(.975)*sqrt(var(Mean_Upload_50_75))

# Bootstrap mean of Upload speeds on prices 75-100
d3 = dat[dat$cost_of_service=="75_100","actual_upload"]
n3 = length(d3)
Mean_Upload_75_100 = rep(NA, B) 
for (i in 1:B) {
  w = sample(n3,n3,replace=TRUE)
  Mean_Upload_75_100[i] = mean(d3[w])
}
# 95% Confidence Interval
m3 = mean(Mean_Upload_75_100)
lower3 = mean(Mean_Upload_75_100)-qnorm(.975)*sqrt(var(Mean_Upload_75_100))
upper3= mean(Mean_Upload_75_100)+qnorm(.975)*sqrt(var(Mean_Upload_75_100))

# Bootstrap mean of Upload speeds on prices 100 or above
d4 = dat[dat$cost_of_service=="100_or_above" & !is.na(dat$actual_upload), "actual_upload"]
n4 = length(d4)
Mean_Upload_100_or_above = rep(NA, B) 
for (i in 1:B) {
  w = sample(n4,n4,replace=TRUE)
  Mean_Upload_100_or_above[i] = mean(d4[w])
}
# 95% Confidence Interval
m4 = mean(Mean_Upload_100_or_above)
lower4 = mean(Mean_Upload_100_or_above)-qnorm(.975)*sqrt(var(Mean_Upload_100_or_above))
upper4 = mean(Mean_Upload_100_or_above)+qnorm(.975)*sqrt(var(Mean_Upload_100_or_above))

# Histograms of distributions by price
hist(Mean_Upload_25_50, col=rgb(1,0,0,.5), xlim = c(8,60), 
     main="Mean Upload Speed Distributions By Price Range", xlab="Mb/s")
hist(Mean_Upload_50_75, col=rgb(0,1,0,.5), add=TRUE)
hist(Mean_Upload_75_100, col=rgb(0,0,1,.5), add=TRUE)
hist(Mean_Upload_100_or_above, col=rgb(.5,0,0,.5), add=TRUE)

# mark means
abline(v=m, lwd=2, col="black")
abline(v=m2, lwd=2, col="black")
abline(v=m3, lwd=2, col="black")
abline(v=m4, lwd=2, col="black")

# mark lower bounds
abline(v=lower, lwd=2, col="orange")
abline(v=lower2, lwd=2, col="orange")
abline(v=lower3, lwd=2, col="orange")
abline(v=lower4, lwd=2, col="orange")

# mark upper bounds
abline(v=upper, lwd=2, col="antiquewhite3")
abline(v=upper2, lwd=2, col="antiquewhite3")
abline(v=upper3, lwd=2, col="antiquewhite3")
abline(v=upper4, lwd=2, col="antiquewhite3")
