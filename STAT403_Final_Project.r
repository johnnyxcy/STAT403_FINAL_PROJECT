# setup the work directory
# setwd('C:/Users/Johnnia/Documents/STAT403_FINAL_PROJECT')

# Data clean-up
require(dplyr)

dat <- read.table('Broadband_Speed_Test.csv', header=T, sep=',')
dat <- select(dat, actual_download, actual_upload, 
                     advertised_download, advertised_upload,
                     cost_of_service, isp_user)
dat$isp <- 'Other'
dat[which(dat$isp_user=='Atlas'),]$isp <- 'Atlas'
dat[which(dat$isp_user=='Atlas Networks'),]$isp <- 'Atlas'
dat[which(dat$isp_user=='Atlas Internet'),]$isp <- 'Atlas'
dat[which(dat$isp_user=='AtlasOnNet'),]$isp <- 'Atlas'

dat[which(dat$isp_user=='Cascadelink Gigabit'),]$isp <- 'Cascade'
dat[which(dat$isp_user=='Cascadelink Gigabit Internet'),]$isp <- 'Cascade'
dat[which(dat$isp_user=='Cascadelink'),]$isp <- 'Cascade'
dat[which(dat$isp_user=='Cascade Link'),]$isp <- 'Cascade'
dat[which(dat$isp_user=='Cascadelink Gig'),]$isp <- 'Cascade'
dat[which(dat$isp_user=='cascadelink'),]$isp <- 'Cascade'

dat[which(dat$isp_user=='centurylink'),]$isp <- 'Centurylink'

dat[which(dat$isp_user=='comcast'),]$isp <- 'Comcast'

dat[which(dat$isp_user=='wave'),]$isp <- 'Wave'
dat[which(dat$isp_user=='WaveG'),]$isp <- 'Wave'
dat[which(dat$isp_user=='Wave-G/CondoInternet'),]$isp <- 'Wave'
dat[which(dat$isp_user=='Wave-G'),]$isp <- 'Wave'
dat[which(dat$isp_user=='CondoInternet Wave-G'),]$isp <- 'Wave'

dat[which(dat$isp_user=='Frontier'),]$isp <- 'Frontier'
dat[which(dat$isp_user=='frontier'),]$isp <- 'Frontier'
dat[which(dat$isp_user=='Frontier Fios'),]$isp <- 'Frontier'
dat[which(dat$isp_user=='Frontier DSL'),]$isp <- 'Frontier'

dat$isp_user <- NULL

# Compare the actual internet speed and the advertised internet speed
upload.dat <- select(dat, 'actual_upload', 'advertised_upload', 'isp')
download.dat <- select(dat, 'actual_download', 'advertised_download', 'isp')
upload.dat <- na.omit(upload.dat)
download.dat <- na.omit(download.dat)
upload.dat$diff <- upload.dat$actual_upload - upload.dat$advertised_upload
upload.dat <- filter(upload.dat, abs(diff) <= 1000)
download.dat$diff <- download.dat$actual_download - download.dat$advertised_download
download.dat <- filter(download.dat, abs(diff) <= 1000)

library(ggplot2)
ggplot(upload.dat, aes(isp, diff)) + geom_boxplot() + 
  geom_hline(yintercept=0, linetype='dashed', color='blue') +
  ggtitle('Difference between actual and advertised upload speed') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Internet Service Provider')
ggplot(download.dat, aes(isp, diff)) + geom_boxplot() +
  geom_hline(yintercept=0, linetype='dashed', color='blue') +
  ggtitle('Difference between actual and advertised download speed') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Internet Service Provider')

# Use bootstrap to get the median of the difference distribution plot
isps <- c('Atlas', 'Casecade', 'Centurylink', 'Comcast', 'Wave', 'Frontier', 'Other')
nisp <- length(isps)

B <- 10000
upload_diff <- data.frame(diff=rep(NA,B * nisp),
                          isp=rep(NA, B * nisp))
download_diff <- data.frame(diff=rep(NA,B * nisp),
                          isp=rep(NA, B * nisp))
set.seed(403)

# Upload
for (k in 1:nisp) {
  cur_isp <- isps[k]
  for (i in 1:B) {
    d <- filter(upload.dat, isp==cur_isp)
    w <- sample(length(d), length(d), replace=T)
    bt.dat <- d[w,]
    upload_diff[B*(k-1)+i,] <- c(median(bt.dat$diff),
                                 cur_isp)
  }
  print(cur_isp)
}
upload_diff$diff <- as.numeric(upload_diff$diff)
ggplot(upload_diff, aes(diff, fill=isp)) + geom_histogram(position='stack',
                                                          stat='bin',
                                                          bins=50) +
  ggtitle('Distribution of Bootstrap Median for Upload') + 
  xlab('Difference between advertised and actual speed') +
  theme(plot.title = element_text(hjust = 0.5))

# Download
for (k in 1:nisp) {
  cur_isp <- isps[k]
  for (i in 1:B) {
    d <- filter(download.dat, isp==cur_isp)
    w <- sample(length(d), length(d), replace=T)
    bt.dat <- d[w,]
    download_diff[B*(k-1)+i,] <- c(median(bt.dat$diff),
                                 cur_isp)
  }
  print(cur_isp)
}

download_diff$diff <- as.numeric(download_diff$diff)
ggplot(download_diff, aes(diff, fill=isp)) + geom_histogram(position='stack',
                                                                      stat='bin',
                                                                      bins=50) +
  ggtitle('Distribution of Bootstrap Median for Download') + 
  xlab('Difference between advertised and actual speed') +
  theme(plot.title = element_text(hjust = 0.5))

# 95% CI of the difference for each isp
# upload
Atlas.up <- filter(upload_diff, isp=='Atlas')
Case.up <- filter(upload_diff, isp=='Casecade')
Cent.up <- filter(upload_diff, isp=='Centurylink')
Com.up <- filter(upload_diff, isp=='Comcast')
Front.up <- filter(upload_diff, isp=='Frontier')
Wave.up <- filter(upload_diff, isp=='Wave')
Other.up <- filter(upload_diff, isp=='Other')

library(gridExtra)
ci <- quantile(Atlas.up$diff, prob=c(0.025, 0.975))
atlas.plt <- ggplot(Atlas.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                                          bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Atlas') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Case.up$diff, prob=c(0.025, 0.975))
case.plt <- ggplot(Case.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                             bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Casecade') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Cent.up$diff, prob=c(0.025, 0.975))
cent.plt <- ggplot(Cent.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                             bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Centurylink') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Com.up$diff, prob=c(0.025, 0.975))
com.plt <- ggplot(Com.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                             bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Comcast') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Front.up$diff, prob=c(0.025, 0.975))
front.plt <- ggplot(Front.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                             bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Frontier') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Wave.up$diff, prob=c(0.025, 0.975))
wave.plt <- ggplot(Wave.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                             bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Wave') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Other.up$diff, prob=c(0.025, 0.975))
other.plt <- ggplot(Other.up, aes(diff)) + geom_histogram(aes(y=..density..),
                                             bins=20,
                                             col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Other') + xlab('Difference of upload speed') +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(atlas.plt, case.plt, cent.plt, com.plt, front.plt, wave.plt, other.plt,
             nrow=4)

# download
Atlas.down <- filter(download_diff, isp=='Atlas')
Case.down <- filter(download_diff, isp=='Casecade')
Cent.down <- filter(download_diff, isp=='Centurylink')
Com.down <- filter(download_diff, isp=='Comcast')
Front.down <- filter(download_diff, isp=='Frontier')
Wave.down <- filter(download_diff, isp=='Wave')
Other.down <- filter(download_diff, isp=='Other')

ci <- quantile(Atlas.down$diff, prob=c(0.025, 0.975))
atlas.plt <- ggplot(Atlas.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                          bins=20,
                                                          col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Atlas') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Case.down$diff, prob=c(0.025, 0.975))
case.plt <- ggplot(Case.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                        bins=20,
                                                        col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Casecade') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Cent.down$diff, prob=c(0.025, 0.975))
cent.plt <- ggplot(Cent.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                        bins=20,
                                                        col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Centurylink') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Com.down$diff, prob=c(0.025, 0.975))
com.plt <- ggplot(Com.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                      bins=20,
                                                      col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Comcast') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Front.down$diff, prob=c(0.025, 0.975))
front.plt <- ggplot(Front.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                          bins=20,
                                                          col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Frontier') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Wave.down$diff, prob=c(0.025, 0.975))
wave.plt <- ggplot(Wave.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                        bins=20,
                                                        col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Wave') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

ci <- quantile(Other.down$diff, prob=c(0.025, 0.975))
other.plt <- ggplot(Other.down, aes(diff)) + geom_histogram(aes(y=..density..),
                                                          bins=20,
                                                          col='black', fill='cornsilk3') +
  geom_density(fill='red', alpha=0.2) +
  geom_vline(xintercept=ci[1], linetype='dashed', col='blue') +
  geom_vline(xintercept=ci[2], linetype='dashed', col='blue') +
  geom_vline(xintercept=0, linetype='longdash', col='red') +
  ggtitle('Other') + xlab('Difference of download speed') +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(atlas.plt, case.plt, cent.plt, com.plt, front.plt, wave.plt, other.plt,
             nrow=4)


