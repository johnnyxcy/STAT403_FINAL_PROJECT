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

dat[which(dat$isp_user=='Cascadelink Gigabit'),]$isp <- 'Casecade'
dat[which(dat$isp_user=='Cascadelink Gigabit Internet'),]$isp <- 'Casecade'
dat[which(dat$isp_user=='Cascadelink'),]$isp <- 'Casecade'
dat[which(dat$isp_user=='Cascade Link'),]$isp <- 'Casecade'
dat[which(dat$isp_user=='Cascadelink Gig'),]$isp <- 'Casecade'
dat[which(dat$isp_user=='cascadelink'),]$isp <- 'Casecade'

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

ggplot(upload_diff, aes(as.numeric(diff), fill=isp)) + geom_histogram(position='stack',
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

ggplot(download_diff, aes(as.numeric(diff), fill=isp)) + geom_histogram(position='stack',
                                                                      stat='bin',
                                                                      bins=50) +
  ggtitle('Distribution of Bootstrap Median for Download') + 
  xlab('Difference between advertised and actual speed') +
  theme(plot.title = element_text(hjust = 0.5))
  




