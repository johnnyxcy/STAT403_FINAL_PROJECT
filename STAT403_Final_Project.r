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
