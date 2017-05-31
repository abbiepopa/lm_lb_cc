
small<-c('2RO30TR', '2RO15NormOW','2RO66CP')
big<-c('2RO39DH', '2RO54DF','2RO70MK', '2RO10NormMW', '2RO48NormIK', '2RO74AR','2RO47JT','2RO15JB','2RO43LR')

setwd('~/Documents/cycle2/linemark/data/processed')
lm<-read.csv('lm_lb.csv')
colnames(lm)<-tolower(colnames(lm))
lb<-read.csv('lb.csv')
colnames(lb)<-tolower(colnames(lb))
colnames(lb)[39]<-'age'
seg_all<-read.csv("seg.csv")
seg_noout<-read.csv('seg_noout.csv')

demo_cols <- c('studyid', 'cabilid','gender','dx','age')

small_rows <- rbind(lb[which(lb$studyid == '2RO30TR'),demo_cols], lm[which(lm$studyid == '2RO15NormOW'),demo_cols])
small_rows <- rbind(small_rows, lm[which(lm$studyid == '2RO66CP'), demo_cols])


kidder<-function(kid){
	return(lm[which(lm$studyid == kid), demo_cols])
}

l <- lapply(big, kidder)

big_rows <- do.call(rbind.data.frame, l)

big_rows <- rbind(big_rows, lb[which(lb$studyid == '2RO54DF'),demo_cols])

big_rows <- rbind(big_rows, lb[which(lb$studyid == '2RO47JT'),demo_cols])

lm_noout<-merge(lm, seg_noout, all.x = F, all.y = F, by.x = 'studyid', by.y = 'Measure.volume')

lb_noout<-merge(lb, seg_noout, all.x = F, all.y = F, by.x = 'studyid', by.y = 'Measure.volume')

###What are the big and little guys like demographically?
table(small_rows$gender)
table(big_rows$gender)
table(lb_noout$gender)

#small fisher
fisher.test(rbind(c(0,3), c(54, 63)))
chisq.test(rbind(c(5,4), c(54, 63)))

library(psych)
describe(big_rows$age)
describe(small_rows$age)

t.test(big_rows$age, lb_noout$age)
t.test(small_rows$age, lb_noout$age)

##set up normed segs
segs<-seg_all[,c("Measure.volume","DX","CC_Posterior","CC_Mid_Posterior","CC_Central","CC_Mid_Anterior","CC_Anterior", "CorticalWhiteMatterVol")]
colnames(segs)<-tolower(colnames(segs))

segs_notnorm<-segs

segs$cc_posterior <- segs$cc_posterior/segs$corticalwhitemattervol
segs$cc_mid_posterior <- segs$cc_mid_posterior/segs$corticalwhitemattervol
segs$cc_central <- segs$cc_central/segs$corticalwhitemattervol
segs$cc_mid_anterior <- segs$cc_mid_anterior/segs$corticalwhitemattervol
segs$cc_anterior <- segs$cc_anterior/segs$corticalwhitemattervol

big_segs<-merge(big_rows, segs, by.x = 'studyid', by.y = 'measure.volume')

##############
###Landmark###
##############

###what is the mean in the big guys? is it lower or higher than the other guys?
biglm <- merge(big_rows, lm, all.x = F, all.y = F)
describe(biglm[,c('left_d_out_in', 'right_d_out_in')])
t.test(biglm$left_d_out_in, lm_noout$left_d_out_in)
t.test(biglm$right_d_out_in, lm_noout$right_d_out_in)

###what is the correlation in the big guys? is it the same as in the other guys?
big_segs_lm <- merge(big_segs, lm)

summary(lm(left_d_out_in~cc_posterior, data = big_segs_lm))
summary(lm(left_d_out_in~cc_mid_posterior, data = big_segs_lm))
summary(lm(left_d_out_in~cc_central, data = big_segs_lm))
summary(lm(left_d_out_in~cc_mid_anterior, data = big_segs_lm))
summary(lm(left_d_out_in~cc_anterior, data = big_segs_lm))


summary(lm(right_d_out_in~cc_posterior, data = big_segs_lm))
summary(lm(right_d_out_in~cc_mid_posterior, data = big_segs_lm))
summary(lm(right_d_out_in~cc_central, data = big_segs_lm))
summary(lm(right_d_out_in~cc_mid_anterior, data = big_segs_lm))
summary(lm(right_d_out_in~cc_anterior, data = big_segs_lm))


###what is the mean in the little guys? is it lower or higher than the other guys?
smalllm <- merge(small_rows, lm, all.x = F, all.y = F)
describe(smalllm[,c('left_d_out_in', 'right_d_out_in')])
t.test(smalllm$left_d_out_in, lm_noout$left_d_out_in)
t.test(smalllm$right_d_out_in, lm_noout$right_d_out_in)

####################
###Line Bisection###
####################

###what is the mean in the big guys? is it lower or higher than the other guys?
###what is the correlation in the big guys? is it the same as in the other guys?

###what is the mean in the little guys? is it lower or higher than the other guys?