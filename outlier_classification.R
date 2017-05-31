
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

##############
###Landmark###
##############

###what is the mean in the big guys? is it lower or higher than the other guys?
###what is the correlation in the big guys? is it the same as in the other guys?

###what is the mean in the little guys? is it lower or higher than the other guys?


####################
###Line Bisection###
####################

###what is the mean in the big guys? is it lower or higher than the other guys?
###what is the correlation in the big guys? is it the same as in the other guys?

###what is the mean in the little guys? is it lower or higher than the other guys?