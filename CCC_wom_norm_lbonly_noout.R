setwd("~/Documents/cycle2/linemark/data/processed")

lb<-read.csv("lb.csv")
seg<-read.csv("seg_noout.csv")

#lb need studyid, RMEAN, LMEAN, ALLMEAN, Gender, Dx, AGEMONTHS
lbs<-lb[,c("StudyID","Gender", "Dx","AGEMONTHS","RMEAN","LMEAN","ALLMEAN")]
colnames(lbs)<-tolower(colnames(lbs))

#seg needs Measure.volume (that's actually studyid), CC_Posterior, CC_Mid_Posterior, CC_Central, CC_Mid_Anterior, CC_Anterior, CorticalWhiteMatterVol
segs<-seg[,c("Measure.volume","DX","CC_Posterior","CC_Mid_Posterior","CC_Central","CC_Mid_Anterior","CC_Anterior", "CorticalWhiteMatterVol")]
colnames(segs)<-tolower(colnames(segs))

segs_notnorm<-segs

segs$cc_posterior <- segs$cc_posterior/segs$corticalwhitemattervol
segs$cc_mid_posterior <- segs$cc_mid_posterior/segs$corticalwhitemattervol
segs$cc_central <- segs$cc_central/segs$corticalwhitemattervol
segs$cc_mid_anterior <- segs$cc_mid_anterior/segs$corticalwhitemattervol
segs$cc_anterior <- segs$cc_anterior/segs$corticalwhitemattervol


colnames(segs)[[1]]<-"studyid"
lb_seg<-merge(lbs, segs, by="studyid")

#or should we have two variables? left error and right error?
lb_seg$err_to_lft <- 0
lb_seg[which(lb_seg$allmean <0), "err_to_lft"]<- lb_seg[which(lb_seg$allmean < 0), "allmean"]
lb_seg$err_to_rgt <- 0
lb_seg[which(lb_seg$allmean > 0), "err_to_rgt"] <- lb_seg[which(lb_seg$allmean > 0), "allmean"]

#lft err, errors to the right are set to zero
llrfit_posterior_lb<-lm(err_to_lft~cc_posterior, data=lb_seg)
llrfit_mid_posterior_lb<-lm(err_to_lft~cc_mid_posterior, data=lb_seg)
llrfit_central_lb<-lm(err_to_lft~cc_central, data=lb_seg)
llrfit_mid_anterior_lb<-lm(err_to_lft~cc_mid_anterior, data=lb_seg)
llrfit_anterior_lb<-lm(err_to_lft~cc_anterior, data=lb_seg)

#rgt err, errors to the left are set to zero
rlrfit_posterior_lb<-lm(err_to_rgt ~cc_posterior, data=lb_seg)
rlrfit_mid_posterior_lb<-lm(err_to_rgt ~cc_mid_posterior, data=lb_seg)
rlrfit_central_lb<-lm(err_to_rgt ~cc_central, data=lb_seg)
rlrfit_mid_anterior_lb<-lm(err_to_rgt ~cc_mid_anterior, data=lb_seg)
rlrfit_anterior_lb<-lm(err_to_rgt ~cc_anterior, data=lb_seg)

#print results
summary(llrfit_posterior_lb)
summary(llrfit_mid_posterior_lb)
summary(llrfit_central_lb)
summary(llrfit_mid_anterior_lb)
summary(llrfit_anterior_lb)

p.adjust(c(0.0000217, 0.6506, 0.382, 0.0419, 0.0321))

summary(lm(err_to_lft ~ cc_posterior, data = lb_seg[which(lb_seg$dx.x == "22q"),]))
summary(lm(err_to_lft ~ cc_mid_posterior, data = lb_seg[which(lb_seg$dx.x == "22q"),]))
summary(lm(err_to_lft ~ cc_central, data = lb_seg[which(lb_seg$dx.x == "22q"),]))
summary(lm(err_to_lft ~ cc_mid_anterior, data = lb_seg[which(lb_seg$dx.x == "22q"),]))
summary(lm(err_to_lft ~ cc_anterior, data = lb_seg[which(lb_seg$dx.x == "22q"),]))

summary(lm(err_to_rgt ~ cc_posterior, data = lb_seg[which(lb_seg$dx.x == "TD"),]))
summary(lm(err_to_rgt ~ cc_mid_posterior, data = lb_seg[which(lb_seg$dx.x == "TD"),]))
summary(lm(err_to_rgt ~ cc_central, data = lb_seg[which(lb_seg$dx.x == "TD"),]))
summary(lm(err_to_rgt ~ cc_mid_anterior, data = lb_seg[which(lb_seg$dx.x == "TD"),]))
summary(lm(err_to_rgt ~ cc_anterior, data = lb_seg[which(lb_seg$dx.x == "TD"),]))
### don't need to redo SCA bc none were removed

write.csv(lb_seg, 'lb_seg.csv',row.names=F)