setwd("~/Documents/Lab/Landmark Line Bisection/Data")

lm<-read.csv("lm.csv")
lb<-read.csv("lb.csv")
seg<-read.csv("seg.csv")

#lm need studyid, gender, dx, age, distance, and acc
lms<-lm[,c("studyid","Gender","DX","age","distance","acc")]
colnames(lms)<-tolower(colnames(lms))

#lb need studyid, RMEAN, LMEAN, ALLMEAN, Gender, Dx, AGEMONTHS
lbs<-lb[,c("StudyID","Gender", "Dx","AGEMONTHS","RMEAN","LMEAN","ALLMEAN")]
colnames(lbs)<-tolower(colnames(lbs))

#seg needs Measure.volume (that's actually studyid), CC_Posterior, CC_Mid_Posterior, CC_Central, CC_Mid_Anterior, CC_Anterior
segs<-seg[,c("Measure.volume","DX","CC_Posterior","CC_Mid_Posterior","CC_Central","CC_Mid_Anterior","CC_Anterior")]
colnames(segs)<-tolower(colnames(segs))

s22q<-which(segs$dx == 2)
sTD<-which(segs$dx == 1)

#probably need to know if CC volumes are different between groups
t.test(segs[s22q, "cc_posterior"], segs[sTD, "cc_posterior"])
t.test(segs[s22q, "cc_mid_posterior"], segs[sTD, "cc_mid_posterior"])
t.test(segs[s22q, "cc_central"], segs[sTD, "cc_central"])
t.test(segs[s22q, "cc_mid_anterior"], segs[sTD, "cc_mid_anterior"])
t.test(segs[s22q, "cc_anterior"], segs[sTD, "cc_anterior"])
#differences in everything except posterior

#need absolute distance for lms, not just "distance"
lms$absdist<-abs(lms$distance)
lms$distleft<-NA
lms[which(lms$distance < 0), "distleft"]<-lms[which(lms$distance < 0 ), "absdist"]
lms$distright<-NA
lms[which(lms$distance > 0), "distright"]<-lms[which(lms$distance > 0), "absdist"]

#absolute distance on lbs also?
lbs$absr<-abs(lbs$rmean)
lbs$absl<-abs(lbs$lmean)
lbs$absall<-abs(lbs$allmean)

#merge with segs
colnames(segs)[[1]]<-"studyid"
lm_seg<-merge(lms, segs, by="studyid")

library(nlme)
fit_posterior_lm<-lme(acc~absdist+cc_posterior, random=~1|studyid, data=lm_seg)
fit_mid_posterior_lm<-lme(acc~absdist+cc_mid_posterior, random=~1|studyid, data=lm_seg)
fit_central_lm<-lme(acc~absdist+cc_central, random=~1|studyid, data=lm_seg)
fit_mid_anterior_lm<-lme(acc~absdist+cc_mid_anterior, random=~1|studyid, data=lm_seg)
fit_anterior_lm<-lme(acc~absdist+cc_anterior, random=~1|studyid, data=lm_seg)
#mid anterior and mid posterior significant

lb_seg<-merge(lbs, segs, by="studyid")
fit_posterior_lb<-lm(absall~cc_posterior, data=lb_seg)
fit_mid_posterior_lb<-lm(absall~cc_mid_posterior, data=lb_seg)
fit_central_lb<-lm(absall~cc_central, data=lb_seg)
fit_mid_anterior_lb<-lm(absall~cc_mid_anterior, data=lb_seg)
fit_anterior_lb<-lm(absall~cc_anterior, data=lb_seg)

###but is that entirely drive by group differences?
dfit_posterior_lm<-lme(acc~absdist+cc_posterior+dx.y, random=~1|studyid, data=lm_seg)
dfit_mid_posterior_lm<-lme(acc~absdist+cc_mid_posterior+dx.y, random=~1|studyid, data=lm_seg)
dfit_central_lm<-lme(acc~absdist+cc_central+dx.y, random=~1|studyid, data=lm_seg)
dfit_mid_anterior_lm<-lme(acc~absdist+cc_mid_anterior+dx.y, random=~1|studyid, data=lm_seg)
dfit_anterior_lm<-lme(acc~absdist+cc_anterior+dx.y, random=~1|studyid, data=lm_seg)
#same as without Dx, but should still check if true within each Dx

dfit_posterior_lb<-lm(absall~cc_posterior+dx.y, data=lb_seg)
dfit_mid_posterior_lb<-lm(absall~cc_mid_posterior+dx.y, data=lb_seg)
dfit_central_lb<-lm(absall~cc_central+dx.y, data=lb_seg)
dfit_mid_anterior_lb<-lm(absall~cc_mid_anterior+dx.y, data=lb_seg)
dfit_anterior_lb<-lm(absall~cc_anterior+dx.y, data=lb_seg)

q22_fit_posterior_lm<-lme(acc~absdist+cc_posterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="22q"),])
q22_fit_mid_posterior_lm<-lme(acc~absdist+cc_mid_posterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="22q"),])
q22_fit_central_lm<-lme(acc~absdist+cc_central, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="22q"),])
q22_fit_mid_anterior_lm<-lme(acc~absdist+cc_mid_anterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="22q"),])
q22_fit_anterior_lm<-lme(acc~absdist+cc_anterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="22q"),])

td_fit_posterior_lm<-lme(acc~absdist+cc_posterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="td"),])
td_fit_mid_posterior_lm<-lme(acc~absdist+cc_mid_posterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="td"),])
td_fit_central_lm<-lme(acc~absdist+cc_central, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="td"),])
td_fit_mid_anterior_lm<-lme(acc~absdist+cc_mid_anterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="td"),])
td_fit_anterior_lm<-lme(acc~absdist+cc_anterior, random=~1|studyid, data=lm_seg[which(lm_seg$dx.x=="td"),])

q22_fit_posterior_lb<-lm(absall~cc_posterior, data=lb_seg[which(lb_seg$dx.x=="22q"),])
q22_fit_mid_posterior_lb<-lm(absall~cc_mid_posterior, data=lb_seg[which(lb_seg$dx.x=="22q"),])
q22_fit_central_lb<-lm(absall~cc_central, data=lb_seg[which(lb_seg$dx.x=="22q"),])
q22_fit_mid_anterior_lb<-lm(absall~cc_mid_anterior, data=lb_seg[which(lb_seg$dx.x=="22q"),])
q22_fit_anterior_lb<-lm(absall~cc_anterior, data=lb_seg[which(lb_seg$dx.x=="22q"),])


td_fit_posterior_lb<-lm(absall~cc_posterior, data=lb_seg[which(lb_seg$dx.x=="TD"),])
td_fit_mid_posterior_lb<-lm(absall~cc_mid_posterior, data=lb_seg[which(lb_seg$dx.x=="TD"),])
td_fit_central_lb<-lm(absall~cc_central, data=lb_seg[which(lb_seg$dx.x=="TD"),])
td_fit_mid_anterior_lb<-lm(absall~cc_mid_anterior, data=lb_seg[which(lb_seg$dx.x=="TD"),])
td_fit_anterior_lb<-lm(absall~cc_anterior, data=lb_seg[which(lb_seg$dx.x=="TD"),])

#plotting

for(i in seq(0, 16, by = 2)){
	quartz()
	plot(lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="22q"), "cc_mid_posterior"], lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="22q"), "acc"], pch = 19, col="hotpink", main=paste("LandMark Data at distance", i), xlab="Corpus Callosum Mid-Posterior", ylab= "Accuracy", ylim=c(0,100))
	points(lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="td"), "cc_mid_posterior"], lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="td"), "acc"], pch = 19, col="blue")
	d <- lm_seg[which(lm_seg$absdist==i),]
	lines(sort(lm_seg[which(lm_seg$absdist==i),"cc_mid_posterior"]), predict(lm(acc~cc_mid_posterior, data=d), d[order(d$cc_mid_posterior),]))
}
for(i in seq(0, 16, by = 2)){
	quartz()
	plot(lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="22q"), "cc_mid_anterior"], lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="22q"), "acc"], pch = 19, col="hotpink", main=paste("LandMark Data at distance", i), xlab="Corpus Callosum Mid-Anterior", ylab= "Accuracy", ylim=c(0,100))
	points(lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="td"), "cc_mid_anterior"], lm_seg[which(lm_seg$absdist == i & lm_seg$dx.x=="td"), "acc"], pch = 19, col="blue")
	d <- lm_seg[which(lm_seg$absdist==i),]
	lines(sort(lm_seg[which(lm_seg$absdist==i),"cc_mid_anterior"]), predict(lm(acc~cc_mid_anterior, data=d), d[order(d$cc_mid_anterior),]))
}
quartz()
plot(1,1, xlim = c(0,10), ylim= c(0,10))
legend(2, 9, c("22q (n = 54)", "TD (n = 34)"), pch = c(19, 19), col = c("hotpink","blue"))

###follow-ups###
# don't collapse across left and right in line bisection
#are there outliers in landmark

#allmean
lrfit_posterior_lb<-lm(allmean~cc_posterior, data=lb_seg)
lrfit_mid_posterior_lb<-lm(allmean~cc_mid_posterior, data=lb_seg)
lrfit_central_lb<-lm(allmean~cc_central, data=lb_seg)
lrfit_mid_anterior_lb<-lm(allmean~cc_mid_anterior, data=lb_seg)
lrfit_anterior_lb<-lm(allmean~cc_anterior, data=lb_seg)

lb_seg$allmeansq <- lb_seg$allmean^2

lrfit_posterior_lb_sq<-lm(cc_posterior~allmean*allmeansq, data=lb_seg)
lrfit_mid_posterior_lb_sq<-lm(cc_mid_posterior~allmean*allmeansq, data=lb_seg)
lrfit_central_lb_sq<-lm(cc_central~allmean*allmeansq, data=lb_seg)
lrfit_mid_anterior_lb_sq<-lm(cc_mid_anterior~allmean*allmeansq, data=lb_seg)
lrfit_anterior_lb_sq<-lm(cc_anterior~allmean*allmeansq, data=lb_seg)
