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

#absolute distance on lbs also?