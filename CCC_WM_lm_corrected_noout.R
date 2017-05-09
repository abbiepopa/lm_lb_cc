setwd("~/Documents/cycle2/linemark/data/processed")

lm<-read.csv("lm_lb.csv")
seg<-read.csv("seg_noout.csv")

lm<-lm[,c("studyid","cabilid","LEFT_d_out_in", "RIGHT_d_out_in", "dx")]

segs<-seg[,c("Measure.volume","DX","CC_Posterior","CC_Mid_Posterior","CC_Central","CC_Mid_Anterior","CC_Anterior", "CorticalWhiteMatterVol")]
colnames(segs)<-tolower(colnames(segs))

segs_notnorm<-segs

segs$cc_posterior <- segs$cc_posterior/segs$corticalwhitemattervol
segs$cc_mid_posterior <- segs$cc_mid_posterior/segs$corticalwhitemattervol
segs$cc_central <- segs$cc_central/segs$corticalwhitemattervol
segs$cc_mid_anterior <- segs$cc_mid_anterior/segs$corticalwhitemattervol
segs$cc_anterior <- segs$cc_anterior/segs$corticalwhitemattervol


#merge with segs
colnames(segs)[[1]]<-"studyid"
lm_seg<-merge(lm, segs, by="studyid")

summary(lm(LEFT_d_out_in~cc_posterior, data = lm_seg))
summary(lm(LEFT_d_out_in~cc_mid_posterior, data = lm_seg))
summary(lm(LEFT_d_out_in~cc_central, data = lm_seg))
summary(lm(LEFT_d_out_in~cc_mid_anterior, data = lm_seg))
summary(lm(LEFT_d_out_in~cc_anterior, data = lm_seg))
###anterior highly correlated

summary(lm(RIGHT_d_out_in~cc_posterior, data = lm_seg))
summary(lm(RIGHT_d_out_in~cc_mid_posterior, data = lm_seg))
summary(lm(RIGHT_d_out_in~cc_central, data = lm_seg))
summary(lm(RIGHT_d_out_in~cc_mid_anterior, data = lm_seg))
summary(lm(RIGHT_d_out_in~cc_anterior, data = lm_seg))
###anterior trend

i22q <- which(lm_seg$dx.x == "22q")
itd <- which(lm_seg$dx.x == "TD")
ixxx <- which(lm_seg$dx.x == "XXX")
ixxy <- which(lm_seg$dx.x == "XXY")
isca <- which(lm_seg$dx.x == "XXX" | lm_seg$dx.x == "XXY")

lm_seg_er <- function(i){
	print('LEFT')
	print(summary(lm(LEFT_d_out_in~cc_posterior, data = lm_seg[i,])))
	print(summary(lm(LEFT_d_out_in~cc_mid_posterior, data = lm_seg[i,])))
	print(summary(lm(LEFT_d_out_in~cc_central, data = lm_seg[i,])))
	print(summary(lm(LEFT_d_out_in~cc_mid_anterior, data = lm_seg[i,])))
	print(summary(lm(LEFT_d_out_in~cc_anterior, data = lm_seg[i,])))
	
	print('RIGHT')
	print(summary(lm(RIGHT_d_out_in~cc_posterior, data = lm_seg[i,])))
	print(summary(lm(RIGHT_d_out_in~cc_mid_posterior, data = lm_seg[i,])))
	print(summary(lm(RIGHT_d_out_in~cc_central, data = lm_seg[i,])))
	print(summary(lm(RIGHT_d_out_in~cc_mid_anterior, data = lm_seg[i,])))
	print(summary(lm(RIGHT_d_out_in~cc_anterior, data = lm_seg[i,])))
}

print("22q")
lm_seg_er(i22q)
#mid-anterior correlated with left

print("TD")
lm_seg_er(itd)
#anterior trend, left

print("XXX")
lm_seg_er(ixxx)
#nothing for xxx alone

print("XXY")
lm_seg_er(ixxy)
#nothing for xxy alone

print("SCA")
lm_seg_er(isca)
#nothing for sca alone