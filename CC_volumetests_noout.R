included <-c((75 - 11), (46 - 3), (17+23))
outliers <- c(11, 3, 0)

d<-rbind (included, outliers)
colnames(d)<-c("q22","td","sca")

setwd("~/Documents/cycle2/linemark/data/processed")

setwd("~/Documents/cycle2/linemark/data/processed")

d<-read.csv("lm_lb.csv")
seg<-read.csv("seg_noout.csv")
sid<-d['studyid']


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

segs<-merge(sid, segs, by = 'studyid', all.x = F, all.y = F)

#same
pairwise.t.test(segs$cc_anterior, segs$dx, p.adjust.method = "fdr")

#mid-anterior different
pairwise.t.test(segs$cc_mid_anterior, segs$dx, p.adjust.method = "none")
pairwise.t.test(segs$cc_mid_anterior, segs$dx, p.adjust.method = "fdr")

spec_t_test_sca <- function(g1,g2,cc){
	print(t.test(segs[which(segs$dx == g1), cc], segs[which(segs$dx == g2), cc]))
	print((mean(segs[which(segs$dx == g1), cc]) - mean(segs[which(segs$dx == g2), cc]))/sd(segs[,cc]))
}

spec_t_test_sca(3,1,"cc_mid_anterior")
#central different
pairwise.t.test(segs$cc_central, segs$dx, p.adjust.method = "none")
pairwise.t.test(segs$cc_central, segs$dx, p.adjust.method = "fdr")
spec_t_test_sca(3,1,"cc_central")


#mid posterior different
pairwise.t.test(segs$cc_mid_posterior, segs$dx, p.adjust.method = "none")
pairwise.t.test(segs$cc_mid_posterior, segs$dx, p.adjust.method = "fdr")


pairwise.t.test(segs$cc_posterior, segs$dx, p.adjust.method = "fdr")

###for plotting###
library(psych)
tab<-describeBy(segs[,c("cc_posterior","cc_mid_posterior","cc_central","cc_mid_anterior","cc_anterior")], group = segs$dx, digits = 15)

plt<-data.frame(c(rep('td',5),rep('22q',5),rep('sca',5)),
rep(c('cc_posterior','cc_mid_posterior','cc_central','cc_mid_anterior','cc_anterior'), 3),
c(tab$`1`[['mean']],tab$`2`[['mean']],tab$`3`[['mean']]),
c(tab$`1`[['sd']],tab$`2`[['sd']],tab$`3`[['sd']]),
c(tab$`1`[['se']],tab$`2`[['se']],tab$`3`[['se']]),
c(tab$`1`[['n']],tab$`2`[['n']],tab$`3`[['n']])
)

colnames(plt)<-c("dx","region","mean","sd","se","n")
write.csv(plt, "CC_region_plots.csv",row.names = F)