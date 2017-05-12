included <-c((75 - 11), (46 - 3), (17+23))
outliers <- c(11, 3, 0)

d<-rbind (included, outliers)
colnames(d)<-c("q22","td","sca")

setwd("~/Documents/Lab/Landmark Line Bisection/Data")

lm<-read.csv("lm.csv")
lb<-read.csv("lb.csv")
seg<-read.csv("seg_noout.csv")

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
