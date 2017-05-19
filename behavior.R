setwd("~/Documents/cycle2/linemark/data/processed")
d<-read.csv("lm_lb.csv")

d<-d[,c("studyid","cabilid","LEFT_d_out_in","LEFT_d_in_out","RIGHT_d_out_in","RIGHT_d_in_out","gender","dx","age", "RMEAN", "LMEAN","ALLMEAN", "HANDEDNESS","Handedness_score")]

dxer <- function(s){
	if(s == "22q"){
		return("q22")
	}
	if(s == "TD"){
		return("1td")
	}
	return(s)
}

d$dx <- unlist(lapply(as.character(d$dx),dxer))

dx2er <- function(s){
	if(s == "XXX" | s == "XXY"){
		return("sca")
	}
	return(s)
}

d$dx2 <- unlist(lapply(d$dx, dx2er))

###Line Bisection###
##SCA separate##
#right hand#
pairwise.t.test(x = d$RMEAN,g = d$dx, p.adjust.method = 'fdr')
#left hand#
pairwise.t.test(x = d$LMEAN,g = d$dx, p.adjust.method = 'fdr')
#both hands#
pairwise.t.test(x = d$ALLMEAN,g = d$dx, p.adjust.method = 'fdr')
pairwise.t.test(x = d$ALLMEAN,g = d$dx, p.adjust.method = 'none')

##SCA combined##
#right hand#
pairwise.t.test(x = d$RMEAN,g = d$dx2, p.adjust.method = 'fdr')
#left hand#
pairwise.t.test(x = d$LMEAN,g = d$dx2, p.adjust.method = 'fdr')
#both hands#
pairwise.t.test(x = d$ALLMEAN,g = d$dx2, p.adjust.method = 'fdr')
pairwise.t.test(x = d$ALLMEAN,g = d$dx2, p.adjust.method = 'none')

#follow-up for right hand 22q vs td since that is what's significant#
t.test(d[which(d$dx == "q22"),"RMEAN"], d[which(d$dx == "1td"),"RMEAN"])

t.test(d[which(d$dx == "q22"),"ALLMEAN"], d[which(d$dx == "1td"),"ALLMEAN"])

library(psych)
(describeBy(d$RMEAN, d$dx)$`1td`[['mean']] - describeBy(d$RMEAN, d$dx)$`q22`[['mean']])/describe(d$RMEAN)[['sd']]

library(psych)
(describeBy(d$ALLMEAN, d$dx)$`1td`[['mean']] - describeBy(d$ALLMEAN, d$dx)$`q22`[['mean']])/describe(d$ALLMEAN)[['sd']]

###for graph###
library(psych)
tab<-describeBy(d[,c('ALLMEAN','RMEAN','LMEAN')],group=d$dx2)
tab_out<-data.frame(c("td","td","td", "22q","22q","22q","sca","sca","sca"),
c("both","right","left","both","right","left","both","right","left"),
c(tab$`1td`[,'mean'], tab$`q22`[,'mean'], tab$`sca`[,'mean']),
c(tab$`1td`[,'sd'], tab$`q22`[,'sd'], tab$`sca`[,'sd']),
c(tab$`1td`[,'se'], tab$`q22`[,'se'], tab$`sca`[,'se']),
c(tab$`1td`[,'n'], tab$`q22`[,'n'], tab$`sca`[,'n']) )
colnames(tab_out)<-c("dx","hand","mean","sd","se","n")
tab_out$mean <- abs(tab_out$mean)
write.csv(tab_out, "linebisection_for_graph.csv", row.names=F)

###Landmark###
##SCA separate##
#left side#
pairwise.t.test(x = d$LEFT_d_out_in, g = d$dx, p.adjust.method = 'fdr')
#pairwise.t.test(x = d$LEFT_d_in_out, g = d$dx, p.adjust.method = 'none')
#right side#
pairwise.t.test(x = d$RIGHT_d_out_in, g = d$dx, p.adjust.method = 'fdr')
#pairwise.t.test(x = d$RIGHT_d_in_out, g = d$dx, p.adjust.method = 'none')
##SCA together##
#left side#
pairwise.t.test(x = d$LEFT_d_out_in, g = d$dx2, p.adjust.method = 'fdr')
#pairwise.t.test(x = d$LEFT_d_in_out, g = d$dx2, p.adjust.method = 'none')
#right side#
pairwise.t.test(x = d$RIGHT_d_out_in, g = d$dx2, p.adjust.method = 'fdr')
#pairwise.t.test(x = d$RIGHT_d_in_out, g = d$dx2, p.adjust.method = 'none')

#f-u for left out in (22q vs every group) and right out in (22q vs td, XXX, and SCA together)

i22q <- which(d$dx == "q22")
itd <- which(d$dx == "1td")
ixxx <- which(d$dx == "XXX")
ixxy <- which(d$dx == "XXY")
isca <- which(d$dx2 == "sca")

t.test(d[i22q, "LEFT_d_out_in"], d[itd, "LEFT_d_out_in"])
(describe(d[i22q, "LEFT_d_out_in"])[['mean']] - describe(d[itd, "LEFT_d_out_in"])[['mean']])/describe(d$LEFT_d_out_in)[['sd']]

t.test(d[i22q, "LEFT_d_out_in"], d[ixxx, "LEFT_d_out_in"])
(describe(d[i22q, "LEFT_d_out_in"])[['mean']] - describe(d[ixxx, "LEFT_d_out_in"])[['mean']])/describe(d$LEFT_d_out_in)[['sd']]

t.test(d[i22q, "LEFT_d_out_in"], d[ixxy, "LEFT_d_out_in"])
(describe(d[i22q, "LEFT_d_out_in"])[['mean']] - describe(d[ixxy, "LEFT_d_out_in"])[['mean']])/describe(d$LEFT_d_out_in)[['sd']]

t.test(d[i22q, "LEFT_d_out_in"], d[isca, "LEFT_d_out_in"])
(describe(d[i22q, "LEFT_d_out_in"])[['mean']] - describe(d[isca, "LEFT_d_out_in"])[['mean']])/describe(d$LEFT_d_out_in)[['sd']]


t.test(d[i22q, "RIGHT_d_out_in"], d[itd, "RIGHT_d_out_in"])
(describe(d[i22q, "RIGHT_d_out_in"])[['mean']] - describe(d[itd, "RIGHT_d_out_in"])[['mean']])/describe(d$RIGHT_d_out_in)[['sd']]

t.test(d[i22q, "RIGHT_d_out_in"], d[ixxx, "RIGHT_d_out_in"])
(describe(d[i22q, "RIGHT_d_out_in"])[['mean']] - describe(d[ixxx, "RIGHT_d_out_in"])[['mean']])/describe(d$RIGHT_d_out_in)[['sd']]

t.test(d[i22q, "RIGHT_d_out_in"], d[isca, "RIGHT_d_out_in"])
(describe(d[i22q, "RIGHT_d_out_in"])[['mean']] - describe(d[isca, "RIGHT_d_out_in"])[['mean']])/describe(d$RIGHT_d_out_in)[['sd']]

###Landmark Graph###
lm_tab<-describeBy(d[,c("LEFT_d_out_in","RIGHT_d_out_in")], group = d$dx2)
lm_tab_out<-data.frame(
c("td","td","22q","22q", "sca","sca"),
c("left","right","left","right","left","right"),
c(lm_tab$`1td`[,'mean'],lm_tab$`q22`[,'mean'],lm_tab$`sca`[,'mean']),
c(lm_tab$`1td`[,'sd'],lm_tab$`q22`[,'sd'],lm_tab$`sca`[,'sd']),
c(lm_tab$`1td`[,'se'],lm_tab$`q22`[,'se'],lm_tab$`sca`[,'se']),
c(lm_tab$`1td`[,'n'],lm_tab$`q22`[,'n'],lm_tab$`sca`[,'n']))

colnames(lm_tab_out)<-c("dx","side","mean","sd","se","n")

lm_tab_out$mean <-abs(lm_tab_out$mean)

write.csv(lm_tab_out, "landmark_for_graph.csv",row.names=F)