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
pairwise.t.test(x = d$RMEAN,g = d$dx, p.adjust.method = 'none')
#left hand#
pairwise.t.test(x = d$LMEAN,g = d$dx, p.adjust.method = 'none')
#both hands#
pairwise.t.test(x = d$ALLMEAN,g = d$dx, p.adjust.method = 'none')

##SCA combined##
#right hand#
pairwise.t.test(x = d$RMEAN,g = d$dx2, p.adjust.method = 'none')
#left hand#
pairwise.t.test(x = d$LMEAN,g = d$dx2, p.adjust.method = 'none')
#both hands#
pairwise.t.test(x = d$ALLMEAN,g = d$dx2, p.adjust.method = 'none')

#follow-up for right hand 22q vs td since that is what's significant#
t.test(d[which(d$dx == "q22"),"RMEAN"], d[which(d$dx == "1td"),"RMEAN"])

library(psych)
(describeBy(d$RMEAN, d$dx)$`1td`[['mean']] - describeBy(d$RMEAN, d$dx)$`q22`[['mean']])/describe(d$RMEAN)[['sd']]

###Landmark###
##SCA separate##
#left side#
pairwise.t.test(x = d$LEFT_d_out_in, g = d$dx, p.adjust.method = 'none')
pairwise.t.test(x = d$LEFT_d_in_out, g = d$dx, p.adjust.method = 'none')
#right side#
pairwise.t.test(x = d$RIGHT_d_out_in, g = d$dx, p.adjust.method = 'none')
pairwise.t.test(x = d$RIGHT_d_in_out, g = d$dx, p.adjust.method = 'none')
##SCA together##
#left side#
pairwise.t.test(x = d$LEFT_d_out_in, g = d$dx2, p.adjust.method = 'none')
pairwise.t.test(x = d$LEFT_d_in_out, g = d$dx2, p.adjust.method = 'none')
#right side#
pairwise.t.test(x = d$RIGHT_d_out_in, g = d$dx2, p.adjust.method = 'none')
pairwise.t.test(x = d$RIGHT_d_in_out, g = d$dx2, p.adjust.method = 'none')

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