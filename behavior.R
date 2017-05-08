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