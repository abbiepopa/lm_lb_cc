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

lm_tout <- c('2RO24SCAEL', '2RO22SCANB','2RO20SCASF','2RO42SK','2RO41RH','2RO40JC','2RO39DH','2RO37JM','2RO35AJ', '2RO34NormAW','2RO32NormMK','2RO31NormTS','2RO29NormSJ','2RO27NormAL')

lmtout_gone <- function(s){
	if(s %in% lm_tout){
		return(1)
	}
	return(0)
}

d$lm_tast_out <- unlist(lapply(d$studyid, lmtout_gone))

d_in <- d
d <- d[which(d$lm_tast_out == 0),]

###Landmark###
##SCA separate##
#left side#
pairwise.t.test(x = d$LEFT_d_out_in, g = d$dx, p.adjust.method = 'none')
#right side#
pairwise.t.test(x = d$RIGHT_d_out_in, g = d$dx, p.adjust.method = 'none')
##SCA together##
#left side#
pairwise.t.test(x = d$LEFT_d_out_in, g = d$dx2, p.adjust.method = 'none')
#right side#
pairwise.t.test(x = d$RIGHT_d_out_in, g = d$dx2, p.adjust.method = 'none')