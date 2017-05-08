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

d<-d[,c("studyid","cabilid","gender", "dx","age","dx2")]

proc_out<-c('2RO10SCAAF', '2RO11SCAAP', '2RO1SCAWR', '2RO23SCAPA', '2RO27SCAJD', '2RO3SCATP', '2RO5SCACM', '2RO9SCAAF','2RO13JW', '2RO24AO', '2RO36VS', '2RO38MS', '2RO44SF', '2RO46HG', '2RO60MM', '2RO61ER', '2RO71OL', '2RO74AR', '2RO9AJ','2RO13NormIL', '2RO17NormDF', '2RO30NormDJ', '2RO36NormAG', '2RO37NormJG', '2RO56NormLV', '2RO7NormKR')

proc_out_df <- data.frame(studyid<-proc_out)
colnames(proc_out_df)<-'studyid'

wasi <- read.csv('iq/WASI.csv')
wasi_ii <- read.csv('iq/WASI-II.csv')
wisc_iii <- read.csv('iq/WISC-III.csv')
wisc_iv <- read.csv('iq/WISC-IV.csv')

wasi<-wasi[,c("Study_ID","CABIL_ID","CABIL_WASI..Gender","CABIL_WASI..Diagnosis","STUDY..AgeAtVisitText")]

d_wasi <- merge(proc_out_df, wasi, by.x = "studyid", by.y = "Study_ID", all.x = F, all.y = F)

colnames(d_wasi)<-c("studyid","cabilid","gender","dx","age")

wisc_iv <- wisc_iv[,c("Study_ID","CABIL_ID","CABIL_WISCIV..Gender","CABIL_WISCIV..Diagnosis","STUDY..AgeAtVisitText")]

d_wisc_iv<-merge(proc_out_df, wisc_iv,by.x="studyid",by.y="Study_ID",all.x=F,all.y=F)


colnames(d_wisc_iv)<-c("studyid","cabilid","gender","dx","age")

proc_out_demo <- rbind(d_wasi, d_wisc_iv)

dxer2<-function(s){
	if(s=="Typical Developing"){
		return('1td')
	}
	if(s=="Trisomy X (XXX)"){
		return('XXX')
	}
	if(s=="Klinefelter's Syndrome (XXY)"){
		return('XXY')
	}
	if(s=="22q11.2DS"){
		return('q22')
	}
	else{
		return('ruh roh')
	}
}

proc_out_demo$dx<-unlist(lapply(proc_out_demo$dx, dxer2))

proc_out_demo$age<-as.character(proc_out_demo$age)

age_in_mo<-function(s){
	v<-unlist(strsplit(s, " "))
	yr<-as.integer(v[1])
	mo<-as.integer(v[3])
	out <- (yr*12)+mo
	return(out)
}

proc_out_demo$age<-unlist(lapply(proc_out_demo$age, age_in_mo))

proc_out_demo$dx2<-unlist(lapply(proc_out_demo$dx, dx2er))

d<-rbind(d,proc_out_demo)