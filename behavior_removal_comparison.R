included <- c((75-11), (46 -7), (17+23 -8))
outliers <- c(11,7,8)

d<-rbind(included,outliers)
colnames(d)<-c("q22","td","sca")

fisher.test(d)
chisq.test(d)