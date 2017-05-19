setwd('~/Documents/cycle2/linemark/data/processed')

d<-read.csv('lm.csv')

library(psych)

tab_22q <- describeBy(d[which(d$DX == '22q'), 'acc'], group = d[which(d$DX == '22q'), 'distance'])

tab_td <- describeBy(d[which(d$DX == 'td'), 'acc'], group = d[which(d$DX == 'td'), 'distance'])

tab_sca <- describeBy(d[which(d$DX == 'xxx' | d$DX == 'xxy'), 'acc'], group = d[which(d$DX == 'xxx' | d$DX == 'xxy'), 'distance'])


collapser<-function(l, column){
	out<-rep(NA, length(l))
	i = 1
	for(r in l){
		out[i]<-r[column]
		i = i + 1
	}
	return(out)
}

plt_22q<-data.frame(seq(-16,16, by = 2), unlist(collapser(tab_22q, 'mean')),  unlist(collapser(tab_22q, 'se')))

colnames(plt_22q)<-c('dist', 'mean','se')
write.csv(plt_22q, 'plot_22q_zoi.csv', row.names=F)

plt_td<-data.frame(seq(-16,16, by = 2), unlist(collapser(tab_td, 'mean')),  unlist(collapser(tab_td, 'se')))

colnames(plt_td)<-c('dist', 'mean','se')
write.csv(plt_td, 'plot_td_zoi.csv', row.names=F)

plt_sca<-data.frame(seq(-16,16, by = 2), unlist(collapser(tab_sca, 'mean')),  unlist(collapser(tab_sca, 'se')))

colnames(plt_sca)<-c('dist', 'mean','se')
write.csv(plt_sca, 'plot_sca_zoi.csv', row.names=F)