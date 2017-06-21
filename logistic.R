# install.packages('gsheet')
# install.packages("ggplot2")
# install.packages("reshape2")
#install.packages('corrplot')
# install.packages('nlme')
# install.packages('Deducer')
library(Deducer)
library(corrplot)
library(ggplot2)
library(GGally)
library(reshape2)
library(gsheet)
#install.packages('GenABEL')
library(GenABEL)
library(nlme)
#install.packages("glmm")
library(glmm)
data=read.csv('pollinator_visitation_fullData.csv')
head(data)
summary(data)
data_dt=split(data, data$Date)



###median as theshold value
theshold=c()
wholedf=data.frame()
for( i in 1:length(data_dt)){
  theshold[i]=median(data_dt[[i]]$Honeybees)
  data_dt[[i]]$visits=(data_dt[[i]]$Honeybees>=theshold[i])
  wholedf=rbind(wholedf, data_dt[[i]])
}


boo=c()
for (i in 1:ncol(wholedf)){
  boo[i]=is.numeric(wholedf[,i])|is.logical(wholedf[,i])
}

names(wholedf)


pdf("6-20-17/logplots_median.pdf")
for (i in which(boo)){
  vars=wholedf[,c(1,i, 29)]
  names(vars)[2]=names(wholedf)[i]
  p=ggplot(data=vars, aes(x=vars[,2], y=visits))+
      geom_jitter(height=0, width=0.15)+
      labs(x=names(vars)[2])+
      facet_wrap(~Date)
    plot(p)
}
graphics.off()






pdf("6-20-17/logplots_median_allDays.pdf")
for (i in which(boo)){
  vars=wholedf[,c(1,i, 29)]
  names(vars)[2]=names(wholedf)[i]
  p=ggplot(data=vars, aes(x=vars[,2], y=visits))+
    geom_jitter(height=0, width=0.15)+
    labs(x=names(vars)[2])
   # facet_wrap(~Date)
  plot(p)
}
graphics.off()

###



pdf("6-20-17/logplots_median.pdf")
for (i in col){
  vars=wholedf[,c(2,i, 14)]
  names(vars)[2]=names(wholedf)[i]
  p=ggplot(data=vars, aes(x=vars[,2], y=visits))+
    geom_jitter(height=0, width=0.15)+
    labs(x=names(vars)[2])+
    facet_wrap(~Date)
  plot(p)
}
graphics.off()




###glmm and lmes


#wholedf$visits=as.numeric(visits)
mylogit_med <- glmm(visits ~  Avg.open.flowers.per.inflorescence+
                      Total.inflorescenses
                    ,  random = list( ~ 0 + factor(Pair), ~0+factor(East)), 
                    varcomps.names = c( "Location", 'East'),data = wholedf, 
                    family.glmm = binomial.glmm, m = 10^4)
summary(mylogit_med)



mylogit_med <- glmm(visits ~  Size
                    ,  random = list(~ 0 + factor(Pair),~0+factor(East)), 
                    varcomps.names = c( "Pair", "East"),data = wholedf, 
                    family.glmm = binomial.glmm, m = 10^4)
summary(mylogit_med)

