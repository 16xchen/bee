library(nlme)
#install.packages("glmm")
library(glmm)


#install_github("ggbiplot", "vqv")
library(ggbiplot)


install.packages('lme4')
library(lme4)

mydat_small=read.csv('pollinator_visitation_fullData.csv')
summary(mydat_small)


mydat_small$Experiment.Week=as.factor(mydat_small$Experiment.Week)
mydat_small$Plant.Number=as.factor(mydat_small$Plant.Number)
mydat_small$Pair=as.factor(mydat_small$Pair)


boo=c()
for (i in 1:ncol(mydat_small)){
  boo[i]=is.numeric(mydat_small[,i])
}



mydat_small_val=mydat_small[,boo]
#mydat_small_val=mydat_small_val[,-c(1)]


mydat_small_val_log=mydat_small_val
mydat_small_val_log$Honeybees=mydat_small_val_log$Honeybees+1
mydat_small_val_log$Speed=mydat_small_val_log$Speed+0.1
for(i in 1:ncol(mydat_small_val_log)){
  mydat_small_val_log[,i]=log10(mydat_small_val_log[,i])
}
# hist(mydat_small_val_log$Honeybees, breaks=20)
# qqnorm(mydat_small_val_log$Honeybees)
# qqnorm(mydat_small_val$Honeybees)

summary(mydat_small_val_log)


mydat_small_val_log$Pair=mydat_small$Pair
mydat_small_val_log$Plant.Number=mydat_small$Plant.Number
mydat_small_val_log$Experiment.Week=mydat_small$Experiment.Week
mydat_small_val_log$Size=mydat_small$Size
mydat_small_val_log$Date=mydat_small$Date
mydat_small_val_log$East=mydat_small$East
mydat_small_val_log$Clustered=mydat_small$Clustered
mydat_small_val_log$hotDay=mydat_small$hotDay


data_dt=split(mydat_small_val_log, mydat_small$Date)
###median as theshold value
theshold=c()
wholedf=data.frame()
for( i in 1:length(data_dt)){
  theshold[i]=median(data_dt[[i]]$Honeybees)
  data_dt[[i]]$visits=(data_dt[[i]]$Honeybees>=theshold[i])
  wholedf=rbind(wholedf, data_dt[[i]])
}

mydat_small_val_log$Visits=wholedf$visits


write.csv(mydat_small_val_log,'pollinator_visitation_fullData_logTrans.csv')

col=c('Size','Avg.open.flowers.per.inflorescence','Total.inflorescenses',
      'Volume','Sugar_conc','Sugar_content','Temperature','Clustered', 'East')

pdf("6-20-17/logplots_median_logTrans.pdf")
for (i in col){
  vars=mydat_small_val_log[,c('Date',i, "Visits")]
  names(vars)[2]=names(wholedf)[i]
  p=ggplot(data=vars, aes(x=vars[,2], y=Visits))+
    geom_jitter(height=0, width=0.15)+
    labs(x=i)+
    facet_wrap(~Date)
  plot(p)
}
graphics.off()

mylogit_med <- glmm(Visits ~  Avg.open.flowers.per.inflorescence+
                      Total.inflorescenses
                    ,  random = list( ~ 0 + factor(Pair), ~0+factor(East)), 
                    varcomps.names = c( "Pair",'East'),data = mydat_small_val_log, 
                    family.glmm = binomial.glmm, m = 10^4)
summary(mylogit_med)


mylogit <- glmm(Visits ~  Size+Clustered
                ,  random = list(~ 0 + factor(Pair), ~0+factor(East)), 
                varcomps.names = c( "Pair",'East'),data = mydat_small_val_log, 
                family.glmm = binomial.glmm, m = 10^4)





mylogit.null <- lmer(Honeybees ~  1+(1|Date)+(1|factor(mydat_small$Location)), data=mydat_small_val_log)
mylogit <- lmer(Honeybees ~
                  Total.inflorescenses+(1|Date)+(1|factor(mydat_small$Location)), data=mydat_small_val_log)
anova(mylogit.null,mylogit)


