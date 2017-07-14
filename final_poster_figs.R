library(nlme)
#install.packages("glmm")
library(glmm)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)
library(lme4)



setwd('../bee')
logData=read.csv('pollinator_visitation_fullData_logTrans.csv')
head(logData)


data_dt=split(logData, logData$Date)


theshold=c()
wholedf=data.frame()
for( i in 1:length(data_dt)){
  theshold[i]=median(data_dt[[i]]$Honeybees)
  data_dt[[i]]$Median=theshold[i]
  wholedf=rbind(wholedf, data_dt[[i]])
}
logData=wholedf


system('mkdir 7-13-2017')
png('7-13-2017/scatter_classic.png')
ggplot(data=logData, aes(x='', y=Honeybees, color=Size))+
  geom_jitter(width = 0.15)+
  facet_grid(~Date)+
  geom_hline(aes(yintercept = Median),lty=2)+
  labs(x="Days")+
  theme_classic()
graphics.off()



png('7-13-2017/scatter_gray.png')
ggplot(data=logData, aes(x='', y=Honeybees, color=Size))+
  geom_jitter(width = 0.15)+
  facet_grid(~Date)+
  geom_hline(aes(yintercept = Median),lty=2)+
  labs(x="Days")
graphics.off()

library(car)

mylogit <- lmer(Honeybees ~
                  Avg.open.flowers.per.inflorescence+
                  Total.inflorescenses+
                  Sugar_content+
                  height+
                  (1|Date)+(1|factor(logData$Location))+
                  (1|factor(logData$East)), data=logData)

ints=ranef(mylogit)$Date
coef = fixef(mylogit)

for(i in 1:nrow(logData)){
  for(j in 1:nrow(ints)){
    if(logData$Date[i]==rownames(ints)[j]){
      logData$Intercept[i]=ints[,1][j]
    }
  }
}
logData$Avg.open.flowers.per.inflorescence_slope=coef['Avg.open.flowers.per.inflorescence']
logData$height_slope=coef['height']
logData$Total.inflorescenses_slope=coef['Total.inflorescenses']
logData$Sugar_content_slope=coef['Sugar_content']


an=Anova(mylogit)
pvals=round(an$`Pr(>Chisq)`,4)

system('mkdir 7-14-2017')
png('7-14-2017/lmerFull_flowersPerInfl.png')
ggplot(data=logData, aes(x=Avg.open.flowers.per.inflorescence, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= Avg.open.flowers.per.inflorescence_slope, color=Date))+
  labs(x=paste0(v," (p-val = ",pvals[1],')'))+
  theme_classic()
graphics.off()

png('7-14-2017/lmerFull_totInfl.png')
ggplot(data=logData, aes(x=Total.inflorescenses, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= Total.inflorescenses_slope, color=Date))+
  labs(x=paste0(v," (p-val = ",pvals[2],')'))+
  theme_classic()
graphics.off()



png('7-14-2017/lmerFull_sugCont.png')
ggplot(data=logData, aes(x=Sugar_content, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= Sugar_content_slope, color=Date))+
  labs(x=paste0(v," (p-val = ",pvals[3],')'))+
  theme_classic()
graphics.off()

png('7-14-2017/lmerFull_height.png')
ggplot(data=logData, aes(x=height, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= height_slope, color=Date))+
  labs(x=paste0(v," (p-val = ",pvals[4],')'))+
  theme_classic()
graphics.off()


vars=c('Avg.open.flowers.per.inflorescence','Total.inflorescenses','Sugar_content',
       'height')

for(v in vars){
  mylogit <- lmer(Honeybees ~
                    logData[,v]+
                  (1|Date)+(1|factor(logData$Location))+
                  (1|factor(logData$East)), data=logData)
  ints=ranef(mylogit)$Date
  coef = fixef(mylogit)
  pval=round(Anova(mylogit)$`Pr(>Chisq)`,4)
for(i in 1:nrow(logData)){
  for(j in 1:nrow(ints)){
    if(logData$Date[i]==rownames(ints)[j]){
      logData$Intercept[i]=ints[,1][j]
    }
  }
}
logData$vars_slope=coef[2]
xax=logData[,v]
png(paste0('7-14-2017/lmerRed_',v,'.png'))
p=ggplot(data=logData, aes(x=xax, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= vars_slope, color=Date))+
  labs(x=paste0(v," (p-val = ",pval,')'))+
  theme_classic()
plot(p)
graphics.off()
}









