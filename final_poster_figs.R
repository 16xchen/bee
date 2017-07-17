library(nlme)
#install.packages("glmm")
library(glmm)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)
library(lme4)

install.packages('dplyr')
library(dplyr)
library(devtools)
devtools::install_github("strengejacke/sjPlot",force = TRUE)
library(sjPlot)
library(sjmisc)


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
                  (1|Date), data=logData)


an=Anova(mylogit)
pvals=round(an$`Pr(>Chisq)`,4)

sjp.setTheme()


vars=c('Avg.open.flowers.per.inflorescence','Total.inflorescenses','Sugar_content',
       'height')
tit=c('Reward+Efficiency','Poential Reward for Colony', 'Reward per Inflorescense','Salience')


#p=sjp.lmer(mylogit, type = "ri.slope")


system('mkdir 7-14-2017')
for(i in 1:length(vars)){
  p=sjp.lmer(mylogit, type = "ri.slope",vars = vars[i], facet.grid = FALSE)
  xax=logData[,vars[i]]
  ploot=p[[2]][[1]]+geom_point(data=logData, aes(y=Honeybees, x=xax, color=Date))+
    labs(x=paste0(vars[i]," (p-val = ",pvals[i],')'))+
    ggtitle(tit[i])+
    theme_classic()
  png(paste0('7-14-2017/lmerFull_fixedSlope_',vars[i],'.png'))
  plot(ploot)
  graphics.off()
}

for(i in 1:length(vars)){
  ploot2=sjp.lmer(mylogit, type = "pred", facet.grid = FALSE,
                  vars = c(vars[i],'Date'))
  ploot2=ploot2[[2]]+
    labs(x=paste0(vars[i]," (p-val = ",pvals[i],')'))+
    ggtitle(tit[i])+
    theme_classic()
  png(paste0('7-14-2017/lmerFull_diffSlope2_',vars[i],'.png'))
  plot(ploot2)
  graphics.off()
}


sjp.lmer(mylogit, type = "coef", facet.grid = FALSE,
         vars = c(vars[i],'Date'))


for(i in 1:nrow(logData)){
  for(j in 1:nrow(ints)){
    if(logData$Date[i]==rownames(ints)[j]){
      logData$Intercept[i]=ints[,1][j]+coef[1]+2.856656e-15
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
  labs(x=paste0(vars[1]," (p-val = ",pvals[1],')'))+
  ggtitle(tit[1])
graphics.off()

png('7-14-2017/lmerFull_totInfl.png')
ggplot(data=logData, aes(x=Total.inflorescenses, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= Total.inflorescenses_slope, color=Date))+
  labs(x=paste0(vars[2]," (p-val = ",pvals[2],')'))+
  ggtitle(tit[2])
graphics.off()



png('7-14-2017/lmerFull_sugCont.png')
ggplot(data=logData, aes(x=Sugar_content, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= Sugar_content_slope, color=Date))+
  labs(x=paste0(vars[3]," (p-val = ",pvals[3],')'))+
  ggtitle(tit[3])
graphics.off()

png('7-14-2017/lmerFull_height.png')
ggplot(data=logData, aes(x=height, y=Honeybees, color=Date))+
  geom_point()+
  geom_abline(data=logData,aes(intercept=Intercept,slope= height_slope, color=Date))+
  labs(x=paste0(vars[4]," (p-val = ",pvals[4],')'))+
  ggtitle(tit[4])
graphics.off()

ind=1
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
  #theme_classic()+
  ggtitle(tit[ind])
plot(p)
graphics.off()
ind=ind+1
}









