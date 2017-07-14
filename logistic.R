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






mylogit_size <- glmm(Visits ~  height+Total.inflorescenses+Sugar_content+Temperature
                    ,  random = list(~ 0 + factor(Pair),~0+factor(East)), 
                    varcomps.names = c( "Pair", "East"),data = mydat_small_val_log, 
                    family.glmm = binomial.glmm, m = 10^4)

summary(mylogit_size)




mylogit_rd <- glmm(Visits ~ Temperature
                     ,  random = list(~ 0 + factor(Pair),~0+factor(East)), 
                     varcomps.names = c( "Pair", "East"),data = mydat_small_val_log, 
                     family.glmm = binomial.glmm, m = 10^3)


mylogit_ht <- glmm(Visits ~  Temperature+height
                     ,  random = list(~ 0 + factor(Pair),~0+factor(East)), 
                     varcomps.names = c( "Pair", "East"),data = mydat_small_val_log, 
                     family.glmm = binomial.glmm, m = 10^3)


mylogit_tot <- glmm(Visits ~  Temperature+height+Total.Flowers
                     ,  random = list(~ 0 + factor(Pair),~0+factor(East)), 
                     varcomps.names = c( "Pair", "East"),data = mydat_small_val_log, 
                     family.glmm = binomial.glmm, m = 10^3)



summary(mylogit_rd)
summary(mylogit_ht)
summary(mylogit_tot)





Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))
train <- sample(1:150, 75)
table(Iris$Sp[train])
## your answer may differ
##  c  s  v
## 22 23 30
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
predict(z, Iris[-train, ])$class
##  [1] s s s s s s s s s s s s s s s s s s s s s s s s s s s c c c
## [31] c c c c c c c v c c c c v c c c c c c c c c c c c v v v v v
## [61] v v v v v v v v v v v v v v v
(z1 <- update(z, . ~ . - Petal.W.))


install.packages('caret')
library(caret)

dt.lda_num <- lda(visits ~ Avg.open.flowers.per.inflorescence+
                Total.inflorescenses+
                Total.Flowers+
                Volume+
                Sugar_conc+
                Sugar_content+
                height
                , data=data)

dt.lda_num$scaling
dt.lda.values <- predict(dt.lda_num)
confusionMatrix(data$visits, dt.lda.values$class)






system('mkdir 7-05-2017')
png('7-05-2017/lda_all.png')
ldahist(data = dt.lda.values$x[,1], g=data$visits)
graphics.off()

