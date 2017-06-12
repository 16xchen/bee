# install.packages('gsheet')
# install.packages("ggplot2")
# install.packages("reshape2")
#install.packages('corrplot')
#install.packages('Deducer')
install.packages("devtools")
library(devtools)
install_github("ggobi/ggally")
library(Deducer)
library(corrplot)
library(ggplot2)
library(GGally)
library(reshape2)
library(gsheet)
install.packages('GenABEL')
library(GenABEL)

setwd('~/bee/')
mydat_small=read.csv('pollinator_visitation_fullData.csv')

system('mkdir 6-08-17')
png("6-08-17/beeVtot.png")
ggplot(mydat_small, aes(x=Total.inflorescenses, y=Honeybees, color=Size, group=Pair))+
  geom_line(color='gray')+
  geom_point()+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()

png("6-03-17/beeVave.png")
ggplot(mydat_small, aes(x=Avg.open.flowers.per.inflorescence, y=Honeybees, color=Size, group=Pair))+
  geom_line(color='gray')+
  geom_point()+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()



png("6-03-17/beeVtotFlower.png")
ggplot(mydat_small, aes(x=Total.inflorescenses*Avg.open.flowers.per.inflorescence, y=Honeybees, color=Size, group=Pair))+
  geom_line(color='gray')+
  geom_point()+
  facet_wrap(~Date)+
  labs(x="Total Flowers")+
  theme_bw()
graphics.off()

url='docs.google.com/spreadsheets/d/1LKr8Ken8p1jpTGpbn2a_napP6uNJ4sEnN8gdDSEQGxY/edit#gid=212997740'
nect=read.csv(text=gsheet2text(url, format='csv'))
head(nect)
# nect$Location=sub("P", "Pair ", nect$Location)
# nect$Location=sub("L", " Large", nect$Location)
# nect$Location=sub("S", " Small", nect$Location)

size=strsplit(as.character(nect$Location), ' ')
df=as.data.frame(t(matrix(unlist(size), 3)))
names(df)=c('non', 'pair', 'size')
nect=cbind(nect, df[,2:3])
nect$sugar.concentration=as.numeric(as.character(nect$sugar.concentration))
nect$sugar.content..µg.=as.numeric(as.character(nect$sugar.content..µg.))

nect=nect[which(nect$sugar.concentration<1.9),]


png("6-03-17/sugar_conc.png")
ggplot(nect, aes(y=sugar.concentration, x=pair, fill=size))+
  geom_boxplot()+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()

png("6-03-17/sugar_content.png")
ggplot(nect, aes(y=sugar.content..µg., x=pair, fill=size))+
  geom_boxplot()+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()


png("6-03-17/sugar_volume.png")
ggplot(nect, aes(y=volume.of.nectar..µl., x=pair, fill=size))+
  geom_boxplot()+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()




png("6-08-17/beeVconc.png")
ggplot(mydat_small, aes(x=Sugar_conc, y=Honeybees, group=Pair, color=Size))+
  geom_point()+
  geom_line(color='gray')+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()

png("6-08-17/beeVcontent.png")
ggplot(mydat_small, aes(x=Sugar_content, y=Honeybees, group=Pair, color=Size))+
  geom_point()+
  geom_line(color='gray')+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()

png("6-08-17/beeVvol.png")
ggplot(mydat_small, aes(x=Volume, y=Honeybees, group=Pair, color=Size))+
  geom_point()+
  geom_line(color='gray')+
  facet_wrap(~Date)+
  theme_bw()
graphics.off()

mydat_small$Experiment.Week=as.factor(mydat_small$Experiment.Week)
mydat_small$Plant.Number=as.factor(mydat_small$Plant.Number)
mydat_small$Pair=as.factor(mydat_small$Pair)



boo=c()
for (i in 1:ncol(mydat_small)){
  boo[i]=is.numeric(mydat_small[,i])
}



mydat_small_val=mydat_small[,boo]
mydat_small_val=mydat_small_val[,-c(1,8:10)]
#M <- cor(merged_val)

# corrplot(M, method="circle")
# wb <- c("white","black")
# 
# 
# cor.mtest <- function(mat, conf.level = 0.95){
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   diag(lowCI.mat) <- diag(uppCI.mat) <- 1
#   for(i in 1:(n-1)){
#     for(j in (i+1):n){
#       tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
#       p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
#       lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
#       uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
#     }
#   }
#   return(list(p.mat, lowCI.mat, uppCI.mat))
# }
# res1 <- cor.mtest(merged_val,0.95)
# 
# corrplot(M, order="hclust",p.mat = res1[[1]],  addrect=2, sig.level=-1,  insig = "p-value")

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue4", color="blue1", ...)
  return(p)
}

#merged_val=merged_val[,-1]

  
gg=ggpairs(mydat_small_val, lower=list(continuous=my_fn))
gg

# d1=mydat_small_val[mydat_small$Date=='5/17/2017',]
# d2=mydat_small_val[mydat_small$Date=='5/23/2017',]
# d3=mydat_small_val[mydat_small$Date=='5/25/2017',]
# d4=mydat_small_val[mydat_small$Date=='5/30/2017',]

p_ <- function(pm) {
  if (interactive()) {
    print(pm)
  }
  invisible()
}

dates=split(mydat_small_val, mydat_small$Date)
for(i in 1:length(dates)){
  png(paste0('6-08-17/pairwise_cor_d',i,'.png'))
  dts=dates[[i]]
  gg=ggpairs(dts, lower=list(continuous=my_fn), verbose=F)
  p_(gg)
  graphics.off()
  }

quartz()
ggpairs(mydat_small_val, lower=list(continuous=my_fn))


#try log transform again
mydat_small_val_log=mydat_small_val
mydat_small_val_log$Honeybees=mydat_small_val_log$Honeybees+1
mydat_small_val_log$Speed=mydat_small_val_log$Speed+1
for(i in 1:ncol(mydat_small_val_log)){
  mydat_small_val_log[,i]=log10(mydat_small_val_log[,i])
}

png('6-08-17/pairwise_cor_all_log.png')
ggpairs(mydat_small_val_log, lower=list(continuous=my_fn), verbose=F)
graphics.off()

png('6-08-17/pairwise_cor_all.png')
ggpairs(mydat_small_val, lower=list(continuous=my_fn), verbose=F)
graphics.off()



