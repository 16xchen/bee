#data formatting for further pollinator visitation analysis
#
#install.packages('GenABEL')
#install.packages("devtools")
#install_github("ggobi/ggally")
# install.packages('gsheet')
# install.packages("ggplot2")
# install.packages("reshape2")
#install.packages('corrplot')
#install.packages('Deducer')
#install.packages('reshape2')
library(reshape2)
library(devtools)
library(Deducer)
library(corrplot)
library(ggplot2)
library(GGally)
library(reshape2)
library(gsheet)
library(GenABEL)


round.POSIXct <- function(x, units = c("mins", "5 mins", "10 mins", "15 
mins", "quarter hours", "30 mins", "half hours", "hours")){
  if(is.numeric(units)) units <- as.character(units)
  units <- match.arg(units)
  r <- switch(units,
              "mins" = 60,
              "5 mins" = 60*5,
              "10 mins" = 60*10,
              "15 mins"=, "quarter hours" = 60*15,
              "30 mins"=, "half hours" = 60*30,
              "hours" = 60*60
  )
  H <- as.integer(format(x, "%H"))
  M <- as.integer(format(x, "%M"))
  S <- as.integer(format(x, "%S"))
  D <- format(x, "%Y-%m-%d")
  secs <- 3600*H + 60*M + S
  as.POSIXct(round(secs/r)*r, origin=D)
}

#Field observation data from google docs
url='docs.google.com/spreadsheets/d/1LKr8Ken8p1jpTGpbn2a_napP6uNJ4sEnN8gdDSEQGxY/edit#gid=1630939193'
mydat=read.csv(text=gsheet2text(url, format='csv'))
head(mydat)
summary(mydat)
dim(mydat)
size=strsplit(as.character(mydat$Location), ' ')
df=as.data.frame(t(matrix(unlist(size), 3)))
names(df)=c('non', 'Pair', 'Size')
mydat=cbind(mydat, df[,2:3])
head(mydat)
names(mydat)
mydat$Total.Flowers=mydat$Avg.open.flowers.per.inflorescenc*mydat$Total.inflorescenses

mydat_small=mydat[, c('Date', 'Experiment.Week','Location', 'Plant.Number','Pair','Size', 'Start.Time',
                      'End.Time', 'Avg.open.flowers.per.inflorescence', 'Total.inflorescenses','Total.Flowers', 'Honeybees')]

#mydat_small=mydat_small[1:60,]

#nectar data from google docs
url='docs.google.com/spreadsheets/d/1LKr8Ken8p1jpTGpbn2a_napP6uNJ4sEnN8gdDSEQGxY/edit#gid=212997740'
nect=read.csv(text=gsheet2text(url, format='csv'))
head(nect)
summary(nect)

size=strsplit(as.character(nect$Location), ' ')
df=as.data.frame(t(matrix(unlist(size), 3)))
names(df)=c('non', 'Pair', 'Size')
nect=cbind(nect[1:nrow(df),], df[,2:3])
nect$sugar.concentration=as.numeric(as.character(nect$sugar.concentration))
nect$sugar.content..µg.=as.numeric(as.character(nect$sugar.content..µg.))

nect=nect[which(nect$sugar.concentration<1.9),]

nect_small=nect[, c('Experiment.Week','Location','Plant.number','Pair', 'Size', 'volume.of.nectar..µl.','sugar.concentration', 
                    'sugar.content..µg.')]
summary(nect_small)


zeros=which(nect_small$sugar.concentration==0)
nect_small=nect_small[-zeros, ]
nect_small=na.omit(nect_small)
summary(nect_small)
##average of 5 flowers
volume=acast(nect_small[,c(1:2,6)],  Experiment.Week~Location, mean)
conc=acast(nect_small[,c(1:2, 7)],  Experiment.Week~Location, mean)
content=acast(nect_small[,c(1:2,8)],  Experiment.Week~Location, mean)

##variance of 5 flowers
volume_var=acast(nect_small[,c(1:2,6)],  Experiment.Week~Location, var)
conc_var=acast(nect_small[,c(1:2, 7)],  Experiment.Week~Location, var)
content_var=acast(nect_small[,c(1:2,8)],  Experiment.Week~Location, var)

booNA=!is.na(melt(volume)$value)


vol=na.omit(melt(volume))
sugar.conc=na.omit(melt(conc))
sugar.content=na.omit(melt(content))


vol_var=melt(volume_var)[booNA,]
sugar.conc_var=melt(conc_var)[booNA,]
sugar.content_var=melt(content_var)[booNA,]

mean_nect=cbind(vol[,1:2], data.frame(vol$value, sugar.conc$value, sugar.content$value, vol_var$value, sugar.conc_var$value,sugar.content_var$value))
names(mean_nect)=c("Experiment.Week", "Location", 'Volume', "Sugar_conc", 'Sugar_content', 'Volume_var', 'Sugar_conc_var', 'Sugar_content_var')


mean_nect=mean_nect[order(mean_nect$Experiment.Week), ]
head(mean_nect)

add1=mean_nect[mean_nect$Experiment.Week==1,]
add2=mean_nect[mean_nect$Experiment.Week==2,]
add3=mean_nect[mean_nect$Experiment.Week==3,]
add4=mean_nect[mean_nect$Experiment.Week==4,]


mean_nect=rbind(mean_nect[mean_nect$Experiment.Week==1,], add2,add2, add3, add3, add4, add4)

mean_nect=mean_nect[order(mean_nect$Location), ]
#
#
mydat_small=mydat_small[order(mydat_small$Location), ]
merged=cbind(mydat_small, mean_nect)

names(merged)
merged=merged[, -c(13:14)]





#merge weather station data with pollinator visitation data
url2='docs.google.com/spreadsheets/d/1Wm0LVMGEWBQnz6jsSKjdrzKi5ARMhUAzZMN_chvg7M4/edit?usp=sharing'
weather=read.csv(text=gsheet2text(url2, format='csv'))
head(weather)

weather=weather[-which(weather$Time=="Time"),]
summary(weather)
summary(merged)

obs_start=paste(merged$Date, merged$Start.Time)
obs_start_pos=as.POSIXct(obs_start , format = "%m/%d/%Y %I:%M")



obs_end=paste(merged$Date, merged$End.Time)
obs_end_pos=as.POSIXct(obs_end , format = "%m/%d/%Y %I:%M")


weatime=paste(weather$Date, weather$Time)
weatime_pos=as.POSIXct(weatime , format = "%m/%d/%Y %I:%M %p")
# 

obs_start_pos_rd=round.POSIXct(obs_start_pos, 'quarter hours')
obs_end_pos_rd=round.POSIXct(obs_end_pos, 'quarter hours')

weatime_pos_rd=round.POSIXct(weatime_pos, 'quarter hours')

weather_index=c()
for(i in 1:length(obs_start_pos)){
  #tms=weatime_pos[which(weatime_pos_rd==obs_start_pos_rd[i]|weatime_pos_rd==obs_start_pos_rd[i]-15*60|weatime_pos_rd==obs_end_pos_rd[i]|(weatime_pos_rd==obs_end_pos_rd[i]+15*60))]
  tms=weatime_pos[which(weatime_pos_rd==obs_start_pos_rd[i]|weatime_pos_rd==obs_end_pos_rd[i])]
  if(length(tms)==0){
    print(i)
    weather_index[i]=0}
  else{
    weather_index[i]=which(weatime_pos_rd==obs_start_pos_rd[i]|weatime_pos_rd==obs_end_pos_rd[i])[1]
  }}


weather_match=weather[weather_index,]

dim(weather_match)
bee_data_final=cbind(merged, weather_match[,c('Temperature', 'Humidity', 'Wind','Speed','Gust', 'Pressure','Solar')])
bee_data_final$Start.Time=obs_start_pos
bee_data_final$End.Time=obs_end_pos
bee_data_final=bee_data_final[order(bee_data_final$Date),]


bee_data_final$Temperature=as.numeric(sub('°F','',bee_data_final$Temperature))

bee_data_final$Speed=as.numeric(sub('mph','',bee_data_final$Speed))

bee_data_final$Solar=as.numeric(sub('w/m²','',bee_data_final$Solar))



bee_data_final$Clustered=(bee_data_final$Pair==2|bee_data_final$Pair==4|bee_data_final$Pair==6|bee_data_final$Pair==7)
bee_data_final$East=(bee_data_final$Pair==2|bee_data_final$Pair==1|bee_data_final$Pair==7)

bee_data_final$hotDay=bee_data_final$Temperature>mean(bee_data_final$Temperature)

data_dt=split(bee_data_final, bee_data_final$Date)


###median as theshold value
theshold=c()
wholedf=data.frame()
for( i in 1:length(data_dt)){
  theshold[i]=median(data_dt[[i]]$Honeybees)
  data_dt[[i]]$visits=(data_dt[[i]]$Honeybees>=theshold[i])
  wholedf=rbind(wholedf, data_dt[[i]])
}

wholedf$Experiment.Week=as.factor(wholedf$Experiment.Week)
wholedf$Plant.Number=as.factor(wholedf$Plant.Number)




url='docs.google.com/spreadsheets/d/1LKr8Ken8p1jpTGpbn2a_napP6uNJ4sEnN8gdDSEQGxY/edit#gid=1620742018'
ht=read.csv(text=gsheet2text(url, format='csv'))
ht=ht[order(ht$White.Sage.Plant),]


for(i in 1:nrow(wholedf)){
  for(j in 1:nrow(ht)){
    if( wholedf$Location[i]==ht$White.Sage.Plant[j] && wholedf$Plant.Number[i]==ht$Plant.Number[j]){
      wholedf$height[i]=ht$Height.m[j]
    }
  }
}


setwd('~/bee/')
write.csv(wholedf, file="pollinator_visitation_fullData.csv")
