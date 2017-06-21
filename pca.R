
#install_github("ggbiplot", "vqv")
library(ggbiplot)

mydat_small_val_log=read.csv('pollinator_visitation_fullData_logTrans.csv')


mydat_small_val_log$East=as.numeric(mydat_small_val_log$East)-1
mydat_small_val_log$Clustered=as.numeric(mydat_small_val_log$Clustered)-1

mydat_small_val_log=na.omit(mydat_small_val_log)


col=c('Avg.open.flowers.per.inflorescence','Total.inflorescenses',
      'Volume','Sugar_conc','Sugar_content','Temperature','Solar')

data.pca <- prcomp(mydat_small_val_log[,col],
                   center = TRUE,
                   scale. = TRUE) 
plot(data.pca, type = "l")

mydat_small_val_log$East=as.factor(mydat_small_val_log$East)
mydat_small_val_log$Clustered=as.factor(mydat_small_val_log$Clustered)
mydat_small_val_log$Pair=as.factor(mydat_small_val_log$Pair)
mydat_small_val_log$Experiment.Week=factor(mydat_small_val_log$Experiment.Week)
png('6-20-17/PCA_date.png')
g <- ggbiplot(data.pca, obs.scale = 1, choices = 1:2, var.scale = 1, 
              groups = mydat_small_val_log$Date, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
graphics.off()



png('6-20-17/PCA_visits.png')
g <- ggbiplot(data.pca, obs.scale = 1, var.scale = 1, choices = 3:4, 
              groups = factor(mydat_small_val_log$Visits), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
graphics.off()

