#install.packages("qtlcharts",  repos = 'http://cran.rstudio.com/')
library(qtlcharts)

mydat_small_val_log=read.csv('pollinator_visitation_fullData_logTrans.csv')
head(mydat_small_val_log)

subset=mydat_small_val_log[,-c(1, 15:23)]

iplotCorr(subset,mydat_small_val_log$Visits, reorder=TRUE,
          chartOpts=list(cortitle="Correlation matrix",
                         scattitle="Scatterplot | Visits",
                         scatcolors=c("red","lightblue")
          ))
