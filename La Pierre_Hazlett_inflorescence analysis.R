library(openxlsx)
library(nlme)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(tidyr)

setwd('C:\\Users\\Kim\\Dropbox\\2015_NSF_LaPierre\\data\\Hazlett data\\fwinflorescencecountspreadsheets')

###################################################
###################################################

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)),
             axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

###bar graph summary statistics function
#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  

###################################################
###################################################

#read in data
achi2016 <- read.xlsx('Hazlett_Achillea_ReproductiveCounts_July.xlsx', sheet='July 7')
agro2016 <- read.xlsx('Hazlett_Agro_ReproductiveCounts_July.xlsx', sheet='July 5')
amor2016 <- read.xlsx('Hazlett_Amoca_ReproductiveCounts_July.xlsx', sheet='July 7')
anem2016 <- read.xlsx('Hazlett_Anenome_ReproductiveCounts_July.xlsx', sheet='July 5')
ascl2016 <- read.xlsx('Hazlett_Asctu_ReproductiveCounts_July.xlsx', sheet='July 12')
lupi2016 <- read.xlsx('Hazlett_LupineSeedCount_June.xlsx', sheet='June 21')





















