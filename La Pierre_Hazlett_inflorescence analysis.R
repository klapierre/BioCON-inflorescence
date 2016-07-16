library(openxlsx)
library(lsmeans)
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
achi2016 <- read.xlsx('Hazlett_Achillea_ReproductiveCounts_July.xlsx', sheet='July 7')%>%
  mutate(spp='achi')
names(achi2016)[names(achi2016)=='Inflor.Stalk.#'] <- 'stalks'
agro2016 <- read.xlsx('Hazlett_Agro_ReproductiveCounts_July.xlsx', sheet='July 5')%>%
  mutate(spp='agro')
names(agro2016)[names(agro2016)=='#.of.Stalks'] <- 'stalks'
amor2016 <- read.xlsx('Hazlett_Amoca_ReproductiveCounts_July.xlsx', sheet='July 7')%>%
  mutate(spp='amor')
names(amor2016)[names(amor2016)=='#.of.Flowers'] <- 'stalks'
anem2016 <- read.xlsx('Hazlett_Anenome_ReproductiveCounts_July.xlsx', sheet='July 5')%>%
  mutate(spp='anem')
names(anem2016)[names(anem2016)=='Compound.#'] <- 'stalks'
ascl2016 <- read.xlsx('Hazlett_Asctu_ReproductiveCounts_July.xlsx', sheet='July 12')%>%
  mutate(spp='ascl', stalks=(Budding.Stalks+Flowering.Stalks+Stalks.Seeding))%>%
  select(-NonFlowering.Stalks, -Budding.Stalks, -Flowering.Stalks, -Stalks.Seeding)

# lupi2016 <- read.xlsx('Hazlett_LupineSeedCount_June.xlsx', sheet='June 21') #lupi is complicated by seed number, so do this later

#stalks were counted in different sized areas, so converting them all to m2 (agro and amor were 0.25 m2 areas, the rest were 0.5 m2 areas)
inflor <- rbind(achi2016, agro2016, amor2016, anem2016, ascl2016)%>%
  mutate(stalks_m2=ifelse(spp=='amor'|spp=='agro', stalks*4, stalks*2))


# #anova for agropyron N response
# summary(agroNmodel <- aov(stalks~Ntreatment, data=agro2016))
# model.tables(agroNmodel, 'means')

#anova for all spp N response
summary(inflorNmodel <- aov(stalks_m2~spp*Ntreatment, data=inflor))
model.tables(inflorNmodel, 'means')


#make a graph
ggplot(data=barGraphStats(data=inflor, variable='stalks_m2', byFactorNames=c('spp', 'Ntreatment')), aes(x=spp, y=mean, fill=Ntreatment)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9), width=0.2) +
  scale_y_continuous(breaks=seq(0, 60, 5)) +
  scale_x_discrete(labels=c('Achillea', 'Agropyron', 'Amorpha', 'Anemone', 'Asclepias')) +
  scale_fill_manual(values=c('#00330033', '#00336666'),
                    labels=c('ambient N', 'enriched N')) +
  ylab('Inflorescence number (m-2)') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5)) +
  ggtitle('N treatment affects\ninflorescence number by species')