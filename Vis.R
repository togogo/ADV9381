##
## L I B R A R I E S
##
library(tidyverse)
library(readxl)
library(plotly)#maybe I don't need this...
library(usmap)
library(ggplot2)
library(viridis)
library(dplyr)
library(waffle)
library(beeswarm)

##
## L O A D I N G  D A T A
##
designers <- read_xlsx("./artsgov/designers_by_residence_2006-2010.xlsx")
architects <- read_xlsx("./artsgov/architects_by_residence_2006-2010.xlsx")
artists <- read_xlsx("./artsgov/artists_by_residence_2006-2010.xlsx")

designersEarnings <- read_xlsx("./artsgov/designers_earning_groups_2006-2010.xlsx")
architectsEarnings <- read_xlsx("./artsgov/architects_earning_groups_2006-2010.xlsx")
artistsEarnings <- read_xlsx("./artsgov/artists_earning_groups_2006-2010.xlsx")

designcensus2017 <- read_csv("./designcensus/DesignCensus2017_Data.csv")
designcensus2019 <- read_csv("./designcensus/DesignCensus2019_RAW DATA.csv")
ethnicity_census2019 <- read_xlsx("./designcensus/ethnicityCount.xlsx")

#View(designcensus2019)

#converting the state information into fips code
designersFipsReady = mutate(designers, fips = fips(State))
architectsFipsReady = mutate(architects, fips = fips(State))
artistsFipsReady = mutate(artists, fips = fips(State))

#print(designcensus2019[0,35])#checking column names...
#ethnicity_census2019 <- select(designcensus2019, "I am:_2")
#View(ethnicity_census2019)#this works!
#View(ethnicity_census2019)

#######
#######convert the data in to long type
#######

#Designers
earningsOrigCategories <- colnames(designersEarnings)
earningsOrigCategories <- earningsOrigCategories[-1]#get rid of the "state" column
#print(earningsOrigCategories)
designersEarningsLong <- designersEarnings %>% gather(key = "earningscat", value = "earningsval",  earningsOrigCategories)

#Architects
architectsEarningsLong <- architectsEarnings %>% gather(key = "earningscat", value = "earningsval",  earningsOrigCategories)

#Artists
artistsEarningsLong <- artistsEarnings %>% gather(key = "earningscat", value = "earningsval",  earningsOrigCategories)

#Designers/Architects/Artists Residence 2006 - 2010
earningscatLevels <- levels(factor(designersEarningsLong$earningscat))
reorderedEarningsCatLevels <- factor(c( "No earnings", "$1 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $124,999", "$125,000 or more" ))
designersEarningsLong$earningscat <- factor(designersEarningsLong$earningscat, levels = rev(reorderedEarningsCatLevels))

designerPlot <- plot_usmap(regions = "states", data = designersFipsReady, values = "Designers", color = "white") +  scale_fill_viridis(option = "viridis", direction = -1) + labs(title = "Designers by Residence 2006 - 2010")
architectPlot <- plot_usmap(regions = "states", data = architectsFipsReady, values = "Architects", color = "white") +  scale_fill_viridis(option = "viridis", direction = -1) + labs(title = "Architects by Residence 2006 - 2010")
artistPlot <- plot_usmap(regions = "states", data = artistsFipsReady, values = "Artists", color = "white") +  scale_fill_viridis(option = "viridis", direction = -1) + labs(title = "Artists by Residence 2006 - 2010")

#Designers/Architects/Artists Earnings 2006 - 2010
designersEarningsPlot <- ggplot(designersEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat)) + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Designers Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")
designersEarningsPlotPercentage <- ggplot(designersEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat), position = "fill") + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Designers Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")
architectsEarningsPlot <- ggplot(architectsEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat)) + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Architects Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")
artistsEarningsPlot <- ggplot(artistsEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat)) + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Artists Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")

#View(designersEarningsLong)

##RESIDENCE PLOT
#designerPlot 
#architectPlot
#artistPlot

##EARNINGS PLOT
#designersEarningsPlot
designersEarningsPlotPercentage
#architectsEarningsPlot
#artistsEarningsPlot

##ETHNICITY PLOT
#

ethnicity_census2019 <- select(designcensus2019, "I am:_2")
situation_census2019 <- select(designcensus2019, "I am:_1")
expertise_census2019 <- select(designcensus2019, "I work in:")
industry_census2019 <- select(designcensus2019, "Industries I work with:")
security_census2019 <- select(designcensus2019, "I feel:")
gender_census2019 <- select(designcensus2019, "I identify as:")
age_census2019 <- select(designcensus2019, "My age is:")

genderAgePlot <- ggplot(designcensus2019, aes_(x = as.name("My age is:"), y = as.name("I identify as:"))) + geom_point()
#genderAgePlot <- ggplot(designcensus2019, aes(x = age_census2019, y = gender_census2019)) + geom_point()
#genderAgePlot

designcensus2019[designcensus2019$"My age is:">100, ]
#View(ethnicity_census2019)
#View(expertise_census2019)
#View(industry_census2019)
#View(security_census2019)
ethnicityCount_census2019 <- table(unlist(designcensus2019$"I am:_2"))
situationCount_census2019 <- table(unlist(designcensus2019$"I am:_1"))
expertiseCount_census2019 <- table(unlist(designcensus2019$"I work in:"))
securityCount_census2019 <- table(unlist(designcensus2019$"I feel:"))
#View(securityCount_census2019)
#View(expertiseCount_census2019)
#View(designcensus2019)
#ethnicityCount_census2019
#ethnicityCount_census2019 <- ethnicityCount_census2019[-0]
#View(ethnicityCount_census2019)



#ethnicityCount_census2019
manualEthnicity <- c("Asian" = 877, "Bi-/Multi-racial" = 443, "Black/African American" = 318, "Latina/Latino/Hispanic" = 732, "Native America/First People" = 21, "Native Hawaiian/Pacific Islander" = 24, "Other/Prefer not to say" = 343, "White/Caucasian" = 6651)
#manualEthnicity <- c("Caucasian" = 6651)
#manualEthnicity <- c("One"=80, "Two"=30, "Three"=20, "Four"=10, "Five" =30, "Okne"=80, "Tkwo"=30, "Thkkree"=20, "Fokur"=10, "Fkive" =30)
#manualEthnicity <- c("Caucasian"=665, "Two"=275, "ha" = 300)
#manualEthnicity
#ethnicity_census2019
ethnicityChart <- waffle(manualEthnicity/10, size= 0, rows = 20)
#ethnicityChart

securityWaffle <- waffle(securityCount_census2019/10, size= 0, rows = 20)
#securityWaffle

situationWaffle <- waffle(situationCount_census2019/10, size= 0, rows = 20)
#situationWaffle

#beeswarm(manualEthnicity)
#beeswarm(ethnicity_census2019) + coord_flip()
#ethnicityPlot <- ggplot(ethnicity_census2019, aes(x = "I am:_2")) + geom_bar(stat = "identity") + theme_minimal()
#ethnicityPlot <- ggplot(ethnicityCount_census2019, aes(x = "Var1", y = "Freq")) + geom_bar(stat = "identity") + theme_minimal()
#ethnicityPlot

#par(las=2)
#ethnicityPlot <- barplot(manualEthnicity, main="Ethnicity", horiz = TRUE, cex.names=0.8)
#ethnicityPlot


#save the plots in PDF
#ggsave(plot = artistPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = architectPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = architectsEarningsPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = artistsEarningsPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = ethnicityPlot, width = 15, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = ethnicityChart, width = 15, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = securityWaffle, width = 15, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = situationWaffle, width = 15, height = 10, dpi = 300, filename = "output.pdf")
ggsave(plot = designersEarningsPlotPercentage, width = 15, height = 10, dpi = 300, filename = "output.pdf")





