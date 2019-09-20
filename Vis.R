library(tidyverse)
library(readxl)
library(plotly)#maybe I don't need this...
library(usmap)
library(ggplot2)
library(viridis)
library(dplyr)

#Loading the data
designers <- read_xlsx("./artsgov/designers_by_residence_2006-2010.xlsx")
architects <- read_xlsx("./artsgov/architects_by_residence_2006-2010.xlsx")
artists <- read_xlsx("./artsgov/artists_by_residence_2006-2010.xlsx")

designersEarnings <- read_xlsx("./artsgov/designers_earning_groups_2006-2010.xlsx")
architectsEarnings <- read_xlsx("./artsgov/architects_earning_groups_2006-2010.xlsx")
artistsEarnings <- read_xlsx("./artsgov/artists_earning_groups_2006-2010.xlsx")

designcensus2019 <- read_csv("./designcensus/DesignCensus2019_RAW DATA.csv")

View(designcensus2019)

#converting the state information into fips code
designersFipsReady = mutate(designers, fips = fips(State))
architectsFipsReady = mutate(architects, fips = fips(State))
artistsFipsReady = mutate(artists, fips = fips(State))

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
designerPlot <- plot_usmap(regions = "states", data = designersFipsReady, values = "Designers", color = "white") +  scale_fill_viridis(option = "viridis", direction = -1) + labs(title = "Designers by Residence 2006 - 2010")
architectPlot <- plot_usmap(regions = "states", data = architectsFipsReady, values = "Architects", color = "white") +  scale_fill_viridis(option = "viridis", direction = -1) + labs(title = "Architects by Residence 2006 - 2010")
artistPlot <- plot_usmap(regions = "states", data = artistsFipsReady, values = "Artists", color = "white") +  scale_fill_viridis(option = "viridis", direction = -1) + labs(title = "Artists by Residence 2006 - 2010")

#Designers/Architects/Artists Earnings 2006 - 2010
designersEarningsPlot <- ggplot(designersEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat)) + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Designers Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")
architectsEarningsPlot <- ggplot(architectsEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat)) + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Architects Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")
artistsEarningsPlot <- ggplot(artistsEarningsLong, aes(x = reorder(State , -earningsval), y = earningsval)) + geom_bar(stat = "identity", aes(fill = earningscat)) + coord_flip() + scale_fill_viridis_d() + theme_minimal() + ggtitle("Artists Earnings by Residence 2006 - 2010") + xlab("States") + ylab("People Count")

##RESIDENCE PLOT
#designerPlot 
#architectPlot
#artistPlot

##EARNINGS PLOT
#designersEarningsPlot
#architectsEarningsPlot
#artistsEarningsPlot

#save the plots in PDF
#ggsave(plot = artistPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = architectPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = architectsEarningsPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")
#ggsave(plot = artistsEarningsPlot, width = 10, height = 10, dpi = 300, filename = "output.pdf")

##VARIOUS TRIALS AND ERRORS...JUST LEAVING THIS AS A MEMO
#View(test)
#plotReady <- select(test['State']) %>%
#  fips(test['State'])
#mutate(test, edited = Designers*2)
#States <- select(test['State'])
#yo <- fips(test["State"])
#yo <- capitalize(test["State"])
#yo
#fipsConverted <- mutate(test, FIPSSTATE = fips("State"))
#fipsConverted
#View(designers)
#View(test)
#print(test['State'])
#testing
#plot_usmap(regions = "counties") + 
#  labs(title = "US Counties",
#       subtitle = "This is a blank map of the counties of the United States.") + 
#  theme(panel.background = element_rect(color = "black", fill = "lightblue"))
#temp <- colnames(designersEarnings)
#print(temp)




