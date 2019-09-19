library(tidyverse)
library(readxl)
library(plotly)#maybe I don't need this...
library(usmap)
library(ggplot2)
library(viridis)

#Loading the data
designers <- read_xlsx("./artsgov/designers_by_residence_2006-2010.xlsx")
architects <- read_xlsx("./artsgov/architects_by_residence_2006-2010.xlsx")

#converting the state information into fips code
designersFipsReady = mutate(designers, fips = fips(State))
architectsFipsReady = mutate(architects, fips = fips(State))

#Designers by Residence 2006 - 2010
designerPlot <- plot_usmap(regiouns = "states", data = designersFipsReady, values = "Designers", color = "white") +  scale_fill_viridis(option = "plasma", direction = -1) + labs(title = "Designers by Residence 2006 - 2010")










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



