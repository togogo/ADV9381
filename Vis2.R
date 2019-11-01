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
library(scales)

##
## L O A D I N G  D A T A
##

#data1
imf_japan_GDP <- read_xlsx("./IMF/imf_report_transposed.xlsx")

#data2
creativity_index_country <- c("United States", "Germany", "United Kingdom", "France", "Japan")
orderedCountriesIndex <- factor(c("United States", "Germany", "United Kingdom", "France", "Japan"))
creativity_index_country <- factor(creativity_index_country, levels = rev(orderedCountriesIndex))
creativity_index_val <- c(0.77, 0.73, 0.71, 0.56, 0.43)
creativity_index <- data.frame("countries" = creativity_index_country, "percentages" = creativity_index_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data3
creativity_selfaware_country <- c("Germany", "United States", "United Kingdom", "France", "Japan")
orderedCountriesSelfAware <- factor(c("Germany", "United States", "United Kingdom", "France", "Japan"))
creativity_selfaware_val <- c(0.57, 0.55, 0.41, 0.40, 0.13)
creativity_selfaware_country <- factor(creativity_selfaware_country, levels = rev(orderedCountriesSelfAware))
creativity_selfaware <- data.frame("countries" = creativity_selfaware_country, "percentages" = creativity_selfaware_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data4
creative_potential_country <- c("United States", "France", "United Kingdom", "Germany", "Japan")
orderedCountriesPotential <- c("United States", "France", "United Kingdom", "Germany", "Japan")
creative_potential_val <- c(0.44, 0.34, 0.33, 0.24, 0.20)
creative_potential_country <- factor(creative_potential_country, levels = rev(orderedCountriesPotential))
creativity_potential <- data.frame("countries" = creative_potential_country, "percentages" = creative_potential_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data 5
most_creative_country <- c("Japan", "United States", "France", "Germany", "United Kingdom", "Other")
most_creative_country_val <- c(0.34, 0.28, 0.11, 0.11, 0.11, 0.05)
orderedCreativeCountry <- c("Japan", "United States", "France", "Germany", "United Kingdom", "Other")
most_creative_country <- factor(most_creative_country, levels = rev(orderedCreativeCountry))
creative_country <- data.frame("countries" = most_creative_country, "percentages" = most_creative_country_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data 6
most_creative_city <- c("Tokyo", "New York", "Paris", "London", "Los Angeles", "San Francisco", "Berlin", "Other")
most_creative_city_val <- c(0.26, 0.23, 0.14, 0.10, 0.07, 0.07, 0.07, 0.05)
orderedCreativeCity <- c("Tokyo", "New York", "Paris", "London", "Los Angeles", "San Francisco", "Berlin", "Other")
most_creative_city <- factor(most_creative_city, levels = rev(orderedCreativeCity))
creative_city <- data.frame("cities" = most_creative_city, "percentages" = most_creative_city_val) %>%
  mutate(highlight_flag = ifelse(cities == 'Tokyo', T, F))

#data 7
creativity_invest_productivity <- c("United States", "Germany", "United Kingdom", "France", "Japan")
creativity_invest_productivity_val <- c(0.88, 0.85, 0.81, 0.80, 0.55)
orderedProductivity <- c("United States", "Germany", "United Kingdom", "France", "Japan")
creativity_invest_productivity <- factor(creativity_invest_productivity, levels = rev(orderedProductivity))
creative_productivity <- data.frame("countries" = creativity_invest_productivity, "percentages" = creativity_invest_productivity_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data 8
creativity_invest_happiness <- c("United States", "Germany", "France", "United Kingdom", "Japan")
creativity_invest_happiness_val <- c(0.88, 0.84, 0.81, 0.80, 0.47)
orderedHappiness <- c("United States", "Germany", "France", "United Kingdom", "Japan")
creativity_invest_happiness <- factor(creativity_invest_happiness, levels = rev(orderedHappiness))
creative_happiness <- data.frame("countries" = creativity_invest_happiness, "percentages" = creativity_invest_happiness_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data 9
creativity_invest_innovation <- c("France", "United States", "Germany", "United Kingdom", "Japan")
creativity_invest_innovation_val <- c(0.89, 0.88, 0.88, 0.84, 0.63)
orderedInnovation <- c("France", "United States", "Germany", "United Kingdom", "Japan")
creativity_invest_innovation <- factor(creativity_invest_innovation, levels = rev(orderedInnovation))
creative_innovation <- data.frame("countries" = creativity_invest_innovation, "percentages" = creativity_invest_innovation_val) %>%
  mutate(highlight_flag = ifelse(countries == 'Japan', T, F))

#data 10

##
## V I S U A L I Z I N G   D A T A
##

#plot1
plot_nominal_GDP <- ggplot(imf_japan_GDP, aes(x = year)) + 
  geom_line(aes(y = Japan, color = "Japan")) + 
  geom_line(aes(y = World, color = "World")) + 
  geom_point(aes(y = Japan, color = "Japan")) + 
  geom_point(aes(y = World, color = "World")) +
  xlab("year") +
  ylab("nominal GDP(Billions USD)") + 
  ggtitle("The Lost 30 Years : Stagnant Japan's Nominal GDP Against the World.") +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  theme_minimal()

#plot2
plot_creativity_index <- ggplot(creativity_index, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("Value of Creativity to Economy") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none')  +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))
 
#plot3
plot_creativity_selfaware <- ggplot(creativity_selfaware, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("% of People who responded they are creative.") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none')  +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))

#plot4
plot_creativity_potential <- ggplot(creativity_potential, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("% of People who feel they are living up to their creative potential") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none')  +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))

#plot5
plot_most_creative_country <- ggplot(creative_country, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("Most creative country") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none') +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))

#plot6
plot_most_creative_city <- ggplot(creative_city, aes(y = percentages, x = cities)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("Most creative country") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none') +
  geom_text(aes(x = cities, y = percentages + 0.02, label = round(percentages, 2)))

#plot7
plot_creativity_productivity <- ggplot(creative_productivity, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("Increase employee productivity") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none') +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))

#plot8
plot_creativity_happiness <- ggplot(creative_happiness, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("Increase employee happiness") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none') +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))

#plot9
plot_creativity_innovation <- ggplot(creative_innovation, aes(y = percentages, x = countries)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  ggtitle("Catalyze innovation") +
  theme_minimal() + 
  coord_flip() +
  theme(legend.position = 'none') +
  geom_text(aes(x = countries, y = percentages + 0.02, label = round(percentages, 2)))

#plot10

##
## P L O T  T H E  D A T A
## 

#plot_nominal_GDP    
#plot_creativity_index
#plot_creativity_selfaware
#plot_creativity_potential
#plot_most_creative_country
#plot_most_creative_city
#plot_creativity_productivity
#plot_creativity_happiness
plot_creativity_innovation

##
## S A V E  P L O T S
##

#ggsave(plot = plot_nominal_GDP, width = 10, height = 5, dpi = 300, filename = "./plots/nominalGPD.pdf")
#ggsave(plot = plot_creativity_index, width = 10, height = 5, dpi = 300, filename = "./plots/creativity_countries.pdf")
#ggsave(plot = plot_creativity_selfaware, width = 10, height = 5, dpi = 300, filename = "./plots/creativity_selfaware.pdf")
#ggsave(plot = plot_creativity_potential, width = 10, height = 5, dpi = 300, filename = "./plots/creativity_potential.pdf")
#ggsave(plot = plot_most_creative_country, width = 5, height = 5, dpi = 300, filename = "./plots/creativity_country.pdf")
#ggsave(plot = plot_most_creative_city, width = 5, height = 5, dpi = 300, filename = "./plots/creativity_city.pdf")
#ggsave(plot = plot_creativity_productivity, width = 10, height = 5, dpi = 300, filename = "./plots/creativity_productivity.pdf")
#ggsave(plot = plot_creativity_happiness, width = 10, height = 5, dpi = 300, filename = "./plots/creativity_happiness.pdf")
ggsave(plot = plot_creativity_innovation, width = 10, height = 5, dpi = 300, filename = "./plots/creativity_innovation.pdf")
                            
            