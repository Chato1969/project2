## Objective 4 - Digging Deeper
# Although these plots do not tell the whole story, they are great for helping
# us determine where to look. Different cities may have different populations, 
# population densities, cultural discrepancies, compliance, and city regulations
# to name a few. We will explore the role of population on these metrics using
# visualizations. Arrange two scatter plots using cowplot’s plot_grid to show 
# the relationship between population and confirmed counts as well as death 
# counts and confirmed counts. You will need to use a log transform on all
# variables to show such a relationship. Please consult the example below for
# an idea of what this may look like. From these graphs we can see that
# population greatly affects confirmations and deaths. This coincides with our
# plots above as Los Angeles’s population is 301% greater than San Diego’s
# population and 406% greater than Riverside’s population!
  

library(knitr)
library(magrittr)
library(RCurl)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(tidyverse)
library(cowplot)


USconfirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
USdeaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

##  Modified population dataset from this website: 
##  https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html

USpop <- read.csv("./data/Population_Data_Join.csv")

## Population of CA cities
Confirmed_needed <- select(confirmed_US, c(Combined_Key, ncol(confirmed_US)))

Pop_confirmed <- left_join(Confirmed_needed, USpop, by = c("Combined_Key" = "Geographic.Area"))
Pop_confirmed <- Pop_confirmed %>% drop_na()
colnames(Pop_confirmed) = c("City", "Confirmed", "Population")

# Make plot
g1 <- ggplot(data = Pop_confirmed) +
  geom_point(mapping = aes(x = Population, y = Confirmed), color = "darkblue") +
  labs(
    title = "Confirmations Vs. Population",
    x = "Population",    
    y = "Confirmation Counts"
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(trans = "log2", limits=c(16,8388608), labels = scales::comma) +  
  scale_x_continuous(trans = "log2", limits=c(128,4194304), labels = scales::comma) +  
  theme(legend.title = element_blank())

## Make death vs confirmation for CA
Deaths_needed <- select(deaths_US, c(Combined_Key, ncol(deaths_US)))
Con_death_comp <- left_join(Confirmed_needed, Deaths_needed, by = "Combined_Key")
Con_death_comp <- Con_death_comp %>% drop_na()
colnames(Con_death_comp) = c("City", "Confirmed", "Deaths")
Con_death_comp <- filter(Con_death_comp, Confirmed > 0)

## Use plot_grid
g2 <- ggplot(data = Con_death_comp) +
  geom_point(mapping = aes(x = Confirmed, y = Deaths), color = "darkred") +
  labs(
    title = "Deaths Vs. Confirmations",
    x = "Confirmed Counts",
    y = "Deaths Counts"
    
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(trans = "log2", limits=c(8,32768), labels = scales::comma) +  
  scale_x_continuous(trans = "log2", limits=c(128,4194304), labels = scales::comma) +  
  theme(legend.title = element_blank())

# Side by side
plot_grid(g1, g2)
#plot_grid(obj4_p1, obj4_p2, vcol = 1)
#plot_grid(obj4_p1, ncol = 1)
#plot_grid(obj4_p2, ncol = 1)

