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

library(readr)
library(tidyverse)
library(cowplot)

# Extract data of US confirmed COVID cases
confirmed_US <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# Extract data of US COVID deaths
deaths_US <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")


## Make death vs confirmation for CA
Population <- deaths_US$Population
City <- deaths_US$Admin2
Confirmation <- confirmed_US$'5/28/21'
State <- confirmed_US$Province_State
Deaths <- deaths_US$'5/28/21'

#data <- data.frame(City, State, Population, Confirmation, Deaths)

# Make plot
p1 <- ggplot(data = data) +  
  geom_point(mapping = aes(x = Population, y = Confirmation), color = "darkblue") +
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

## Use plot_grid
p2 <- ggplot(data = data) +
  geom_point(mapping = aes(x = Confirmation, y = Deaths), color = "darkred") +
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
plot_grid(p1, p2)