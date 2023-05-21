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

#Global data sets downloaded on 05/28/2021
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

#US data sets downloaded on 05/28/2021
deaths_US <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
confirmed_US <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")


Population <- deaths_US$Population
City <- deaths_US$Admin2
Confirmation <- confirmed_US$'5/20/22'
State <- confirmed_US$Province_State
Deaths <- deaths_US$'5/20/22'

data <- data.frame(City, State, Population, Confirmation, Deaths)


g1 <- ggplot(data = data) +
  geom_point(mapping = aes(x = Population, y = Confirmation), color = "darkblue") +
  labs(
    title = "Confirmations Vs. Population",
    y = "Confirmations Counts",
    x = "Population"
  ) +
  scale_y_log10(limits=c(16,8388608), labels = scales::comma) +
  scale_x_log10(limits=c(128,4194304), labels = scales::comma) 

g2 <- ggplot(data = data) +
  geom_point(mapping = aes(x = Confirmation, y = Deaths), color = "darkred") +
  labs(
    title = "Deaths Vs. Confirmations",
    y = "Deaths Counts",
    x = "Confirmed Counts"
  ) +
  scale_y_log10(limits=c(8,32768), labels = scales::comma) +
  scale_x_log10(limits=c(128,4194304), labels = scales::comma)


plot_grid(g1, g2) 
