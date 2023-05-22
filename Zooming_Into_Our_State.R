## Objective 4 - Zooming into our State
# After reading the top tables, you are stunned! The US overtakes every other
# country in terms of COVID-19 confirmations. As such, you are concerned about
# the state you live in and would like to understand how COVID-19 events have
# shaped the trajectory of the disease. Create two scatter plots to gain a
# better understanding. The first scatter plot should be California’s trajectory
# for confirmations. The second scatter plot should show California’s top three
# city trajectories for confirmations. You are interested in studying how the
# vaccine affected the number of confirmations. The Moderna vaccine was first
# available as an emergency use authorized (EUA) vaccine and required two shots
# spaced six weeks apart. Indicate on the plots the day the second dosage was
# given to those that received the first dosage the day Moderna was EUA 
# (January 29th, 2021). As a diligent scientist that knows that new COVID
# variants have mutations in the spike protein (the region that the vaccine was
# developed for), you also want to study how confirmation rates change as new 
# variants become the dominant infectious strain. Indicate on the plots when 
# the delta and omicron variants became the dominant strain in California
# (May 11th, 2021 and November 26th, 2021 respectively). In the example below,
# the function plot_grid from the R package cowplot was to organize the graphs 
# into a grid to more easily compare statewide vs top city plots.

library(knitr)
library(magrittr)
library(RCurl)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(stringr)


confirmed_US_download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
confirmed_US <- read.csv(text=USconfirmed_download, stringsAsFactors = FALSE)

deaths_US_download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
deaths_US <- read.csv(text=USdeath_download, stringsAsFactors = FALSE)

### Objective 3 - Zooming Into Our State and California
#Zooming Into Our State
#After reading the top tables, you are stunned! The US overtakes every other country in terms of COVID-19 confirmations. As such, you are concerned about the state you live in and would like to understand how COVID-19 events have shaped the trajectory of the disease. Create two scatter plots to gain a better understanding. The first scatter plot should be California’s trajectory for confirmations. The second scatter plot should show California’s top three city trajectories for confirmations. You are interested in studying how the vaccine affected the number of confirmations. The Moderna vaccine was first available as an emergency use authorized (EUA) vaccine and required two shots spaced six weeks apart. Indicate on the plots the day the second dosage was given to those that received the first dosage the day Moderna was EUA (January 29th, 2021). As a diligent scientist that knows that new COVID variants have mutations in the spike protein (the region that the vaccine was developed for), you also want to study how confirmation rates change as new variants become the dominant infectious strain. Indicate on the plots when the delta and omicron variants became the dominant strain in California (May 11th, 2021 and November 26th, 2021 respectively). In the example below, the function plot_grid from the R package cowplot was to organize the graphs into a grid to more easily compare statewide vs top city plots.

#<!-- Chart 1 -->
  
 # ```{r obj3-code1, echo=TRUE, eval=TRUE}
# creates new data frame for only CA confirmed cases
dfConfirmedCAChartData <- confirmed_US %>%
  
  # removes all columns with numeric values that aren't case counts
  select(-(`UID`)) %>%
  select(-(`code3`)) %>%
  select(-(`FIPS`)) %>%
  select(-(`Lat`)) %>%
  select(-(`Long_`)) %>%
  
  # retains only the California row
  filter(`Province_State` == "California") %>%
  
  # groups by State
  group_by(`Province_State`) %>%
  
  # totals all columns that are numeric
  summarise_if(is.numeric, sum) %>%
  
  # gathers data into long form
  gather(key=Date, value=`Confirmed Cases`, -`Province_State`) %>%
  
  # converts the date column into real dates for the chart
  mutate(`Date`=as.Date(`Date`, format = "%m/%d/%y"))
#```


#```{r obj3-code2, echo=TRUE, eval=TRUE}
# creates new data frame for only CA deaths
dfDeathsCAChartData <- deaths_US %>%
  
  # removes all columns with numeric values that aren't case counts
  select(-(`UID`)) %>%
  select(-(`code3`)) %>%
  select(-(`FIPS`)) %>%
  select(-(`Lat`)) %>%
  select(-(`Long_`)) %>%
  select(-(`Population`)) %>%
  
  # retains only the California row
  filter(`Province_State` == "California") %>%
  
  # groups by State
  group_by(`Province_State`) %>%
  
  # totals all columns that are numeric
  summarise_if(is.numeric, sum) %>%
  
  # gathers data into long form
  gather(key=Date, value=`Deaths`, -`Province_State`) %>%
  
  # converts the date column into real dates for the chart
  mutate(`Date`=as.Date(`Date`, format = "%m/%d/%y"))
#```

#Below is a chart showing the deaths and confirmed cases for the state of California over time. The black vertical line provides a visual representation of when the first shutdowns occurred on March 19, 2020.

#```{r obj3-chart1, including=FALSE, echo=TRUE, eval=FALSE}
obj4chart1colors <- c("Confirmed Cases" = "blue", "Deaths" = "red")

# these are two disparate time series glued together
ggplot() +
  geom_line(data = dfConfirmedCAChartData,
            aes(x = `Date`, y = `Confirmed Cases`,
                color = "Confirmed Cases")) +
  geom_line(data = dfDeathsCAChartData,
            aes(x = `Date`, y = `Deaths`,
                color = "Deaths")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Cases", color = "California Total") +
  ggtitle("California's Trajectory for COVID-19") +
  scale_color_manual(values = obj4chart1colors) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")),
             color = "black")
#```


#<!-- Chart 2 -->
  
#  ```{r obj3-code3, echo=TRUE, eval=TRUE}
# reuses earlier data frame for only CA confirmed cases by county
dfConfirmedCAChartData <- confirmed_US %>%
  
  # removes all columns with numeric values that aren't case counts
  select(-(`UID`)) %>%
  select(-(`code3`)) %>%
  select(-(`FIPS`)) %>%
  select(-(`Lat`)) %>%
  select(-(`Long_`)) %>%
  
  # retains only the California rows
  filter(`Province_State` == "California") %>%
  
  # retains only the rows for San Diego, Riverside, LA
  filter(`Admin2` %in%
           c("San Diego", "Riverside", "Los Angeles")) %>%
  
  # groups by County (aka Admin2)
  group_by(`Admin2`) %>%
  
  # totals all columns that are numeric
  summarise_if(is.numeric, sum) %>%
  
  # gathers data into long form
  gather(key=Date, value=`Confirmed Cases`, -`Admin2`) %>%
  
  # converts the date column into real dates for the chart
  mutate(`Date`=as.Date(`Date`, format = "%m/%d/%y"))
#```

#Below is a chart showing the confirmed cases over time, only for the counties of Los Angeles, Riverside, and San Diego. The black vertical line provides a visual representation of when the first shutdowns occurred on March 19, 2020.


#```{r obj3-chart2, including=FALSE, echo=TRUE, eval=FALSE}
c1 <- ggplot(dfConfirmedCAChartData,
             aes(x = `Date`, y = `Confirmed Cases`,
                 group = `Admin2`)) +
  geom_line(aes(color=`Admin2`), size = 2) +
  ggtitle("") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Cases", color = "")
#+
#geom_vline(xintercept = as.numeric(as.Date("2020-03-19")),
#           color = "black")

## plotting vertical lines
c2 <- c1 + 
  # geom_vline(aes(xintercept=as.Date("2020-03-19")),color="white", linetype = "longdash") +
  
  geom_vline(aes(xintercept=as.Date("2021-03-19")),color="darkgreen", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-07-04")),color="darkred", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-11-20")),color="brown", linetype = "longdash")


## labeling vertical lines
c3 <- c2 + geom_text(aes(as.Date("2020-03-19"), 2500000, label ="", hjust="right"))  + geom_text(aes(as.Date("2021-01-29"), 2500000, label ="", hjust="right")) + geom_text(aes(as.Date("2021-07-04"), 2500000, label ="", hjust="right")) + geom_text(aes(as.Date("2021-12-20"), 2500000, label ="", hjust="right"))

## adding tick marks
c4 <- c3 + scale_x_continuous(breaks = c(as.Date("2020-03-19"), as.Date("2021-01-29"), as.Date("2021-07-04"), as.Date("2021-12-20")), labels = c("2020-07", "2021-01", "2021-07", "2022-01")) 
plot_grid(c4, ncol = 1, align = "h")
#```


#```{r obj3-chart3, echo=TRUE, eval=TRUE}
## Isolate CA confirmations
confirmed_US %>% select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key)) %>% filter(Province_State == "California") %>% data.frame() -> CA
CA %<>% select(-Province_State)
colnames(CA) <- gsub("X","",colnames(CA))
colnames(CA) <- as.Date(colnames(CA) , format = "%m.%d.%y")
CA_sum <- colSums(CA)
date <- colnames(CA)
ready <- data.frame(date,CA_sum)
ready$date <- as.Date(ready$date)

## Making graph for state
p <- ggplot(ready, aes(date,CA_sum)) + geom_point(color="darkblue") + theme_gray() + labs(x="", y="Confirmations", title = "COVID-19 confirmations in California")

## plotting vertical lines
p1 <- p + geom_vline(aes(xintercept=as.Date("2020-03-19")),color="green", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-01-29")),color="darkgreen", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-07-04")),color="darkred", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-12-20")),color="brown", linetype = "longdash")

## labeling vertical lines
p2 <- p1 + geom_text(aes(as.Date("2020-03-19"), 7000000, label ="SAH", hjust="right"))  + geom_text(aes(as.Date("2021-01-29"), 7000000, label ="EUA + 6 WEEKS", hjust="right")) + geom_text(aes(as.Date("2021-07-04"), 7000000, label ="Delta", hjust="right")) + geom_text(aes(as.Date("2021-12-20"), 7000000, label ="Omicron", hjust="right"))

## adding tick marks
p3 <- p2 + scale_x_continuous(breaks = c(as.Date("2020-03-19"), as.Date("2021-01-29"), as.Date("2021-07-04"), as.Date("2021-12-20")), labels = c("2020-03-19", "2020-01-29", "2020-07-04", "2021-12-20")) 


c1 <- ggplot(dfConfirmedCAChartData,
             aes(x = `Date`, y = `Confirmed Cases`,
                 group = `Admin2`)) +
  #geom_line(aes(color=`Admin2`), size = 2) +     
  geom_line(aes(color=`Admin2`), size = 2) +
  ggtitle("") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Cases", color = "")
#+
#geom_vline(xintercept = as.numeric(as.Date("2020-03-19")),
#           color = "black"
#             )

################

## plotting vertical lines
c2 <- c1 + 
  # geom_vline(aes(xintercept=as.Date("2020-03-19")),color="white", linetype = "longdash") +
  
  geom_vline(aes(xintercept=as.Date("2021-03-19")),color="darkgreen", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-07-04")),color="darkred", linetype = "longdash") + geom_vline(aes(xintercept=as.Date("2021-11-20")),color="brown", linetype = "longdash")


## labeling vertical lines
c3 <- c2 + geom_text(aes(as.Date("2020-03-19"), 2500000, label ="", hjust="right"))  + geom_text(aes(as.Date("2021-01-29"), 2500000, label ="", hjust="right")) + geom_text(aes(as.Date("2021-07-04"), 2500000, label ="", hjust="right")) + geom_text(aes(as.Date("2021-12-20"), 2500000, label ="", hjust="right"))

## adding tick marks
c4 <- c3 + scale_x_continuous(breaks = c(as.Date("2020-03-19"), as.Date("2021-01-29"), as.Date("2021-07-04"), as.Date("2021-12-20")), labels = c("2020-07", "2021-01", "2021-07", "2022-01")) 

#plot_grid(c4)
#plot_grid(p3, ncol = 1, align = "h")
plot_grid(p3, c4, ncol = 1, align = "h")
#```

#California has reached past 3.5million confirmed cases of COVID-19 since January 2020. Number of deaths has remained well below 100,000.
