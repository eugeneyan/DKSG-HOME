# install and load packages as necessary
# install.packages('ggplot2')
# install.packages('treemap')
# install.packages('dplyr')
library(ggplot2)
library(treemap)
library(dplyr)

###############################################################################
#20: How many migrants do we see by year, broken down by FDW, 
#    construction worker etc., and nationality 
###############################################################################
# load data (ensure home_data_anon.csv is in working directory)
# use getwd() to figure out what your working directory is
home <- read.csv("home_data_anon.csv", header = T, na.strings = c('NA', ''))

# reformatting the dates so R can understand it (done previously)
# home$case_opening_dt <- as.Date(home$case_opening_dt, "%d/%m/%Y")
# home$case_closing_dt <- as.Date(home$case_closing_dt, "%d/%m/%Y")
# home$case_year <- as.numeric(format(home$case_opening_dt, '%Y'))
# home$case_yearmon <- as.yearmon(home$case_opening_dt)

# plot of proportion of domestic vs non-domestic over years
ggplot(data = home, aes(x = case_year, y = ..count..)) + 
  geom_bar(aes(fill = worker_tp), binwidth = 0.5, position = 'dodge') + 
  labs(title = 'No. of domestic vs non-domestic over time', 
       x = 'Year-Month', y = 'No. of domesic workers') +
  annotate("text", x = 2018.5, y = 200, label = "There are insignificant cases\n in 2009 and beyond 2020")

# removing outliers from the dates
home <- home[!is.na(home$case_opening_dt), ]  # remove where date = NA
home <- home[!home$case_year > 2016, ]  # remove where date is in future
home <- home[!home$case_year < 2010, ]  # very few cases in 2009

# plot of proportion of domestic vs non-domestic over time
ggplot(data = home, aes(x = case_year, y = ..count..)) + 
  geom_bar(aes(fill = worker_tp), binwidth = 0.5, position = 'dodge') + 
  labs(title = 'No. of domestic vs non-domestic over time', 
       x = 'Year-Month', y = 'No. of domesic workers')


###################################################################################
#19: As a case worker, given a specific nationality, I want to know which 
#    employement agencies are the top sources
###################################################################################
# Nationality was available via natL_code (drop down menu) 
# and natl_other (free text)
summary(home$natl_other)  # have to be recoded and merged into natl_code
# after recoding the nationality data (thanks to Yan for recoding)
summary(home$natl_code)

### Plot domesitic workers by country
# Restructure the data
nat <- home %>%
  filter(worker_tp == 'D') %>%  # select domestic workers only
  group_by(natl_code)  %>%  # group them by nationality
  summarize(count = n()) %>%  # calculate counts of each nationality
  mutate(percent = round(100*count/sum(count), 2)) %>%  # calculate percentages
  arrange(desc(count))  # arrange by descending order

# how does the restructured data look like?
nat

# make nationality a factor (otherwise, they will be arranged 
# alphabetically on the chart)
nat$natl_code <- factor(nat$natl_code, levels = nat$natl_code, ordered = TRUE)

# create plot
ggplot(data = nat, aes(x = natl_code, y = count)) + 
  geom_bar(stat = 'identity') + 
  labs(title = 'No. of domestic workers by Country', 
       x = 'Countries', y = 'No. of domestic workers') + 
  geom_text(data = nat, aes(label = paste0(percent, '%'), 
                            y = count + 75), size = 4)


### Plot domestic workers by agency
# group by agencies only
agen <- home %>%
  filter(worker_tp == 'D') %>%
  group_by(agency_nm)  %>%
  summarize(count = n()) %>%
  mutate(percent = round(100*count/sum(count), 2)) %>%
  na.omit() %>%
  arrange(desc(count))

# how does agen look like?
agen

# similar to above, make agency a factor
agen$agency_nm <- factor(agen$agency_nm, levels = agen$agency_nm)

# counts by agency
ggplot(data = agen[1:20, ], aes(x = factor(agency_nm, levels = agency_nm, ordered = T), y = count)) + geom_bar(stat = 'identity') + labs(title = ' No. of domestic workers by Agencies', x = 'Agencies', y = 'No. of domestic workers') + geom_text(data = agen[1:20, ], aes(label = paste0(percent, '%'), y = count, vjust = -0.3), size = 3.5)


### Plot Agencies by Nationality
# group by agencies and nationality
nat.agen <- home %>%
  filter(worker_tp == 'D') %>%  # select domestic workers only
  group_by(natl_code, agency_nm)  %>%  # group by nationality and agency
  summarize(count = n()) %>%  # calculate counts
  na.omit() %>%  # exclude NAs (where some agencies are not listed)
  mutate(percent = round(100*count/sum(count), 2)) %>%  # calculate percentages
  arrange(natl_code, desc(count))  # arrange by descending order

# how does the data look like after restructuring it by nationality 
# and agency?
nat.agen

# create tree map of nat.agen (thanks to Ricky for the suggestion)
# depending on your computer, this make take a while to run
treemap(as.data.frame(nat.agen), c('natl_code', 'agency_nm'), c('count'), 
        title = 'Agencies Placing Domestic Workers by Country')

# Any questions?

