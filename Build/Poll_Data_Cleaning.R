###################################################################################
### set-up

if (TRUE) {
  rm(list = ls())
  setwd("C:/Users/donggwan.kim/Desktop/")
  options(stringsAsFactors = FALSE)
  library(dplyr)
}

##################################################################################
### import data

### USC Dornsife / LA Times poll can be found and downloaded from the link below:
### https://graphics.latimes.com/usc-presidential-poll-dashboard/

poll = read.csv('Daily_Polls.csv')

### Clinton

poll_clinton = poll[ , c('DATE', 'CLINTON_PERCENT')]
poll_clinton$Vote_Share = poll_clinton$CLINTON_PERCENT
poll_clinton$CLINTON_PERCENT = NULL
poll_clinton$DEM = 1

### Trump

poll_trump = poll[ , c('DATE', 'TRUMP_PERCENT')]
poll_trump$Vote_Share = poll_trump$TRUMP_PERCENT
poll_trump$TRUMP_PERCENT = NULL
poll_trump$DEM = 0

### combine

poll = rbind(poll_clinton, poll_trump)
rm(poll_clinton, poll_trump)

##################################################################################
### add weeks

### write a function

weekly_indicator = function(arg) {
  arg = as.character(arg)
  date_list = filter(poll, DEM == 1)$DATE
  week = 0
  if (arg %in% c("11/6/2016", "11/7/2016")) {
    week = week + 18
  } else {
    position = match(arg, date_list)
    week = week + ceiling(position / 7) 
  }
  return (week)
}

print(weekly_indicator("7/11/2016")) # 1
print(weekly_indicator("7/17/2016")) # 2
print(weekly_indicator("7/25/2016")) # 3
print(weekly_indicator("11/4/2016")) # 17
print(weekly_indicator("11/6/2016")) # 18

### apply the function

temp_list = c()
for (i in poll$DATE) {
  i = as.character(i)
  temp_list = c(temp_list, weekly_indicator(i))
}
poll$WEEK = temp_list
rm(temp_list)

##################################################################################
### lagged

Lagged_VS = c(NA, filter(poll, DEM == 1)$Vote_Share[1: nrow(filter(poll, DEM == 1)) - 1],
              NA, filter(poll, DEM == 0)$Vote_Share[1: nrow(filter(poll, DEM == 0)) - 1])
poll$Lagged_VS = Lagged_VS
rm(Lagged_VS)

##################################################################################
### days till election (a variable used in the advertising data)

poll$DAYS_TILL_ELECTION = c(seq(121, 1, by = -1), seq(121, 1, by = -1))

### save it

head(filter(poll, DEM == 1))
tail(filter(poll, DEM == 1))

# write.csv('vote_pref_cleaned.csv')

