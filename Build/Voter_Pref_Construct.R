###################################################################################
### set-up

if (TRUE) {
  rm(list = ls())
  setwd("C:/Users/donggwan.kim/Desktop/Replication/Build")
  options(stringsAsFactors = FALSE)
  library(dplyr)
  library(ggplot2)
  library(readtext)
  library(lsa)
  library(stargazer)
  library(cowplot)
  library(plm)
  library(lmtest)
}

##################################################################################
### import USC/LA Times data

poll = read.csv('C:/Users/donggwan.kim/Desktop/Replication/Input Data/vote_pref_cleaned.csv')
poll$REP = 1 - poll$DEM

##################################################################################
### import Kantar Stradegy data 

df_final = read.csv('C:/Users/donggwan.kim/Desktop/Replication/Input Data/ad_data.csv')

###########################################################################################################
### daily measures of slant and consistency
###########################################################################################################

### create empty lists

if (TRUE) {
  temp_day = c()
  temp_c_slant = c()
  temp_c_cosine = c()
  temp_t_slant = c()
  temp_t_cosine = c()
}

### loop

for (i in seq(0, 131, by = 1)){
  
  # update the day list
  temp_day = c(temp_day, i)
  
  # 1 means Clinton and 0 means Trump
  for (j in c(1, 0)) {
    
    # if Clinton
    if (j == 1) {
      
      # select the political ads from Clinton
      tmp = df_final %>% filter((DEM == j) & (DAYS_TILL_ELECTION == i))
      slant = 0 # initial
      cosine = 0 # initial
      
      # if there is at least 1 ad
      if (nrow(tmp) >= 1) {
        TOT_RATPRE = sum(tmp$RATPRE)
        slant = slant + sum(tmp$slant_adjusted * (tmp$RATPRE / sum(tmp$RATPRE)))
        cosine = cosine + sum(tmp$cosine * (tmp$RATPRE / sum(tmp$RATPRE)))
        # if there are no political ads
      } else {
        slant = slant + 0
        cosine = cosine + 0
      }
      # update the slant list
      temp_c_slant = c(temp_c_slant, slant)
      temp_c_cosine = c(temp_c_cosine, cosine)
      
      # if Trump
    } else {
      
      # select the political ads from Trump
      tmp = df_final %>% filter((DEM == j) & (DAYS_TILL_ELECTION == i)) #  + 1
      slant = 0 # initial
      cosine = 0 # initial
      
      # if there is at least 1 ad
      if (nrow(tmp) >= 1) {
        TOT_RATPRE = sum(tmp$RATPRE)
        slant = slant + sum(tmp$slant_adjusted * (tmp$RATPRE / sum(tmp$RATPRE)))
        cosine = cosine + sum(tmp$cosine * (tmp$RATPRE / sum(tmp$RATPRE)))
        # if there are no political ads
      } else {
        slant = slant + 0
        cosine = cosine + 0
      }
      # update the slant list
      temp_t_slant = c(temp_t_slant, slant)
      temp_t_cosine = c(temp_t_cosine, cosine)
    }
  }
}

### combined the dataframes

if (TRUE) {
  tmp_c = data.frame(temp_day, temp_c_slant, temp_c_cosine)
  colnames(tmp_c) = c('DAYS_TILL_ELECTION', "Weighted_Slant", 'Weighted_Cosine')
  tmp_c$DEM = 1
  
  tmp_t = data.frame(temp_day, temp_t_slant, temp_t_cosine)
  colnames(tmp_t) = c('DAYS_TILL_ELECTION', "Weighted_Slant", 'Weighted_Cosine')
  tmp_t$DEM = 0
  
  a = rbind(tmp_c, tmp_t)
  
  rm(tmp_c, tmp_t, temp_day, temp_c_slant, temp_c_cosine, temp_t_slant, temp_t_cosine, tmp, i, j, slant, cosine, TOT_RATPRE)
}

### make it lagged 
### ads that aired 10 days before election affect vote preference measured 9 days before election

a$DAYS_TILL_ELECTION = a$DAYS_TILL_ELECTION - 1

### Merge it to the poll data

total = left_join(poll, a, by = c("DAYS_TILL_ELECTION", "DEM"))

### remove

rm(poll, a)

###########################################################################################################
### Month

if (TRUE) {
  a = ifelse(substr(total$DATE, start = 1, stop = 2) == '07', 7, 0)
  b = ifelse(substr(total$DATE, start = 1, stop = 2) == '08', 8, 0)
  c = ifelse(substr(total$DATE, start = 1, stop = 2) == '09', 9, 0)
  d = ifelse(substr(total$DATE, start = 1, stop = 2) == '10', 10, 0)
  e = ifelse(substr(total$DATE, start = 1, stop = 2) == '11', 11, 0)
  total$month = as.character(a + b + c + d + e)
}

### remove

rm(a, b, c, d, e)

#########################################################################################################
# Indicator for days with no advertising

if (TRUE) {
  total$no_ad_day = ifelse(total$Weighted_Slant == 0, 1, 0)
  total$ad_day = ifelse(total$Weighted_Slant != 0, 1, 0)
}

############################################################################################################
### audience size

### group by

temp = df_final %>%
  group_by(DEM, DAYS_TILL_ELECTION) %>%
  summarise(tot_audience = sum(RATPRE))

### make it lagged

temp$DAYS_TILL_ELECTION = temp$DAYS_TILL_ELECTION - 1

### merge

total = merge(total, temp, by = c('DEM', 'DAYS_TILL_ELECTION'), all.x = TRUE)

### fill NA with 0

total$tot_audience[is.na(total$tot_audience)] = 0

############################################################################################################
### competitor ads

temp = df_final %>%
  group_by(DEM, DAYS_TILL_ELECTION) %>%
  summarise(comp_audience = sum(RATPRE))

### change candidate dummy

temp$DEM = as.numeric(as.character(temp$DEM))
temp$DEM = 1 - temp$DEM

### make it lagged

temp$DAYS_TILL_ELECTION = temp$DAYS_TILL_ELECTION - 1

### merge

total = merge(total, temp, by = c('DEM', 'DAYS_TILL_ELECTION'), all.x = TRUE)

### fill NA with 0

total$comp_audience[is.na(total$comp_audience)] = 0

############################################################################################################
### additional variables

### create weighted avg. for each of the variables

temp = df_final %>%
  group_by(DEM, DAYS_TILL_ELECTION) %>%
  summarise(NEG = weighted.mean(ATT, RATPRE),
            # relative position in ad break
            POSITION = weighted.mean(AD_RATIO, RATPRE),
            # 1 if over 30 seconds
            LENGTH = weighted.mean(AD_LENGTH_3, RATPRE), 
            TOT_INSTANCE = n())

### Make it lagged

temp$DAYS_TILL_ELECTION = temp$DAYS_TILL_ELECTION -1 
temp$DEM = as.numeric(as.character(temp$DEM))

### append it to the main poll data

total = left_join(total, temp, by = c("DAYS_TILL_ELECTION", "DEM"))
head(total)

### fill NAs - this is not necessary as I will interact this with the "ad_day" variable

total$NEG[is.na(total$NEG)] = 0
total$POSITION[is.na(total$POSITION)] = 0
total$LENGTH[is.na(total$LENGTH)] = 0
total$TOT_INSTANCE[is.na(total$TOT_INSTANCE)] = 0

### clear

rm(temp)

###############################################################################################################
### Program Genre
###############################################################################################################

df_ad = df_final[ , c('DEM', 'ATT', 'RATPRE', 'DAYS_TILL_ELECTION', 'DATE', 'AD_LENGTH', 'AD_RATIO', 'PROG_GENRE')]
genre_list = sort(unique(df_ad$PROG_GENRE))

if (TRUE) {
  tmp_day = c()
  ### lists for Clinton
  tmp_c_gen_1 = c()
  tmp_c_gen_2 = c()
  tmp_c_gen_3 = c()
  tmp_c_gen_4 = c()
  tmp_c_gen_5 = c()
  tmp_c_gen_6 = c()
  tmp_c_gen_7 = c()
  tmp_c_gen_8 = c()
  tmp_c_gen_9 = c()
  ### lists for Trump
  tmp_t_gen_1 = c()
  tmp_t_gen_2 = c()
  tmp_t_gen_3 = c()
  tmp_t_gen_4 = c()
  tmp_t_gen_5 = c()
  tmp_t_gen_6 = c()
  tmp_t_gen_7 = c()
  tmp_t_gen_8 = c()
  tmp_t_gen_9 = c()
  
}

for (i in seq(1, 121, by = 1)) { 
  ### dgo up to 121 as it's the start day of the survey
  tmp_day = c(tmp_day, i)
  # Candidate
  for (j in c(1, 0)) {
    # Clinton
    if (j == 1) {
      tmp = filter(df_ad, (DEM == j) & (DAYS_TILL_ELECTION == (i)))
      tot = sum(tmp$RATPRE)
      tmp = tmp %>%
        group_by(PROG_GENRE) %>%
        summarise(ratio = sum(RATPRE)/tot)
      
      dat = as.data.frame(genre_list, ncol = 1)
      colnames(dat) = 'PROG_GENRE'
      dat2 = merge(dat, tmp, by = 'PROG_GENRE', all.x = TRUE)
      dat2[is.na(dat2)] <- 0
      
      tmp_c_gen_1 = c(tmp_c_gen_1, dat2[1, 2])
      tmp_c_gen_2 = c(tmp_c_gen_2, dat2[2, 2])
      tmp_c_gen_3 = c(tmp_c_gen_3, dat2[3, 2])
      tmp_c_gen_4 = c(tmp_c_gen_4, dat2[4, 2])
      tmp_c_gen_5 = c(tmp_c_gen_5, dat2[5, 2])
      tmp_c_gen_6 = c(tmp_c_gen_6, dat2[6, 2])
      tmp_c_gen_7 = c(tmp_c_gen_7, dat2[7, 2])
      tmp_c_gen_8 = c(tmp_c_gen_8, dat2[8, 2])
      tmp_c_gen_9 = c(tmp_c_gen_9, dat2[9, 2])
      # Trump
    } else {
      tmp = filter(df_ad, (DEM == j) & (DAYS_TILL_ELECTION == (i)))
      tot = sum(tmp$RATPRE)
      tmp = tmp %>%
        group_by(PROG_GENRE) %>%
        summarise(ratio = sum(RATPRE)/tot)
      
      dat = as.data.frame(genre_list, ncol = 1)
      colnames(dat) = 'PROG_GENRE'
      dat2 = merge(dat, tmp, by = 'PROG_GENRE', all.x = TRUE)
      dat2[is.na(dat2)] <- 0
      
      tmp_t_gen_1 = c(tmp_t_gen_1, dat2[1, 2])
      tmp_t_gen_2 = c(tmp_t_gen_2, dat2[2, 2])
      tmp_t_gen_3 = c(tmp_t_gen_3, dat2[3, 2])
      tmp_t_gen_4 = c(tmp_t_gen_4, dat2[4, 2])
      tmp_t_gen_5 = c(tmp_t_gen_5, dat2[5, 2])
      tmp_t_gen_6 = c(tmp_t_gen_6, dat2[6, 2])
      tmp_t_gen_7 = c(tmp_t_gen_7, dat2[7, 2])
      tmp_t_gen_8 = c(tmp_t_gen_8, dat2[8, 2])
      tmp_t_gen_9 = c(tmp_t_gen_9, dat2[9, 2])
    }
  }
}

if (TRUE) {
  tmp_c3 = data.frame(tmp_day, tmp_c_gen_1, tmp_c_gen_2, tmp_c_gen_3, 
                      tmp_c_gen_4, tmp_c_gen_5, tmp_c_gen_6,
                      tmp_c_gen_7, tmp_c_gen_8, tmp_c_gen_9)
  colnames(tmp_c3) = c('DAYS_TILL_ELECTION', "AWARDS", "COMEDY", "DOCUMENTARY", "DRAMA_ADVENTURE",  
                       "MOVIE", "NEWS_POLITICAL", "SLICE_OF_LIFE", "SPORTS", 'SUSPENSE_MYSTERY') # sorted
  tmp_c3$DEM = 1
  
  tmp_t3 = data.frame(tmp_day, tmp_t_gen_1, tmp_t_gen_2, tmp_t_gen_3,
                      tmp_t_gen_4, tmp_t_gen_5, tmp_t_gen_6,
                      tmp_t_gen_7, tmp_t_gen_8, tmp_t_gen_9) # sorted
  colnames(tmp_t3) = c('DAYS_TILL_ELECTION', "AWARDS", "COMEDY", "DOCUMENTARY", "DRAMA_ADVENTURE",  
                       "MOVIE", "NEWS_POLITICAL", "SLICE_OF_LIFE", "SPORTS", 'SUSPENSE_MYSTERY')
  tmp_t3$DEM = 0
  
  a = rbind(tmp_c3, tmp_t3)
  
  ### clear
  rm(tmp_day, tmp_c_gen_1, tmp_c_gen_2, tmp_c_gen_3, 
     tmp_c_gen_4, tmp_c_gen_5, tmp_c_gen_6,
     tmp_c_gen_7, tmp_c_gen_8, tmp_c_gen_9,  
     tmp_t_gen_1, tmp_t_gen_2, tmp_t_gen_3,
     tmp_t_gen_4, tmp_t_gen_5, tmp_t_gen_6,
     tmp_t_gen_7, tmp_t_gen_8, tmp_t_gen_9,
     i, j, tot, tmp, dat, dat2, genre_list,
     tmp_c3, tmp_t3)
}

### make it lagged

a$DAYS_TILL_ELECTION = a$DAYS_TILL_ELECTION - 1

### merge

total = left_join(total, a, by = c("DAYS_TILL_ELECTION", "DEM"))

### clear

rm(a, df_ad)

###############################################################################################################
### 10 major networks + others
###############################################################################################################

df_ad = df_final[ , c('DEM', 'ATT', 'DAYS_TILL_ELECTION', 'DATE', 'RATPRE', 'NETWORK')]

### 10 networks

net_list = c('A&E', 'CNN', 'DISC', 'FNEW', 'HIST', 
             'ID', 'LIFE', 'MSNB', 'TLC' ,'NBC')

### change network names

df_ad$NETWORK = ifelse(df_ad$NETWORK %in% net_list, df_ad$NETWORK, 'OTHERS')
sort(unique(df_ad$NETWORK))

### lists

if (TRUE) {
  tmp_day = c()
  
  list_c_1 = c()
  list_c_2 = c()
  list_c_3 = c()
  list_c_4 = c()
  list_c_5 = c()
  list_c_6 = c()
  list_c_7 = c()
  list_c_8 = c()
  list_c_9 = c()
  list_c_10 = c()
  list_c_11 = c()
  
  list_t_1 = c()
  list_t_2 = c()
  list_t_3 = c()
  list_t_4 = c()
  list_t_5 = c()
  list_t_6 = c()
  list_t_7 = c()
  list_t_8 = c()
  list_t_9 = c()
  list_t_10 = c()
  list_t_11 = c()
  
}

### for loop

for (i in seq(1, 121, by = 1)) {
  tmp_day = c(tmp_day, i)
  # Candidate
  for (j in c(1, 0)) {
    # Clinton
    if (j == 1) {
      tmp = filter(df_ad, (DEM == j) & (DAYS_TILL_ELECTION == (i))) # + 1
      tmp = tmp %>%
        group_by(NETWORK) %>%
        summarise(ratio = sum(RATPRE)/sum(tmp$RATPRE))
      
      dat = as.data.frame(sort(unique(df_ad$NETWORK)), ncol = 1)
      colnames(dat) = 'NETWORK'
      
      dat2 = merge(dat, tmp, by = 'NETWORK', all.x = TRUE)
      dat2[is.na(dat2)] <- 0
      
      list_c_1 = c(list_c_1, dat2[1, 2])
      list_c_2 = c(list_c_2, dat2[2, 2])
      list_c_3 = c(list_c_3, dat2[3, 2])
      list_c_4 = c(list_c_4, dat2[4, 2])
      list_c_5 = c(list_c_5, dat2[5, 2])
      list_c_6 = c(list_c_6, dat2[6, 2])
      list_c_7 = c(list_c_7, dat2[7, 2])
      list_c_8 = c(list_c_8, dat2[8, 2])
      list_c_9 = c(list_c_9, dat2[9, 2])
      list_c_10 = c(list_c_10, dat2[10, 2])
      list_c_11 = c(list_c_11, dat2[11, 2])
      
      # Trump
    } else {
      
      tmp = filter(df_ad, (DEM == j) & (DAYS_TILL_ELECTION == (i))) # + 1
      tmp = tmp %>%
        group_by(NETWORK) %>%
        summarise(ratio = sum(RATPRE)/sum(tmp$RATPRE))
      
      dat = as.data.frame(sort(unique(df_ad$NETWORK)), ncol = 1)
      colnames(dat) = 'NETWORK'
      
      dat2 = merge(dat, tmp, by = 'NETWORK', all.x = TRUE)
      dat2[is.na(dat2)] <- 0
      
      list_t_1 = c(list_t_1, dat2[1, 2])
      list_t_2 = c(list_t_2, dat2[2, 2])
      list_t_3 = c(list_t_3, dat2[3, 2])
      list_t_4 = c(list_t_4, dat2[4, 2])
      list_t_5 = c(list_t_5, dat2[5, 2])
      list_t_6 = c(list_t_6, dat2[6, 2])
      list_t_7 = c(list_t_7, dat2[7, 2])
      list_t_8 = c(list_t_8, dat2[8, 2])
      list_t_9 = c(list_t_9, dat2[9, 2])
      list_t_10 = c(list_t_10, dat2[10, 2])
      list_t_11 = c(list_t_11, dat2[11, 2])
    }
  }
}

if (TRUE) {
  
  tmp_c3 = data.frame(tmp_day, 
                      list_c_1, list_c_2, list_c_3, list_c_4,
                      list_c_5, list_c_6, list_c_7, list_c_8, 
                      list_c_9, list_c_10, list_c_11)
  colnames(tmp_c3) = c('DAYS_TILL_ELECTION', sort(unique(df_ad$NETWORK)))
  tmp_c3$DEM = 1
  
  tmp_t3 = data.frame(tmp_day, 
                      list_t_1, list_t_2, list_t_3, list_t_4,
                      list_t_5, list_t_6, list_t_7, list_t_8, 
                      list_t_9, list_t_10, list_t_11)
  colnames(tmp_t3) = c('DAYS_TILL_ELECTION', sort(unique(df_ad$NETWORK)))
  tmp_t3$DEM = 0
  
  a = rbind(tmp_c3, tmp_t3)
}

### make it lagged

a$DAYS_TILL_ELECTION = a$DAYS_TILL_ELECTION - 1

### merge it to the list

total = left_join(total, a, by = c("DAYS_TILL_ELECTION", "DEM"))
total$ANE = total$`A&E`

### clear

rm(a, i, j, dat, dat2, tmp, tmp_day, tmp_c3, tmp_t3,
   list_c_1, list_c_2, list_c_3, list_c_4,
   list_c_5, list_c_6, list_c_7, list_c_8, 
   list_c_9, list_c_10, list_c_11, list_t_1, 
   list_t_2, list_t_3, list_t_4,
   list_t_5, list_t_6, list_t_7, list_t_8, 
   list_t_9, list_t_10, list_t_11, net_list)

###############################################################################################################
### save it 
###############################################################################################################

# write.csv(total, 'C:/Users/donggwan.kim/Desktop/Replication/Data/VOTER_PREF_DATA.csv')
