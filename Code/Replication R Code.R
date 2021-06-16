###################################################################################
### Description: replicate the main results along with the descriptive statistics
### Note: Given NDAs, we disguise the WOM variables and remove the audience size.
###       Further, to avoid reposting public data, we disguise voter preference.
###       As a result, estimates may be different from those in the original paper.
###################################################################################

###################################################################################
### Set-up
###################################################################################

rm(list = ls())
setwd("C:/Users/donggwan.kim/Desktop/Replication/Code")
options(stringsAsFactors = FALSE)
library(dplyr)
library(stargazer)
library(lfe)

###################################################################################
### 1. WOM
###################################################################################

### import data

df_wom = read.csv('C:/Users/donggwan.kim/Desktop/Replication/Data/WOM_DATA.csv')

### descriptive statistics (Part of Table 1)

df_wom$candidate = ifelse(df_wom$DEM == 1, 'Clinton', 'Trump')

df_wom %>%
  group_by(candidate) %>%
  summarise(num_ad_airings = n(),
            pct_attact = mean(ATT),
            ad_position_in_break = mean(AD_POSITION))

### descriptive statistics (Table 3)

df_wom %>%
  group_by(candidate) %>%
  summarise(slant_min = min(slant),
            slant_10th = quantile(slant, 0.1),
            slant_50th = quantile(slant, 0.5),
            slant_90th = quantile(slant, 0.9),
            slant_max = max(slant),
            consistency_min = min(consistency),
            consistency_10th = quantile(consistency, 0.1),
            consistency_50th = quantile(consistency, 0.5),
            consistency_90th = quantile(consistency, 0.9),
            consistency_max = max(consistency))

### descriptive statistics (Part of Table 4)

df_wom$pct_change_wom = (exp(df_wom$LOG_CAN_WOM_5_POST) - exp(df_wom$LOG_CAN_WOM_5_PRE)) / (exp(df_wom$LOG_CAN_WOM_5_PRE) + 1)

df_wom %>%
  group_by(candidate) %>%
  summarise(pct_change_mean = mean(pct_change_wom),
            pct_change_sd = sd(pct_change_wom),
            pct_change_min = min(pct_change_wom),
            pct_change_25th = quantile(pct_change_wom, 0.25),
            pct_change_50th = quantile(pct_change_wom, 0.5),
            pct_change_75th = quantile(pct_change_wom, 0.75),
            pct_change_max = max(pct_change_wom))

### run regressions (Table 5)

reg1 = felm(LOG_CAN_WOM_5_POST ~ LOG_CAN_WOM_5_PRE + DEM + AD_LENGTH_IND + AD_POSITION + 
              slant + consistency + ATT | WEEK + Day + TIME_WINDOW + PROG_GENRE + NETWORK | 0 | DEM, data = df_wom)

reg2 = felm(LOG_CAN_WOM_5_POST ~ LOG_CAN_WOM_5_PRE + DEM + AD_LENGTH_IND + AD_POSITION + 
              slant:PRE_OCT_1 + slant:POST_OCT_1 +
              PRE_OCT_1:consistency + POST_OCT_1:consistency + 
              PRE_OCT_1:ATT + POST_OCT_1:ATT | WEEK + Day + TIME_WINDOW + PROG_GENRE + NETWORK | 0 | DEM, data = df_wom)

### results (without the audience size variable)

stargazer(reg1, reg2, 
          se = list(coef(summary(reg1))[ ,2],
                    coef(summary(reg2))[ ,2]),
          dep.var.labels=c("ln(WoM Post)"),
          column.labels = c("5 min", "5 min"),
          covariate.labels = c('log(WoM Pre)', 'Pro-Clinton Ad', 
                               'Ad Length (1 if > 30 Seconds)', 'Ad Position in Break', 
                               'Slant', 'Consistency', 'Attack',
                               'Slant X Pre Oct. 1', 'Slant X Post Oct. 1',
                               'Consistency X Pre Oct. 1', 'Consistency X Post Oct. 1',
                               'Attack X Pre Oct. 1', 'Attack X Post Oct. 1'),
          add.lines = list(c("Week, day, time FEs", 'Yes', 'Yes'),
                           c("Prog. Genre FEs", 'Yes', 'Yes'),
                           c("Network FEs", 'Yes', 'Yes')),
          omit.stat=c("LL","ser","f"),
          type = 'text')

###################################################################################
### 2. Voter Preference Analysis
###################################################################################

### import data

df_vp = read.csv('C:/Users/donggwan.kim/Desktop/Replication/Data/VOTER_PREF_DATA.csv')
head(df_vp)

### descriptive statistics (Part of Table 4)

df_vp$candidate = ifelse(df_vp$DEM == 1, 'Clinton', 'Trump')

df_vp %>%
  group_by(candidate) %>%
  summarise(pct_change_mean = mean(Vote_Share),
            pct_change_sd = sd(Vote_Share),
            pct_change_min = min(Vote_Share),
            pct_change_25th = quantile(Vote_Share, 0.25),
            pct_change_50th = quantile(Vote_Share, 0.5),
            pct_change_75th = quantile(Vote_Share, 0.75),
            pct_change_max = max(Vote_Share))

### run regressions

plm1 = plm(Vote_Share ~ Lagged_VS + no_ad_day + 
             ad_day:Weighted_Slant + ad_day:Weighted_Cosine + ad_day:NEG + 
             ad_day:TOT_INSTANCE + ad_day:POSITION + ad_day:LENGTH + 
             ad_day:AWARDS + ad_day:COMEDY + ad_day:DOCUMENTARY + ad_day:DRAMA_ADVENTURE +
             ad_day:MOVIE + ad_day:NEWS_POLITICAL + ad_day:SLICE_OF_LIFE + ad_day:SPORTS + ad_day:SUSPENSE_MYSTERY +
             ad_day:ANE + ad_day:CNN + ad_day:DISC + ad_day:FNEW + ad_day:HIST + ad_day:ID + 
             ad_day:LIFE + ad_day:MSNB + ad_day:NBC + ad_day:OTHERS + ad_day:TLC +
             as.factor(WEEK),
           data = na.omit(df_vp), 
           model = "within", 
           index = c("DEM"))

plm2 = plm(Vote_Share ~ Lagged_VS + no_ad_day +
             ad_day:Weighted_Slant:before_oct + ad_day:Weighted_Slant:after_oct +
             ad_day:Weighted_Cosine:before_oct + ad_day:Weighted_Cosine:after_oct +
             ad_day:NEG:before_oct + ad_day:NEG:after_oct +
             ad_day:TOT_INSTANCE + ad_day:POSITION + ad_day:LENGTH + 
             ad_day:AWARDS + ad_day:COMEDY + ad_day:DOCUMENTARY + ad_day:DRAMA_ADVENTURE +
             ad_day: MOVIE + ad_day:NEWS_POLITICAL + ad_day:SLICE_OF_LIFE + ad_day:SPORTS + ad_day:SUSPENSE_MYSTERY +
             ad_day:ANE + ad_day:CNN + ad_day:DISC + ad_day:FNEW + ad_day:HIST + ad_day:ID + 
             ad_day:LIFE + ad_day:MSNB + ad_day:NBC + ad_day:OTHERS + ad_day:TLC +
             as.factor(WEEK),
           data = na.omit(df_vp), 
           model = "within", 
           index = c("DEM"))

stargazer(plm1, plm2,
          se = list(coeftest(plm1, vcov = vcovHC(plm1), type = 'HC1')[ ,2],
                    coeftest(plm2, vcov = vcovHC(plm2), type = 'HC1')[ ,2]),
          omit = c('WEEK', 'AWARDS', 'COMEDY', 'DOCUMENTARY', 'DRAMA_ADVENTURE', 'MOVIE', 'NEWS_POLITICAL', 
                   'SLICE_OF_LIFE', 'SPORTS', 'SUSPENSE_MYSTERY', 'ANE', 'CNN', 'DISC', 'FNEW', 'HIST', 'ID', 
                   'LIFE', 'MSNB', 'NBC', 'OTHERS', 'TLC', 'TV1', 'CBS', 'LMN'),
          omit.stat=c("LL","ser","f"),
          column.labels = c("Voter Preference", "Voter Preference"),
          covariate.labels = c('Lagged Voter Pref.', '1(Day with No Ad)', 
                               'Slant', 'Consistency', 'Attack',
                               '# of Ad Airings',
                               'Avg. Ad Position in Break', 'Avg. Ad Length (1 if > 30 Seconds)', 
                               'Slant X Pre Oct. 1', 'Slant X Post Oct. 1',
                               'Consistency X Pre Oct. 1', 'Consistency X Post Oct. 1',
                               'Attack X Pre Oct. 1', 'Attack X Post Oct. 1'),
          add.lines = list(c("Candidate and Week FEs", 'Yes', 'Yes'),
                           c("Prog. Genre and Network Controlled", 'Yes', 'Yes')),
          type = 'text')
