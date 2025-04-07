library (dplyr)
library (haven)
library(nephro)
library (lubridate)
library(tidyr)
library(mice)

#datasets
KIDPAN_DATA <-  read_dta("KIDPAN_DATA.DTA")
KIDNEY_FOLLOWUP_DATA <- read_dta("KIDNEY_FOLLOWUP_DATA.DTA")


#select only duplicates during follow-up
follow_up<- KIDNEY_FOLLOWUP_DATA %>% filter(duplicated(PT_CODE) | duplicated(PT_CODE, fromLast = TRUE))
rm (KIDNEY_FOLLOWUP_DATA)

#selection criteria: TX date >2010, first-time, adult, kidney-only, no rejection
kidney<- KIDPAN_DATA %>% filter (AGE>=18) %>% filter(TX_DATE >= as.Date('2010-01-01'))
kidney<- kidney %>% filter (PREV_TX_ANY != 'Y') %>% filter(PREV_KI_TX == 'N') %>% filter(MULTIORG != 'Y')  
kidney<- kidney %>% filter (TRTREJ1Y_KI!="Y" & TRTREJ6M_KI!="Y")
rm (KIDPAN_DATA)

#add center codes
Kidney_panc_CTR <- read_dta("Kidney panc CTR.dta")
kidney <- merge(kidney, Kidney_panc_CTR, by="TRR_ID_CODE")
rm(Kidney_panc_CTR)

#select duplicate pt codes in kidney
PT_CODEs_FU <- follow_up$PT_CODE
kidney <- kidney %>%
  filter(PT_CODE %in% PT_CODEs_FU)

#merge
follow_up_with_tx_date <- follow_up %>%
  inner_join(kidney %>% dplyr::select(PT_CODE, TX_DATE), by = "PT_CODE")

#define follow-up times, total and from tx to specific follow-up
follow_up_with_tx_date$INTER_FU_TIME_YEARS <- (as.Date(follow_up_with_tx_date$VAL_DT_TRF)-follow_up_with_tx_date$TX_DATE)/365.25
follow_up_with_tx_date$FU_TIME_YEARS <- (follow_up_with_tx_date$PX_STAT_DATE-follow_up_with_tx_date$TX_DATE)/365.25

# filter follow-up >9 months and time from first follow-up >9 months
follow_up_with_tx_date <- follow_up_with_tx_date %>% filter (FU_TIME_YEARS>="0.75")
follow_up_with_tx_date <- follow_up_with_tx_date %>% filter (INTER_FU_TIME_YEARS>="0.75")
follow_up_with_tx_date <- follow_up_with_tx_date %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))

#>2 creatinines
follow_up_creatinine <- follow_up_with_tx_date %>%
  group_by(PT_CODE) %>%
  filter(sum(!is.na(CREAT)) > 1) %>%  # Count non-NA CREAT values
  ungroup()
follow_up_creatinine<- follow_up_creatinine %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))

creatinine_counts <- follow_up_creatinine %>%
  group_by(PT_CODE) %>%
  summarise(num_creatinine_measurements = sum(!is.na(CREAT))) 
summary(creatinine_counts$num_creatinine_measurements)

PT_CODE_final_creatinine <- follow_up_creatinine$PT_CODE
data_wide <- kidney %>%
  filter(PT_CODE %in% PT_CODE_final_creatinine)

#merge
follow_up_final <- follow_up_creatinine %>%
  inner_join(data_wide, by = "PT_CODE")

#create censoring variable
follow_up_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  mutate(max_PX_STAT_DATE = max(PX_STAT_DATE.y, na.rm = TRUE)) %>%  # Get the maximum PX_STAT_DATE.y per PT_CODE
  ungroup() %>%  # Ungroup to go back to the main data structure
  mutate(CENSOR_DATE = case_when(
    !is.na(FAILDATE_KI) & FAILDATE_KI > as.Date("2010-01-01") ~ FAILDATE_KI,
    !is.na(RESUM_MAINT_DIAL_DT) & (is.na(FAILDATE_KI) | RESUM_MAINT_DIAL_DT < FAILDATE_KI) & RESUM_MAINT_DIAL_DT > as.Date("2010-01-01") ~ RESUM_MAINT_DIAL_DT,
    !is.na(RETXDATE_KI.x) & RETXDATE_KI.x > as.Date("2010-01-01") ~ RETXDATE_KI.x,
    !is.na(RETXDATE_KI.y) & RETXDATE_KI.y > as.Date("2010-01-01") ~ RETXDATE_KI.y,
    !is.na(DEATH_DATE) & DEATH_DATE > as.Date("2010-01-01") ~ DEATH_DATE,
    !is.na(max_PX_STAT_DATE) ~ max_PX_STAT_DATE,  # Use the maximum PX_STAT_DATE if none of the above is available
    TRUE ~ NA_Date_  # Default to NA if none of the above is available
  ))
follow_up_final$FOLLOW_UP_UNTIL_CENSORING_YEARS<- (as.Date(follow_up_final$CENSOR_DATE)-as.Date(follow_up_final$TX_DATE.y))/365.25

#filter if censored <9 months post-tx and re-select
follow_up_final <- follow_up_final %>% filter (FOLLOW_UP_UNTIL_CENSORING_YEARS>="0.75")
follow_up_final <- follow_up_final %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))
follow_up_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  filter(sum(!is.na(CREAT)) > 1) %>%  # Count non-NA CREAT values
  ungroup()
follow_up_final<- follow_up_final %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))
censor_time<- follow_up_final %>% dplyr::select(PT_CODE, TX_DATE.x, CENSOR_DATE, VAL_DT_TRF, FOLLOW_UP_UNTIL_CENSORING_YEARS) %>% filter(CENSOR_DATE>VAL_DT_TRF) %>% mutate(FOLLOW_UP_UNTIL_CENSORING_YEARS=as.numeric(FOLLOW_UP_UNTIL_CENSORING_YEARS))
follow_up_final <- follow_up_final %>% filter(CENSOR_DATE>VAL_DT_TRF)
follow_up_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  filter(sum(!is.na(CREAT)) > 1) %>%  # Count non-NA CREAT values
  ungroup()
follow_up_final<- follow_up_final %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))
follow_up_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  mutate(creat_count = sum(!is.na(CREAT))) %>%  # Count non-missing PX_STAT_DATE.y values
  ungroup()
follow_up_final <- follow_up_final %>% filter (!is.na(CREAT))
follow_up_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  filter(sum(!is.na(CREAT)) > 1) %>%  # Count non-NA CREAT values
  ungroup()
follow_up_final<- follow_up_final %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))
creatinine_counts_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  summarise(num_creatinine_measurements = sum(!is.na(CREAT))) 
summary(creatinine_counts$num_creatinine_measurements)
follow_up_final$FOLLOW_UP_UNTIL_CENSORING_YEARS <- as.numeric(follow_up_final$FOLLOW_UP_UNTIL_CENSORING_YEARS)
follow_up_final$INTER_FU_TIME_YEARS <- as.numeric(follow_up_final$INTER_FU_TIME_YEARS)

follow_up_final<- follow_up_final %>% group_by(PT_CODE) %>%
  mutate(first_INTER_FU_TIME_YEARS = first(INTER_FU_TIME_YEARS)) %>%
  ungroup()

#first measurement between >=0.75 and <=1.25 years post-tx
follow_up_final <- follow_up_final %>% filter (first_INTER_FU_TIME_YEARS<=1.25)
follow_up_final <- follow_up_final %>%
  group_by(PT_CODE) %>%
  filter(sum(!is.na(CREAT)) > 1) %>%  # Count non-NA CREAT values
  ungroup()
follow_up_final<- follow_up_final %>% filter(duplicated(PT_CODE)| duplicated(PT_CODE, fromLast = TRUE))

follow_up_final<- follow_up_final %>% filter(VAL_DT_TRF<CENSOR_DATE)

#final selection
data_long <- follow_up_final
rm(follow_up_final, follow_up_creatinine, follow_up_with_tx_date, follow_up)

#create time from previous measurement variable
data_long <- data_long %>%
  group_by(PT_CODE) %>%
  arrange(PT_CODE, VAL_DT_TRF) %>%  # Sort by PT_CODE and VAL_DT_TRF
  mutate(time_diff_from_prev_VAL_DT_TRF = as.numeric(difftime(VAL_DT_TRF, lag(VAL_DT_TRF), units = "days")) / 365.25) %>%  # Calculate the difference between consecutive VAL_DT_TRF in years
  ungroup()

data_long <- data_long %>%
  group_by(PT_CODE) %>%
  mutate(time_diff_from_first_years = as.numeric(as.Date(VAL_DT_TRF) - min(as.Date(VAL_DT_TRF))) / 365.25) %>%
  ungroup()

#eGFR calculation
data_long$GENDER_MALE1_FEMALE0<- NA
data_long$GENDER_MALE1_FEMALE0[data_long$GENDER=="M"]<- 1
data_long$GENDER_MALE1_FEMALE0[data_long$GENDER=="F"]<- 0
data_long$GENDER_MALE1_FEMALE0<- as.numeric(data_long$GENDER_MALE1_FEMALE0)

data_long <- data_long %>%
  group_by(PT_CODE) %>%
  arrange(VAL_DT_TRF) %>%
  mutate(
    adjusted_age = if_else(is.na(time_diff_from_prev_VAL_DT_TRF), AGE, AGE + time_diff_from_prev_VAL_DT_TRF)) %>%
  ungroup()

data_long$GFR_CKD_EPI<- CKDEpi_RF.creat(data_long$CREAT, data_long$GENDER_MALE1_FEMALE0, data_long$adjusted_age)

#GFR variables
data_long <- data_long%>% group_by(PT_CODE) %>% mutate(change_GFR_CKD_EPI = as.numeric(GFR_CKD_EPI - first(GFR_CKD_EPI))) %>% ungroup()
data_long <- data_long%>% group_by(PT_CODE) %>% mutate(overall_change_GFR_CKD_EPI = as.numeric(last(GFR_CKD_EPI) - first(GFR_CKD_EPI))) %>% ungroup()
data_long <- data_long%>% group_by(PT_CODE) %>% mutate(percent_change_GFR_CKD_EPI = (as.numeric(GFR_CKD_EPI - first(GFR_CKD_EPI))/first(GFR_CKD_EPI))*100) %>% ungroup()
data_long <- data_long%>% group_by(PT_CODE) %>% mutate(overall_percent_change_GFR_CKD_EPI = as.numeric(last(GFR_CKD_EPI) - first(GFR_CKD_EPI))/first(GFR_CKD_EPI)*100) %>% ungroup()
data_long <- data_long%>% group_by(PT_CODE) %>% mutate(GFR_CKD_EPI_percent = (GFR_CKD_EPI/first(GFR_CKD_EPI))*100)%>% ungroup()

data_long <- data_long %>%
  group_by(PT_CODE) %>%
  mutate(creat_count = sum(!is.na(CREAT))) %>%
  ungroup()
