library (tidyverse)

#define periods to join with climate data
data_long <- data_long %>%
  mutate(
    day=day(VAL_DT_TRF),
    follow_up_month = month(VAL_DT_TRF),
    year = year(VAL_DT_TRF))

data_long_daily <- data_long %>%
  inner_join(period_mapping, 
             by = c("day"="day",
                    "follow_up_month" = "month", 
                    "year" = "year"))

data_long_daily <- data_long_daily %>%
  mutate(NORMALIZED_ZIP = substr(TX_CTR_ZIP, 1, 5))

data_with_index <- data_long_daily %>%  mutate(zipcode = NORMALIZED_ZIP)

data_with_index<- data_with_index%>% arrange(PT_CODE, period)

data_with_ranges <- data_with_index %>%
  arrange(PT_CODE, period) %>%
  group_by(PT_CODE) %>%
  mutate(
    start_period = ifelse(row_number() == 1, period, lag(period) + 1),
    end_period = period 
  ) %>%
  ungroup()

data_with_ranges <- data_with_ranges %>%
  arrange(PT_CODE, period) %>%           # Sort by PT_CODE and period
  group_by(PT_CODE) %>%                  # Group by PT_CODE
  mutate(sub_period = row_number()) %>%  # Create sub_period as row number within each PT_CODE
  ungroup()                              # Ungroup to remove grouping structure

periods<-data_with_ranges %>% dplyr::select(PT_CODE, zipcode, period, start_period, end_period, sub_period)
periods$zipcode<- as.integer(periods$zipcode)

#create heat exposure by period and percentiles
heat_indices_long<- heat_indices_long %>% mutate(heat_index_above_27=as.numeric(heat_index_value>27), heat_index_above_28=as.numeric(heat_index_value>28), heat_index_above_29=as.numeric(heat_index_value>29), 
                                                 heat_index_above_30=as.numeric(heat_index_value>30), heat_index_above_31=as.numeric(heat_index_value>31))
heat_indices_long <- heat_indices_long %>%
  group_by(zipcode) %>%
  mutate(
    heat_index_90th = ifelse(heat_index_value > quantile(heat_index_value, 0.9, na.rm = TRUE), 1, 0),
    heat_index_95th = ifelse(heat_index_value > quantile(heat_index_value, 0.95, na.rm = TRUE), 1, 0),
    heat_index_99th = ifelse(heat_index_value > quantile(heat_index_value, 0.99, na.rm = TRUE), 1, 0)
  ) %>%
  ungroup()

# Create a sequence of periods for each row and expand the dataframe
data_final <- periods %>%
  rowwise() %>%
  mutate(periods = list(seq(start_period, end_period))) %>%
  unnest(periods)
data_final<- data_final %>% inner_join(heat_indices_long, by = c("zipcode" = "zipcode", "periods" = "period"))

HEAT_30 <- data_final %>% filter(sub_period>1) %>%
  group_by(PT_CODE, sub_period) %>%
  summarize(
    HEAT_27 = sum(heat_index_above_27 == 1) / n(),
    HEAT_28 = sum(heat_index_above_28 == 1)/ n(),
    HEAT_29 = sum(heat_index_above_29 == 1)/ n(),
    HEAT_30 = sum(heat_index_above_30 == 1)/ n(),
    HEAT_31 = sum(heat_index_above_31 == 1)/ n(),
    HEAT_90 = sum(heat_index_90th == 1) / n(),
    HEAT_95 = sum(heat_index_95th == 1) / n(),
    HEAT_99 = sum(heat_index_99th == 1) / n(),
    subaccum_HEAT_27 = sum(heat_index_above_27 == 1),
    subaccum_HEAT_28 = sum(heat_index_above_28 == 1),
    subaccum_HEAT_29 = sum(heat_index_above_29 == 1),
    subaccum_HEAT_30 = sum(heat_index_above_30 == 1),
    subaccum_HEAT_31 = sum(heat_index_above_31 == 1),
    subaccum_HEAT_90 = sum(heat_index_90th == 1),
    subaccum_HEAT_95 = sum(heat_index_95th == 1),
    subaccum_HEAT_99 = sum(heat_index_99th == 1),
    accum_heat_index=sum(heat_index_value),
    mean_heat_index=mean(heat_index_value),
    sd_heat_index=sd(heat_index_value),
    median_heat_index=median(heat_index_value),
    iqr_heat_index=IQR(heat_index_value),
    observations=n(),
    .groups = "drop"
  )

rm(data_final)

data_long <- data_with_ranges %>%
  inner_join(HEAT_30, by = c("PT_CODE", "sub_period"))
rm(data_with_ranges)

descriptive_grouped_all <- data_long %>%
  group_by(PT_CODE) %>%
  summarize(
    cr_count = min(creat_count, na.rm = TRUE),
    FOLLOW_UP_UNTIL_CENSORING_YEARS = min(FOLLOW_UP_UNTIL_CENSORING_YEARS, na.rm = TRUE),
    CENSOR_DATE = min(CENSOR_DATE, na.rm = TRUE),
    overall_percent_change_GFR_CKD_EPI = min(overall_percent_change_GFR_CKD_EPI, na.rm = TRUE),
    overall_change_GFR_CKD_EPI = min(overall_change_GFR_CKD_EPI, na.rm = TRUE),
    accum_HEAT_27 = sum(subaccum_HEAT_27, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_28 = sum(subaccum_HEAT_28, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_29 = sum(subaccum_HEAT_29, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_30 = sum(subaccum_HEAT_30, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_31 = sum(subaccum_HEAT_31, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_32 = sum(subaccum_HEAT_32, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_90 = sum(subaccum_HEAT_90, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_95 = sum(subaccum_HEAT_95, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_HEAT_99 = sum(subaccum_HEAT_99, na.rm = TRUE) / sum(observations, na.rm = TRUE),
    accum_heat_index = sum(accum_heat_index, na.rm = TRUE),
    observations = sum(observations, na.rm = TRUE)
  )

data_wide<- merge(descriptive_grouped_all, data_wide, by="PT_CODE")
data_wide <- data_wide %>%
  mutate(zipcode = substr(TX_CTR_ZIP, 1, 5))

#select KTRs with consistent follow-up, only those that have 1 year periods throughout
pt_codes_to_remove <- data_long %>%
  group_by(PT_CODE) %>%
  filter(any(time_diff_from_prev_VAL_DT_TRF < 0.9 | time_diff_from_prev_VAL_DT_TRF > 1.1))

data_consistentFU_final <- data_long %>%
  anti_join(pt_codes_to_remove, by = "PT_CODE")

wide_consistent_FU <- data_consistentFU_final %>% group_by(PT_CODE) %>% summarize(cr_count=min(creat_count), FOLLOW_UP_UNTIL_CENSORING_YEARS=min(FOLLOW_UP_UNTIL_CENSORING_YEARS))

pt_codes<- unique(wide_consistent_FU$PT_CODE)

data_wide_consistent<- data_wide %>%
  filter(PT_CODE %in% pt_codes)

data_wide_consistent$HI_accum_quartile <- cut(data_wide_consistent$HI_accum_daily, 
                                              breaks = quantile(data_wide_consistent$HI_accum_daily, probs = seq(0, 1, 0.25), na.rm = TRUE), 
                                              include.lowest = TRUE, 
                                              labels = c("Q1", "Q2", "Q3", "Q4"))

data_consistentFU_final <- data_consistentFU_final %>%
  group_by(PT_CODE) %>%
  mutate(time_diff_from_first_years = as.numeric(as.Date(VAL_DT_TRF) - min(as.Date(VAL_DT_TRF))) / 365.25) %>%
  ungroup()

data_consistentFU_final <- data_consistentFU_final %>%
  left_join(data_wide_consistent %>% dplyr::select(PT_CODE, TX_YEAR), by = "PT_CODE")
