#subgroup analysis/Marginal effects: 
library(emmeans)
library(nlme)
library(tidyverse)
library(splines)

#tacrolimus
model_tac_nlme <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*TAC +
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(model_tac_nlme)
tac_effect<-emtrends(model_tac_nlme, ~TAC, var="HEAT_30",at=c("0", "1"))
summary(tac_effect)
rm(model_tac_nlme)

#DCD
model_dcd_nlme <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*DCD +
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + TAC + DECEASED + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(model_dcd_nlme)

dcd_effect<-emtrends(model_dcd_nlme, ~DCD, var="HEAT_30",at=c("0", "1"))
summary(dcd_effect)
rm(model_dcd_nlme)

#deceased donor
model_deceased_nlme <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*DECEASED  +
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + TAC + DCD + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(model_deceased_nlme)

deceased_effect<-emtrends(model_deceased_nlme, ~DECEASED, var="HEAT_30",at=c("0", "1"))
summary(deceased_effect)
rm(model_deceased_nlme)

#KDPI
data_consistentFU_final$KDPI85<- NA
data_consistentFU_final$KDPI85[data_consistentFU_final$KDPI_DON_TY_imputed>0.85]<- 1
data_consistentFU_final$KDPI85[data_consistentFU_final$KDPI_DON_TY_imputed<=0.85]<- 0
data_consistentFU_final$KDPI85<- as.factor(data_consistentFU_final$KDPI85)
kdpi_subgroup <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*KDPI85 +
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + DONOR+ TAC + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(kdpi_subgroup)
kdpi_effect<-emtrends(kdpi_subgroup, ~KDPI85, var="HEAT_30",at=c("0", "1"))
summary(kdpi_effect)
rm(kdpi_subgroup)

#steroid maintenance
model_steroid_nlme <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*steroid + DONOR+
    GENDER_MALE1_FEMALE0 + AA +  depleting + scale(AGE) + TAC + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(model_steroid_nlme)

steroid_effect<-emtrends(model_steroid_nlme, ~steroid, var="HEAT_30",at=c("1", "2"))
summary(steroid_effect)
#p=0.1324
rm(model_steroid_nlme)

#diabetes
model_dm_nlme <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*DIABETES_imputed+ steroid + 
    DONOR+ GENDER_MALE1_FEMALE0 + AA +  depleting + scale(AGE) + TAC  + 
    DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(model_dm_nlme)

dm_effect<-emtrends(model_dm_nlme, ~DIABETES_imputed, var="HEAT_30",at=c("0", "1"))
summary(dm_effect)
rm(model_dm_nlme)

#DGF
dgf_subgroup <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*DGF_imputed +
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + TAC + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(dgf_subgroup)
dgf_effect<-emtrends(dgf_subgroup, ~DGF_imputed, var="HEAT_30",at=c("0", "1"))
summary(dgf_effect)
rm(dgf_subgroup)

#income level
#<29,999, 30-59,999, 60-89999, 90-119,999, 120-149,999, >150,000, median 75,149
zip_codes<- reverse_zipcode(zip_codes$TX_CTR_ZIP)
zip_codes<- zip_codes %>% mutate(country="US")
zip_codes<- zip_codes %>% dplyr::select(zipcode, major_city, county, state, country)
state_lookup <- setNames(state.name, state.abb)
zip_codes$state_full <- state_lookup[zip_codes$state]
zip_codes$county_state <- paste(zip_codes$county, zip_codes$state_full, sep = ", ")
zip_codes2<- zip_codes %>% dplyr::select(zipcode, county_state)

#manually add zipcodes not found in ZCTA (not shared)

#read census data
county_incomes <- read.csv("County_incomes.csv")
county_incomes$median_income<- as.numeric(county_incomes$median_income)
county_incomes<- drop_na(county_incomes)
county_incomes$above_median<- NA
county_incomes$above_median[county_incomes$median_income>75149]<-1
county_incomes$above_median[county_incomes$median_income<=75149]<-0
counties<- data_consistentFU_final$county_state
county_incomes_final<- county_incomes %>% filter (county_state %in% counties)

data_consistentFU_final<- merge(data_consistentFU_final, county_incomes_final, by="county_state")
data_consistentFU_final$above_median <-as.factor(data_consistentFU_final$above_median)

model_income_nlme <- lme(
  fixed = GFR_CKD_EPI_percent ~ time_diff_from_first_years + HEAT_30*above_median + DONOR+
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + TAC + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
summary(model_income_nlme)
income_effect<-emtrends(model_income_nlme, ~above_median, var="HEAT_30",at=c("0", "1"))
summary(income_effect)
rm(model_income_nlme)

#baseline GFR subgroups
data_consistentFU_final <- data_consistentFU_final %>%
  group_by(PT_CODE) %>%
  mutate(baseline_GFR = first(GFR_CKD_EPI, order_by = VAL_DT_TRF)) %>%
  ungroup()
data_consistentFU_final$BASELINE_GFR_CAT<-NA
data_consistentFU_final$BASELINE_GFR_CAT[data_consistentFU_final$baseline_GFR>=60]<-0
data_consistentFU_final$BASELINE_GFR_CAT[data_consistentFU_final$baseline_GFR>=45&data_consistentFU_final$baseline_GFR<60]<-1
data_consistentFU_final$BASELINE_GFR_CAT[data_consistentFU_final$baseline_GFR>=30&data_consistentFU_final$baseline_GFR<45]<-2
data_consistentFU_final$BASELINE_GFR_CAT[data_consistentFU_final$baseline_GFR<30]<-3
data_consistentFU_final$BASELINE_GFR_CAT<- as.numeric(data_consistentFU_final$BASELINE_GFR_CAT)

model_baseline_GFR_nlme <- lme(
  fixed = log(GFR_CKD_EPI) ~ time_diff_from_first_years + HEAT_30*BASELINE_GFR_CAT + DONOR+
    GENDER_MALE1_FEMALE0 + AA + steroid + depleting + scale(AGE) + TAC + 
    DIABETES_imputed + DIABETES_DONOR_imputed + AGE_DON +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final,
  method = "REML"
)
model_baseline_GFR_nlme<- summary(model_baseline_GFR_nlme)
baseline_GFR_effect<-emtrends(model_baseline_GFR_nlme, ~BASELINE_GFR_CAT, var="HEAT_30",at=c("0", "1", "2", "3"), rg.limit =16384)
summary(baseline_GFR_effect)
rm(model_baseline_GFR_nlme)
