library(nloptr)
library(lme4)
library(nlme)
library(dplyr)
library(tidyr)
library(splines)
library(broom.mixed)
library(ggplot2)
library(survival)
library(gnm)
library(data.table)
library(scales)
library(foreach)
library(doParallel)

#mixed-effects models
model_5_1_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_30 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_1_nlme, level = 0.95,
          which = "fixed")
summary(model_5_1_nlme)

model_5_2_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_27 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_2_nlme, level = 0.95,
          which = "fixed")
summary(model_5_2_nlme)

model_5_3_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_28 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting + DONOR+ 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_3_nlme, level = 0.95,
          which = "fixed")
summary(model_5_3_nlme)

model_5_4_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_29 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_4_nlme, level = 0.95,
          which = "fixed")
summary(model_5_4_nlme)

model_5_5_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_31 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_5_nlme, level = 0.95,
          which = "fixed")
summary(model_5_5_nlme)

model_5_6_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_90 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting +DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_6_nlme, level = 0.95,
          which = "fixed")
summary(model_5_6_nlme)

model_5_7_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_95 +
    GENDER_MALE1_FEMALE0.x + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting + DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_7_nlme, level = 0.95,
          which = "fixed")
summary(model_5_7_nlme)

model_5_8_nlme <- lme(
  fixed = percent_change_GFR_CKD_EPI ~ time_diff_from_first_years + HEAT_99 +
    GENDER_MALE1_FEMALE0 + scale(AGE) + AA + scale(AGE_DON)  + TAC + steroid + depleting +DONOR + 
    DIABETES_imputed + DIABETES_DONOR_imputed  +
    scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
    scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
    scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed + ns(follow_up_month, knots = c(3, 6, 9)),
  random = ~ 1 + time_diff_from_first_years | TX_CTR_CODE/PT_CODE,
  data = data_consistentFU_final2,
  method = "REML"
)

intervals(model_5_8_nlme, level = 0.95,
          which = "fixed")
summary(model_5_8_nlme)

#case time series
data_CTS<- data_consistentFU_final %>% select (percent_change_GFR_CKD_EPI, time_diff_from_first_years, SEASON, HEAT_27, HEAT_28, HEAT_29, HEAT_30, HEAT_31, HEAT_32, HEAT_90, HEAT_95, HEAT_99, TX_CTR_CODE, PT_CODE, follow_up_month)
data_CTS$TX_CTR_CODE<- as.factor(data_CTS$TX_CTR_CODE)
data_CTS$PT_CODE<- as.factor(data_CTS$PT_CODE)
data_CTS <- na.omit(data_CTS)
data_CTS$follow_up_month<- as.numeric(data_CTS$follow_up_month)

cts30 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_30 +ns(follow_up_month, knots = c(3, 6, 9)), eliminate=TX_CTR_CODE:PT_CODE,
            data=data_CTS, family=gaussian)
cts_ci<- confint(cts30, parm="HEAT_30")
cts30<- summary(cts30)

cts27 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_27 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci27<- confint(cts27, parm="HEAT_27")
cts27<- summary(cts27)

cts28 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_28 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci28<- confint(cts28, parm="HEAT_28")
cts28<- summary(cts28)

cts29 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_29 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci29<- confint(cts29, parm="HEAT_29")
cts29<-summary(cts29)

cts31 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_31 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci31<- confint(cts31, parm="HEAT_31")
cts31<-summary(cts31)

cts90 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_90 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci90<-confint(cts90, parm="HEAT_90")
cts90 <-summary(cts90)

cts95 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_95 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci95<-confint(cts95, parm="HEAT_95")
cts95 <-summary(cts95)

cts99 <-gnm(percent_change_GFR_CKD_EPI~time_diff_from_first_years + HEAT_99 + ns(follow_up_month, knots = c(3, 6, 9)), eliminate=factor(TX_CTR_CODE):factor(PT_CODE),
            data=data_CTS, family=gaussian)
cts_ci99<-confint(cts99, parm="HEAT_99")
cts99 <-summary(cts99)

#creatinine doubling log-log models

#outcome definition
data_consistentFU_final <- data_consistentFU_final %>%
  group_by(PT_CODE) %>%
  mutate(baseline_creatinine = first(CREAT, order_by = VAL_DT_TRF)) %>%  # Define baseline creatinine as the first measure for each PT_CODE
  mutate(CR_STATUS = case_when(
    CREAT >= 2 * baseline_creatinine ~ 1,
    TRUE ~ 0
  )) %>%
  ungroup()

data_log_log<- data_consistentFU_final %>% filter(sub_period<=10) %>% group_by (PT_CODE) %>% mutate(CR_cumsum = cumsum(CR_STATUS == 1)) %>% filter(CR_cumsum == 0 | (CR_STATUS == 1 & lag(CR_cumsum, default = 0) == 0)) %>% ungroup()


Model_Multi30_creat <- glmer(creatinine_status ~ as.factor(sub_period) + HEAT_30 + GENDER + AA + TAC + steroid + depleting + scale(AGE) + as.factor(DONOR)+ 
                               DIABETES_imputed + DIABETES_DONOR_imputed + scale(AGE_DON) +
                               scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
                               scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
                               scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed+ ns(follow_up_month, knots=c(3,6,9)) + (1|TX_CTR_CODE:PT_CODE), family = binomial("cloglog"),
                             data = data_log_log, control = glmerControl(optimizer = "bobyqa"), nAGQ=0)
HR_30_creat <- exp(fixef(Model_Multi30_creat))
CI_30_creat<- exp(confint(Model_Multi30_creat, method = "Wald"))

Model_Multi27_creat <- glmer(creatinine_status ~ as.factor(sub_period) + HEAT_27 + GENDER + AA + TAC + steroid + depleting + scale(AGE) + as.factor(DONOR)+ 
                               DIABETES_imputed + DIABETES_DONOR_imputed + scale(AGE_DON) +
                               scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
                               scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
                               scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed+ ns(follow_up_month, knots=c(3,6,9)) + (1|TX_CTR_CODE:PT_CODE), family = binomial("cloglog"),
                             data = data_log_log, control = glmerControl(optimizer = "bobyqa"), nAGQ=0)
HR_27_creat <- exp(fixef(Model_Multi27_creat))
CI_27_creat<- exp(confint(Model_Multi27_creat, method = "Wald"))


Model_Multi28_creat <- glmer(creatinine_status ~ as.factor(sub_period) + HEAT_28 + GENDER + AA + TAC + steroid + depleting + scale(AGE) + as.factor(DONOR)+ 
                               DIABETES_imputed + DIABETES_DONOR_imputed + scale(AGE_DON) +
                               scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
                               scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
                               scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed+ ns(follow_up_month, knots=c(3,6,9)) + (1|TX_CTR_CODE:PT_CODE), family = binomial("cloglog"),
                             data = data_log_log, control = glmerControl(optimizer = "bobyqa"), nAGQ=0)
HR_28_creat <- exp(fixef(Model_Multi28_creat))
CI_28_creat<- exp(confint(Model_Multi28_creat, method = "Wald"))

Model_Multi29_creat <-  glmer(creatinine_status ~ as.factor(sub_period) + HEAT_29 + GENDER + AA + TAC + steroid + depleting + scale(AGE) + as.factor(DONOR)+ 
                                DIABETES_imputed + DIABETES_DONOR_imputed + scale(AGE_DON) +
                                scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
                                scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
                                scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed+ ns(follow_up_month, knots=c(3,6,9)) + (1|TX_CTR_CODE:PT_CODE), family = binomial("cloglog"),
                              data = data_log_log, control = glmerControl(optimizer = "bobyqa"), nAGQ=0)
HR_29_creat <- exp(fixef(Model_Multi29_creat))
CI_29_creat<- exp(confint(Model_Multi29_creat, method = "Wald"))

Model_Multi31_creat <- glmer(creatinine_status ~ as.factor(sub_period) + HEAT_31 + GENDER + AA + TAC + steroid + depleting + scale(AGE) + as.factor(DONOR)+ 
                               DIABETES_imputed + DIABETES_DONOR_imputed + scale(AGE_DON) +
                               scale(END_BMI_CALC_imputed) + scale(HLAMIS_imputed) + scale(BMI_DON_CALC_imputed) + 
                               scale(DIALYSIS_TIME_imputed) + scale(END_CPRA_imputed) + scale(COLD_ISCH_KI_imputed) + 
                               scale(KDPI_DON_TY_imputed) + PAYER + WORK_imputed + EDLEVEL_imputed+ ns(follow_up_month, knots=c(3,6,9)) + (1|TX_CTR_CODE:PT_CODE), family = binomial("cloglog"),
                             data = data_log_log, control = glmerControl(optimizer = "bobyqa"), nAGQ=0)
HR_31_creat <- exp(fixef(Model_Multi31_creat))
CI_31_creat<- exp(confint(Model_Multi31_creat, method = "Wald"))


#MAKE analysis

#define MAKE
data_consistentFU_final <- data_consistentFU_final %>%
  group_by(PT_CODE) %>%
  mutate(baseline_creatinine = first(CREAT, order_by = VAL_DT_TRF)) %>%  # Define baseline creatinine as the first measure for each PT_CODE
  mutate(creatinine_status = case_when(
    CREAT >= 2 * baseline_creatinine ~ 1,
    TRUE ~ 0
  )) %>%
  ungroup()

data_consistentFU_final <- data_consistentFU_final %>%
  group_by(PT_CODE) %>% mutate(CREAT_DOUBLING_DATE = if_else(creatinine_status == 1, VAL_DT_TRF, as.Date(NA))) %>% ungroup()

data_consistentFU_final <- data_consistentFU_final %>%
  group_by(PT_CODE) %>%
  mutate(MAKE_CENSOR_DATE = case_when(
    !is.na(CREAT_DOUBLING_DATE) & CREAT_DOUBLING_DATE < CENSOR_DATE ~ CREAT_DOUBLING_DATE,
    TRUE ~ CENSOR_DATE)) %>% ungroup()
data_consistentFU_final$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS<- (as.Date(data_consistentFU_final$MAKE_CENSOR_DATE)-as.Date(data_consistentFU_final$TX_DATE.x))/365.25

make_censoring<- data_consistentFU_final %>% dplyr::select(PT_CODE, creatinine_status, MAKE_CENSOR_DATE, FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS)

make_censoring<- make_censoring %>% group_by(PT_CODE) %>% summarize(CREATININE_STATUS=max(creatinine_status), MAKE_CENSOR_DATE=min(MAKE_CENSOR_DATE), FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS=min(FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS)) 

data_wide_consistent <- merge(data_wide_consistent, make_censoring, by="PT_CODE")

data_wide_consistent$MAKE<-NA
data_wide_consistent$MAKE<-0
data_wide_consistent$MAKE[data_wide_consistent$GSTATUS_KI==1|data_wide_consistent$PSTATUS==1|data_wide_consistent$DOUBLING_CR.y==1]<-1


data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_10YEARS<-as.numeric(data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS)
data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_10YEARS[data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS>=10]<-10

data_wide_consistent$MAKE_10YEARS<-NA
data_wide_consistent$MAKE_10YEARS[data_wide_consistent$MAKE==0]<- 0
data_wide_consistent$MAKE_10YEARS[data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS>=10 & data_wide_consistent$MAKE==1]<-0
data_wide_consistent$MAKE_10YEARS[data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS<10 & data_wide_consistent$MAKE==1]<-1
data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_10YEARS<-as.numeric(data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS)
data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_10YEARS[data_wide_consistent$FOLLOW_UP_UNTIL_MAKE_CENSORING_YEARS>=10]<-10


#landmark design
cox_data<- data_wide_consistent %>% filter(
  !(CR_10YEARS == 1 & FOLLOW_UP_UNTIL_SCR_10YEARS <= 2),
  !(MAKE_10YEARS == 1 & FOLLOW_UP_UNTIL_MAKE_CENSORING_10YEARS <= 2)
)

cox <- coxph(Surv(as.numeric(FOLLOW_UP_UNTIL_MAKE_CENSORING_10YEARS), as.numeric(MAKE_10YEARS)) ~ 
               overall_HI_quartile + 
               GENDER_MALE1_FEMALE0 + AGE +AA + AGE_DON+
               TAC.y +
               steroid.y + depleting.y + DONOR  + 
               + DIABETES_imputed.y + 
               + DIABETES_DONOR_imputed.y  + 
               scale(END_BMI_CALC_imputed.y) + scale(BMI_DON_CALC_imputed.y) + HLAMIS_imputed.y  +
               scale(DIALYSIS_TIME_imputed.y) + scale(END_CPRA_imputed.y) + scale(COLD_ISCH_KI_imputed.y) + scale(KDPI_DON_TY_imputed.y) + PAYER + WORK_imputed.y + EDLEVEL_imputed,
             cluster = TX_CTR_CODE, 
             data = cox_data)