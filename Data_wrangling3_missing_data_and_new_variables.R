library (dplyr)
library (haven)
library(tidyr)
library(nlme)
library(mice)

#IS data
KIDPAN_IMMUNO_DISCHARGE_DATA <- read_dta("~/UNOS 2024/SAS Export to STATA 202406/Kidney_ Pancreas_ Kidney-Pancreas/Immunosuppression/KIDPAN_IMMUNO_DISCHARGE_DATA.DTA")
kidney_IS <- KIDPAN_IMMUNO_DISCHARGE_DATA %>% group_by(TRR_ID_CODE) %>% summarise( MMF = sum((!is.na(GENERICMMF_MAINT) & GENERICMMF_MAINT == "1") | (!is.na(GENERICMYFORTIC_MAINT) & GENERICMYFORTIC_MAINT == "1") | (!is.na(CELLCEPT_MAINT) & CELLCEPT_MAINT == "1")),
                                                                                   TAC = sum((!is.na(GENERICTACROLIMUS_MAINT) & GENERICTACROLIMUS_MAINT == "1") | 
                                                                                               (!is.na(ENVARSUSXR_MAINT) & ENVARSUSXR_MAINT == "1") | 
                                                                                               (!is.na(PROGRAF_MAINT) & PROGRAF_MAINT == "1") | 
                                                                                               (!is.na(ASTAGRAFXL_MAINT) & ASTAGRAFXL_MAINT == "1")),
                                                                                   depleting = sum((!is.na(OKT3_IND) & OKT3_IND == "1") | 
                                                                                                     (!is.na(OKT4_IND) & OKT4_IND == "1") | 
                                                                                                     (!is.na(THYMOGLOBULIN_IND) & THYMOGLOBULIN_IND == "1") | 
                                                                                                     (!is.na(CAMPATH_IND) & CAMPATH_IND == "1") | 
                                                                                                     (!is.na(ATGAM_IND) & ATGAM_IND == "1")),
                                                                                   non_depleting = sum((!is.na(SIMULECT_IND) & SIMULECT_IND == "1") | 
                                                                                                         (!is.na(ZENAPAX_IND) & ZENAPAX_IND == "1") | 
                                                                                                         (!is.na(DAB486IL2_IND) & DAB486IL2_IND == "1")),
                                                                                   steroid = sum((!is.na(STEROIDS_MAINT) & STEROIDS_MAINT == "1"))
)

kidney_IS$MMF[kidney_IS$MMF == "2"] <- "1" 
kidney_IS$TAC[kidney_IS$TAC == "2"] <- "1" 
kidney_IS$TAC[kidney_IS$depleting == "2"] <- "1" 
kidney_IS$TAC[kidney_IS$non_depleting == "2"] <- "1"
rm(KIDPAN_IMMUNO_DISCHARGE_DATA)

data_wide_consistent <- merge(data_wide_consistent, kidney_IS, by = "TRR_ID_CODE", all.x = TRUE)
rm(KIDNEY_IS)
#create new variables
data_wide_consistent$DIALYSIS <- NA 
data_wide_consistent$DIALYSIS[data_wide_consistent$DIAL_TRR == "N" | data_wide_consistent$DIAL_TRR== "U"] <- 0 
data_wide_consistent$DIALYSIS[data_wide_consistent$DIAL_TRR == "Y"] <- 1 

data_wide_consistent$DIALYSIS_TIME <- as.numeric(difftime(data_wide_consistent$TX_DATE, data_wide_consistent$DIALYSIS_DATE, units = "days")) 
data_wide_consistent$DIALYSIS_TIME[data_wide_consistent$DIALYSIS == 0] <- 0 


data_wide_consistent$AA <- 0 
data_wide_consistent$AA[data_wide_consistent$ETHCAT == 2] <- 1

data_wide_consistent$DIABETES <- NA 
data_wide_consistent$DIABETES[data_wide_consistent$DIAB == 998] <- NA 
data_wide_consistent$DIABETES[data_wide_consistent$DIAB == 1] <- 0 
data_wide_consistent$DIABETES[data_wide_consistent$DIAB == 2] <- 1 
data_wide_consistent$DIABETES[data_wide_consistent$DIAB == 3] <- 1 
data_wide_consistent$DIABETES[data_wide_consistent$DIAB == 4] <- 1 
data_wide_consistent$DIABETES[data_wide_consistent$DIAB == 5] <- 1

data_wide_consistent$DGF[data_wide_consistent$FIRST_WK_DIAL=="Y"] <-1 
data_wide_consistent$DGF[data_wide_consistent$FIRST_WK_DIAL=="N"] <-0 

data_wide_consistent$TX_YEAR<- year(data_wide_consistent$TX_DATE)

data_wide_consistent$GENDER_MALE1_FEMALE0<- NA
data_wide_consistent$GENDER_MALE1_FEMALE0[data_wide_consistent$GENDER=="M"]<- 1
data_wide_consistent$GENDER_MALE1_FEMALE0[data_wide_consistent$GENDER=="F"]<- 0
data_wide_consistent$GENDER_MALE1_FEMALE0<- as.numeric(data_wide_consistent$GENDER_MALE1_FEMALE0)

data_wide_consistent$DIABETES_DONOR <- NA 
data_wide_consistent$DIABETES_DONOR[data_wide_consistent$DIABETES_DON == "U"] <- NA 
data_wide_consistent$DIABETES_DONOR[data_wide_consistent$DIABETES_DON == "N"] <- 0 
data_wide_consistent$DIABETES_DONOR[data_wide_consistent$DIABETES_DON == "Y"] <- 1 

data_wide_consistent$END_CPRA[is.na(data_wide_consistent$END_CPRA)] <- data_wide_consistent$INIT_CPRA[is.na(data_wide_consistent$END_CPRA)]
data_wide_consistent$END_CPRA[is.na(data_wide_consistent$END_CPRA)] <- data_wide_consistent$CURRENT_PRA[is.na(data_wide_consistent$END_CPRA)]

data_wide_consistent$DCD <- 0
data_wide_consistent$DCD[data_wide_consistent$NON_HRT_DON == "Y"] <- 1

data_wide_consistent$DECEASED<-NA
data_wide_consistent$DECEASED[data_wide_consistent$DON_TY=="L"]<-0
data_wide_consistent$DECEASED[data_wide_consistent$DON_TY!="L"]<-1

data_wide_consistent$DONOR[data_wide_consistent$DECEASED=="0"]<-0
data_wide_consistent$DONOR[data_wide_consistent$DECEASED=="1" & data_wide_consistent$DCD=="0"]<-1
data_wide_consistent$DONOR[data_wide_consistent$DCD=="1"]<-2

data_wide_consistent$KDPI_DON_TY<- data_wide_consistent$KDPI
data_wide_consistent$KDPI_DON_TY[is.na(data_wide_consistent$KDPI) & data_wide_consistent$DECEASED==0]<- 0

data_wide_consistent$TX_YEAR<- as.numeric(data_wide_consistent$TX_YEAR)

data_wide_consistent$PAYER<-NA
data_wide_consistent$PAYER[data_wide_consistent$PRI_PAYMENT_TRR_KI>=2 & data_wide_consistent$PRI_PAYMENT_TRR_KI<8]<- 0
data_wide_consistent$PAYER[data_wide_consistent$PRI_PAYMENT_TRR_KI>=12 & data_wide_consistent$PRI_PAYMENT_TRR_KI<=14]<- 0
data_wide_consistent$PAYER[data_wide_consistent$PRI_PAYMENT_TRR_KI==1|data_wide_consistent$PRI_PAYMENT_TRR_KI==8]<- 1
data_wide_consistent$PAYER[data_wide_consistent$PRI_PAYMENT_TRR_KI>=9&data_wide_consistent$PRI_PAYMENT_TRR_KI<=11]<- 1
data_wide_consistent$PAYER<- as.factor(data_wide_consistent$PAYER)

data_wide_consistent$EDLEVEL<-NA
data_wide_consistent$EDLEVEL[data_wide_consistent$EDUCATION==996]<-NA
data_wide_consistent$EDLEVEL[data_wide_consistent$EDUCATION==998]<-NA
data_wide_consistent$EDLEVEL[data_wide_consistent$EDUCATION<=3]<-0
data_wide_consistent$EDLEVEL[data_wide_consistent$EDUCATION>3&data_wide_consistent$EDUCATION<=6]<-1

data_wide_consistent$WORK<- NA
data_wide_consistent$WORK[data_wide_consistent$WORK_INCOME_TCR=="Y"]<-1
data_wide_consistent$WORK[data_wide_consistent$WORK_INCOME_TCR=="N"]<-0
data_wide_consistent$WORK[data_wide_consistent$WORK_INCOME_TCR=="U"]<-NA

categorical <- c("DIABETES", "DIALYSIS", "DGF", "steroid", "depleting", "TAC", "MMF", "DCD", "DECEASED", "DIABETES_DONOR", "DONOR", "PAYER", "EDLEVEL", "WORK")

for (variable in categorical) {
  data_wide_consistent[[variable]] <- as.factor(data_wide_consistent[[variable]])
}

continuous <- c("AGE_DON", "KDPI", "AGE",  "HLAMIS", "END_BMI_CALC", "BMI_DON_CALC", "DIALYSIS_TIME", "END_CPRA", "COLD_ISCH_KI", "KDPI_DON_TY")

for (variable in continuous) {
  data_wide_consistent[[variable]] <- as.numeric(data_wide_consistent[[variable]])}


#missing data (multiple imputation)
data_wide_selected<- data_wide_consistent %>% dplyr::select ("PT_CODE", "DONOR", "HLAMIS", "TX_CTR_CODE", "DIABETES", "DIALYSIS", "DGF",
                                                             "steroid", "depleting", "TAC", "MMF", "DIABETES_DONOR", "AGE_DON", "AGE", "END_BMI_CALC", "BMI_DON_CALC",
                                                             "DIALYSIS_TIME", "END_CPRA", "COLD_ISCH_KI", "KDPI_DON_TY", "WORK", "EDLEVEL")
predictorMatrix <- make.predictorMatrix(data_wide_selected)
predictorMatrix[,] <- 1  # Use all variables as predictors
diag(predictorMatrix) <- 0  # Exclude self-prediction
imputed_data <- mice(data_wide_selected, 
                     method = NULL, 
                     predictorMatrix = predictorMatrix, 
                     m = 5)
imputed_data$method
imputed_data_results <- complete(imputed_data, action = "long")


imputed_vars <- c("PT_CODE", "DIABETES", "DIALYSIS", "DGF", "HLAMIS", "steroid", "depleting", "TAC", "MMF", "DIABETES_DONOR", "BMI_DON_CALC", "KDPI_DON_TY", "END_BMI_CALC", "DIALYSIS_TIME", "END_CPRA", "COLD_ISCH_KI", "EDLEVEL", "WORK")
imputed_subset <- imputed_data_results[, imputed_vars]

continuous_vars <- c("BMI_DON_CALC", "KDPI_DON_TY", "HLAMIS", "END_BMI_CALC", "DIALYSIS_TIME", "END_CPRA", "COLD_ISCH_KI")
categorical_vars <- c("DIABETES", "DGF", "DIABETES_DONOR", "EDLEVEL", "WORK")

get_mode <- function(x) {
  x <- na.omit(x)  # Remove NA values
  if (length(x) == 0) return(NA)  # If all values are NA
  freq <- table(x)
  mode_value <- names(freq)[freq == max(freq)]  # Identify most frequent value(s)
  if (length(mode_value) > 1) mode_value <- mode_value[1]  # Break ties by selecting the first
  return(mode_value)
}
imputed_subset <- imputed_subset %>% group_by(PT_CODE) %>%
  summarise(
    across(all_of(continuous_vars), ~ mean(.x)),
    across(all_of(categorical_vars), ~ get_mode(.x))
  )
colnames(imputed_subset) <- paste0(colnames(imputed_subset), "_imputed")
colnames(imputed_subset)[colnames(imputed_subset) == "PT_CODE_imputed"] <- "PT_CODE"

imputed_subset$DCD<- data_wide_consistent$DCD
imputed_subset$DIALYSIS<- data_wide_consistent$DIALYSIS
imputed_subset$TAC<- data_wide_consistent$TAC
imputed_subset$MMF<- data_wide_consistent$MMF
imputed_subset$steroid<- data_wide_consistent$steroid
imputed_subset$depleting<- data_wide_consistent$depleting
imputed_subset$DECEASED<- data_wide_consistent$DECEASED

#merge with wide
data_wide_consistent <- merge(data_wide_consistent, imputed_subset, by = "PT_CODE", all.x = TRUE)

#merge with long
data_consistentFU_final <- data_consistentFU_final %>%
  inner_join(imputed_subset, by = "PT_CODE")