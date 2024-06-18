### Analysis

setwd("~/R/Data/Masters_Thesis/Final_Data")

options(scipen = 999)

# Load packages
library(car)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(jtools)
library(rempsyc)
library(gtsummary)
library(nnet)
library(sandwich)
library(interactions)
library(flextable)
library(MASS)
library(lmtest)
library(apaTables)
library(cli)
library(stdmod)
library(broom)
library(ggpubr)
library(ggsignif)
library(sjPlot)
library(effsize)
library(officer)
library(marginaleffects)
library(Rcpp)
library(report)
library(robustbase)
library(coin)
library(rstatix)

# add smoking status as a variable
library(readxl)
Smoking_status <- read_excel("Smoking_status.xlsx")

data_all
Smoking_status$study_id <- as.numeric(Smoking_status$study_id)

data_all$smoking <- ''
for( i in 1: nrow(data_all)){
  row = which(data_all$study_id[i] ==Smoking_status$study_id )
  if(length(row) >0){
    data_all$smoking[i] = Smoking_status$smoking[row]
  }
}


#-------------------------- Sample Characteristics ----------------------------------------------

data_all$SCID <- ifelse(data_all$SCID == 1, "Depressed", "Nondepressed")
data_all$Groups <- ifelse(data_all$SCID == "Depressed", "Depressed", "Nondepressed")

# extract variables for table of sample characteristics
data_subset <- data_all %>% dplyr::select(Groups, age, nationality, cult_back, rela_stat, education, dep_sev, BMI, testo_testo, biol_pulse, biol_blood_sys, biol_blood_dias)
data_subset$cult_back <- ifelse(data_subset$cult_back == "Western / European", "Western / European", "Non-Western")
data_subset$education <- ifelse(data_subset$education == "Tertiary eductaion" , "Tertiary education", "No tertiary education")
data_subset$rela_stat <- ifelse(data_subset$rela_stat == "Single", "Single", "Relationship")
data_subset$nationality[data_subset$nationality == "Lichtenstein"] <- "Other"
data_subset$nationality <- factor(data_subset$nationality, levels = c("Switzerland", "Germany", "Other"))
data_subset$cult_back <- factor(data_subset$cult_back, levels = c("Western / European", "Non-Western"))

# create table with t.test, CIs and p-values
summary_table <-data_subset %>% tbl_summary(by = Groups, statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)"), 
                                            digits = all_continuous() ~ 2, label = c(age~ "Age", nationality ~ "Nationality",  cult_back="Cultural Background", 
                                                                                     rela_stat= "Relationship Status", children= "Children", education = "Education", dep_sev= "Depression Severity",
                                                                                     BMI= "Body Mass Index (kg/m�)", testo_testo= "Testosterone (nmol/L)", biol_pulse= "Resting Heart Rate (bpm)", 
                                                                                     biol_blood_sys= "Resting Systolic Blood Pressure (mmHg)", biol_blood_dias= "Resting Diastolic Blood Pressure (mmHg)"), 
                                            missing_text = "Missing") %>% add_difference() %>% add_n() %>% modify_fmt_fun(statistic ~ style_number) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Status**") %>% modify_footnote(all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  modify_caption("**Table 1. Sample Characteristics**") %>% bold_labels() 

# necessary step to print it as a PDF
flex_table <- as_flex_table(summary_table) 

# print flextable to PDF
print(flex_table)

# create a Word document
doc <- read_docx()

# insert the flextable into the Word document
doc <- doc %>%
  body_add_flextable(value = flex_table)

# save the Word document
print(doc, target = "summary_table.docx")

# Fisher's exact test for differences among two categorical dependent variables
fisher.test(data_subset$education, data_subset$Groups) # p= 0.5 --> no significant difference


######

# without add_difference

# create table with t.test, CIs and p-values
summary_table2 <-data_subset %>% tbl_summary(by = Groups, statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)"), 
                                             digits = all_continuous() ~ 2, label = c(age~ "Age", nationality ~ "Nationality",  cult_back="Cultural Background", 
                                                                                      rela_stat= "Relationship Status", children= "Children", education = "Education", dep_sev= "Depression Severity",
                                                                                      BMI= "Body Mass Index (kg/m�)", testo_testo= "Testosterone (nmol/L)", biol_pulse= "Resting Heart Rate (bpm)", 
                                                                                      biol_blood_sys= "Resting Systolic Blood Pressure (mmHg)", biol_blood_dias= "Resting Diastolic Blood Pressure (mmHg)"), 
                                             missing_text = "missing") %>% add_p() %>% add_n() %>% modify_fmt_fun(statistic ~ style_number) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Status**") %>% modify_footnote(all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  modify_caption("**Table 1. Sample Characteristics**") %>% bold_labels() 

# necessary step to print it as a PDF
flex_table2 <- as_flex_table(summary_table2) 

# print flextable to PDF
print(flex_table2)

# create a Word document
doc <- read_docx()

# insert the flextable into the Word document
doc <- doc %>%
  body_add_flextable(value = flex_table2)

# save the Word document
print(doc, target = "summary_table_2.docx")


#-------------------------- Regression Models without Covariates ---------------------------------

############################# RQ1 #############################################

# RQ1: Do depressed and nondepressed men differ with regard to aggression levels?

# relevel SCID --> nondepressed men as reference
data_all$SCID <- as.factor(data_all$SCID)
data_all$SCID <- relevel(data_all$SCID, ref = "Nondepressed")

# t.test(af_bp_SUM ~ SCID, data = data_all)
lm_h1  = lm(af_bp_SUM ~ SCID, data = data_all)

shapiro.test(data_all$af_bp_SUM) # normality assumption violated! --> Wilcoxon-Test
leveneTest(af_bp_SUM ~ SCID, data = data_all) # homogeneity of variance fulfilled

wilcox.test <-wilcox.test(af_bp_SUM ~ SCID, data= data_all, alternative = "less", conf.int=TRUE)
wilcox.test
# if nondepressed is the reference level -> alternative = "less"
# if depressed is the reference level -> alternative = "greater"
# conf.int = TRUE --> hodges lehmann effect size

# z score (needed for claculating the effect size)
coin::wilcox_test(af_bp_SUM ~ SCID, data= data_all, alternative = "less", conf.int=TRUE)

# effect size
data_all %>% wilcox_effsize(af_bp_SUM ~ SCID)
# effect size of 0.32 --> moderate effect


#unstandardized
summ(lm_h1)


#standardized
summ(lm_h1, scale = TRUE)

# depressed men are significantly more aggressive than nondepressed men


############################# RQ2 #############################################

# RQ2: Are TMI associated with aggression?

lm_h2.1  = lm(af_bp_SUM ~ MRNI_SUM, data = data_all)
lm_h2.2  = lm(af_bp_SUM ~ CMNI_SUM, data = data_all)
lm_h2.3a  = lm(af_bp_SUM ~ MRNI_SUM*SCID, data = data_all)
lm_h2.3b  = lm(af_bp_SUM ~ CMNI_SUM*SCID, data = data_all)


#unstandardized

summ(lm_h2.1)
summ(lm_h2.2)
summ(lm_h2.3a)
summ(lm_h2.3b)


# standardized
summ(lm_h2.1, scale = TRUE)
summ(lm_h2.2, scale = TRUE)
summ(lm_h2.3a, scale = TRUE)
summ(lm_h2.3b, scale = TRUE)


# Support for H2.1 and H2.2. Depression status (H2.3a&b) does not moderate the relationship between TMI and aggression


############################# RQ3 #############################################

# RQ3: Is resting HR associated with aggression?

lm_h3.1 = lm(af_bp_SUM ~ biol_pulse, data = data_all)
lm_h3.2 = lm(af_bp_SUM ~ biol_pulse*SCID, data = data_all)


#unstandardized
summ(lm_h3.1)
summ(lm_h3.2)


# standardized
summ(lm_h3.1, scale = TRUE)
summ(lm_h3.2, scale = TRUE)

# Resting HR does not correlate with aggression (H3.1). Depression status does not moderate the relationship 
# between resting HR and aggression (H3.2)


############################# RQ4 #############################################

# RQ4: Is BP associated with aggression?

lm_h4.1a = lm(af_bp_SUM ~ biol_blood_sys, data = data_all)
lm_h4.1b = lm(af_bp_SUM ~ biol_blood_dias, data = data_all)


#unstandardized
summ(lm_h4.1a)
summ(lm_h4.1b)


# standardized
summ(lm_h4.1a, scale = TRUE)
summ(lm_h4.1b, scale = TRUE)


# Blood pressure does not correlate with aggression


############################# RQ5 #############################################

# RQ5: Is T associated with aggression?

lm_h5.1 = lm(af_bp_SUM ~ testo_testo, data = data_all)
lm_h5.2 = lm(af_bp_SUM ~ testo_testo*SCID, data = data_all)


#unstandardized
summ(lm_h5.1)
summ(lm_h5.2)


# standardized
summ(lm_h5.1, scale = TRUE)
summ(lm_h5.2, scale = TRUE)


# T does not correlate with aggression (H5.1). Depression status does not moderate the relationship between
# T and aggression (H5.2)


############################# RQ6 #############################################

# RQ6: Does resting HR interact with TMI to explain aggression?

# HR group (HR as a moderator)

data_all$HR_Group <- ifelse(data_all$biol_pulse > mean(data_all$biol_pulse, na.rm = TRUE) + sd(data_all$biol_pulse, na.rm = TRUE), "High", ifelse(data_all$biol_pulse < mean(data_all$biol_pulse, na.rm = TRUE) - sd(data_all$biol_pulse, na.rm = TRUE), "Low", "Medium"))
data_all$HR_Group <- factor(data_all$HR_Group)
data_all$HR_Group <- relevel(data_all$HR_Group, ref = "Medium")

lm_h6.1a = lm(af_bp_SUM ~ MRNI_SUM*HR_Group, data = data_all)
lm_h6.1b = lm(af_bp_SUM ~ CMNI_SUM*HR_Group, data = data_all)
lm_h6.2a = lm(af_bp_SUM ~ MRNI_SUM*HR_Group*SCID, data = data_all)
lm_h6.2b = lm(af_bp_SUM ~ CMNI_SUM*HR_Group*SCID, data = data_all)

#unstandardized
summ(lm_h6.1a)
summ(lm_h6.1b)
summ(lm_h6.2a)
summ(lm_h6.2b)


# standardized
summ(lm_h6.1a, scale = TRUE)
summ(lm_h6.1b, scale = TRUE)
summ(lm_h6.2a, scale = TRUE)
summ(lm_h6.2b, scale = TRUE)

# Resting HR does not moderate the relationship between TMI and aggression (H6.1a & b). 3-way interaction models
# are insignificant (H6.2a & b).

# interestingly, high resting HR group moderates the relationship between MRNI and aggression when the additional
# 3-way interaction term is added to the model (H6.2a). 

reg1 <- lm(af_bp_PA ~ CMNI_SUM*HR_Group, data = data_all)
summary(reg1)

aov1 <- aov(af_bp_PA ~ HR_Group, data = data_all)
summary(aov1)

reg2 <- lm(af_bp_PA ~ HR_Group, data = data_all)
summary(reg2)
boxplot(af_bp_PA ~ HR_Group, data = data_all)
# TMI groups (MRNI and CMNI as moderators)
# MRNI group


data_all$MRNI_Group <- ifelse(data_all$MRNI_SUM > mean(data_all$MRNI_SUM) + sd(data_all$MRNI_SUM), "High", ifelse(data_all$MRNI_SUM < mean(data_all$MRNI_SUM) - sd(data_all$MRNI_SUM), "Low", "Medium"))
data_all$MRNI_Group <- factor(data_all$MRNI_Group)
data_all$MRNI_Group <- relevel(data_all$MRNI_Group, ref = "Medium")

# CMNI group
mean(data_all$CMNI_SUM, na.rm = TRUE)
sd(data_all$CMNI_SUM, na.rm = TRUE)


data_all$CMNI_Group <- ifelse(data_all$CMNI_SUM > mean(data_all$CMNI_SUM, na.rm = TRUE) + sd(data_all$CMNI_SUM, na.rm = TRUE) , "High", ifelse(data_all$CMNI_SUM < mean(data_all$CMNI_SUM, na.rm = TRUE) - sd(data_all$CMNI_SUM, na.rm = TRUE), "Low", "Medium"))
data_all$CMNI_Group <- factor(data_all$CMNI_Group)
data_all$CMNI_Group <- relevel(data_all$CMNI_Group, ref = "Medium")


lm_h6.2c = lm(af_bp_SUM ~ biol_pulse*MRNI_Group, data = data_all)
lm_h6.2d = lm(af_bp_SUM ~ biol_pulse*CMNI_Group, data = data_all)

#unstandardized
summ(lm_h6.2c)
summ(lm_h6.2d)

# standardized
summ(lm_h6.2c, scale = TRUE)
summ(lm_h6.2d, scale = TRUE)

# MRNI and CMNI do not moderate the relationship between resting HR and aggression


# Testing for an interaction between TMI groups and HR groups

lm_h6.2e = lm(af_bp_SUM ~ MRNI_Group*HR_Group, data = data_all)
lm_h6.2f = lm(af_bp_SUM ~ CMNI_Group*HR_Group, data = data_all)


#unstandardized
summ(lm_h6.2e)
summ(lm_h6.2f)

# standardized
summ(lm_h6.2e, scale = TRUE)
summ(lm_h6.2f, scale = TRUE)

# no interaction effect between categorical TMI and HR groups


############################# RQ7 #############################################

# RQ7: Does BP interact with TMI to explain aggression?

# SBP group (SBP as a moderator)

data_all$SBP_Group <- ifelse(data_all$biol_blood_sys > mean(data_all$biol_blood_sys, na.rm = TRUE) + sd(data_all$biol_blood_sys, na.rm = TRUE) , "High", ifelse(data_all$biol_blood_sys < mean(data_all$biol_blood_sys, na.rm = TRUE) - sd(data_all$biol_blood_sys, na.rm = TRUE), "Low", "Medium"))
data_all$SBP_Group <- factor(data_all$SBP_Group)
data_all$SBP_Group <- relevel(data_all$SBP_Group, ref = "Medium")


# DBP group (DBP as a moderator)
mean(data_all$biol_blood_dias, na.rm = TRUE)
sd(data_all$biol_blood_dias, na.rm = TRUE)


data_all$DBP_Group <- ifelse(data_all$biol_blood_dias > mean(data_all$biol_blood_dias, na.rm = TRUE) + sd(data_all$biol_blood_dias, na.rm = TRUE), "High", ifelse(data_all$biol_blood_dias < mean(data_all$biol_blood_dias, na.rm = TRUE) - sd(data_all$biol_blood_dias, na.rm = TRUE), "Low", "Medium"))
data_all$DBP_Group <- factor(data_all$DBP_Group)
data_all$DBP_Group <- relevel(data_all$DBP_Group, ref = "Medium")


lm_h7.1a = lm(af_bp_SUM ~ MRNI_SUM*SBP_Group, data = data_all)
lm_h7.1b = lm(af_bp_SUM ~ CMNI_SUM*SBP_Group, data = data_all)
lm_h7.1c = lm(af_bp_SUM ~ MRNI_SUM*DBP_Group, data = data_all)
lm_h7.1d = lm(af_bp_SUM ~ CMNI_SUM*DBP_Group, data = data_all)


#unstandardized
summ(lm_h7.1a)
summ(lm_h7.1b)
summ(lm_h7.1c)
summ(lm_h7.1d)

# standardized
summ(lm_h7.1a, scale = TRUE)
summ(lm_h7.1b, scale = TRUE)
summ(lm_h7.1c, scale = TRUE)
summ(lm_h7.1d, scale = TRUE)


# Blood pressure (SBP & DBP) does not moderate the relationship between TMI (MRNI & CMNI) and aggression.


############################# RQ8 #############################################

# RQ8: Does T interact with TMI to explain aggression?

# T group (T as a moderator)

data_all$T_Group <- ifelse(data_all$testo_testo > mean(data_all$testo_testo, na.rm = TRUE) + sd(data_all$testo_testo, na.rm = TRUE), "High", ifelse(data_all$testo_testo < mean(data_all$testo_testo, na.rm = TRUE) - sd(data_all$testo_testo, na.rm = TRUE), "Low", "Medium"))
data_all$T_Group <- factor(data_all$T_Group)
data_all$T_Group <- relevel(data_all$T_Group, ref = "Medium")


lm_h8.1a = lm(af_bp_SUM ~ MRNI_SUM*T_Group, data = data_all)
lm_h8.1b = lm(af_bp_SUM ~ CMNI_SUM*T_Group, data = data_all)
lm_h8.2a = lm(af_bp_SUM ~ MRNI_SUM*T_Group*SCID, data = data_all)
lm_h8.2b = lm(af_bp_SUM ~ CMNI_SUM*T_Group*SCID, data = data_all)


#unstandardized
summ(lm_h8.1a)
summ(lm_h8.1b)
summ(lm_h8.2a)
summ(lm_h8.2b)

# standardized
summ(lm_h8.1a, scale = TRUE)
summ(lm_h8.1b, scale = TRUE)
summ(lm_h8.2a, scale = TRUE)
summ(lm_h8.2b, scale = TRUE)

# T does not moderate the relationship between TMI and aggression (H8.1a & b). 3-way interaction models with
# depression status as an additional moderator are insignificant.


############################# RQ9 #############################################

# RQ9: Do resting HR, BP, T and TMI differ in the amount of explained variance in aggression?

lm_h9a = lm(af_bp_SUM ~ MRNI_SUM, data = data_all)
lm_h9b = lm(af_bp_SUM ~ CMNI_SUM, data = data_all)
lm_h9c = lm(af_bp_SUM ~ biol_pulse, data = data_all)
lm_h9d = lm(af_bp_SUM ~ biol_blood_sys, data = data_all)
lm_h9e = lm(af_bp_SUM ~ biol_blood_dias, data = data_all)
lm_h9f = lm(af_bp_SUM ~ testo_testo, data = data_all)

#unstandardized
summ(lm_h9a)
summ(lm_h9b)
summ(lm_h9c)
summ(lm_h9d)
summ(lm_h9e)
summ(lm_h9f)


# standardized
summ(lm_h9a, scale = TRUE) # MRNI Adj. R� = 0.12
summ(lm_h9b, scale = TRUE) # CMNI Adj. R� = 0.22
summ(lm_h9c, scale = TRUE) # resting HR Adj. R� = 0.01 
summ(lm_h9d, scale = TRUE) # resting SBP Adj. R� = -0.01
summ(lm_h9e, scale = TRUE) # resting DBP Adj. R� = -0.01
summ(lm_h9f, scale = TRUE) # T Adj. R� = -0.01

# TMI explain more variance in aggression than the biological factors.


#-------------------------- Regression Models with Covariates ---------------------------------

############################# covariates RQ2 #############################################

# RQ2: Are TMI associated with aggression?

# Checking for depression severity as a covariate/confounder
bdi_cov <- lm(af_bp_SUM ~ BDI_SUM, data = data_all)
summary(bdi_cov) # small p --> depression severity is a potential confounder


# Checking for depression severity as a covariate/confounder
bdi_cov2 <- lm(MRNI_SUM ~ BDI_SUM, data = data_all)
summary(bdi_cov2)

bdi_cov3 <- lm(CMNI_SUM ~ BDI_SUM, data = data_all)
summary(bdi_cov3)
# BDI is associated with MRNI/CMNI and aggression --> will be included as a covariate/confounder for regression models
# with MRNI & CMNI predicting aggression, but will be left excluded from moderated regression models since 
# it is multicollinear with the moderator depression status

# Models with BDI
lm_h2.1_bdi  = lm(af_bp_SUM ~ MRNI_SUM + BDI_SUM, data = data_all)
lm_h2.2_bdi  = lm(af_bp_SUM ~ CMNI_SUM + BDI_SUM, data = data_all)

# Moderated regression models without BDI as a covariate
lm_h2.3a  = lm(af_bp_SUM ~ MRNI_SUM*SCID, data = data_all)
lm_h2.3b = lm(af_bp_SUM ~ CMNI_SUM*SCID, data = data_all)

# unstandardized
summ(lm_h2.1_bdi) # Adj. R� = 0.23
p.corr = p.adjust(summ(lm_h2.1_bdi)$coef[, "p"], method = "holm")
summ(lm_h2.2_bdi) # Adj. R� = 0.29 
p.corr = p.adjust(summ(lm_h2.2_bdi)$coef[, "p"], method = "holm")
summ(lm_h2.3a) # Adj. R� = 0.17
p.corr = p.adjust(summ(lm_h2.3a_bdi)$coef[, "p"], method = "holm")
summ(lm_h2.3b) # Adj. R� = 0.24
p.corr = p.adjust(summ(lm_h2.3b_bdi)$coef[, "p"], method = "holm")


# standardized
summ(lm_h2.1_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h2.1_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h2.2_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h2.2_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h2.3a, scale = TRUE)
p.corr = p.adjust(summ(lm_h2.3a_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h2.3b, scale = TRUE)
p.corr = p.adjust(summ(lm_h2.3b_bdi, scale = TRUE)$coef[, "p"], method = "holm")

# other potential covariates such as relationship status, age, and education were neither associated with
# TMI nor with aggression --> BDI the only covariate


############################# covariates RQ3 #############################################

# RQ3: Is resting HR associated with aggression?

# Checking for depression severity as a covariate/confounder
bdi_cov4 <- lm(biol_pulse ~ BDI_SUM, data = data_all)
summary(bdi_cov4)
# BDI does not correlate with HR, but with aggression --> will be included as a covariate/confounder in models
# without depression status as a moderator


# Model with BDI
lm_h3.1_bdi = lm(af_bp_SUM ~ biol_pulse + BDI_SUM, data = data_all)

# unstandardized
summ(lm_h3.1_bdi) # Adj. R� = 0.17
p.corr = p.adjust(summ(lm_h3.1_bdi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h3.1_bdi, scale = TRUE) # Adj. R� = 0.17
p.corr = p.adjust(summ(lm_h3.1_bdi, scale = TRUE)$coef[, "p"], method = "holm")


# Checking for BMI as a covariate/confounder
BMI_cov <- lm(af_bp_SUM ~ BMI, data = data_all)
summary(BMI_cov) # BMI does not correlate with aggression

BMI_cov2 <- lm(biol_pulse ~ BMI, data = data_all)
summary(BMI_cov2) # BMI does correlate with HR --> included as a covariate

# Models with BDI and BMI
lm_h3.1_bdi_bmi = lm(af_bp_SUM ~ biol_pulse + BDI_SUM + BMI, data = data_all)


# Moderated regression model with BMI only
lm_h3.2_bmi = lm(af_bp_SUM ~ biol_pulse*SCID + BMI, data = data_all)

# unstandardized
summ(lm_h3.1_bdi_bmi) # Adj. R� = 0.16
p.corr = p.adjust(summ(lm_h3.1_bdi_bmi)$coef[, "p"], method = "holm")
summ(lm_h3.2_bmi) # Adj. R� = 0.07
p.corr = p.adjust(summ(lm_h3.2_bmi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h3.1_bdi_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h3.1_bdi_bmi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h3.2_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h3.2_bmi, scale = TRUE)$coef[, "p"], method = "holm")

# Best models

# BDI will be included as a covariate for linear regression models
# BMI will NOT be included since its inclusion does not increase the adj. R� in all models

lm_h3.1_bdi = lm(af_bp_SUM ~ biol_pulse + BDI_SUM, data = data_all)
lm_h3.2 = lm(af_bp_SUM ~ biol_pulse*SCID, data = data_all) # without covariates


############################# covariates RQ4 #############################################


# RQ4: Is BP associated with aggression?

# Checking for depression severity as a covariate/confounder
bdi_cov5 <- lm(biol_blood_sys ~ BDI_SUM, data = data_all)
summary(bdi_cov5)
bdi_cov6 <- lm(biol_blood_dias ~ BDI_SUM, data = data_all)
summary(bdi_cov6) # significant
# BDI does not correlate with SBP, but with DBP and aggression --> will be included as a covariate/confounder 
# in models without depression status as a moderator

# Models with BDI
lm_h4.1a_bdi = lm(af_bp_SUM ~ biol_blood_sys + BDI_SUM, data = data_all)
lm_h4.1b_bdi = lm(af_bp_SUM ~ biol_blood_dias + BDI_SUM, data = data_all)

# unstandardized
summ(lm_h4.1a_bdi) # Adj. R� = 0.16
p.corr = p.adjust(summ(lm_h4.1a_bdi)$coef[, "p"], method = "holm")
summ(lm_h4.1b_bdi) # Adj. R� = 0.17
p.corr = p.adjust(summ(lm_h4.1b_bdi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h4.1a_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h4.1b_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h4.1b_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h4.1b_bdi, scale = TRUE)$coef[, "p"], method = "holm")


# checking for BMI as a covariate/confounder

BMI_cov3 <- lm(biol_blood_sys ~ BMI, data = data_all)
summary(BMI_cov3) 

BMI_cov4 <- lm(biol_blood_dias ~ BMI, data = data_all)
summary(BMI_cov4)
# BMI correlates with both SBP and DBP --> will be included in models as a covariate

# Models with BDI and BMI
lm_h4.1a_bdi_bmi = lm(af_bp_SUM ~ biol_blood_sys + BDI_SUM + BMI, data = data_all)
lm_h4.1b_bdi_bmi = lm(af_bp_SUM ~ biol_blood_dias + BDI_SUM + BMI, data = data_all)


# unstandardized
summ(lm_h4.1a_bdi_bmi) # Adj. R� = 0.16
p.corr = p.adjust(summ(lm_h4.1a_bdi_bmi)$coef[, "p"], method = "holm")
summ(lm_h4.1b_bdi_bmi) # Adj. R� = 0.16
p.corr = p.adjust(summ(lm_h4.1b_bdi_bmi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h4.1a_bdi_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h4.1b_bdi_bmi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h4.1b_bdi_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h4.1b_bdi_bmi, scale = TRUE)$coef[, "p"], method = "holm")

# inclusion of BMI as covariate does not add anything to the variance in both models

# in model h4.1a: with BMI vs. without BMI?
AIC(lm_h4.1a_bdi, lm_h4.1a_bdi_bmi)
BIC(lm_h4.1a_bdi, lm_h4.1a_bdi_bmi)
# model 4.1 without BMI is a better fit


# Checking for age as a covariate/confounder
age_cov <- lm(biol_blood_sys ~ age, data = data_all)
summary(age_cov)
age_cov2 <- lm(biol_blood_dias ~ age, data = data_all)
summary(age_cov2)
# age does not correlate with SBP, but with DBP

# Models with BDI and age
lm_h4.1a_bdi_A = lm(af_bp_SUM ~ biol_blood_sys + BDI_SUM + age, data = data_all)
lm_h4.1b_bdi_A = lm(af_bp_SUM ~ biol_blood_dias + BDI_SUM + age, data = data_all)

# unstandardized
summ(lm_h4.1a_bdi_A) # Adj. R� = 0.15
p.corr = p.adjust(summ(lm_h4.1a_bdi_A)$coef[, "p"], method = "holm")
summ(lm_h4.1b_bdi_A) # Adj. R� = 0.17
p.corr = p.adjust(summ(lm_h4.1b_bdi_A)$coef[, "p"], method = "holm")


# standardized
summ(lm_h4.1a_bdi_A, scale = TRUE)
p.corr = p.adjust(summ(lm_h4.1b_bdi_A, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h4.1b_bdi_A, scale = TRUE)
p.corr = p.adjust(summ(lm_h4.1b_bdi_A, scale = TRUE)$coef[, "p"], method = "holm")

# model h4.1b with our without age?
AIC(lm_h4.1b_bdi, lm_h4.1b_bdi_A)
BIC(lm_h4.1b_bdi, lm_h4.1b_bdi_A)
# model is better without age as a covariate


# Best models
lm_h4.1a_bdi = lm(af_bp_SUM ~ biol_blood_sys + BDI_SUM, data = data_all)
summ(lm_h4.1a_bdi)
lm_h4.1b_bdi = lm(af_bp_SUM ~ biol_blood_dias + BDI_SUM, data = data_all)
summ(lm_h4.1b_bdi)

############################# covariates RQ5 #############################################

# RQ5: Is T associated with aggression?

# Checking for depression severity as a covariate/confounder
bdi_cov8 <- lm(testo_testo ~ BDI_SUM, data = data_all)
summary(bdi_cov8)
# BDI correlates with aggression, but not with T --> will be included as a covariate in model without
# depression status as a moderator

# Model with BDI
lm_h5.1_bdi = lm(af_bp_SUM ~ testo_testo + BDI_SUM, data = data_all)

# unstandardized
summ(lm_h5.1_bdi) # Adj. R� = 0.15
p.corr = p.adjust(summ(lm_h5.1_bdi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h5.1_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h5.1_bdi, scale = TRUE)$coef[, "p"], method = "holm")


# checking for BMI as a covariate/confounder
BMI_cov5 <- lm(testo_testo ~ BMI, data = data_all)
summary(BMI_cov5)
# BMI correlates with T

# Model with BDI and BMI
lm_h5.1_bdi_bmi = lm(af_bp_SUM ~ testo_testo + BDI_SUM + BMI, data = data_all)

# Model with BMI only
lm_h5.2_bmi = lm(af_bp_SUM ~ testo_testo*SCID + BMI, data = data_all)

# unstandardized
summ(lm_h5.1_bdi_bmi) # Adj. R� = 0.15
p.corr = p.adjust(summ(lm_h5.1_bdi_bmi)$coef[, "p"], method = "holm")
summ(lm_h5.2_bmi) # Adj. R� = 0.05
p.corr = p.adjust(summ(lm_h5.2_bmi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h5.1_bdi_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h5.1_bdi_bmi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h5.2_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h5.2_bmi, scale = TRUE)$coef[, "p"], method = "holm")

# model h5.2 with BMI explains more variance than the original model without 
# it included as a covariate

# model h5.1 with or without BMI?
AIC(lm_h5.1_bdi,lm_h5.1_bdi_bmi)
BIC(lm_h5.1_bdi,lm_h5.1_bdi_bmi)
# model h5.1 without BMI is better

# Checking for age as a covariate/confounder
age_cov3 <- lm(testo_testo ~ age, data = data_all)
summary(age_cov3)
# age does neither correlate with T nor with aggression --> no covariate/confounder

# Best models
lm_h5.1_bdi = lm(af_bp_SUM ~ testo_testo + BDI_SUM, data = data_all)
summ(lm_h5.1_bdi)
lm_h5.2_bmi = lm(af_bp_SUM ~ testo_testo*SCID + BMI, data = data_all)
summ(lm_h5.2_bmi)


############################# covariates RQ6 #############################################

# RQ6: Does resting HR interact with TMI to explain aggression?

# Models with BDI as a covariate
lm_h6.1a_bdi = lm(af_bp_SUM ~ MRNI_SUM*HR_Group + BDI_SUM, data = data_all)
lm_h6.1b_bdi = lm(af_bp_SUM ~ CMNI_SUM*HR_Group + BDI_SUM, data = data_all)

# Moderated models with BMI as a covariate
lm_h6.2a_bmi = lm(af_bp_SUM ~ MRNI_SUM*HR_Group*SCID + BMI, data = data_all)
lm_h6.2b_bmi = lm(af_bp_SUM ~ CMNI_SUM*HR_Group*SCID + BMI, data = data_all)

# unstandardized
summ(lm_h6.1a_bdi) # Adj. R� = 0.25
p.corr = p.adjust(summ(lm_h6.1a_bdi)$coef[, "p"], method = "holm")
summ(lm_h6.1b_bdi) # Adj. R� = 0.33
p.corr = p.adjust(summ(lm_h6.1b_bdi)$coef[, "p"], method = "holm")
summ(lm_h6.2a_bmi) # Adj. R� = 0.21
p.corr = p.adjust(summ(lm_h6.2a_bmi)$coef[, "p"], method = "holm")
summ(lm_h6.2b_bmi) # Adj. R� = 0.33
p.corr = p.adjust(summ(lm_h6.2b_bmi)$coef[, "p"], method = "holm")


# standardized
summ(lm_h6.1a_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.1a_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h6.1b_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.1b_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h6.2a_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.2a_bmi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h6.2b_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.2b_bmi, scale = TRUE)$coef[, "p"], method = "holm")


# Models with BDI & age as covariates
lm_h6.1a_bdi_A = lm(af_bp_SUM ~ MRNI_SUM*HR_Group + BDI_SUM + age, data = data_all)
lm_h6.1b_bdi_A = lm(af_bp_SUM ~ CMNI_SUM*HR_Group + BDI_SUM + age, data = data_all)

# Moderated models with BMI and age as covariates
lm_h6.2a_bmi_A = lm(af_bp_SUM ~ MRNI_SUM*HR_Group*SCID + BMI + age, data = data_all)
lm_h6.2b_bmi_A = lm(af_bp_SUM ~ CMNI_SUM*HR_Group*SCID + BMI + age, data = data_all)

# unstandardized
summ(lm_h6.1a_bdi_A) # Adj. R� = 0.24
p.corr = p.adjust(summ(lm_h6.1a_bdi_A)$coef[, "p"], method = "holm")
summ(lm_h6.1b_bdi_A) # Adj. R� = 0.33
p.corr = p.adjust(summ(lm_h6.1b_bdi_A)$coef[, "p"], method = "holm")
summ(lm_h6.2a_bmi_A) # Adj. R� = 0.20
p.corr = p.adjust(summ(lm_h6.2a_bmi_A)$coef[, "p"], method = "holm")
summ(lm_h6.2b_bmi_A) # Adj. R� = 0.32
p.corr = p.adjust(summ(lm_h6.2b_bmi_A)$coef[, "p"], method = "holm")


# standardized
summ(lm_h6.1a_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.1a_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h6.1b_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.1b_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h6.2a_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.2a_bmi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h6.2b_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h6.2b_bmi, scale = TRUE)$coef[, "p"], method = "holm")

# age does not improve the models --> excluded as a covariate

# Best models
lm_h6.1a_bdi = lm(af_bp_SUM ~ MRNI_SUM*HR_Group + BDI_SUM, data = data_all)
summ(lm_h6.1a_bdi)
lm_h6.1b_bdi = lm(af_bp_SUM ~ CMNI_SUM*HR_Group + BDI_SUM, data = data_all)
summ(lm_h6.1b_bdi)

lm_h6.2a_bmi = lm(af_bp_SUM ~ MRNI_SUM*HR_Group*SCID + BMI, data = data_all)
summ(lm_h6.2a_bmi)
lm_h6.2b_bmi = lm(af_bp_SUM ~ CMNI_SUM*HR_Group*SCID + BMI, data = data_all)
summ(lm_h6.2b_bmi)


############################# covariates RQ7 #############################################

# RQ7: Does BP interact with TMI to explain aggression?

# only BDI as a covariate; other covariates such as age and BMI do not add value to the
# models

# Models with BDI as a covariate
lm_h7.1a_bdi = lm(af_bp_SUM ~ MRNI_SUM*SBP_Group + BDI_SUM, data = data_all)
lm_h7.1b_bdi = lm(af_bp_SUM ~ CMNI_SUM*SBP_Group + BDI_SUM, data = data_all)
lm_h7.1c_bdi = lm(af_bp_SUM ~ MRNI_SUM*DBP_Group + BDI_SUM, data = data_all)
lm_h7.1d_bdi = lm(af_bp_SUM ~ CMNI_SUM*DBP_Group + BDI_SUM, data = data_all)

# unstandardized
summ(lm_h7.1a_bdi) # Adj. R� = 0.22
p.corr = p.adjust(summ(lm_h7.1a_bdi)$coef[, "p"], method = "holm")
summ(lm_h7.1b_bdi) # Adj. R� = 0.29
p.corr = p.adjust(summ(lm_h7.1b_bdi)$coef[, "p"], method = "holm")
summ(lm_h7.1c_bdi) # Adj. R� = 0.22
p.corr = p.adjust(summ(lm_h7.1c_bdi)$coef[, "p"], method = "holm")
summ(lm_h7.1d_bdi) # Adj. R� = 0.26
p.corr = p.adjust(summ(lm_h7.1d_bdi)$coef[, "p"], method = "holm")


# standardized
summ(lm_h7.1a_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h7.1a_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h7.1b_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h7.1b_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h7.1c_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h7.1c_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h7.1d_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h7.1d_bdi, scale = TRUE)$coef[, "p"], method = "holm")

# Best models
lm_h7.1a_bdi = lm(af_bp_SUM ~ MRNI_SUM*SBP_Group + BDI_SUM, data = data_all)
lm_h7.1b_bdi = lm(af_bp_SUM ~ CMNI_SUM*SBP_Group + BDI_SUM, data = data_all)
lm_h7.1c_bdi = lm(af_bp_SUM ~ MRNI_SUM*DBP_Group + BDI_SUM, data = data_all)
lm_h7.1d_bdi = lm(af_bp_SUM ~ CMNI_SUM*DBP_Group + BDI_SUM, data = data_all)


############################# covariates RQ8 #############################################

# RQ8: Does T interact with TMI to explain aggression?

# Models with BDI as a covariate
lm_h8.1a_bdi = lm(af_bp_SUM ~ MRNI_SUM*T_Group + BDI_SUM, data = data_all)
lm_h8.1b_bdi = lm(af_bp_SUM ~ CMNI_SUM*T_Group + BDI_SUM, data = data_all)

# Models with BMI as a covariate
lm_h8.2a_bmi = lm(af_bp_SUM ~ MRNI_SUM*T_Group*SCID + BMI, data = data_all)
lm_h8.2b_bmi = lm(af_bp_SUM ~ CMNI_SUM*T_Group*SCID + BMI, data = data_all)


# unstandardized
summ(lm_h8.1a_bdi) # Adj. R� = 0.17
p.corr = p.adjust(summ(lm_h8.1a_bdi)$coef[, "p"], method = "holm")
summ(lm_h8.1b_bdi) # Adj. R� = 0.26
p.corr = p.adjust(summ(lm_h8.1b_bdi)$coef[, "p"], method = "holm")
summ(lm_h8.2a_bmi) # Adj. R� = 0.10
p.corr = p.adjust(summ(lm_h8.2a_bmi)$coef[, "p"], method = "holm")
summ(lm_h8.2b_bmi) # Adj. R� = 0.19
p.corr = p.adjust(summ(lm_h8.2b_bmi)$coef[, "p"], method = "holm")

# standardized
summ(lm_h8.1a_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h8.1a_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h8.1b_bdi, scale = TRUE)
p.corr = p.adjust(summ(lm_h8.1b_bdi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h8.2a_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h8.2a_bmi, scale = TRUE)$coef[, "p"], method = "holm")
summ(lm_h8.2b_bmi, scale = TRUE)
p.corr = p.adjust(summ(lm_h8.2a_bmi, scale = TRUE)$coef[, "p"], method = "holm")

# Best models
lm_h8.1a_bdi = lm(af_bp_SUM ~ MRNI_SUM*T_Group + BDI_SUM, data = data_all)
lm_h8.1b_bdi = lm(af_bp_SUM ~ CMNI_SUM*T_Group + BDI_SUM, data = data_all)
lm_h8.2a_bmi = lm(af_bp_SUM ~ MRNI_SUM*T_Group*SCID + BMI, data = data_all)
lm_h8.2b_bmi = lm(af_bp_SUM ~ CMNI_SUM*T_Group*SCID + BMI, data = data_all)


#-------------------------- Checking Model Assumptions ---------------------------------

# h2.1

# visual assessment

plot(lm_h2.1)
plot(lm_h2.1_bdi)  

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h2.1_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h2.1)
dwtest(lm_h2.1_bdi)


# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h2.1))
shapiro.test(rstudent(lm_h2.1_bdi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h2.1)
bptest(lm_h2.1_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h2.1)) # below
plot(cooks.distance(lm_h2.1_bdi)) #below .5

############ robust regression for h2.1


# h2.2

# visual assessment
plot(lm_h2.2)
plot(lm_h2.2_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h2.2_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h2.2)
dwtest(lm_h2.2_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h2.2))
shapiro.test(rstudent(lm_h2.2_bdi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h2.2)
bptest(lm_h2.2_bdi)
# equal variance

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h2.2)) # below .5
plot(cooks.distance(lm_h2.2_bdi)) #below .5

############ robust regression for h2.2

# h2.3a

# visual assessment
plot(lm_h2.3a)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h2.3a)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h2.3a)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h2.3a))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h2.3a)
# equal variance

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h2.3a)) #below .5

########### robust regression for h2.3a


# h2.3b

# visual assessment
plot(lm_h2.3b)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h2.3b)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h2.3b)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h2.3b))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h2.3b)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h2.3b)) #below .5

######### robust regression for h2.3b

# h3.1

# visual assessment
plot(lm_h3.1_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h3.1_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h3.1_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h3.1_bdi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h3.1_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h3.1_bdi)) #below .5

######### robust regression for h3.1


# h3.2

# visual assessment
plot(lm_h3.2)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h3.2)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h3.2)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h3.2))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h3.2)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h3.2)) #below .5

######## robust regression for h3.2


# h4.1a

# visual assessment
plot(lm_h4.1a_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h4.1a_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h4.1a_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h4.1a_bdi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h4.1a_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h4.1a_bdi)) #below .5


########### robust regression for h4.1a


# h4.1b

# visual assessment
plot(lm_h4.1b_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h4.1b_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h4.1b_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h4.1b_bdi))
# normality assumption violated!


# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h4.1b_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h4.1b_bdi)) #below .5


########## robust regression for h4.1b


# h5.1

# visual assessment
plot(lm_h5.1_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h5.1_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h5.1_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h5.1_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h5.1_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h5.1_bdi)) #below .5


# h5.2

# visual assessment
plot(lm_h5.2_bmi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h5.2_bmi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h5.2_bmi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h5.2_bmi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h5.2_bmi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h5.2_bmi)) #below .5


# h6.1a

# visual assessment
plot(lm_h6.1a_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h6.1a_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h6.1a_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h6.1a_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h6.1a_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h6.1a_bdi)) #below .5


# h6.1b

# visual assessment
plot(lm_h6.1b_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h6.1b_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h6.1b_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h6.1b_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h6.1b_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h6.1b_bdi)) # below .5


# h6.2a

# visual assessment
plot(lm_h6.2a_bmi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h6.2a_bmi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h6.2a_bmi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h6.2a_bmi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h6.2a_bmi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h6.2a_bmi)) # one outlier (far above .5)


# h6.2b

# visual assessment
plot(lm_h6.2b_bmi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h6.2b_bmi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h6.2b_bmi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h6.2b_bmi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h6.2b_bmi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h6.2b_bmi)) # one outlier (far above .5)


# h7.1a

# visual assessment
plot(lm_h7.1a_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h7.1a_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h7.1a_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h7.1a_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h7.1a_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h7.1a_bdi)) # below .5


# h7.1b

# visual assessment
plot(lm_h7.1b_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h7.1b_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h7.1b_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h7.1b_bdi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h7.1b_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h7.1b_bdi)) # below .5

########### robust regression for h7.1b


# h7.1c

# visual assessment
plot(lm_h7.1c_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h7.1c_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h7.1c_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h7.1c_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h7.1c_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h7.1c_bdi)) # below .5


# h7.1d

# visual assessment
plot(lm_h7.1d_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h7.1d_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h7.1d_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h7.1d_bdi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h7.1d_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h7.1d_bdi)) # below .5

############# robust regression for h7.1d


# h8.1a

# visual assessment
plot(lm_h8.1a_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h8.1a_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h8.1a_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h8.1a_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h8.1a_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h8.1a_bdi)) # below .5


# h8.1b

# visual assessment
plot(lm_h8.1b_bdi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h8.1b_bdi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h8.1b_bdi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h8.1b_bdi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h8.1b_bdi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h8.1b_bdi)) # below .5


# h8.2a

# visual assessment
plot(lm_h8.2a_bmi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h8.2a_bmi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h8.2a_bmi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h8.2a_bmi))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h8.2a_bmi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h8.2a_bmi)) # below .5


# h8.2b

# visual assessment
plot(lm_h8.2b_bmi)

# check for multicollinearity (if values < 10 [better if < 5] no multicollin.)
vif(lm_h8.2b_bmi)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h8.2b_bmi)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h8.2b_bmi))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h8.2b_bmi)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h8.2b_bmi)) # below .5

############# robust regression for h8.2b


# h9c

# visual assessment

plot(lm_h9c)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h9c)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h9c))
# normality assumption violated

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h9c)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h9c)) # below .5


# h9d

# visual assessment

plot(lm_h9d)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h9d)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h9d))
# normality assumption violated

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h9d)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h9d)) # below .5


# h9e

# visual assessment

plot(lm_h9e)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h9e)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h9e))
# normality assumption violated

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h9e)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h9e)) # below .5


# h9f

# visual assessment

plot(lm_h9f)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(lm_h9f)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(lm_h9f))

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(lm_h9f)

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(lm_h9f)) # below .5

########### robust regressions for h9c,d, and e

# summary:
# robust regressions for all models apart from h5.1, h5.2, h6.1a, h6.1b, h6.2a, h6.2b,
# h7.1a, h7.1c, h8.1a, h8.1b & h8.2a



#--------------------------------- Correlational Analysis ----------------------------------------

# datasets for depressed and nondepressed men each
dataset_depressed <- subset(data_all_HR_BP, data_all_HR_BP$SCID != "Nondepressed")
dataset_nondepressed <- subset(data_all_HR_BP, data_all_HR_BP$SCID != "Depressed")

# Correlations of all predictors with each other

all_variables <- data_all[, c("af_bp_SUM", "MRNI_SUM", "CMNI_SUM", "biol_pulse", "biol_blood_sys", "biol_blood_dias",
                              "testo_testo")]

all_variables_corr <- apa.cor.table(all_variables, table.number=2, 
                                    filename = "Table 2. Correlation Matrix for Main Variables.doc")


# CMNI Subscales and Aggression


cmni_aggress = data_all[, c("af_bp_SUM", "CMNI_SUM","EmC", "Het", "Viol", "Self", "Work", "Win", "Status", "Women", "Play", "Risk")]
cmni_corrs <- apa.cor.table(cmni_aggress, table.number=3, 
                            filename = "Table 3. Correlation Matrix of Total Scores and Subscales of AF-BP and CMNI-30.doc")

print(cmni_corrs, filename ="CMNI corrs")


# Resting HR, SBP, DBP, T and subscales of Aggression (total sample)

Bio_SUB_aggress = data_all[, c("af_bp_PA", "af_bp_VA", "af_bp_A", "af_bp_H", "biol_pulse", "biol_blood_sys", "biol_blood_dias", "testo_testo")]
Bio_SUB_aggress_corr <- apa.cor.table(Bio_SUB_aggress, table.number = 4, 
                                      filename = "Table 4. Correlation Matrix of Subscales of AF-BP and Biological Factors.doc")

# TMI & Biological factors and aggression in depressed men
Bio_SUB_aggress = dataset_depressed[, c("af_bp_SUM", "af_bp_PA", "af_bp_VA", "af_bp_A", "af_bp_H", "MRNI_SUM", "CMNI_SUM", "biol_pulse", "biol_blood_sys", "biol_blood_dias", "testo_testo")]
Bio_SUB_aggress_corr <- apa.cor.table(Bio_SUB_aggress, table.number = 5, 
                                      filename = "Table 5. Correlation Matrix of Aggression and Biological Factors in Depressed Men.doc")


# TMI & Biological factors and aggression in nondepressed men
Bio_SUB_aggress2 = dataset_nondepressed[, c("af_bp_SUM", "af_bp_PA", "af_bp_VA", "af_bp_A", "af_bp_H", "MRNI_SUM", "CMNI_SUM", "biol_pulse", "biol_blood_sys", "biol_blood_dias", "testo_testo")]
Bio_SUB_aggress_corr2<- apa.cor.table(Bio_SUB_aggress2, table.number = 6, 
                                      filename = "Table 6. Correlation Matrix of Aggression and Biological Factors in Nondepressed Men.doc")


# CMNI and Resting Heart Rate
CMNI_RHR = data_all[, c("biol_pulse", "CMNI_SUM","EmC", "Het", "Viol", "Self", "Work", "Win", "Status", "Women", "Play", "Risk")]
CMNI_RHR_corr <- apa.cor.table(CMNI_RHR, table.number = 7,
                               filename= "Table 7. Correlation Matrix of Resting Heart Rate and Total Scores and Subscales of CMNI-30.doc")


# Blood Pressure and Aggression

BP_aggress = data_all[, c("af_bp_SUM", "af_bp_PA", "af_bp_VA", "af_bp_A", "af_bp_H", "biol_blood_sys", "biol_blood_dias")]
BP_aggress_corr <- apa.cor.table(BP_aggress, table.number = 8, 
                                 filename = "Table 8. Correlation Matrix of Total Scores and Subscales of AF-BP and Resting Blood Pressure.doc")

# Testosterone and Aggression

T_aggress = data_all[, c("af_bp_SUM", "af_bp_PA", "af_bp_VA", "af_bp_A", "af_bp_H", "testo_testo")]
T_aggress_corr <- apa.cor.table(T_aggress, table.number = 9, 
                                filename = "Table 9. Correlation Matrix of Total Scores and Subscales of AF-BP and Testosterone.doc")


###################################### Robust Regressions ###############################

# h2.1 without covariates
rob_h2.1  = lmrob(af_bp_SUM ~ MRNI_SUM, data = data_all)
summary(rob_h2.1)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h2.1), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h2.1)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")



# h2.1 with covariates
rob_h2.1_bdi = lmrob(af_bp_SUM ~ MRNI_SUM + BDI_SUM, data = data_all)
summary(rob_h2.1_bdi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h2.1_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h2.1_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h2.2 without covariates
rob_h2.2  = lmrob(af_bp_SUM ~ CMNI_SUM, data = data_all)
summary(rob_h2.2)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h2.2), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h2.2)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h2.2 with covariates
rob_h2.2_bdi  = lmrob(af_bp_SUM ~ CMNI_SUM + BDI_SUM, data = data_all)
summary(rob_h2.2_bdi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h2.2_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h2.2_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h2.3a without covariates

rob_h2.3a  = lmrob(af_bp_SUM ~ MRNI_SUM*SCID, data = data_all)
summary(rob_h2.3a)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h2.3a), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h2.3a)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h2.3b without covariates

rob_h2.3b  = lmrob(af_bp_SUM ~ CMNI_SUM*SCID, data = data_all)
summary(rob_h2.3b)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h2.3b), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h2.3b)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")
p.corr


# h3.1 without covariates
rob_h3.1 = lmrob(af_bp_SUM ~ biol_pulse, data = data_all)
summary(rob_h3.1)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h3.1), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h3.1)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h3.1 with covariates
rob_h3.1_bdi = lmrob(af_bp_SUM ~ biol_pulse + BDI_SUM, data = data_all)
summary(rob_h3.1_bdi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h3.1_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h3.1_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h3.2 without covariates
rob_h3.2 = lmrob(af_bp_SUM ~ biol_pulse*SCID, data = data_all)
summary(rob_h3.2)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h3.2), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h3.2)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")
p.corr

# h4.1a without covariates
rob_h4.1a = lmrob(af_bp_SUM ~ biol_blood_sys, data = data_all)
summary(rob_h4.1a)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h4.1a), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h4.1a)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h4.1a with covariates
rob_h4.1a_bdi = lmrob(af_bp_SUM ~ biol_blood_sys + BDI_SUM, data = data_all)
summary(rob_h4.1a_bdi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h4.1a_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h4.1a_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h4.1b without covariates
rob_h4.1b = lmrob(af_bp_SUM ~ biol_blood_dias, data = data_all)
summary(rob_h4.1b)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h4.1b), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h4.1b)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")



# h4.1b with covariates
rob_h4.1b_bdi = lmrob(af_bp_SUM ~ biol_blood_dias + BDI_SUM, data = data_all)
summary(rob_h4.1b_bdi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h4.1b_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h4.1b_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h7.1b without covariates
rob_h7.1b = lmrob(af_bp_SUM ~ CMNI_SUM*SBP_Group, data = data_all)
summary(rob_h7.1b)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h7.1b), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h7.1b)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")

##### simple slopes of CMNI_SUM for different SBP groups

# generating three datasets containing only participants with either low, medium or
# high SBP
data_low_SBP <- subset(data_all, data_all$SBP_Group != "Medium" & data_all$SBP_Group != "High")
data_medium_SBP <- subset(data_all, data_all$SBP_Group != "Low" & data_all$SBP_Group != "High")
data_high_SBP <- subset(data_all, data_all$SBP_Group != "Medium" & data_all$SBP_Group != "Low")

low_SBP_CMNI <- lmrob(af_bp_SUM ~ CMNI_SUM, data = data_low_SBP)
summary(low_SBP_CMNI)
# slope of CMNI when SBP group low is significant and moderately strong

medium_SBP_CMNI <- lmrob(af_bp_SUM ~ CMNI_SUM, data = data_medium_SBP)
summary(medium_SBP_CMNI)
# slope of CMNI when SBP group is medium is significant as well, but weaker

high_SBP_CMNI <- lmrob(af_bp_SUM ~ CMNI_SUM, data = data_high_SBP)
summary(high_SBP_CMNI)
# slope of CMNI when SBP group is high is insignificant


# h7.1b with covariates
rob_h7.1b_bdi = lmrob(af_bp_SUM ~ CMNI_SUM*SBP_Group + BDI_SUM, data = data_all)
summary(rob_h7.1b_bdi)
### low SBP moderates the relationship between CMNI and aggression***

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h7.1b_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h7.1b_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h7.1d without covariates
rob_h7.1d = lmrob(af_bp_SUM ~ CMNI_SUM*DBP_Group, data = data_all)
summary(rob_h7.1d)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h7.1d), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h7.1d)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h7.1d with covariates
rob_h7.1d_bdi = lmrob(af_bp_SUM ~ CMNI_SUM*DBP_Group + BDI_SUM, data = data_all)
summary(rob_h7.1d_bdi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h7.1d_bdi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h7.1d_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")
p.corr

# h8.2b without covariates
rob_h8.2b = lmrob(af_bp_SUM ~ CMNI_SUM*T_Group*SCID, data = data_all)
summary(rob_h8.2b)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h8.2b), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h8.2b)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


# h8.2b with covariates
rob_h8.2b_bmi = lmrob(af_bp_SUM ~ CMNI_SUM*T_Group*SCID + BMI, data = data_all)
summary(rob_h8.2b_bmi)

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_h8.2b_bmi), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_h8.2b_bmi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")


### robust regression
library("robustbase")

rob1 <- lmrob(af_bp_SUM ~ CMNI_SUM*HR_Group, data = data_all)
summary(rob1)

reg1 <- lm(af_bp_PA ~ CMNI_SUM*HR_Group, data = data_all)
summary(reg1)

confint.default(object = rob1, parm = "scale(CMNI_SUM)", level = 0.95)


rob_lm_h2.1 = lmrob(af_bp_SUM ~ MRNI_SUM, data = data_all)
summary(rob_lm_h2.1)

### CIs of robust regression
plot_coeftest= broom::tidy(lmtest::coeftest(rob1), conf.int = TRUE)
plot_coeftest


# robust regression with adjusted p values
rob1 <- lmrob(af_bp_SUM ~ CMNI_SUM * HR_Group, data = data_all)
summary(rob1)

p_values <- summary(rob1)$coefficients[, "Pr(>|t|)"]
adjusted_p_values <- p.adjust(p_values, method = "holm")
summary_table <- summary(rob1)$coefficients
summary_table[, "Pr(>|t|)"] <- adjusted_p_values
print(summary_table)




####################################### Additional Analyses ##################################################

# taking physical aggression as the outcome

reg1 <- lm(af_bp_PA ~ CMNI_SUM*HR_Group, data = data_all)
summary(reg1)
# low resting HR moderates the relationship between CMNI and physical aggression

# checking model assumptions
vif(reg1)

# check for autocorrelation (if p > .05 no autocorrelation) 
# Autocorrelation refers to the degree of closeness or correlation between values of the same variable or
# data series at different periods
dwtest(reg1)

# check if studentized residuals are normally distributed 
# (if p > .05 they follow normal distribution)
shapiro.test(rstudent(reg1))
# normality assumption violated!

# check if residuals are homoscedastic (equal variance)
# (if p > .05 they are homoscedastic)
bptest(reg1)
# equal variance

#check for outliers --> values above .5 indicate influential outliers
plot(cooks.distance(reg1)) #below .5


# reg1 <- lm(af_bp_PA ~ CMNI_SUM*HR_Group, data = data_all)
# does not fulfill parametric assumptions
# robust regression required

rob_PA <- lmrob(af_bp_PA ~ CMNI_SUM*HR_Group, data = data_all)
summary(rob_PA)
# low resting HR moderates the association between CMNI and physical aggression!!!

# CI
plot_coeftest= broom::tidy(lmtest::coeftest(rob_PA), conf.int = TRUE)
plot_coeftest

# corrected p-values
p_values <- summary(rob_PA_bdi)$coefficients[, "Pr(>|t|)"]
p.corr <- p.adjust(p_values, method = "holm")
# this interaction model sustains adjustment for multiple testing!

# with covariate BDI
rob_PA_bdi <- lmrob(af_bp_PA ~ CMNI_SUM*HR_Group + BDI_SUM, data = data_all)
summary(rob_PA_bdi)


rob_PA_CMNI <- lmrob(af_bp_PA ~ CMNI_SUM, data = data_all)
summary(rob_PA_CMNI)
# compared to the model only containing CMNI as a predictor, the moderated model
# with low RHR explains more variance!

rob_T_PA <- lmrob(af_bp_PA ~ CMNI_SUM*T_Group, data = data_all)
summary(rob_T_PA)
# men with higher CMNI and low T are significantly LESS aggressive than men with medium 
# and high T



#---------------------------- Regression Tables------------------------------

### Linear regression tables
generate_regression_table <- function(model, description_text, additional_note ="") {
  # Extract coefficients and summary statistics
  stats_table <- as.data.frame(summary(model)$coefficients)
  CI <- confint(model)
  Beta_std <- round(summ(model, scale = TRUE)$coef[, "Est."], digits = 2)
  p_values <- round(summary(model)$coef[, "Pr(>|t|)"], digits = 3)
  p_corr <- as.data.frame(p.adjust(p_values, method = "holm"))
  adjusted_r_squared <- round(summary(model)$adj.r.squared, digits = 3)
  
  # Combine data into a single table
  stats_table <- cbind(row.names(stats_table), stats_table, p_corr, Beta_std, CI, adjusted_r_squared)
  names(stats_table) <- c("Term", "Braw", "SE", "t", "p", "95% CI lower", "95% CI upper", "p (corr.)", "Ba", "Adj. R^2")
  
  # Print the table
  print(stats_table)
  
  # Print the nice table
  table_MRNI_Agg <- nice_table(stats_table, title = c(description_text),
                               note = c(additional_note,"* p < .05, ** p < .01, *** p < .001"))
  print(table_MRNI_Agg, preview = "docx")
}

### you can type in any regression model in the functions below and receive the corresponding Word file
generate_regression_table(lm_h6.1a, "***TABLE 5. Regressions***", "SD=Standard Deviation")



# Robust Regression Tables (without standardized betas, p.corr, and adj. R�)
generate_robust_regression_table <- function(model, description_text, additional_note ="") {
  # Extract coefficients and summary statistics
  coefs <- coef(summary(model))
  CI <- confint(model)
  p_values <- round(summary(model)$coef[, "Pr(>|t|)"], digits = 3)
  
  
  # Extract variable names
  terms <- rownames(coefs)
  
  # Combine data into a single table
  stats_table <- cbind(terms, coefs, CI)
  names(stats_table) <- c("Term", "Braw", "SE", "t", "p", "95% CI lower", "95% CI upper")
  
  # Print the table
  print(stats_table)
  
  # Print the nice table
  table_robust <- nice_table(stats_table, title = c(description_text),
                             note = c(additional_note, "* p < .05, ** p < .01, *** p < .001"))
  print(table_robust, preview = "docx")
}

generate_robust_regression_table <- function(model, description_text, additional_note ="") {
  # Extract coefficients and summary statistics
  coefs <- coef(summary(model))
  CI <- confint(model)
  p_values <- summary(model)$coef[, "Pr(>|t|)"]
  
  # Round coefficients, SE, t-values, and CI to two digits
  coefs <- round(coefs, digits = 2)
  CI <- round(CI, digits = 2)
  p_values_rounded <- round(p_values, digits = 3)
  
  # Replace extremely small p-values with "< .001"
  p_values_rounded[p_values_rounded < 0.001] <- "< .001"
  
  # Replace the last column of the coefs dataframe with rounded p-values
  coefs[, ncol(coefs)] <- p_values_rounded
  
  # Extract variable names
  terms <- rownames(coefs)
  
  # Combine data into a single table
  stats_table <- cbind(terms, coefs, CI)
  names(stats_table) <- c("Term", "Braw", "SE", "t", "p", "95% CI lower", "95% CI upper")
  
  # Print the table
  print(stats_table)
  
  # Print the nice table
  table_robust <- nice_table(stats_table, title = c(description_text),
                             note = c(additional_note, "* p < .05, ** p < .01, *** p < .001"))
  print(table_robust, preview = "docx")
}


generate_robust_regression_table(rob_PA_bdi, "hier eine Beschreibung", "hier eine Notiz")
generate_robust_regression_table(rob_h4.1a, "text")



#-------------------------- Plots ---------------------------------

# RQ1: Do depressed and nondepressed men differ with regard to aggression levels?

# Calculate mean and standard deviation for each group
group_means <- tapply(data_all$af_bp_SUM, data_all$SCID, mean)
group_sds <- tapply(data_all$af_bp_SUM, data_all$SCID, sd)

group_sds
wilcoxon_result <- wilcox.test(data_all$af_bp_SUM ~ data_all$SCID, alternative= "less", var.equal= TRUE)

# Calculate difference in means
mean_diff <- diff(group_means)

# Define significance levels
significance <- ifelse(wilcoxon_result$p.value < 0.05, "*", "")

# Plot the barplot
ggbarplot(data_all, x = "SCID", y = "af_bp_SUM", add = c("mean_sd"), fill = c("grey","darkblue"), 
          ylab = "Aggression (BPAQ)", xlab = "Groups")+
  annotate("text", x = 1.5, y = max(group_means) + max(group_sds), label = significance, size = 6, vjust = -0.29)
# add "jitter" next to "mean_sd" and you see the single scores/dots of each participant

group_depressed <- data_all[data_all$SCID == "Depressed", "af_bp_SUM"]
group_nondepressed <- data_all[data_all$SCID == "Nondepressed", "af_bp_SUM"]
cohens_d <- cohen.d(group_depressed, group_nondepressed)
cohens_d # .65 Medium effect


# RQ2: Are TMI associated with aggression?

# h2.1   MRNI & Aggression (without covariates)
lm_h2.1  = lm(af_bp_SUM ~ MRNI_SUM, data = data_all)
summ(lm_h2.1)

cor.test(data_all$MRNI_SUM, data_all$af_bp_SUM, method = "spearman")

ggplot(data_all, aes(x = MRNI_SUM, y = af_bp_SUM))+ geom_point() + geom_smooth(method = "lmrob", se = TRUE, color= "#4ab8fc") + 
  labs(x = "Endorsement of Masculine Norms (MRNI-SF)", y = "Aggression (BPAQ)") + stat_cor(method = "spearman",
                                                                                           cor.coef.name = "r", digits = 2) + theme_classic()

# h2.2 CMNI & Aggression (without covariates)
lm_h2.2 = lm(af_bp_SUM ~ CMNI_SUM, data = data_all)
summ(lm_h2.2)

cor.test(data_all$CMNI_SUM, data_all$af_bp_SUM)

ggplot(data_all, aes(x = CMNI_SUM, y = af_bp_SUM))+ geom_point() + geom_smooth(method = "lmrob", se = TRUE, color= "#4ab8fc") + 
  labs(x = "Conformity to Masculine Norms (CMNI-30)", y = "Aggression (BPAQ)") + stat_cor(method = "spearman",
                                                                                          cor.coef.name = "r", digits = 2) + theme_classic()


# h2.3a MRNI*SCID & Aggression

# without covariates
lm_h2.3a <- lm(af_bp_SUM ~ MRNI_SUM * SCID, data = data_all)
summ(lm_h2.3a)

# robust regression
rob_h2.3a

p <- plotmod(rob_h2.3a, x = "MRNI_SUM", w = "SCID", x_label = "Endorsement of Masculine Norms (MRNI-SF)", 
             y_label= "Aggression (BPAQ)", w_label= "Groups", title = NULL)
p+ scale_color_manual(values = c("darkblue", "grey")) + theme_classic() + theme(plot.subtitle = element_blank())

probe_interaction(lm_h2.3a, pred= MRNI_SUM, modx= SCID, alpha= .05)


# h2.3b CMNI*SCID & Aggression

# without covariates
lm_h2.3b = lm(af_bp_SUM ~ CMNI_SUM*SCID, data = data_all)
summ(lm_h2.3b)

# robust regression
summary(rob_h2.3b)

p2 <- plotmod(rob_h2.3b, x = "CMNI_SUM", w = "SCID", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = NULL) +  ylim(c(30,70)) + xlim(c(50,100))
p2+ scale_color_manual(values = c("blue", "grey")) + theme_classic() + theme(plot.subtitle = element_blank())+ geom_smooth()

# h3.1 HR & Aggression (without covariates)

# create new a dataset that excludes participants with missing HR and BP values
data_all_HR_BP <- data_all[!is.na(data_all$biol_pulse), ]

lm_h3.1 = lm(af_bp_SUM ~ biol_pulse, data = data_all_HR_BP)
summ(lm_h3.1)

ggplot(data_all_HR_BP, aes(x = biol_pulse, y = af_bp_SUM))+ geom_point() + geom_smooth(method = "lmrob", se = TRUE, color= "#4ab8fc") + 
  labs(x = "Resting Heart Rate (bpm)", y = "Aggression (BPAQ)") + stat_cor(method = "spearman", cor.coef.name = "r", digits = 2) + theme_classic()


# h3.2 HR*SCID & Aggression

# without covariates
lm_h3.2 = lm(af_bp_SUM ~ biol_pulse*SCID, data = data_all_HR_BP)
summ(lm_h3.2)

# robust regression
summary(rob_h3.2)

p3 <- plotmod(rob_h3.2, x = "biol_pulse", w = "SCID", x_label = "Resting Heart Rate (bpm)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = NULL) + ylim(c(35,60)) + xlim(c(50,80))
p3+ scale_color_manual(values = c("blue", "grey")) + theme_classic() + theme(plot.subtitle = element_blank())+ geom_smooth(method = "lmrob", se = TRUE)

probe_interaction(lm_h3.2, pred = "biol_pulse", modx = "SCID")

# RQ4: Is BP associated with aggression?

# h4.1a SBP & Aggression (without covariates)
lm_h4.1a = lm(af_bp_SUM ~ biol_blood_sys, data = data_all_HR_BP)
summ(lm_h4.1a)

ggplot(data_all_HR_BP, aes(x = biol_blood_sys, y = af_bp_SUM))+ geom_point() + geom_smooth(method = "lmrob", se = TRUE, color= "#4ab8fc") + 
  labs(x = "Resting Systolic Blood Pressure (mmHg)", y = "Aggression (BPAQ)") + stat_cor(method = "spearman", cor.coef.name = "r", digits = 2) + 
  theme_classic()


# h4.1b DBP & Aggression (without covariates)
lm_h4.1b = lm(af_bp_SUM ~ biol_blood_dias, data= data_all_HR_BP)
summ(lm_h4.1b)

ggplot(data_all_HR_BP, aes(x = biol_blood_dias, y = af_bp_SUM))+ geom_point() + geom_smooth(method = "lmrob", se = TRUE, color= "#4ab8fc") + 
  labs(x = "Resting Diastolic Blood Pressure (mmHg)", y = "Aggression (BPAQ)") + stat_cor(method = "spearman", cor.coef.name = "r", digits = 2) +
  theme_classic()


# RQ5: Is T associated with aggression?

# h5.1 T and Aggression (without covariates)
lm_h5.1 = lm(af_bp_SUM ~ testo_testo, data = data_all)
summ(lm_h5.1)


ggplot(data_all, aes(x = testo_testo, y = af_bp_SUM))+ geom_point() + geom_smooth(method = "lmrob", se = TRUE, color= "#4ab8fc") + 
  labs(x = "Testosterone (nmol/L)", y = "Aggression (BPAQ)") + stat_cor(method = "spearman", cor.coef.name = "r", digits = 2) +
  theme_classic()


# h5.2 T*SCID & Aggression

# without covariates
lm_h5.2 = lm(af_bp_SUM ~ testo, data = data_all)

summ(lm_h5.2)

p4 <- plotmod(lm_h5.2, x = "testo_testo", w = "SCID", x_label = "Testosterone (nmol/L)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = "Testosterone Effects for Depressed and Nondepressed Men", digits = 5)
p4+ scale_color_manual(values = c("blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# with covariates
p4_cov <- plotmod(lm_h5.2_bmi, x = "testo_testo", w = "SCID", x_label = "Testosterone (nmol/L)", y_label = "Aggression (BPAQ)",
                  w_label= "Groups", title = NULL) + ylim(c(40,60)) + xlim(c(12,30))
p4_cov+ scale_color_manual(values = c("blue", "grey")) + theme_classic() + theme(plot.subtitle = element_blank())

summ(lm_h5.2_bmi)

# RQ6: Does resting HR interact with TMI to explain aggression?

# h6.1a MRNI*HR Group & Aggression

### change order of the presentation of Low, Medium, and High HR Group
### data_all_HR_BP$HR_Group <- factor(data_all_HR_BP$HR_Group, levels = c("Low", "Medium", "High"))

# without covariates (MRNI)
lm_h6.1a = lm(af_bp_SUM ~ MRNI_SUM*HR_Group, data = data_all)
summ(lm_h6.1a)


p5 <- plotmod(lm_h6.1a, x = "MRNI_SUM", w = "HR_Group", x_label = "Endorsement of Masculine Norms (MRNI-SF)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = "MRNI Effects for Low, Medium and High Heart Rate", digits = 2)
p5+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# with covariates
p5_cov <- plotmod(lm_h6.1a_bdi, x = "MRNI_SUM", w = "HR_Group", x_label = "Endorsement of Masculine Norms (MRNI-SF)", y_label = "Aggression (BPAQ)",
                  w_label= "Groups", title = NULL)
p5_cov+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank()) 




# h6.1b CMNI*HR_Group & Aggression

# without covariates (CMNI)
lm_h6.1b = lm(af_bp_SUM ~ CMNI_SUM*HR_Group, data = data_all)
summ(lm_h6.1b)

p6 <- plotmod(lm_h6.1b, x = "CMNI_SUM", w = "HR_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = "CMNI Effects for Low, Medium and High Heart Rate", digits = 2)
p6+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# with covariates
p6_cov <- plotmod(lm_h6.1b_bdi, x = "CMNI_SUM", w = "HR_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
                  w_label= "Groups", title = NULL) + ylim(c(30,70)) + xlim(c(55,100))
p6_cov+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())





# h6.2a MRNI*HR_Group*SCID & Aggression (three-way interaction)

# without covariates
lm_h6.2a = lm(af_bp_SUM ~ MRNI_SUM*HR_Group*SCID, data = data_all)
summ(lm_h6.2a)

plot7 <- interact_plot(lm_h6.2a, pred = "MRNI_SUM", modx = "HR_Group", mod2= "SCID", interval = FALSE,
                       x.label = "Endorsement of Masculine Norms (MRNI-SF)", y.label = "Aggression (BPAQ)",
                       mod2.labels = c("Nondepressed", "Depressed"), main.title = "3-Way-Interaction between MRNI, HR and Depression Status") + theme_classic()
plot7 + scale_color_manual(values = c("darkblue", "blue", "grey"))+ theme(plot.subtitle = element_blank())

probe_interaction(lm_h6.2a, pred= MRNI_SUM, modx= HR_Group, mod2 = SCID, alpha= .05)


# h6.2b CMNI*HR_Group*SCID & Aggression (three-way interaction)

# without covariates
lm_h6.2b = lm(af_bp_SUM ~ CMNI_SUM*HR_Group*SCID, data = data_all)
summ(lm_h6.2b)

plot8 <- interact_plot(lm_h6.2b, pred = "CMNI_SUM", modx = "HR_Group", mod2= "SCID", interval = FALSE,
                       x.label = "Conformity to Masculine Norms (CMNI-30)", y.label = "Aggression (BPAQ)",
                       mod2.labels = c("Nondepressed", "Depressed"), main.title = NULL) + theme_classic()
plot8 + scale_color_manual(values = c("darkblue", "blue", "grey"))+ theme(plot.subtitle = element_blank())

probe_interaction(lm_h6.2b, pred= CMNI_SUM, modx= HR_Group, mod2 = SCID, alpha= .05)


# with covariates
plot8_cov <- interact_plot(lm_h6.2b_bmi, pred = "CMNI_SUM", modx = "HR_Group", mod2= "SCID", interval = FALSE,
                           x.label = "Conformity to Masculine Norms (CMNI-30)", y.label = "Aggression (BPAQ)",
                           mod2.labels = c("Nondepressed", "Depressed"), main.title = NULL) + theme_classic()
plot8_cov + scale_color_manual(values = c("darkblue", "blue", "grey")) + theme(plot.subtitle = element_blank())


# RQ7: Does low BP interact with TMI to explain aggression?

# h7.1a MRNI*SBP_Group & Aggression

# without covariates
lm_h7.1a = lm(af_bp_SUM ~ MRNI_SUM*SBP_Group, data = data_all)
summ(lm_h7.1a)

p7 <- plotmod(lm_h7.1a, x = "MRNI_SUM", w = "SBP_Group", x_label = "Endorsement of Masculine Norms (MRNI-SF)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = NULL)
p7+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# h7.1b CMNI*SBP_Group & Aggression

# without covariates
lm_h7.1b = lm(af_bp_SUM ~ CMNI_SUM*SBP_Group, data = data_all)
summ(lm_h7.1b)

# robust regression
summary(rob_h7.1b)


p8 <- plotmod(rob_h7.1b, x = "CMNI_SUM", w = "SBP_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = NULL) + ylim(c(35,60))+ xlim(c(55,100))
p8+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())

probe_interaction(lm_h7.1b, pred= CMNI_SUM, modx= SBP_Group, alpha= .05)


p8_cov <- plotmod(rob_h7.1b_bdi, x = "CMNI_SUM", w = "SBP_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
                  w_label= "Groups", title = NULL) + ylim(c(35,60))+ xlim(c(55,100))
p8_cov+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank()) 


# h7.1c MRNI*DBP_Group & Aggression

# without covariates
lm_h7.1c = lm(af_bp_SUM ~ MRNI_SUM*DBP_Group, data = data_all)
summ(lm_h7.1c)


p9 <- plotmod(lm_h7.1c, x = "MRNI_SUM", w = "DBP_Group", x_label = "Endorsement of Masculine Norms (MRNI-SF)", y_label = "Aggression (BPAQ)",
              w_label= "Groups", title = "MRNI Effects for Low, Medium and High DBP", digits = 2)
p9+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# h7.1d CMNI*DBP_Group & Aggression

# without covariates
lm_h7.1d = lm(af_bp_SUM ~ CMNI_SUM*DBP_Group, data = data_all)
summ(lm_h7.1d)

# robust regression
summary(rob_h7.1d)

p10 <- plotmod(rob_h7.1d, x = "CMNI_SUM", w = "DBP_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
               w_label= "Groups", title = "CMNI Effects for Low, Medium and High DBP", digits = 2)
p10+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# RQ8: Does T interact with TMI to explain aggression?

# h8.1a MRNI*T_Group & Aggression

# without covariates
lm_h8.1a = lm(af_bp_SUM ~ MRNI_SUM*T_Group, data = data_all)
summ(lm_h8.1a)

p11 <- plotmod(lm_h8.1a, x = "MRNI_SUM", w = "T_Group", x_label = "Endorsement of Masculine Norms (MRNI-SF)", y_label = "Aggression (BPAQ)",
               w_label= "Groups", title = "MRNI Effects for Low, Medium and High Testosterone", digits = 2)
p11+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# h8.1b CMNI*T_Group & Aggression

# without covariates
lm_h8.1b = lm(af_bp_SUM ~ CMNI_SUM*T_Group, data = data_all)
summ(lm_h8.1b)

p12 <- plotmod(lm_h8.1b, x = "CMNI_SUM", w = "T_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
               w_label= "Groups", title = "CMNI Effects for Low, Medium and High Testosterone", digits = 2)
p12+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())


# with covariates
summ(lm_h8.1b_bdi)
p12_cov <- plotmod(lm_h8.1b_bdi, x = "CMNI_SUM", w = "T_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", y_label = "Aggression (BPAQ)",
                   w_label= "Groups", title = NULL) + ylim(c(30,60)) + xlim(c(55,100))
p12_cov+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic() + theme(plot.subtitle = element_blank())

probe_interaction(lm_h8.1b_bdi, pred = "CMNI_SUM", modx = "T_Group")

# h6.2a MRNI*T_Group*SCID & Aggression (three-way interaction)

# without covariates
lm_h8.2a = lm(af_bp_SUM ~ MRNI_SUM*T_Group*SCID, data = data_all)
summ(lm_h8.2a)

plot9 <- interact_plot(lm_h8.2a, pred = "MRNI_SUM", modx = "T_Group", mod2= "SCID", interval = FALSE,
                       x.label = "Endorsement of Masculine Norms (MRNI-SF)", y.label = "Aggression (BPAQ)",
                       mod2.labels = c("Nondepressed", "Depressed"), main.title = "3-Way-Interaction between MRNI, T and Depression Status") + theme_classic()
plot9 + scale_color_manual(values = c("darkblue", "blue", "grey"))+ theme(plot.subtitle = element_blank())

probe_interaction(lm_h8.2a, pred= MRNI_SUM, modx= T_Group, mod2 = SCID, alpha= .05)



# h8.2b CMNI*T_Group*SCID & Aggression (three-way interaction)

# without covariates
lm_h8.2b = lm(af_bp_SUM ~ CMNI_SUM*T_Group*SCID, data = data_all)
summ(lm_h8.2b)

# robust regression
summary(rob_h8.2b)

plot10 <- interact_plot(rob_h8.2b, pred = "CMNI_SUM", modx = "T_Group", mod2= "SCID", interval = FALSE,
                        x.label = "Conformity to Masculine Norms (CMNI-30)", y.label = "Aggression (BPAQ)",
                        mod2.labels = c("Nondepressed", "Depressed"), main.title = "3-Way-Interaction between CMNI, T and Depression Status") + theme_classic()
plot10 + scale_color_manual(values = c("darkblue", "blue", "grey"))+ theme(plot.subtitle = element_blank())

probe_interaction(rob_h8.2b, pred= CMNI_SUM, modx= T_Group, mod2 = SCID, alpha= .05)
### robust regression does not work for probe_interaction apparently!!!


## additional plot: Low RHR moderates CMNI-aggression relationship

p13 <- plotmod(rob_PA, x = "CMNI_SUM", w = "HR_Group", x_label = "Conformity to Masculine Norms (CMNI-30)", 
               y_label= "Physical Aggression (BPAQ)", w_label= "Groups", title = NULL) + ylim(c(10,18)) + xlim(c(55,100))
p13+ scale_color_manual(values = c("darkblue", "blue", "grey")) + theme_classic()+ theme(plot.subtitle = element_blank())



##################### Plots for Presentation ############

# without covariates
rob_h2.1= lmrob(af_bp_SUM ~ MRNI_SUM, data = data_all)

# with covariates
rob_h2.1_bdi  = lm(af_bp_SUM ~ MRNI_SUM + BDI_SUM, data = data_all)


reg_lines <- data.frame(
  intercept = c(rob_h2.1$coef[[1]], rob_h2.1_bdi$coef[[1]]),
  slope = c(rob_h2.1$coef[[2]], rob_h2.1_bdi$coef[[2]]),
  lty = factor(c(1,2)))

ggplot(data_all, aes(x = MRNI_SUM, y = af_bp_SUM)) +
  geom_point() + #add scatter points
  #add line for model without covariates
  geom_abline(data = reg_lines,
              aes(intercept = intercept,
                  slope = slope,
                  lty = lty),
              color = "blue",
              size = 1.2) + theme_classic() +
  #change the labeling
  labs(
    x = "Endorsement of Masculine Norms (MRNI-SF)",
    y = "Aggression (BPAQ") +
  #change the legend labels
  scale_linetype_manual(name = "",
                        values = c(2,1),
                        labels = c("without covariates",
                                   "with covariates"))


# without covariates
rob_h2.2  = lmrob(af_bp_SUM ~ CMNI_SUM, data = data_all)

# with covariates
rob_h2.2_bdi  = lmrob(af_bp_SUM ~ CMNI_SUM + BDI_SUM, data = data_all)

reg_lines <- data.frame(
  intercept = c(rob_h2.2$coef[[1]], rob_h2.2_bdi$coef[[1]]),
  slope = c(rob_h2.2$coef[[2]], rob_h2.2_bdi$coef[[2]]),
  lty = factor(c(1,2)))

ggplot(data_all, aes(x = CMNI_SUM, y = af_bp_SUM)) +
  geom_point() + #add scatter points
  #add line for model without covariates
  geom_abline(data = reg_lines,
              aes(intercept = intercept,
                  slope = slope,
                  lty = lty),
              color = "blue",
              size = 1.2) + theme_classic() + ylim(c(30,80)) +
  #change the labeling
  labs(
    x = "Conformity to Masculine Norms (CMNI-30)",
    y = "Aggression (BPAQ)") +
  #change the legend labels
  scale_linetype_manual(name = "",
                        values = c(2,1),
                        labels = c("without covariates",
                                   "with covariates"))




# without covariates
rob_h3.1 = lmrob(af_bp_SUM ~ biol_pulse, data = data_all)

# with covariates
rob_h3.1_bdi = lmrob(af_bp_SUM ~ biol_pulse + BDI_SUM, data = data_all)

reg_lines <- data.frame(
  intercept = c(rob_h3.1$coef[[1]], rob_h3.1_bdi$coef[[1]]),
  slope = c(rob_h3.1$coef[[2]], rob_h3.1_bdi$coef[[2]]),
  lty = factor(c(1,2)))

ggplot(data_all, aes(x = biol_pulse, y = af_bp_SUM)) +
  geom_point() + #add scatter points
  #add line for model without covariates
  geom_abline(data = reg_lines,
              aes(intercept = intercept,
                  slope = slope,
                  lty = lty),
              color = "blue",
              size = 1.2) + theme_classic() + ylim(c(30,80)) +
  #change the labeling
  labs(
    x = "Resting Heart Rate (bpm)",
    y = "Aggression (BPAQ)") +
  #change the legend labels
  scale_linetype_manual(name = "",
                        values = c(2,1),
                        labels = c("without covariates",
                                   "with covariates"))


# h4.1a without covariates
rob_h4.1a = lmrob(af_bp_SUM ~ biol_blood_sys, data = data_all)

# h4.1a with covariates
rob_h4.1a_bdi = lmrob(af_bp_SUM ~ biol_blood_sys + BDI_SUM, data = data_all)

reg_lines <- data.frame(
  intercept = c(rob_h4.1a$coef[[1]], rob_h4.1a_bdi$coef[[1]]),
  slope = c(rob_h4.1a$coef[[2]], rob_h4.1a_bdi$coef[[2]]),
  lty = factor(c(1,2)))

ggplot(data_all, aes(x = biol_blood_sys, y = af_bp_SUM)) +
  geom_point() + #add scatter points
  #add line for model without covariates
  geom_abline(data = reg_lines,
              aes(intercept = intercept,
                  slope = slope,
                  lty = lty),
              color = "blue",
              size = 1.2) + theme_classic() +
  #change the labeling
  labs(
    x = "Resting Systolic Blood Pressure (mmHg) ",
    y = "Aggression (BPAQ)") +
  #change the legend labels
  scale_linetype_manual(name = "",
                        values = c(2,1),
                        labels = c("without covariates",
                                   "with covariates"))


# h4.1b without covariates
rob_h4.1b = lmrob(af_bp_SUM ~ biol_blood_dias, data = data_all)

# h4.1b with covariates
rob_h4.1b_bdi = lmrob(af_bp_SUM ~ biol_blood_dias + BDI_SUM, data = data_all)

reg_lines <- data.frame(
  intercept = c(rob_h4.1b$coef[[1]], rob_h4.1b_bdi$coef[[1]]),
  slope = c(rob_h4.1b$coef[[2]], rob_h4.1b_bdi$coef[[2]]),
  lty = factor(c(1,2)))

ggplot(data_all, aes(x = biol_blood_dias, y = af_bp_SUM)) +
  geom_point() + #add scatter points
  #add line for model without covariates
  geom_abline(data = reg_lines,
              aes(intercept = intercept,
                  slope = slope,
                  lty = lty),
              color = "blue",
              size = 1.2) + theme_classic() +
  #change the labeling
  labs(
    x = "Resting Diastolic Blood Pressure (mmHg) ",
    y = "Aggression (BPAQ)") +
  #change the legend labels
  scale_linetype_manual(name = "",
                        values = c(2,1),
                        labels = c("without covariates",
                                   "with covariates"))


# without covariates
lm_h5.1 = lm(af_bp_SUM ~ testo_testo, data = data_all)

# with covariates
lm_h5.1_bdi = lm(af_bp_SUM ~ testo_testo + BDI_SUM, data = data_all)

reg_lines <- data.frame(
  intercept = c(lm_h5.1$coef[[1]], lm_h5.1_bdi$coef[[1]]),
  slope = c(lm_h5.1$coef[[2]], lm_h5.1_bdi$coef[[2]]),
  lty = factor(c(1,2)))

ggplot(data_all, aes(x = testo_testo, y = af_bp_SUM)) +
  geom_point() + #add scatter points
  #add line for model without covariates
  geom_abline(data = reg_lines,
              aes(intercept = intercept,
                  slope = slope,
                  lty = lty),
              color = "blue",
              size = 1.2) + theme_classic() +
  #change the labeling
  labs(
    x = "Testosterone (nmol/L)",
    y = "Aggression (BPAQ)") +
  #change the legend labels
  scale_linetype_manual(name = "",
                        values = c(2,1),
                        labels = c("without covariates",
                                   "with covariates"))


hist(data_all$af_bp_SUM, xlab = "Aggression (BPAQ)", main = NULL, col = "blue")
hist(data_all$MRNI_SUM, xlab = "Endorsement of Masculine Norms (MRNI-SF)", main = NULL, col = "blue")
hist(data_all$CMNI_SUM, xlab = "Conformity to Masculine Norms (CMNI-30)", main = NULL, col = "blue")
hist(data_all$biol_pulse, xlab = "Resting Heart Rate (bpm)", main = NULL, col = "blue")
hist(data_all$biol_blood_sys, xlab = "Resting Systolic Blood Pressure (mmHg)", main = NULL, col = "blue")
hist(data_all$biol_blood_dias, xlab = "Resting Diastolic Blood Pressure (mmHg)", main = NULL, col = "blue")
hist(data_all$testo_testo, xlab = "Testosterone (nmol/L)", main = NULL, col = "blue")


bxp <- ggboxplot(data_all, x = "SCID", y = "af_bp_PA", fill = c("grey","blue"),
                 ylab = "Physical Aggression (BPAQ)", xlab = "Groups", add = ("mean")) + geom_signif(comparisons = list(c("Nondepressed", "Depressed")), 
                                                                                                     map_signif_level=TRUE)
bxp

bxp <- ggboxplot(data_all, x = "SCID", y = "af_bp_VA", fill = c("grey","blue"),
                 ylab = "Verbal Aggression (BPAQ)", xlab = "Groups", add = ("mean")) + geom_signif(comparisons = list(c("Nondepressed", "Depressed")), 
                                                                                                   map_signif_level=TRUE)
bxp

bxp <- ggboxplot(data_all, x = "SCID", y = "af_bp_A", fill = c("grey","blue"),
                 ylab = "Anger (BPAQ)", xlab = "Groups", add = ("mean")) + geom_signif(comparisons = list(c("Nondepressed", "Depressed")), 
                                                                                       map_signif_level=TRUE)
bxp

bxp <- ggboxplot(data_all, x = "SCID", y = "af_bp_H", fill = c("grey","blue"),
                 ylab = "Hostility (BPAQ)", xlab = "Groups", add = ("mean")) + geom_signif(comparisons = list(c("Nondepressed", "Depressed")), 
                                                                                           map_signif_level=TRUE)
bxp

#######

library(ggplot2)

# Your data and ggbarplot function
# Assuming group_means, group_sds, and significance are defined elsewhere in your code
plot <- ggbarplot(data_all, x = "SCID", y = "af_bp_H", 
                  add = c("mean_sd"), fill = c("grey", "darkblue"),
                  ylab = "Hostility (BPAQ)", xlab = "Groups") + geom_signif(comparisons = list(c("Nondepressed", "Depressed")), 
                                                                            map_signif_level=TRUE)
annotate("text", x = 1.5, y = max(group_means) + max(group_sds) + 2, 
         label = significance, size = 6, vjust = -0.29)

# Extend the height of the plot
ggsave("barplot.png", plot, width = 8, height = 8)

# Or, if you're saving as PDF
# ggsave("barplot.pdf", plot, width = 8, height = 6)


data_all$T_Group[data_all$T_Group == "High"]


reg1 <- lmrob(af_bp_SUM ~ CMNI_SUM + BDI_SUM, data = data_medium_SBP)
summary(reg1)

cor.test(data_medium_SBP$af_bp_SUM, data_medium_SBP$CMNI_SUM)
cor.test(data_high_SBP$CMNI_SUM, data_high_SBP$af_bp_SUM)

boxplot(CMNI_SUM ~ HR_Group, data = data_all)


probe_interaction(lm_h6.1b, pred = "CMNI_SUM", modx = "HR_Group")




########### datasets for low, medium, and high HR as well as T groups + effect sizes

data_low_HR <- subset(data_all, data_all$HR_Group != "Medium" & data_all$HR_Group != "High")
data_medium_HR <- subset(data_all, data_all$HR_Group != "Low" & data_all$HR_Group != "High")
data_high_HR <- subset(data_all, data_all$HR_Group != "Medium" & data_all$HR_Group != "Low")

cor.test(data_low_HR$af_bp_SUM, data_low_HR$CMNI_SUM, method = "pearson")
cor.test(data_medium_HR$af_bp_SUM, data_medium_HR$CMNI_SUM, method = "pearson")
cor.test(data_high_HR$af_bp_SUM, data_high_HR$CMNI_SUM, method = "pearson")


data_low_T <- subset(data_all, data_all$T_Group != "Medium" & data_all$T_Group != "High")
data_medium_T <- subset(data_all, data_all$T_Group != "Low" & data_all$T_Group != "High")
data_high_T <- subset(data_all, data_all$T_Group != "Medium" & data_all$T_Group != "Low")

cor.test(data_low_T$af_bp_SUM, data_low_T$CMNI_SUM, method = "pearson")
cor.test(data_medium_T$af_bp_SUM, data_medium_T$CMNI_SUM, method = "pearson")
cor.test(data_high_T$af_bp_SUM, data_high_T$CMNI_SUM, method = "pearson")


data_low_DBP <- subset(data_all, data_all$DBP_Group != "Medium" & data_all$DBP_Group != "High")
data_medium_DBP <- subset(data_all, data_all$DBP_Group != "Low" & data_all$DBP_Group != "High")
data_high_DBP <- subset(data_all, data_all$DBP_Group != "Medium" & data_all$DBP_Group != "Low")



cor.test(dataset_nondepressed$CMNI_SUM, dataset_nondepressed$af_bp_SUM)
cor.test(dataset_depressed$CMNI_SUM, dataset_depressed$af_bp_SUM)

cor.test(data_low_SBP$af_bp_SUM, data_low_SBP$CMNI_SUM, method = "pearson")
cor.test(data_medium_SBP$af_bp_SUM, data_medium_SBP$CMNI_SUM, method = "pearson")
cor.test(data_high_SBP$af_bp_SUM, data_high_SBP$CMNI_SUM, method = "pearson")


cor.test(data_all$af_bp_SUM, data_all$CMNI_SUM)

cor.test(dataset_nondepressed$af_bp_SUM, dataset_nondepressed$CMNI_SUM)
cor.test(dataset_depressed$af_bp_SUM, dataset_depressed$CMNI_SUM)


cor.test(data_all$af_bp_SUM, data_all$biol_pulse)
cor.test(dataset_nondepressed$af_bp_SUM, dataset_nondepressed$biol_pulse)
cor.test(dataset_depressed$af_bp_SUM, dataset_depressed$biol_pulse)

cor.test(data_all$af_bp_SUM, data_all$biol_blood_sys)

cor.test(data_all$af_bp_SUM, data_all$testo_testo)
cor.test(dataset_nondepressed$af_bp_SUM, dataset_nondepressed$testo_testo)
cor.test(dataset_depressed$af_bp_SUM, dataset_depressed$testo_testo)


