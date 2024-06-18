setwd("~/R/Data/Masters_Thesis/Final_Data")


library("dplyr")
library("psych")
library("tidyverse")


#---------------------------------------- Create Dataset: MDD Patients -------------------------------------------------

dataset<- read.csv2("arm1_raw.csv", header = TRUE, sep = ",", quote = "\"", dec = ",", fill = TRUE, comment.char = "")


### Recode
### Concatenating multiple elements from multiple vectors into one single element --> define items

afbp_items <- paste("af_bp_", seq (1, 29, 1), sep = "")
cmni_items <- paste("cmni30_", seq(1,30,1), sep = "")
mrni_items <- paste("mrni_", seq(1,21,1), sep = "")
bdi_items <- paste("bdi_ii_", seq(1,21,1), sep = "")

# Heart Rate

dataset_HR <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == "") ,
                      c("study_id", "biol_pulse")]
# create dataset HR under the condition to choose all rows named "examination_appoin_arm_1 in the column "redcap_event_name"
# ONLY IF the column redcap_repeat_instrument is empty (thereby removes rows that contain the same study id more than once)

# removing the superfluous study_id 214 since it appears twice (we keep the one with the HR score)
dataset_HR <- dataset_HR %>% filter(!(study_id == 214 & is.na(biol_pulse)))

dataset_HR$biol_pulse <- as.numeric(dataset_HR$biol_pulse)


# Blood Pressure

dataset_BP <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == "") ,
                      c("study_id", "biol_blood_sys", "biol_blood_dias")]

dataset_BP <- dataset_BP %>% filter(!(study_id == 214 & is.na(biol_blood_sys) & is.na(biol_blood_dias)))

dataset_BP$biol_blood_sys <- as.numeric(dataset_BP$biol_blood_sys)
dataset_BP$biol_blood_dias <- as.numeric(dataset_BP$biol_blood_dias)



#Testosterone

dataset_T <- dataset[dataset$redcap_event_name == "testosterone_deter_arm_1", c("study_id","testo_testo", "testo_testo_2")]

dataset_T$testo_testo <- as.numeric(dataset_T$testo_testo)
dataset_T$testo_testo_2 <- as.numeric(dataset_T$testo_testo_2)

dataset_T <- dataset_T %>% mutate(testo_testo = ifelse(dataset_T$testo_testo < 12, 
                                                       (dataset_T$testo_testo + dataset_T$testo_testo_2) / 2, dataset_T$testo_testo))
# mutate function --> modifying the column "testo_testo"
# ifelse --> checking the condition whether a value in the column "testo_testo" is less than 12

# (dfd_t$testo_testo + dfd_t$testo_testo_2) / 2 --> if the condition (dfd_t$testo_testo < 12) is true, this part of the command 
# calculates the average of testo_testo and testo_testo_2 columns and assigns it to the testo_testo column

# dfd_t$testo_testo --> if the condition (dfd_t$testo_testo < 12) is false, this part of the command assigns the current values of the 
# testo_testo column to the testo_testo column itself, leaving it unchanged


dataset_T <- dataset_T[, c("study_id","testo_testo")]

# removing study_id 214 since it appears twice (we keep the 214 with the testosterone and delete the one with an NA)
dataset_T <- dataset_T %>% filter(!(study_id == 214 & is.na(testo_testo)))


# Aggression

dataset_Agg <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == "") ,
                       c("study_id", 
                         afbp_items)]
dataset_Agg <- dataset_Agg %>% filter(!(study_id == 214 & is.na(af_bp_1)))

#Delete Missings
dataset_Agg <- na.omit(dataset_Agg)
#Create Sum Scores
dataset_Agg$af_bp_SUM <- rowSums(dataset_Agg[,afbp_items])

### Create sum scores for subscales of Buss & Perry

dataset_Agg$af_bp_PA <- rowSums(dataset_Agg[, c("af_bp_1", "af_bp_5", "af_bp_9", "af_bp_12", "af_bp_16", "af_bp_18", "af_bp_19", "af_bp_22", "af_bp_25")])
dataset_Agg$af_bp_VA <- rowSums(dataset_Agg[, c("af_bp_2", "af_bp_6", "af_bp_13", "af_bp_17", "af_bp_29")])
dataset_Agg$af_bp_A <- rowSums(dataset_Agg[, c("af_bp_3", "af_bp_7", "af_bp_10", "af_bp_14", "af_bp_20", "af_bp_23", "af_bp_26")])
dataset_Agg$af_bp_H <- rowSums(dataset_Agg[, c("af_bp_4", "af_bp_8", "af_bp_11", "af_bp_15", "af_bp_21", "af_bp_24", "af_bp_28")])

# TMI
# MRNI-SF

dataset_MRNI <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == ""), c("study_id",mrni_items)]
#Delete Missings
dataset_MRNI <- na.omit(dataset_MRNI)
#Create Sum Scores
dataset_MRNI$MRNI_SUM <- rowSums(dataset_MRNI[,mrni_items])


# CMNI-30

dataset_CMNI <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == ""), c("study_id",cmni_items)]
#Delete Missings
dataset_CMNI <- na.omit(dataset_CMNI)
#Create Sum Scores
dataset_CMNI$CMNI_SUM <- rowSums(dataset_CMNI[,cmni_items])


# BDI-2 (depression severity might be an important confounder)
dataset_bdi <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == "") ,
                       c("study_id", 
                         bdi_items)]

#Delete Missings
dataset_bdi <- na.omit(dataset_bdi)
#Create Sum Scores
dataset_bdi$BDI_SUM <- rowSums(dataset_bdi[,bdi_items])

#Depression Severity: 1 = minimal ; 2 = mild ; 3 = moderate ; 4 = severe
dataset_bdi$dep_sev <- ifelse(dataset_bdi$BDI_SUM < 13, 1, ifelse(dataset_bdi$BDI_SUM > 12 & dataset_bdi$BDI_SUM < 20, 2, ifelse(dataset_bdi$BDI_SUM > 19 & dataset_bdi$BDI_SUM < 29, 3, 4)))


# merge all datasets
dataset_final <- merge(dataset_Agg, dataset_CMNI, by = "study_id")
dataset_final <- merge(dataset_final, dataset_MRNI, by = "study_id")
dataset_final <- merge(dataset_final, dataset_HR, by = "study_id")
dataset_final<- merge(dataset_final, dataset_BP, by = "study_id")
dataset_final<- merge(dataset_final, dataset_T, by = "study_id")
dataset_final<- merge(dataset_final, dataset_bdi, by = "study_id")

#Add SCID
dataset_final$SCID <- 1



# ------------------------------------ Create Dataset: Nondepressed (healthy) -----------------------------------------------

dataset2 <- read.csv2("arm2_raw.csv", header = TRUE, sep = ",", quote = "\"", dec = ",", fill = TRUE, comment.char = "")

### Recode
### Concatenating multiple elements from multiple vectors into one single element --> define items

afbp_items <- paste("af_bp_", seq (1, 29, 1), sep = "")
cmni_items <- paste("cmni30_", seq(1,30,1), sep = "")
mrni_items <- paste("mrni_", seq(1,21,1), sep = "")
bdi_items <- paste("bdi_ii_", seq(1,21,1), sep = "")

# Heart Rate

dataset2_HR <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2") & (dataset2$redcap_repeat_instrument == "") ,
                        c("study_id", "biol_pulse")]
dataset2_HR$biol_pulse <- as.numeric(dataset2_HR$biol_pulse)


# Blood Pressure

dataset2_BP <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2"),
                        c("study_id", "biol_blood_sys", "biol_blood_dias")]

dataset2_BP$biol_blood_sys <- as.numeric(dataset2_BP$biol_blood_sys)
dataset2_BP$biol_blood_dias <- as.numeric(dataset2_BP$biol_blood_dias)


#Testosterone

dataset2_T <- dataset2[dataset2$redcap_event_name == "testosterone_deter_arm_2", c("study_id","testo_testo", "testo_testo_2")]

dataset2_T$testo_testo <- as.numeric(dataset2_T$testo_testo)
dataset2_T$testo_testo_2 <- as.numeric(dataset2_T$testo_testo_2)

dataset2_T <- dataset2_T %>% mutate(testo_testo = ifelse(dataset2_T$testo_testo < 12, 
                                                         (dataset2_T$testo_testo + dataset2_T$testo_testo_2) / 2, dataset2_T$testo_testo))

dataset2_T <- dataset2_T[, c("study_id","testo_testo")]


# Aggression

dataset2_Agg <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2") & (dataset2$redcap_repeat_instrument == "") ,
                         c("study_id", 
                           afbp_items)]

#Delete Missings
dataset2_Agg <- na.omit(dataset2_Agg)
#Create Sum Scores
dataset2_Agg$af_bp_SUM <- rowSums(dataset2_Agg[,afbp_items])


### Create sum scores for subscales of Buss & Perry

dataset2_Agg$af_bp_PA <- rowSums(dataset2_Agg[, c("af_bp_1", "af_bp_5", "af_bp_9", "af_bp_12", "af_bp_16", "af_bp_18", "af_bp_19", "af_bp_22", "af_bp_25")])
dataset2_Agg$af_bp_VA <- rowSums(dataset2_Agg[, c("af_bp_2", "af_bp_6", "af_bp_13", "af_bp_17", "af_bp_29")])
dataset2_Agg$af_bp_A <- rowSums(dataset2_Agg[, c("af_bp_3", "af_bp_7", "af_bp_10", "af_bp_14", "af_bp_20", "af_bp_23", "af_bp_26")])
dataset2_Agg$af_bp_H <- rowSums(dataset2_Agg[, c("af_bp_4", "af_bp_8", "af_bp_11", "af_bp_15", "af_bp_21", "af_bp_24", "af_bp_28")])


# TMI
# MRNI-SF

dataset2_MRNI <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2") & (dataset2$redcap_repeat_instrument == ""), c("study_id",mrni_items)]
#Delete Missings
dataset2_MRNI <- na.omit(dataset2_MRNI)
#Create Sum Scores
dataset2_MRNI$MRNI_SUM <- rowSums(dataset2_MRNI[,mrni_items])


# CMNI-30

dataset2_CMNI <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2") & (dataset2$redcap_repeat_instrument == ""), c("study_id",cmni_items)]
#Delete Missings
dataset2_CMNI <- na.omit(dataset2_CMNI)
#Create Sum Scores
dataset2_CMNI$CMNI_SUM <- rowSums(dataset2_CMNI[,cmni_items])


# BDI-2 (depression severity might be an important confounder)

dataset2_bdi <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2") & (dataset2$redcap_repeat_instrument == "") ,
                         c("study_id", 
                           bdi_items)]

#Delete Missings
dataset2_bdi <- na.omit(dataset2_bdi)
#Create Sum Scores
dataset2_bdi$BDI_SUM <- rowSums(dataset2_bdi[,bdi_items])

#Depression Severity: 1 = minimal ; 2 = mild ; 3 = moderate ; 4 = severe
dataset2_bdi$dep_sev <- ifelse(dataset2_bdi$BDI_SUM < 13, 1, ifelse(dataset2_bdi$BDI_SUM > 12 & dataset2_bdi$BDI_SUM < 20, 2, ifelse(dataset2_bdi$BDI_SUM > 19 & dataset2_bdi$BDI_SUM < 29, 3, 4)))

# merge all datasets
dataset2_final <- merge(dataset2_Agg, dataset2_CMNI, by = "study_id")
dataset2_final <- merge(dataset2_final, dataset2_MRNI, by = "study_id")
dataset2_final <- merge(dataset2_final, dataset2_HR, by = "study_id")
dataset2_final<- merge(dataset2_final, dataset2_BP, by = "study_id")
dataset2_final<- merge(dataset2_final, dataset2_T, by = "study_id")
dataset2_final<- merge(dataset2_final, dataset2_bdi, by = "study_id")

dataset2_final$SCID <- 0


# combine both datasets MDD and nondepressed
df_FINAL <- rbind(dataset_final, dataset2_final)

ncol(dataset_final)
ncol(dataset2_final)

min(df_FINAL$testo_testo)
df_FINAL$SCID <- as.factor(df_FINAL$SCID)


#-----------------------------------------------Control Variables---------------------------------------------

dataset_bio <- dataset[(dataset$redcap_event_name == "examination_appoin_arm_1") & (dataset$redcap_repeat_instrument == "") ,
                       c("study_id", 
                         "biol_height", "biol_weight", "biol_grip")]

dataset2_bio <- dataset2[(dataset2$redcap_event_name == "examination_appoin_arm_2") & (dataset2$redcap_repeat_instrument == "") ,
                         c("study_id", 
                           "biol_height", "biol_weight", "biol_grip")]
### I also have to control for smoking status --> It needs to be added to this command later on

#Delete Missings
dataset_bio <- na.omit(dataset_bio)
dataset2_bio <- na.omit(dataset2_bio)

#Merge
df_FINAL_bio <- rbind(dataset_bio, dataset2_bio)
df_FINAL2 <- merge(df_FINAL, df_FINAL_bio, by = "study_id")

df_FINAL <- df_FINAL2

df_FINAL$biol_grip <- as.numeric(df_FINAL$biol_grip)
df_FINAL$biol_height <- as.numeric(df_FINAL$biol_height)
df_FINAL$biol_weight <- as.numeric(df_FINAL$biol_weight)

df_FINAL$BMI <- df_FINAL$biol_weight / df_FINAL$biol_height^2
df_FINAL$bio_index <- df_FINAL$BMI / df_FINAL$biol_grip


#---------------------------- Sociodemographic Information ---------------------------------------

library(readr)
OS2_data <- read_csv("OS2_data.csv")
dataset_soc <- OS2_data

# remove weird columns with an X
dataset_soc <- dplyr::select(dataset_soc, -contains("X"))

dataset_soc  

# rename columns

names(dataset_soc)[names(dataset_soc) == "v_597"] <- "nationality"
names(dataset_soc)[names(dataset_soc) == "v_607"] <- "ethn_back"
names(dataset_soc)[names(dataset_soc) == "v_613"] <- "cult_back"
names(dataset_soc)[names(dataset_soc) == "v_626"] <- "rela_stat"
names(dataset_soc)[names(dataset_soc) == "v_631"] <- "sex_orient"
names(dataset_soc)[names(dataset_soc) == "v_637"] <- "children"
names(dataset_soc)[names(dataset_soc) == "v_659"] <- "education"
names(dataset_soc)[names(dataset_soc) == "v_668"] <- "occupation"
names(dataset_soc)[names(dataset_soc) == "study_code"] <- "study_id"


dataset_soc$nationality <- ifelse(dataset_soc$nationality == 1, "Switzerland", 
                                  ifelse(dataset_soc$nationality == 2, "Germany", 
                                         ifelse(dataset_soc$nationality == 3, "Austria", 
                                                ifelse(dataset_soc$nationality == 4, "Lichtenstein", 
                                                       ifelse(dataset_soc$nationality == 5, "Luxembourg", 
                                                              ifelse(dataset_soc$nationality == 6, "France", 
                                                                     ifelse(dataset_soc$nationality == 7, "Italy", 
                                                                            ifelse(dataset_soc$nationality == 8, "Belgium", "Other"))))))))


dataset_soc$ethn_back <- ifelse(dataset_soc$ethn_back == 1, "American", 
                                ifelse(dataset_soc$ethn_back == 2, "Central / Southern Asian", 
                                       ifelse(dataset_soc$ethn_back == 3, "Eastern Asian", 
                                              ifelse(dataset_soc$ethn_back == 4, "European", 
                                                     ifelse(dataset_soc$ethn_back == 5, "Middle East", 
                                                            ifelse(dataset_soc$ethn_back == 6, "Oceania", 
                                                                   ifelse(dataset_soc$ethn_back == 7, "Subsaharian Africa", 
                                                                          ifelse(dataset_soc$ethn_back == 8, "Afro American", "Latino"))))))))

dataset_soc$cult_back <- ifelse(dataset_soc$cult_back == 1, "Western / European", 
                                ifelse(dataset_soc$cult_back == 2, "African", 
                                       ifelse(dataset_soc$cult_back == 3, "Asian", 
                                              ifelse(dataset_soc$cult_back == 4, "Arabic", 
                                                     ifelse(dataset_soc$cult_back == 5, "Latin American", 
                                                            ifelse(dataset_soc$cult_back == 6, "Mixed", "Other"))))))

dataset_soc$rela_stat <- ifelse(dataset_soc$rela_stat == 1, "Relationship", 
                                ifelse(dataset_soc$rela_stat == 2, "Single", 
                                       ifelse(dataset_soc$rela_stat == 3, "Relationship","Single")))

dataset_soc$sex_orient <- ifelse(dataset_soc$sex_orient == 1, "heterosexual", "LGBT+")

dataset_soc$children <- ifelse(dataset_soc$children == 1, "yes","no")

dataset_soc$education <- ifelse(dataset_soc$education == 1, "no completed schooling", 
                                ifelse(dataset_soc$education == 9, "Other",
                                       ifelse(dataset_soc$education == 8, "Tertiary eductaion", "Secondary education")))

dataset_soc$occupation <- ifelse(dataset_soc$occupation == 1, "fulltime job ", 
                                 ifelse(dataset_soc$occupation == 2, "parttime job", 
                                        ifelse(dataset_soc$occupation == 3, "parttime job", 
                                               ifelse(dataset_soc$occupation == 4, "unemployed", 
                                                      ifelse(dataset_soc$occupation == 5, "Disability Insurance Recipient", 
                                                             ifelse(dataset_soc$occupation == 7, "Early retirement", 
                                                                    ifelse(dataset_soc$occupation == 7, "Student / in training", 
                                                                           ifelse(dataset_soc$occupation == 8, "Parental leave / housework", "Other"))))))))



df_FINAL2 <- merge(df_FINAL, dataset_soc, by = "study_id")

df_FINAL2$dep_sev <- ifelse(df_FINAL2$dep_sev == 1, "minimal / mild", 
                            ifelse(df_FINAL2$dep_sev == 2, "minimal / mild", 
                                   ifelse(df_FINAL2$dep_sev == 3, "moderate /severe", 
                                          ifelse(df_FINAL2$dep_sev == 4, "moderate /severe", "healthy"))))



df_FINAL<- df_FINAL2


#------------------------Covariates-----------------------------------------------

dataset_cov <- dataset_soc[,c("study_id", "rela_stat", "sex_orient", "children", "education")]

dataset_cov$rela_dummy <- ifelse(dataset_cov$rela_stat == "Single", 0, 1)

dataset_cov$sex_dummy <- ifelse(dataset_cov$sex_orient == "heterosexual", 1, 0)

dataset_cov$children_dummy <- ifelse(dataset_soc$children == "yes", 1, 0)

dataset_cov$education_dummy <- ifelse(dataset_soc$education == "Tertiary Education", 1, 0)

dataset_cov2 <- dataset_cov[, c("study_id", "rela_dummy", "sex_dummy","children_dummy", "education_dummy")]

df_FINAL_cov <- merge(df_FINAL, dataset_cov2, by = "study_id")


#-----------------------------------------------Reverse Coded Items---------------------------------------------

# reverse code items of CMNI_30
df_FINAL_cov$cmni30_1 <- 6 - df_FINAL_cov$cmni30_1
df_FINAL_cov$cmni30_3 <- 6 - df_FINAL_cov$cmni30_3
df_FINAL_cov$cmni30_5 <- 6 - df_FINAL_cov$cmni30_5
df_FINAL_cov$cmni30_9 <- 6 - df_FINAL_cov$cmni30_9
df_FINAL_cov$cmni30_13 <-6 - df_FINAL_cov$cmni30_13
df_FINAL_cov$cmni30_15 <- 6 - df_FINAL_cov$cmni30_15
df_FINAL_cov$cmni30_18 <- 6 - df_FINAL_cov$cmni30_18
df_FINAL_cov$cmni30_22 <- 6 - df_FINAL_cov$cmni30_22
df_FINAL_cov$cmni30_28 <- 6 - df_FINAL_cov$cmni30_28

df_FINAL_cov$CMNI_SUM <- rowSums(df_FINAL_cov[,cmni_items])

# reverse coded items of afbp
df_FINAL_cov$af_bp_14 <- 4 - df_FINAL_cov$af_bp_14
df_FINAL_cov$af_bp_22 <- 4 - df_FINAL_cov$af_bp_22


#------------------------Subscales-------------------------------------------

df_FINAL3 <- df_FINAL_cov 

# rows to remove (the ones which occur at least twice)
rows_to_remove <- c(7:14, 17:19, 21:23, 32:34, 40:42, 45:47, 50:52, 61:63, 65:67, 82:84, 103:105, 112:114, 119:121)
df_FINAL4 <- df_FINAL3[-rows_to_remove, ]


#CMNI-30

df_final_subs <- df_FINAL4

df_final_subs$EmC = rowSums(df_final_subs[, c("cmni30_1","cmni30_13","cmni30_5")])
df_final_subs$Het = rowSums(df_final_subs[, c("cmni30_2","cmni30_11","cmni30_17")])
df_final_subs$Viol  = rowSums(df_final_subs[, c("cmni30_3","cmni30_15","cmni30_24")])
df_final_subs$Self  = rowSums(df_final_subs[, c("cmni30_4","cmni30_25","cmni30_28")])
df_final_subs$Work = rowSums(df_final_subs[, c("cmni30_6","cmni30_21","cmni30_26")])
df_final_subs$Win = rowSums(df_final_subs[, c("cmni30_7","cmni30_16","cmni30_23")])
df_final_subs$Risk = rowSums(df_final_subs[, c("cmni30_8","cmni30_19","cmni30_30")])
df_final_subs$Status = rowSums(df_final_subs[, c("cmni30_9","cmni30_18","cmni30_22")])
df_final_subs$Women = rowSums(df_final_subs[, c("cmni30_10","cmni30_20","cmni30_27")])
df_final_subs$Play = rowSums(df_final_subs[, c("cmni30_12","cmni30_14","cmni30_29")])

df_FINAL4 <- df_final_subs

df_FINAL4$CMNI_SUM <- rowSums(df_FINAL4[,cmni_items])

data_all <- df_FINAL4


