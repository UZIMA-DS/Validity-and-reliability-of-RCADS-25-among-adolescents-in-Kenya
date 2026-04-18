#-------------------------------------------------------------------------------------------------------------------
#Validity and reliability of the Swahili version of the 25-item Revised Child Anxiety and Depression Scale in Kenya
#-------------------------------------------------------------------------------------------------------------------

#Section 2

#Install required packages
install.packages("readxl")  # To read Excel files
install.packages("dplyr")    # For data manipulation
install.packages("psych")    # For psychometric analysis

#Load packages
library(readxl)
library(dplyr)
library(psych)

#Setting working directory
setwd("C:/Users/Admin/Desktop/RCADS MANUSCRIPT/UZIMA ECD _ dataset/RCADS DATA SET")

## Read your data files
rcads_data <- read_excel("KEMRI ECD DATA.xlsx")   #contains RCADS-25 and WHOQOL data
gad_phq_data <-read.csv("Ecd_Acasi_data_set.csv") #Contains GAD-7 and PHQ-9 data

###View the data
head(rcads_data)
head(gad_phq_data)


#-------------------------------------------------------------------------------
#SELECT AND CLEAN RCADS DATA FROM EXCEL FILE
rcads_clean <- rcads_data %>%
  filter(!if_any(rcads_1:rcads_25, ~ is.na(.) | . == "")) %>%
  mutate(
    anxiety_score    = rowSums(across(paste0("rcads_", c(2,3,5,6,7,9,11,12,14,17,18,20,22,23,25))), na.rm = TRUE),
    depression_score = rowSums(across(paste0("rcads_", c(1,4,8,10,13,15,16,19,21,24))), na.rm = TRUE),
    total_score      = rowSums(across(rcads_1:rcads_25), na.rm = TRUE)
  )


#SELECT, CLEAN AND RECODE PHQ DATA FROM FILE
phq9_clean <- gad_phq_data %>%
  select(study_number, X_65:X_73) %>%
  mutate(across(X_65:X_73, ~ na_if(., ""))) %>%       # blanks → NA
  mutate(across(X_65:X_73, ~ na_if(., "..."))) %>%    # "..." → NA
  mutate(across(X_65:X_73, ~ recode(.,
                                    "A" = 0, "B" = 1,
                                    "C" = 2, "D" = 3,
                                    "a" = 0, "b" = 1,
                                    "c" = 2, "d" = 3,
                                    .default = NA_real_))) %>%
  filter(if_any(X_65:X_73, ~ !is.na(.))) %>%          
  mutate(phq9 = rowSums(across(X_65:X_73), na.rm = TRUE))


#SELECT, CLEAN AND RECODE GAD-7 DATA FROM FILE
gad7_clean <- gad_phq_data %>%
  select(study_number, X_75:X_81) %>%
  mutate(across(X_75:X_81, ~ na_if(., ""))) %>%       # blanks → NA
  mutate(across(X_75:X_81, ~ na_if(., "..."))) %>%    # "..." → NA
  mutate(across(X_75:X_81, ~ recode(.,
                                    "A" = 0, "B" = 1,
                                    "C" = 2, "D" = 3,
                                    "a" = 0, "b" = 1,
                                    "c" = 2, "d" = 3,
                                    .default = NA_real_))) %>%
  filter(if_any(X_75:X_81, ~ !is.na(.))) %>%          
  mutate(gad7 = rowSums(across(X_75:X_81), na.rm = TRUE))



#Check duplicates in data set
rcads_dupes <- rcads_clean %>% filter(duplicated(study_number))
phq9_dupes  <- phq9_clean  %>% filter(duplicated(study_number))
gad7_dupes  <- gad7_clean  %>% filter(duplicated(study_number))

nrow(rcads_dupes)
nrow(phq9_dupes)
nrow(gad7_dupes)


#Remove duplicates by merging
rcads_clean <- rcads_clean %>% distinct(study_number, .keep_all = TRUE)
phq9_clean  <- phq9_clean  %>% distinct(study_number, .keep_all = TRUE)
gad7_clean  <- gad7_clean  %>% distinct(study_number, .keep_all = TRUE)


#Merging data
merged_data <- rcads_clean %>%
  select(study_number, anxiety_score, depression_score, total_score) %>%
  inner_join(phq9_clean %>% select(study_number, phq9), by = "study_number") %>%
  inner_join(gad7_clean %>% select(study_number, gad7), by = "study_number")


#check merged data
dim(merged_data)        # number of rows and columns
head(merged_data)       # preview first few rows
summary(merged_data)    # quick descriptive stats


#-------------------------------------------------------------------------------
#Convergent validity - Spearman's correlation
cor.test(merged_data$anxiety_score, merged_data$gad7, method = "spearman")
cor.test(merged_data$depression_score, merged_data$phq9, method = "spearman")
cor.test(merged_data$total_score, merged_data$gad7, method = "spearman")
cor.test(merged_data$total_score, merged_data$phq9, method = "spearman")
cor.test(merged_data$anxiety_score, merged_data$phq9, method = "spearman")
cor.test(merged_data$depression_score, merged_data$gad7, method = "spearman")


#Linear regression
model_phq9 <- lm(scale(phq9) ~ scale(total_score), data = merged_data)
summary(model_phq9)
model_gad7 <- lm(scale(gad7) ~ scale(total_score), data = merged_data)
summary(model_gad7)


#------------------------------------------------------------------------------
#Clean WHOQOL items and compute total
rcads_clean <- rcads_clean %>%
  mutate(across(whoqol1:whoqol26, as.character)) %>%       # force to character
  mutate(across(whoqol1:whoqol26, ~ na_if(., ""))) %>%     # blanks → NA
  mutate(across(whoqol1:whoqol26, ~ na_if(., "..."))) %>%  # odd entries → NA
  mutate(across(whoqol1:whoqol26, as.numeric)) %>%         # back to numeric
  mutate(whoqol_total = rowSums(across(whoqol1:whoqol26), na.rm = TRUE))

#Divergent validity - Spearman's correlation
#Overall scale
cor.test(rcads_clean$total_score, rcads_clean$whoqol_total, method = "spearman")

# Anxiety vs WHOQOL total
cor.test(rcads_clean$anxiety_score, rcads_clean$whoqol_total, method = "spearman")

# Depression vs WHOQOL total
cor.test(rcads_clean$depression_score, rcads_clean$whoqol_total, method = "spearman")





