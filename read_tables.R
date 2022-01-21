climate_change_indicators <- read.table("datasets/climate-change-indicators-for-united-states-1.csv",
                                        sep = ',',
                                        header = TRUE)
names(climate_change_indicators)
summary(climate_change_indicators)
head(climate_change_indicators)
nrow(climate_change_indicators)
numerical_climate_change_indicators <- data.frame(climate_change_indicators$Year,
                                                  climate_change_indicators$Value)
cor(numerical_climate_change_indicators)
summary(lm(numerical_climate_change_indicators))

financial_sector_indicators <- read.table("datasets/financial-sector-indicators-for-united-states-1.csv",
                                          sep = ',',
                                          header = TRUE)
names(financial_sector_indicators)
summary(financial_sector_indicators)
head(financial_sector_indicators)
nrow(financial_sector_indicators)

education_indicators_indicators <- read.table("datasets/education-indicators-for-united-states-1.csv",
                                              sep = ',',
                                              header = TRUE)
names(education_indicators_indicators)
summary(education_indicators_indicators)
head(education_indicators_indicators)
nrow(education_indicators_indicators)

airbnb <- read.csv("/Users/vidya/vk-workspace/Spring 2021/235A/Project/datasets/AB_NYC_2019.csv",
                       sep =',',
                       header = TRUE,
                       fill = TRUE,)
summary(airbnb)
max(airbnb$price, na.rm = TRUE)
head(as.numeric(airbnb$minimum_nights), na.rm = TRUE)

temp_change_indicators <- read.table("datasets/Environment_Temperature_change_E_All_Data_NOFLAG.csv",
                                      sep = ',',
                                      header = TRUE,
                                      fill = TRUE)
names(temp_change_indicators)
summary(temp_change_indicators)
head(temp_change_indicators)
nrow(temp_change_indicators)
temp_change_df <- data.frame(Y2015 = temp_change_indicators$Y2015,
                             Y2016 = temp_change_indicators$Y2016,
                             Y2017 = temp_change_indicators$Y2017,
                             Y2018 = temp_change_indicators$Y2018,
                             Y2019 = temp_change_indicators$Y2019)
summary(temp_change_df)
cor(temp_change_df, use = "complete.obs")
lm(temp_change_df)
summary(lm(temp_change_df))
plot()

airbnb <- read.csv("/Users/vidya/vk-workspace/Spring 2021/235A/Project/datasets/AB_NYC_2019.csv",
                   sep =',',
                   header = TRUE,
                   fill = TRUE,)
summary(airbnb)
nrow(airbnb)
ncol(airbnb)
names(airbnb)
max(airbnb$price, na.rm = TRUE)
head(as.numeric(airbnb$minimum_nights), na.rm = TRUE)
airbnb_num <- data.frame(airbnb$price, airbnb$minimum_nights, airbnb$availability_365)
head(airbnb_num)
pairs(airbnb_num)
pairs.panels(airbnb_num, col = "red", hist.col = "blue")
cor(airbnb_num)


califire <- read.csv("/Users/vidya/vk-workspace/Spring 2021/235A/Project/datasets/California_Fire_Incidents.csv",
                   sep =',',
                   header = TRUE,
                   fill = TRUE,)
summary(califire)
names(califire)
nrow(califire)
ncol(califire)


global.superstore <- read.csv("/Users/vidya/vk-workspace/Spring 2021/235A/Project/datasets/Global_Superstore2.csv",
                              header = TRUE,
                              fill = TRUE)
summary(global.superstore)
names(global.superstore)
nrow(global.superstore)
ncol(global.superstore)
length(unique(global.superstore$Product.Name))
global.superstore.num <- data.frame(global.superstore$Sales,
                                    global.superstore$Quantity,
                                    global.superstore$Profit,
                                    global.superstore$Shipping.Cost,
                                    global.superstore$Discount)
pairs(global.superstore.num)
cor(global.superstore.num)
library(psych)
pairs(global.superstore.num, col = "red")
pairs.panels(global.superstore.num, col = "red", hist.col = "blue")
summary(lm(global.superstore.num))

sapply(global.superstore, class)

heart_failure <- read.csv("datasets/heart_failure_clinical_records_dataset.csv")
summary(heart_failure)
names(heart_failure)
#  [1] "age"                      "anaemia"                  "creatinine_phosphokinase" "diabetes"                
# [5] "ejection_fraction"        "high_blood_pressure"      "platelets"                "serum_creatinine"        
# [9] "serum_sodium"             "sex"                      "smoking"                  "time"                    
# [13] "DEATH_EVENT"
nrow(heart_failure)
ncol(heart_failure)
pairs.panels(heart_failure, col = "red", hist.col = "blue")
pairs(heart_failure)

summary(lm(rep(DEATH_EVENT,2) ~ c(age, smoking), data= heart_failure))$r.squared
summary(lm(rep(DEATH_EVENT,2) ~ c(anaemia, time), data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ creatinine_phosphokinase, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ diabetes, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ ejection_fraction, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ high_blood_pressure, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ platelets, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ serum_creatinine, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ serum_sodium, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ sex, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ smoking, data= heart_failure))$r.squared
summary(lm(DEATH_EVENT ~ time, data= heart_failure))$r.squared
cor(heart_failure)

student_alchohol <- read.csv("datasets/student alchohol/student-mat.csv")
summary(student_alchohol)
names(student_alchohol)
# [1] "school"     "sex"        "age"        "address"    "famsize"    "Pstatus"    "Medu"       "Fedu"      
# [9] "Mjob"       "Fjob"       "reason"     "guardian"   "traveltime" "studytime"  "failures"   "schoolsup" 
# [17] "famsup"     "paid"       "activities" "nursery"    "higher"     "internet"   "romantic"   "famrel"    
# [25] "freetime"   "goout"      "Dalc"       "Walc"       "health"     "absences"   "G1"         "G2"        
# [33] "G3"
nrow(student_alchohol)
ncol(student_alchohol)
pairs.panels(student_alchohol, col = "red", hist.col = "blue")
student_alchohol.num <- data.frame(student_alchohol$age, 
                          student_alchohol$Medu, student_alchohol$Fedu, 
                          student_alchohol$traveltime, student_alchohol$studytime,
                          student_alchohol$failures, student_alchohol$famrel,
                          student_alchohol$freetime, student_alchohol$goout,
                          student_alchohol$Dalc, student_alchohol$Walc,
                          student_alchohol$health, student_alchohol$absences,
                          student_alchohol$G1, student_alchohol$G2, student_alchohol$G3)

cor(student_alchohol.num)
summary(student_alchohol.num)

grad_admission <- read.csv("datasets/grad admission/Admission_Predict.csv")
cor(grad_admission)
summary(grad_admission)
sapply(grad_admission, class)
names(grad_admission)

sapply(grad_admission, summary)

grad_admission$University.Rating[grad_admission$GRE.Score < 305 & grad_admission$University.Rating %in% c(4,5)]
grad_admission[grad_admission$GRE.Score < 305 & grad_admission$University.Rating %in% c(4,5),]
options(digits = 2)
