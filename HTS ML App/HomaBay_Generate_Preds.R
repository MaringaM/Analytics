library(caret)
library(dplyr)
library(ROCR)
library(PRROC)
library(randomForest)
library(lubridate)


hts <- read.csv('C:/Users/Admin/OneDrive/R Work/Homabay App/HomaBayPreds/Rangwe.csv', stringsAsFactors = FALSE)

# Filter to Positive and Negative (what about inconclusive?)
hts <- hts %>%
  filter(FinalTestResult %in% c("Positive", "Negative")) %>%
  mutate(FinalTestResult = ifelse(FinalTestResult == 'Positive', 'Positive', 'Negative'))

# Per Kenya team's guidance, let's limit to initial tests.
hts <- hts %>%
  filter(TestType %in% c('Initial', 'Initial Test'))

# Remove observations with unlikely DOB
hts$DOB <- ymd(hts$DOB)
hts <- hts[hts$DOB > '1920-01-01', ]

# convert to date
hts$TestDate <- ymd(hts$TestDate)
# Drop test dates that are erroneously in the future
hts <- hts[hts$TestDate < '2021-03-12', ]

# drop tests prior to 2019 for data quality reasons
hts <- hts[hts$TestDate >= '2017-01-01', ]

# limit to observations where DOB is before or same as test date
hts <- hts[hts$DOB <= hts$TestDate, ]

# Calculate age at time of test date in term sof years
hts$AgeAtTest <- floor((hts$TestDate - hts$DOB)/365)

# Get Month and Day of Week
hts$month_of_test <- month(hts$TestDate)
hts$dayofweek <- wday(hts$TestDate)

# Group Key Population type
hts$KeyPopulationType <- tolower(hts$KeyPopType)
hts <- hts %>%
  mutate(KeyPopulationType = ifelse(grepl("msm", KeyPopulationType) | grepl("men", KeyPopulationType), "MSM",
                                    ifelse(grepl("sw", KeyPopulationType) | grepl("sex", KeyPopulationType), "SW",
                                           ifelse(grepl("pwid", KeyPopulationType) | grepl("drugs", KeyPopulationType), "PWID",
                                                  ifelse(KeyPopulationType %in% c("", " ", "n/a") | is.na(KeyPopulationType), "GP", "OtherKP")))))

# Group Marital Status
hts$MaritalStatus <- tolower(hts$MaritalStatus)
hts <- hts %>%
  mutate(MaritalStatus = ifelse(grepl("married", MaritalStatus) | grepl("cohabit", MaritalStatus) | grepl("partner", MaritalStatus), "Married",
                                ifelse((grepl("single", MaritalStatus) | grepl("never", MaritalStatus)) & AgeAtTest >= 15, "Single",
                                       ifelse((grepl("single|never|unknown", MaritalStatus) | MaritalStatus == "") & AgeAtTest < 15, "Minor",
                                              ifelse(grepl("divorced", MaritalStatus) | grepl("separated", MaritalStatus), "Divorced",
                                                     ifelse(grepl("widow", MaritalStatus), "Widowed",
                                                            ifelse(grepl("poly", MaritalStatus), "Polygamous", "Unknown")))))))
# Group patient disabled
hts <- hts %>%
  mutate(PatientDisabled = ifelse(PatientDisabled %in% c('<NA>', '', 'No', 'C: Couple (includes polygamous)', 'I: Individual'), 'Not Disabled', 'Disabled'))

# Group Ever Tested
hts <- hts %>%
  filter(EverTestedForHIV %in% c('No', 'Yes'))

# Calculate Months since last test
hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)

# If months since last test is negative of more than 10 years ago, drop
hts <- hts %>% filter((MonthsSinceLastTest>=0 & MonthsSinceLastTest <= 120) | is.na(MonthsSinceLastTest))

# Set months to zero for never tested
hts <- hts %>%
  mutate(MonthsSinceLastTest = ifelse(EverTestedForHIV == 'No', 0, MonthsSinceLastTest))

# Set months to average for tested with missing months value
MSLT_AVG <- round(mean(filter(hts, EverTestedForHIV == 'Yes')$MonthsSinceLastTest, na.rm = T))
hts <- hts %>%
  mutate(MonthsSinceLastTest = ifelse(EverTestedForHIV == 'Yes' & is.na(MonthsSinceLastTest), MSLT_AVG, MonthsSinceLastTest))

# Group into couple, individual, other
hts$ClientTestedAs <- tolower(hts$ClientTestedAs)
hts <- hts %>%
  mutate(ClientTestedAs = ifelse(grepl('couple', ClientTestedAs), 'Couple', 'Individual'))

# # EntryPoint
hts$EntryPoint <- tolower(hts$EntryPoint)
hts <- hts %>%
  mutate(EntryPoint = ifelse(grepl('ccc|comprehensive', EntryPoint), 'CCC',
                             ifelse(grepl('pmtct|pnc|mch|maternity|anc|mother|cwc', EntryPoint), 'MTC',
                                    ifelse(grepl('home|hbtc', EntryPoint), "HB",
                                           ifelse(grepl('inpatient|ipd', EntryPoint), "IPD",
                                                  ifelse(grepl('outpatient|opd|pitc', EntryPoint), "OPD",
                                                         ifelse(grepl('mobile|outreach', EntryPoint), "MOBILE",
                                                                ifelse(grepl('ped|peadiatric', EntryPoint), "PEDS",
                                                                       ifelse(grepl('tb', EntryPoint), "TB",
                                                                              ifelse(grepl('vct|voluntary', EntryPoint), "VCT",
                                                                                     ifelse(grepl('vmmc', EntryPoint), "VMMC", "Other")))))))))))

#Grou Testing Strategy
hts$TestingStrategy <- tolower(hts$TestingStrategy)
hts <- hts %>%
  mutate(TestingStrategy = ifelse(grepl('hb|home based', TestingStrategy), "HB",
                                  ifelse(grepl('ipd|hp|opd|mch|maternity', TestingStrategy), "HP",
                                         ifelse(grepl('vct|vi|vs|non provider', TestingStrategy), "VCT",
                                                ifelse(grepl('np', TestingStrategy), "NP",
                                                       ifelse(grepl('mobile', TestingStrategy), "MOBILE",
                                                              ifelse(grepl('pitc', TestingStrategy), "PITC",
                                                                     ifelse(grepl('pns', TestingStrategy), "PNS", "Other"))))))))

# Group TB Screening
hts$TBScreening <- tolower(hts$TBScreening)
table(hts$TBScreening, useNA = 'always')

# group as not done, no signs, and done
hts <- hts %>%
  filter(TBScreening != 'not done') %>%
  mutate(TBScreening = ifelse(grepl('yes|tb rx|tbrx|tb confirmed|prtb|presumed tb|on tb treatment', TBScreening), "Presumed TB", "No Presumed TB"))

# if not asked, set to No, which is dominant class
hts <- hts %>%
  mutate(ClientSelfTested = ifelse(ClientSelfTested %in% c('1', 'Yes'), 'Yes', 'No'))

# Select Variables
hts <- hts %>%
  select(FinalTestResult, AgeAtTest, KeyPopulationType, MaritalStatus, Gender, PatientDisabled, EverTestedForHIV,
         MonthsSinceLastTest, ClientTestedAs, EntryPoint, TestingStrategy, TBScreening, ClientSelfTested,
         Sitecode, County, month_of_test, dayofweek, FacilityName)

# Read in Local Demographic and HIV data
facilities <- readRDS('C:/Users/Admin/OneDrive/R Work/Homabay App/HomaBayPreds/facilities_homabay_5km.rds') %>%
  filter(!is.na(hiv_prev)) 

facilities[, 23:36] <- facilities[, 23:36] / facilities[, 22]

fac_pca <- facilities %>% select(4:36, 38:91)
no_var <- apply(fac_pca, 2, var)
fac_pca <- fac_pca[, no_var != 0]
fac_pca <- prcomp(fac_pca, center = TRUE,scale. = TRUE)
summary(fac_pca) # three components capture all the variance
fac_pca_out <- fac_pca$x[, 1:10]

facilities <- cbind.data.frame(facilities[, 1:3], fac_pca_out)
facilities <- facilities %>%
  mutate(FacilityType = ifelse(grepl('Hospital', Facility.Name), "Hospital",
                               ifelse(grepl("Dispensary", Facility.Name), "Dispensary", "Health Center")))

hts <- merge(facilities, hts, by.x = "Facility.Name", by.y = "FacilityName")

# Key Population Type - group into
kp_other <- names(which(prop.table(table(hts$KeyPopulationType, useNA='always')) < .01))
hts$KeyPopulationType <- ifelse(hts$KeyPopulationType %in% kp_other, 'OtherKP', hts$KeyPopulationType)

# If any testing strategy has <1% of observations, group it in Other
ts_other <- names(which(prop.table(table(hts$TestingStrategy, useNA = 'always')) < .01))
hts$TestingStrategy <- ifelse(hts$TestingStrategy %in% ts_other, 'Other', hts$TestingStrategy)

hts$AgeAtTest <- as.numeric(hts$AgeAtTest)
hts$FinalTestResult <- factor(hts$FinalTestResult, levels = c("Positive", "Negative"))
hts$KeyPopulationType <- factor(hts$KeyPopulationType, levels = c("GP", "OtherKP", "SW"))
hts$MaritalStatus <- factor(hts$MaritalStatus, levels = c("Unknown","Married","Polygamous", 
                                                          "Widowed", "Divorced", "Minor"))
hts$Gender <- factor(hts$Gender, levels = c("Female", "Male"))
hts$PatientDisabled <- factor(hts$PatientDisabled, levels = c("Not Disabled", "Disabled"))
hts$EverTestedForHIV <- factor(hts$EverTestedForHIV, levels = c("Yes", "No"))
hts$ClientTestedAs <- factor(hts$ClientTestedAs, levels = c("Individual", "Couple"))
hts$EntryPoint <- factor(hts$EntryPoint, levels = c("MOBILE", "OPD", "VCT", "Other",
                                                    "HB", "PEDS", "MTC", "IPD"))
hts$TestingStrategy <- factor(hts$TestingStrategy, levels = c("MOBILE", "VCT", "HB", "Other"))
hts$TBScreening <- factor(hts$TBScreening, levels = c("No Presumed TB", "Presumed TB"))
hts$ClientSelfTested <- factor(hts$ClientSelfTested, levels = c("Yes", "No"))
hts$FacilityType <- factor(hts$FacilityType, levels = c("Dispensary", "Hospital", "Health Center"))
# hts$Sitecode <- factor(hts$Sitecode)
hts$month_of_test <- factor(hts$month_of_test, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))
hts$dayofweek <- factor(hts$dayofweek, levels = c('1', '2', '3', '4', '5', '6', '7'))

hts <- hts %>% select(-c(Facility.Name, Longitude, Latitude, PatientDisabled, EntryPoint))

# Read in saved model
rf <- readRDS('C:/Users/Admin/OneDrive/R Work/Homabay App/HomaBayPreds/rf_homabay_20210315.rds')

# Generate Predictions
pred_val = predict(rf, newdata=hts[, names(hts) != 'Sitecode'], type = "prob")

fg <- pred_val[hts$FinalTestResult == "Positive", 1]
bg <- pred_val[hts$FinalTestResult == "Negative", 1]

prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc)

# Output Predictions
hts_out <- cbind(pred_val, hts)
write.csv(hts_out, 'C:/Users/Admin/OneDrive/R Work/Homabay App/HomaBayPreds/rangwe_preds_MM.csv')
