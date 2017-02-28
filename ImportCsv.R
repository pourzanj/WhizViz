library(readr)
library(tidyr)
library(dplyr)
library(boot)
library(lubridate)
library(purrr)

#
Clinical <- read_csv("~/Alzheimers/data/Clinical.csv")
names(Clinical)[1] <- "HMRI"
Clinical <- Clinical %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>% 
  #lm(log(`B Amyloid Level pg/ml (Abby)`)~log(`CSF A_42 (pg/mL) (MSD 6E10) (Zlokovic lab)`), data = General)
  #lm(log(`Total Tau Level pg/ml (Abby)`)~log(`CSF Total tau (pg/mL) (MSD) (Zlokovic lab)`), data = General)
  mutate(EstimatedAbAbby = exp(log(`CSF A_42 (pg/mL) (MSD 6E10) (Zlokovic lab)`)*0.7482+2.5525)) %>%
  mutate(EstimatedTauAbby = exp(log(`CSF Total tau (pg/mL) (MSD) (Zlokovic lab)`)*0.73608+1.57221)) %>%
  mutate(EstimatedAbAbby = ifelse(is.na(`B Amyloid Level pg/ml (Abby)`), EstimatedAbAbby, `B Amyloid Level pg/ml (Abby)`)) %>%
  mutate(EstimatedTauAbby = ifelse(is.na(`Total Tau Level pg/ml (Abby)`), EstimatedTauAbby, `Total Tau Level pg/ml (Abby)`)) %>%
  mutate(EstimatedPat = EstimatedAbAbby <= EstimatedTauAbby*3.41 - 185.28) %>%
  mutate(EstimatedPat = ifelse(EstimatedPat, "PAT", "NAT")) %>%
  mutate(EstimatedPat = as.factor(EstimatedPat)) %>%
  mutate(EstimatedClassification = paste(`Clinical Dx`,EstimatedPat)) %>%
  mutate(EstimatedClassification = factor(EstimatedClassification)) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

#
NeuroPsych <- read_csv("~/Alzheimers/data/NeuroPsych.csv")
names(NeuroPsych)[1] <- "ID"
NeuroPsych <- NeuroPsych %>%
  separate(ID, c("Code", "VisitNumber"), sep = "-") %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

#ensure there are no patients who aren't in the clinical df
select(NeuroPsych, Code, VisitNumber) %>% anti_join(select(Clinical, Code))

#
UrineNormalizers <- read_csv("~/Alzheimers/data/UrineNormalizers.csv")
names(UrineNormalizers)[1:2] <- c("Classification", "HMRI")
UrineNormalizers <- UrineNormalizers[2:nrow(UrineNormalizers),]
UrineNormalizers <- UrineNormalizers %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-") %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

select(UrineNormalizers, Code, VisitNumber) %>% anti_join(select(Clinical, Code))

#
#label all 12 as 
LhVolume12 <- read_csv("~/Alzheimers/data/Volumetrics/LhVolume12.csv")
#names(LhVolume12) <- paste(names(LhVolume12), "2010-2012")
names(LhVolume12)[1] <- "Code"
LhVolume12 <- LhVolume12 %>% mutate(Code = as.integer(Code), VisitNumber = 1L)

RhVolume12 <- read_csv("~/Alzheimers/data/Volumetrics/RhVolume12.csv")
names(RhVolume12)[1] <- "Code"
RhVolume12 <- RhVolume12 %>% mutate(Code = as.integer(Code), VisitNumber = 1L)

Cont12 <- read_csv("~/Alzheimers/data/Volumetrics/Cont12.csv")
names(Cont12)[1] <- "Code"
Cont12 <- Cont12 %>% mutate(Code = as.integer(Code), VisitNumber = 1L) %>%
  #classification is included in cont12 but not cont14 and is therefore being removed so they can be binded
  select(-Classification)

#random code to compare date fluids taken to date MRI was taken  
# select(Clinical, Code, VisitNumber, contains("date")) %>%
#   right_join(select(LhVolume12, Code, `MRI Date 2010-2012`)) %>%
#   mutate_at(vars(`Date of Fluids`, `MRI Date 2010-2012`), mdy) %>%
#   mutate(DaysBetweenFluidsAndMri = `MRI Date 2010-2012` - `Date of Fluids`) %>% ggplot() + geom_histogram(aes(DaysBetweenFluidsAndMri))

LhVolume14 <- read_csv("~/Alzheimers/data/Volumetrics/LhVolume14.csv")
names(LhVolume14)[1] <- "Code"
LhVolume14 <- LhVolume14 %>% mutate(Code = as.integer(Code), VisitNumber = 2L)

RhVolume14 <- read_csv("~/Alzheimers/data/Volumetrics/RhVolume14.csv")
names(RhVolume14)[1] <- "Code"
RhVolume14 <- RhVolume14 %>% mutate(Code = as.integer(Code), VisitNumber = 2L)

Cont14 <- read_csv("~/Alzheimers/data/Volumetrics/Cont14.csv")
names(Cont14)[1] <- "Code"
Cont14 <- Cont14 %>% mutate(Code = as.integer(Code), VisitNumber = 2L)

#shouldn't do the full join causes naming problems
Volumetrics12 <- LhVolume12 %>%
  full_join(RhVolume12) %>%
  full_join(Cont12) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

Volumetrics14 <- LhVolume14 %>%
  full_join(RhVolume14) %>%
  full_join(Cont14) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

Volumetrics <- rbind(Volumetrics12, Volumetrics14)

#anti- join to make sure there are no people with volumetrics who aren't in the Master list
#select(Volumetrics, Code, VisitNumber) %>% anti_join(select(Clinical, Code, VisitNumber))

#WhizViz will join a df of X-axis variables and a df Y-axis variables to the master frame.
#master frame contains meta information such as Age and Dx that are important for coloring
#and faceting, and also for examining the hover textbox.
#Meta informtion includes info from Clinical, UrineNormalizers, Neuropsych, and Volumetrics.
#Meta information selected from those dataframes for the Master frame should subsequently be
#removed from that original frame, because otherwise when Whizviz rejoins that frame to Master
#there will be an annoying naming conflict.

MasterColumnsClinical <- c("EstimatedClassification", "EstimatedAbAbby", "EstimatedTauAbby",
                           "CSF A_42 (pg/mL) (MSD 6E10) (Zlokovic lab)",
                           "CSF Total tau (pg/mL) (MSD) (Zlokovic lab)", "B Amyloid Level pg/ml (Abby)",
                           "Total Tau Level pg/ml (Abby)", "Clinical Dx", "Sex", "Age at Sample", "Education",
                           "BMI", "Hypertension", "Traumatic Brain Injury", "Other Medical Conditions", "ApoE",
                           "Meds and Supplements")

MasterColumnsUrineNormalizers <- c("Creatinine Adjusted Final Con. (ug/mL)", "Total Protein  (TPA) ug/ml",
                                   "Albumin (ug/mL)", "UACR (mg/g) ALB/CRN")

MasterColumnsNeuroPsych <- c("STROOP INTERFERENCE RAW", "STROOP INTERFERENCE Z-SCORE")

#we have to get Dx from UrineNormalizers, because the Dx from CLinical does
#not include the PAT and NAT specification
MasterList <- Clinical %>%
  left_join(UrineNormalizers, by = c("Code", "VisitNumber")) %>%
  left_join(NeuroPsych, by = c("Code", "VisitNumber")) %>%
  #change names specifically so that they don't match names of columns of Clinical or any other Df
  #this causes problems later when you join and there are duplicate column names
  select(one_of(c("Code", "VisitNumber", MasterColumnsClinical,
                  MasterColumnsUrineNormalizers, MasterColumnsNeuroPsych))) %>%
  
  #mutate(VisitNumber = factor(VisitNumber)) %>%
  mutate(EstimatedClassification = factor(EstimatedClassification)) %>%
  mutate(`Clinical Dx` = factor(`Clinical Dx`)) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(Education = factor(Education)) %>%
  mutate(ApoE = factor(ApoE)) %>%
  mutate(Hypertension = as.logical(Hypertension)) %>%
  separate(BMI, c("BmiClassification", "BmiNumber"), sep = "=") %>%
  mutate(BmiClassification = factor(BmiClassification)) %>%
  mutate(EstimatedAbAbbyQuantiles= cut(EstimatedAbAbby,
                        breaks = quantile(EstimatedAbAbby, na.rm = TRUE),
                        labels = c("Q1", "Q2", "Q3", "Q4"),
                        include.lowest = TRUE)) %>%
  mutate(EstimatedTauAbbyQuantiles = cut(EstimatedTauAbby,
                        breaks = quantile(EstimatedTauAbby, na.rm = TRUE),
                        labels = c("Q1", "Q2", "Q3", "Q4"),
                        include.lowest = TRUE)) %>%
  mutate(AgeLevels = cut(`Age at Sample`,
                         breaks = quantile(`Age at Sample`, na.rm = TRUE),
                         labels = c("Q1", "Q2", "Q3", "Q4"),
                         include.lowest = TRUE))

#now remove the columns we used in MasterList from the df's they originally came from
Clinical <- Clinical %>% select(-one_of(MasterColumnsClinical))
UrineNormalizers <- UrineNormalizers %>% select(-one_of(MasterColumnsUrineNormalizers))
NeuroPsych <- NeuroPsych %>% select(-one_of(MasterColumnsNeuroPsych))

#now keep the names of the columns we selected, because in WhizViz when we select the 
#data frame e.g. Clinical we want those old column names to still appear in that category
#so we can plot it. Although technically at that point, those columns will be coming from the
#MasterList in the join.
#for the purposes of plotting in WhizViz we only want the numeric columns shown
#note also that later on in this script we change these column names with the
#make.names function so the names in these vectors won't match the renamed columns
#unless we apply that function to these vectors too
MasterColumnsClinicalNum <- MasterList %>% select(one_of(MasterColumnsClinical)) %>% purrr::keep(is.numeric) %>% colnames %>% make.names
MasterColumnsUrineNormalizersNum <- MasterList %>% select(one_of(MasterColumnsUrineNormalizers)) %>% purrr::keep(is.numeric) %>% colnames %>% make.names
MasterColumnsNeuroPsychNum <- MasterList %>% select(one_of(MasterColumnsNeuroPsych)) %>% purrr::keep(is.numeric) %>% colnames %>% make.names

#
UrineFfa <- read_csv("~/Alzheimers/data/UrineFfa.csv", 
                      col_types = cols(`C12:1 (ng/mL)` = col_double(), 
                                  `C13:1 (ng/mL)` = col_double(), `C20:1 (ng/mL)` = col_double(), 
                                   `C20:2  (ng/mL)` = col_double(), 
                                   `C20:4 (ng/mL)` = col_double(), `C22:5 (ng/mL)` = col_double(), 
                                   `DHA/AA` = col_double(), `EPA/AA` = col_double())) %>%
  filter(!is.na(Code)) %>%
  separate(Code, c("Code", "VisitNumber"), sep = "-") %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

select(UrineFfa, Code, VisitNumber) %>% anti_join(select(Clinical, Code))

#
UrineDca <- read_csv("~/Alzheimers/data/UrineDca.csv")
names(UrineDca)[1:2] <- c("Dx", "HMRI")
UrineDca <- UrineDca %>%
  select(-Dx, -X11, -X12, -X13, -X14, -X15, -X16) %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-") %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

select(UrineDca, Code, VisitNumber) %>% anti_join(select(Clinical, Code))

#
UrineTfa <- read_csv("~/Alzheimers/data/UrineTfa.csv")
names(UrineTfa)[1] <- c("HMRI")
UrineTfa <- UrineTfa %>%  
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

select(UrineTfa, Code, VisitNumber) %>% anti_join(select(Clinical, Code))

#
CsfFfa <- read_csv("~/Alzheimers/data/CsfFfa.csv")
names(CsfFfa)[1] <- c("HMRI")
CsfFfa <- CsfFfa %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295))) %>%
  #remove this column for now since it has divide by zero entries. mention to Alfred.
  select(-`DPA/EPA`)

select(CsfFfa, Code, VisitNumber) %>% anti_join(select(Clinical, Code))

#
CsfFfaPercent <- read_csv("~/Alzheimers/data/CsfFfaPercent.csv")
names(CsfFfaPercent)[1] <- c("HMRI")
CsfFfaPercent <- CsfFfaPercent %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295))) %>%
  #remove this column for now since it has divide by zero entries. mention to Alfred.
  select(-`DPA/EPA`)

#
CsfSf <- read_csv("~/Alzheimers/data/CsfSf.csv")
names(CsfSf)[1] <- c("HMRI")
CsfSf <- CsfSf %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295))) %>%
  #remove this column for now since it has divide by zero entries. mention to Alfred.
  select(-`DPA/EPA`)

#
CsfSfPercent <- read_csv("~/Alzheimers/data/CsfSfPercent.csv")
names(CsfSfPercent)[1] <- c("HMRI")
CsfSfPercent <- CsfSfPercent %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295))) %>%
  #remove this column for now since it has divide by zero entries. mention to Alfred.
  select(-`N-3/N-6`)

#
CsfNp <- read_csv("~/Alzheimers/data/CsfNp.csv")
names(CsfNp)[1] <- c("HMRI")
CsfNp <- CsfNp %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295))) %>%
  #remove this column for now since it has divide by zero entries. mention to Alfred.
  select(-`AA/DGLA`, -`SDA/ALA`)

#
CsfNpPercent <- read_csv("~/Alzheimers/data/CsfNpPercent.csv")
names(CsfNpPercent)[1] <- c("HMRI")
CsfNpPercent <- CsfNpPercent %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
  mutate_at(vars(Code, VisitNumber), as.integer) %>%
  #remove the following patients see Mike's email from 2.23.17
  filter(!(Code %in% c(1411, 1348, 1345, 1321, 1316, 1295)))

#for WhizViz we only want numeric or integer Columns
#we have to use purrr package, bc as of 2/6/17 there
#is a bug in dplyr's select_if
ClinicalWv <- Clinical
names(ClinicalWv) <- make.names(names(ClinicalWv))
NeuroPsychWv <- NeuroPsych
names(NeuroPsychWv) <- make.names(names(NeuroPsychWv))
VolumetricsWv <- Volumetrics
names(VolumetricsWv) <- make.names(names(VolumetricsWv))
MasterListWv <- MasterList
names(MasterListWv) <- make.names(names(MasterListWv))

UrineFfaWv <- inner_join(UrineNormalizers, UrineFfa)
names(UrineFfaWv) <- make.names(names(UrineFfaWv))
UrineDcaWv <- inner_join(UrineNormalizers, UrineDca)
names(UrineDcaWv) <- make.names(names(UrineDcaWv))
UrineTfaWv <- inner_join(UrineNormalizers, UrineTfa)
names(UrineTfaWv) <- make.names(names(UrineTfaWv))

CsfFfaWv <- CsfFfa
names(CsfFfaWv) <- make.names(names(CsfFfaWv))
CsfFfaPercentWv <- CsfFfaPercent
names(CsfFfaPercentWv) <- make.names(names(CsfFfaPercentWv))

CsfSfWv <- CsfSf
names(CsfSfWv) <- make.names(names(CsfSfWv))
CsfSfPercentWv <- CsfSfPercent
names(CsfSfPercentWv) <- make.names(names(CsfSfPercentWv))

CsfNpWv <- CsfNp
names(CsfNpWv) <- make.names(names(CsfNpWv))
CsfNpPercentWv <- CsfNpPercent
names(CsfNpPercentWv) <- make.names(names(CsfNpPercentWv))

save(ClinicalWv, NeuroPsychWv, VolumetricsWv, MasterListWv,
     UrineFfaWv, UrineDcaWv, UrineTfaWv,
     CsfFfaWv, CsfFfaPercentWv,
     CsfSfWv, CsfSfPercentWv,
     CsfNpWv, CsfNpPercentWv,
     MasterColumnsClinicalNum, MasterColumnsNeuroPsychNum, MasterColumnsUrineNormalizersNum,
     file = "data/WhizViz.Rdata")
