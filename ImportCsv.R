library(readr)
library(tidyr)
library(dplyr)
library(boot)

#
Clinical <- read_csv("~/Alzheimers/data/Clinical.csv")
names(Clinical)[1] <- "HMRI"
Clinical <- Clinical %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")  %>%
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
  mutate(EstimatedClassification = factor(EstimatedClassification))

#
NeuroPsych <- read_csv("~/Alzheimers/data/NeuroPsych.csv")
names(NeuroPsych)[1] <- "ID"
NeuroPsych <- NeuroPsych %>%
  separate(ID, c("Code", "VisitNumber"), sep = "-")

#
UrineNormalizers <- read_csv("~/Alzheimers/data/UrineNormalizers.csv")
names(UrineNormalizers)[1:2] <- c("Classification", "HMRI")
UrineNormalizers <- UrineNormalizers[2:nrow(UrineNormalizers),]
UrineNormalizers <- UrineNormalizers %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")

#
LhVolume12 <- read_csv("~/Alzheimers/data/Volumetrics/LhVolume12.csv")
names(LhVolume12) <- paste(names(LhVolume12), "2010-2012")
names(LhVolume12)[1] <- "HMRI"

LhVolume14 <- read_csv("~/Alzheimers/data/Volumetrics/LhVolume14.csv")
names(LhVolume14) <- paste(names(LhVolume14), "2014-2016")
names(LhVolume14)[1] <- "HMRI"

RhVolume12 <- read_csv("~/Alzheimers/data/Volumetrics/RhVolume12.csv")
names(RhVolume12) <- paste(names(RhVolume12), "2010-2012")
names(RhVolume12)[1] <- "HMRI"

RhVolume14 <- read_csv("~/Alzheimers/data/Volumetrics/RhVolume14.csv")
names(RhVolume14) <- paste(names(RhVolume14), "2014-2016")
names(RhVolume14)[1] <- "HMRI"

Cont12 <- read_csv("~/Alzheimers/data/Volumetrics/Cont12.csv")
names(Cont12) <- paste(names(Cont12), "2010-2012")
names(Cont12)[1] <- "HMRI"

Cont14 <- read_csv("~/Alzheimers/data/Volumetrics/Cont14.csv")
names(Cont14) <- paste(names(Cont14), "2014-2016")
names(Cont14)[1] <- "HMRI"

Volumetrics <- LhVolume12 %>%
  full_join(RhVolume12) %>%
  full_join(Cont12) %>%
  full_join(LhVolume14) %>%
  full_join(RhVolume14) %>%
  full_join(Cont14) %>%
  mutate(ChangeInIntraCranialVolume = `IntraCranialVol 2010-2012` - `IntraCranialVol 2014-2016`) %>%
  mutate(ChangeInGrayMatterVolume = `TotalGrayVol 2014-2016` - `TotalGrayVol 2010-2012`) %>%
  mutate(Code = as.character(HMRI))

#we have to get Dx from UrineNormalizers, because the Dx from CLinical does
#not include the PAT and NAT specification
General <- Clinical %>%
  left_join(UrineNormalizers, by = c("Code", "VisitNumber")) %>%
  left_join(NeuroPsych, by = c("Code", "VisitNumber")) %>%
  left_join(Volumetrics, by = c("Code")) %>%
  #change names specifically so that they don't match names of columns of Clinical or any other Df
  #this causes problems later when you join and there are duplicate column names
  select(Code, VisitNumber, EstimatedClassification_ = EstimatedClassification, CsfAb42_Zlokovic = `CSF A_42 (pg/mL) (MSD 6E10) (Zlokovic lab)`,
         CsfTau_Zlokovic = `CSF Total tau (pg/mL) (MSD) (Zlokovic lab)`,
         AbAbby = `B Amyloid Level pg/ml (Abby)`, TauAbby = `Total Tau Level pg/ml (Abby)`,
         ClinicalDx = `Clinical Dx`, Sex_ = Sex, Age_ = `Age at Sample`, Education_ = Education,
         BMI_ = BMI, Hypertension_ = Hypertension, TBI_ = `Traumatic Brain Injury`,
         Other_Medical_Conditions_ = `Other Medical Conditions`, ApoE_ = ApoE, MedsAndSupplements = `Meds and Supplements`,
         StroopRaw = `STROOP INTERFERENCE RAW`, StroopZ = `STROOP INTERFERENCE Z-SCORE`,
         ChangeInCranialVol = ChangeInIntraCranialVolume, ChangeInGrayVol = ChangeInGrayMatterVolume) %>%
  mutate(VisitNumber = factor(VisitNumber)) %>%
  mutate(EstimatedClassification_ = factor(EstimatedClassification_)) %>%
  mutate(ClinicalDx = factor(ClinicalDx)) %>%
  mutate(Sex_ = factor(Sex_)) %>%
  mutate(Education_ = factor(Education_)) %>%
  mutate(ApoE_ = factor(ApoE_)) %>%
  mutate(Hypertension_ = as.logical(Hypertension_)) %>%
  separate(BMI_, c("BmiClassification", "BmiNumber"), sep = "=") %>%
  mutate(BmiClassification = factor(BmiClassification)) %>%
  mutate(AbLevels = cut(AbAbby,
                        breaks = quantile(AbAbby, na.rm = TRUE),
                        labels = c("Q1", "Q2", "Q3", "Q4"),
                        include.lowest = TRUE)) %>%
  mutate(TauLevels = cut(TauAbby,
                        breaks = quantile(TauAbby, na.rm = TRUE),
                        labels = c("Q1", "Q2", "Q3", "Q4"),
                        include.lowest = TRUE)) %>%
  mutate(AgeLevels = cut(Age_,
                         breaks = quantile(Age_, na.rm = TRUE),
                         labels = c("Q1", "Q2", "Q3", "Q4"),
                         include.lowest = TRUE))

#
UrineFfa <- read_csv("~/Alzheimers/data/UrineFfa.csv", 
                      col_types = cols(`C12:1 (ng/mL)` = col_double(), 
                                  `C13:1 (ng/mL)` = col_double(), `C20:1 (ng/mL)` = col_double(), 
                                   `C20:2  (ng/mL)` = col_double(), 
                                   `C20:4 (ng/mL)` = col_double(), `C22:5 (ng/mL)` = col_double(), 
                                   `DHA/AA` = col_double(), `EPA/AA` = col_double())) %>%
  filter(!is.na(Code)) %>%
  separate(Code, c("Code", "VisitNumber"), sep = "-")

#
UrineDca <- read_csv("~/Alzheimers/data/UrineDca.csv")
names(UrineDca)[1:2] <- c("Dx", "HMRI")
UrineDca <- UrineDca[2:nrow(UrineDca),]
UrineDca <- UrineDca %>%
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")

#
UrineTfa <- read_csv("~/Alzheimers/data/UrineTfa.csv")
names(UrineTfa)[1] <- c("HMRI")
UrineTfa <- UrineTfa %>%  
  separate(HMRI, c("Code", "VisitNumber"), sep = "-")

#
CsfFfa <- read_csv("~/Alzheimers/data/CsfFfa.csv", 
                   col_types = cols(`C10:0 (ng/mL)` = col_double(), 
                                    `CSF Volume used for S3 prep (mL)` = col_double(), 
                                    `DPA/EPA (ng/mL)` = col_double(), 
                                    `O-MUFA (ng/mL)` = col_double(), 
                                    `TPC S3 (ng/mL)` = col_double(), 
                                    `TPCn S3 (μg/mL)` = col_double()))
CsfFfa <- CsfFfa[2:nrow(CsfFfa),] %>%  
  separate(HmriID, c("Code", "VisitNumber"), sep = "-")

#
CsfTfa <- read_csv("~/Alzheimers/data/CsfTfa.csv", 
                   col_types = cols(`C10:0 (ng/mL)` = col_double(), 
                                    `CSF Volume used for S3 prep (mL)` = col_double(), 
                                    Dx = col_character(), HmriID = col_character(), 
                                    `TPC S3 (ng/mL)` = col_double(), 
                                    `TPCn S3 (μg/mL)` = col_double()))
CsfTfa <- CsfTfa[2:nrow(CsfTfa),] %>%  
  separate(HmriID, c("Code", "VisitNumber"), sep = "-")



#for WhizViz we only want numeric or integer Columns
#we have to use purrr package, bc as of 2/6/17 there
#is a bug in dplyr's select_if
ClinicalWv <- Clinical
names(ClinicalWv) <- make.names(names(ClinicalWv))
NeuroPsychWv <- NeuroPsych
names(NeuroPsychWv) <- make.names(names(NeuroPsychWv))
UrineFfaWv <- inner_join(UrineNormalizers, UrineFfa)
names(UrineFfaWv) <- make.names(names(UrineFfaWv))
UrineDcaWv <- inner_join(UrineNormalizers, UrineDca)
names(UrineDcaWv) <- make.names(names(UrineDcaWv))
UrineTfaWv <- inner_join(UrineNormalizers, UrineTfa)
names(UrineTfaWv) <- make.names(names(UrineTfaWv))
CsfFfaWv <- CsfFfa
names(CsfFfaWv) <- make.names(names(CsfFfaWv))
CsfTfaWv <- CsfTfa
names(CsfTfaWv) <- make.names(names(CsfTfaWv))
Volumetrics <- Volumetrics
names(Volumetrics) <- make.names(names(Volumetrics))

save(General, ClinicalWv, NeuroPsychWv, UrineFfaWv,
     UrineDcaWv, UrineTfaWv, CsfFfaWv, CsfTfaWv, Volumetrics,
     file = "data/WhizViz.Rdata")
