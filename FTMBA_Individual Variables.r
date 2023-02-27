#Load required R packages
install.packages("dplyr")
install.packages("car")
install.packages("forcats")
install.packages("gplots")
install.packages("effects")

#Load required R packages
library("dplyr")
library("car")
library("forcats")
library("gplots")
library("effects")

FTMBA <- read.csv ("FTMBA.csv", stringsAsFactors = TRUE) 

View(FTMBA)

variable.summary(FTMBA)

#CGPA vs Gender
FTMBAGender <- lm(formula = CGPA ~ Gender,data = FTMBA)
summary(FTMBAGender)

#CGPA vs Age
FTMBAAge <- lm(formula = CGPA ~ Age,data = FTMBA)
summary(FTMBAAge)

#CGPA vs Residency
FTMBAResidency <- lm(formula = CGPA ~ Residency,data = FTMBA)
summary(FTMBAResidency)

#CGPA vs Citizenship
FTMBACitizenship <- lm(formula = CGPA ~ Citizenship,data = FTMBA)
summary(FTMBACitizenship)

#CGPA vs Institution
FTMBAInstitution <- lm(formula = CGPA ~ Institution,data = FTMBA)
summary(FTMBAInstitution)

#CGPA vs SchoolRank
FTMBASchoolRank <- lm(formula = CGPA ~ SchoolRank,data = FTMBA)
summary(FTMBASchoolRank)

#CGPA vs CanadianEduc
FTMBACanadianEduc <- lm(formula = CGPA ~ CanadianEduc,data = FTMBA)
summary(FTMBACanadianEduc)

#CGPA vs AcademicBgrd
FTMBAAcademicBgrd <- lm(formula = CGPA ~ AcademicBgrd,data = FTMBA)
summary(FTMBAAcademicBgrd)

#CGPA vs Function
FTMBAFunction <- lm(formula = CGPA ~ Function,data = FTMBA)
summary(FTMBAFunction)

#CGPA vs Industry
FTMBAIndustry <- lm(formula = CGPA ~ Industry,data = FTMBA)
summary(FTMBAIndustry)

#CGPA vs WorkExp
FTMBAWorkExp <- lm(formula = CGPA ~ WorkExp,data = FTMBA)
summary(FTMBAWorkExp)

#CGPA vs MgmtExp
FTMBAMgmtExp <- lm(formula = CGPA ~ MgmtExp,data = FTMBA)
summary(FTMBAMgmtExp)

#CGPA vs PostGradDip
FTMBAPostGradDip <- lm(formula = CGPA ~ PostGradDip,data = FTMBA)
summary(FTMBAPostGradDip)

#CGPA vs WorkingApp
FTMBAWorkingApp <- lm(formula = CGPA ~ WorkingApp,data = FTMBA)
summary(FTMBAWorkingApp)

#CGPA vs ProfessionalAff.Cert
FTMBAProfessionalAff.Cert <- lm(formula = CGPA ~ ProfessionalAff.Cert,data = FTMBA)
summary(FTMBAProfessionalAff.Cert)

#CGPA vs NativeLanguage
FTMBANativeLanguage <- lm(formula = CGPA ~ NativeLanguage,data = FTMBA)
summary(FTMBANativeLanguage)

#CGPA vs CommunityEng
FTMBACommunityEng <- lm(formula = CGPA ~ CommunityEng,data = FTMBA)
summary(FTMBACommunityEng)

#CGPA vs UGPA
FTMBAUGPA <- lm(formula = CGPA ~ UGPA,data = FTMBA)
summary(FTMBAUGPA)

#CGPA vs GMAT_Status
FTMBAGMAT <- lm(formula = CGPA ~ GMAT_Status,data = FTMBA)
summary(FTMBAGMAT)

#CGPA vs GMAT_Score
FTMBAGMAT_Score <- lm(formula = CGPA ~ GMAT_Score,data = FTMBA)
summary(FTMBAGMAT_Score)

#CGPA vs GMAT_Verbal
FTMBAGMAT_Verbal <- lm(formula = CGPA ~ GMAT_Verbal,data = FTMBA)
summary(FTMBAGMAT_Verbal)

#CGPA vs GMAT_Quant
FTMBAGMAT_Quant <- lm(formula = CGPA ~ GMAT_Quant,data = FTMBA)
summary(FTMBAGMAT_Quant)


