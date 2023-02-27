#Load required R packages
library("dplyr")
library("car")
library("forcats")
library("gplots")
library("effects")

FTMBA <- read.csv ("FTMBA.csv", stringsAsFactors = TRUE) 

View(FTMBA)

paste(names(FTMBA), collapse = " + ")

#FTMBA TopRank Logistic Regression
FTMBATop <- glm(formula = TopRank ~ Gender + Age + Residency +
+ Citizenship + Institution + SchoolRank + CanadianEduc
+ AcademicBgrd + Function + Industry + WorkExp + MgmtExp + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))

#print results
summary(FTMBATop)

#stepwise logistic regresson to find best model
FTMBATopStep <- step(FTMBATop, direction = "both")

#print results
summary(FTMBATopStep)

#FTMBA BottomRank Logistic Regression
FTMBABottom <- glm(formula = BottomRank ~ Gender + Age + Residency +
+ Citizenship + Institution + SchoolRank + CanadianEduc
+ AcademicBgrd + Function + Industry + WorkExp + MgmtExp + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))

#printresults
summary(FTMBABottom)

#stepwise logistic regresson to find best model **didn't work**
FTMBABottomStep <- step(FTMBABottom, direction = "both")
summary(FTMBABottomStep)

#dropped variables one by one in each trial to reach minimized AIC for best model
#FTMBA trial 1 Bottom Rank Minimized AIC
FTMBABottom <- glm(formula = BottomRank ~  +  + Residency +
+ Citizenship + Institution + SchoolRank + CanadianEduc
+ AcademicBgrd + Function + Industry + WorkExp + MgmtExp + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))
summary(FTMBABottom)

#FTMBA trial 2 Bottom Rank Minimized AIC
FTMBABottom <- glm(formula = BottomRank ~  +  + Residency +
+ Citizenship + Institution +  + CanadianEduc
+ AcademicBgrd + Function + Industry + WorkExp + MgmtExp + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))
summary(FTMBABottom)

#FTMBA trial 3 Bottom Rank Minimized AIC
FTMBABottom <- glm(formula = BottomRank ~  +  + Residency +
+ Citizenship + Institution +  + CanadianEduc
+ AcademicBgrd + Function +  + WorkExp + MgmtExp + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))
summary(FTMBABottom)

#FTMBA trial 4 Bottom Rank Minimized AIC
FTMBABottom <- glm(formula = BottomRank ~  +  + Residency +
+ Citizenship + Institution +  + CanadianEduc
+ AcademicBgrd + Function +  +  + MgmtExp + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))
summary(FTMBABottom)

#BottomRank trial 5 Bottom Rank Minimized AIC
FTMBABottom <- glm(formula = BottomRank ~  +  + Residency +
+ Citizenship + Institution +  + CanadianEduc
+ AcademicBgrd + Function +  +  +  + PostGradDip
+ WorkingApp + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))
summary(FTMBABottom)

#BottomRank trial 6 Bottom Rank Minimized AIC
FTMBABottom <- glm(formula = BottomRank ~  +  + Residency +
+ Citizenship + Institution +  + CanadianEduc
+ AcademicBgrd + Function +  +  +  + PostGradDip
+  + ProfessionalAff.Cert + NativeLanguage +
CommunityEng + UGPA + GMAT_Score + GMAT_Verbal + GMAT_Quant,
data = FTMBA,
family = binomial(logit))

summary(FTMBABottom)


