#libraries
library(tidyverse)
library(skimr)


FBSCurrent.Comb <- readRDS("FBSCurrentComb.rds")
#analyzing the impact of the pilot

FBSdata <- FBSCurrent.Comb %>% 
  mutate(PredictAfter_Cat = ifelse(PredictAfter >= .20, "High", "Low"),
         PredictBefore_Cat = ifelse(PredictBefore >= .20, "High", "Low"))

skim(FBSdata)

FBSdata %>% 
  count(StartedAfterPilot)

FBSdata %>% 
  ggplot(aes(x = FinalRiskincDHS, y = PredictBefore)) +
  geom_jitter(width = .3, aes(color = PredictBefore_Cat)) +
  geom_boxplot(alpha = 0, outlier.shape = NA) +
  labs(title = "Prediction Vs Reality Before FBS") +
  facet_wrap(~StartedAfterPilot)



FBSdata %>% 
  ggplot(aes(x = FinalRiskincDHS, y = PredictAfter)) +
  geom_jitter(width = .3, aes(color = PredictAfter_Cat)) +
  geom_boxplot(alpha = .3, outlier.shape = NA) +
  labs(title = "Prediction Vs Reality After FBS") +
  facet_wrap(~StartedAfterPilot)


#Who did we predict was low risk but actually got removed?

#Before
FBSdata %>% 
  filter(FinalRiskincDHS == "Removed From Community" 
         & PredictBefore_Cat == "Low") #77 people

#After
FBSdata %>% 
  filter(FinalRiskincDHS == "Removed From Community" 
         & PredictAfter_Cat == "Low") #65 people

#Before and after
FBSdata %>% 
  filter(FinalRiskincDHS == "Removed From Community" 
         & PredictBefore_Cat == "Low" & PredictAfter_Cat == "Low") #48 people

#overall before
table(FBSdata$PredictBefore_Cat, FBSdata$FinalRiskincDHS)
#before overlap
table(FBSdata$PredictBefore_Cat[FBSdata$StartedAfterPilot == "OverlapPilot"], 
      FBSdata$FinalRiskincDHS[FBSdata$StartedAfterPilot == "OverlapPilot"])

#before pure pilot
table(FBSdata$PredictBefore_Cat[FBSdata$StartedAfterPilot == "StartedInPilot"], 
      FBSdata$FinalRiskincDHS[FBSdata$StartedAfterPilot == "StartedInPilot"])


#AfterOverall
table(FBSdata$PredictAfter_Cat, FBSdata$FinalRiskincDHS)

#AfterOverlap
table(FBSdata$PredictAfter_Cat[FBSdata$StartedAfterPilot == "OverlapPilot"], 
      FBSdata$FinalRiskincDHS[FBSdata$StartedAfterPilot == "OverlapPilot"])

#After Pure Pilot
table(FBSdata$PredictAfter_Cat[FBSdata$StartedAfterPilot == "StartedInPilot"], 
      FBSdata$FinalRiskincDHS[FBSdata$StartedAfterPilot == "StartedInPilot"])


#Run all of this same stuff split by pilot status, check these 23 cases against spreadsheets if possible

#Confusion Matrices and performance: Before####
#Original dataset that built the model
predicted.FBSComb.CalcModelB <- as.factor(ifelse(predicted.FBSComb.CalcModelB > 0.2,1,0)) #makes the cut at .5 and codes 0/1
levels(predicted.FBSComb.CalcModelB)<-c("P: In Community", "P: Removed From Community")
table(predicted.FBSComb.CalcModelB)

#Confusion Matrix
LR.FBSCalcModelB.ConfusionMatrix <- table(predicted.FBSComb.CalcModelB, FBS.Comb$FinalRiskincDHS)
print(LR.FBSCalcModelB.ConfusionMatrix)

#accuracy
LR.FBSCalcModelB.TestAccuracy <- sum(diag(LR.FBSCalcModelB.ConfusionMatrix)) / sum(LR.FBSCalcModelB.ConfusionMatrix)
print(LR.FBSCalcModelB.TestAccuracy) 

#Potential Harm
LR.FBSCalcModelB.ConfusionMatrix[1,2]/ sum(LR.FBSCalcModelB.ConfusionMatrix)

#Current Dataset
Current_CM_Before <- table(FBSdata$PredictBefore_Cat, FBSdata$FinalRiskincDHS)
print(Current_CM_Before)

#Accuracy
(Current_CM_Before[2,1] + Current_CM_Before[1,2])/sum(Current_CM_Before)

#Potential Harm
Current_CM_Before[2,1]/sum(Current_CM_Before)

#Confusion matrices and performance: After####

#Checking performance on original dataset, this only runs after you run FBSCleanedCodeforBuildingModels

#Confusion Matrix
LR.FBSCalcModelA.ConfusionMatrix <- table(predicted.FBSComb.CalcModelA, FBS.Comb$FinalRiskincDHS)
print(LR.FBSCalcModelA.ConfusionMatrix)
#Percent w potential harm
LR.FBSCalcModelA.ConfusionMatrix[1,2]/sum(LR.FBSCalcModelA.ConfusionMatrix)
#Percent w No Harm
(sum(LR.FBSCalcModelA.ConfusionMatrix) - LR.FBSCalcModelA.ConfusionMatrix[1,2])/sum(LR.FBSCalcModelA.ConfusionMatrix)

#Accuracy of in community predictions
LR.FBSCalcModelA.TestAccuracy <- sum(diag(LR.FBSCalcModelA.ConfusionMatrix)) / sum(LR.FBSCalcModelA.ConfusionMatrix)
print(LR.FBSCalcModelA.TestAccuracy) 

#Current data
#Confusion Matrix 
Current_CM_After <- table(FBSdata$PredictAfter_Cat, FBSdata$FinalRiskincDHS)
print(Current_CM_After)

#Percent with potential harm
Current_CM_After[2,2]/sum(Current_CM_After)

#Percent with no harm
(sum(Current_CM_After) - Current_CM_After[2,2])/sum(Current_CM_After)

#Accuracy of in-community predictions
sum(Current_CM_After[1,2], Current_CM_After[2,1])/sum(Current_CM_After)

#saving list of all cases that were incorrectly identified as low risk but really removed from community later on
FBSdata %>% 
  filter(FinalRiskincDHS == "Removed From Community" 
         & (PredictBefore_Cat == "Low" | PredictAfter_Cat == "Low")) %>% 
  select(MasterBK, PredictBefore_Cat, PredictAfter_Cat, FinalRiskincDHS) %>% 
  rename(ActualOutcome = "FinalRiskincDHS") %>% 
  mutate(LowRiskForBoth = ifelse(PredictBefore_Cat == "Low" & PredictAfter_Cat == "Low", "Yes", "No")) %>% 
  write.csv(file = "PeopleIncorrectlyIdentifiedasLowRisk.csv")

FBSdata %>% 
  count(FinalRiskincDHS)
225/(876+225)

FBS.Comb %>% 
  count(FinalRiskincDHS) 
606/(1835+606)
