#importing file and checking basic data quality

FBS<-read.csv("FBSrisk.csv") #saved this file in the CBCAFS/FBS Folder and noted that the CSV is the starting point for developing this code 
head(FBS)
summary(FBS)
str(FBS)

##*****************************************************************************************************
##Creating factors, combining variables, variable casting on base data set
##*****************************************************************************************************

#changing variable types
FBS$MasterBK<-as.character(FBS$MasterBK)
FBS$Full_Name<-as.character(FBS$Full_Name)


FBS$AfterWrapMulti<-as.factor(FBS$AfterWrapMulti)
FBS$BeforeWrapMulti<-as.factor(FBS$BeforeWrapMulti)
FBS$DuringWrapMulti<-as.factor(FBS$DuringWrapMulti)

FBS$DHSBefore<-as.factor(FBS$DHSBefore)
FBS$DHSDuring<-as.factor(FBS$DHSDuring)
FBS$DHSAfter<-as.factor(FBS$DHSAfter)
levels(FBS$DHSBefore)<-c("not DHS", "DHS")
levels(FBS$DHSDuring)<-c("not DHS", "DHS")
levels(FBS$DHSAfter)<-c("not DHS", "DHS")

FBS$Sum.of.Risk<-as.factor(FBS$Sum.of.Risk)
FBS$RiskwoDHS<-as.factor(FBS$RiskwoDHS)
FBS$FinalRiskincDHS<-as.factor(FBS$FinalRiskincDHS)
levels(FBS$FinalRiskincDHS)<-c("In Community", "Removed From Community")
levels(FBS$RiskwoDHS)<-c("In Community", "Removed From Community")

FBS$beforebegin<-as.Date(FBS$beforebegin,format="%m/%d/%Y")
FBS$beforeend<-as.Date(FBS$beforeend,format="%m/%d/%Y")
FBS$duringbegin<-as.Date(FBS$duringbegin,format="%m/%d/%Y")
FBS$duringend<-as.Date(FBS$duringend,format="%m/%d/%Y")
FBS$afterbegin<-as.Date(FBS$afterbegin,format="%m/%d/%Y")
FBS$afterend<-as.Date(FBS$afterend,format="%m/%d/%Y")

#creating larger "other" category
#a number of categories that are broken out now are quite small.  BeforeOther1, DuringOther1, and AfterOther1
#all to include the base other category and the following categories collapsed in -> Crisis, CRR, host home, preschool, residential.  

#FamFFT (108 in before but v low in others) left as standalone category, wouldnt include in "during" but would include in before and after

FBS$BeforeOther1<-(FBS$BeforeCrisis+FBS$BeforeCRR+FBS$BeforeHH+FBS$BeforeOther+ FBS$BeforePreschool+ FBS$BeforeResidential)

FBS$DuringOther1<-(FBS$DuringCrisis+FBS$DuringCRR+FBS$DuringHH+FBS$DuringOther+ FBS$DuringPreschool+ FBS$DuringResidential)

FBS$AfterOther1<-(FBS$AfterCrisis+FBS$AfterOther+ FBS$AfterPreschool) #Did not include CRR, Host Home, and Residentialin the after category as they are outcomes

#create overall wrap categories and then do boxplots for them

FBS$BeforeWrapAny <- FBS$BeforeWrapBS + FBS$BeforeWrapMT + FBS$BeforeWrapOther + FBS$BeforeWrapTSS
FBS$DuringWrapAny <- FBS$DuringWrapBS + FBS$DuringWrapMT + FBS$DuringWrapOther + FBS$DuringWrapTSS
FBS$AfterWrapAny <- FBS$AfterWrapBS + FBS$AfterWrapMT + FBS$AfterWrapOther + FBS$AfterWrapTSS

#FBS LOS and Dose
FBS$LOS <- FBS$duringend-FBS$duringbegin + 1
FBS$LOS <-as.integer(FBS$LOS)

FBS$Dose<-FBS$DuringFBS/FBS$LOS * 100 # N days per 100 days in community

#FBS RaceCat
levels(FBS$RaceEthnic_Label)
FBS$RaceCat<- ifelse(FBS$RaceEthnic_Label == "ASIAN                                   " | FBS$RaceEthnic_Label == "OTHER                                   " | FBS$RaceEthnic_Label == "N.AMER.INDIAN/ALASKAN NATIVE            ","Other",
                    ifelse(FBS$RaceEthnic_Label == "HISPANIC                                ", "Hispanic",
                            ifelse(FBS$RaceEthnic_Label == "BLACK OR AFRICAN AMERICAN               ", "Black",
                                   ifelse(FBS$RaceEthnic_Label == "WHITE                                   ", "White", "Error"))))

FBS$RaceCat<-as.factor(FBS$RaceCat)


##*****************************************************************************************************
## FBS.Subset
## Defining a subset with cases that have a full 6 month after period
##*****************************************************************************************************

#creating a subset that removes FBS cases ending after 5/31/2016
FBS.Subset<-FBS[FBS$duringend <= "2016-05-31",]
FBS.IncompCase<-FBS[FBS$duringend > "2016-05-31",]

##*****************************************************************************************************
## FBS.Subset1
## Defining a subset limiting FBS.Subset to just raw input variables
##*****************************************************************************************************
#limiting columns in subset to just potential test variables
AllTestVariables<-c("FinalRiskincDHS","ageatservicebegin",
    "MasterBK", "Gender_Label", "RaceCat", "LOS", "Dose",
    "BeforeAIP", "DuringAIP",
    "BeforeCRC", "DuringCRC",
    "BeforeEval", "DuringEval",
    "BeforeFamFFT", "DuringFamFFT",
    "BeforeFBS", "DuringFBS",
    "BeforeOP", "DuringOP",
    "BeforePartial", "DuringPartial",
    "BeforeRTF", "DuringRTF",
    "BeforeSTS", "DuringSTS",
    "BeforeTCM", "DuringTCM",
    "BeforeWrapBS", "DuringWrapBS",
    "BeforeWrapMT", "DuringWrapMT",
    "BeforeWrapTSS", "DuringWrapTSS",
    "BeforeWrapOther", "DuringWrapOther",
    "BeforeWrapAny", "DuringWrapAny", #total days for ANY BHRS
    "BeforeOther1", "DuringOther1",
    "BeforeWrapMulti", "DuringWrapMulti", #r they using 0, 1, 2, or 3 Wrap Services
    "DHSBefore", "DHSDuring", "DHSAfter") 

FBS.Subset1<-FBS.Subset[AllTestVariables] 

##*****************************************************************************************************
## In original code file, there is code creating decision forests using raw variables 
## and code creating decision forests using almost all categorical variables
## did this mostly for exploration and as a baseline to assess other models
## Not necessary to run this
##*****************************************************************************************************


##*****************************************************************************************************
## FBS.Comb
## Creating new subset with a hybrid of categorical variables and interval variables
## This is my working dataset for most analytic modeling
## Many of these decisions based on previous models of raw vs categorical and what makes sense for 
## clinical to actually use in a predictive model
##*****************************************************************************************************

FBS.Comb<-FBS.Subset1

#recodes for variables that will remain factors

#creating AIP Category for ease of clinical use
FBS.Comb$BeforeAIP.Cat <- as.factor(ifelse(FBS.Comb$BeforeAIP == 0, 0, ifelse(FBS.Comb$BeforeAIP <=30, 1, 2)))
levels(FBS.Comb$BeforeAIP.Cat) <- c("No AIP", "30 days or less", "More than 30 days")
FBS.Comb$DuringAIP.Cat <- as.factor(ifelse(FBS.Comb$DuringAIP == 0, 0, ifelse(FBS.Comb$DuringAIP <=30, 1, 2)))
levels(FBS.Comb$DuringAIP.Cat) <- c("No AIP", "30 days or less", "More than 30 days")

#Making CRC 3 categories for ease of clinical use
FBS.Comb$BeforeCRC <- as.factor(ifelse(FBS.Comb$BeforeCRC == 0, 0, ifelse(FBS.Comb$BeforeCRC == 1, 1, 2)))
levels(FBS.Comb$BeforeCRC)<-c("No days", "1 day", "More than 1 day")
FBS.Comb$DuringCRC <- as.factor(ifelse(FBS.Comb$DuringCRC == 0, 0, ifelse(FBS.Comb$DuringCRC==1, 1, 2)))
levels(FBS.Comb$DuringCRC)<-c("No days", "1 day", "More than 1 day")

#Making eval binary
FBS.Comb$BeforeEval <- as.factor(ifelse(FBS.Comb$BeforeEval == 0, 0, 1))
FBS.Comb$DuringEval <- as.factor(ifelse(FBS.Comb$DuringEval == 0, 0, 1))

#FBS Before is a binary (don't think i put this in the models), kept FBS as days for during time period, along with LOS
FBS.Comb$BeforeFBS.Bin <- as.factor(ifelse(FBS.Comb$BeforeFBS == 0, 0, 1))

#Creating 3 OP categories (None, Average, High) (cutpoints based on boxplot stats)
FBS.Comb$BeforeOP.Cat <- as.factor(ifelse(FBS.Comb$BeforeOP == 0, 0, ifelse(FBS.Comb$BeforeOP > 0 & FBS.Comb$BeforeOP <= 12, 1, 2)))
levels(FBS.Comb$BeforeOP.Cat) <- c("None", "Average", "High")
FBS.Comb$DuringOP.Cat <- as.factor(ifelse(FBS.Comb$DuringOP == 0, 0, ifelse(FBS.Comb$DuringOP >0 & FBS.Comb$DuringOP <= 5, 1, 2)))
levels(FBS.Comb$DuringOP.Cat) <- c("None", "Average", "High")

#AlsoCreating OP Binary
FBS.Comb$BeforeOP.Bin <- as.factor(ifelse(FBS.Comb$BeforeOP == 0, 0, 1))
FBS.Comb$DuringOP.Bin <- as.factor(ifelse(FBS.Comb$DuringOP == 0, 0, 1))

#Making partial binary, usually minimal variation in LOS
FBS.Comb$BeforePartial <- as.factor(ifelse(FBS.Comb$BeforePartial == 0, 0, 1))
FBS.Comb$DuringPartial <- as.factor(ifelse(FBS.Comb$DuringPartial == 0, 0, 1))

#Making RTF Binary after exploring distributions on days
FBS.Comb$BeforeRTF <- as.factor(ifelse(FBS.Comb$BeforeRTF == 0, 0, 1)) #understand these are monstly long LOS
FBS.Comb$DuringRTF <- as.factor(ifelse(FBS.Comb$DuringRTF == 0, 0, 1)) #understand these are all under 90 days

##Making WrapCategories based on boxplot stats. put WrapAny back in model with days, just to check
#sum(table(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0]))#819 ppl have bhrs before
#summary(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0])
#boxplot(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0])
#boxplot.stats(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0]) # most people under 104 days, 57 outliers
#sum(table(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0])) #556 people have bhrs during
#summary(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0])
#boxplot.stats(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0]) #most people under 47 days,68 outliers )

#based on Wrapany boxplots, creating 3 categories for before and after - none, average, high
FBS.Comb$BeforeWrap.Cat <- as.factor(ifelse(FBS.Comb$BeforeWrapAny == 0, 0, ifelse(FBS.Comb$BeforeWrapAny > 0 & FBS.Comb$BeforeWrapAny <=50, 2, 3)))
levels(FBS.Comb$BeforeWrap.Cat) <- c("None", "Average", "High")
FBS.Comb$DuringWrap.Cat <- as.factor(ifelse(FBS.Comb$DuringWrapAny == 0, 0, ifelse(FBS.Comb$DuringWrapAny > 0 & FBS.Comb$DuringWrapAny <= 20, 2, 3)))
levels(FBS.Comb$DuringWrap.Cat) <- c("None", "Average", "High")

#creatingLOSquartiles
FBS.Comb$LOSquart<-as.factor(ifelse(FBS.Comb$LOS <= 43, 1,ifelse(FBS.Comb$LOS > 43 & FBS.Comb$LOS <= 165, 2,ifelse(FBS.Comb$LOS > 165 & FBS.Comb$LOS <= 235, 3, 4))))
levels(FBS.Comb$LOSquart)<- c("43 Days or less", "44 to 165", "166 to 235", "GT 235")

#Creating LOScat - after seeing the results of LRmodel with LOSQuart, it was clear those numbers are weird cutpoints for clinical to use.  Rounded to closest month
#Also created a category for people who have LOS < 30 days just based on the fact that modal use was 1 day and a fair number had 30 days or less.
FBS.Comb$LOScat<-as.factor(ifelse(FBS.Comb$LOS < 30, 1,ifelse(FBS.Comb$LOS >= 30 & FBS.Comb$LOS <= 120, 2,ifelse(FBS.Comb$LOS > 121 & FBS.Comb$LOS <= 240, 3, 4))))
levels(FBS.Comb$LOScat)<- c("LT 1 month", "30 days to 4 months", "121 days to 8 months", "GT 8 months")

#Making STS Binary
FBS.Comb$BeforeSTS.Bin <- as.factor(ifelse(FBS.Comb$BeforeSTS == 0, 0, 1))

#Making TCM Binary  
FBS.Comb$BeforeTCM.Bin <- as.factor(ifelse(FBS.Comb$BeforeTCM == 0, 0, 1))

#kept Age raw bc it looked like the percent of kids that are removed steadily increased with each additional year of age

##*****************************************************************************************************
## Modeling on FBS.Comb
##*****************************************************************************************************


##*****************************************************************************************************
## Decision Forest to look at variable importance
##*****************************************************************************************************
#############################################
#Creating test/train sets

#Creating test and training divisions

## randomly choose 70% of the data set as training data
set.seed(23)
FBSComb.trainingindex <- sample(1:nrow(FBS.Comb), 0.7*nrow(FBS.Comb), replace=F) #creates the indices for the sample
FBSComb.Train <- FBS.Comb[FBSComb.trainingindex,] #pull data where training indices are true

## select the other 30% as the testing data
FBSComb.Test <- FBS.Comb[-FBSCat.trainingindex,] #Pulls data where training indices are false

#############################################
#Decision forest model 2 (uses LOSCat, RFComb.model1 used LOS, in original code)
FBSComb.RFmodel2 <- randomForest(FinalRiskincDHS ~ Gender_Label + RaceCat + LOScat + ageatservicebegin +
                                   BeforeAIP + DuringAIP + 
                                   BeforeCRC + DuringCRC + 
                                   BeforeEval + DuringEval + 
                                   DuringFBS +
                                   BeforeOP.Cat + DuringOP.Cat + 
                                   BeforePartial + DuringPartial + 
                                   BeforeRTF + DuringRTF + 
                                   BeforeWrap.Cat + DuringWrap.Cat +
                                   BeforeWrapMulti + DuringWrapMulti, data = FBSComb.Train, importance = TRUE, ntree = 500)
print(FBSComb.RFmodel2)
importance(FBSComb.RFmodel2)
varImpPlot(FBSComb.RFmodel2)

##predicting for model 2 on test set.
FBSComb.Model2Predictions <- predict(FBSComb.RFmodel2, FBSComb.Test, type = "response")
##confusion matrix
FBSComb.Model2.TestConfusion <- table(FBSComb.Model2Predictions, FBSComb.Test$FinalRiskincDHS)
print(FBSComb.Model2.TestConfusion)
##accuracy
FBSComb.Model2.TestAccuracy <- sum(diag(FBSComb.Model2.TestConfusion)) / sum(FBSComb.Model2.TestConfusion)
print(FBSComb.Model2.TestAccuracy) 
##precision
FBSComb.Model2.TestPrecision <- FBSComb.Model2.TestConfusion[2,2] / sum(FBSComb.Model2.TestConfusion[2,])
print(FBSComb.Model2.TestPrecision)
##recall
FBSComb.Model2.TestRecall <- FBSComb.Model2.TestConfusion[2,2]  / sum(FBSComb.Model2.TestConfusion[,2])
print(FBSComb.Model2.TestRecall)


##*****************************************************************************************************
## Logistic Regressions - Before and After Models
##*****************************************************************************************************

library(pscl) #for finding pseudo R2

#####################################################
#####Creating test and training divisions

## randomly choose 70% of the data set as training data
set.seed(23)
FBSComb.trainingindex <- sample(1:nrow(FBS.Comb), 0.7*nrow(FBS.Comb), replace=F) #creates the indices for the sample
FBSComb.Train <- FBS.Comb[FBSComb.trainingindex,] #pull vector where training indices are true


## select the other 30% as the testing data
FBSComb.Test <- FBS.Comb[-FBSCat.trainingindex,]

##################################
#LR Before Training Model
##################################

#Creating this model so clinical can run predictive model based only on information they know when a member starts FBS

FBSComb.LRmodelBefore<-glm(FinalRiskincDHS ~ 
                        Gender_Label + RaceCat + ageatservicebegin +
                        BeforeAIP.Cat + #3 level cat
                        BeforeCRC + #3 level cat
                        BeforeEval + #Binary
                        BeforeFBS.Bin + #Binary
                        BeforeOP.Cat + #3 level cat based on boxplot stats
                        BeforePartial + #Bin
                        BeforeRTF + #Binary
                        BeforeSTS.Bin +
                        BeforeTCM.Bin +
                        BeforeWrap.Cat + #3 level cat based on boxplot stats
                        BeforeWrapMulti + #3 level cat for number of concurrent services
                        DHSBefore #Binary
                      , family = binomial(link = logit), data = FBSComb.Train)
summary(FBSComb.LRmodelBefore)

anova(FBSComb.LRmodelBefore, test = "Chisq")

###Getting pseudo r2 for model above
pR2(FBSComb.LRmodelBefore)

#Predicting Risk of Removal
predicted.FBSComb.ModelBefore <- predict(FBSComb.LRmodelBefore,newdata=FBSComb.Test,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.ModelBefore[FBSComb.Test$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.ModelBefore[FBSComb.Test$FinalRiskincDHS == "Removed From Community"])

predicted.FBSComb.ModelBefore <- as.factor(ifelse(predicted.FBSComb.ModelBefore > 0.2,1,0)) #makes the cut at .2 and codes 0/1, cut at .2 to ensure our in community box is as pure as possible
levels(predicted.FBSComb.ModelBefore)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.ModelBefore)

#Confusion Matrix
LR.FBSModelBefore.ConfusionMatrix <- table(predicted.FBSComb.ModelBefore, FBSComb.Test$FinalRiskincDHS)
print(LR.FBSModelBefore.ConfusionMatrix)

#accuracy
LR.FBSModelBefore.TestAccuracy <- sum(diag(LR.FBSModelBefore.ConfusionMatrix)) / sum(LR.FBSModelBefore.ConfusionMatrix)
print(LR.FBSModelBefore.TestAccuracy) 

#Precision
LR.FBSModelBefore.TestPrecision <- LR.FBSModelBefore.ConfusionMatrix[2,2] / sum(LR.FBSModelBefore.ConfusionMatrix[2,])
print(LR.FBSModelBefore.TestPrecision)

#recall - trying to maximize recall in these models
LR.FBSModelBefore.TestRecall <- LR.FBSModelBefore.ConfusionMatrix[2,2]  / sum(LR.FBSModelBefore.ConfusionMatrix[,2])
print(LR.FBSModelBefore.TestRecall)


##################################
#LR During Training Model (LRModel1 used LOS days variable, in orig code)
##################################

FBSComb.LRmodel2<-glm(FinalRiskincDHS ~ 
                       Gender_Label + RaceCat + LOScat + ageatservicebegin +
                       BeforeAIP.Cat + DuringAIP.Cat + #3 level cat
                       BeforeCRC + DuringCRC + #3 level cats
                       BeforeEval + DuringEval + #binary
                       DuringFBS + #days
                       BeforeOP.Cat + DuringOP.Cat + #3 level cats based on boxplot
                       BeforePartial + DuringPartial + #Binary
                       BeforeRTF + DuringRTF + #Binary
                       BeforeWrap.Cat + DuringWrap.Cat + #3 level cats
                       BeforeWrapMulti + DuringWrapMulti #3 level cat for n of concurrent services
                     , family = binomial(link = logit), data = FBSComb.Train)
summary(FBSComb.LRmodel2)
anova(FBSComb.LRmodel2, test = "Chisq")
###Getting pseudo r2 for model above
pR2(FBSComb.LRmodel2)

#Predicting Risk of Removal
predicted.FBSComb.Model2 <- predict(FBSComb.LRmodel2,newdata=FBSComb.Test,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.Model2[FBSComb.Test$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.Model2[FBSComb.Test$FinalRiskincDHS == "Removed From Community"])

predicted.FBSComb.Model2 <- as.factor(ifelse(predicted.FBSComb.Model2 > 0.2,1,0)) 
levels(predicted.FBSComb.Model2)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.Model2)

#Confusion Matrix
LR.FBSModel2.ConfusionMatrix <- table(predicted.FBSComb.Model2, FBSComb.Test$FinalRiskincDHS)
print(LR.FBSModel2.ConfusionMatrix)

#accuracy
LR.FBSModel2.TestAccuracy <- sum(diag(LR.FBSModel2.ConfusionMatrix)) / sum(LR.FBSModel2.ConfusionMatrix)
print(LR.FBSModel2.TestAccuracy) 

#Precision
LR.FBSModel2.TestPrecision <- LR.FBSModel2.ConfusionMatrix[2,2] / sum(LR.FBSModel2.ConfusionMatrix[2,])
print(LR.FBSModel2.TestPrecision)

#recall - looking to maximize recall in these models
LR.FBSModel2.TestRecall <- LR.FBSModel2.ConfusionMatrix[2,2]  / sum(LR.FBSModel2.ConfusionMatrix[,2])
print(LR.FBSModel2.TestRecall)


####################################
#Skipping cross validation for the time being, and just running all the data to create the final model
#Creating this model so clinical can run predictive model based only on information they know when a member starts FBS
####################################

##*****************************************************************************************************
## Production Models using entire FBS.Comb dataset
##*****************************************************************************************************

####################################
#Production: Before
####################################

FBSComb.LRmodelBefore2<-glm(FinalRiskincDHS ~ 
                             Gender_Label + RaceCat + ageatservicebegin +
                             BeforeAIP.Cat + #3 level cat
                             BeforeCRC + #3 level cat
                             BeforeEval + #Binary
                             BeforeFBS.Bin + #Binary
                             BeforeOP.Cat + #3 level cat
                             BeforePartial + #Bin
                             BeforeRTF + #Binary
                             BeforeSTS.Bin +
                             BeforeTCM.Bin +
                             BeforeWrap.Cat + #3 level cat
                             BeforeWrapMulti + #3 level cat for concurrent services
                             DHSBefore #Binary
                           , family = binomial(link = logit), data = FBS.Comb)
summary(FBSComb.LRmodelBefore2)
anova(FBSComb.LRmodelBefore2, test = "Chisq")
###Getting pseudo r2 for model above
pR2(FBSComb.LRmodelBefore2)



#Predicting Risk of Removal
predicted.FBSComb.ModelBefore2 <- predict(FBSComb.LRmodelBefore2,newdata=FBS.Comb,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.ModelBefore2[FBS.Comb$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.ModelBefore2[FBS.Comb$FinalRiskincDHS == "Removed From Community"])

#adding probability column to FBS.Comb, exploring outcomes
FBS.Comb$ProbabilityBefore <- predicted.FBSComb.ModelBefore2
summary(FBS.Comb)
boxplot(FBS.Comb$ProbabilityBefore ~ FBS.Comb$FinalRiskincDHS, main = "Predicted probability vs reality")
boxplot(FBS.Comb$ProbabilityBefore[FBS.Comb$FinalRiskincDHS== "In Community"])
boxplot(FBS.Comb$ProbabilityBefore[FBS.Comb$FinalRiskincDHS== "Removed From Community"])


#********
#Keeping this sequence of code but no need to run, this checks how well the model performed w predictions vs actual outcome on entire dataset
#********
predicted.FBSComb.ModelBefore2 <- as.factor(ifelse(predicted.FBSComb.ModelBefore2 > 0.2,1,0)) #makes the cut at .5 and codes 0/1
levels(predicted.FBSComb.ModelBefore2)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.ModelBefore2)

#Confusion Matrix
LR.FBSModelBefore2.ConfusionMatrix <- table(predicted.FBSComb.ModelBefore2, FBS.Comb$FinalRiskincDHS)
print(LR.FBSModelBefore2.ConfusionMatrix)

#accuracy
#LR.FBSModelBefore2.TestAccuracy <- sum(diag(LR.FBSModelBefore2.ConfusionMatrix)) / sum(LR.FBSModelBefore2.ConfusionMatrix)
#print(LR.FBSModelBefore2.TestAccuracy) 

#Precision
#LR.FBSModelBefore2.TestPrecision <- LR.FBSModelBefore2.ConfusionMatrix[2,2] / sum(LR.FBSModelBefore2.ConfusionMatrix[2,])
#print(LR.FBSModelBefore2.TestPrecision)

#recall
#LR.FBSModelBefore2.TestRecall <- LR.FBSModelBefore2.ConfusionMatrix[2,2]  / sum(LR.FBSModelBefore2.ConfusionMatrix[,2])
#print(LR.FBSModelBefore2.TestRecall)


####################################
#Production: After
####################################
#options(scipen = 999) - turns off scientific notation, this model sometimes uses it
FBSComb.LRmodelAfter<-glm(FinalRiskincDHS ~ 
                              Gender_Label + RaceCat + LOScat + ageatservicebegin +
                              BeforeAIP.Cat + DuringAIP.Cat + #3 level cat
                              BeforeCRC + DuringCRC + #3 level cat
                              BeforeEval + DuringEval + #Binary
                              BeforeFBS.Bin + DuringFBS + #Binary
                              BeforeOP.Cat + DuringOP.Cat + #3 level cat
                              BeforePartial + DuringPartial + #Bin
                              BeforeRTF + DuringRTF + #Binary
                              BeforeSTS.Bin + 
                              BeforeTCM.Bin + 
                              BeforeWrap.Cat + DuringWrap.Cat + #3 level cat
                              BeforeWrapMulti + DuringWrapMulti + #3 level cat for concurrent services
                              DHSBefore #Binary
                            , family = binomial(link = logit), data = FBS.Comb)
summary(FBSComb.LRmodelAfter)
anova(FBSComb.LRmodelAfter, test = "Chisq")
###Getting pseudo r2 for model above
pR2(FBSComb.LRmodelAfter)

#Predicting Risk of Removal
predicted.FBSComb.ModelAfter <- predict(FBSComb.LRmodelAfter,newdata=FBS.Comb,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.ModelAfter[FBS.Comb$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.ModelAfter[FBS.Comb$FinalRiskincDHS == "Removed From Community"])

#adding probability column
FBS.Comb$ProbabilityAfter <- predicted.FBSComb.ModelAfter
summary(FBS.Comb)
boxplot(FBS.Comb$ProbabilityAfter ~ FBS.Comb$FinalRiskincDHS, main = "Predicted probability vs reality")
boxplot.stats(FBS.Comb$ProbabilityAfter[FBS.Comb$FinalRiskincDHS== "In Community"])
boxplot.stats(FBS.Comb$ProbabilityAfter[FBS.Comb$FinalRiskincDHS== "Removed From Community"])

#********
#Keeping this sequence of code but no need to run, this checks how well the model performed w predictions vs actual outcome on entire dataset
#********
#predicted.FBSComb.ModelAfter <- as.factor(ifelse(predicted.FBSComb.ModelAfter > 0.2,1,0)) #makes the cut at .5 and codes 0/1
#table(predicted.FBSComb.ModelAfter) #checks the summary of 0/1, looks like 1 = kids
#levels(predicted.FBSComb.ModelAfter)<-c("In Community", "Removed From Community")
#table(predicted.FBSComb.ModelAfter)

#Confusion Matrix
#LR.FBSModelAfter.ConfusionMatrix <- table(predicted.FBSComb.ModelAfter, FBS.Comb$FinalRiskincDHS)
#print(LR.FBSModelAfter.ConfusionMatrix)

#accuracy
#LR.FBSModelAfter.TestAccuracy <- sum(diag(LR.FBSModelAfter.ConfusionMatrix)) / sum(LR.FBSModelAfter.ConfusionMatrix)
#print(LR.FBSModelAfter.TestAccuracy) 

#Precision
#LR.FBSModelAfter.TestPrecision <- LR.FBSModelAfter.ConfusionMatrix[2,2] / sum(LR.FBSModelAfter.ConfusionMatrix[2,])
#print(LR.FBSModelAfter.TestPrecision)

#recall
#LR.FBSModelAfter.TestRecall <- LR.FBSModelAfter.ConfusionMatrix[2,2]  / sum(LR.FBSModelAfter.ConfusionMatrix[,2])
#print(LR.FBSModelAfter.TestRecall)

##*****************************************************************************************************
## Figure Creation for the Report
##*****************************************************************************************************

#Distribution of LOS
boxplot(FBS.Comb$LOS, 
	main = "Figure 1: Distribution of LOS in FBS", 
	ylab = "Days")
mtext("2012 to 2015") #creates a subtitle. Can also use \n to start a new line in main
text(y=boxplot.stats(FBS.Comb$LOS)$stats, 
	labels = boxplot.stats(FBS.Comb$LOS)$stats, #Creates labels based on boxplot stats function
	x = 1.3)  #x value ensures enough space between label and box

#Distribution of Dose
boxplot(FBS.Comb$Dose, 
	main = "Figure 2: Distribution of Dose in FBS", 
	ylab = "Number of FBS Claims per 100 Days in FBS")
mtext("2012 to 2015") #creates a subtitle. Can also use \n to start a new line in main
text(y=boxplot.stats(FBS.Comb$Dose)$stats, 
	labels = round(boxplot.stats(FBS.Comb$Dose)$stats, digits = 1), 
	x = 1.3)


#Boxplots for Service Day Distribution
boxlab<-c("AIP", "Fam FFT", "OP", "Partial", "RTF", "STS", "TCM", "Wrap BSC", "Wrap MT", "Wrap TSS", "Wrap - Any")

par(mar = c(10, 5, 5, 2)) #default par = par(mar=c(5.1, 4.1, 4.1, 2.1))

#Before
boxplot(
  FBS.Subset1$BeforeAIP[FBS.Subset1$BeforeAIP>0], 
  FBS.Subset1$BeforeFamFFT[FBS.Subset1$BeforeFamFFT>0],
  FBS.Subset1$BeforeOP[FBS.Subset1$BeforeOP>0], 
  FBS.Subset1$BeforePartial[FBS.Subset1$BeforePartial>0],
  FBS.Subset1$BeforeRTF[FBS.Subset1$BeforeRTF>0],
  FBS.Subset1$BeforeSTS[FBS.Subset1$BeforeSTS>0],
  FBS.Subset1$BeforeTCM[FBS.Subset1$BeforeTCM>0],
  FBS.Subset1$BeforeWrapBS[FBS.Subset1$BeforeWrapBS>0],
  FBS.Subset1$BeforeWrapMT[FBS.Subset1$BeforeWrapMT>0],
  FBS.Subset1$BeforeWrapTSS[FBS.Subset1$BeforeWrapTSS>0],
  FBS.Subset1$BeforeWrapAny[FBS.Subset1$BeforeWrapAny>0], 
  main = "Figure 3: Distribution of Members' Service Utilization 6 Months Prior to FBS", 
  ylab = "Days of Service Use",
  las = 2,
  names = boxlab)
mtext("Only members that used these services are included in the distribution", 
	side = 1, 
	line = 7)

#During
boxplot(
  FBS.Subset1$DuringAIP[FBS.Subset1$DuringAIP>0], 
  FBS.Subset1$DuringFamFFT[FBS.Subset1$DuringFamFFT>0],
  FBS.Subset1$DuringOP[FBS.Subset1$DuringOP>0], 
  FBS.Subset1$DuringPartial[FBS.Subset1$DuringPartial>0],
  FBS.Subset1$DuringRTF[FBS.Subset1$DuringRTF>0],
  FBS.Subset1$DuringSTS[FBS.Subset1$DuringSTS>0],
  FBS.Subset1$DuringTCM[FBS.Subset1$DuringTCM>0],
  FBS.Subset1$DuringWrapBS[FBS.Subset1$DuringWrapBS>0],
  FBS.Subset1$DuringWrapMT[FBS.Subset1$DuringWrapMT>0],
  FBS.Subset1$DuringWrapTSS[FBS.Subset1$DuringWrapTSS>0],
  FBS.Subset1$DuringWrapAny[FBS.Subset1$DuringWrapAny>0], 
  main = "Figure 4: Distribution of Members' Service Utilization Concurrent with FBS", 
  ylab = "Days of Service Use",
  las = 2, #rotates x axis names 90 degrees
  names = boxlab, #sets x axis names
  ylim = c(0,150), #trims y axis from 0 to 150
  yaxp = c(0, 150, 15)) #y axis, from 0 to 150, 15 tick marks
mtext("Only members that used these services are included in the distribution", 
	side = 1, 
	line = 7)
mtext("Days of Service use beyond 150 days trimmed.  Partial, STS, TSS, and Wrap-Any are impacted.", 
	side = 1, 
	line = 8)

#Figure 5 - variable importance plot (remember these may change slightly depending on if I regenerated the model)
write.csv(importance(FBSComb.RFmodel2), file = "VarImpt.csv") #note that the original figure was created off FBSComb.RFmodel1, but that code isn't included here. Diff is raw LOS (model1) vs LOScat(Model2)
varImpPlot(FBSComb.RFmodel2)
  
#Figure 6 - model performance
par(mfrow = c(1,2)) 

boxplot(FBS.Comb$Probability ~ FBS.Comb$FinalRiskincDHS, main = "FBS Before", names = c("In Community", "Removed"), ylab = "Probability of Removal")
mtext("Predicted probability vs reality", line = 0.5)
boxplot(FBS.Comb$ProbabilityAfter ~ FBS.Comb$FinalRiskincDHS, main = "FBS Complete", names = c("In Community", "Removed"))
mtext("Predicted probability vs reality", line = 0.5)

#two confusion matrices from entire models run with all data
#Note that these two confusion matrices are for the production models, and the code above that produces the tables is currently commented out
#can comment back in to create these confusion matrices
#print(LR.FBSModelBefore2.ConfusionMatrix)
#print(LR.FBSModelAfter.ConfusionMatrix)


##*****************************************************************************************************
## Calculator Models for excel sheet
##*****************************************************************************************************

####################################
#Production Calculator: Before
####################################

FBSComb.CalcModelB<-glm(FinalRiskincDHS ~ 
                              ageatservicebegin +
                              BeforeAIP.Cat + #3 level cat
                              BeforeCRC + #3 level cat
                              BeforePartial + #Bin
                              BeforeRTF  #Binary
                            , family = binomial(link = logit), data = FBS.Comb)
summary(FBSComb.CalcModelB)
anova(FBSComb.CalcModelB, test = "Chisq")
###Getting pseudo r2 for model above
pR2(FBSComb.CalcModelB)



#Predicting Risk of Removal
predicted.FBSComb.CalcModelB <- predict(FBSComb.CalcModelB,newdata=FBS.Comb,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.CalcModelB[FBS.Comb$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.CalcModelB[FBS.Comb$FinalRiskincDHS == "Removed From Community"])

#adding probability column to FBS.Comb, exploring outcomes
FBS.Comb$ProbabilityBeforeCM <- predicted.FBSComb.CalcModelB
summary(FBS.Comb)
boxplot(FBS.Comb$ProbabilityBeforeCM ~ FBS.Comb$FinalRiskincDHS, main = "Predicted probability vs reality")
boxplot.stats(FBS.Comb$ProbabilityBeforeCM[FBS.Comb$FinalRiskincDHS== "In Community"])
boxplot.stats(FBS.Comb$ProbabilityBeforeCM[FBS.Comb$FinalRiskincDHS== "Removed From Community"])


#********
#Keeping this sequence of code but no need to run, this checks how well the model performed w predictions vs actual outcome on entire dataset
#********
#predicted.FBSComb.CalcModelB <- as.factor(ifelse(predicted.FBSComb.CalcModelB > 0.2,1,0)) #makes the cut at .5 and codes 0/1
#levels(predicted.FBSComb.CalcModelB)<-c("In Community", "Removed From Community")
#table(predicted.FBSComb.CalcModelB)

#Confusion Matrix
#LR.FBSCalcModelB.ConfusionMatrix <- table(predicted.FBSComb.CalcModelB, FBS.Comb$FinalRiskincDHS)
#print(LR.FBSCalcModelB.ConfusionMatrix)

#accuracy
#LR.FBSCalcModelB.TestAccuracy <- sum(diag(LR.FBSCalcModelB.ConfusionMatrix)) / sum(LR.FBSCalcModelB.ConfusionMatrix)
#print(LR.FBSCalcModelB.TestAccuracy) 

#Precision
#LR.FBSCalcModelB.TestPrecision <- LR.FBSCalcModelB.ConfusionMatrix[2,2] / sum(LR.FBSCalcModelB.ConfusionMatrix[2,])
#print(LR.FBSCalcModelB.TestPrecision)

#recall
#LR.FBSCalcModelB.TestRecall <- LR.FBSCalcModelB.ConfusionMatrix[2,2]  / sum(LR.FBSCalcModelB.ConfusionMatrix[,2])
#print(LR.FBSCalcModelB.TestRecall)


####################################
#Production Calculator: After
####################################
#options(scipen = 999) - turns off scientific notation, this model sometimes uses it
FBSComb.CalcModelA<-glm(FinalRiskincDHS ~ 
                            LOScat + ageatservicebegin +
                            BeforeAIP.Cat + DuringAIP.Cat + #3 level cat
                            BeforeCRC + DuringCRC + #3 level cat
                            DuringEval + #Binary
                             #Binary
                             #3 level cat
                            BeforePartial + #Bin
                            BeforeRTF + DuringRTF #Binary
                             #Binary
                          , family = binomial(link = logit), data = FBS.Comb)
summary(FBSComb.CalcModelA)
anova(FBSComb.CalcModelA, test = "Chisq")
###Getting pseudo r2 for model above
pR2(FBSComb.CalcModelA)

#Predicting Risk of Removal
predicted.FBSComb.CalcModelA <- predict(FBSComb.CalcModelA,newdata=FBS.Comb,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.CalcModelA[FBS.Comb$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.CalcModelA[FBS.Comb$FinalRiskincDHS == "Removed From Community"])

#adding probability column
FBS.Comb$ProbabilityAfterCM <- predicted.FBSComb.CalcModelA
summary(FBS.Comb)
boxplot(FBS.Comb$ProbabilityAfterCM ~ FBS.Comb$FinalRiskincDHS, main = "Predicted probability vs reality")
boxplot.stats(FBS.Comb$ProbabilityAfterCM[FBS.Comb$FinalRiskincDHS== "In Community"])
boxplot.stats(FBS.Comb$ProbabilityAfterCM[FBS.Comb$FinalRiskincDHS== "Removed From Community"])

#********
#Keeping this sequence of code but no need to run, this checks how well the model performed w predictions vs actual outcome on entire dataset
#********
#predicted.FBSComb.CalcModelA <- as.factor(ifelse(predicted.FBSComb.CalcModelA > 0.2,1,0)) #makes the cut at .5 and codes 0/1
#table(predicted.FBSComb.CalcModelA) #checks the summary of 0/1, looks like 1 = kids
#levels(predicted.FBSComb.CalcModelA)<-c("P: In Community", "P: Removed From Community")
#table(predicted.FBSComb.CalcModelA)

#Confusion Matrix
#LR.FBSCalcModelA.ConfusionMatrix <- table(predicted.FBSComb.CalcModelA, FBS.Comb$FinalRiskincDHS)
#print(LR.FBSCalcModelA.ConfusionMatrix)

#accuracy
#LR.FBSCalcModelA.TestAccuracy <- sum(diag(LR.FBSCalcModelA.ConfusionMatrix)) / sum(LR.FBSCalcModelA.ConfusionMatrix)
#print(LR.FBSCalcModelA.TestAccuracy) 

#Precision
#LR.FBSCalcModelA.TestPrecision <- LR.FBSCalcModelA.ConfusionMatrix[2,2] / sum(LR.FBSCalcModelA.ConfusionMatrix[2,])
#print(LR.FBSCalcModelA.TestPrecision)

#recall
#LR.FBSCalcModelA.TestRecall <- LR.FBSCalcModelA.ConfusionMatrix[2,2]  / sum(LR.FBSCalcModelA.ConfusionMatrix[,2])
#print(LR.FBSCalcModelA.TestRecall)

##Quick analysis for comparing actual performance to predicted performance###
#Percent w potential harm
#LR.FBSCalcModelA.ConfusionMatrix[1,2]/sum(LR.FBSCalcModelA.ConfusionMatrix)
#Percent w No Harm
#(sum(LR.FBSCalcModelA.ConfusionMatrix) - LR.FBSCalcModelA.ConfusionMatrix[1,2])/sum(LR.FBSCalcModelA.ConfusionMatrix)


