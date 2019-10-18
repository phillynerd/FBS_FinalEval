library(tidyverse)
library(skimr)

FBSCurrent<-read.csv(file = "FBSupdate_10-2019_indiv.csv")
summary(FBSCurrent)

tail(FBSCurrent)
skim(FBSCurrent)

#changing variable types
FBSCurrent$MasterBK<-as.character(FBSCurrent$MasterBK)
FBSCurrent$Full_Name<-as.character(FBSCurrent$Full_Name)


FBSCurrent$AfterWrapMulti<-as.factor(FBSCurrent$AfterWrapMulti)
FBSCurrent$BeforeWrapMulti<-as.factor(FBSCurrent$BeforeWrapMulti)
FBSCurrent$DuringWrapMulti<-as.factor(FBSCurrent$DuringWrapMulti)

FBSCurrent$DHSBefore<-as.factor(FBSCurrent$DHSBefore)
FBSCurrent$DHSDuring<-as.factor(FBSCurrent$DHSDuring)
FBSCurrent$DHSAfter<-as.factor(FBSCurrent$DHSAfter)
levels(FBSCurrent$DHSBefore)<-c("not DHS", "DHS")
levels(FBSCurrent$DHSDuring)<-c("not DHS", "DHS")
levels(FBSCurrent$DHSAfter)<-c("not DHS", "DHS")

FBSCurrent$Sum.of.Risk<-as.factor(FBSCurrent$Sum.of.Risk)
FBSCurrent$RiskwoDHS<-as.factor(FBSCurrent$RiskwoDHS)
FBSCurrent$FinalRiskincDHS<-as.factor(FBSCurrent$FinalRiskincDHS)
levels(FBSCurrent$FinalRiskincDHS)<-c("In Community", "Removed From Community")
levels(FBSCurrent$RiskwoDHS)<-c("In Community", "Removed From Community")

FBSCurrent$beforebegin<-as.Date(FBSCurrent$beforebegin,format="%m/%d/%Y")
FBSCurrent$beforeend<-as.Date(FBSCurrent$beforeend,format="%m/%d/%Y")
FBSCurrent$duringbegin<-as.Date(FBSCurrent$duringbegin,format="%m/%d/%Y")
FBSCurrent$duringend<-as.Date(FBSCurrent$duringend,format="%m/%d/%Y")
FBSCurrent$afterbegin<-as.Date(FBSCurrent$afterbegin,format="%m/%d/%Y")
FBSCurrent$afterend<-as.Date(FBSCurrent$afterend,format="%m/%d/%Y")

#creating larger "other" category
#a number of categories that are broken out now are quite small.  BeforeOther1, DuringOther1, and AfterOther1
#all to include the base other category and the following categories collapsed in -> Crisis, CRR, host home, preschool, residential.  

#FamFFT (108 in before but v low in others) left as standalone category, wouldnt include in "during" but would include in before and after

FBSCurrent$BeforeOther1<-(#FBSCurrent$BeforeCrisis+
                            FBSCurrent$BeforeCRR+
                            FBSCurrent$BeforeHH+
                            FBSCurrent$BeforeOther+ 
                            #FBSCurrent$BeforePreschool+ 
                            FBSCurrent$BeforeResidential)

FBSCurrent$DuringOther1<-(#FBSCurrent$DuringCrisis+
                        FBSCurrent$DuringCRR+
                          FBSCurrent$DuringHH+
                          FBSCurrent$DuringOther+ 
                          #FBSCurrent$DuringPreschool+ 
                          FBSCurrent$DuringResidential)

FBSCurrent$AfterOther1<-(#FBSCurrent$AfterCrisis+
                           FBSCurrent$AfterOther)#+
                           #FBSCurrent$AfterPreschool) #Did not include CRR, Host Home, and Residentialin the after category as they are outcomes

#create overall wrap categories and then do boxplots for them

FBSCurrent$BeforeWrapAny <- FBSCurrent$BeforeWrapBS + FBSCurrent$BeforeWrapMT + FBSCurrent$BeforeWrapOther + FBSCurrent$BeforeWrapTSS
FBSCurrent$DuringWrapAny <- FBSCurrent$DuringWrapBS + FBSCurrent$DuringWrapMT + FBSCurrent$DuringWrapOther + FBSCurrent$DuringWrapTSS
FBSCurrent$AfterWrapAny <- FBSCurrent$AfterWrapBS + FBSCurrent$AfterWrapMT + FBSCurrent$AfterWrapOther + FBSCurrent$AfterWrapTSS

#FBS LOS and Dose
FBSCurrent$LOS <- FBSCurrent$duringend-FBSCurrent$duringbegin + 1
FBSCurrent$LOS <-as.integer(FBSCurrent$LOS)

FBSCurrent$Dose<-FBSCurrent$DuringFBS/FBSCurrent$LOS * 100 # N days per 100 days in community

#FBS RaceCat
levels(FBSCurrent$RaceEthnic_Label)
FBSCurrent$RaceCat<- ifelse(FBSCurrent$RaceEthnic_Label == "ASIAN                                   " | FBSCurrent$RaceEthnic_Label == "OTHER                                   " | FBSCurrent$RaceEthnic_Label == "N.AMER.INDIAN/ALASKAN NATIVE            ","Other",
                            ifelse(FBSCurrent$RaceEthnic_Label == "HISPANIC                                ", "Hispanic",
                                   ifelse(FBSCurrent$RaceEthnic_Label == "BLACK OR AFRICAN AMERICAN               ", "Black",
                                          ifelse(FBSCurrent$RaceEthnic_Label == "WHITE                                   ", "White", "Error"))))

FBSCurrent$RaceCat<-as.factor(FBSCurrent$RaceCat)


##*****************************************************************************************************
## FBS.Subset
## Defining a subset with cases that have a full 6 month after period
##*****************************************************************************************************

##*****************************************************************************************************
## FBS.Subset1
## Defining a subset limiting FBS.Subset to just raw input variables
##*****************************************************************************************************
#limiting columns in subset to just potential test variables
# AllTestVariables<-c("FinalRiskincDHS","ageatservicebegin",
#                     "MasterBK", "Gender_Label", "RaceCat", "LOS", "Dose",
#                     "BeforeAIP", "DuringAIP",
#                     "BeforeCRC", "DuringCRC",
#                     "BeforeEval", "DuringEval",
#                     "BeforeFamFFT", "DuringFamFFT",
#                     "BeforeFBS", "DuringFBS",
#                     "BeforeOP", "DuringOP",
#                     "BeforePartial", "DuringPartial",
#                     "BeforeRTF", "DuringRTF",
#                     "BeforeSTS", "DuringSTS",
#                     "BeforeTCM", "DuringTCM",
#                     "BeforeWrapBS", "DuringWrapBS",
#                     "BeforeWrapMT", "DuringWrapMT",
#                     "BeforeWrapTSS", "DuringWrapTSS",
#                     "BeforeWrapOther", "DuringWrapOther",
#                     "BeforeWrapAny", "DuringWrapAny", #total days for ANY BHRS
#                     "BeforeOther1", "DuringOther1",
#                     "BeforeWrapMulti", "DuringWrapMulti", #r they using 0, 1, 2, or 3 Wrap Services
#                     "DHSBefore", "DHSDuring", "DHSAfter") 

FBSCurrent.Comb<- 
as_tibble(FBSCurrent) %>% 
  filter(afterend <= Sys.Date() - 90 ) %>%  #making sure max after date is at least 90 days back
  mutate(StartedAfterPilot = case_when(duringbegin >= as.Date("09/01/2017", format = "%m/%d/%Y") ~ "StartedInPilot",
                                       duringbegin < as.Date("09/01/2017", format = "%m/%d/%Y") & 
                                         duringend > as.Date("09/01/2017", format = "%m/%d/%Y") ~ "OverlapPilot",
                                       TRUE ~ "NotInPilot")) %>%
  filter(StartedAfterPilot != "NotInPilot") %>% 
  select(StartedAfterPilot, FinalRiskincDHS, ageatservicebegin, MasterBK, Gender_Label, RaceCat, LOS, Dose, 
         BeforeAIP, DuringAIP, 
         BeforeCRC, DuringCRC,
         BeforeEval, DuringEval,
         BeforeFBS, DuringFBS, 
         BeforeOP, DuringOP,
         BeforePartial, DuringPartial, 
         BeforeRTF, DuringRTF,
         BeforeSTS, DuringSTS, 
         BeforeTCM, DuringTCM,
         BeforeWrapBS, DuringWrapBS,
         BeforeWrapMT, DuringWrapMT,
         BeforeWrapTSS, DuringWrapTSS,
         BeforeWrapAny, DuringWrapAny,
         BeforeWrapOther, DuringWrapOther,
         BeforeWrapAny, DuringWrapAny,
         BeforeOther1, DuringOther1,
         BeforeWrapMulti, DuringWrapMulti,
         DHSBefore, DHSDuring, DHSAfter) 

FBSCurrent.Comb %>% count(StartedAfterPilot)


#recodes for variables that will remain factors

#creating AIP Category for ease of clinical use
FBSCurrent.Comb$BeforeAIP.Cat <- as.factor(ifelse(FBSCurrent.Comb$BeforeAIP == 0, 0, ifelse(FBSCurrent.Comb$DuringAIP <=30, 1, 2)))
levels(FBSCurrent.Comb$BeforeAIP.Cat) <- c("No AIP", "30 days or less", "More than 30 days")
FBSCurrent.Comb$DuringAIP.Cat <- as.factor(ifelse(FBSCurrent.Comb$DuringAIP == 0, 0, ifelse(FBSCurrent.Comb$DuringAIP <=30, 1, 2)))
levels(FBSCurrent.Comb$DuringAIP.Cat) <- c("No AIP", "30 days or less", "More than 30 days")

#Making CRC 3 categories for ease of clinical use
FBSCurrent.Comb$BeforeCRC <- as.factor(ifelse(FBSCurrent.Comb$BeforeCRC == 0, 0, ifelse(FBSCurrent.Comb$BeforeCRC == 1, 1, 2)))
levels(FBSCurrent.Comb$BeforeCRC)<-c("No days", "1 day", "More than 1 day")
FBSCurrent.Comb$DuringCRC <- as.factor(ifelse(FBSCurrent.Comb$DuringCRC == 0, 0, ifelse(FBSCurrent.Comb$DuringCRC==1, 1, 2)))
levels(FBSCurrent.Comb$DuringCRC)<-c("No days", "1 day", "More than 1 day")

#Making eval binary
FBSCurrent.Comb$BeforeEval <- as.factor(ifelse(FBSCurrent.Comb$BeforeEval == 0, 0, 1))
FBSCurrent.Comb$DuringEval <- as.factor(ifelse(FBSCurrent.Comb$DuringEval == 0, 0, 1))

#FBS Before is a binary (don't think i put this in the models), kept FBS as days for during time period, along with LOS
FBSCurrent.Comb$BeforeFBS.Bin <- as.factor(ifelse(FBSCurrent.Comb$BeforeFBS == 0, 0, 1))

#Creating 3 OP categories (None, Average, High) (cutpoints based on boxplot stats)
FBSCurrent.Comb$BeforeOP.Cat <- as.factor(ifelse(FBSCurrent.Comb$BeforeOP == 0, 0, ifelse(FBSCurrent.Comb$BeforeOP > 0 & FBSCurrent.Comb$BeforeOP <= 12, 1, 2)))
levels(FBSCurrent.Comb$BeforeOP.Cat) <- c("None", "Average", "High")
FBSCurrent.Comb$DuringOP.Cat <- as.factor(ifelse(FBSCurrent.Comb$DuringOP == 0, 0, ifelse(FBSCurrent.Comb$DuringOP >0 & FBSCurrent.Comb$DuringOP <= 5, 1, 2)))
levels(FBSCurrent.Comb$DuringOP.Cat) <- c("None", "Average", "High")

#AlsoCreating OP Binary
FBSCurrent.Comb$BeforeOP.Bin <- as.factor(ifelse(FBSCurrent.Comb$BeforeOP == 0, 0, 1))
FBSCurrent.Comb$DuringOP.Bin <- as.factor(ifelse(FBSCurrent.Comb$DuringOP == 0, 0, 1))

#Making partial binary, usually minimal variation in LOS
FBSCurrent.Comb$BeforePartial <- as.factor(ifelse(FBSCurrent.Comb$BeforePartial == 0, 0, 1))
FBSCurrent.Comb$DuringPartial <- as.factor(ifelse(FBSCurrent.Comb$DuringPartial == 0, 0, 1))

#Making RTF Binary after exploring distributions on days
FBSCurrent.Comb$BeforeRTF <- as.factor(ifelse(FBSCurrent.Comb$BeforeRTF == 0, 0, 1)) #understand these are monstly long LOS
FBSCurrent.Comb$DuringRTF <- as.factor(ifelse(FBSCurrent.Comb$DuringRTF == 0, 0, 1)) #understand these are all under 90 days

##Making WrapCategories based on boxplot stats. put WrapAny back in model with days, just to check
#sum(table(FBSCurrent.Comb$BeforeWrapAny[FBSCurrent.Comb$BeforeWrapAny > 0]))#819 ppl have bhrs before
#summary(FBSCurrent.Comb$BeforeWrapAny[FBSCurrent.Comb$BeforeWrapAny > 0])
#boxplot(FBSCurrent.Comb$BeforeWrapAny[FBSCurrent.Comb$BeforeWrapAny > 0])
#boxplot.stats(FBSCurrent.Comb$BeforeWrapAny[FBSCurrent.Comb$BeforeWrapAny > 0]) # most people under 104 days, 57 outliers
#sum(table(FBSCurrent.Comb$DuringWrapAny[FBSCurrent.Comb$DuringWrapAny > 0])) #556 people have bhrs during
#summary(FBSCurrent.Comb$DuringWrapAny[FBSCurrent.Comb$DuringWrapAny > 0])
#boxplot.stats(FBSCurrent.Comb$DuringWrapAny[FBSCurrent.Comb$DuringWrapAny > 0]) #most people under 47 days,68 outliers )

#based on Wrapany boxplots, creating 3 categories for before and after - none, average, high
FBSCurrent.Comb$BeforeWrap.Cat <- as.factor(ifelse(FBSCurrent.Comb$BeforeWrapAny == 0, 0, ifelse(FBSCurrent.Comb$BeforeWrapAny > 0 & FBSCurrent.Comb$BeforeWrapAny <=50, 2, 3)))
levels(FBSCurrent.Comb$BeforeWrap.Cat) <- c("None", "Average", "High")
FBSCurrent.Comb$DuringWrap.Cat <- as.factor(ifelse(FBSCurrent.Comb$DuringWrapAny == 0, 0, ifelse(FBSCurrent.Comb$DuringWrapAny > 0 & FBSCurrent.Comb$DuringWrapAny <= 20, 2, 3)))
levels(FBSCurrent.Comb$DuringWrap.Cat) <- c("None", "Average", "High")

#creatingLOSquartiles
FBSCurrent.Comb$LOSquart<-as.factor(ifelse(FBSCurrent.Comb$LOS <= 43, 1,ifelse(FBSCurrent.Comb$LOS > 43 & FBSCurrent.Comb$LOS <= 165, 2,ifelse(FBSCurrent.Comb$LOS > 165 & FBSCurrent.Comb$LOS <= 235, 3, 4))))
levels(FBSCurrent.Comb$LOSquart)<- c("43 Days or less", "44 to 165", "166 to 235", "GT 235")

#Creating LOScat - after seeing the results of LRmodel with LOSQuart, it was clear those numbers are weird cutpoints for clinical to use.  Rounded to closest month
#Also created a category for people who have LOS < 30 days just based on the fact that modal use was 1 day and a fair number had 30 days or less.
FBSCurrent.Comb$LOScat<-as.factor(ifelse(FBSCurrent.Comb$LOS < 30, 1,ifelse(FBSCurrent.Comb$LOS >= 30 & FBSCurrent.Comb$LOS <= 120, 2,ifelse(FBSCurrent.Comb$LOS > 121 & FBSCurrent.Comb$LOS <= 240, 3, 4))))
levels(FBSCurrent.Comb$LOScat)<- c("LT 1 month", "30 days to 4 months", "121 days to 8 months", "GT 8 months")

#Making STS Binary
FBSCurrent.Comb$BeforeSTS.Bin <- as.factor(ifelse(FBSCurrent.Comb$BeforeSTS == 0, 0, 1))

#Making TCM Binary  
FBSCurrent.Comb$BeforeTCM.Bin <- as.factor(ifelse(FBSCurrent.Comb$BeforeTCM == 0, 0, 1))








#levels(FBSCurrent$Gender_Label)

###this uses the complete model; sequence below uses just the calculator info

PredictBefore<-predict(FBSComb.LRmodelBefore2,newdata=FBSCurrent.Comb,type='response')
PredictAfter<-predict(FBSComb.LRmodelAfter,newdata=FBSCurrent.Comb,type='response')
FBSCurrent.Comb$PredictBefore<-PredictBefore
FBSCurrent.Comb$PredictAfter<-PredictAfter


##This uses the calculator only models
PredictBefore<-predict(FBSComb.CalcModelB,newdata=FBSCurrent.Comb,type='response')
PredictAfter<-predict(FBSComb.CalcModelA,newdata=FBSCurrent.Comb,type='response')
FBSCurrent.Comb$PredictBefore<-PredictBefore
FBSCurrent.Comb$PredictAfter<-PredictAfter

saveRDS(FBSCurrent.Comb, file = "FBSCurrentComb.rds")
write.csv(FBSCurrent.Comb, file = "FBSCurrentPredictions.csv")



