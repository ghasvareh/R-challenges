setwd("C:/Users/pooya")


library(reshape2)
library(data.table)


##Measures

Measures_Y1= as.data.frame(cbind(
  School_Y1= as.character(HR.AllDataWide_45$Baseline),
  Gender_Y1=as.numeric(as.character(HR.AllDataWide_45$DEM_01_Y1)),
  int_Y1=as.numeric(as.character(HR.AllDataWide_45$intervention)),
  SURPS_ANX_Y1=HR.AllDataWide_45$SURPS_echelle_TotalAnxiete_Y1,
  SURPS_IMP_Y1=HR.AllDataWide_45$SURPS_echelle_TotalImpulsivite_Y1,
  SURPS_NEG_Y1=HR.AllDataWide_45$SURPS_echelle_TotalPenseesNegatives_Y1,
  SURPS_SENS_Y1=HR.AllDataWide_45$SURPS_echelle_TotalSensationsFortes_Y1,
  HASH_Use_Y1= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y1)),
  HASH_Base_Y1= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y1))
 
))

Measures_Y2= as.data.frame(cbind(
  School_Y2= as.character(HR.AllDataWide_45$Baseline),
  Gender_Y2=as.numeric(as.character(HR.AllDataWide_45$DEM_01_Y1)),
  int_Y2=as.numeric(as.character(HR.AllDataWide_45$intervention)),
  SURPS_ANX_Y2=HR.AllDataWide_45$SURPS_echelle_TotalAnxiete_Y1,
  SURPS_IMP_Y2=HR.AllDataWide_45$SURPS_echelle_TotalImpulsivite_Y1,
  SURPS_NEG_Y2=HR.AllDataWide_45$SURPS_echelle_TotalPenseesNegatives_Y1,
  SURPS_SENS_Y2=HR.AllDataWide_45$SURPS_echelle_TotalSensationsFortes_Y1,
  HASH_Use_Y2= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y2)),
  HASH_Base_Y1= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y1))
  
))

Measures_Y3= as.data.frame(cbind(
  School_Y3= as.character(HR.AllDataWide_45$Baseline),
  Gender_Y3=as.numeric(as.character(HR.AllDataWide_45$DEM_01_Y1)),
  int_Y3=as.numeric(as.character(HR.AllDataWide_45$intervention)),
  SURPS_ANX_Y3=HR.AllDataWide_45$SURPS_echelle_TotalAnxiete_Y1,
  SURPS_IMP_Y3=HR.AllDataWide_45$SURPS_echelle_TotalImpulsivite_Y1,
  SURPS_NEG_Y3=HR.AllDataWide_45$SURPS_echelle_TotalPenseesNegatives_Y1,
  SURPS_SENS_Y3=HR.AllDataWide_45$SURPS_echelle_TotalSensationsFortes_Y1,
  HASH_Use_Y3= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y3)),
  HASH_Base_Y1= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y1))
 
))

Measures_Y4= as.data.frame(cbind(
  School_Y4= as.character(HR.AllDataWide_45$Baseline),
  Gender_Y4=as.numeric(as.character(HR.AllDataWide_45$DEM_01_Y1)),
  int_Y4=as.numeric(as.character(HR.AllDataWide_45$intervention)),
  SURPS_ANX_Y4=HR.AllDataWide_45$SURPS_echelle_TotalAnxiete_Y1,
  SURPS_IMP_Y4=HR.AllDataWide_45$SURPS_echelle_TotalImpulsivite_Y1,
  SURPS_NEG_Y4=HR.AllDataWide_45$SURPS_echelle_TotalPenseesNegatives_Y1,
  SURPS_SENS_Y4=HR.AllDataWide_45$SURPS_echelle_TotalSensationsFortes_Y1,
  HASH_Use_Y4= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y4)),
  HASH_Base_Y1= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y1))
 
))

Measures_Y5= as.data.frame(cbind(
  School_Y5= as.character(HR.AllDataWide_45$Baseline),
  Gender_Y5=as.numeric(as.character(HR.AllDataWide_45$DEM_01_Y1)),
  int_Y5=as.numeric(as.character(HR.AllDataWide_45$intervention)),
  SURPS_ANX_Y5=HR.AllDataWide_45$SURPS_echelle_TotalAnxiete_Y1,
  SURPS_IMP_Y5=HR.AllDataWide_45$SURPS_echelle_TotalImpulsivite_Y1,
  SURPS_NEG_Y5=HR.AllDataWide_45$SURPS_echelle_TotalPenseesNegatives_Y1,
  SURPS_SENS_Y5=HR.AllDataWide_45$SURPS_echelle_TotalSensationsFortes_Y1,
  HASH_Use_Y5= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y5)),
  HASH_Base_Y1= as.numeric(as.character(HR.AllDataWide_45$DEPAPO_HASH_Y1))
 
))
## Measures
Measures= as.data.frame(cbind(
  Measures_Y1= Measures_Y1,
  Measures_Y2= Measures_Y2,
  Measures_Y3= Measures_Y3,
  Measures_Y4= Measures_Y4,
  Measures_Y5= Measures_Y5
))
##AGE
Age = as.data.frame(cbind(
  Age_Y1=as.numeric(as.character(HR.AllDataWide_45$AgeAtTesting_Y1)),
  Age_Y2=as.numeric(as.character(HR.AllDataWide_45$AgeAtTesting_Y2)),
  Age_Y3=as.numeric(as.character(HR.AllDataWide_45$AgeAtTesting_Y3)),
  Age_Y4=as.numeric(as.character(HR.AllDataWide_45$AgeAtTesting_Y4)),
  Age_Y5=as.numeric(as.character(HR.AllDataWide_45$AgeAtTesting_Y5))
))

library(data.table)

### Convert To Long

## Assemble
Data_wide_Pooya_HR = as.data.frame(cbind(Age,Measures))

#Add ID variable
Data_wide_Pooya_HR$ID=1:nrow(Data_wide_Pooya_HR)

colnames(Data_wide_Pooya_HR)
Data_long_Pooya_HR=melt(setDT(Data_wide_Pooya_HR), id="ID", 
                        measure=patterns("Age_", "School_","Gender_","int_",
                                         "SURPS_ANX_", "SURPS_IMP_", "SURPS_NEG_", "SURPS_SENS_","HASH_Use_","HASH_Base")
                        ,variable.name="Year"
                        , value.name=c("Age","School","Sex","int", 
                                       "SURPS_ANX","SURPS_IMP","SURPS_NEG","SURPS_SENS","HASH_Use","HASH_Use_Base")
                        ,value.factor=TRUE, na.rm=F)

write.csv(Data_long_Pooya_HR,'Data_long_Pooya_HR_11.csv')
