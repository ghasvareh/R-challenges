setwd("C:/Users/pooya")


HR.AllDataWide<-AllDataWide[
  as.numeric(as.character(AllDataWide$ZSURPS_AS_School_Y1))>1|
    as.numeric(as.character(AllDataWide$ZSURPS_NT_School_Y1))>1| 
    as.numeric(as.character(AllDataWide$ZSURPS_IMP_School_Y1))>1|
    as.numeric(as.character(AllDataWide$ZSURPS_SS_School_Y1))>1
  ,]


#----merge two layers (alcohol use level 4 and 5)

HR.AllDataWide$DEPAPO_ALC_Y1[as.numeric(as.character(HR.AllDataWide$DEPAPO_ALC_Y1))>4]=4
HR.AllDataWide$DEPAPO_ALC_Y2[as.numeric(as.character(HR.AllDataWide$DEPAPO_ALC_Y2))>4]=4
HR.AllDataWide$DEPAPO_ALC_Y3[as.numeric(as.character(HR.AllDataWide$DEPAPO_ALC_Y3))>4]=4
HR.AllDataWide$DEPAPO_ALC_Y4[as.numeric(as.character(HR.AllDataWide$DEPAPO_ALC_Y4))>4]=4
HR.AllDataWide$DEPAPO_ALC_Y5[as.numeric(as.character(HR.AllDataWide$DEPAPO_ALC_Y5))>4]=4


#------sub schoolid with numbers-----
a=substr(HR.AllDataWide$Baseline, 1, 3)
lvl=unique(a[!is.na(a)])
table(a)

for (i in 1:31) {
  a[a==lvl[i]]=i
}

HR.AllDataWide$Baseline=as.numeric(a)

#--------apply the effect of intervention---
intervention = rep(0,nrow(HR.AllDataWide))
intervention<-as.numeric(as.character(intervention))
HR.AllDataWide<-cbind(HR.AllDataWide,intervention)
write.csv(HR.AllDataWide,"HR.AllDataWide.csv")
HR.AllDataWide$intervention

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

HR.AllDataWide<-completeFun(HR.AllDataWide,"Baseline")
for(i in 1:nrow(HR.AllDataWide)){ #nrow(HR.AllDataWide)
  if (HR.AllDataWide$Baseline[[i]]==1)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==2)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==3)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==4)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==5)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==6)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==7)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==8)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==9)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==10)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==11)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==12)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==13)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==14)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==15)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==16)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==17)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==18)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==19)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==20)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==21)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==22)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==23)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==24)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==25)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==26)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==27)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==28)
  {HR.AllDataWide$intervention[[i]]=0}
  if (HR.AllDataWide$Baseline[[i]]==29)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==30)
  {HR.AllDataWide$intervention[[i]]=1}
  if (HR.AllDataWide$Baseline[[i]]==31)
  {HR.AllDataWide$intervention[[i]]=1}
  }

HR.AllDataWide_45<-HR.AllDataWide

write.csv(HR.AllDataWide,"HR.AllDataWide_45.csv")

sum=summary(model3)
View(sum)
sum$coefficients
