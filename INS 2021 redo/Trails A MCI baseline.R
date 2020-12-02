# Trails A 
#Basline DX= MCI based on Jak/Bondi.
#12 month retest window
# was log transformed in script b/c of non-normal distribution.
# was then exp back to normal at end of script. 


##################################

library(readr)
library(psych)
library(dplyr)
library(haven)
library(readr)

{

    #create databases by variable. merge by id and exam date? 
  NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
  NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
  ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
  trails.V1=(DXs.2)
  trails.V2=(DXs.m12)
  
  #create attrition sub database [= any subs with ONLY baseline]
  notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
  potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
  Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  
  #Baseline 
  TRA_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTETHCAT', 'DX_bl','VISCODE'))
  
  TRA_baseline$match=paste(TRA_baseline$RID, sep='_', TRA_baseline$EXAMDATE)
  TRA_baseline=merge(TRA_baseline, ANART, by = 'RID', all.x = TRUE)
  TRA_baseline=merge(TRA_baseline, trails.V1, by = 'RID', all.x = TRUE)
  
  #TRA_baseline$TRAASCOR.log=log(TRA_baseline$TRAASCOR)
  TRA_baseline$TRAASCOR=log(TRA_baseline$TRAASCOR+10)
  TRA_baseline$ANARTERR.log=log(TRA_baseline$ANARTERR+1)
  TRA_baseline=TRA_baseline[!is.na(TRA_baseline$TRAASCOR),] #removes NA from the cog variable of interest
  TRA_baseline=TRA_baseline[!is.na(TRA_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score
  TRA_baseline$anart.zscore=scale(TRA_baseline$ANARTERR.log)
  #residualize cog variable to education 
  TRA_baseline$PTEDUCAT.zscore=scale(TRA_baseline$PTEDUCAT)
  
  TRA_baseline.MCI=subset(TRA_baseline, MCI_any=='1')
  TRA_baseline.MCI=subset(TRA_baseline.MCI, NP_DX!='2')
  TRA_baseline_MCIs_ID=subset(TRA_baseline.MCI, select='RID')
  
  #Follow up (06 months is first)
  TRA_followup=subset(Subject_selection_V1, VISCODE=='m12',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTEDUCAT','PTETHCAT','DX','VISCODE'))
  #TRA_followup$TRA_followup=TRA_followup$TRAASCOR*-1
  TRA_followup=merge(TRA_followup, ANART, by = 'RID', all.x = TRUE)
  TRA_followup=merge(TRA_followup, trails.V2, by = 'RID', all.x = TRUE)
  
  TRA_followup$ANARTERR.log=log(TRA_followup$ANARTERR+10)
  TRA_followup$TRAASCOR=log(TRA_followup$TRAASCOR+1)
  
  TRA_followup=TRA_followup[!is.na(TRA_followup$TRAASCOR),] #removes NA from the cog variable of interest
  TRA_followup=TRA_followup[!is.na(TRA_followup$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score 
  TRA_followup$TRAASCOR.anart.zscore=scale(TRA_followup$ANARTERR.log)

  TRA_followup$PTEDUCAT.zscore=scale(TRA_followup$PTEDUCAT)

  TRA_followup$age_fu=TRA_followup$AGE+(TRA_followup$Month_bl/12)
  TRA_followup$match=paste(TRA_followup$RID, sep='_', TRA_followup$EXAMDATE)
  TRA_followup.ID=subset(TRA_followup, select = 'RID')
  
  #Returnee database creation 
  TRA_returnee.MCI_baseline.ID=merge(TRA_followup.ID, TRA_baseline_MCIs_ID, by='RID')
  TRA_returnee.MCI_baseline.DATA.V2=merge(TRA_returnee.MCI_baseline.ID, TRA_followup, by='RID')
  #returnee.baseline
  TRA_returnee.MCI_baseline.DATA.V1=merge(TRA_baseline.MCI, TRA_returnee.MCI_baseline.ID, by='RID')
  #create database for standardizeing. excludes anyone who is AD
  TRA_baseline.NOTAD=subset(TRA_baseline, NP_DX!='2')
  

}
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################


                #to set up blanks for loop###
{PE=0

TRA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
anart_TRA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
edu_TRA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
ZRAW_TRA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
RAW_TRA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
mean_TRA.bootstrap=data.frame('baseline'=1000,
                              'baseline.SD'=1000,
                              'returnee.SD'=1000,
                              'returnee.baseline'=1000,
                              'returnee.followup'=1000, 
                              'replcacement'=1000,
                              'returnee.base.sd'=1000,
                              'replcacement.SD'=1000)
RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)}


databaselist.returnee.followup=list()
databaselist.replcaements=list()
databaselist.basleine=list()
databaselist.returnee.baseline=list()



################################
while (PE<5000) {
  restart=FALSE
  
    #425 subs in the TRB returneee pool. Limiting to half that for the calcualtion of PE
  r.sample.returnees <- TRA_returnee.MCI_baseline.DATA.V2[sample(nrow(TRA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
  TRA_potential_PR=TRA_baseline.MCI[-which(TRA_baseline.MCI$RID %in% r.sample.returnees$RID),]
  #  TRA_potential_PR=TRA_potential_PR[-which(TRA_potential_PR$RID %in% Attrition_baseline$RID),]
  describe(TRA_potential_PR$RID)
  
  age_bounds=describe(r.sample.returnees$age_fu)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  TRA_potential_PR$keep=ifelse(TRA_potential_PR$AGE>u_bound,
                               -1,
                               ifelse(TRA_potential_PR$AGE<l_bound,
                                      -1,1))
  x=subset(TRA_potential_PR, keep==1) #removes anyone outside age range
  describe(x$AGE)
  count<-0
  count.4=0
  count.5=0
  count.low=0
  count.high=0
  total.count=0
  
  success <- 0
  pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size
  while (success==0) {
    # random select sub, add to database
    if (mean(pseudo.R.subs$AGE)>mean(r.sample.returnees$age_fu)) {
      n=subset(x, AGE<mean(r.sample.returnees$age_fu))
      i=sample(n$RID, 1)
      count.low=count.low+1
    }
    if (mean(pseudo.R.subs$AGE)<mean(r.sample.returnees$age_fu)) {
      m=subset(x, AGE>mean(r.sample.returnees$age_fu))
      i=sample(m$RID, 1) 
      count.high=count.high+1
    }
    pseudo.R.subs.temp=subset(x, RID==i) #random select sub row
    #add the new subs to a continually growing dataset 
    pseudo.R.subs=rbind(pseudo.R.subs.temp, pseudo.R.subs) #binds sub row to growing dataset
    pseudo.R.subs=(pseudo.R.subs %>% distinct())#removes a subject if repeated 
    age_dif=t.test(pseudo.R.subs$AGE, r.sample.returnees$age_fu) #looks for sig difference between age values 
    edu_diff=t.test(pseudo.R.subs$PTEDUCAT, r.sample.returnees$PTEDUCAT) #looks for sig difference between edu values 
    anart_diff=t.test(pseudo.R.subs$ANARTERR, r.sample.returnees$ANARTERR)
    success.1=ifelse(edu_diff$p.value>.7, 1, 0) #how dif the edu can be. 
    success.2=ifelse(age_dif$p.value>.8, 1, 0) #how dif the ages can be. 
    success.3=ifelse(anart_diff$p.value>.7, 1, 0) #how dif the anart can be. 
    
    
    pr.sex=as.factor(pseudo.R.subs$PTGENDER)
    re.sex=as.factor(r.sample.returnees$PTGENDER)
    n <- max(length(pr.sex), length(re.sex))
    length(re.sex) <- n                      
    length(pr.sex) <- n
    sex.temp=as.data.frame(cbind(pr.sex, re.sex))
    sex.test=chisq.test(as.factor(sex.temp$pr.sex), as.factor(sex.temp$re.sex))
    success.sex=ifelse(sex.test$p.value>.7, 1, 0) #how dif the anart can be. 
    
    
    success.4=ifelse(nrow(pseudo.R.subs)>(120), 1, 0)# sets maximum of subs
    success.5=ifelse(nrow(pseudo.R.subs)<80, 1, 0)# minimum number of subs
    #final check to see if leave loop
    #success=ifelse(success.2==1 & success.3==1, 1, 0) #final check to see if leave loop
    success=ifelse(success.1==1 & success.2==1 & success.3==1 &success.sex==1, 1, 0) #final check to see if leave loop
    #  k=as.numeric(sample(x=(row.names(pseudo.R.subs)), size=1))
    #  if (success==0) {
    #    pseudo.R.subs=pseudo.R.subs[-c(k),]
    #  } #removes 1 random subs if not meeting success conditions 
    count=(count+1)
    if (count>80) {
      r.sample.returnees <- TRA_returnee.MCI_baseline.DATA.V2[sample(nrow(TRA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      TRA_potential_PR=TRA_baseline.MCI[-which(TRA_baseline.MCI$RID %in% r.sample.returnees$RID),]
      TRA_potential_PR=TRA_potential_PR[-which(TRA_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      TRA_potential_PR$keep=ifelse(TRA_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(TRA_potential_PR$AGE<l_bound,
                                          -1,1))
      x=subset(TRA_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      
      count=0
      total.count=total.count+1
      ptemp=c('fail', total.count)
      print(ptemp)
    }
    if (success.4==1){
      r.sample.returnees <- TRA_returnee.MCI_baseline.DATA.V2[sample(nrow(TRA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      TRA_potential_PR=TRA_baseline.MCI[-which(TRA_baseline.MCI$RID %in% r.sample.returnees$RID),]
      TRA_potential_PR=TRA_potential_PR[-which(TRA_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      TRA_potential_PR$keep=ifelse(TRA_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(TRA_potential_PR$AGE<l_bound,
                                          -1,1))
      x=subset(TRA_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count.4=count.4+1
      success=0
      total.count=total.count+1
      ptemp=c('over120', total.count)
      print(ptemp)
    }
    if (success.5==1){
      r.sample.returnees <- TRA_returnee.MCI_baseline.DATA.V2[sample(nrow(TRA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      TRA_potential_PR=TRA_baseline.MCI[-which(TRA_baseline.MCI$RID %in% r.sample.returnees$RID),]
      TRA_potential_PR=TRA_potential_PR[-which(TRA_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      TRA_potential_PR$keep=ifelse(TRA_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(TRA_potential_PR$AGE<l_bound,
                                          -1,1))
      x=subset(TRA_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      success=0
      total.count=total.count+1
      ptemp=c('under80', total.count)
      print(ptemp)
      
    }
    if (total.count>150) {
      restart=TRUE
      failtext='UNABLE TO MATCH REPlACMENTS'
      print(failtext)
      success=1
    }
  }
  #make sure that replacement subs are not in baseline data set. 
  test=TRA_baseline.MCI
  pseudo.R.subs.ID=as.list(pseudo.R.subs$match)
  test=test[-which(test$match %in% pseudo.R.subs.ID),]
  TRA_baseline.MCI.withoutPR=test
  r.sample.returnees.ID=subset(r.sample.returnees, select='RID')
  
  #database
  baseline=TRA_baseline.MCI.withoutPR #does not include attrition replacements
  returnee.baseline=merge(TRA_returnee.MCI_baseline.DATA.V1, r.sample.returnees.ID, by = "RID")
  returnee.followup=r.sample.returnees    
  replcaements=pseudo.R.subs #matched on age and edu 
  Attrition_baseline=Attrition_baseline
  
  
  #creates baseline that is similar to returnee baseline
  age_bounds=describe(returnee.baseline$AGE)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  LM_temp_baseline=baseline
  LM_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                               -1,
                               ifelse(baseline$AGE<l_bound,
                                      -1,1))
  y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
  baseline.match=0
  count=0
  total.count=0
  LM_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
  while (baseline.match==0) {

     # random select sub, add to database
    if (mean(LM_matched_baseline$AGE)>mean(returnee.baseline$AGE)) {
      n=subset(y, AGE<mean(returnee.baseline$AGE))
      i=sample(n$RID, 1)
      count.low=count.low+1
    }
    if (mean(LM_matched_baseline$AGE)<mean(returnee.baseline$AGE)) {
      m=subset(y, AGE>mean(returnee.baseline$AGE))
      i=sample(m$RID, 1) 
      count.high=count.high+1
    }
    LM_matched_baseline.temp=subset(y, RID==i) #random select sub row
    #add the new subs to a continually growing dataset 
    LM_matched_baseline=rbind(LM_matched_baseline.temp, LM_matched_baseline) #binds sub row to growing dataset
    LM_matched_baseline=(LM_matched_baseline %>% distinct())#removes a subject if repeated 
    
    age_dif=t.test(LM_matched_baseline$AGE, returnee.baseline$AGE) #looks for sig difference between age values 
    edu_diff=t.test(LM_matched_baseline$PTEDUCAT, returnee.baseline$PTEDUCAT) #looks for sig difference between edu values 
    anart_diff=t.test(LM_matched_baseline$ANARTERR, returnee.baseline$ANARTERR)
    baseline.match.2=ifelse(age_dif$p.value>.9, 1, 0) #how dif the ages can be. 
    baseline.match.4=ifelse(nrow(LM_matched_baseline)>(120), 1, 0)# sets maximum of subs
    baseline.match.5=ifelse(nrow(LM_matched_baseline)<80, 1, 0)# minimum number of subs
    #final check to see if leave loop
    #baseline.match=ifelse(baseline.match.2==1 & baseline.match.3==1, 1, 0) #final check to see if leave loop
    baseline.match=ifelse(baseline.match.2==1, 1, 0) #final check to see if leave loop
    
    count=(count+1)
    if (count>50) {
      LM_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound,
                                          -1,1))
      y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      LM_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count=0
      ptemp=c('baseline fail', total.count)
      total.count=total.count+1
      
      print(ptemp)
    }
    if (baseline.match.4==1){
      LM_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound,
                                          -1,1))
      y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      LM_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count.4=count.4+1
      baseline.match=0
      ptemp=c('baseline over n', total.count)
      total.count=total.count+1
      
      print(ptemp)
    }
    if (baseline.match.5==1){
      LM_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound,
                                          -1,1))
      y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      LM_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count.5=count.5+1
      baseline.match=0
      ptemp=c('baseline under n', total.count)
      total.count=total.count+1
      
      print(ptemp)
    }
    if (restart==TRUE |total.count>150) {
      baseline.match=1
      failtext='UNABLE TO MATCH BASELINE'
      print(failtext)
      restart=TRUE
    }

  }

while (restart==FALSE) {
  

  baseline=LM_matched_baseline
  #standardize Raw scores for Z score
  #create database for standardizeing. excludes anyone who is AD
  TRA_baseline.NOTAD=subset(TRA_baseline, DX_bl!='AD')
  #use below if want to base standardizeation on FULL sample at baseline, minus AD subs. 
  #baseline.mean=mean(TRA_baseline.NOTAD$TRAASCOR)
  #baseline.SD=SD(TRA_baseline.NOTAD$TRAASCOR)
  # use below if want to base standardization on only people with this DX, in this sample
  baseline.mean=mean(baseline$TRAASCOR)
  baseline.SD=SD(baseline$TRAASCOR)
  
  baseline$TRAASCOR.Z=(baseline$TRAASCOR-baseline.mean)/baseline.SD
  returnee.baseline$TRAASCOR.Z=(returnee.baseline$TRAASCOR-baseline.mean)/baseline.SD
  returnee.followup$TRAASCOR.Z=(returnee.followup$TRAASCOR-baseline.mean)/baseline.SD
  replcaements$TRAASCOR.Z=(replcaements$TRAASCOR-baseline.mean)/baseline.SD

  
  #calculate PE for RAW Zscores, no resid for edu or anart
  RAW_TRA_MCI_Difference_score=exp(mean(returnee.followup$TRAASCOR))-exp(mean(replcaements$TRAASCOR))
  RAW_TRA_MCI_Attrition_effect=exp(mean(returnee.baseline$TRAASCOR))-exp(mean(baseline$TRAASCOR))
  RAW_TRA_MCI_Practice_effect=RAW_TRA_MCI_Difference_score-RAW_TRA_MCI_Attrition_effect
  
  #combine bootstrapped results   
  RAW_TRA_MCI_PE.bootstrap=rbind(RAW_TRA_MCI_PE.bootstrap, RAW_TRA_MCI_Practice_effect)
  RAW_attritioneffect.bootstrap=rbind(RAW_attritioneffect.bootstrap, RAW_TRA_MCI_Attrition_effect)
  
  baseline$TRAASCOR=exp(baseline$TRAASCOR)-10
  returnee.baseline$TRAASCOR=exp(returnee.baseline$TRAASCOR)-10
  returnee.followup$TRAASCOR=exp(returnee.followup$TRAASCOR)-10
  replcaements$TRAASCOR=exp(replcaements$TRAASCOR)-10
  mean_TRA.temp=data.frame('baseline'=mean(baseline$TRAASCOR),
                           'baseline.SD'=SD(baseline$TRAASCOR),
                           'returnee.SD'=SD(returnee.followup$TRAASCOR),
                           'returnee.baseline'=mean(returnee.baseline$TRAASCOR),
                           'returnee.followup'=mean(returnee.followup$TRAASCOR), 
                           'replcacement'=mean(replcaements$TRAASCOR),
                           'returnee.base.sd'=SD(returnee.baseline$TRAASCOR),
                           'replcacement.SD'=SD(replcaements$TRAASCOR))
  
  mean_TRA.bootstrap=rbind(mean_TRA.bootstrap,mean_TRA.temp )
  PE=PE+1
  print('###########################################')
  print('###########################################')
  print('###########################################')
  yay=c('Succesful match, PE=', PE)
  print(yay)
  print(PE)
  print(PE)
  print(PE)
  print(PE)
  print('###########################################')
  print('###########################################')
  print('###########################################')
  
  

  databaselist.returnee.followup[[PE]]=returnee.followup
  databaselist.replcaements[[PE]]=replcaements
  databaselist.basleine[[PE]]=baseline
  databaselist.returnee.baseline[[PE]]=returnee.followup
  
  restart=TRUE
  }
}

RAW_TRA_MCI_PE.bootstrap=subset(RAW_TRA_MCI_PE.bootstrap, Pract.effect!=1000)
RAW_attritioneffect.bootstrap=subset(RAW_attritioneffect.bootstrap, Pract.effect!=1000)
mean_TRA.bootstrap=subset(mean_TRA.bootstrap,  baseline!=1000 & returnee.baseline!=1000 &
                            returnee.followup!=1000 & replcacement!=1000 & baseline.SD!=1000
                          & returnee.base.sd!=1000)
describe(mean_TRA.bootstrap)
describe(RAW_TRA_MCI_PE.bootstrap)
describe(RAW_attritioneffect.bootstrap$Pract.effect)






