

#database set up 
{
  #create databases by variable. merge by id and exam date? 
#  NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
#  NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
#  ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
  
  
  #create attrition sub database [= any subs with ONLY baseline]
 # notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
#  potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
#  Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  
  #create databases by variable. merge by id and exam date? 
  NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
  NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
  ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
  
  
  #create attrition sub database [= any subs with ONLY baseline]
  notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
  potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
  Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  
  #Baseline 
  TRB_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'DX_bl','VISCODE'))
  TRB_baseline=merge(TRB_baseline, DXs.2, by = 'RID') #DXs.2 has Trails A in it. 
  TRB_baseline$match=paste(TRB_baseline$RID, sep='_', TRB_baseline$EXAMDATE)
  TRB_baseline=merge(TRB_baseline, ANART, by = 'RID', all.x = TRUE)
  
  
  TRB_baseline$TRABSCOR.log=log(TRB_baseline$TRABSCOR)
  TRB_baseline$TRABSCOR=log(TRB_baseline$TRABSCOR+10)
  
  #TRB_baseline$TRABSCOR=TRB_baseline$TRABSCOR*-1
  
  TRB_baseline$ANARTERR.log=log(TRB_baseline$ANARTERR+1)
  TRB_baseline=TRB_baseline[!is.na(TRB_baseline$TRABSCOR),] #removes NA from the cog variable of interest
  TRB_baseline=TRB_baseline[!is.na(TRB_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score
  TRB_baseline$anart.zscore=scale(TRB_baseline$ANARTERR.log)
  #TRB_baseline=subset(TRB_baseline, anart.zscore>1)
  TRABSCOR.anart.resid=lm(TRABSCOR~anart.zscore,data=TRB_baseline)
  TRB_baseline$TRABSCOR.anart.resid=(residuals(TRABSCOR.anart.resid)+TRABSCOR.anart.resid$coefficients[1])
  #residualize cog variable to education 
  TRB_baseline$PTEDUCAT.zscore=scale(TRB_baseline$PTEDUCAT)
  TRABSCOR.edu.resid=lm(TRABSCOR~PTEDUCAT.zscore,data=TRB_baseline)
  TRB_baseline$TRABSCOR.edu.resid=(residuals(TRABSCOR.edu.resid)+TRABSCOR.edu.resid$coefficients[1])
  
  TRB_baseline.MCI_BL=subset(TRB_baseline, MCI_any==1)
  TRB_baseline.MCI_BL=subset(TRB_baseline.MCI_BL, NP_DX!='2')
  TRB_baseline_MCI_ID=subset(TRB_baseline.MCI_BL, select='RID')
  
  #Follow up (06 months is first)
  TRB_followup=subset(Subject_selection_V1, VISCODE=='m12',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTEDUCAT','PTETHCAT','DX','VISCODE'))
  TRB_followup=merge(TRB_followup, DXs.m12, by = 'RID')
  TRB_followup$TRABSCOR=log(TRB_followup$TRABSCOR+10)
  
  #TRB_followup$TRB_followup=TRB_followup$TRABSCOR*-1
  TRB_followup=merge(TRB_followup, ANART, by = 'RID', all.x = TRUE)
  TRB_followup$ANARTERR.log=log(TRB_followup$ANARTERR+1)
  
  TRB_followup=TRB_followup[!is.na(TRB_followup$TRABSCOR),] #removes NA from the cog variable of interest
  TRB_followup=TRB_followup[!is.na(TRB_followup$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score 
  TRB_followup$TRABSCOR.anart.zscore=scale(TRB_followup$ANARTERR.log)
  #TRB_followup=subset(TRB_followup, anart.zscore>1)
  TRABSCOR.anart.resid=lm(TRABSCOR~TRABSCOR.anart.zscore,data=TRB_followup)
  TRB_followup$TRABSCOR.anart.resid=(residuals(TRABSCOR.anart.resid)+TRABSCOR.anart.resid$coefficients[1])
  #residualize cog variable to education
  TRB_followup$PTEDUCAT.zscore=scale(TRB_followup$PTEDUCAT)
  TRABSCOR.edu.resid=lm(TRABSCOR~PTEDUCAT.zscore,data=TRB_followup)
  TRB_followup$TRABSCOR.edu.resid=(residuals(TRABSCOR.edu.resid)+TRABSCOR.edu.resid$coefficients[1])
  
  TRB_followup$age_fu=TRB_followup$AGE+(TRB_followup$Month_bl/12)
  TRB_followup$match=paste(TRB_followup$RID, sep='_', TRB_followup$EXAMDATE)
  TRB_followup.ID=subset(TRB_followup, select = 'RID')
  
  #Returnee database creation 
  TRB_returnee.MCI_baseline.ID=merge(TRB_followup.ID, TRB_baseline_MCI_ID, by='RID')
  TRB_returnee.MCI_baseline.DATA.V2=merge(TRB_returnee.MCI_baseline.ID, TRB_followup, by='RID')
  #returnee.baseline
  TRB_returnee.MCI_baseline.DATA.V1=merge(TRB_baseline.MCI_BL, TRB_returnee.MCI_baseline.ID, by='RID')
  #create database for standardizeing. excludes anyone who is AD
  TRB_baseline.NOTAD=subset(TRB_baseline, NP_DX!='2')
  

}
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################


                                #to set up blanks for loop###

PE=0

TRB_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
RAW_TRB_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
RAW_TRB_MCI_Difference_score.bootstrap=data.frame(Pract.effect=1000)
mean_TRB.bootstrap=data.frame('baseline'=1000,
                              'baseline.SD'=1000,
                              'returnee.SD'=1000,
                              'returnee.baseline'=1000,
                              'returnee.followup'=1000, 
                              'replcacement'=1000,
                              'returnee.base.sd'=1000,
                              'replcacement.SD'=1000)
RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)


databaselist.returnee.followup=list()
databaselist.replcaements=list()
databaselist.basleine=list()
databaselist.returnee.baseline=list()

library(readr)
library(psych)
library(dplyr)
library(haven)


while (PE<5000) {
  restart=FALSE
  
  #425 subs in the TRB returneee pool. Limiting to half that for the calcualtion of PE
  r.sample.returnees <- TRB_returnee.MCI_baseline.DATA.V2[sample(nrow(TRB_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
  TRB_potential_PR=TRB_baseline.MCI_BL[-which(TRB_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
  #TRB_potential_PR=TRB_potential_PR[-which(TRB_potential_PR$RID %in% Attrition_baseline$RID),]
  describe(TRB_potential_PR$RID)
  
  age_bounds=describe(r.sample.returnees$age_fu)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  TRB_potential_PR$keep=ifelse(TRB_potential_PR$AGE>u_bound,
                               -1,
                               ifelse(TRB_potential_PR$AGE<l_bound,
                                      -1,1))
  x=subset(TRB_potential_PR, keep==1) #removes anyone outside age range
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
      n=subset(TRB_potential_PR, AGE<mean(r.sample.returnees$age_fu))
      i=sample(n$RID, 1)
      count.low=count.low+1
    }
    if (mean(pseudo.R.subs$AGE)<mean(r.sample.returnees$age_fu)) {
      m=subset(TRB_potential_PR, AGE>mean(r.sample.returnees$age_fu))
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
    
    success.4=ifelse(nrow(pseudo.R.subs)>(120), 1, 0)# sets maximum of subs
    success.5=ifelse(nrow(pseudo.R.subs)<80, 1, 0)# minimum number of subs
    
    
    
    pr.sex=as.factor(pseudo.R.subs$PTGENDER)
    re.sex=as.factor(r.sample.returnees$PTGENDER)
    n <- max(length(pr.sex), length(re.sex))
    length(re.sex) <- n                      
    length(pr.sex) <- n
    sex.temp=as.data.frame(cbind(pr.sex, re.sex))
    sex.test=chisq.test(as.factor(sex.temp$pr.sex), as.factor(sex.temp$re.sex))
    success.sex=ifelse(sex.test$p.value>.7, 1, 0) #how dif the anart can be. 
    
    #final check to see if leave loop
    #success=ifelse(success.2==1 & success.3==1, 1, 0) #final check to see if leave loop
    success=ifelse(success.1==1 & success.2==1 & success.3==1 & success.sex==1, 1, 0) #final check to see if leave loop
    #  k=as.numeric(sample(x=(row.names(pseudo.R.subs)), size=1))
    #  if (success==0) {
    #    pseudo.R.subs=pseudo.R.subs[-c(k),]
    #  } #removes 1 random subs if not meeting success conditions 
    
    
    
    count=(count+1)
    if (count>30) {
      r.sample.returnees <- TRB_returnee.MCI_baseline.DATA.V2[sample(nrow(TRB_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      TRB_potential_PR=TRB_baseline.MCI_BL[-which(TRB_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #TRB_potential_PR=TRB_potential_PR[-which(TRB_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      TRB_potential_PR$keep=ifelse(TRB_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(TRB_potential_PR$AGE<l_bound,
                                          -1,1))
      x=subset(TRB_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count=0
      total.count=total.count+1
      ptemp=c('fail', total.count)
      print(ptemp)
    }
    if (success.4==1){
      r.sample.returnees <- TRB_returnee.MCI_baseline.DATA.V2[sample(nrow(TRB_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      TRB_potential_PR=TRB_baseline.MCI_BL[-which(TRB_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #TRB_potential_PR=TRB_potential_PR[-which(TRB_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      TRB_potential_PR$keep=ifelse(TRB_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(TRB_potential_PR$AGE<l_bound,
                                          -1,1))
      x=subset(TRB_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count.4=count.4+1
      success=0
      total.count=total.count+1
      ptemp=c('over120', total.count)
      print(ptemp)
    }
    if (success.5==1){
      r.sample.returnees <- TRB_returnee.MCI_baseline.DATA.V2[sample(nrow(TRB_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      TRB_potential_PR=TRB_baseline.MCI_BL[-which(TRB_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #TRB_potential_PR=TRB_potential_PR[-which(TRB_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      TRB_potential_PR$keep=ifelse(TRB_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(TRB_potential_PR$AGE<l_bound,
                                          -1,1))
      x=subset(TRB_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      success=0
      total.count=total.count+1
      ptemp=c('under80', total.count)
      print(ptemp)
      
    }
    if (total.count>150) {
      restart=TRUE
      success=1
      failtext='UNABLE TO MATCH REPlACMENTS'
      print(failtext)
      
    }
  }
  describe(pseudo.R.subs$AGE)
  
  #make sure that replacement subs are not in baseline data set. 
  test=TRB_baseline.MCI_BL
  pseudo.R.subs.ID=as.list(pseudo.R.subs$match)
  test=test[-which(test$match %in% pseudo.R.subs.ID),]
  TRB_baseline.MCI_BL.withoutPR=test
  r.sample.returnees.ID=subset(r.sample.returnees, select='RID')
  
  #database
  baseline=TRB_baseline.MCI_BL.withoutPR #does not include attrition replacements
  returnee.baseline=merge(TRB_returnee.MCI_baseline.DATA.V1, r.sample.returnees.ID, by = "RID")
  returnee.followup=r.sample.returnees    
  replcaements=pseudo.R.subs#matched on age and edu 
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
  baseline.match=0
  baseline.match.1=0
  baseline.match.2=0
  baseline.match.3=0
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
                                   ifelse(baseline$AGE<l_bound+age_bounds,
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
                                   ifelse(baseline$AGE<l_bound+age_bounds,
                                          -1,1))
      y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      LM_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count.4=count.4+1
      ptemp=c('baseline over n', total.count)
      total.count=total.count+1
      
      print(ptemp)
    }
    if (baseline.match.5==1){
      LM_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound+age_bounds,
                                          -1,1))
      y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      LM_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count.5=count.5+1
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
  baseline=LM_matched_baseline
  
  
  while (restart==FALSE) {
  TRB_baseline.NOTAD=subset(TRB_baseline, NP_DX!='2')
  baseline.mean=mean(baseline$TRABSCOR)
  baseline.SD=SD(baseline$TRABSCOR)
  
  baseline$TRABSCOR.Z=(baseline$TRABSCOR-baseline.mean)/baseline.SD
  returnee.baseline$TRABSCOR.Z=(returnee.baseline$TRABSCOR-baseline.mean)/baseline.SD
  returnee.followup$TRABSCOR.Z=(returnee.followup$TRABSCOR-baseline.mean)/baseline.SD
  replcaements$TRABSCOR.Z=(replcaements$TRABSCOR-baseline.mean)/baseline.SD
  

  #calculate PE for RAW Zscores, no resid for edu or anart
  RAW_TRB_MCI_Difference_score=exp(mean(returnee.followup$TRABSCOR))-exp(mean(replcaements$TRABSCOR))
  RAW_TRB_MCI_Attrition_effect=exp(mean(returnee.baseline$TRABSCOR))-exp(mean(baseline$TRABSCOR))
  RAW_TRB_MCI_Practice_effect=RAW_TRB_MCI_Difference_score-RAW_TRB_MCI_Attrition_effect
  
  
  #combine bootstrapped results   
  RAW_TRB_MCI_PE.bootstrap=rbind(RAW_TRB_MCI_PE.bootstrap, RAW_TRB_MCI_Practice_effect)
  RAW_TRB_MCI_Difference_score.bootstrap=rbind(RAW_TRB_MCI_Difference_score.bootstrap, RAW_TRB_MCI_Difference_score)
  RAW_attritioneffect.bootstrap=rbind(RAW_attritioneffect.bootstrap, RAW_TRB_MCI_Attrition_effect)
  
  baseline$TRABSCOR=exp(baseline$TRABSCOR)-10
  returnee.baseline$TRABSCOR=exp(returnee.baseline$TRABSCOR)-10
  returnee.followup$TRABSCOR=exp(returnee.followup$TRABSCOR)-10
  replcaements$TRABSCOR=exp(replcaements$TRABSCOR)-10
  mean_TRB.temp=data.frame('baseline'=mean(baseline$TRABSCOR),
                           'baseline.SD'=SD(baseline$TRABSCOR),
                           'returnee.SD'=SD(returnee.followup$TRABSCOR),
                           'returnee.baseline'=mean(returnee.baseline$TRABSCOR),
                           'returnee.followup'=mean(returnee.followup$TRABSCOR), 
                           'replcacement'=mean(replcaements$TRABSCOR),
                           'returnee.base.sd'=SD(returnee.baseline$TRABSCOR),
                           'replcacement.SD'=SD(replcaements$TRABSCOR))
  
  mean_TRB.bootstrap=rbind(mean_TRB.bootstrap,mean_TRB.temp )
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
  
  
  databaselist.returnee.followup[[PE]]=returnee.followup
  databaselist.replcaements[[PE]]=replcaements
  databaselist.basleine[[PE]]=baseline
  databaselist.returnee.baseline[[PE]]=returnee.followup
  
  
  while (restart==TRUE) {
    fail=c("Iteration Failed, 
           Restarting at:
           
           PE =",
           PE)
    
    restart=FALSE
    
  }
}
RAW_TRB_MCI_Difference_score.bootstrap=subset(RAW_TRB_MCI_Difference_score.bootstrap,Pract.effect!=1000)
RAW_TRB_MCI_PE.bootstrap=subset(RAW_TRB_MCI_PE.bootstrap, Pract.effect!=1000)
mean_TRB.bootstrap=subset(mean_TRB.bootstrap,  baseline!=1000 & returnee.baseline!=1000 &
                            returnee.followup!=1000 & replcacement!=1000 & baseline.SD!=1000)
RAW_attritioneffect.bootstrap=subset(RAW_attritioneffect.bootstrap, Pract.effect!=1000)
describe(RAW_attritioneffect.bootstrap$Pract.effect)
describe(RAW_TRB_MCI_PE.bootstrap)
describe(RAW_TRB_MCI_Difference_score.bootstrap)
describe(mean_TRB.bootstrap)



########## 

