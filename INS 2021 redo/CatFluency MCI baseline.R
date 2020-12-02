#Category Fluency script. follow up is 12 months
# normal sample
# Test name = CATANIMSC
# qualifier = CFA
#
#
###
################################################################33

{
  library(readr)
  library(psych)
  library(dplyr)
  library(haven)
  #create databases by variable. merge by id and exam date? 
#  NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
#  NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
#  ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
  CFA=subset(NP_allSubs,VISCODE=='bl' | VISCODE=='m06', select=c('RID','CATANIMSC', 'VISCODE'))
  CFA_bl=subset(DXs.2, CATANIMSC>4 & CATANIMSC<50)
  CFA_fu=subset(DXs.m12, CATANIMSC>4 &CATANIMSC<50)
  
  
  #create attrition sub database [= any subs with ONLY baseline]
  notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
  potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
  Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  Attrition_baseline=merge(Attrition_baseline, CFA_bl, by = 'RID', all.x = TRUE)
  
  #Baseline 
  CFA_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTETHCAT', 'DX_bl','VISCODE'))
  CFA_baseline$match=paste(CFA_baseline$RID, sep='_', CFA_baseline$EXAMDATE)
  CFA_baseline=merge(CFA_baseline, ANART, by = 'RID', all.x = TRUE)
  CFA_baseline=merge(CFA_baseline, CFA_bl, by = 'RID', all.x = TRUE)
  
  CFA_baseline$ANARTERR.log=log(CFA_baseline$ANARTERR+1)
  CFA_baseline=CFA_baseline[!is.na(CFA_baseline$CATANIMSC),] #removes NA from the cog variable of interest
  CFA_baseline=CFA_baseline[!is.na(CFA_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score
  CFA_baseline$anart.zscore=scale(CFA_baseline$ANARTERR.log)
  #CFA_baseline=subset(CFA_baseline, anart.zscore>1)
  CATANIMSC.anart.resid=lm(CATANIMSC~anart.zscore,data=CFA_baseline)
  CFA_baseline$CATANIMSC.anart.resid=(residuals(CATANIMSC.anart.resid)+CATANIMSC.anart.resid$coefficients[1])
  #residualize cog variable to education 
  CFA_baseline$PTEDUCAT.zscore=scale(CFA_baseline$PTEDUCAT)
  CATANIMSC.edu.resid=lm(CATANIMSC~PTEDUCAT.zscore,data=CFA_baseline)
  CFA_baseline$CATANIMSC.edu.resid=(residuals(CATANIMSC.edu.resid)+CATANIMSC.edu.resid$coefficients[1])
  
  CFA_baseline.MCI_B=subset(CFA_baseline, MCI_any=='1')
  #CFA_baseline.MCI_B=subset(CFA_baseline.MCI_B, DX_bl!='AD')
  CFA_baseline_MCIs_ID=subset(CFA_baseline.MCI_B, select='RID') 
  
  
  #Follow up (6 months is first)
  CFA_followup=subset(Subject_selection_V1, VISCODE=='m12',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTEDUCAT','PTETHCAT','DX','VISCODE'))
  
  CFA_followup=merge(CFA_followup, ANART, by = 'RID', all.x = TRUE)
  CFA_followup=merge(CFA_followup, CFA_fu, by = 'RID', all.x = TRUE)
  
  CFA_followup$ANARTERR.log=log(CFA_followup$ANARTERR+1)
  
  CFA_followup=CFA_followup[!is.na(CFA_followup$CATANIMSC),] #removes NA from the cog variable of interest
  CFA_followup=CFA_followup[!is.na(CFA_followup$ANARTERR.log),] #removes NA from the cog variable of interest
  

  CFA_followup$age_fu=CFA_followup$AGE+(CFA_followup$Month_bl/12)
  CFA_followup$match=paste(CFA_followup$RID, sep='_', CFA_followup$EXAMDATE)
  CFA_followup.ID=subset(CFA_followup, select = 'RID')
  
  #Returnee database creation 
  CFA_returnee.MCI_baseline.ID=merge(CFA_followup.ID, CFA_baseline_MCIs_ID, by='RID')
  CFA_returnee.MCI_baseline.DATA.V2=merge(CFA_returnee.MCI_baseline.ID, CFA_followup, by='RID') #220, not horrible skew
  #returnee.baseline
  CFA_returnee.MCI_baseline.DATA.V1=merge(CFA_baseline.MCI_B, CFA_returnee.MCI_baseline.ID, by='RID') #220, some skew left
  #create database for standardizeing. excludes anyone who is AD
  CFA_baseline.NOTAD=subset(CFA_baseline, NP_DX!='2')

  
}

########################################3

{PE=0
  
  CFA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  anart_CFA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  edu_CFA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  ZRAW_CFA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  RAW_CFA_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  mean_CFA.bootstrap=data.frame('baseline'=1000,
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
  
  
} #zero out dataset lists


library(readr)
library(psych)
library(dplyr)
library(haven)

while (PE<5000) {
  restart=FALSE
  
  #selection of returnee subjects. Because dont' have enough replacement subs. 
  r.sample.returnees <- CFA_returnee.MCI_baseline.DATA.V2[sample(nrow(CFA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
  #potential PR pool
  CFA_potential_PR=CFA_baseline.MCI_B[-which(CFA_baseline.MCI_B$RID %in% r.sample.returnees$RID),]
  #CFA_potential_PR=CFA_potential_PR[-which(CFA_potential_PR$RID %in% Attrition_baseline$RID),]
  age_bounds=describe(r.sample.returnees$age_fu)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  CFA_potential_PR$keep=ifelse(CFA_potential_PR$AGE>u_bound,
                               -1,
                               ifelse(CFA_potential_PR$AGE<l_bound,
                                      -1,1))
  
  x=subset(CFA_potential_PR, keep==1) #removes anyone outside age range
  if (mean(CFA_potential_PR$keep)<.3){
    #selection of returnee subjects. Because dont' have enough replacement subs. 
    r.sample.returnees <- CFA_returnee.MCI_baseline.DATA.V2[sample(nrow(CFA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
    #potential PR pool
    CFA_potential_PR=CFA_baseline.MCI_B[-which(CFA_baseline.MCI_B$RID %in% r.sample.returnees$RID),]
    #CFA_potential_PR=CFA_potential_PR[-which(CFA_potential_PR$RID %in% Attrition_baseline$RID),]
    
    age_bounds=describe(r.sample.returnees$age_fu)
    u_bound=age_bounds$max  
    l_bound=age_bounds$min
    CFA_potential_PR$keep=ifelse(CFA_potential_PR$AGE>u_bound,
                                 -1,
                                 ifelse(CFA_potential_PR$AGE<l_bound+age_bounds$sd+5,
                                        -1,1))
    
    x=subset(CFA_potential_PR, keep==1) #removes anyone outside age range)
  }
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
    success.1=ifelse(edu_diff$p.value>.8, 1, 0) #how dif the edu can be. 
    success.2=ifelse(age_dif$p.value>.8, 1, 0) #how dif the ages can be. 
    success.3=ifelse(anart_diff$p.value>.8, 1, 0) #how dif the anart can be. 
    
    
    pr.sex=as.factor(pseudo.R.subs$PTGENDER)
    re.sex=as.factor(r.sample.returnees$PTGENDER)
    n <- max(length(pr.sex), length(re.sex))
    length(re.sex) <- n                      
    length(pr.sex) <- n
    sex.temp=as.data.frame(cbind(pr.sex, re.sex))
    sex.test=chisq.test(as.factor(sex.temp$pr.sex), as.factor(sex.temp$re.sex))
    success.sex=ifelse(sex.test$p.value>.8, 1, 0) #how dif the anart can be. 
    
    success.4=ifelse(nrow(pseudo.R.subs)>(120), 1, 0)# sets maximum of subs
    success.5=ifelse(nrow(pseudo.R.subs)<80, 1, 0)# minimum number of subs
    
    
    #final check to see if leave loop
    #success=ifelse(success.2==1 & success.3==1, 1, 0) #final check to see if leave loop
    success=ifelse(success.1==1 & success.2==1 & success.3==1 & success.sex==1, 1, 0) #final check to see if leave loop
    #  k=as.numeric(sample(x=(row.names(pseudo.R.subs)), size=1))
    #  if (success==0) {
    #    pseudo.R.subs=pseudo.R.subs[-c(k),]
    #  } #removes 1 random subs if not meeting success conditions 
    count=(count+1)
    if (count>50) {
      r.sample.returnees <- CFA_returnee.MCI_baseline.DATA.V2[sample(nrow(CFA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      CFA_potential_PR=CFA_baseline.MCI_B[-which(CFA_baseline.MCI_B$RID %in% r.sample.returnees$RID), ]
      # CFA_potential_PR=CFA_potential_PR[-which(CFA_potential_PR$RID %in% Attrition_baseline$RID),]
      
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      CFA_potential_PR$keep=ifelse(CFA_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(CFA_potential_PR$AGE<l_bound,
                                          -1,1))     
      x=subset(CFA_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size      count.5=count.5+1
      
      success=0
      count=0
      total.count=total.count+1
      ptemp=c('fail', total.count)
      print(ptemp)
      
      
    }
    if (success.4==1){
      r.sample.returnees <- CFA_returnee.MCI_baseline.DATA.V2[sample(nrow(CFA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      CFA_potential_PR=CFA_baseline.MCI_B[-which(CFA_baseline.MCI_B$RID %in% r.sample.returnees$RID),]
      #CFA_potential_PR=CFA_potential_PR[-which(CFA_potential_PR$RID %in% Attrition_baseline$RID),]
      
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      CFA_potential_PR$keep=ifelse(CFA_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(CFA_potential_PR$AGE<l_bound,
                                          -1,1))      
      x=subset(CFA_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size      count.5=count.5+1
      count.4=count.4+1
      success=0
      total.count=total.count+1
      ptemp=c('over120', total.count)
      print(ptemp)

    }
    if (success.5==1){
      r.sample.returnees <- CFA_returnee.MCI_baseline.DATA.V2[sample(nrow(CFA_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      CFA_potential_PR=CFA_baseline.MCI_B[-which(CFA_baseline.MCI_B$RID %in% r.sample.returnees$RID),]
      #CFA_potential_PR=CFA_potential_PR[-which(CFA_potential_PR$RID %in% Attrition_baseline$RID),]
      
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      CFA_potential_PR$keep=ifelse(CFA_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(CFA_potential_PR$AGE<l_bound,
                                          -1,1))      
      x=subset(CFA_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size      count.5=count.5+1
      success=0
      total.count=total.count+1
      ptemp=c('under80', total.count)
      print(ptemp)
    }
   
    if (total.count>100) {
      restart=TRUE
      success=1
      failtext='UNABLE TO MATCH REPlACMENTS'
      print(failtext)
    }
     
  }
  describe(pseudo.R.subs$AGE)
  
  #make sure that replacement subs are not in baseline data set. 
  test=CFA_baseline.MCI_B
  pseudo.R.subs.ID=as.list(pseudo.R.subs$match)
  test=test[-which(test$match %in% pseudo.R.subs.ID),]
  CFA_baseline.MCI_B.withoutPR=test
  r.sample.returnees.ID=subset(r.sample.returnees, select='RID')
  
  #database
  baseline=CFA_baseline.MCI_B.withoutPR #does not include attrition replacements
  returnee.baseline=merge(CFA_returnee.MCI_baseline.DATA.V1, r.sample.returnees.ID, by = "RID")
  returnee.followup=r.sample.returnees    
  replcaements=pseudo.R.subs#matched on age and edu 
  Attrition_baseline=Attrition_baseline
  
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
    
    if (restart==TRUE |total.count>100) {
      baseline.match=1
      failtext='UNABLE TO MATCH BASELINE'
      print(failtext)
      restart=TRUE
      
    }
    
  }
  baseline=LM_matched_baseline
  
  
  while (restart==FALSE) {
    
    baseline.mean=mean(baseline$CATANIMSC)
    baseline.SD=SD(baseline$CATANIMSC)
    
    baseline$CATANIMSC.Z=(baseline$CATANIMSC-baseline.mean)/baseline.SD
    returnee.baseline$CATANIMSC.Z=(returnee.baseline$CATANIMSC-baseline.mean)/baseline.SD
    returnee.followup$CATANIMSC.Z=(returnee.followup$CATANIMSC-baseline.mean)/baseline.SD
    replcaements$CATANIMSC.Z=(replcaements$CATANIMSC-baseline.mean)/baseline.SD
    
    #calculate PE for RAW Zscores, no resid for edu or anart
    RAW_CFA_MCI_Difference_score=mean(returnee.followup$CATANIMSC)-mean(replcaements$CATANIMSC)
    RAW_CFA_MCI_Attrition_effect=mean(returnee.baseline$CATANIMSC)-mean(baseline$CATANIMSC)
    RAW_CFA_MCI_Practice_effect=RAW_CFA_MCI_Difference_score-RAW_CFA_MCI_Attrition_effect
    
    #combine bootstrapped results   
    RAW_CFA_MCI_PE.bootstrap=rbind(RAW_CFA_MCI_PE.bootstrap, RAW_CFA_MCI_Practice_effect)
    RAW_attritioneffect.bootstrap=rbind(RAW_attritioneffect.bootstrap, RAW_CFA_MCI_Attrition_effect)
    
    mean_CFA.temp=data.frame('baseline'=mean(baseline$CATANIMSC),
                             'baseline.SD'=SD(baseline$CATANIMSC),
                             'returnee.SD'=SD(returnee.followup$CATANIMSC),
                             'returnee.baseline'=mean(returnee.baseline$CATANIMSC),
                             'returnee.followup'=mean(returnee.followup$CATANIMSC), 
                             'replcacement'=mean(replcaements$CATANIMSC),
                             'replcacement.SD'=SD(replcaements$CATANIMSC),
                             'returnee.base.sd'=SD(returnee.baseline$CATANIMSC))


    mean_CFA.bootstrap=rbind(mean_CFA.bootstrap,mean_CFA.temp )
    
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

RAW_CFA_MCI_PE.bootstrap=subset(RAW_CFA_MCI_PE.bootstrap, Pract.effect!=1000)
RAW_attritioneffect.bootstrap=subset(RAW_attritioneffect.bootstrap, Pract.effect!=1000)
mean_CFA.bootstrap=subset(mean_CFA.bootstrap, baseline!=1000 & returnee.baseline!=1000 &
                            returnee.followup!=1000 & replcacement!=1000 & baseline.SD!=1000)
describe(RAW_CFA_MCI_PE.bootstrap)
describe(RAW_attritioneffect.bootstrap$Pract.effect)
describe(mean_CFA.bootstrap)
