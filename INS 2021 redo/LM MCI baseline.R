# Logical memory practice effects
# MCI only
# 12 month retest interval. 

#updated with jak bondi criteria DXs
#upddated with age batched All baseline sample. 
#############################################

#updated with higher p value for matching (.7), changed sample size, log transformed 
    #LM variable, used median instead of mean. 

{
  

  #       RUN DATA SETUP SCRIPT       #
  #                                   #
  #                                   #
  
  library(readr)
  library(psych)
  library(dplyr)
  library(haven)
  
  #create databases by variable. merge by id and exam date? 
  NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
  NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
  ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR', 'LMSTORY'), all=TRUE)
  
  
  Subject_selection_BL_removed=subset(Subject_selection_V1, VISCODE=='bl' & LDELTOTAL==0, select = c('RID', 'VISCODE','LDELTOTAL'))
  Subject_selection_12m_removed=subset(Subject_selection_V1, VISCODE=='m12' & LDELTOTAL==0, select = c('RID', 'VISCODE','LDELTOTAL'))
  colnames(Subject_selection_12m_removed)[3]='LDELTOTAL_M12'
  removedsublist=merge(Subject_selection_12m_removed, Subject_selection_BL_removed, by='RID')
  removedsublist=subset(removedsublist, select = 'RID')
  
  Subject_selection_BL_MAX=subset(Subject_selection_V1, VISCODE=='bl' & LDELTOTAL==25, select = c('RID', 'VISCODE','LDELTOTAL'))
  Subject_selection_12m_MAX=subset(Subject_selection_V1, VISCODE=='m12' & LDELTOTAL==25, select = c('RID', 'VISCODE','LDELTOTAL'))
  colnames(Subject_selection_12m_MAX)[3]='LDELTOTAL_M12'
  removedsublistMAX=merge(Subject_selection_BL_MAX, Subject_selection_12m_MAX, by='RID')
  removedsublistMAX=subset(removedsublistMAX, select = 'RID')

  
  
    #logical memoery. 
  #Baseline 
  #NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)
  #NP_DX_LM (0=Jak/Bondi normal; 1=Jak/Bondi MCI; includes LM instead of RAVLT Recognition 
  
  library(haven)
  
  LM_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                     select = c('RID','AGE','EXAMDATE','Month_bl',
                                'PTETHCAT', 'DX_bl','VISCODE'))
  LM_baseline=merge(LM_baseline, DXs.2, by = 'RID')
  LM_baseline$match=paste(LM_baseline$RID, sep='_', LM_baseline$EXAMDATE)
  LM_baseline=merge(LM_baseline, ANART, by = 'RID', all.x = TRUE)
  LM_baseline$ANARTERR.log=log(LM_baseline$ANARTERR+1)
  LM_baseline=LM_baseline[!is.na(LM_baseline$LDELTOTAL),] #removes NA from the cog variable of interest
  LM_baseline=LM_baseline[!is.na(LM_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
  
  # since data is skewed, transform Logical memory variable
  LM_baseline$LDELTOTAL=log(LM_baseline$LDELTOTAL+10)
  
  #LM_baseline.AD_BL=subset(LM_baseline, NP_DX==2)
  #LM_baseline.AD_BL.ID=subset(LM_baseline.AD_BL, select='RID')
  
  LM_baseline.MCI_BL=subset(LM_baseline, MCI_any==1)
  LM_baseline.MCI_BL.ID=subset(LM_baseline.MCI_BL, select='RID')
  
  #Follow up (12 months is first)
  LM_followup=subset(Subject_selection_V1, VISCODE=='m12',
                     select = c('RID','AGE','EXAMDATE','Month_bl',
                                'PTEDUCAT','PTETHCAT','DX','VISCODE'))
  
  LM_followup=merge(LM_followup, DXs.m12, by = 'RID') #this DX is for BASELINE. they don't have it at 6months.
  
  LM_followup=merge(LM_followup, ANART, by = 'RID', all.x = TRUE)
  LM_followup$ANARTERR.log=log(LM_followup$ANARTERR+1)
  
  LM_followup=LM_followup[!is.na(LM_followup$LDELTOTAL),] #removes NA from the cog variable of interest
  LM_followup=LM_followup[!is.na(LM_followup$ANARTERR.log),] #removes NA from the cog variable of interest
  
  # since data is skewed, transform Logical memory variable
  LM_followup$LDELTOTAL=log(LM_followup$LDELTOTAL+10)
  
  LM_followup$age_fu=LM_followup$AGE+(LM_followup$Month_bl/12)
  LM_followup$match=paste(LM_followup$RID, sep='_', LM_followup$EXAMDATE)
  
  
  
sum1=nrow(LM_followup[which(LM_followup$RID %in% removedsublist$RID),])
  if (sum1!=0) {
    LM_followup=LM_followup[-which(LM_followup$RID %in% removedsublist$RID),]
  }
  
  
  sum2=nrow(LM_followup[which(LM_followup$RID %in% removedsublistMAX$RID),])
    if (sum2!=0) {
      LM_followup=LM_followup[-which(LM_followup$RID %in% removedsublistMAX$RID),]
    }

  
  sum3=nrow(LM_baseline.MCI_BL[which(LM_baseline.MCI_BL$RID %in% removedsublist$RID),])
  if (sum3!=0) {
    LM_baseline.MCI_BL=LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% removedsublist$RID),]
  }

  sum4=nrow(LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% removedsublistMAX$RID),])
  if (sum4!=0) {
    LM_baseline.MCI_BL=LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% removedsublistMAX$RID),]
  }
  
    #Returnee database creation 
  LM_returnee.MCI_baseline.ID=merge(LM_followup.ID, LM_baseline.MCI_BL.ID, by='RID')
  LM_returnee.MCI_baseline.DATA.V2=merge(LM_returnee.MCI_baseline.ID, LM_followup, by='RID')
  
  #returnee.baseline
  LM_returnee.MCI_baseline.DATA.V1=merge(LM_baseline.MCI_BL, LM_returnee.MCI_baseline.ID, by='RID')
  
  #create database for standardizeing. excludes anyone who is AD
  LM_baseline.NOTAD=subset(LM_baseline, NP_DX!='2')
  
  #create attrition sub database [= any subs with ONLY baseline]
  notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
  potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
  Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  Attrition_baseline=subset(Attrition_baseline, select = 'RID')
  Attrition_baseline=merge(Attrition_baseline, DXs.2, by = 'RID')
  Attrition_baseline=subset(Attrition_baseline, NP_DX!=2)
  
  
  #remove people who have 0 at both time points
  

}  #run this block to set up the dataset for LM

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################


{PE=0
LM_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
RAW_LM_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
RAW_LM_MCI_Difference.bootstrap=data.frame(Pract.effect=1000)
median_LM.bootstrap=data.frame('baseline'=1000,
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

}     #empty database set up


#################################################################

while (PE<5000) {
  restart=FALSE
  
  #428 subs in the LM returneee pool. Limiting to half that for the calcualtion of PE
  r.sample.returnees <- LM_returnee.MCI_baseline.DATA.V2[sample(nrow(LM_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
  #potential PR pool
  LM_potential_PR=LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
  #LM_potential_PR=LM_potential_PR[-which(LM_potential_PR$RID %in% Attrition_baseline$RID),]
  
  age_bounds=describe(r.sample.returnees$age_fu)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  LM_potential_PR$keep=ifelse(LM_potential_PR$AGE>u_bound,
                              -1,
                              ifelse(LM_potential_PR$AGE<l_bound+age_bounds,
                                     -1,1))
  x=subset(LM_potential_PR, keep==1) #removes anyone outside age range
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
    success=ifelse(success.1==1 & success.2==1 & success.3==1 & success.sex==1, 1, 0) #final check to see if leave loop
    
    count=(count+1)
    if (count>80) {
      r.sample.returnees <- LM_returnee.MCI_baseline.DATA.V2[sample(nrow(LM_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      LM_potential_PR=LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #LM_potential_PR=LM_potential_PR[-which(LM_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      LM_potential_PR$keep=ifelse(LM_potential_PR$AGE>u_bound,
                                  -1,
                                  ifelse(LM_potential_PR$AGE<l_bound+age_bounds,
                                         -1,1))
      x=subset(LM_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count=0
      total.count=total.count+1
      ptemp=c('fail', total.count)
      print(ptemp)
    }
    if (success.4==1){
      r.sample.returnees <- LM_returnee.MCI_baseline.DATA.V2[sample(nrow(LM_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      LM_potential_PR=LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #LM_potential_PR=LM_potential_PR[-which(LM_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      LM_potential_PR$keep=ifelse(LM_potential_PR$AGE>u_bound,
                                  -1,
                                  ifelse(LM_potential_PR$AGE<l_bound,
                                         -1,1))
      x=subset(LM_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count.4=count.4+1
      success=0
      total.count=total.count+1
      ptemp=c('over120', total.count)
      print(ptemp)
    }
    if (success.5==1){
      r.sample.returnees <- LM_returnee.MCI_baseline.DATA.V2[sample(nrow(LM_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      LM_potential_PR=LM_baseline.MCI_BL[-which(LM_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #LM_potential_PR=LM_potential_PR[-which(LM_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      LM_potential_PR$keep=ifelse(LM_potential_PR$AGE>u_bound,
                                  -1,
                                  ifelse(LM_potential_PR$AGE<l_bound,
                                         -1,1))
      x=subset(LM_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      success=0
      total.count=total.count+1
      ptemp=c('under80', total.count)
      print(ptemp)
    }
    if (total.count>300) {
      restart=TRUE
      success=1
      failtext='UNABLE TO MATCH REPlACMENTS'
      print(failtext)
    }
  }

  print('matched PR')
  #make sure that replacement subs are not in baseline data set. 
  test=LM_baseline.MCI_BL
  pseudo.R.subs.ID=as.list(pseudo.R.subs$match)
  test=test[-which(test$match %in% pseudo.R.subs.ID),]
  LM_baseline.MCI_BL.withoutPR=test
  r.sample.returnees.ID=subset(r.sample.returnees, select='RID')
  
  #database
  baseline=LM_baseline.MCI_BL.withoutPR #does not include attrition replacements
  returnee.baseline=merge(LM_returnee.MCI_baseline.DATA.V1, r.sample.returnees.ID, by = "RID")
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
                               ifelse(baseline$AGE<l_bound+age_bounds,
                                      -1,1))
  y=subset(LM_temp_baseline, keep==1) #removes anyone outside age range
  baseline.match=0
  count=0
  baseline.match=0
  baseline.match.2=0
  Total.count=0
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
    if (count>80) {
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
    if (restart==TRUE | total.count>300) {
      baseline.match=1
      failtext='UNABLE TO MATCH BASELINE'
      print(failtext)
      restart=TRUE
    }
    
  }
  
  while (restart==FALSE) {
  baseline=LM_matched_baseline

  #zscore raw scores, not residualized
  baseline.mean=mean(LM_baseline.NOTAD$LDELTOTAL)
  baseline.SD=SD(LM_baseline.NOTAD$LDELTOTAL)

  #calculate PE for RAW Zscores [used median because non-normal]
  RAW_LM_MCI_Difference_score=median(exp(returnee.followup$LDELTOTAL))-median(exp(replcaements$LDELTOTAL))
  RAW_LM_MCI_Attrition_effect=median(exp(returnee.baseline$LDELTOTAL))-median(exp(baseline$LDELTOTAL))
  RAW_LM_MCI_Practice_effect=RAW_LM_MCI_Difference_score-RAW_LM_MCI_Attrition_effect
  
  #Z score calcualtions 
  
#  ZRAW_LM_MCI_Difference_score=mean(returnee.followup$LDELTOTAL.z)-mean(replcaements$LDELTOTAL.z)
 # ZRAW_LM_MCI_Attrition_effect=mean(returnee.baseline$LDELTOTAL.z)-mean(baseline$LDELTOTAL.z)
  #ZRAW_LM_MCI_Practice_effect=ZRAW_LM_MCI_Difference_score-ZRAW_LM_MCI_Attrition_effect
  
  #combine bootstrapped results   
  RAW_LM_MCI_PE.bootstrap=rbind(RAW_LM_MCI_PE.bootstrap, RAW_LM_MCI_Practice_effect)
  RAW_LM_MCI_Difference.bootstrap=rbind(RAW_LM_MCI_Difference.bootstrap, RAW_LM_MCI_Difference_score)
  RAW_attritioneffect.bootstrap=rbind(RAW_attritioneffect.bootstrap, RAW_LM_MCI_Attrition_effect)
  
  baseline$LDELTOTAL=exp(baseline$LDELTOTAL)-10
  returnee.baseline$LDELTOTAL=exp(returnee.baseline$LDELTOTAL)-10
  returnee.followup$LDELTOTAL=exp(returnee.followup$LDELTOTAL)-10
  replcaements$LDELTOTAL=exp(replcaements$LDELTOTAL)-10
  
  median_LM.temp=data.frame('baseline'=median(baseline$LDELTOTAL),
                          'baseline.SD'=SD(baseline$LDELTOTAL),
                          'returnee.SD'=SD(returnee.followup$LDELTOTAL),
                          'returnee.baseline'=median(returnee.baseline$LDELTOTAL),
                          'returnee.followup'=median(returnee.followup$LDELTOTAL), 
                          'replcacement'=median(replcaements$LDELTOTAL),
                          'returnee.base.sd'=SD(returnee.baseline$LDELTOTAL),
                          'replcacement.SD'=SD(replcaements$LDELTOTAL))
  median_LM.bootstrap=rbind(median_LM.bootstrap,median_LM.temp )
  
  
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
  
  
} #run this block to calc PE for LM

RAW_LM_MCI_Difference.bootstrap=subset(RAW_LM_MCI_Difference.bootstrap, Pract.effect!=1000)
RAW_LM_MCI_PE.bootstrap=subset(RAW_LM_MCI_PE.bootstrap, Pract.effect!=1000)
RAW_attritioneffect.bootstrap=subset(RAW_attritioneffect.bootstrap, Pract.effect!=1000)
median_LM.bootstrap=subset(median_LM.bootstrap, baseline!=1000 & returnee.baseline!=1000 &
                           returnee.followup!=1000 & replcacement!=1000 & baseline.SD!=1000)

describe(RAW_LM_MCI_Difference.bootstrap)
describe(RAW_attritioneffect.bootstrap$Pract.effect)
describe(RAW_LM_MCI_PE.bootstrap)
describe(median_LM.bootstrap)


#2/21/20
> describe(RAW_LM_MCI_Difference.bootstrap)
vars    n  mean   sd median trimmed mad min max range  skew kurtosis   se
X1    1 2000 -0.04 0.69      0   -0.04   0  -3   2     5 -0.07     0.77 0.02
> describe(RAW_attritioneffect.bootstrap$Pract.effect)
vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
X1    1 2000 0.29 0.64      0    0.36 0.74  -2   2     4 -0.41    -0.57 0.01
> describe(RAW_LM_MCI_PE.bootstrap)
vars    n  mean   sd median trimmed  mad min max range skew kurtosis   se
X1    1 2000 -0.33 0.88      0   -0.35 1.48  -3 2.5   5.5 0.27     0.02 0.02
> describe(median_LM.bootstrap)
vars    n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
baseline             1 2000 4.29 0.46   4.00    4.24 0.00 3.00 6.00  3.00  0.87    -0.64 0.01
baseline.SD          2 2000 3.72 0.19   3.71    3.72 0.20 3.07 4.38  1.31  0.03     0.03 0.00
returnee.SD          3 2000 4.51 0.14   4.51    4.51 0.14 3.97 4.91  0.95 -0.20    -0.04 0.00
returnee.baseline    4 2000 4.59 0.47   5.00    4.61 0.00 4.00 5.00  1.00 -0.35    -1.79 0.01
returnee.followup    5 2000 4.01 0.43   4.00    4.01 0.00 3.00 5.00  2.00  0.04     1.94 0.01
replcacement         6 2000 4.05 0.50   4.00    4.06 0.00 3.00 6.00  3.00  0.08     0.88 0.01
replcacement.SD      7 2000 3.79 0.21   3.79    3.79 0.21 3.09 4.44  1.34 -0.02    -0.12 0.00





##output 10/14/19
> describe(RAW_LM_MCI_Difference.bootstrap)
vars    n mean   sd median trimmed  mad   min max range skew kurtosis   se
X1    1 2000 0.79 0.42   0.79    0.79 0.42 -0.61 2.2  2.81 0.02    -0.17 0.01
> describe(RAW_attritioneffect.bootstrap$Pract.effect)
vars    n  mean   sd median trimmed  mad   min  max range  skew kurtosis se
X1    1 2000 -0.16 0.14  -0.16   -0.16 0.14 -0.68 0.32     1 -0.01     0.12  0
> describe(RAW_LM_MCI_PE.bootstrap)
vars    n mean   sd median trimmed  mad   min  max range skew kurtosis   se
X1    1 2000 0.94 0.44   0.93    0.94 0.44 -0.37 2.44  2.82 0.03    -0.19 0.01
> describe(median_LM.bootstrap)
vars    n  mean   sd median trimmed  mad   min   max range  skew kurtosis   se
baseline             1 2000 10.70 0.12  10.70   10.70 0.12 10.32 11.13  0.81  0.04    -0.09 0.00
baseline.SD          2 2000  4.30 0.08   4.30    4.30 0.08  4.02  4.55  0.53 -0.10    -0.10 0.00
returnee.SD          3 2000  4.86 0.08   4.86    4.86 0.07  4.58  5.10  0.52 -0.17     0.05 0.00
returnee.baseline    4 2000 10.55 0.11  10.55   10.55 0.11 10.19 10.87  0.67 -0.08    -0.06 0.00
returnee.followup    5 2000 11.62 0.12  11.62   11.62 0.12 11.23 12.03  0.80  0.03    -0.10 0.00
replcacement         6 2000 10.83 0.37  10.83   10.83 0.36  9.50 11.93  2.43 -0.02    -0.11 0.01
replcacement.SD      7 2000  4.42 0.25   4.43    4.42 0.26  3.50  5.23  1.73 -0.09    -0.06 0.01

