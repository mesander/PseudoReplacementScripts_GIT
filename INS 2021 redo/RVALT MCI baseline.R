#RVALT recognition script. follow up is 6 months
#mci sample
# Test name = AVDELTOT
# qualifier = Arec
#
#
###################################################################33
library(readr)
library(dplyr)
library(psych)

{ 
  ################################## ###
  #       RUN DATA SETUP SCRIPT       #
  #                                   #
  #                                   #
  
  
  
  #create databases by variable. merge by id and exam date? 
  NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
  NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
  ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR', 'LMSTORY'), all=TRUE)
  
  #remove people with perfect scores at both time points
  avlt_temp=subset(DXs, Visit=='0' | Visit=='12')
  avlt_temp.base=subset(avlt_temp, Visit=='0' & AVDEL30MIN==15, select = 'RID') #max base
  avlt_temp.base_temp.fu=subset(avlt_temp, Visit=='12' & AVDEL30MIN==15, select = 'RID') #max fu
  exclude_avlt_temp.base=merge(avlt_temp.base, avlt_temp.base_temp.fu, by = 'RID') #list of people with max at FU and base
  
  avlt_temp.base.zero=subset(avlt_temp, Visit=='0' & AVDEL30MIN==0, select = 'RID') #max base
  avlt_temp.base_temp.fu.zero=subset(avlt_temp, Visit=='12' & AVDEL30MIN==0, select = 'RID') #max fu
  exclude_avlt_temp.base.zero=merge(avlt_temp.base.zero, avlt_temp.base_temp.fu.zero, by = 'RID') #list of people with max at FU and base
  
  RD_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                     select = c('RID','AGE','EXAMDATE','Month_bl',
                                'PTETHCAT', 'DX_bl','VISCODE'))
  RD_baseline=merge(RD_baseline, DXs.2, by = 'RID')
  RD_baseline$match=paste(RD_baseline$RID, sep='_', RD_baseline$EXAMDATE)
  RD_baseline=merge(RD_baseline, ANART, by = 'RID', all.x = TRUE)
  RD_baseline$ANARTERR.log=log(RD_baseline$ANARTERR+1)
  RD_baseline=RD_baseline[!is.na(RD_baseline$AVDEL30MIN),] #removes NA from the cog variable of interest
  RD_baseline=RD_baseline[!is.na(RD_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score
  RD_baseline$anart.zscore=scale(RD_baseline$ANARTERR.log)
  RD_baseline$PTEDUCAT.zscore=scale(RD_baseline$PTEDUCAT)
  
  #line below removes people with perfect at both time points. 
  RD_baseline.MCI_BL=subset(RD_baseline, MCI_any==1)
  RD_baseline.MCI_BL.ID=subset(RD_baseline.MCI_BL, select='RID')
  
  #Follow up (12 months is first)
  RD_followup=subset(Subject_selection_V1, VISCODE=='m12',
                     select = c('RID','AGE','EXAMDATE','Month_bl','PTEDUCAT',
                                'PTETHCAT','DX','VISCODE'))
  
  RD_followup=merge(RD_followup, DXs.m12, by = 'RID') #this DX is for BASELINE. they don't have it at 6months.
  
  RD_followup=merge(RD_followup, ANART, by = 'RID', all.x = TRUE)
  RD_followup$ANARTERR.log=log(RD_followup$ANARTERR+1)
  
  RD_followup=RD_followup[!is.na(RD_followup$AVDEL30MIN),] #removes NA from the cog variable of interest
  RD_followup=RD_followup[!is.na(RD_followup$ANARTERR.log),] #removes NA from the cog variable of interest
  
  
  RD_followup$age_fu=RD_followup$AGE+(RD_followup$Month_bl/12)
  RD_followup$match=paste(RD_followup$RID, sep='_', RD_followup$EXAMDATE)

    #remove people with perfect or zero scores ar baseline and FU. 
  sum1=nrow(RD_followup[which(RD_followup$RID %in% exclude_avlt_temp.base.zero$RID),])
  sum2=nrow(RD_baseline.MCI_BL[which(RD_baseline.MCI_BL$RID %in% exclude_avlt_temp.base.zero$RID),])
  sum3=nrow(RD_followup[which(RD_followup$RID %in% exclude_avlt_temp.base$RID),])
  sum4=nrow(RD_baseline.MCI_BL[which(RD_baseline.MCI_BL$RID %in% exclude_avlt_temp.base$RID),])
  
  if (sum1!=0) {
    RD_baseline.MCI_BL=RD_baseline.MCI_BL[-which(RD_baseline.MCI_BL$RID %in% exclude_avlt_temp.base.zero$RID),]
  }
  if (sum2!=0) {
    RD_followup=RD_followup[-which(RD_followup$RID %in% exclude_avlt_temp.base.zero$RID),]
  }
  if (sum3!=0) {
    RD_followup=RD_followup[-which(RD_followup$RID %in% exclude_avlt_temp.base$RID),]
  }
  if (sum4!=0) {
    RD_baseline.MCI_BL=RD_baseline.MCI_BL[-which(RD_baseline.MCI_BL$RID %in% exclude_avlt_temp.base$RID),]
  }
  
  RD_followup.ID=subset(RD_followup, select = 'RID')  #is large because includes subs with any baseline DX. 
  
  #Returnee database creation 
  RD_returnee.MCI_baseline.ID=merge(RD_followup.ID, RD_baseline.MCI_BL.ID, by='RID')
  RD_returnee.MCI_baseline.DATA.V2=merge(RD_returnee.MCI_baseline.ID, RD_followup, by='RID')
  #returnee.baseline
  RD_returnee.MCI_baseline.DATA.V1=merge(RD_baseline.MCI_BL, RD_returnee.MCI_baseline.ID, by='RID')
  
  #create database for standardizeing. excludes anyone who is AD
  RD_baseline.NOTAD=subset(RD_baseline, NP_DX!='2')
  
  
  #below code log transforms the Xe
  RD_returnee.MCI_baseline.DATA.V1$AVDEL30MIN=log(RD_returnee.MCI_baseline.DATA.V1$AVDEL30MIN+1)
  RD_returnee.MCI_baseline.DATA.V2$AVDEL30MIN=log(RD_returnee.MCI_baseline.DATA.V2$AVDEL30MIN+1)
  RD_baseline.MCI_BL$AVDEL30MIN=log(RD_baseline.MCI_BL$AVDEL30MIN+1)
  
  
  #create attrition sub database [= any subs with ONLY baseline]
  #notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
  #potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
  #Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  #Attrition_baseline=subset(Attrition_baseline, select = 'RID')
  #Attrition_baseline=merge(Attrition_baseline, DX_BL, by = 'RID')
  #Attrition_baseline=subset(Attrition_baseline, NP_DX!=2)
  
  PE=0
  RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  anart_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  edu_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  ZRAW_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  RAW_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  ZRAW_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  RAW_RD_CN_Difference.bootstrap=data.frame(Pract.effect=1000)
  mean_LM.bootstrap=data.frame('baseline'=1000,
                               'baseline.SD'=1000,
                               'returnee.SD'=1000,
                               'returnee.baseline'=1000,
                               'returnee.followup'=1000, 
                               'replcacement'=1000,
                               'returnee.base.sd'=1000,
                               'replcacement.SD'=1000)
  RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
} #run this block to set up databased for avlt

{
  PE=0
  RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  anart_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  edu_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  ZRAW_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  RAW_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  ZRAW_RD_CN_PE.bootstrap=data.frame(Pract.effect=1000)
  RAW_RD_CN_Difference.bootstrap=data.frame(Pract.effect=1000)
  mean_LM.bootstrap=data.frame('baseline'=1000,
                               'baseline.SD'=1000,
                               'returnee.SD'=1000,
                               'returnee.baseline'=1000,
                               'returnee.followup'=1000,
                               'returnee.base.sd'=1000,
                               'replcacement'=1000,
                               'replcacement.SD'=1000)
  RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
  
  
  databaselist.returnee.followup=list()
  databaselist.replcaements=list()
  databaselist.basleine=list()
  databaselist.returnee.baseline=list()}#sets up blank storage lists


while (PE<5000) {
  restart=FALSE
  #428 subs in the LM returneee pool. Limiting to half that for the calcualtion of PE
  r.sample.returnees <- RD_returnee.MCI_baseline.DATA.V2[sample(nrow(RD_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
  #potential PR pool
  RD_potential_PR=RD_baseline.MCI_BL[-which(RD_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
  #RD_potential_PR=RD_potential_PR[-which(RD_potential_PR$RID %in% Attrition_baseline$RID),]
  
  age_bounds=describe(r.sample.returnees$age_fu)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  RD_potential_PR$keep=ifelse(RD_potential_PR$AGE>u_bound,
                              -1,
                              ifelse(RD_potential_PR$AGE<l_bound,
                                     -1,1))
  x=subset(RD_potential_PR, keep==1) #removes anyone outside age range
  describe(x$AGE)
  count<-0
  count.4=0
  count.5=0
  count.low=0
  count.high=0
  
  success <- 0
  pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size
  total.count=1
  
  while (success==0) {
    # random select sub, add to database
    if (mean(pseudo.R.subs$AGE)>mean(r.sample.returnees$age_fu)) {
      n=subset(RD_potential_PR, AGE<mean(r.sample.returnees$age_fu))
      i=sample(n$RID, 1)
      count.low=count.low+1
    }
    if (mean(pseudo.R.subs$AGE)<mean(r.sample.returnees$age_fu)) {
      m=subset(RD_potential_PR, AGE>mean(r.sample.returnees$age_fu))
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
    
    
    pr.sex=as.factor(pseudo.R.subs$PTGENDER)
    re.sex=as.factor(r.sample.returnees$PTGENDER)
    n <- max(length(pr.sex), length(re.sex))
    length(re.sex) <- n                      
    length(pr.sex) <- n
    sex.temp=as.data.frame(cbind(pr.sex, re.sex))
    sex.test=chisq.test(as.factor(sex.temp$pr.sex), as.factor(sex.temp$re.sex))
    success.sex=ifelse(sex.test$p.value>.7, 1, 0) #how dif the anart can be. 
    
    
    
    success.1=ifelse(edu_diff$p.value>.7, 1, 0) #how dif the edu can be. 
    success.2=ifelse(age_dif$p.value>.8, 1, 0) #how dif the ages can be. 
    success.3=ifelse(anart_diff$p.value>.7, 1, 0) #how dif the anart can be. 
    
    
    success.4=ifelse(nrow(pseudo.R.subs)>(120), 1, 0)# sets maximum of subs
    success.5=ifelse(nrow(pseudo.R.subs)<80, 1, 0)# minimum number of subs
    #final check to see if leave loop
    #success=ifelse(success.2==1 & success.3==1, 1, 0) #final check to see if leave loop
    success=ifelse(success.1==1 & success.2==1 & success.3==1 & success.sex==1, 1, 0) #final check to see if leave loop
    
    count=(count+1)
    if (count>80) {
      r.sample.returnees <- RD_returnee.MCI_baseline.DATA.V2[sample(nrow(RD_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      RD_potential_PR=RD_baseline.MCI_BL[-which(RD_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #RD_potential_PR=RD_potential_PR[-which(RD_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      RD_potential_PR$keep=ifelse(RD_potential_PR$AGE>u_bound,
                                  -1,
                                  ifelse(RD_potential_PR$AGE<l_bound,
                                         -1,1))
      x=subset(RD_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count=0
      total.count=total.count+1
      ptemp=c('fail', total.count)
      print(ptemp)
    }
    if (success.4==1){
      r.sample.returnees <- RD_returnee.MCI_baseline.DATA.V2[sample(nrow(RD_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      RD_potential_PR=RD_baseline.MCI_BL[-which(RD_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #RD_potential_PR=RD_potential_PR[-which(RD_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      RD_potential_PR$keep=ifelse(RD_potential_PR$AGE>u_bound,
                                  -1,
                                  ifelse(RD_potential_PR$AGE<l_bound,
                                         -1,1))
      x=subset(RD_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count.4=count.4+1
      success=0
      total.count=total.count+1
      ptemp=c('over120', total.count)
      print(ptemp)
    }
    if (success.5==1){
      r.sample.returnees <- RD_returnee.MCI_baseline.DATA.V2[sample(nrow(RD_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      RD_potential_PR=RD_baseline.MCI_BL[-which(RD_baseline.MCI_BL$RID %in% r.sample.returnees$RID),]
      #RD_potential_PR=RD_potential_PR[-which(RD_potential_PR$RID %in% Attrition_baseline$RID),]
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      RD_potential_PR$keep=ifelse(RD_potential_PR$AGE>u_bound,
                                  -1,
                                  ifelse(RD_potential_PR$AGE<l_bound,
                                         -1,1))
      x=subset(RD_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] #restarts cycle if too low
      count.5=count.5+1
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
  test=RD_baseline.MCI_BL
  pseudo.R.subs.ID=as.list(pseudo.R.subs$match)
  test=test[-which(test$match %in% pseudo.R.subs.ID),]
  RD_baseline.MCI_BL.withoutPR=test
  r.sample.returnees.ID=subset(r.sample.returnees, select='RID')
  
  #database
  baseline=RD_baseline.MCI_BL.withoutPR #does not include attrition replacements
  returnee.baseline=merge(RD_returnee.MCI_baseline.DATA.V1, r.sample.returnees.ID, by = "RID")
  returnee.followup=r.sample.returnees    
  replcaements=pseudo.R.subs#matched on age and edu 
  Attrition_baseline=Attrition_baseline
  
  #creates baseline that is similar to returnee baseline
  age_bounds=describe(returnee.baseline$AGE)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  RD_temp_baseline=baseline
  RD_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                               -1,
                               ifelse(baseline$AGE<l_bound+age_bounds$sd+1,
                                      -1,1))
  y=subset(RD_temp_baseline, keep==1) #removes anyone outside age range
  baseline.match=0
  count=0
  RD_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
  print('matching baseline')
  total.count=1
  while (baseline.match==0) {
    # random select sub, add to database
    if (mean(RD_matched_baseline$AGE)>mean(returnee.baseline$AGE)) {
      n=subset(y, AGE<mean(returnee.baseline$AGE))
      i=sample(n$RID, 1)
      count.low=count.low+1
    }
    if (mean(RD_matched_baseline$AGE)<mean(returnee.baseline$AGE)) {
      m=subset(y, AGE>mean(returnee.baseline$AGE))
      i=sample(m$RID, 1) 
      count.high=count.high+1
    }
    RD_matched_baseline.temp=subset(y, RID==i) #random select sub row
    #add the new subs to a continually growing dataset 
    RD_matched_baseline=rbind(RD_matched_baseline.temp, RD_matched_baseline) #binds sub row to growing dataset
    RD_matched_baseline=(RD_matched_baseline %>% distinct())#removes a subject if repeated 
    
    age_dif=t.test(RD_matched_baseline$AGE, returnee.baseline$AGE) #looks for sig difference between age values 
    baseline.match.2=ifelse(age_dif$p.value>.9, 1, 0) #how dif the ages can be. 
    
    
    baseline.match.4=ifelse(nrow(RD_matched_baseline)>(120), 1, 0)# sets maximum of subs
    baseline.match.5=ifelse(nrow(RD_matched_baseline)<80, 1, 0)# minimum number of subs
    #final check to see if leave loop
    #baseline.match=ifelse(baseline.match.2==1 & baseline.match.3==1, 1, 0) #final check to see if leave loop
    baseline.match=ifelse(baseline.match.2==1, 1, 0) #final check to see if leave loop
    
    count=(count+1)
    if (count>80) {
      RD_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound,
                                          -1,1))
      y=subset(RD_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      RD_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count=0
      total.count=total.count+1
      ptemp=c('baseline fail', total.count)
      print(ptemp)
    }
    if (baseline.match.4==1){
      RD_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound,
                                          -1,1))
      y=subset(RD_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      RD_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count.4=count.4+1
      baseline.match=0
      total.count=total.count+1
      ptemp=c('baseline over n', total.count)
      print(ptemp)
    }
    if (baseline.match.5==1){
      RD_temp_baseline$keep=ifelse(baseline$AGE>u_bound,
                                   -1,
                                   ifelse(baseline$AGE<l_bound,
                                          -1,1))
      y=subset(RD_temp_baseline, keep==1) #removes anyone outside age range
      baseline.match=0
      RD_matched_baseline <- y[sample(nrow(y), 100), ] # starting sample size
      count.5=count.5+1
      baseline.match=0
      total.count=total.count+1
      ptemp=c('baseline under n', total.count)
      print(ptemp)
      
    }
    if (restart==TRUE | total.count>100) {
      baseline.match=1
      failtext='UNABLE TO MATCH BASELINE'
      print(failtext)
      print(failtext)
      print(failtext)
      
      restart=TRUE
    }
  }
  baseline=RD_matched_baseline
  #Test differences in some key variables
  while (restart==FALSE) {
    #zscore raw scores, not residualized
    baseline.mean=mean(RD_baseline.NOTAD$AVDEL30MIN)
    baseline.SD=SD(RD_baseline.NOTAD$AVDEL30MIN)
    # use below if want to base standardization on only people with this DX, in this sample
    # baseline.mean=mean(baseline$AVDEL30MIN)
    #  baseline.SD=SD(baseline$AVDEL30MIN) baseline$AVDEL30MIN.z=(baseline$AVDEL30MIN-baseline.mean)/baseline.SD
    returnee.baseline$AVDEL30MIN.z=(returnee.baseline$AVDEL30MIN-baseline.mean)/baseline.SD
    returnee.followup$AVDEL30MIN.z=(returnee.followup$AVDEL30MIN-baseline.mean)/baseline.SD
    replcaements$AVDEL30MIN.z=(replcaements$AVDEL30MIN-baseline.mean)/baseline.SD
    baseline$AVDEL30MIN.z<-(baseline$AVDEL30MIN-baseline.mean)/baseline.SD
    #residualize the cognitive scores
    
    #calculate PE for RAW Zscores, no resid for edu or anart
    RAW_RD_CN_Difference_score=median(exp(returnee.followup$AVDEL30MIN))-median(exp(replcaements$AVDEL30MIN))
    RAW_RD_CN_Attrition_effect=median(exp(returnee.baseline$AVDEL30MIN))-median(exp(baseline$AVDEL30MIN))
    RAW_RD_CN_Practice_effect=RAW_RD_CN_Difference_score-RAW_RD_CN_Attrition_effect
    
    #RAW_RD_CN_Difference_score=mean(exp(returnee.followup$AVDEL30MIN))-mean(exp(replcaements$AVDEL30MIN))
    #RAW_RD_CN_Attrition_effect=mean(exp(returnee.baseline$AVDEL30MIN))-mean(exp(baseline$AVDEL30MIN))
    #RAW_RD_CN_Practice_effect=RAW_RD_CN_Difference_score-RAW_RD_CN_Attrition_effect
    
    baseline$AVDEL30MIN=exp(baseline$AVDEL30MIN)-1
    returnee.baseline$AVDEL30MIN=exp(returnee.baseline$AVDEL30MIN)-1
    returnee.followup$AVDEL30MIN=exp(returnee.followup$AVDEL30MIN)-1
    replcaements$AVDEL30MIN=exp(replcaements$AVDEL30MIN)-1
    
    
    #Z score calcualtions 
    
    #combine bootstrapped results   
    RAW_RD_CN_PE.bootstrap=rbind(RAW_RD_CN_PE.bootstrap, RAW_RD_CN_Practice_effect)
    RAW_RD_CN_Difference.bootstrap=rbind(RAW_RD_CN_Difference.bootstrap, RAW_RD_CN_Difference_score)
    RAW_attritioneffect.bootstrap=rbind(RAW_attritioneffect.bootstrap, RAW_RD_CN_Attrition_effect)
    mean_LM.temp=data.frame('baseline'=mean(baseline$AVDEL30MIN),
                            'baseline.SD'=SD(baseline$AVDEL30MIN),
                            'returnee.SD'=SD(returnee.followup$AVDEL30MIN),
                            'returnee.baseline'=mean(returnee.baseline$AVDEL30MIN),
                            'returnee.base.sd'=SD(returnee.baseline$AVDEL30MIN),
                            'returnee.followup'=mean(returnee.followup$AVDEL30MIN), 
                            'replcacement'=mean(replcaements$AVDEL30MIN),
                            'replcacement.SD'=SD(replcaements$AVDEL30MIN))
    mean_LM.bootstrap=rbind(mean_LM.bootstrap,mean_LM.temp )
    
    
    
    
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
}  #run this block to calc PE. 
#ZRAW_RD_CN_PE.bootstrap=subset(ZRAW_RD_CN_PE.bootstrap, Pract.effect!=1000)
#describe(ZRAW_RD_CN_PE.bootstrap)

RAW_RD_CN_Difference.bootstrap=subset(RAW_RD_CN_Difference.bootstrap, Pract.effect!=1000)
RAW_RD_CN_PE.bootstrap=subset(RAW_RD_CN_PE.bootstrap, Pract.effect!=1000)
RAW_attritioneffect.bootstrap=subset(RAW_attritioneffect.bootstrap, Pract.effect!=1000)
mean_LM.bootstrap=subset(mean_LM.bootstrap, baseline!=1000 & returnee.baseline!=1000 &
                           returnee.followup!=1000 & replcacement!=1000 & baseline.SD!=1000
                         & returnee.base.sd!=1000)

describe(RAW_RD_CN_Difference.bootstrap)
describe(RAW_attritioneffect.bootstrap$Pract.effect)
describe(RAW_RD_CN_PE.bootstrap)
describe(mean_LM.bootstrap)

