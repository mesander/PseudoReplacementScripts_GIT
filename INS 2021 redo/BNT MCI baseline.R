#BNT script. 12 month follow up (actually 3rd visit)
# MCi subjects


###################################################################33


{
  #create databases by variable. merge by id and exam date? 
  #NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
  #NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
  #ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
  BNT=subset(NP_allSubs,VISCODE=='bl' | VISCODE=='m12', select=c('RID','BNTTOTAL', 'VISCODE'))
  BNT_bl=subset(DXs.2, BNTTOTAL>4)
  BNT_fu=subset(DXs.m12, BNTTOTAL>4)
  
  
  #create attrition sub database [= any subs with ONLY baseline]
  notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
  potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
  Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
  Attrition_baseline=merge(Attrition_baseline, BNT_bl, by = 'RID', all.x = TRUE)
  
  #Baseline 
  BNT_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTETHCAT', 'DX_bl','VISCODE'))
  #BNT_baseline=merge(BNT_baseline, DXs.2, by = 'RID')
  
  BNT_baseline$match=paste(BNT_baseline$RID, sep='_', BNT_baseline$EXAMDATE)
  BNT_baseline=merge(BNT_baseline, ANART, by = 'RID', all.x = TRUE)
  BNT_baseline=merge(BNT_baseline, BNT_bl, by = 'RID', all.x = TRUE)
  
  BNT_baseline$ANARTERR.log=log(BNT_baseline$ANARTERR+1)
  BNT_baseline=BNT_baseline[!is.na(BNT_baseline$BNTTOTAL),] #removes NA from the cog variable of interest
  BNT_baseline=BNT_baseline[!is.na(BNT_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score
  BNT_baseline$anart.zscore=scale(BNT_baseline$ANARTERR.log)
  #residualize cog variable to education 
  BNT_baseline$PTEDUCAT.zscore=scale(BNT_baseline$PTEDUCAT)
  
  #NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)
  BNT_baseline.MCIBL=subset(BNT_baseline, MCI_any=='1')
  BNT_baseline.MCIBL=subset(BNT_baseline.MCIBL, NP_DX!='AD')
  BNT_baseline_normals_ID=subset(BNT_baseline.MCIBL, select='RID')
  
  #  BNT_baseline.AD_BL=subset(BNT_baseline, NP_DX=='2')
  #  BNT_baseline.AD_BL.ID=subset(BNT_baseline.AD_BL, select='RID')
  
  BNT_baseline.MCI_BL=subset(BNT_baseline, MCI_any=='1')
  BNT_baseline.MCI_BL.ID=subset(BNT_baseline.MCI_BL, select='RID')
  
  #Follow up (6 months is first)
  BNT_followup=subset(Subject_selection_V1, VISCODE=='m12',
                      select = c('RID','AGE','EXAMDATE','Month_bl',
                                 'PTEDUCAT','PTETHCAT','DX','VISCODE'))
  
  BNT_followup=merge(BNT_followup, ANART, by = 'RID', all.x = TRUE)
  BNT_followup=merge(BNT_followup, BNT_fu, by = 'RID')
  
  BNT_followup$ANARTERR.log=log(BNT_followup$ANARTERR+1)
  
  BNT_followup=BNT_followup[!is.na(BNT_followup$BNTTOTAL),] #removes NA from the cog variable of interest
  BNT_followup=BNT_followup[!is.na(BNT_followup$ANARTERR.log),] #removes NA from the cog variable of interest
  
  #residualize cog variable to ANARt score 
  BNT_followup$BNTTOTAL.anart.zscore=scale(BNT_followup$ANARTERR.log)
  #BNT_followup=subset(BNT_followup, anart.zscore>1)
  BNTTOTAL.anart.resid=lm(BNTTOTAL~BNTTOTAL.anart.zscore,data=BNT_followup)
  BNT_followup$BNTTOTAL.anart.resid=(residuals(BNTTOTAL.anart.resid)+BNTTOTAL.anart.resid$coefficients[1])
  
  #adjust BNT score so that more normally distrbuted
  #BNT_followup$BNTTOTAL=log(31-BNT_followup$BNTTOTAL)
  
  #residualize cog variable to education
  BNT_followup$PTEDUCAT.zscore=scale(BNT_followup$PTEDUCAT)
  BNTTOTAL.edu.resid=lm(BNTTOTAL~PTEDUCAT.zscore,data=BNT_followup)
  BNT_followup$BNTTOTAL.edu.resid=(residuals(BNTTOTAL.edu.resid)+BNTTOTAL.edu.resid$coefficients[1])
  
  BNT_followup$age_fu=BNT_followup$AGE+(BNT_followup$Month_bl/12)
  BNT_followup$match=paste(BNT_followup$RID, sep='_', BNT_followup$EXAMDATE)
  BNT_followup.ID=subset(BNT_followup, select = 'RID')
  
  #Returnee database creation 
  BNT_returnee.MCI_baseline.ID=merge(BNT_followup.ID, BNT_baseline_normals_ID, by='RID')
  #BNT_returnee.AD_baseline.ID=merge(BNT_followup.ID, BNT_baseline.AD_BL.ID, by='RID')
  BNT_returnee.MCI_baseline.ID=merge(BNT_followup.ID, BNT_baseline.MCI_BL.ID, by='RID')
  
  BNT_returnee.MCI_baseline.DATA.V2=merge(BNT_returnee.MCI_baseline.ID, BNT_followup, by='RID')
  #BNT_returnee.AD_baseline.DATA.V2=merge(BNT_returnee.AD_baseline.ID, BNT_followup, by='RID')
  BNT_returnee.MCI_baseline.DATA.V2=merge(BNT_returnee.MCI_baseline.ID, BNT_followup, by='RID')
  
  #returnee.baseline
  BNT_returnee.MCI_baseline.DATA.V1=merge(BNT_baseline.MCIBL, BNT_returnee.MCI_baseline.ID, by='RID')
  #BNT_returnee.AD_baseline.DATA.V1=merge(BNT_baseline.AD_BL, BNT_returnee.AD_baseline.ID, by='RID')
  BNT_returnee.MCI_baseline.DATA.V1=merge(BNT_baseline.MCI_BL, BNT_returnee.MCI_baseline.ID, by='RID')
  
  #create database for standardizeing. excludes anyone who is AD
  BNT_baseline.NOTAD=subset(BNT_baseline, NP_DX!='2')
  

} #BNT database set up, Normals, baseline and m12
########################################
{
  PE=0
  BNT_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  anart_BNT_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  edu_BNT_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  ZRAW_BNT_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  RAW_BNT_MCI_PE.bootstrap=data.frame(Pract.effect=1000)
  mean_BNT.bootstrap=data.frame('baseline'=1000,
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
  
}
                                         
while (PE<5000) {
  restart=FALSE
  
    #selection of returnee subjects. Because dont' have enough replacement subs. 
  r.sample.returnees <- BNT_returnee.MCI_baseline.DATA.V2[sample(nrow(BNT_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
  #potential PR pool
  BNT_potential_PR=BNT_baseline.MCIBL[-which(BNT_baseline.MCIBL$RID %in% r.sample.returnees$RID),]
  #BNT_potential_PR=BNT_potential_PR[-which(BNT_potential_PR$RID %in% Attrition_baseline$RID),]
  
  age_bounds=describe(r.sample.returnees$age_fu)
  u_bound=age_bounds$max  
  l_bound=age_bounds$min
  BNT_potential_PR$keep=ifelse(BNT_potential_PR$AGE>u_bound,
                               -1,
                               ifelse(BNT_potential_PR$AGE<l_bound,
                                      -1,1))
  
  x=subset(BNT_potential_PR, keep==1) #removes anyone outside age range
  describe(x$RID)
  if (mean(BNT_potential_PR$keep)<.3){
    #selection of returnee subjects. Because dont' have enough replacement subs. 
    r.sample.returnees <- BNT_returnee.MCI_baseline.DATA.V2[sample(nrow(BNT_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
    #potential PR pool
    BNT_potential_PR=BNT_baseline.MCIBL[-which(BNT_baseline.MCIBL$RID %in% r.sample.returnees$RID),]
    #BNT_potential_PR=BNT_potential_PR[-which(BNT_potential_PR$RID %in% Attrition_baseline$RID),]
    
    age_bounds=describe(r.sample.returnees$age_fu)
    u_bound=age_bounds$max  
    l_bound=age_bounds$min
    BNT_potential_PR$keep=ifelse(BNT_potential_PR$AGE>u_bound,
                                 -1,
                                 ifelse(BNT_potential_PR$AGE<l_bound+age_bounds$sd,
                                        -1,1))
    
    x=subset(BNT_potential_PR, keep==1) #removes anyone outside age range)
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
      n=subset(BNT_potential_PR, AGE<mean(r.sample.returnees$age_fu))
      i=sample(n$RID, 1)
      count.low=count.low+1
    }
    if (mean(pseudo.R.subs$AGE)<mean(r.sample.returnees$age_fu)) {
      m=subset(BNT_potential_PR, AGE>mean(r.sample.returnees$age_fu))
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
    success.2=ifelse(age_dif$p.value>.7, 1, 0) #how dif the ages can be. 
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
    #  k=as.numeric(sample(x=(row.names(pseudo.R.subs)), size=1))
    #  if (success==0) {
    #    pseudo.R.subs=pseudo.R.subs[-c(k),]
    #  } #removes 1 random subs if not meeting success conditions 
    count=(count+1)
    if (count>30) {
      r.sample.returnees <- BNT_returnee.MCI_baseline.DATA.V2[sample(nrow(BNT_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      BNT_potential_PR=BNT_baseline.MCIBL[-which(BNT_baseline.MCIBL$RID %in% r.sample.returnees$RID), ]
      #BNT_potential_PR=BNT_potential_PR[-which(BNT_potential_PR$RID %in% Attrition_baseline$RID),]
      
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      BNT_potential_PR$keep=ifelse(BNT_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(BNT_potential_PR$AGE<l_bound,
                                          -1,1))     
      x=subset(BNT_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size      count.5=count.5+1
      count=0
      total.count=total.count+1
      ptemp=c('fail', total.count)
      print(ptemp)
    }
    if (success.4==1){
      r.sample.returnees <- BNT_returnee.MCI_baseline.DATA.V2[sample(nrow(BNT_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      BNT_potential_PR=BNT_baseline.MCIBL[-which(BNT_baseline.MCIBL$RID %in% r.sample.returnees$RID),]
      #BNT_potential_PR=BNT_potential_PR[-which(BNT_potential_PR$RID %in% Attrition_baseline$RID),]
      
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      BNT_potential_PR$keep=ifelse(BNT_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(BNT_potential_PR$AGE<l_bound,
                                          -1,1))      
      x=subset(BNT_potential_PR, keep==1) #removes anyone outside age range
      pseudo.R.subs <- x[sample(nrow(x), 100), ] # starting sample size      count.5=count.5+1
      count.4=count.4+1
      success=0
      total.count=total.count+1
      ptemp=c('over120', total.count)
      print(ptemp)
    }
    if (success.5==1){
      r.sample.returnees <- BNT_returnee.MCI_baseline.DATA.V2[sample(nrow(BNT_returnee.MCI_baseline.DATA.V2), 100), ] # starting sample size
      BNT_potential_PR=BNT_baseline.MCIBL[-which(BNT_baseline.MCIBL$RID %in% r.sample.returnees$RID),]
      #BNT_potential_PR=BNT_potential_PR[-which(BNT_potential_PR$RID %in% Attrition_baseline$RID),]
      
      age_bounds=describe(r.sample.returnees$age_fu)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      BNT_potential_PR$keep=ifelse(BNT_potential_PR$AGE>u_bound,
                                   -1,
                                   ifelse(BNT_potential_PR$AGE<l_bound,
                                          -1,1))      
      x=subset(BNT_potential_PR, keep==1) #removes anyone outside age range
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
  test=BNT_baseline.MCIBL
  pseudo.R.subs.ID=as.list(pseudo.R.subs$match)
  test=test[-which(test$match %in% pseudo.R.subs.ID),]
  BNT_baseline.MCIBL.withoutPR=test
  r.sample.returnees.ID=subset(r.sample.returnees, select='RID')
  
  #database
  baseline=BNT_baseline.MCIBL.withoutPR #does not include attrition replacements
  returnee.baseline=merge(BNT_returnee.MCI_baseline.DATA.V1, r.sample.returnees.ID, by = "RID")
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
  count=0
  baseline.match=0
  baseline.match.2=0
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
    baseline.match.2=ifelse(age_dif$p.value>.7, 1, 0) #how dif the ages can be. 

    
    baseline.match.4=ifelse(nrow(LM_matched_baseline)>(120), 1, 0)# sets maximum of subs
    baseline.match.5=ifelse(nrow(LM_matched_baseline)<80, 1, 0)# minimum number of subs
    #final check to see if leave loop
    #baseline.match=ifelse(baseline.match.2==1 & baseline.match.3==1, 1, 0) #final check to see if leave loop
    baseline.match=ifelse(baseline.match.2==1, 1, 0) #final check to see if leave loop
    
    count=(count+1)
    total.count
    if (count>100) {
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
                                   ifelse(baseline$AGE<l_bound+age_bounds$sd+1,
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
    if (restart==TRUE | total.count>50) {
      baseline.match=1
      failtext='UNABLE TO MATCH BASELINE'
      print(failtext)
      restart=TRUE
    }
  }
  
  
 while (restart==FALSE) {
   baseline=LM_matched_baseline
   
   baseline.SD=SD(baseline$BNTTOTAL)
   baseline$BNTTOTAL.Z=(baseline$BNTTOTAL-baseline.mean)/baseline.SD
   returnee.baseline$BNTTOTAL.Z=(returnee.baseline$BNTTOTAL-baseline.mean)/baseline.SD
   returnee.followup$BNTTOTAL.Z=(returnee.followup$BNTTOTAL-baseline.mean)/baseline.SD
   replcaements$BNTTOTAL.Z=(replcaements$BNTTOTAL-baseline.mean)/baseline.SD
   
   
   #calculate PE for RAW Zscores, no resid for edu or anart
   RAW_BNT_MCI_Difference_score=mean(returnee.followup$BNTTOTAL)-mean(replcaements$BNTTOTAL)
   RAW_BNT_MCI_Attrition_effect=mean(returnee.baseline$BNTTOTAL)-mean(baseline$BNTTOTAL)
   RAW_BNT_MCI_Practice_effect=RAW_BNT_MCI_Difference_score-RAW_BNT_MCI_Attrition_effect  
   #combine bootstrapped results   
   RAW_BNT_MCI_PE.bootstrap=rbind(RAW_BNT_MCI_PE.bootstrap, RAW_BNT_MCI_Practice_effect)
   RAW_attritioneffect.bootstrap=rbind(RAW_attritioneffect.bootstrap, RAW_BNT_MCI_Attrition_effect)
   
   mean_BNT.temp=data.frame('baseline'=mean(baseline$BNTTOTAL),
                            'baseline.SD'=SD(baseline$BNTTOTAL),
                            'returnee.SD'=SD(returnee.followup$BNTTOTAL),
                            'returnee.baseline'=mean(returnee.baseline$BNTTOTAL),
                            'returnee.followup'=mean(returnee.followup$BNTTOTAL), 
                            'replcacement'=mean(replcaements$BNTTOTAL),
                            'replcacement.SD'=SD(replcaements$BNTTOTAL),
                            'returnee.base.sd'=SD(returnee.baseline$BNTTOTAL))
   
   
   mean_BNT.bootstrap=rbind(mean_BNT.bootstrap,mean_BNT.temp )
   
   
   
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

RAW_BNT_MCI_PE.bootstrap=subset(RAW_BNT_MCI_PE.bootstrap, Pract.effect!=1000)
describe(RAW_BNT_MCI_PE.bootstrap)
RAW_attritioneffect.bootstrap=subset(RAW_attritioneffect.bootstrap, Pract.effect!=1000)
describe(RAW_attritioneffect.bootstrap$Pract.effect)
mean_BNT.bootstrap=subset(mean_BNT.bootstrap, baseline!=1000 & returnee.baseline!=1000 &
                            returnee.followup!=1000 & replcacement!=1000 & baseline.SD!=1000)
describe(mean_BNT.bootstrap)

###
ZRAW_BNT_MCI_PE.bootstrap=subset(ZRAW_BNT_MCI_PE.bootstrap, Pract.effect!=1000)
describe(ZRAW_BNT_MCI_PE.bootstrap)
anart_BNT_MCI_PE.bootstrap=subset(anart_BNT_MCI_PE.bootstrap, Pract.effect!=1000)
describe(anart_BNT_MCI_PE.bootstrap)

edu_BNT_MCI_PE.bootstrap=subset(edu_BNT_MCI_PE.bootstrap, Pract.effect!=1000)
describe(edu_BNT_MCI_PE.bootstrap)
