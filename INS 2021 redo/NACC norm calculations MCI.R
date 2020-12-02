# NACC norms clauclations               #
# based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3308021/ 
#
#Uses modified JAk/BONDI. Took out RVALT recog and replaced with LM delay recall
###############################################################################################################

#NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)
#Note, all people with AD were removed from DATA.V1 and DATA.V2 databases. 
   #so NO SUBS have AD at baseline or FU. 

#the below section makes the baseline and FU databases
#it makes sure not to remove people with perfect scores at both time points.
{
  { 
    #create databases by variable. merge by id and exam date? 
    NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
    NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
    ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR', 'LMSTORY'), all=TRUE)
    #remove people with perfect scores at both time points
    avlt_temp=subset(DXs, Visit=='0' | Visit=='12')
    avlt_temp.base=subset(avlt_temp, Visit=='0' & AVDEL30MIN==15, select = 'RID') #523
    avlt_temp.base_temp.fu=subset(avlt_temp, Visit=='12' & AVDEL30MIN==15, select = 'RID') #335
    exclude_avlt_temp.base=merge(avlt_temp.base, avlt_temp.base_temp.fu, by = 'RID') #147
    
    RD_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                       select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                  'PTEDUCAT','PTETHCAT', 'DX_bl','VISCODE'))
    RD_baseline=merge(RD_baseline, DXs.2, by = 'RID')
    RD_baseline$match=paste(RD_baseline$RID, sep='_', RD_baseline$EXAMDATE)
    RD_baseline=merge(RD_baseline, ANART, by = 'RID', all.x = TRUE)
    RD_baseline$ANARTERR.log=log(RD_baseline$ANARTERR+1)
    RD_baseline=RD_baseline[!is.na(RD_baseline$AVDEL30MIN),] #removes NA from the cog variable of interest
    RD_baseline=RD_baseline[!is.na(RD_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
    
    #residualize cog variable to ANARt score
    RD_baseline$anart.zscore=scale(RD_baseline$ANARTERR.log)
    #RD_baseline=subset(RD_baseline, anart.zscore>1)
    AVDEL30MIN.anart.resid=lm(AVDEL30MIN~anart.zscore,data=RD_baseline)
    RD_baseline$AVDEL30MIN.anart.resid=(residuals(AVDEL30MIN.anart.resid)+AVDEL30MIN.anart.resid$coefficients[1])
    #residualize cog variable to education 
    RD_baseline$PTEDUCAT.zscore=scale(RD_baseline$PTEDUCAT)
    AVDEL30MIN.edu.resid=lm(AVDEL30MIN~PTEDUCAT.zscore,data=RD_baseline)
    RD_baseline$AVDEL30MIN.edu.resid=(residuals(AVDEL30MIN.edu.resid)+AVDEL30MIN.edu.resid$coefficients[1])
    
    #line below removes people with perfect at both time points. 
    RD_baseline=RD_baseline[-which(RD_baseline$RID %in% exclude_avlt_temp.base$RID),]
    
    RD_baseline.normalBL=subset(RD_baseline, MCI_any==0)
    #RD_baseline.normalBL=subset(RD_baseline.normalBL, DX_bl!='AD')
    RD_baseline_normals_ID=subset(RD_baseline.normalBL, select='RID')
    
    #  RD_baseline.AD_BL=subset(RD_baseline, NP_DX==2)
    #  RD_baseline.AD_BL.ID=subset(RD_baseline.AD_BL, select='RID')
    
    RD_baseline.MCI_BL=subset(RD_baseline, MCI_any==1)
    RD_baseline.MCI_BL.ID=subset(RD_baseline.MCI_BL, select='RID')
    
    #Follow up (12 months is first)
    RD_followup=subset(Subject_selection_V1, VISCODE=='m12',
                       select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                  'PTEDUCAT','PTETHCAT','DX','VISCODE'))
    
    RD_followup=merge(RD_followup, DXs.m12, by = 'RID') #this DX is for BASELINE. they don't have it at 6months.
    
    RD_followup=merge(RD_followup, ANART, by = 'RID', all.x = TRUE)
    RD_followup$ANARTERR.log=log(RD_followup$ANARTERR+1)
    
    RD_followup=RD_followup[!is.na(RD_followup$AVDEL30MIN),] #removes NA from the cog variable of interest
    RD_followup=RD_followup[!is.na(RD_followup$ANARTERR.log),] #removes NA from the cog variable of interest
    
    #residualize cog variable to ANARt score 
    RD_followup$AVDEL30MIN.anart.zscore=scale(RD_followup$ANARTERR.log)
    #RD_followup=subset(RD_followup, anart.zscore>1)
    AVDEL30MIN.anart.resid=lm(AVDEL30MIN~AVDEL30MIN.anart.zscore,data=RD_followup)
    RD_followup$AVDEL30MIN.anart.resid=(residuals(AVDEL30MIN.anart.resid)+AVDEL30MIN.anart.resid$coefficients[1])
    
    #residualize cog variable to education
    RD_followup$PTEDUCAT.zscore=scale(RD_followup$PTEDUCAT)
    AVDEL30MIN.edu.resid=lm(AVDEL30MIN~PTEDUCAT.zscore,data=RD_followup)
    RD_followup$AVDEL30MIN.edu.resid=(residuals(AVDEL30MIN.edu.resid)+AVDEL30MIN.edu.resid$coefficients[1])
    
    RD_followup$age_fu=RD_followup$AGE+(RD_followup$Month_bl/12)
    RD_followup$match=paste(RD_followup$RID, sep='_', RD_followup$EXAMDATE)
    RD_followup.ID=subset(RD_followup, select = 'RID')
    
    #Returnee database creation 
    RD_returnee.CN_baseline.ID=merge(RD_followup.ID, RD_baseline_normals_ID, by='RID')
    # RD_returnee.AD_baseline.ID=merge(RD_followup.ID, RD_baseline.AD_BL.ID, by='RID')
    RD_returnee.MCI_baseline.ID=merge(RD_followup.ID, RD_baseline.MCI_BL.ID, by='RID')
    
    RD_returnee.CN_baseline.DATA.V2=merge(RD_returnee.CN_baseline.ID, RD_followup, by='RID')
    #RD_returnee.AD_baseline.DATA.V2=merge(RD_returnee.AD_baseline.ID, RD_followup, by='RID')
    RD_returnee.MCI_baseline.DATA.V2=merge(RD_returnee.MCI_baseline.ID, RD_followup, by='RID')
    
    #returnee.baseline
    RD_returnee.CN_baseline.DATA.V1=merge(RD_baseline.normalBL, RD_returnee.CN_baseline.ID, by='RID')
    #RD_returnee.AD_baseline.DATA.V1=merge(RD_baseline.AD_BL, RD_returnee.AD_baseline.ID, by='RID')
    RD_returnee.MCI_baseline.DATA.V1=merge(RD_baseline.MCI_BL, RD_returnee.MCI_baseline.ID, by='RID')
    
    #create database for standardizeing. excludes anyone who is AD
    RD_baseline.NOTAD=subset(RD_baseline, NP_DX!='2')
    
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
                                 'replcacement.SD'=1000)
    RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
  } #AVLT
  {
  
    #create databases by variable. merge by id and exam date? 
    NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
    NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
    ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR', 'LMSTORY'), all=TRUE)
    #remove subs with perfects at both time points 
    # there are NO subjects with perfects at both time points. 
    #logical memoery. 
    #Baseline 
    #NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)
    #NP_DX_LM (0=Jak/Bondi normal; 1=Jak/Bondi MCI; includes LM instead of RAVLT Recognition 
    
    library(haven)
    
    LM_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                       select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                  'PTEDUCAT','PTETHCAT', 'DX_bl','VISCODE'))
    LM_baseline=merge(LM_baseline, DXs.2, by = 'RID')
    LM_baseline$match=paste(LM_baseline$RID, sep='_', LM_baseline$EXAMDATE)
    LM_baseline=merge(LM_baseline, ANART, by = 'RID', all.x = TRUE)
    LM_baseline$ANARTERR.log=log(LM_baseline$ANARTERR+1)
    LM_baseline=LM_baseline[!is.na(LM_baseline$LDELTOTAL),] #removes NA from the cog variable of interest
    LM_baseline=LM_baseline[!is.na(LM_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
    
    #residualize cog variable to ANARt score
    LM_baseline$anart.zscore=scale(LM_baseline$ANARTERR.log)
    #LM_baseline=subset(LM_baseline, anart.zscore>1)
    LDELTOTAL.anart.resid=lm(LDELTOTAL~anart.zscore,data=LM_baseline)
    LM_baseline$LDELTOTAL.anart.resid=(residuals(LDELTOTAL.anart.resid)+LDELTOTAL.anart.resid$coefficients[1])
    #residualize cog variable to education 
    LM_baseline$PTEDUCAT.zscore=scale(LM_baseline$PTEDUCAT)
    LDELTOTAL.edu.resid=lm(LDELTOTAL~PTEDUCAT.zscore,data=LM_baseline)
    LM_baseline$LDELTOTAL.edu.resid=(residuals(LDELTOTAL.edu.resid)+LDELTOTAL.edu.resid$coefficients[1])
    
    LM_baseline.normalBL=subset(LM_baseline, MCI_any==0)
    #LM_baseline.normalBL=subset(LM_baseline.normalBL, DX_bl!='AD')
    LM_baseline_normals_ID=subset(LM_baseline.normalBL, select='RID')
    
    #LM_baseline.AD_BL=subset(LM_baseline, NP_DX==2)
    #LM_baseline.AD_BL.ID=subset(LM_baseline.AD_BL, select='RID')
    
    LM_baseline.MCI_BL=subset(LM_baseline, MCI_any==1)
    LM_baseline.MCI_BL.ID=subset(LM_baseline.MCI_BL, select='RID')
    
    
    #Follow up (12 months is first)
    LM_followup=subset(Subject_selection_V1, VISCODE=='m12',
                       select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                  'PTEDUCAT','PTETHCAT','DX','VISCODE'))
    
    LM_followup=merge(LM_followup, DXs.m12, by = 'RID') #this DX is for BASELINE. they don't have it at 6months.
    
    LM_followup=merge(LM_followup, ANART, by = 'RID', all.x = TRUE)
    LM_followup$ANARTERR.log=log(LM_followup$ANARTERR+1)
    
    LM_followup=LM_followup[!is.na(LM_followup$LDELTOTAL),] #removes NA from the cog variable of interest
    LM_followup=LM_followup[!is.na(LM_followup$ANARTERR.log),] #removes NA from the cog variable of interest
    
    #residualize cog variable to ANARt score 
    LM_followup$LDELTOTAL.anart.zscore=scale(LM_followup$ANARTERR.log)
    #LM_followup=subset(LM_followup, anart.zscore>1)
    LDELTOTAL.anart.resid=lm(LDELTOTAL~LDELTOTAL.anart.zscore,data=LM_followup)
    LM_followup$LDELTOTAL.anart.resid=(residuals(LDELTOTAL.anart.resid)+LDELTOTAL.anart.resid$coefficients[1])
    #residualize cog variable to education
    LM_followup$PTEDUCAT.zscore=scale(LM_followup$PTEDUCAT)
    LDELTOTAL.edu.resid=lm(LDELTOTAL~PTEDUCAT.zscore,data=LM_followup)
    LM_followup$LDELTOTAL.edu.resid=(residuals(LDELTOTAL.edu.resid)+LDELTOTAL.edu.resid$coefficients[1])
    
    LM_followup$age_fu=LM_followup$AGE+(LM_followup$Month_bl/12)
    LM_followup$match=paste(LM_followup$RID, sep='_', LM_followup$EXAMDATE)
    LM_followup.ID=subset(LM_followup, select = 'RID')
    
    #Returnee database creation 
    LM_returnee.CN_baseline.ID=merge(LM_followup.ID, LM_baseline_normals_ID, by='RID')
    #LM_returnee.AD_baseline.ID=merge(LM_followup.ID, LM_baseline.AD_BL.ID, by='RID')
    LM_returnee.MCI_baseline.ID=merge(LM_followup.ID, LM_baseline.MCI_BL.ID, by='RID')
    
    LM_returnee.CN_baseline.DATA.V2=merge(LM_returnee.CN_baseline.ID, LM_followup, by='RID')
    #LM_returnee.AD_baseline.DATA.V2=merge(LM_returnee.AD_baseline.ID, LM_followup, by='RID')
    LM_returnee.MCI_baseline.DATA.V2=merge(LM_returnee.MCI_baseline.ID, LM_followup, by='RID')
    
    #returnee.baseline
    LM_returnee.CN_baseline.DATA.V1=merge(LM_baseline.normalBL, LM_returnee.CN_baseline.ID, by='RID')
    #LM_returnee.AD_baseline.DATA.V1=merge(LM_baseline.AD_BL, LM_returnee.AD_baseline.ID, by='RID')
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
    
    
    PE=0
    LM_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    anart_LM_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    edu_LM_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    ZRAW_LM_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    RAW_LM_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    ZRAW_LM_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    RAW_LM_CN_Difference.bootstrap=data.frame(Pract.effect=1000)
    mean_LM.bootstrap=data.frame('baseline'=1000,
                                 'baseline.SD'=1000,
                                 'returnee.SD'=1000,
                                 'returnee.baseline'=1000,
                                 'returnee.followup'=1000, 
                                 'replcacement'=1000,
                                 'replcacement.SD'=1000)
    RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
  } #LM
  {
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
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                   'PTEDUCAT','PTETHCAT', 'DX_bl','VISCODE'))
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
    
    TRB_baseline.normalBL=subset(TRB_baseline, MCI_any==0)
    TRB_baseline.normalBL=subset(TRB_baseline.normalBL, NP_DX!='2')
    TRB_baseline_normals_ID=subset(TRB_baseline.normalBL, select='RID')
    
    
    #Follow up (06 months is first)
    TRB_followup=subset(Subject_selection_V1, VISCODE=='m12',
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
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
    TRB_returnee.CN_baseline.ID=merge(TRB_followup.ID, TRB_baseline_normals_ID, by='RID')
    TRB_returnee.CN_baseline.DATA.V2=merge(TRB_returnee.CN_baseline.ID, TRB_followup, by='RID')
    #returnee.baseline
    TRB_returnee.CN_baseline.DATA.V1=merge(TRB_baseline.normalBL, TRB_returnee.CN_baseline.ID, by='RID')
    #create database for standardizeing. excludes anyone who is AD
    TRB_baseline.NOTAD=subset(TRB_baseline, NP_DX!='2')
    
    TRB_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    anart_TRB_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    edu_TRB_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    ZRAW_TRB_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    RAW_TRB_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    mean_TRB.bootstrap=data.frame('baseline'=1000,
                                  'baseline.SD'=1000,
                                  'returnee.SD'=1000,
                                  'returnee.baseline'=1000,
                                  'returnee.followup'=1000, 
                                  'replcacement'=1000,
                                  'replcacement.SD'=1000)
    RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
    RAW_TRB_CN_Difference_score.bootstrap=data.frame(Pract.effect=1000)
  } # trails B
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
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                   'PTEDUCAT','PTETHCAT', 'DX_bl','VISCODE'))
    
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
    
    TRA_baseline.normalBL=subset(TRA_baseline, MCI_any=='0')
    TRA_baseline.normalBL=subset(TRA_baseline.normalBL, NP_DX!='2')
    TRA_baseline_normals_ID=subset(TRA_baseline.normalBL, select='RID')
    
    
    #Follow up (06 months is first)
    TRA_followup=subset(Subject_selection_V1, VISCODE=='m12',
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
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
    #TRA_followup=subset(TRA_followup, anart.zscore>1)
    #TRAASCOR.anart.resid=lm(TRAASCOR~TRAASCOR.anart.zscore,data=TRA_followup)
    #TRA_followup$TRAASCOR.anart.resid=(residuals(TRAASCOR.anart.resid)+TRAASCOR.anart.resid$coefficients[1])
    #residualize cog variable to education
    TRA_followup$PTEDUCAT.zscore=scale(TRA_followup$PTEDUCAT)
    #TRAASCOR.edu.resid=lm(TRAASCOR~PTEDUCAT.zscore,data=TRA_followup)
    #TRA_followup$TRAASCOR.edu.resid=(residuals(TRAASCOR.edu.resid)+TRAASCOR.edu.resid$coefficients[1])
    
    TRA_followup$age_fu=TRA_followup$AGE+(TRA_followup$Month_bl/12)
    TRA_followup$match=paste(TRA_followup$RID, sep='_', TRA_followup$EXAMDATE)
    TRA_followup.ID=subset(TRA_followup, select = 'RID')
    
    #Returnee database creation 
    TRA_returnee.CN_baseline.ID=merge(TRA_followup.ID, TRA_baseline_normals_ID, by='RID')
    TRA_returnee.CN_baseline.DATA.V2=merge(TRA_returnee.CN_baseline.ID, TRA_followup, by='RID')
    #returnee.baseline
    TRA_returnee.CN_baseline.DATA.V1=merge(TRA_baseline.normalBL, TRA_returnee.CN_baseline.ID, by='RID')
    #create database for standardizeing. excludes anyone who is AD
    TRA_baseline.NOTAD=subset(TRA_baseline, NP_DX!='2')
    
    TRA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    anart_TRA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    edu_TRA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    ZRAW_TRA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    RAW_TRA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    mean_TRA.bootstrap=data.frame('baseline'=1000,
                                  'baseline.SD'=1000,
                                  'returnee.SD'=1000,
                                  'returnee.baseline'=1000,
                                  'returnee.followup'=1000, 
                                  'replcacement'=1000,
                                  'replcacement.SD'=1000)
    RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
  } # trails A
  {
    #create databases by variable. merge by id and exam date? 
    NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
    NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
    ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
    BNT=subset(NP_allSubs,VISCODE=='bl' | VISCODE=='m12', select=c('RID','BNTTOTAL', 'VISCODE'))
    BNT_bl=subset(DXs.2, BNTTOTAL>4)
    BNT_fu=subset(DXs.m12, BNTTOTAL>4)
    
    BNT_temp=subset(DXs, Visit=='0' | Visit=='12')
    BNT_temp.base=subset(BNT_temp, Visit=='0' & BNTTOTAL==30, select = 'RID') #523
    BNT_temp.base_temp.fu=subset(BNT_temp, Visit=='12' & BNTTOTAL==30, select = 'RID') #335
    exclude_BNT_temp.base=merge(BNT_temp.base, BNT_temp.base_temp.fu, by = 'RID') #147
    
    #create attrition sub database [= any subs with ONLY baseline]
    notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
    potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
    Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
    Attrition_baseline=merge(Attrition_baseline, BNT_bl, by = 'RID', all.x = TRUE)
    
    #Baseline 
    BNT_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                   'PTEDUCAT','PTETHCAT', 'DX_bl'))
    #BNT_baseline=merge(BNT_baseline, DXs.2, by = 'RID')
    
    BNT_baseline$match=paste(BNT_baseline$RID, sep='_', BNT_baseline$EXAMDATE)
    BNT_baseline=merge(BNT_baseline, ANART, by = 'RID', all.x = TRUE)
    BNT_baseline=merge(BNT_baseline, BNT_bl, by = 'RID', all.x = TRUE)
    
    BNT_baseline$ANARTERR.log=log(BNT_baseline$ANARTERR+1)
    BNT_baseline=BNT_baseline[!is.na(BNT_baseline$BNTTOTAL),] #removes NA from the cog variable of interest
    BNT_baseline=BNT_baseline[!is.na(BNT_baseline$ANARTERR.log),] #removes NA from the cog variable of interest
    
    #residualize cog variable to ANARt score
    BNT_baseline$anart.zscore=scale(BNT_baseline$ANARTERR.log)
    #BNT_baseline=subset(BNT_baseline, anart.zscore>1)
    BNTTOTAL.anart.resid=lm(BNTTOTAL~anart.zscore,data=BNT_baseline)
    BNT_baseline$BNTTOTAL.anart.resid=(residuals(BNTTOTAL.anart.resid)+BNTTOTAL.anart.resid$coefficients[1])
    #residualize cog variable to education 
    BNT_baseline$PTEDUCAT.zscore=scale(BNT_baseline$PTEDUCAT)
    BNTTOTAL.edu.resid=lm(BNTTOTAL~PTEDUCAT.zscore,data=BNT_baseline)
    BNT_baseline$BNTTOTAL.edu.resid=(residuals(BNTTOTAL.edu.resid)+BNTTOTAL.edu.resid$coefficients[1])
    
    #adjust BNT score so that more normally distrbuted
    #BNT_baseline$BNTTOTAL=log(31-BNT_baseline$BNTTOTAL)
    
    #NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)
    BNT_baseline.normalBL=subset(BNT_baseline, MCI_any=='0')
    BNT_baseline.normalBL=subset(BNT_baseline.normalBL, NP_DX!='AD')
    BNT_baseline_normals_ID=subset(BNT_baseline.normalBL, select='RID')
    
    #  BNT_baseline.AD_BL=subset(BNT_baseline, NP_DX=='2')
    #  BNT_baseline.AD_BL.ID=subset(BNT_baseline.AD_BL, select='RID')
    
    BNT_baseline.MCI_BL=subset(BNT_baseline, MCI_any=='1')
    BNT_baseline.MCI_BL.ID=subset(BNT_baseline.MCI_BL, select='RID')
    
    #Follow up (6 months is first)
    BNT_followup=subset(Subject_selection_V1, VISCODE=='m12',
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                   'PTEDUCAT','PTETHCAT','DX'))
    
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
    BNT_returnee.CN_baseline.ID=merge(BNT_followup.ID, BNT_baseline_normals_ID, by='RID')
    #BNT_returnee.AD_baseline.ID=merge(BNT_followup.ID, BNT_baseline.AD_BL.ID, by='RID')
    BNT_returnee.MCI_baseline.ID=merge(BNT_followup.ID, BNT_baseline.MCI_BL.ID, by='RID')
    
    BNT_returnee.CN_baseline.DATA.V2=merge(BNT_returnee.CN_baseline.ID, BNT_followup, by='RID')
    #BNT_returnee.AD_baseline.DATA.V2=merge(BNT_returnee.AD_baseline.ID, BNT_followup, by='RID')
    BNT_returnee.MCI_baseline.DATA.V2=merge(BNT_returnee.MCI_baseline.ID, BNT_followup, by='RID')
    
    #returnee.baseline
    BNT_returnee.CN_baseline.DATA.V1=merge(BNT_baseline.normalBL, BNT_returnee.CN_baseline.ID, by='RID')
    #BNT_returnee.AD_baseline.DATA.V1=merge(BNT_baseline.AD_BL, BNT_returnee.AD_baseline.ID, by='RID')
    BNT_returnee.MCI_baseline.DATA.V1=merge(BNT_baseline.MCI_BL, BNT_returnee.MCI_baseline.ID, by='RID')
    
    #create database for standardizeing. excludes anyone who is AD
    BNT_baseline.NOTAD=subset(BNT_baseline, NP_DX!='2')
    
    BNT_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    anart_BNT_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    edu_BNT_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    ZRAW_BNT_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    RAW_BNT_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    mean_BNT.bootstrap=data.frame('baseline'=1000,
                                  'baseline.SD'=1000,
                                  'returnee.SD'=1000,
                                  'returnee.baseline'=1000,
                                  'returnee.followup'=1000, 
                                  'replcacement'=1000,
                                  'replcacement.SD'=1000)
    RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
  } #BNT 
  {
    #create databases by variable. merge by id and exam date? 
    NP_allSubs=read_csv("~/adni/NEUROBAT.csv")
    NP_allSubs$match=paste(NP_allSubs$RID, sep='_', NP_allSubs$EXAMDATE)
    ANART=subset(NP_allSubs, VISCODE2=='bl',select = c('RID','ANARTERR'), all=TRUE)
    CFA=subset(NP_allSubs,VISCODE=='bl' | VISCODE=='m06', select=c('RID','CATANIMSC', 'VISCODE'))
    CFA_bl=subset(DXs.2, CATANIMSC>4)
    CFA_fu=subset(DXs.m12, CATANIMSC>4)
    
    
    #create attrition sub database [= any subs with ONLY baseline]
    notbase=subset(Subject_selection_V1, VISCODE!='bl', select = 'RID')
    potential_follow_up_subs=(notbase %>% distinct())#all subs with at least 1 FU visit
    Attrition_baseline=Subject_selection_V1[-which(Subject_selection_V1$RID %in% potential_follow_up_subs$RID),]
    Attrition_baseline=merge(Attrition_baseline, CFA_bl, by = 'RID', all.x = TRUE)
    
    #Baseline 
    CFA_baseline=subset(Subject_selection_V1, VISCODE=='bl',
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                   'PTEDUCAT','PTETHCAT', 'DX_bl'))
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
    
    CFA_baseline.normalBL=subset(CFA_baseline, MCI_any=='0')
    #CFA_baseline.normalBL=subset(CFA_baseline.normalBL, DX_bl!='AD')
    CFA_baseline_normals_ID=subset(CFA_baseline.normalBL, select='RID') #638, not horrible scew
    
    #CFA_baseline.AD_BL=subset(CFA_baseline, NP_DX=='2')
    #CFA_baseline.AD_BL.ID=subset(CFA_baseline.AD_BL, select='RID') #195, heavy left skew
    
    CFA_baseline.MCI_BL=subset(CFA_baseline, MCI_any=='1')
    CFA_baseline.MCI_BL.ID=subset(CFA_baseline.MCI_BL, select='RID') #428, not horrible scew
    
    #Follow up (6 months is first)
    CFA_followup=subset(Subject_selection_V1, VISCODE=='m06',
                        select = c('RID','AGE','PTGENDER','EXAMDATE','Month_bl',
                                   'PTEDUCAT','PTETHCAT','DX'))
    
    CFA_followup=merge(CFA_followup, ANART, by = 'RID', all.x = TRUE)
    CFA_followup=merge(CFA_followup, CFA_fu, by = 'RID', all.x = TRUE)
    
    CFA_followup$ANARTERR.log=log(CFA_followup$ANARTERR+1)
    
    CFA_followup=CFA_followup[!is.na(CFA_followup$CATANIMSC),] #removes NA from the cog variable of interest
    CFA_followup=CFA_followup[!is.na(CFA_followup$ANARTERR.log),] #removes NA from the cog variable of interest
    
    #residualize cog variable to ANARt score 
    CFA_followup$CATANIMSC.anart.zscore=scale(CFA_followup$ANARTERR.log)
    #CFA_followup=subset(CFA_followup, anart.zscore>1)
    CATANIMSC.anart.resid=lm(CATANIMSC~CATANIMSC.anart.zscore,data=CFA_followup)
    CFA_followup$CATANIMSC.anart.resid=(residuals(CATANIMSC.anart.resid)+CATANIMSC.anart.resid$coefficients[1])
    #residualize cog variable to education
    CFA_followup$PTEDUCAT.zscore=scale(CFA_followup$PTEDUCAT)
    CATANIMSC.edu.resid=lm(CATANIMSC~PTEDUCAT.zscore,data=CFA_followup)
    CFA_followup$CATANIMSC.edu.resid=(residuals(CATANIMSC.edu.resid)+CATANIMSC.edu.resid$coefficients[1])
    
    CFA_followup$age_fu=CFA_followup$AGE+(CFA_followup$Month_bl/12)
    CFA_followup$match=paste(CFA_followup$RID, sep='_', CFA_followup$EXAMDATE)
    CFA_followup.ID=subset(CFA_followup, select = 'RID')
    
    #Returnee database creation 
    CFA_returnee.CN_baseline.ID=merge(CFA_followup.ID, CFA_baseline_normals_ID, by='RID')
    #CFA_returnee.AD_baseline.ID=merge(CFA_followup.ID, CFA_baseline.AD_BL.ID, by='RID')
    CFA_returnee.MCI_baseline.ID=merge(CFA_followup.ID, CFA_baseline.MCI_BL.ID, by='RID')
    
    CFA_returnee.CN_baseline.DATA.V2=merge(CFA_returnee.CN_baseline.ID, CFA_followup, by='RID') #220, not horrible skew
    #CFA_returnee.AD_baseline.DATA.V2=merge(CFA_returnee.AD_baseline.ID, CFA_followup, by='RID')
    CFA_returnee.MCI_baseline.DATA.V2=merge(CFA_returnee.MCI_baseline.ID, CFA_followup, by='RID') #493, not horrible skew
    
    #returnee.baseline
    CFA_returnee.CN_baseline.DATA.V1=merge(CFA_baseline.normalBL, CFA_returnee.CN_baseline.ID, by='RID') #220, some skew left
    #CFA_returnee.AD_baseline.DATA.V1=merge(CFA_baseline.AD_BL, CFA_returnee.AD_baseline.ID, by='RID')
    CFA_returnee.MCI_baseline.DATA.V1=merge(CFA_baseline.MCI_BL, CFA_returnee.MCI_baseline.ID, by='RID') #some skew right
    
    #create database for standardizeing. excludes anyone who is AD
    CFA_baseline.NOTAD=subset(CFA_baseline, NP_DX!='2')
    
    CFA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    anart_CFA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    edu_CFA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    ZRAW_CFA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    RAW_CFA_CN_PE.bootstrap=data.frame(Pract.effect=1000)
    mean_CFA.bootstrap=data.frame('baseline'=1000,
                                  'baseline.SD'=1000,
                                  'returnee.SD'=1000,
                                  'returnee.baseline'=1000,
                                  'returnee.followup'=1000, 
                                  'replcacement'=1000,
                                  'replcacement.SD'=1000)
    RAW_attritioneffect.bootstrap=data.frame(Pract.effect=1000)
  } #fluency
  
} #runs set up of databases. Shoulnd't need to run. 


{
#############     Mem:   LM NORMALS 12 MONTHS          ################################
#Logical Memory A: Delayed [LDELTOTAL]; had to remove the effect of time delay since we dont' have it. I don't think this is ok.
PE.value=(0) #normal's practice effect estiamte.
Post_PE_data_LM_MCI=subset(LM_returnee.MCI_baseline.DATA.V1, select=c('Month_bl','RID',
                                                                    'VISCODE','LDELTOTAL','AGE'))
Post_PE_data_LM_MCI$LDELTOTAL=round((exp(Post_PE_data_LM_MCI$LDELTOTAL)-10), digits=2)

Post_PE_data_LM_MCI$LDELTOTAL.baseline=Post_PE_data_LM_MCI$LDELTOTAL
Post_PE_data_LM_MCI$LDELTOTAL=NULL
temp_pe=subset(LM_returnee.MCI_baseline.DATA.V2, select = c('RID','LDELTOTAL', 'DX', 'age_fu'))
temp_pe$LDELTOTAL=round((exp(temp_pe$LDELTOTAL)-10), digits=2)
temp_pe$LDELTOTAL.PE=temp_pe$LDELTOTAL-PE.value
temp_pe$LDELTOTAL.NONPE=temp_pe$LDELTOTAL
temp_pe$LDELTOTAL=NULL
Post_PE_data_LM_MCI=merge(Post_PE_data_LM_MCI, temp_pe, by='RID')
#gender #men = 0, women = 1
Post_PE_data_LM_MCI=merge(Post_PE_data_LM_MCI, DXs.2, by = 'RID')
#creates a variable for Z scores for PE and nonPE
Post_PE_data_LM_MCI$LDELTOTAL.baseline.Zscore=(Post_PE_data_LM_MCI$LDELTOTAL.baseline - 
                                                (9.1318338 + (-1.38710825*Post_PE_data_LM_MCI$PTGENDER) + 
                                                   (-0.02623125*Post_PE_data_LM_MCI$AGE) + 
                                                   (0.40779118*Post_PE_data_LM_MCI$PTEDUCAT)))/4.058524
Post_PE_data_LM_MCI$LDELTOTAL.PE.Zscore=(Post_PE_data_LM_MCI$LDELTOTAL.PE - 
                                          (9.1318338 + (-1.38710825*Post_PE_data_LM_MCI$PTGENDER) + 
                                             (-0.02623125*Post_PE_data_LM_MCI$age_fu) + 
                                             (0.40779118*Post_PE_data_LM_MCI$PTEDUCAT)))/4.058524
Post_PE_data_LM_MCI$LDELTOTAL.NONPE.Zscore=(Post_PE_data_LM_MCI$LDELTOTAL.NONPE - 
                                             (9.1318338 + (-1.38710825*Post_PE_data_LM_MCI$PTGENDER) + 
                                                (-0.02623125*Post_PE_data_LM_MCI$age_fu) + 
                                                (0.40779118*Post_PE_data_LM_MCI$PTEDUCAT)))/4.058524
#now seperates into DX groups for PE and NON PE
Post_PE_data_LM_MCI$BL_DX_1.5SD_LM=ifelse(Post_PE_data_LM_MCI$LDELTOTAL.baseline.Zscore<(-1.5), -2,0)
      #above are peopel who are dx as MCI at baseline via 1.5 cutoff. There are some that are CN because I used Jak at baseline. 
Post_PE_data_LM_MCI$BL_DX_1SD_LM=ifelse(Post_PE_data_LM_MCI$LDELTOTAL.baseline.Zscore<(-1), -2,0)
      #above are people dxed as MCI at baseline. Some are DXed as CN bceause Jak requires BOTH tests to be impaired. 
      #-2 means is impaired on that test. 0 = CN
Post_PE_data_LM_MCI$DX_PE_1.5SD_LM=ifelse(Post_PE_data_LM_MCI$LDELTOTAL.PE.Zscore<(-1.5), 1,2)
Post_PE_data_LM_MCI$DX_PE_1SD_LM=ifelse(Post_PE_data_LM_MCI$LDELTOTAL.PE.Zscore<(-1), 1,2)
Post_PE_data_LM_MCI$DX_NONPE_1.5SD_LM=ifelse(Post_PE_data_LM_MCI$LDELTOTAL.NONPE.Zscore<(-1.5), 1,2)
Post_PE_data_LM_MCI$DX_NONPE_1SD_LM=ifelse(Post_PE_data_LM_MCI$LDELTOTAL.NONPE.Zscore<(-1), 1,2)
      #1 means impaired on that test. 2 means ok. 



#stability of MCI and CN based on PE, non PE; 1.5 SD_LM, 1 SD_LM
#-1 = stable MCI; 0 = reverters; 2=stable; 1=converters
Post_PE_data_LM_MCI$stability.1SD_LM.noPE=(Post_PE_data_LM_MCI$BL_DX_1SD_LM+Post_PE_data_LM_MCI$DX_NONPE_1SD_LM)
Post_PE_data_LM_MCI$stability.1SD_LM.PE=(Post_PE_data_LM_MCI$BL_DX_1SD_LM+Post_PE_data_LM_MCI$DX_PE_1SD_LM)  

Post_PE_data_LM_MCI$stability.1.5SD_LM.noPE=(Post_PE_data_LM_MCI$BL_DX_1.5SD_LM+Post_PE_data_LM_MCI$DX_NONPE_1.5SD_LM)
Post_PE_data_LM_MCI$stability.1.5SD_LM.PE=(Post_PE_data_LM_MCI$BL_DX_1.5SD_LM+Post_PE_data_LM_MCI$DX_PE_1.5SD_LM)  


sum(Post_PE_data_LM_MCI$stability.1SD_LM.noPE==-1, na.rm = TRUE) # 271 stable MCI
sum(Post_PE_data_LM_MCI$stability.1SD_LM.noPE==0, na.rm = TRUE)  # 47 reverters on THAT measure
sum(Post_PE_data_LM_MCI$stability.1SD_LM.noPE==2, na.rm = TRUE)  #20 stable CN
sum(Post_PE_data_LM_MCI$stability.1SD_LM.noPE==1, na.rm = TRUE)  #7 converters. NOTE this means they DXed as CN on THIS test. Could be MCI because of other test performance. 

sum(Post_PE_data_LM_MCI$stability.1SD_LM.PE==-1, na.rm = TRUE) #271 stable MCI
sum(Post_PE_data_LM_MCI$stability.1SD_LM.PE==0, na.rm = TRUE)  #47 reverters on THAT measure
sum(Post_PE_data_LM_MCI$stability.1SD_LM.PE==2, na.rm = TRUE)  #20 stable CN
sum(Post_PE_data_LM_MCI$stability.1SD_LM.PE==1, na.rm = TRUE)  #7 converters. NOTE this means they DXed as CN on THIS test. Could be MCI because of other test performance.



sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.noPE==-1, na.rm = TRUE) #205
sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.noPE==0, na.rm = TRUE)  #44
sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.noPE==2, na.rm = TRUE)  #64
sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.noPE==1, na.rm = TRUE)  #32

sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.PE==-1, na.rm = TRUE) #205
sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.PE==0, na.rm = TRUE)  #44
sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.PE==2, na.rm = TRUE)  #64
sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.PE==1, na.rm = TRUE)  #32

#summary of PE effect on MCI diagnosis

sum_baseline1=sum(Post_PE_data_LM_MCI$BL_DX_1SD_LM==-2, na.rm = TRUE) #318 impaired at baseline
sub_baseline1.5=sum(Post_PE_data_LM_MCI$BL_DX_1.5SD_LM==-2, na.rm = TRUE) #249 impaired at baseline
sum1=sum(Post_PE_data_LM_MCI$DX_PE_1.5SD_LM==1, na.rm = TRUE) #237 impaired with PE
sum2=sum(Post_PE_data_LM_MCI$DX_NONPE_1.5SD_LM==1, na.rm = TRUE) #237 impaired without PE
Percent_increase=(sum1-sum2)/sum2  #0% increase with 1.5 SD

sum3=sum(Post_PE_data_LM_MCI$DX_PE_1SD_LM==1, na.rm = TRUE) #278
sum4=sum(Post_PE_data_LM_MCI$DX_NONPE_1SD_LM==1, na.rm = TRUE) #278
Percent_increase.2=(sum3-sum4)/sum4 # 0% increase 1ith 1 SD      

#effect of PE on reversion rate
R_sum1=sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.PE==0, na.rm = TRUE)   #44
R_sum2=sum(Post_PE_data_LM_MCI$stability.1.5SD_LM.noPE==0, na.rm = TRUE) #44
R_Percent_increase=(R_sum1-R_sum2)/R_sum2                                #0% b/c no PE

R_sum3=sum(Post_PE_data_LM_MCI$stability.1SD_LM.PE==0, na.rm = TRUE)     #47
R_sum4=sum(Post_PE_data_LM_MCI$stability.1SD_LM.noPE==0, na.rm = TRUE)   #47
R_Percent_increase=(R_sum3-R_sum4)/R_sum4                                #0% b/c no PE


#############     Mem:   RVALT delay normals  #########################
# Memory A: Delayed [AVDEL30MIN]; had to remove the effect of time delay since we dont' have it. I don't think this is ok.
PE.value=(.2) #normal's practice effect estiamte.
Post_PE_data_RD_MCI=subset(RD_returnee.MCI_baseline.DATA.V1, select=c('Month_bl','RID',
                                                                    'VISCODE','AVDEL30MIN','AGE'))
Post_PE_data_RD_MCI$AVDEL30MIN.baseline=Post_PE_data_RD_MCI$AVDEL30MIN
Post_PE_data_RD_MCI$AVDEL30MIN=NULL
temp_pe=subset(RD_returnee.MCI_baseline.DATA.V2, select = c('RID','AVDEL30MIN', 'DX', 'age_fu'))
temp_pe$AVDEL30MIN.PE=temp_pe$AVDEL30MIN-PE.value
temp_pe$AVDEL30MIN.NONPE=temp_pe$AVDEL30MIN
temp_pe$AVDEL30MIN=NULL
Post_PE_data_RD_MCI=merge(Post_PE_data_RD_MCI, temp_pe, by='RID')
Post_PE_data_RD_MCI=merge(Post_PE_data_RD_MCI, DXs.2, by = 'RID')
#creates a variable for Z scores for PE and nonPE

#USING 

Post_PE_data_RD_MCI$AVDEL30MIN.baseline.Zscore=(Post_PE_data_RD_MCI$AVDEL30MIN.baseline-(12.95236 + (1.63084*Post_PE_data_RD_MCI$PTGENDER) + 
                                                                                         (-0.15040*Post_PE_data_RD_MCI$AGE) + 
                                                                                         (0.27743*Post_PE_data_RD_MCI$PTEDUCAT)))/3.801968

Post_PE_data_RD_MCI$AVDEL30MIN.PE.Zscore=(Post_PE_data_RD_MCI$AVDEL30MIN.PE-(12.95236 + (1.63084*Post_PE_data_RD_MCI$PTGENDER) + 
                                                                             (-0.15040*Post_PE_data_RD_MCI$age_fu) + 
                                                                             (0.27743*Post_PE_data_RD_MCI$PTEDUCAT)))/3.801968

Post_PE_data_RD_MCI$AVDEL30MIN.NONPE.Zscore=(Post_PE_data_RD_MCI$AVDEL30MIN.NONPE-(12.95236 + (1.63084*Post_PE_data_RD_MCI$PTGENDER) + 
                                                                                   (-0.15040*Post_PE_data_RD_MCI$age_fu) + 
                                                                                   (0.27743*Post_PE_data_RD_MCI$PTEDUCAT)))/3.801968

# NEED TO DOUBLE CHECK THESE, GETTING ERRORS. lots of NAs when seperate into DX groups. Eror with PE normning.
#  HAPPENING BECAUSE CODE WRITTEN TO BE == TO A VALUE (E.G., 5) BUT NOW SUBTRACT PE HAS MADE A DECIMAL (E.G., 4.72)
#   baseline



#now seperates into DX groups for PE and NON PE
Post_PE_data_RD_MCI$BL_DX_1.5SD_RD=ifelse(Post_PE_data_RD_MCI$AVDEL30MIN.baseline.Zscore<(-1.5), -2,0)
Post_PE_data_RD_MCI$BL_DX_1SD_RD=ifelse(Post_PE_data_RD_MCI$AVDEL30MIN.baseline.Zscore<(-1), -2,0)
Post_PE_data_RD_MCI$DX_PE_1.5SD_RD=ifelse(Post_PE_data_RD_MCI$AVDEL30MIN.PE.Zscore<(-1.5), 1,2)
Post_PE_data_RD_MCI$DX_PE_1SD_RD=ifelse(Post_PE_data_RD_MCI$AVDEL30MIN.PE.Zscore<(-1), 1,2)
Post_PE_data_RD_MCI$DX_NONPE_1.5SD_RD=ifelse(Post_PE_data_RD_MCI$AVDEL30MIN.NONPE.Zscore<(-1.5), 1,2)
Post_PE_data_RD_MCI$DX_NONPE_1SD_RD=ifelse(Post_PE_data_RD_MCI$AVDEL30MIN.NONPE.Zscore<(-1), 1,2)
#stability of MCI and CN based on PE, non PE; 1.5 SD_RD, 1 SD_RD
#-1 = stable MCI; 0 = reverters; 2=stable; 1=converters
Post_PE_data_RD_MCI$stability.1SD_RD.noPE=(Post_PE_data_RD_MCI$BL_DX_1SD_RD+Post_PE_data_RD_MCI$DX_NONPE_1SD_RD)
Post_PE_data_RD_MCI$stability.1SD_RD.PE=(Post_PE_data_RD_MCI$BL_DX_1SD_RD+Post_PE_data_RD_MCI$DX_PE_1SD_RD)  

Post_PE_data_RD_MCI$stability.1.5SD_RD.noPE=(Post_PE_data_RD_MCI$BL_DX_1.5SD_RD+Post_PE_data_RD_MCI$DX_NONPE_1.5SD_RD)
Post_PE_data_RD_MCI$stability.1.5SD_RD.PE=(Post_PE_data_RD_MCI$BL_DX_1.5SD_RD+Post_PE_data_RD_MCI$DX_PE_1.5SD_RD)  

sum(Post_PE_data_RD_MCI$stability.1SD_RD.noPE==-1, na.rm = TRUE) #229  stable MCI
sum(Post_PE_data_RD_MCI$stability.1SD_RD.noPE==0, na.rm = TRUE)  #51   reverters
sum(Post_PE_data_RD_MCI$stability.1SD_RD.noPE==2, na.rm = TRUE)  #49   stable CN? 
sum(Post_PE_data_RD_MCI$stability.1SD_RD.noPE==1, na.rm = TRUE)  #15   Converters

sum(Post_PE_data_RD_MCI$stability.1SD_RD.PE==-1, na.rm = TRUE)   #237
sum(Post_PE_data_RD_MCI$stability.1SD_RD.PE==0, na.rm = TRUE)    #43
sum(Post_PE_data_RD_MCI$stability.1SD_RD.PE==2, na.rm = TRUE)    #49
sum(Post_PE_data_RD_MCI$stability.1SD_RD.PE==1, na.rm = TRUE)    #16

sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.noPE==-1, na.rm = TRUE) #131
sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.noPE==0, na.rm = TRUE)  #35
sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.noPE==2, na.rm = TRUE)  #140
sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.noPE==1, na.rm = TRUE)  #38

sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.PE==-1, na.rm = TRUE)   #137
sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.PE==0, na.rm = TRUE)    #29
sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.PE==2, na.rm = TRUE)    #133
sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.PE==1, na.rm = TRUE)    #45

#summary of PE effect on MCI diagnosis

sum_baseline1=sum(Post_PE_data_RD_MCI$BL_DX_1SD_RD==-2, na.rm = TRUE)     #280/344
sub_baseline1.5=sum(Post_PE_data_RD_MCI$BL_DX_1.5SD_RD==-2, na.rm = TRUE) #166/344
sum1=sum(Post_PE_data_RD_MCI$DX_PE_1.5SD_RD==1, na.rm = TRUE)             #182/344
sum2=sum(Post_PE_data_RD_MCI$DX_NONPE_1.5SD_RD==1, na.rm = TRUE)          #169/344
Percent_increase=(sum1-sum2)/sum2  #this is for 1.5 SD_RD                  7.6923%

sum3=sum(Post_PE_data_RD_MCI$DX_PE_1SD_RD==1, na.rm = TRUE)               #252/344
sum4=sum(Post_PE_data_RD_MCI$DX_NONPE_1SD_RD==1, na.rm = TRUE)            #244/344
Percent_increase.2=(sum3-sum4)/sum4 # this is for 1 SD_RD                  3.278689%

#effect of PE on reversion rate
R_sum1=sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.PE==0, na.rm = TRUE)   #29
R_sum2=sum(Post_PE_data_RD_MCI$stability.1.5SD_RD.noPE==0, na.rm = TRUE) #35
R_Percent_increase=(R_sum1-R_sum2)/R_sum2                                #-15.68627%

R_sum3=sum(Post_PE_data_RD_MCI$stability.1SD_RD.PE==0, na.rm = TRUE)     #43
R_sum4=sum(Post_PE_data_RD_MCI$stability.1SD_RD.noPE==0, na.rm = TRUE)   #51
R_Percent_increase=(R_sum3-R_sum4)/R_sum4                                #-15.68627%. Same as 1.5?? ?  ?

##############    Lang:  Category fluency, normals, 6 monthb ###########
#Category fluency:  [CATANIMSC]; had to remove the effect of time delay since we dont' have it. I don't think this is ok.

PE=(.0) #normal's practice effect estiamte.
Post_PE_data_CAT_MCI=subset(CFA_returnee.MCI_baseline.DATA.V1, select=c('Month_bl','RID','PTGENDER',
                                                                      'PTEDUCAT','Visit','CATANIMSC','AGE'))
Post_PE_data_CAT_MCI$CATANIMSC.baseline=Post_PE_data_CAT_MCI$CATANIMSC
Post_PE_data_CAT_MCI$CATANIMSC=NULL
temp_pe=subset(CFA_returnee.MCI_baseline.DATA.V2, select = c('RID','CATANIMSC', 'DX', 'age_fu'))
temp_pe$CATANIMSC.PE=temp_pe$CATANIMSC-PE
temp_pe$CATANIMSC.NONPE=temp_pe$CATANIMSC
temp_pe$CATANIMSC=NULL
Post_PE_data_CAT_MCI=merge(Post_PE_data_CAT_MCI, temp_pe, by='RID')

#creates a variable for Z scores for PE and nonPE
Post_PE_data_CAT_MCI$CATANIMSC.baseline.Zscore=(Post_PE_data_CAT_MCI$CATANIMSC.baseline - 
                                                 (20.65774665 + (0.00454905*Post_PE_data_CAT_MCI$PTGENDER) + 
                                                    (-0.12844788*Post_PE_data_CAT_MCI$age_fu) + 
                                                    (0.56508779*Post_PE_data_CAT_MCI$PTEDUCAT)))/5.210138
Post_PE_data_CAT_MCI$CATANIMSC.PE.Zscore=(Post_PE_data_CAT_MCI$CATANIMSC.PE - 
                                           (20.65774665 + (0.00454905*Post_PE_data_CAT_MCI$PTGENDER) + 
                                              (-0.12844788*Post_PE_data_CAT_MCI$age_fu) + 
                                              (0.56508779*Post_PE_data_CAT_MCI$PTEDUCAT)))/5.210138
Post_PE_data_CAT_MCI$CATANIMSC.NONPE.Zscore=(Post_PE_data_CAT_MCI$CATANIMSC.NONPE - 
                                              (20.65774665 + (0.00454905*Post_PE_data_CAT_MCI$PTGENDER) + 
                                                 (-0.12844788*Post_PE_data_CAT_MCI$age_fu) + 
                                                 (0.56508779*Post_PE_data_CAT_MCI$PTEDUCAT)))/5.210138
#now seperates into DX groups for PE and NON PE
Post_PE_data_CAT_MCI$BL_DX_1.5SD_CAT=ifelse(Post_PE_data_CAT_MCI$CATANIMSC.baseline.Zscore<(-1.5), -2,0)
Post_PE_data_CAT_MCI$BL_DX_1SD_CAT=ifelse(Post_PE_data_CAT_MCI$CATANIMSC.baseline.Zscore<(-1), -2,0)
Post_PE_data_CAT_MCI$DX_PE_1.5SD_CAT=ifelse(Post_PE_data_CAT_MCI$CATANIMSC.PE.Zscore<(-1.5), 1,2)
Post_PE_data_CAT_MCI$DX_PE_1SD_CAT=ifelse(Post_PE_data_CAT_MCI$CATANIMSC.PE.Zscore<(-1), 1,2)
Post_PE_data_CAT_MCI$DX_NONPE_1.5SD_CAT=ifelse(Post_PE_data_CAT_MCI$CATANIMSC.NONPE.Zscore<(-1.5), 1,2)
Post_PE_data_CAT_MCI$DX_NONPE_1SD_CAT=ifelse(Post_PE_data_CAT_MCI$CATANIMSC.NONPE.Zscore<(-1), 1,2)
#stability of MCI and CN based on PE, non PE; 1.5 SD_CAT, 1 SD_CAT
#-1 = stable MCI; 0 = reverters; 2=stable; 1=converters
Post_PE_data_CAT_MCI$stability.1SD_CAT.noPE=(Post_PE_data_CAT_MCI$BL_DX_1SD_CAT+Post_PE_data_CAT_MCI$DX_NONPE_1SD_CAT)
Post_PE_data_CAT_MCI$stability.1SD_CAT.PE=(Post_PE_data_CAT_MCI$BL_DX_1SD_CAT+Post_PE_data_CAT_MCI$DX_PE_1SD_CAT)  
Post_PE_data_CAT_MCI$stability.1.5SD_CAT.noPE=(Post_PE_data_CAT_MCI$BL_DX_1.5SD_CAT+Post_PE_data_CAT_MCI$DX_NONPE_1.5SD_CAT)
Post_PE_data_CAT_MCI$stability.1.5SD_CAT.PE=(Post_PE_data_CAT_MCI$BL_DX_1.5SD_CAT+Post_PE_data_CAT_MCI$DX_PE_1.5SD_CAT)  




sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.noPE==-1, na.rm = TRUE) #71  stable impairment on this test
sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.noPE==0, na.rm = TRUE)  #15  reverters: as in impaired at baseline, then not impaired anymore. 
sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.noPE==2, na.rm = TRUE)  #81 stable Not impaired, on this test. likely are MCI because of performance in anothe domain.
sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.noPE==1, na.rm = TRUE)  #38 converters: normal at baseline, impaired at followup

sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.PE==-1, na.rm = TRUE) #71 
sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.PE==0, na.rm = TRUE)  #15 
sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.PE==2, na.rm = TRUE)  #75 
sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.PE==1, na.rm = TRUE)  #44 






sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.noPE==-1, na.rm = TRUE) #28
sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.noPE==0, na.rm = TRUE)  #13
sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.noPE==2, na.rm = TRUE)  #130
sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.noPE==1, na.rm = TRUE)  #34

sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.PE==-1, na.rm = TRUE) #37
sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.PE==0, na.rm = TRUE)  #12
sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.PE==2, na.rm = TRUE)  #127
sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.PE==1, na.rm = TRUE)  #37

#summary of PE effect on MCI diagnosis
sum1=sum(Post_PE_data_CAT_MCI$DX_PE_1.5SD_CAT==1, na.rm = TRUE)    #66/205
sum2=sum(Post_PE_data_CAT_MCI$DX_NONPE_1.5SD_CAT==1, na.rm = TRUE) #62/205
Percent_increase=(sum1-sum2)/sum2                                  #6.451613%

sum3=sum(Post_PE_data_CAT_MCI$DX_PE_1SD_CAT==1, na.rm = TRUE)      #115
sum4=sum(Post_PE_data_CAT_MCI$DX_NONPE_1SD_CAT==1, na.rm = TRUE)   #109
Percent_increase.2=(sum3-sum4)/sum4                                #5.504587

#effect of PE on reversion rate
R_sum1=sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.PE==0, na.rm = TRUE)   #12
R_sum2=sum(Post_PE_data_CAT_MCI$stability.1.5SD_CAT.noPE==0, na.rm = TRUE) #13
R_Percent_increase=(R_sum1-R_sum2)/R_sum2                                #-7.692308

R_sum3=sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.PE==0, na.rm = TRUE)     #15
R_sum4=sum(Post_PE_data_CAT_MCI$stability.1SD_CAT.noPE==0, na.rm = TRUE)   #15
R_Percent_increase=(R_sum3-R_sum4)/R_sum4                                #0 b/c no change at 1 SD.


#############     Lang:  BNT, normals, 6 months ##############################
#BNT: Delayed [BNTTOTAL]; had to remove the effect of time delay since we dont' have it. I don't think this is ok.
PE.value=(.34) #NORMAL's practice effect estiamte.
Post_PE_data_BNT_MCI=subset(BNT_returnee.MCI_baseline.DATA.V1, select=c('Month_bl','RID', 'PTGENDER',
                                                                      'PTEDUCAT','Visit','BNTTOTAL','AGE'))
Post_PE_data_BNT_MCI$BNTTOTAL.baseline=Post_PE_data_BNT_MCI$BNTTOTAL
Post_PE_data_BNT_MCI$BNTTOTAL=NULL
temp_pe=subset(BNT_returnee.MCI_baseline.DATA.V2, select = c('RID','BNTTOTAL', 'DX', 'age_fu'))
temp_pe$BNTTOTAL.PE=temp_pe$BNTTOTAL-PE.value
temp_pe$BNTTOTAL.NONPE=temp_pe$BNTTOTAL
temp_pe$BNTTOTAL=NULL
Post_PE_data_BNT_MCI=merge(Post_PE_data_BNT_MCI, temp_pe, by='RID')
#creates a variable for Z scores for PE and nonPE
Post_PE_data_BNT_MCI$BNTTOTAL.baseline.Zscore=(Post_PE_data_BNT_MCI$BNTTOTAL.baseline-
                                                (25.49287807 + (0.61686473*Post_PE_data_BNT_MCI$PTGENDER) + 
                                                   (-0.05142736*Post_PE_data_BNT_MCI$AGE) + 
                                                   (0.33669818*Post_PE_data_BNT_MCI$PTEDUCAT)))/2.992948
Post_PE_data_BNT_MCI$BNTTOTAL.PE.Zscore=(Post_PE_data_BNT_MCI$BNTTOTAL.PE - 
                                          (25.49287807 + (0.61686473*Post_PE_data_BNT_MCI$PTGENDER) + 
                                             (-0.05142736*Post_PE_data_BNT_MCI$age_fu) + 
                                             (0.33669818*Post_PE_data_BNT_MCI$PTEDUCAT)))/2.992948
Post_PE_data_BNT_MCI$BNTTOTAL.NONPE.Zscore=(Post_PE_data_BNT_MCI$BNTTOTAL.NONPE - 
                                             (25.49287807 + (0.61686473*Post_PE_data_BNT_MCI$PTGENDER) + 
                                                (-0.05142736*Post_PE_data_BNT_MCI$age_fu) + 
                                                (0.33669818*Post_PE_data_BNT_MCI$PTEDUCAT)))/2.992948
#now seperates into DX groups for PE and NON PE

Post_PE_data_BNT_MCI$BL_DX_1.5SD_BNT=ifelse(Post_PE_data_BNT_MCI$BNTTOTAL.baseline.Zscore<(-1.5), -2,0) #-2 = impaired
Post_PE_data_BNT_MCI$BL_DX_1SD_BNT=ifelse(Post_PE_data_BNT_MCI$BNTTOTAL.baseline.Zscore<(-1), -2,0)  #-2 = impaired

Post_PE_data_BNT_MCI$DX_PE_1.5SD_BNT=ifelse(Post_PE_data_BNT_MCI$BNTTOTAL.PE.Zscore<(-1.5), 1,2) #1=impaired
Post_PE_data_BNT_MCI$DX_PE_1SD_BNT=ifelse(Post_PE_data_BNT_MCI$BNTTOTAL.PE.Zscore<(-1), 1,2) #1=impaired
Post_PE_data_BNT_MCI$DX_NONPE_1.5SD_BNT=ifelse(Post_PE_data_BNT_MCI$BNTTOTAL.NONPE.Zscore<(-1.5), 1,2) #1=impaired
Post_PE_data_BNT_MCI$DX_NONPE_1SD_BNT=ifelse(Post_PE_data_BNT_MCI$BNTTOTAL.NONPE.Zscore<(-1), 1,2) #1=impaired


#stability of MCI and CN based on PE, non PE; 1.5 SD_BNT, 1 SD_BNT
#-1 = stable MCI; 0 = reverters; 2=stable; 1=converters
Post_PE_data_BNT_MCI$stability.1SD_BNT.noPE=(Post_PE_data_BNT_MCI$BL_DX_1SD_BNT+Post_PE_data_BNT_MCI$DX_NONPE_1SD_BNT)
Post_PE_data_BNT_MCI$stability.1SD_BNT.PE=(Post_PE_data_BNT_MCI$BL_DX_1SD_BNT+Post_PE_data_BNT_MCI$DX_PE_1SD_BNT)  

Post_PE_data_BNT_MCI$stability.1.5SD_BNT.noPE=(Post_PE_data_BNT_MCI$BL_DX_1.5SD_BNT+Post_PE_data_BNT_MCI$DX_NONPE_1.5SD_BNT)
Post_PE_data_BNT_MCI$stability.1.5SD_BNT.PE=(Post_PE_data_BNT_MCI$BL_DX_1.5SD_BNT+Post_PE_data_BNT_MCI$DX_PE_1.5SD_BNT)  

sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.noPE==-1, na.rm = TRUE) #49  stable MCI
sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.noPE==0, na.rm = TRUE)  #23  reverters
sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.noPE==2, na.rm = TRUE)  #121 stable CN
sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.noPE==1, na.rm = TRUE)  #12 converters

sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.PE==-1, na.rm = TRUE) #49 stable MCI
sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.PE==0, na.rm = TRUE)  #23 reverters
sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.PE==2, na.rm = TRUE)  #117 stable CN
sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.PE==1, na.rm = TRUE)  #16 converters 

sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.noPE==-1, na.rm = TRUE) #30
sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.noPE==0, na.rm = TRUE)  #18
sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.noPE==2, na.rm = TRUE)  #146
sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.noPE==1, na.rm = TRUE)  #11

sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.PE==-1, na.rm = TRUE) #34
sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.PE==0, na.rm = TRUE)  #14
sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.PE==2, na.rm = TRUE)  #143
sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.PE==1, na.rm = TRUE)  #14

#summary of PE effect on MCI diagnosis
sum1=sum(Post_PE_data_BNT_MCI$DX_PE_1.5SD_BNT==1, na.rm = TRUE)    #48/205
sum2=sum(Post_PE_data_BNT_MCI$DX_NONPE_1.5SD_BNT==1, na.rm = TRUE) #41/205
Percent_increase=(sum1-sum2)/sum2                                  #17.07317%

sum3=sum(Post_PE_data_BNT_MCI$DX_PE_1SD_BNT==1, na.rm = TRUE)      #65
sum4=sum(Post_PE_data_BNT_MCI$DX_NONPE_1SD_BNT==1, na.rm = TRUE)   #61
Percent_increase.2=(sum3-sum4)/sum4                                #6.557377%



#effect of PE on reversion rate
R_sum1=sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.PE==0, na.rm = TRUE)   #14
R_sum2=sum(Post_PE_data_BNT_MCI$stability.1.5SD_BNT.noPE==0, na.rm = TRUE) #18
R_Percent_increase=(R_sum1-R_sum2)/R_sum2                                #-22.2222%

R_sum3=sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.PE==0, na.rm = TRUE)     #15
R_sum4=sum(Post_PE_data_BNT_MCI$stability.1SD_BNT.noPE==0, na.rm = TRUE)   #15
R_Percent_increase=(R_sum3-R_sum4)/R_sum4                                #0 b/c no change at 1 SD.


#############     At/EF: Trails B, normals, 6 months ##############################
PE=(-1.54) #NORMAL's practice effect estiamte.
Post_PE_data_TRB_MCI=subset(TRB_returnee.MCI_baseline.DATA.V1, select=c('Month_bl','RID', 'PTGENDER','MCI_any',
                                                                      'PTEDUCAT','VISCODE','TRABSCOR', 'AGE'))
Post_PE_data_TRB_MCI$TRABSCOR.baseline=exp(Post_PE_data_TRB_MCI$TRABSCOR)-10
Post_PE_data_TRB_MCI$TRABSCOR=NULL
temp_pe=subset(TRB_returnee.MCI_baseline.DATA.V2, select = c('RID','TRABSCOR', 'DX', 'age_fu'))
temp_pe$TRABSCOR.PE=(exp(temp_pe$TRABSCOR)-10)-PE
temp_pe$TRABSCOR.NONPE=(exp(temp_pe$TRABSCOR)-10)
temp_pe$TRABSCOR=NULL
Post_PE_data_TRB_MCI=merge(Post_PE_data_TRB_MCI, temp_pe, by='RID')
#creates a variable for Z scores for PE and nonPE
Post_PE_data_TRB_MCI$TRABSCOR.baseline.Zscore=(-1)*((Post_PE_data_TRB_MCI$TRABSCOR.baseline-(43.216988 + 
                                                                                             (-2.948423*Post_PE_data_TRB_MCI$PTGENDER) + 
                                                                                             (1.700573*Post_PE_data_TRB_MCI$AGE) + 
                                                                                             (-4.884626*Post_PE_data_TRB_MCI$PTEDUCAT)))/(44.42661))
Post_PE_data_TRB_MCI$TRABSCOR.PE.Zscore=(-1)*((Post_PE_data_TRB_MCI$TRABSCOR.PE-(43.216988 + 
                                                                                 (-2.948423*Post_PE_data_TRB_MCI$PTGENDER) + 
                                                                                 (1.700573*Post_PE_data_TRB_MCI$age_fu) + 
                                                                                 (-4.884626*Post_PE_data_TRB_MCI$PTEDUCAT)))/(44.42661))
Post_PE_data_TRB_MCI$TRABSCOR.NONPE.Zscore=(-1)*((Post_PE_data_TRB_MCI$TRABSCOR.NONPE-(43.216988 + 
                                                                                       (-2.948423*Post_PE_data_TRB_MCI$PTGENDER) + 
                                                                                       (1.700573*Post_PE_data_TRB_MCI$age_fu) + 
                                                                                       (-4.884626*Post_PE_data_TRB_MCI$PTEDUCAT)))/(44.42661))
#now seperates into DX groups for PE and NON PE
Post_PE_data_TRB_MCI$DX_PE_1.5SD_TRB=ifelse(Post_PE_data_TRB_MCI$TRABSCOR.PE.Zscore<(-1.5), 1,2)
Post_PE_data_TRB_MCI$DX_PE_1SD_TRB=ifelse(Post_PE_data_TRB_MCI$TRABSCOR.PE.Zscore<(-1), 1,2)
Post_PE_data_TRB_MCI$DX_NONPE_1.5SD_TRB=ifelse(Post_PE_data_TRB_MCI$TRABSCOR.NONPE.Zscore<(-1.5), 1,2)
Post_PE_data_TRB_MCI$DX_NONPE_1SD_TRB=ifelse(Post_PE_data_TRB_MCI$TRABSCOR.NONPE.Zscore<(-1), 1,2)
Post_PE_data_TRB_MCI$BL_DX_1.5SD_TRB=ifelse(Post_PE_data_TRB_MCI$TRABSCOR.baseline.Zscore<(-1.5), -2,0)
Post_PE_data_TRB_MCI$BL_DX_1SD_TRB=ifelse(Post_PE_data_TRB_MCI$TRABSCOR.baseline.Zscore<(-1), -2,0)

#stability of MCI and CN based on PE, non PE; 1.5 SD_TRB, 1 SD_TRB
#-1 = stable MCI; 0 = reverters; 2=stable; 1=converters
Post_PE_data_TRB_MCI$stability.1SD_TRB.noPE=(Post_PE_data_TRB_MCI$BL_DX_1SD_TRB+Post_PE_data_TRB_MCI$DX_NONPE_1SD_TRB)
Post_PE_data_TRB_MCI$stability.1SD_TRB.PE=(Post_PE_data_TRB_MCI$BL_DX_1SD_TRB+Post_PE_data_TRB_MCI$DX_PE_1SD_TRB)  
Post_PE_data_TRB_MCI$stability.1.5SD_TRB.noPE=(Post_PE_data_TRB_MCI$BL_DX_1.5SD_TRB+Post_PE_data_TRB_MCI$DX_NONPE_1.5SD_TRB)
Post_PE_data_TRB_MCI$stability.1.5SD_TRB.PE=(Post_PE_data_TRB_MCI$BL_DX_1.5SD_TRB+Post_PE_data_TRB_MCI$DX_PE_1.5SD_TRB)  

sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.noPE==-1, na.rm = TRUE) #  87 stable impaired
sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.noPE==0, na.rm = TRUE)  #  37 impaired at baseline, then ok
sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.noPE==2, na.rm = TRUE)  # 175 stable normal
sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.noPE==1, na.rm = TRUE)  # 39 impaired at baseline then normal

sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.PE==-1, na.rm = TRUE) # 88
sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.PE==0, na.rm = TRUE)  # 36
sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.PE==2, na.rm = TRUE)  # 175
sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.PE==1, na.rm = TRUE)  # 39

sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.noPE==-1, na.rm = TRUE) #67
sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.noPE==0, na.rm = TRUE)  #25
sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.noPE==2, na.rm = TRUE)  #220
sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.noPE==1, na.rm = TRUE)  #26

sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.PE==-1, na.rm = TRUE) #67
sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.PE==0, na.rm = TRUE)  #25
sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.PE==2, na.rm = TRUE)  #220
sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.PE==1, na.rm = TRUE)  #26

#summary of PE effect on MCI diagnosis
sum1=sum(Post_PE_data_TRB_MCI$DX_PE_1.5SD_TRB==1, na.rm = TRUE)    #93
sum2=sum(Post_PE_data_TRB_MCI$DX_NONPE_1.5SD_TRB==1, na.rm = TRUE) #93
Percent_increase=(sum1-sum2)/sum2                                  #0%

sum3=sum(Post_PE_data_TRB_MCI$DX_PE_1SD_TRB==1, na.rm = TRUE)      #127
sum4=sum(Post_PE_data_TRB_MCI$DX_NONPE_1SD_TRB==1, na.rm = TRUE)   #126
Percent_increase=(sum3-sum4)/sum4                                  #.793 <1% b/c only 1 changes)

#effect of PE on reversion rate
R_sum1=sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.PE==0, na.rm = TRUE)   #25
R_sum2=sum(Post_PE_data_TRB_MCI$stability.1.5SD_TRB.noPE==0, na.rm = TRUE) #25
R_Percent_increase=(R_sum1-R_sum2)/R_sum2                                #0 b/c no change at 1 SD

R_sum3=sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.PE==0, na.rm = TRUE)     #23
R_sum4=sum(Post_PE_data_TRB_MCI$stability.1SD_TRB.noPE==0, na.rm = TRUE)   #23
R_Percent_increase=(R_sum3-R_sum4)/R_sum4                                #0 b/c no change at 1 SD.



#############     At:EF  Trails A, Normals, 6 months  ######
PE=(-11.37) #NORMAL's practice effect estiamte.
Post_PE_data_TRA_MCI=subset(TRA_returnee.MCI_baseline.DATA.V1, select=c('Month_bl','RID', 'PTGENDER',
                                                                      'PTEDUCAT','VISCODE','TRAASCOR', 'AGE'))
Post_PE_data_TRA_MCI$TRAASCOR.baseline=exp(Post_PE_data_TRA_MCI$TRAASCOR)-10
Post_PE_data_TRA_MCI$TRAASCOR=NULL
temp_pe=subset(TRA_returnee.MCI_baseline.DATA.V2, select = c('RID','TRAASCOR', 'DX', 'age_fu'))
temp_pe$TRAASCOR.PE=(exp(temp_pe$TRAASCOR)-10)-PE
temp_pe$TRAASCOR.NONPE=(exp(temp_pe$TRAASCOR)-10)
temp_pe$TRAASCOR=NULL
Post_PE_data_TRA_MCI=merge(Post_PE_data_TRA_MCI, temp_pe, by='RID')
#creates a variable for Z scores for PE and nonPE T
Post_PE_data_TRA_MCI$TRAASCOR.baseline.Zscore=(-1)*((Post_PE_data_TRA_MCI$TRAASCOR.baseline-(9.7995236 + 
                                                                                             (-0.327358*Post_PE_data_TRA_MCI$PTGENDER) + 
                                                                                             (0.5602899*Post_PE_data_TRA_MCI$AGE) +
                                                                                             (-1.0250806*Post_PE_data_TRA_MCI$PTEDUCAT)))/(13.91769))
Post_PE_data_TRA_MCI$TRAASCOR.PE.Zscore=(-1)*((Post_PE_data_TRA_MCI$TRAASCOR.PE-(9.7995236 + 
                                                                                 (-0.327358*Post_PE_data_TRA_MCI$PTGENDER) + 
                                                                                 (0.5602899*Post_PE_data_TRA_MCI$AGE) +
                                                                                 (-1.0250806*Post_PE_data_TRA_MCI$PTEDUCAT)))/(13.91769))
Post_PE_data_TRA_MCI$TRAASCOR.NONPE.Zscore=(-1)*((Post_PE_data_TRA_MCI$TRAASCOR.NONPE-(9.7995236 + 
                                                                                       (-0.327358*Post_PE_data_TRA_MCI$PTGENDER) + 
                                                                                       (0.5602899*Post_PE_data_TRA_MCI$AGE) +
                                                                                       (-1.0250806*Post_PE_data_TRA_MCI$PTEDUCAT)))/(13.91769))
#now seperates into DX groups for PE and NON PE
Post_PE_data_TRA_MCI$BL_DX_1.5SD_TRA=ifelse(Post_PE_data_TRA_MCI$TRAASCOR.baseline.Zscore<(-1.5), -2,0)
Post_PE_data_TRA_MCI$BL_DX_1SD_TRA=ifelse(Post_PE_data_TRA_MCI$TRAASCOR.baseline.Zscore<(-1), -2,0)
Post_PE_data_TRA_MCI$DX_PE_1.5SD_TRA=ifelse(Post_PE_data_TRA_MCI$TRAASCOR.PE.Zscore<(-1.5), 1,2)
Post_PE_data_TRA_MCI$DX_PE_1SD_TRA=ifelse(Post_PE_data_TRA_MCI$TRAASCOR.PE.Zscore<(-1), 1,2)
Post_PE_data_TRA_MCI$DX_NONPE_1.5SD_TRA=ifelse(Post_PE_data_TRA_MCI$TRAASCOR.NONPE.Zscore<(-1.5), 1,2)
Post_PE_data_TRA_MCI$DX_NONPE_1SD_TRA=ifelse(Post_PE_data_TRA_MCI$TRAASCOR.NONPE.Zscore<(-1), 1,2)

#stability of MCI and CN based on PE, non PE; 1.5 SD_TRA, 1 SD_TRA
#-1 = stable MCI; 0 = reverters; 2=stable; 1=converters
Post_PE_data_TRA_MCI$stability.1SD_TRA.noPE=(Post_PE_data_TRA_MCI$BL_DX_1SD_TRA+Post_PE_data_TRA_MCI$DX_NONPE_1SD_TRA)
Post_PE_data_TRA_MCI$stability.1SD_TRA.PE=(Post_PE_data_TRA_MCI$BL_DX_1SD_TRA+Post_PE_data_TRA_MCI$DX_PE_1SD_TRA)  
Post_PE_data_TRA_MCI$stability.1.5SD_TRA.noPE=(Post_PE_data_TRA_MCI$BL_DX_1.5SD_TRA+Post_PE_data_TRA_MCI$DX_NONPE_1.5SD_TRA)
Post_PE_data_TRA_MCI$stability.1.5SD_TRA.PE=(Post_PE_data_TRA_MCI$BL_DX_1.5SD_TRA+Post_PE_data_TRA_MCI$DX_PE_1.5SD_TRA)  

sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.noPE==-1, na.rm = TRUE) # 55 stable MCI
sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.noPE==0, na.rm = TRUE)  # 70 reverters
sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.noPE==2, na.rm = TRUE)  # 211 stable CN
sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.noPE==1, na.rm = TRUE)  # 10 converters

sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.PE==-1, na.rm = TRUE) # 85 stable MCI
sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.PE==0, na.rm = TRUE)  # 40 reverters
sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.PE==2, na.rm = TRUE)  # 179 stable CN
sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.PE==1, na.rm = TRUE)  # 42 converters 

sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.noPE==-1, na.rm = TRUE) #35
sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.noPE==0, na.rm = TRUE)  #39
sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.noPE==2, na.rm = TRUE)  #259
sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.noPE==1, na.rm = TRUE)  #13

sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.PE==-1, na.rm = TRUE) #45
sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.PE==0, na.rm = TRUE)  #29
sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.PE==2, na.rm = TRUE)  #236
sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.PE==1, na.rm = TRUE)  #36

#summary of PE effect on MCI diagnosis
sum1=sum(Post_PE_data_TRA_MCI$DX_PE_1.5SD_TRA==1, na.rm = TRUE)    #81/346
sum2=sum(Post_PE_data_TRA_MCI$DX_NONPE_1.5SD_TRA==1, na.rm = TRUE) #48/346
Percent_increase=(sum1-sum2)/sum2                                  #68.8% increase? 

sum3=sum(Post_PE_data_TRA_MCI$DX_PE_1SD_TRA==1, na.rm = TRUE)      #127
sum4=sum(Post_PE_data_TRA_MCI$DX_NONPE_1SD_TRA==1, na.rm = TRUE)   #65
Percent_increase=(sum3-sum4)/sum4                                  #95.3% increase

#effect of PE on reversion rate
R_sum1=sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.PE==0, na.rm = TRUE)    #29/346
R_sum2=sum(Post_PE_data_TRA_MCI$stability.1.5SD_TRA.noPE==0, na.rm = TRUE) #39/346
R_Percent_increase=(R_sum1-R_sum2)/R_sum2                                  #-25.6%. Means account for PE reduced reversio by 25.6%

R_sum3=sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.PE==0, na.rm = TRUE)     #40
R_sum4=sum(Post_PE_data_TRA_MCI$stability.1SD_TRA.noPE==0, na.rm = TRUE)   #70
R_Percent_increase=(R_sum3-R_sum4)/R_sum4                                  # -42.85714%





} #runs set up of PE adjusted databses. Run before affect of PE on diagnoses rate 

######Analysis B: Effect of PE on MCI diagnosis rate #####  

###################################################
#             Langauge: BNT and Cat               #
###################################################

#NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)

library(haven)
Post_PE_data_BNT_MCI=merge(Post_PE_data_BNT_MCI, DXs.2, by='RID')
MCI_comp_MCIbaseline.1=subset(Post_PE_data_BNT_MCI, select=c('RID','NP_DX','MCI_any',
                                                   'DX_PE_1.5SD_BNT', 'DX_NONPE_1.5SD_BNT',
                                                   'DX_PE_1SD_BNT','DX_NONPE_1SD_BNT','DX',
                                                   'BL_DX_1.5SD_BNT','BL_DX_1SD_BNT'))
Post_PE_data_CAT_MCI=merge(Post_PE_data_CAT_MCI, DXs.2, by='RID')
MCI_comp_MCIbaseline.2=subset(Post_PE_data_CAT_MCI, select=c('RID',
                                                   'DX_PE_1.5SD_CAT', 'DX_NONPE_1.5SD_CAT',
                                                   'DX_PE_1SD_CAT','DX_NONPE_1SD_CAT',
                                                   'BL_DX_1.5SD_CAT','BL_DX_1SD_CAT'))
MCI_merge.fluency=merge(MCI_comp_MCIbaseline.1, MCI_comp_MCIbaseline.2, by='RID', all = TRUE)


MCI_merge.fluency$sum.fluency.PE<-(MCI_merge.fluency$DX_PE_1SD_BNT+MCI_merge.fluency$DX_PE_1SD_CAT) #2 = impaired on both. 3 = impaired on 1. 4= NI on both.
MCI_merge.fluency$sum.fluency.NOPE<-(MCI_merge.fluency$DX_NONPE_1SD_BNT+MCI_merge.fluency$DX_NONPE_1SD_CAT) #2 = impaired on both. 3 = impaired on 1. 4= NI on both.


sum(MCI_merge.fluency$sum.fluency.NOPE==2, na.rm = TRUE) #44  impaired on both tests at FU; MCI
sum(MCI_merge.fluency$sum.fluency.NOPE==3, na.rm = TRUE) #78  impaired on 1 test at FU
sum(MCI_merge.fluency$sum.fluency.NOPE==4, na.rm = TRUE) #80  NI on both tests at FU
sum(is.na(MCI_merge.fluency$sum.fluency.NOPE))           #6   Missing a test. 

sum(MCI_merge.fluency$sum.fluency.PE==2, na.rm = TRUE)   #49  
sum(MCI_merge.fluency$sum.fluency.PE==3, na.rm = TRUE)   #78  
sum(MCI_merge.fluency$sum.fluency.PE==4, na.rm = TRUE)   #75  
sum(is.na(MCI_merge.fluency$sum.fluency.PE))             #6  


MCI_merge.fluency$NewDXinfoPE=MCI_merge.fluency$sum.fluency.PE
MCI_merge.fluency$NewDXinfo_NONPE=MCI_merge.fluency$sum.fluency.NOPE


MCI_merge.fluency$sum.fluency.PE_PETERSON<-ifelse(MCI_merge.fluency$DX_PE_1.5SD_BNT==1 |MCI_merge.fluency$DX_PE_1.5SD_CAT==1,1,0)
MCI_merge.fluency$sum.fluency.NOPE_PETERSON<-ifelse(MCI_merge.fluency$DX_NONPE_1.5SD_BNT==1|MCI_merge.fluency$DX_NONPE_1.5SD_CAT==1,1,0)
sum(MCI_merge.fluency$sum.fluency.NOPE_PETERSON==1, na.rm=TRUE) #81 MCI with no PE
sum(MCI_merge.fluency$sum.fluency.PE_PETERSON==1, na.rm=TRUE)   #87 MCi with PE

#set up for making new PE DX database
New_DX_data.1=subset(MCI_merge.fluency, select=c('RID',
                                                 'DX_PE_1.5SD_BNT', 'DX_NONPE_1.5SD_BNT',
                                                 'DX_PE_1SD_BNT','DX_NONPE_1SD_BNT',
                                                 'DX_PE_1.5SD_CAT', 'DX_NONPE_1.5SD_CAT',
                                                 'DX_PE_1SD_CAT','DX_NONPE_1SD_CAT',
                                                 'sum.fluency.PE', 'sum.fluency.NOPE',
                                                 'sum.fluency.PE_PETERSON','sum.fluency.NOPE_PETERSON'))
############################################
#             Trails A and Trials B        #
############################################
#NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)

#Post_PE_data_TRB_MCI=merge(Post_PE_data_TRB_MCI, DXs.2, by='RID')
MCI_comp_MCIbaseline.1=subset(Post_PE_data_TRB_MCI, select=c('RID','MCI_any',
                                                   'DX_PE_1.5SD_TRB', 'DX_NONPE_1.5SD_TRB',
                                                   'DX_PE_1SD_TRB','DX_NONPE_1SD_TRB','DX',
                                                   'BL_DX_1.5SD_TRB','BL_DX_1SD_TRB'))
#Post_PE_data_TRA_MCI=merge(Post_PE_data_TRA_MCI, DXs.2, by='RID')
MCI_comp_MCIbaseline.2=subset(Post_PE_data_TRA_MCI, select=c('RID',
                                                   'DX_PE_1.5SD_TRA', 'DX_NONPE_1.5SD_TRA',
                                                   'DX_PE_1SD_TRA','DX_NONPE_1SD_TRA',
                                                   'BL_DX_1.5SD_TRA','BL_DX_1SD_TRA'))
MCI_merge.ATEF=merge(MCI_comp_MCIbaseline.1, MCI_comp_MCIbaseline.2, by='RID', all = TRUE)

MCI_merge.ATEF$sum.ATEF.PE<-(MCI_merge.ATEF$DX_PE_1SD_TRB+MCI_merge.ATEF$DX_PE_1SD_TRA)
MCI_merge.ATEF$sum.ATEF.NOPE<-(MCI_merge.ATEF$DX_NONPE_1SD_TRB+MCI_merge.ATEF$DX_NONPE_1SD_TRA)


sum(MCI_merge.ATEF$sum.ATEF.NOPE==2, na.rm = TRUE) #44  impaired on both tests at FU; MCI
sum(MCI_merge.ATEF$sum.ATEF.NOPE==3, na.rm = TRUE) #99  impaired on 1 test at FU
sum(MCI_merge.ATEF$sum.ATEF.NOPE==4, na.rm = TRUE) #195  NI on both tests at FU
sum(is.na(MCI_merge.ATEF$sum.ATEF.NOPE))           #8   Missing a test. 

sum(MCI_merge.ATEF$sum.ATEF.PE==2, na.rm = TRUE)   #79
sum(MCI_merge.ATEF$sum.ATEF.PE==3, na.rm = TRUE)   #92  
sum(MCI_merge.ATEF$sum.ATEF.PE==4, na.rm = TRUE)   #167  
sum(is.na(MCI_merge.ATEF$sum.ATEF.PE))             #8

MCI_merge.ATEF$NewDXinfoPE=MCI_merge.ATEF$sum.ATEF.PE
MCI_merge.ATEF$NewDXinfo_NONPE=MCI_merge.ATEF$
  

#peterson: any test in that domain that falls below 1.5SD
MCI_merge.ATEF$sum.ATEF.PE_PETERSON<-ifelse(MCI_merge.ATEF$DX_PE_1.5SD_TRB==1|MCI_merge.ATEF$DX_PE_1.5SD_TRA==1,1,0)
MCI_merge.ATEF$sum.ATEF.NOPE_PETERSON<-ifelse(MCI_merge.ATEF$DX_NONPE_1.5SD_TRB==1|MCI_merge.ATEF$DX_NONPE_1.5SD_TRA==1,1,0)
sum(MCI_merge.ATEF$sum.ATEF.NOPE_PETERSON==1, na.rm=TRUE) #109 MCI with no PE
sum(MCI_merge.ATEF$sum.ATEF.PE_PETERSON==1, na.rm=TRUE)   #130 MCi with PE

#set up for new MCI database
#set up for merging databases
New_DX_data.2=subset(MCI_merge.ATEF, select=c('RID',
                                              'DX_PE_1.5SD_TRB', 'DX_NONPE_1.5SD_TRB',
                                              'DX_PE_1SD_TRB','DX_NONPE_1SD_TRB',
                                              'DX_PE_1.5SD_TRA', 'DX_NONPE_1.5SD_TRA',
                                              'DX_PE_1SD_TRA','DX_NONPE_1SD_TRA',
                                              'sum.ATEF.NOPE','sum.ATEF.PE',
                                              'sum.ATEF.PE_PETERSON','sum.ATEF.NOPE_PETERSON'))


############################################
#             Memory: RVLT delay, LM delay        #
############################################
#NP_DX (0=Jak/Bondi normal; 1=Jak/Bondi MCI; 2=ADNI dementia)
MCI_comp_MCIbaseline.1=subset(Post_PE_data_RD_MCI, select=c('RID',
                                                  'DX_PE_1.5SD_RD', 'DX_NONPE_1.5SD_RD',
                                                  'DX_PE_1SD_RD','DX_NONPE_1SD_RD','DX',
                                                  'BL_DX_1.5SD_RD','BL_DX_1SD_RD'))
#Post_PE_data_LM_CN_test=merge(Post_PE_data_LM_CN, DXs.2, by='RID')
MCI_comp_MCIbaseline.2=subset(Post_PE_data_LM_MCI, select=c('RID','MCI_any','Visit',
                                                  'DX_PE_1.5SD_LM', 'DX_NONPE_1.5SD_LM',
                                                  'DX_PE_1SD_LM','DX_NONPE_1SD_LM',
                                                  'BL_DX_1.5SD_LM','BL_DX_1SD_LM'))
MCI_merge.memory=merge(MCI_comp_MCIbaseline.1, MCI_comp_MCIbaseline.2, by='RID', all = TRUE)
#below code combines dif tests.

MCI_merge.memory$sum.memory.PE<-(MCI_merge.memory$DX_PE_1SD_RD+MCI_merge.memory$DX_PE_1SD_LM)
MCI_merge.memory$sum.memory.NOPE<-(MCI_merge.memory$DX_NONPE_1SD_RD+MCI_merge.memory$DX_NONPE_1SD_LM)
  #sum variables range from 2-4: 2= impaired on both. 4= normal on both. 3= impaired on 1.   

sum(MCI_merge.memory$sum.memory.NOPE==2, na.rm = TRUE) #217  impaired on both tests at FU; MCI
sum(MCI_merge.memory$sum.memory.NOPE==3, na.rm = TRUE) #87 impaired on 1 test at FU
sum(MCI_merge.memory$sum.memory.NOPE==4, na.rm = TRUE) #39  NI on both tests at FU
sum(is.na(MCI_merge.memory$sum.memory.NOPE))           #3   Missing a test. 

sum(MCI_merge.memory$sum.memory.PE==2, na.rm = TRUE)   #225
sum(MCI_merge.memory$sum.memory.PE==3, na.rm = TRUE)   #79 
sum(MCI_merge.memory$sum.memory.PE==4, na.rm = TRUE)   #39  
sum(is.na(MCI_merge.memory$sum.memory.PE))             #3

MCI_merge.memory$NewDXinfoPE=MCI_merge.memory$sum.memory.PE
MCI_merge.memory$NewDXinfo_NONPE=MCI_merge.memory$sum.memory.NOPE


#peterson: any test in that domain that falls below 1.5SD
MCI_merge.memory$sum.memory.PE_PETERSON<-ifelse(MCI_merge.memory$DX_PE_1.5SD_RD==1 |MCI_merge.memory$DX_PE_1.5SD_LM==1,1,0)
MCI_merge.memory$sum.memory.NOPE_PETERSON<-ifelse(MCI_merge.memory$DX_NONPE_1.5SD_RD==1|MCI_merge.memory$DX_NONPE_1.5SD_LM==1,1,0)
sum(MCI_merge.memory$sum.memory.NOPE_PETERSON==1, na.rm=TRUE) #267 MCI with no PE
sum(MCI_merge.memory$sum.memory.PE_PETERSON==1, na.rm=TRUE)   #270 MCi with PE


MCI_merge.memory$NewDXinfo_PE_1.5.mem=MCI_merge.memory$sum.memory.PE_PETERSON
MCI_merge.memory$NewDXinfo_NONPE_1.5.mem=MCI_merge.memory$sum.memory.NOPE_PETERSON

#set up for merging databases
New_DX_data.3=subset(MCI_merge.memory, select=c('RID',
                                                'DX_PE_1.5SD_RD', 'DX_NONPE_1.5SD_RD',
                                                'DX_PE_1SD_RD','DX_NONPE_1SD_RD',
                                                'DX_PE_1.5SD_LM', 'DX_NONPE_1.5SD_LM',
                                                'DX_PE_1SD_LM','DX_NONPE_1SD_LM',
                                                'sum.memory.PE','sum.memory.NOPE',
                                                'sum.memory.PE_PETERSON','sum.memory.NOPE_PETERSON',
                                                'NewDXinfo_PE_1.5.mem','NewDXinfo_NONPE_1.5.mem'))

###############################################
#below section combines new DX databases + Xe of interest
#into new database for use of progression to AD, biomarkers etc
###############################################
{
#merge together DX databases
#DX.m12=subset(DXs, Visit=='12')
library(sjstats)
New_DX_data=merge(New_DX_data.1, New_DX_data.2, by='RID', all=TRUE)
New_DX_data=merge(New_DX_data, New_DX_data.3, by='RID', all=TRUE)
New_DX_data=merge(New_DX_data, DXs.m12, by = 'RID', all.x=TRUE)

  #bring in baseline domain dxs. 
baseline.merge=subset(DXs.2, select = c('RID','lang_impaired_new',"ATEF_impaired_new", "mem_impaired_LM"))
  colnames(baseline.merge)[2]='lang_impaired_baseline'
  colnames(baseline.merge)[3]='ATEF_impaired_new_baseline'
  colnames(baseline.merge)[4]='mem_impaired_LM_baseline'

  
New_DX_data=merge(New_DX_data, baseline.merge, by = 'RID', all.x = TRUE)

#totals in Who stays MCI by PE and NOPE (some people are impaired in multiple domains)

New_DX_data$reverter_count_memory=ifelse(New_DX_data$mem_impaired_LM_baseline==1 & New_DX_data$sum.memory.NOPE==2,1,0)
New_DX_data$reverter_count_lang=ifelse(New_DX_data$lang_impaired_baseline==1 & New_DX_data$sum.fluency.NOPE==2,1,0)
New_DX_data$reverter_count_atef=ifelse(New_DX_data$ATEF_impaired_new_baseline ==1 & New_DX_data$sum.ATEF.NOPE ==2,1, 0)

sum(New_DX_data$reverter_count_memory==1, na.rm = TRUE) #207
sum(New_DX_data$reverter_count_lang==1, na.rm = TRUE)   #31
sum(New_DX_data$reverter_count_atef==1, na.rm = TRUE)   #38


New_DX_data$NOPE_reverter_count_memory=ifelse(New_DX_data$mem_impaired_LM_baseline==1 & New_DX_data$sum.memory.PE==2,1,0)
New_DX_data$NOPE_reverter_count_lang=ifelse(New_DX_data$lang_impaired_baseline==1 & New_DX_data$sum.fluency.PE==2,1,0)
New_DX_data$NOPE_reverter_count_atef=ifelse(New_DX_data$ATEF_impaired_new_baseline ==1 & New_DX_data$sum.ATEF.PE ==2,1, 0)

sum(New_DX_data$NOPE_reverter_count_memory==1, na.rm = TRUE) #215
sum(New_DX_data$NOPE_reverter_count_lang==1, na.rm = TRUE)   #31
sum(New_DX_data$NOPE_reverter_count_atef==1, na.rm = TRUE)   #53


#total differnce in MCI prevealnce within each domain (some people are impaired in multiple domains)
      #if person was able to complete all tests at FU. Will probably need to divide by dif number each time. 

  #memory prevelence [NO PE]
  sum(New_DX_data$sum.memory.NOPE==2, na.rm=TRUE) #217
  sum(New_DX_data$sum.memory.NOPE!=2, na.rm=TRUE) #126
    sum(is.na(New_DX_data$sum.memory.NOPE))         #4 NAs. 343 total. -> 339 completed tests
  #fluency [NO PE]
  sum(New_DX_data$sum.fluency.NOPE==2, na.rm=TRUE) #44
  sum(New_DX_data$sum.fluency.NOPE!=2, na.rm=TRUE) #158
    sum(is.na(New_DX_data$sum.fluency.NOPE))         #145 NAs. 202 rows -> 57 completed tests. 
  #atef [NO PE]
  sum(New_DX_data$sum.ATEF.NOPE==2, na.rm=TRUE) #44
  sum(New_DX_data$sum.ATEF.NOPE!=2, na.rm=TRUE) #294
    sum(is.na(New_DX_data$sum.ATEF.NOPE))         #9 NAs. 338 rows -> 329 completed tests. 
  
    
  #memory prevelence
  sum(New_DX_data$sum.memory.PE==2, na.rm=TRUE) #225
  sum(New_DX_data$sum.memory.PE!=2, na.rm=TRUE) #118
  sum(is.na(New_DX_data$sum.memory.PE))         #4 NAs. 343 total. -> 339 completed tests
  #fluency
  sum(New_DX_data$sum.fluency.PE==2, na.rm=TRUE) #44
  sum(New_DX_data$sum.fluency.PE!=2, na.rm=TRUE) #153
  sum(is.na(New_DX_data$sum.fluency.PE))         #145 NAs. 202 rows -> 57 completed tests. 
  #atef
  sum(New_DX_data$sum.ATEF.PE==2, na.rm=TRUE) #79
  sum(New_DX_data$sum.ATEF.PE!=2, na.rm=TRUE) #259
  sum(is.na(New_DX_data$sum.ATEF.PE))         #9 NAs. 338 rows -> 329 completed tests. 
  
    
  
#below gets total prevelance of MCI.  Did not exclude people who had incomplete tests in other domains.People can be multiple domain MCI, and will be counted in each domain. 
  
  New_DX_data$totalMCI.PE=ifelse(New_DX_data$sum.memory.PE==2 |New_DX_data$sum.fluency.PE==2 | New_DX_data$sum.ATEF.PE==2,1,0)
  New_DX_data$totalMCI.NOPE=ifelse(New_DX_data$sum.memory.NOPE==2 |New_DX_data$sum.fluency.NOPE==2 | New_DX_data$sum.ATEF.NOPE==2,1,0)
  
  New_DX_data_TOTALCOUNTS=subset(New_DX_data, New_DX_data$totalMCI.PE!='NA' &New_DX_data$totalMCI.NOPE!='NA') #total of 291 people with a posisble DX. 

sum(New_DX_data_TOTALCOUNTS$totalMCI.NOPE==1)
sum(New_DX_data_TOTALCOUNTS$totalMCI.PE==1)


#below looks at who progresses from single domain to multil


      #creates database for who is single domain at baseline
  New_DX_data$SingleDomainBasline=ifelse(New_DX_data$lang_impaired_baseline==1 & New_DX_data$mem_impaired_LM_baseline==0 & New_DX_data$ATEF_impaired_new_baseline==0, 1,
                                  ifelse(New_DX_data$lang_impaired_baseline==0 & New_DX_data$mem_impaired_LM_baseline==1 & New_DX_data$ATEF_impaired_new_baseline==0, 1,
                                  ifelse(New_DX_data$lang_impaired_baseline==0 & New_DX_data$mem_impaired_LM_baseline==0 & New_DX_data$ATEF_impaired_new_baseline==1, 1, 0)))

  sum(New_DX_data$SingleDomainBasline==1, na.rm=TRUE)
  sum(is.na(New_DX_data$SingleDomainBasline)) #NA =2. Because all these subs ARE MCI at baseline, that means 2 subs don't hav data in all three categories
        #looked up these individual NA subs (New_DX_data[35,]) (New_DX_data[2,]). 1 is ATEF impaired, CN on lang, missing memory. 1 is Mem impaired. missing AT, Cn on lang. 

  New_DX_data$SingleDomainBasline_lang=ifelse(New_DX_data$lang_impaired_baseline==1 & New_DX_data$mem_impaired_LM_baseline==0 & New_DX_data$ATEF_impaired_new_baseline==0, 1,0)
  New_DX_data$SingleDomainBasline_memory=ifelse(New_DX_data$lang_impaired_baseline==0 & New_DX_data$mem_impaired_LM_baseline==1 & New_DX_data$ATEF_impaired_new_baseline==0, 1,0)
  New_DX_data$SingleDomainBasline_atef=ifelse(New_DX_data$lang_impaired_baseline==0 & New_DX_data$mem_impaired_LM_baseline==0 & New_DX_data$ATEF_impaired_new_baseline==1, 1, 0)
  
  sum(New_DX_data$SingleDomainBasline_memory==1, na.rm=TRUE)
  sum(New_DX_data$SingleDomainBasline_lang==1, na.rm = TRUE)
  sum(New_DX_data$SingleDomainBasline_atef==1, na.rm = TRUE)
  
  #!!!!!!!!!!!!!!!!!!manually edited the NA subs to the correct DX here. 
  New_DX_data$SingleDomainBasline[2]=1
  New_DX_data$SingleDomainBasline[35]=1
  New_DX_data$SingleDomainBasline_memory[2]=1
  New_DX_data$SingleDomainBasline_atef[35]=1
  
  
  #create IDs for those who were single domain at baseline, stayed that domain, and progressed to another domain. 
        #no practice effects
  New_DX_data$mem_progression=ifelse(New_DX_data$SingleDomainBasline_memory==1 & New_DX_data$sum.memory.NOPE==2 & (New_DX_data$sum.fluency.NOPE==2 | New_DX_data$sum.ATEF.NOPE==2),1,0)
  New_DX_data$atef_progression=ifelse(New_DX_data$SingleDomainBasline_atef==1 & New_DX_data$sum.ATEF.NOPE==2 & (New_DX_data$sum.fluency.NOPE==2 | New_DX_data$sum.memory.NOPE==2),1,0)
  New_DX_data$lang_progression=ifelse(New_DX_data$SingleDomainBasline_lang==1 & New_DX_data$sum.fluency.NOPE==2 & (New_DX_data$sum.memory.NOPE==2 | New_DX_data$sum.ATEF.NOPE==2),1,0)
        #practice effects
  New_DX_data$mem_progression_PE=ifelse(New_DX_data$SingleDomainBasline_memory==1 & New_DX_data$sum.memory.PE==2 & (New_DX_data$sum.fluency.PE==2 | New_DX_data$sum.ATEF.PE==2),1,0)
  New_DX_data$atef_progression_PE=ifelse(New_DX_data$SingleDomainBasline_atef==1 & New_DX_data$sum.ATEF.PE==2 & (New_DX_data$sum.fluency.PE==2 | New_DX_data$sum.memory.PE==2),1,0)
  New_DX_data$lang_progression_PE=ifelse(New_DX_data$SingleDomainBasline_lang==1 & New_DX_data$sum.fluency.PE==2 & (New_DX_data$sum.memory.PE==2 | New_DX_data$sum.ATEF.PE==2),1,0)
  
  
      #memnory
  sum(New_DX_data$mem_progression==1, na.rm=TRUE)     #11
  sum(New_DX_data$mem_progression_PE==1, na.rm=TRUE)  #23

      #ATEF
  sum(New_DX_data$atef_progression==1, na.rm=TRUE)    #3
  sum(New_DX_data$atef_progression_PE==1, na.rm=TRUE) #3
      #lang
  sum(New_DX_data$lang_progression==1, na.rm=TRUE)    #1
  sum(New_DX_data$lang_progression_PE==1, na.rm=TRUE) #2
  
  


#below IDs people who are CN on all tests. 
New_DX_data$CN_NONPE=ifelse(New_DX_data$sum.ATEF.NOPE==4 &  New_DX_data$sum.memory.NOPE==4 &  New_DX_data$sum.fluency.NOPE==4, 1,0)
New_DX_data$CN_PE=ifelse(New_DX_data$sum.ATEF.PE==4 &  New_DX_data$sum.memory.PE==4 &  New_DX_data$sum.fluency.PE==4, 1,0)

sum(New_DX_data$CN_NONPE==1, na.rm = TRUE)
sum(New_DX_data$CN_PE==1, na.rm = TRUE)

#below creates new DX varlaibes with 1= ANY MCI, and 0 = normal. Using 1SD. 
New_DX_data$MCI_any_NONPE=ifelse(New_DX_data$sum.ATEF.NOPE==2 |  New_DX_data$sum.memory.NOPE==2 |  New_DX_data$sum.fluency.NOPE==2, 1,0)
New_DX_data$MCI_any_PE=ifelse(New_DX_data$sum.ATEF.PE==2 |  New_DX_data$sum.memory.PE==2 |  New_DX_data$sum.fluency.PE==2, 1,0)

#peterson any MCI variable. 
New_DX_data$MCI_any_NONPE_Pet=ifelse(New_DX_data$sum.ATEF.NOPE_PETERSON==1 |  New_DX_data$sum.memory.NOPE_PETERSON==1 |  New_DX_data$sum.fluency.NOPE_PETERSON==1, 1,0)
New_DX_data$MCI_any_PE_Pet=ifelse(New_DX_data$sum.ATEF.PE_PETERSON==1 |  New_DX_data$sum.memory.PE_PETERSON==1 |  New_DX_data$sum.fluency.PE_PETERSON==1, 1,0)


#below sums up DXs.
sum(New_DX_data$MCI_any_NONPE==1, na.rm = TRUE) #248
sum(New_DX_data$MCI_any_PE==1, na.rm = TRUE) #266
sum(is.na(New_DX_data$MCI_any_NONPE)) #56
sum(is.na(New_DX_data$MCI_any_PE))  #50

#sum(New_DX_data$MCI_any==1, na.rm = TRUE) #96

sum(New_DX_data$sum.ATEF.NOPE==2, na.rm = TRUE)      #44
sum(New_DX_data$sum.ATEF.PE==2, na.rm = TRUE)        #79

sum(New_DX_data$sum.memory.NOPE==2, na.rm = TRUE)    #217
sum(New_DX_data$sum.memory.PE==2, na.rm = TRUE)      #225

sum(New_DX_data$sum.fluency.NOPE==2, na.rm = TRUE)   #44
sum(New_DX_data$sum.fluency.PE==2, na.rm = TRUE)     #49




#DEMOGRAPHICS
demobaseline=subset(DXs.2, select = c('RID','Age','PTGENDER','PTEDUCAT', 'MCI_any'))
demoFU=subset(DXs, Visit==12, select = c('RID','Age','Visit', 'MCI_any'))

New_DX_data_demo=merge(New_DX_data, demobaseline, by = 'RID', all.x = TRUE)

New_DX_data_demo_FU=merge(New_DX_data, demoFU, by = 'RID', all.x = TRUE)


#total differnce in impaired DX (some people are impaired in multiple domains)
NONPE=(sum(New_DX_data$sum.ATEF.NOPE==2, na.rm = TRUE) +sum(New_DX_data$sum.memory.NOPE==2, na.rm = TRUE)+sum(New_DX_data$sum.fluency.NOPE==2, na.rm = TRUE)) #305
PE=(sum(New_DX_data$sum.ATEF.PE==2, na.rm = TRUE) +sum(New_DX_data$sum.memory.PE==2, na.rm = TRUE)+sum(New_DX_data$sum.fluency.PE==2, na.rm = TRUE)) #353

New_DX_data$MCI_any_PE
#NONPE_reverters=(sum(New_DX_data$sum.ATEF.NOPE==4, na.rm = TRUE) +sum(New_DX_data$sum.memory.NOPE==4, na.rm = TRUE)+sum(New_DX_data$sum.fluency.NOPE==4, na.rm = TRUE)) #314
#PE_reverters=(sum(New_DX_data$sum.ATEF.PE==4, na.rm = TRUE) +sum(New_DX_data$sum.memory.PE==4, na.rm = TRUE)+sum(New_DX_data$sum.fluency.PE==4, na.rm = TRUE)) #353
#Total_change_reverters=PE_reverters-NONPE_reverters #28









#counting the normals
sum(New_DX_data$MCI_any_PE==0, na.rm=TRUE) #599
sum(New_DX_data$MCI_any_NONPE==0, na.rm=TRUE) #621

#summary of Jak/Bondi MCI diagnoses
sum(New_DX_data$MCI_any_PE==1, na.rm=TRUE) #131/646
sum(New_DX_data$MCI_any_NONPE==1, na.rm=TRUE) #105/646
mcnemar.test(table(New_DX_data$MCI_any_PE, New_DX_data$MCI_any_NONPE))
  #McNemar's chi-squared = 24.038, df = 1, p-value = 9.443e-07. 10/17/19

sum(New_DX_data$imp_mem_1SD_PE==1, na.rm=TRUE) #106/738
sum(New_DX_data$imp_mem_1SD_NONPE==1, na.rm=TRUE) #87/738
mcnemar.test(table(New_DX_data$imp_mem_1SD_PE, New_DX_data$imp_mem_1SD_NONPE))
  #McNemar's chi-squared = 17.053, df = 1, p-value = 3.636e-05. 10/17/19


sum(New_DX_data$imp_atef_1SD_PE==1, na.rm=TRUE) #27/736
sum(New_DX_data$imp_atef_1SD_NONPE==1, na.rm=TRUE) #16/736
mcnemar.test(table(New_DX_data$imp_atef_1SD_PE, New_DX_data$imp_atef_1SD_NONPE))
    #McNemar's chi-squared = 9.0909, df = 1, p-value = 0.002569. 10/17/19

sum(New_DX_data$imp_flu_1SD_PE==1, na.rm=TRUE) #18/721
sum(New_DX_data$imp_flu_1SD_NONPE==1, na.rm=TRUE) #13/721
mcnemar.test(table(New_DX_data$imp_flu_1SD_PE, New_DX_data$imp_flu_1SD_NONPE))
    #McNemar's chi-squared = 3.2, df = 1, p-value = 0.07364. 10/17/19

#summary of peterson diagnoses 
sum(New_DX_data$MCI_any_PE_Pet==1, na.rm=TRUE) #270/682
sum(New_DX_data$MCI_any_NONPE_Pet==1, na.rm=TRUE) #231/682
mcnemar.test(table(New_DX_data$MCI_any_PE_Pet, New_DX_data$MCI_any_NONPE_Pet))
  #McNemar's chi-squared = 36.026, df = 1, p-value = 1.947e-09. 10/17/19

sum(New_DX_data$sum.memory.PE_PETERSON==1, na.rm=TRUE) #205/738
sum(New_DX_data$sum.memory.NOPE_PETERSON==1, na.rm=TRUE) #176/738
mcnemar.test(table(New_DX_data$sum.memory.PE_PETERSON, New_DX_data$sum.memory.NOPE_PETERSON))
  #McNemar's chi-squared = 27.034, df = 1, p-value = 1.999e-07 2/2//20

sum(New_DX_data$sum.ATEF.PE_PETERSON==1, na.rm=TRUE) #66/736
sum(New_DX_data$sum.ATEF.NOPE_PETERSON==1, na.rm=TRUE) #50/736
mcnemar.test(table(New_DX_data$sum.ATEF.PE_PETERSON, New_DX_data$sum.ATEF.NOPE_PETERSON))
  #McNemar's chi-squared = 14.062, df = 1, p-value = 0.0001768. 10/17/19

sum(New_DX_data$sum.fluency.PE_PETERSON==1, na.rm=TRUE) #59/721
sum(New_DX_data$sum.fluency.NOPE_PETERSON==1, na.rm=TRUE) #42/721
mcnemar.test(table(New_DX_data$sum.fluency.PE_PETERSON, New_DX_data$sum.fluency.NOPE_PETERSON))
  #McNemar's chi-squared = 15.059, df = 1, p-value = 0.0001042 2/2/20


}  #Creates merged DX datbases. Sum up all MCI DX with Jak bondi, run McNemar tests. 
###################################################################
# below analyses includes progresison to AD, biomarker positivity,
# levels of intial biomarkers. 
{
  
  library(dplyr)
  library(psych)
  
  #bring in AD diagnosis into PE vs NPE DX database.  
  Prog_data=subset(Subject_selection_V1, DX=='Dementia' & VISCODE!='bl' & VISCODE!='m12' & VISCODE!='m06', select = c('RID','Month_bl','DX', 'VISCODE'))
  Prog_data=(Prog_data %>% arrange(RID, Month_bl) %>% group_by(RID) %>% slice(1))
  #bring in biomarker data. INcludes biomarkers from basline
  bio=subset(Subject_selection_V1, Month_bl==0 & VISCODE=='bl', select = c('RID',"PTAU", "TAU", "ABETA", "PIB", "AV45",
                                                                           'Hippocampus_bl','APOE4', 'EXAMDATE'))
  #use JE script to create biomarker positivity. 
  #open JE script and run it "applyBiomarkerCutpoints"
  biocuts=applyCutpoints(bio) #biocuts includes the cutoff variables. see script for details 
  biocuts$EXAMDATE=NULL
  bio.2=merge(bio, biocuts, by = 'RID')
  #bring in bio data from MONTH 12, renamed as well. 
  bio.m12=subset(Subject_selection_V1,VISCODE=='m12', select = c('RID',"PTAU", "TAU", "ABETA", "PIB", "AV45",
                                                                 'Hippocampus_bl','APOE4', 'EXAMDATE'))
  biocuts=applyCutpoints(bio.m12) #biocuts includes the cutoff variables. see script for details 
  biocuts$EXAMDATE=NULL
  colnames(biocuts)=paste("m12", colnames(biocuts), sep = "_")
  colnames(biocuts)[1]='RID'
  colnames(bio.m12)=paste("m12", colnames(bio.m12), sep = "_")
  colnames(bio.m12)[1]='RID'
  bio.m12.2=merge(bio.m12, biocuts, by = 'RID')
  
  #merge together data
  Prog_data.2=merge(Prog_data, bio.2, all=TRUE)
  Prog_data.3=merge(Prog_data.2, bio.m12.2, all=TRUE)
  bio.dx.data=merge(New_DX_data, Prog_data.3, by='RID', all=TRUE)
  
  #remove people who are missing PE or NPE diagnoses Jak/bondi. 
  bio.dx.data=subset(bio.dx.data, bio.dx.data$MCI_any_PE!='NA')
  bio.dx.data=subset(bio.dx.data, bio.dx.data$MCI_any_NONPE!='NA')
  
  #creates grouping variable to seperate who is MCI by which dataset. Jak Bondi. 
  bio.dx.data$grouping=ifelse((bio.dx.data$MCI_any_NONPE==0 & bio.dx.data$MCI_any_PE==0),0,
                              ifelse((bio.dx.data$MCI_any_NONPE==0 & bio.dx.data$MCI_any_PE==1), 1,
                                     ifelse((bio.dx.data$MCI_any_NONPE==1 & bio.dx.data$MCI_any_PE==1),2,
                                            ifelse((bio.dx.data$MCI_any_NONPE==1 & bio.dx.data$MCI_any_PE==0),3,-5))))
  sum(bio.dx.data$grouping==0,na.rm=TRUE)#599, normals by both datasets 599
  sum(bio.dx.data$grouping==1,na.rm=TRUE)#22, MCI by PE and not NPE
  sum(bio.dx.data$grouping==2,na.rm=TRUE)#90, MCI by both datasets
  sum(bio.dx.data$grouping==3,na.rm=TRUE)# 0, sanity check. SHould be zero. 
  sum(bio.dx.data$grouping==(-5),na.rm=TRUE) #0, sanity check. 
  
  
  #### looks at converstion to AD based on PE or not PE. 
  library(psych)
  describe(bio.dx.data$Month_bl) #gives total months to ad DX for total sample n=89
  
  
  CN_NPE=subset(bio.dx.data, MCI_any_NONPE==0, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #n=617, number FU cognitively normal by NPE
  describe(CN_NPE$Month_bl)# n=59,  mean 66.2, median 65.11, SD 34.45 [17.67, 133.54] (n=59, convert to AD)
  
  CN_PE=subset(bio.dx.data, MCI_any_PE==0, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #591, number FU cognitively normal by PE
  describe(CN_PE$Month_bl)#n=56 mean 70.87, median 71.51, SD 33.76 [17.67, 133.54] (n=56, convert to AD)
  
  MCI_NPE=subset(bio.dx.data, MCI_any_NONPE==1, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #105, number FU MCI by NPE
  MCI_NPE$Month_bl=ifelse(MCI_NPE$Month_bl<12.5, NA, MCI_NPE$Month_bl) #removes anyone with dementia dx at <12 months
  describe(MCI_NPE$Month_bl) #
  
  MCI_PE=subset(bio.dx.data, MCI_any_PE==1, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #131, number FU MCI by PE
  MCI_PE$Month_bl=ifelse(MCI_PE$Month_bl<12.5, NA, MCI_PE$Month_bl) #removes anyone with dementia dx at <12 months
  describe(MCI_PE$Month_bl) #
  
  prog_dif=subset(bio.dx.data, grouping==1) #26, number of MCI dx by PE but NOT by NPE. 
  describe(prog_dif$Month_bl)#
  
  #### looks at converstion to AD based on PE or not PE. Peterson criteria
  ##--not as good as Jak/Bondi. Not as large of group differnces. 
  library(psych)
  CN_NPE=subset(bio.dx.data.PET, MCI_any_NONPE_Pet==0)
  describe(CN_NPE$Month_bl)
  
  CN_PE=subset(bio.dx.data.PET, MCI_any_PE_Pet==0) 
  describe(CN_PE$Month_bl)
  
  MCI_NPE=subset(bio.dx.data.PET, MCI_any_NONPE_Pet==1) 
  MCI_NPE$Month_bl=ifelse(MCI_NPE$Month_bl<12.5, NA, MCI_NPE$Month_bl) #removes anyone with dementia dx at <12 months
  describe(MCI_NPE$Month_bl) 
  
  MCI_PE=subset(bio.dx.data.PET, MCI_any_PE_Pet==1) 
  MCI_PE$Month_bl=ifelse(MCI_PE$Month_bl<12.5, NA, MCI_PE$Month_bl) 
  describe(MCI_PE$Month_bl)
  
  prog_dif=subset(bio.dx.data.PET, grouping==1) 
  describe(prog_dif$Month_bl)
  
  
  
  ###########################################################
  
  
  
  
  
  ##same as above but with subtle cognitive decline: this is being odd, have some we
  {bio.dx.data$subtleG=ifelse((bio.dx.data$Subtle_decline_noPE.1SD==0 & bio.dx.data$Subtle_decline_PE.1SD==0),0,
                              ifelse((bio.dx.data$Subtle_decline_noPE.1SD==0 & bio.dx.data$Subtle_decline_PE.1SD==1),1,
                                     ifelse((bio.dx.data$Subtle_decline_noPE.1SD==1 & bio.dx.data$Subtle_decline_PE.1SD==1),2,
                                            ifelse((bio.dx.data$Subtle_decline_noPE.1SD==1 & bio.dx.data$Subtle_decline_PE.1SD==0),10,-5))))
  
  sum(bio.dx.data$subtleG==0,na.rm=TRUE)#69 CN by both who progress to AD
  sum(bio.dx.data$subtleG==1,na.rm=TRUE)#5 MCI by only the PE who progress
  sum(bio.dx.data$subtleG==2,na.rm=TRUE)#9 agreement MCI
  sum(bio.dx.data$subtleG==10,na.rm=TRUE)#4 ???????????????????
  sum(bio.dx.data$subtleG==(-5),na.rm=TRUE)#0
  sum(is.na(bio.dx.data$subtleG))#1
  
  
  }# not sure why the subtle cog decline groups are acting odd. have some people DX as impaired based on non-pe, but not pe. 
  
  ########################################
  
  {    #total number of people with each biomarker. Need for percentages
    bio.dx.data.count=subset(bio.dx.data, MCI_any_PE==1|MCI_any_PE==0)
    
    {    
      bio.dx.data.count.tau.abeta=subset(bio.dx.data.count, TAU.ABETA.pos==1|TAU.ABETA.pos==0)
      describe(bio.dx.data.count.tau.abeta$TAU.ABETA.pos) #519 subjects
      ##PE counts
      Tau.abeta_CN.Bneg.PE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==0 & bio.dx.data.count.tau.abeta$MCI_any_PE==0))
      Tau.abeta_MCI.Bneg.PE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==0 & bio.dx.data.count.tau.abeta$MCI_any_PE==1))
      Tau.abeta_CN.Bpos.PE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==1 & bio.dx.data.count.tau.abeta$MCI_any_PE==0))
      Tau.abeta_MCI.Bpos.PE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==1 & bio.dx.data.count.tau.abeta$MCI_any_PE==1))
      ## NPE counts
      Tau.abeta_CN.Bneg.NPE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==0 & bio.dx.data.count.tau.abeta$MCI_any_NONPE==0))
      Tau.abeta_MCI.Bneg.NPE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==0 & bio.dx.data.count.tau.abeta$MCI_any_NONPE==1))
      Tau.abeta_CN.Bpos.NPE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==1 & bio.dx.data.count.tau.abeta$MCI_any_NONPE==0))
      Tau.abeta_MCI.Bpos.NPE=(sum(bio.dx.data.count.tau.abeta$TAU.ABETA.po==1 & bio.dx.data.count.tau.abeta$MCI_any_NONPE==1))
      #dif (NPE- PE)
      Tau.abeta_CN.Bneg.diff=Tau.abeta_CN.Bneg.NPE-Tau.abeta_CN.Bneg.PE
      Tau.abeta_MCI.Bneg.diff=Tau.abeta_MCI.Bneg.NPE-Tau.abeta_MCI.Bneg.PE
      Tau.abeta_CN.Bpos.diff=Tau.abeta_CN.Bpos.NPE-Tau.abeta_CN.Bpos.PE
      Tau.abeta_MCI.Bpos.diff=Tau.abeta_MCI.Bpos.NPE-Tau.abeta_MCI.Bpos.PE
      
    } ####Tau.ABETA
    {bio.dx.data.count.PTAU.ABETA=subset(bio.dx.data.count, PTAU.ABETA.pos==1|PTAU.ABETA.pos==0)
      describe(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos) #518 subjects
      ##PE counts
      PTAU.ABETA_CN.Bneg.PE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==0 & bio.dx.data.count.PTAU.ABETA$MCI_any_PE==0))
      PTAU.ABETA_MCI.Bneg.PE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==0 & bio.dx.data.count.PTAU.ABETA$MCI_any_PE==1))
      PTAU.ABETA_CN.Bpos.PE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==1 & bio.dx.data.count.PTAU.ABETA$MCI_any_PE==0))
      PTAU.ABETA_MCI.Bpos.PE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==1 & bio.dx.data.count.PTAU.ABETA$MCI_any_PE==1))
      ## NPE counts
      PTAU.ABETA_CN.Bneg.NPE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==0 & bio.dx.data.count.PTAU.ABETA$MCI_any_NONPE==0))
      PTAU.ABETA_MCI.Bneg.NPE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==0 & bio.dx.data.count.PTAU.ABETA$MCI_any_NONPE==1))
      PTAU.ABETA_CN.Bpos.NPE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==1 & bio.dx.data.count.PTAU.ABETA$MCI_any_NONPE==0))
      PTAU.ABETA_MCI.Bpos.NPE=(sum(bio.dx.data.count.PTAU.ABETA$PTAU.ABETA.pos==1 & bio.dx.data.count.PTAU.ABETA$MCI_any_NONPE==1))
      #dif (NPE- PE)
      PTAU.ABETA_CN.Bneg.diff=PTAU.ABETA_CN.Bneg.NPE-PTAU.ABETA_CN.Bneg.PE
      PTAU.ABETA_MCI.Bneg.diff=PTAU.ABETA_MCI.Bneg.NPE-PTAU.ABETA_MCI.Bneg.PE
      PTAU.ABETA_CN.Bpos.diff=PTAU.ABETA_CN.Bpos.NPE-PTAU.ABETA_CN.Bpos.PE
      PTAU.ABETA_MCI.Bpos.diff=PTAU.ABETA_MCI.Bpos.NPE-PTAU.ABETA_MCI.Bpos.PE
    } ####PTAU.ABETA
    {PTAU.bio.dx.data.count.ABETA=subset(bio.dx.data.count, ABETA.pos==1|ABETA.pos==0)
      describe(PTAU.bio.dx.data.count.ABETA$ABETA.pos) #514 subjects
      ##PE counts
      PTAU.ABETA_CN.Bneg.PE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==0 & PTAU.bio.dx.data.count.ABETA$MCI_any_PE==0))
      PTAU.ABETA_MCI.Bneg.PE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==0 & PTAU.bio.dx.data.count.ABETA$MCI_any_PE==1))
      PTAU.ABETA_CN.Bpos.PE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==1 & PTAU.bio.dx.data.count.ABETA$MCI_any_PE==0))
      PTAU.ABETA_MCI.Bpos.PE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==1 & PTAU.bio.dx.data.count.ABETA$MCI_any_PE==1))
      ## NPE counts
      PTAU.ABETA_CN.Bneg.NPE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==0 & PTAU.bio.dx.data.count.ABETA$MCI_any_NONPE==0))
      PTAU.ABETA_MCI.Bneg.NPE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==0 & PTAU.bio.dx.data.count.ABETA$MCI_any_NONPE==1))
      PTAU.ABETA_CN.Bpos.NPE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==1 & PTAU.bio.dx.data.count.ABETA$MCI_any_NONPE==0))
      PTAU.ABETA_MCI.Bpos.NPE=(sum(PTAU.bio.dx.data.count.ABETA$ABETA.pos==1 & PTAU.bio.dx.data.count.ABETA$MCI_any_NONPE==1))
      #dif (NPE- PE)
      PTAU.ABETA_CN.Bneg.diff=PTAU.ABETA_CN.Bneg.NPE-PTAU.ABETA_CN.Bneg.PE
      PTAU.ABETA_MCI.Bneg.diff=PTAU.ABETA_MCI.Bneg.NPE-PTAU.ABETA_MCI.Bneg.PE
      PTAU.ABETA_CN.Bpos.diff=PTAU.ABETA_CN.Bpos.NPE-PTAU.ABETA_CN.Bpos.PE
      PTAU.ABETA_MCI.Bpos.diff=PTAU.ABETA_MCI.Bpos.NPE-PTAU.ABETA_MCI.Bpos.PE} ####ABETA
    {bio.dx.data.count.TAU=subset(bio.dx.data.count, TAU.pos==1|TAU.pos==0)
      describe(bio.dx.data.count.TAU$TAU.pos) #512 subjects
      ##PE counts
      PTAU.TAU_CN.Bneg.PE=(sum(bio.dx.data.count.TAU$TAU.pos==0 & bio.dx.data.count.TAU$MCI_any_PE==0))
      PTAU.TAU_MCI.Bneg.PE=(sum(bio.dx.data.count.TAU$TAU.pos==0 & bio.dx.data.count.TAU$MCI_any_PE==1))
      PTAU.TAU_CN.Bpos.PE=(sum(bio.dx.data.count.TAU$TAU.pos==1 & bio.dx.data.count.TAU$MCI_any_PE==0))
      PTAU.TAU_MCI.Bpos.PE=(sum(bio.dx.data.count.TAU$TAU.pos==1 & bio.dx.data.count.TAU$MCI_any_PE==1))
      ## NPE counts
      PTAU.TAU_CN.Bneg.NPE=(sum(bio.dx.data.count.TAU$TAU.pos==0 & bio.dx.data.count.TAU$MCI_any_NONPE==0))
      PTAU.TAU_MCI.Bneg.NPE=(sum(bio.dx.data.count.TAU$TAU.pos==0 & bio.dx.data.count.TAU$MCI_any_NONPE==1))
      PTAU.TAU_CN.Bpos.NPE=(sum(bio.dx.data.count.TAU$TAU.pos==1 & bio.dx.data.count.TAU$MCI_any_NONPE==0))
      PTAU.TAU_MCI.Bpos.NPE=(sum(bio.dx.data.count.TAU$TAU.pos==1 & bio.dx.data.count.TAU$MCI_any_NONPE==1))
      #dif (NPE- PE)
      PTAU.TAU_CN.Bneg.diff=PTAU.TAU_CN.Bneg.NPE-PTAU.TAU_CN.Bneg.PE
      PTAU.TAU_MCI.Bneg.diff=PTAU.TAU_MCI.Bneg.NPE-PTAU.TAU_MCI.Bneg.PE
      PTAU.TAU_CN.Bpos.diff=PTAU.TAU_CN.Bpos.NPE-PTAU.TAU_CN.Bpos.PE
      PTAU.TAU_MCI.Bpos.diff=PTAU.TAU_MCI.Bpos.NPE-PTAU.TAU_MCI.Bpos.PE} ####Tau
    {bio.dx.data.count.PTAU.PTAU=subset(bio.dx.data.count, PTAU.pos==1|PTAU.pos==0)
      describe(bio.dx.data.count.PTAU.PTAU$PTAU.pos) #518 subjects
      ##PE counts
      PTAU.PTAU_CN.Bneg.PE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==0 & bio.dx.data.count.PTAU.PTAU$MCI_any_PE==0))
      PTAU.PTAU_MCI.Bneg.PE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==0 & bio.dx.data.count.PTAU.PTAU$MCI_any_PE==1))
      PTAU.PTAU_CN.Bpos.PE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==1 & bio.dx.data.count.PTAU.PTAU$MCI_any_PE==0))
      PTAU.PTAU_MCI.Bpos.PE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==1 & bio.dx.data.count.PTAU.PTAU$MCI_any_PE==1))
      ## NPE counts
      PTAU.PTAU_CN.Bneg.NPE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==0 & bio.dx.data.count.PTAU.PTAU$MCI_any_NONPE==0))
      PTAU.PTAU_MCI.Bneg.NPE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==0 & bio.dx.data.count.PTAU.PTAU$MCI_any_NONPE==1))
      PTAU.PTAU_CN.Bpos.NPE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==1 & bio.dx.data.count.PTAU.PTAU$MCI_any_NONPE==0))
      PTAU.PTAU_MCI.Bpos.NPE=(sum(bio.dx.data.count.PTAU.PTAU$PTAU.pos==1 & bio.dx.data.count.PTAU.PTAU$MCI_any_NONPE==1))
      #dif (NPE- PE)
      PTAU.PTAU_CN.Bneg.diff=PTAU.PTAU_CN.Bneg.NPE-PTAU.PTAU_CN.Bneg.PE
      PTAU.PTAU_MCI.Bneg.diff=PTAU.PTAU_MCI.Bneg.NPE-PTAU.PTAU_MCI.Bneg.PE
      PTAU.PTAU_CN.Bpos.diff=PTAU.PTAU_CN.Bpos.NPE-PTAU.PTAU_CN.Bpos.PE
      PTAU.PTAU_MCI.Bpos.diff=PTAU.PTAU_MCI.Bpos.NPE-PTAU.PTAU_MCI.Bpos.PE} ####Ptau
    {bio.dx.data.count.PTAU.AV45=subset(bio.dx.data.count, AV45.pos==1|AV45.pos==0)
      describe(bio.dx.data.count.PTAU.AV45$AV45.pos) #518 subjects
      ##PE counts
      PTAU.AV45_CN.Bneg.PE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==0 & bio.dx.data.count.PTAU.AV45$MCI_any_PE==0))
      PTAU.AV45_MCI.Bneg.PE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==0 & bio.dx.data.count.PTAU.AV45$MCI_any_PE==1))
      PTAU.AV45_CN.Bpos.PE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==1 & bio.dx.data.count.PTAU.AV45$MCI_any_PE==0))
      PTAU.AV45_MCI.Bpos.PE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==1 & bio.dx.data.count.PTAU.AV45$MCI_any_PE==1))
      ## NPE counts
      PTAU.AV45_CN.Bneg.NPE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==0 & bio.dx.data.count.PTAU.AV45$MCI_any_NONPE==0))
      PTAU.AV45_MCI.Bneg.NPE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==0 & bio.dx.data.count.PTAU.AV45$MCI_any_NONPE==1))
      PTAU.AV45_CN.Bpos.NPE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==1 & bio.dx.data.count.PTAU.AV45$MCI_any_NONPE==0))
      PTAU.AV45_MCI.Bpos.NPE=(sum(bio.dx.data.count.PTAU.AV45$AV45.pos==1 & bio.dx.data.count.PTAU.AV45$MCI_any_NONPE==1))
      #dif (NPE- PE)
      PTAU.AV45_CN.Bneg.diff=PTAU.AV45_CN.Bneg.NPE-PTAU.AV45_CN.Bneg.PE
      PTAU.AV45_MCI.Bneg.diff=PTAU.AV45_MCI.Bneg.NPE-PTAU.AV45_MCI.Bneg.PE
      PTAU.AV45_CN.Bpos.diff=PTAU.AV45_CN.Bpos.NPE-PTAU.AV45_CN.Bpos.PE
      PTAU.AV45_MCI.Bpos.diff=PTAU.AV45_MCI.Bpos.NPE-PTAU.AV45_MCI.Bpos.PE} ####Av45
  }#biomarker positivity counts 
  
  
  { 
    #databse set up, creates an MCI and a CN databse for PE and NonPE; 4 total databses. 
    bio.dx.data.count=subset(bio.dx.data, MCI_any_PE==1|MCI_any_PE==0)
    bio.dx.data.count$ABETA.adj=ifelse(bio.dx.data.count$ABETA==">1700",1700,bio.dx.data.count$ABETA) #cut out >1700 values per JE suggestion
    bio.dx.data.count$ABETA.adj=as.numeric(bio.dx.data.count$ABETA.adj)
    bio.dx.data.count.MCI=subset(bio.dx.data.count, MCI_any_PE==1)
    bio.dx.data.count.CN=subset(bio.dx.data.count, MCI_any_PE==0)
    bio.dx.data.count.MCI.NPE=subset(bio.dx.data.count, MCI_any_NONPE==1)
    bio.dx.data.count.CN.NPE=subset(bio.dx.data.count, MCI_any_NONPE==0)
    
    #sets of T tests for each biomarker. 
    t.test(log(bio.dx.data.count.MCI$TAU), log(bio.dx.data.count.CN$TAU))
    wilcox.test(log(bio.dx.data.count.MCI$TAU), log(bio.dx.data.count.CN$TAU),
                paired = FALSE, alternative = "two.sided")
    t.test(log(bio.dx.data.count.MCI.NPE$TAU), log(bio.dx.data.count.CN.NPE$TAU))
    wilcox.test(log(bio.dx.data.count.MCI.NPE$TAU), log(bio.dx.data.count.CN.NPE$TAU),
                paired = FALSE, alternative = "two.sided")
    
    #t.test(log(bio.dx.data.count.MCI$PTAU), log(bio.dx.data.count.CN$PTAU))
    t.test(log(bio.dx.data.count.MCI$PTAU), log(bio.dx.data.count.CN$PTAU))
    wilcox.test(log(bio.dx.data.count.MCI$PTAU), log(bio.dx.data.count.CN$PTAU),
                paired = FALSE, alternative = "two.sided")
    
    t.test(log(bio.dx.data.count.MCI.NPE$PTAU), log(bio.dx.data.count.CN.NPE$PTAU))
    wilcox.test(log(bio.dx.data.count.MCI.NPE$PTAU), log(bio.dx.data.count.CN.NPE$PTAU),
                paired = FALSE, alternative = "two.sided")
    
    
    t.test(log(bio.dx.data.count.MCI$ABETA.adj), log(bio.dx.data.count.CN$ABETA.adj))
    wilcox.test(log(bio.dx.data.count.MCI$ABETA.adj), log(bio.dx.data.count.CN$ABETA.adj),
                paired = FALSE, alternative = "two.sided")
    t.test(log(bio.dx.data.count.MCI.NPE$ABETA.adj), log(bio.dx.data.count.CN.NPE$ABETA.adj))
    wilcox.test((bio.dx.data.count.MCI.NPE$ABETA.adj), (bio.dx.data.count.CN.NPE$ABETA.adj),
                paired = FALSE, alternative = "two.sided")
    
    library(ANOVAreplication)
    variance=data.frame(y=bio.dx.data.count$TAU,g=bio.dx.data.count$MCI_any_PE)
    variance.PE=subset(variance, y!='NA')
    pooled.sd(variance.PE) #98.65661
    variance.2=data.frame(y=bio.dx.data.count$TAU,g=bio.dx.data.count$MCI_any_NONPE)
    variance.NONPE=subset(variance.2, y!='NA')
    pooled.sd(variance.NONPE) #98.63003
  }  #T test set up for biomarkers
  
  
  #mcnemar tests comparing differnce in DX and biomarker.
  #create cells by multiplying varibales 
  
  bio.dx.data$PtauPos_MCI_NPE=ifelse((bio.dx.data$MCI_any_NONPE==0 & bio.dx.data$PTAU.pos==0),0,
                                     ifelse((bio.dx.data$MCI_any_NONPE==1 & bio.dx.data$PTAU.pos==1),1,
                                            ifelse((bio.dx.data$MCI_any_NONPE==1 & bio.dx.data$PTAU.pos==0),2,
                                                   ifelse((bio.dx.data$MCI_any_NONPE==0 & bio.dx.data$PTAU.pos==1),3,-5))))
  
  bio.dx.data$PtauPos_MCI_PE=ifelse((bio.dx.data$MCI_any_PE==0 & bio.dx.data$PTAU.pos==0),0,
                                    ifelse((bio.dx.data$MCI_any_PE==1 & bio.dx.data$PTAU.pos==1),1,
                                           ifelse((bio.dx.data$MCI_any_PE==1 & bio.dx.data$PTAU.pos==0),2,
                                                  ifelse((bio.dx.data$MCI_any_PE==0 & bio.dx.data$PTAU.pos==1),3,-5))))
  
  sum(bio.dx.data$PtauPos_MCI_NPE==0, na.rm=TRUE) #264
  sum(bio.dx.data$PtauPos_MCI_PE==0, na.rm=TRUE)  #254
  #PE has 10 less who CN and Bio neg
  sum(bio.dx.data$PtauPos_MCI_NPE==1, na.rm=TRUE) #47
  sum(bio.dx.data$PtauPos_MCI_PE==1, na.rm=TRUE)  #61
  #PE has 14 more who are MCI and bio pos
  sum(bio.dx.data$PtauPos_MCI_NPE==2, na.rm=TRUE) #30
  sum(bio.dx.data$PtauPos_MCI_PE==2, na.rm=TRUE)  #40
  #PE has 10 less who are MCI and bio neg. 
  sum(bio.dx.data$PtauPos_MCI_NPE==3, na.rm=TRUE) #177
  sum(bio.dx.data$PtauPos_MCI_PE==3, na.rm=TRUE)  #163
  #PE has 14 less who are CN and Bio pos
  
  #correct mcnemar tests based on talk with JE
  #below is repeat code, from grouping above. 
  sum(bio.dx.data$grouping==0,na.rm=TRUE)#599, normals by both datasets
  sum(bio.dx.data$grouping==1,na.rm=TRUE)#22, MCI by PE and not NPE
  sum(bio.dx.data$grouping==2,na.rm=TRUE)#90, MCI by both datasets
  sum(bio.dx.data$grouping==3,na.rm=TRUE)# 0, sanity check. SHould be zero. 
  sum(bio.dx.data$grouping==(-5),na.rm=TRUE) #0, sanity check. 
  
  bio.dx.data$MCI_group_MCtest=ifelse(bio.dx.data$grouping==1,0,
                                      ifelse(bio.dx.data$grouping==2,1,NA))
  #MCI_group_MCtest: 0 = people who are MCI by PE only. 
  #                  1 = people who are MCI by both (so really = NPE)
  
  sum(bio.dx.data$MCI_group_MCtest==1, na.rm = TRUE) # 105
  
  temp=subset(bio.dx.data, MCI_group_MCtest==1)
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$PTAU.pos))
  #        0  1                 Neg   Pos   totals
  #     0 10 14       PE  MCI   10    14      24 (smaller than total MCI b/c some missing biomarkers) 
  #     1 30 47       NPE MCI   30    47      77 (smaller than total MCI b/c some missing biomarkers)
  #                                           101 of 131, because some people don't have biomarkers. 
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$TAU.pos))
  #McNemar's chi-squared = 16, df = 1, p-value = 6.334e-05
  #          0  1
  #       0 14 10   24 total
  #       1 39 38   77 total
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$PTAU.ABETA.pos))
  #McNemar's chi-squared = 11.256, df = 1, p-value = 0.0007937
  #     0  1
  #   0 14 10       24 total
  #   1 33 44       77 total
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$TAU.ABETA.pos))
  #McNemar's chi-squared = 11.256, df = 1, p-value = 0.0007937
  #      0  1
  #   0 14 10       24 total
  #   1 33 44       77 total
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$PTAU.pos))
  #McNemar's chi-squared = 5.1136, df = 1, p-value = 0.02374
  #      0  1
  #   0 10 14
  #   1 30 47
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$ABETA.pos))
  
  
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$AV45.pos))
  mcnemar.test(table(bio.dx.data$MCI_group_MCtest, bio.dx.data$PIB.pos))
  
}#Biomarker anlayses
##################################################################



###################################
#
#   Peterson DX info
#
{#----------------------------------
#summary of peterson diagnoses 
New_DX_data.PET=(subset(New_DX_data, MCI_any_PE_Pet==1 | MCI_any_PE_Pet==0))
New_DX_data.PET=(subset(New_DX_data.PET, MCI_any_NONPE_Pet==1 | MCI_any_NONPE_Pet==0))

sum(New_DX_data.PET$MCI_any_PE_Pet==1, na.rm=TRUE) #274/722
sum(New_DX_data.PET$MCI_any_PE_Pet==0, na.rm=TRUE) #438/726
  
sum(New_DX_data.PET$MCI_any_NONPE_Pet==1, na.rm=TRUE) #220/725
sum(New_DX_data.PET$MCI_any_NONPE_Pet==0, na.rm=TRUE) #492/725

mcnemar.test(table(New_DX_data.PET$MCI_any_PE_Pet, New_DX_data.PET$MCI_any_NONPE_Pet))
#McNemar's chi-squared = 36.026, df = 1, p-value = 1.947e-09. 10/17/19

sum(New_DX_data.PET$sum.memory.PE_PETERSON==1, na.rm=TRUE) #199/738
sum(New_DX_data.PET$sum.memory.NOPE_PETERSON==1, na.rm=TRUE) #176/738
mcnemar.test(table(New_DX_data.PET$sum.memory.PE_PETERSON, New_DX_data.PET$sum.memory.NOPE_PETERSON))
#McNemar's chi-squared = 21.043, df = 1, p-value = 4.49e-06. 10/17/19

sum(New_DX_data.PET$sum.ATEF.PE_PETERSON==1, na.rm=TRUE) #66/736
sum(New_DX_data.PET$sum.ATEF.NOPE_PETERSON==1, na.rm=TRUE) #50/736
mcnemar.test(table(New_DX_data.PET$sum.ATEF.PE_PETERSON, New_DX_data.PET$sum.ATEF.NOPE_PETERSON))
#McNemar's chi-squared = 14.062, df = 1, p-value = 0.0001768. 10/17/19

sum(New_DX_data.PET$sum.fluency.PE_PETERSON==1, na.rm=TRUE) #63/721
sum(New_DX_data.PET$sum.fluency.NOPE_PETERSON==1, na.rm=TRUE) #44/721
mcnemar.test(table(New_DX_data.PET$sum.fluency.PE_PETERSON, New_DX_data.PET$sum.fluency.NOPE_PETERSON))
#McNemar's chi-squared = 17.053, df = 1, p-value = 3.636e-05. 10/17/19

mcnemar.test(table(New_DX_data.PET$MCI_any_PE_Pet, New_DX_data.PET$MCI_any_NONPE_Pet))



###---biomarker analyses using peterson criteria instrad of Jak Bondi.---###




#creates grouping variable to seperate who is MCI by which dataset. Peterson. 
bio.dx.data.PET$grouping=ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==0 & bio.dx.data.PET$MCI_any_PE_Pet==0),0,
                                ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==0 & bio.dx.data.PET$MCI_any_PE_Pet==1), 1,
                                       ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==1 & bio.dx.data.PET$MCI_any_PE_Pet==1),2,
                                              ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==1 & bio.dx.data.PET$MCI_any_PE_Pet==0),3,-5))))
sum(bio.dx.data.PET$grouping==0,na.rm=TRUE)#456, normals by both datasets
sum(bio.dx.data.PET$grouping==1,na.rm=TRUE)#38, MCI by PE and not NPE
sum(bio.dx.data.PET$grouping==2,na.rm=TRUE)#228, MCI by both datasets
sum(bio.dx.data.PET$grouping==3,na.rm=TRUE)# 0, sanity check. SHould be zero. 
sum(bio.dx.data.PET$grouping==(-5),na.rm=TRUE) #0, sanity check. 


#biomarker positivity counts 
library(plyr)
CN_PE_Biomarker_data=ddply(.data= CN_PE,        #takes the dataset
                           .variables= "MCI_any_PE_Pet",  #just making sure everyone is CN. 
                           summarize,             #needed to create a new variable
                           count_TAU.ABETA.pos=sum(TAU.ABETA.pos, na.rm=TRUE),
                           count_PTAU.ABETA.pos=sum(PTAU.ABETA.pos, na.rm=TRUE),
                           count_PTAU.pos=sum(PTAU.pos, na.rm=TRUE),
                           count_ABETA.pos=sum(ABETA.pos, na.rm=TRUE),
                           count_TAU.pos=sum(TAU.pos, na.rm=TRUE),
                           count_AV45.pos=sum(AV45.pos, na.rm=TRUE),
                           count_PIB.pos=sum(PIB.pos, na.rm=TRUE)
)
  CN_NPE_Biomarker_data=ddply(.data= CN_NPE,        #takes the dataset
                            .variables= "MCI_any_NONPE_Pet",  #just making sure everyone is CN. 
                            summarize,             #needed to create a new variable
                            count_TAU.ABETA.pos=sum(TAU.ABETA.pos, na.rm=TRUE),
                            count_PTAU.ABETA.pos=sum(PTAU.ABETA.pos, na.rm=TRUE),
                            count_PTAU.pos=sum(PTAU.pos, na.rm=TRUE),
                            count_ABETA.pos=sum(ABETA.pos, na.rm=TRUE),
                            count_TAU.pos=sum(TAU.pos, na.rm=TRUE),
                            count_AV45.pos=sum(AV45.pos, na.rm=TRUE),
                            count_PIB.pos=sum(PIB.pos, na.rm=TRUE)
)
MCI_NPE_Biomarker_data=ddply(.data= MCI_NPE,        #takes the dataset
                             .variables= "MCI_any_NONPE_Pet",  #just making sure everyone is CN. 
                             summarize,             #needed to create a new variable
                             count_TAU.ABETA.pos=sum(TAU.ABETA.pos, na.rm=TRUE),
                             count_PTAU.ABETA.pos=sum(PTAU.ABETA.pos, na.rm=TRUE),
                             count_PTAU.pos=sum(PTAU.pos, na.rm=TRUE),
                             count_ABETA.pos=sum(ABETA.pos, na.rm=TRUE),
                             count_TAU.pos=sum(TAU.pos, na.rm=TRUE),
                             count_AV45.pos=sum(AV45.pos, na.rm=TRUE),
                             count_PIB.pos=sum(PIB.pos, na.rm=TRUE)
)
MCI_PE_Biomarker_data=ddply(.data= MCI_PE,        #takes the dataset
                            .variables= "MCI_any_PE_Pet",  #just making sure everyone is CN. 
                            summarize,             #needed to create a new variable
                            count_TAU.ABETA.pos=sum(TAU.ABETA.pos, na.rm=TRUE),
                            count_PTAU.ABETA.pos=sum(PTAU.ABETA.pos, na.rm=TRUE),
                            count_PTAU.pos=sum(PTAU.pos, na.rm=TRUE),
                            count_ABETA.pos=sum(ABETA.pos, na.rm=TRUE),
                            count_TAU.pos=sum(TAU.pos, na.rm=TRUE),
                            count_AV45.pos=sum(AV45.pos, na.rm=TRUE),
                            count_PIB.pos=sum(PIB.pos, na.rm=TRUE)
)
prog_dif_Biomarker_data=ddply(.data= prog_dif,        #takes the dataset
                              .variables= "grouping",  #just making sure everyone is CN. 
                              summarize,             #needed to create a new variable
                              count_TAU.ABETA.pos=sum(TAU.ABETA.pos, na.rm=TRUE),
                              count_PTAU.ABETA.pos=sum(PTAU.ABETA.pos, na.rm=TRUE),
                              count_PTAU.pos=sum(PTAU.pos, na.rm=TRUE),
                              count_ABETA.pos=sum(ABETA.pos, na.rm=TRUE),
                              count_TAU.pos=sum(TAU.pos, na.rm=TRUE),
                              count_AV45.pos=sum(AV45.pos, na.rm=TRUE),
                              count_PIB.pos=sum(PIB.pos, na.rm=TRUE)
                              
)
CN_NPE_Biomarker_data
CN_PE_Biomarker_data
MCI_NPE_Biomarker_data
MCI_PE_Biomarker_data
prog_dif_Biomarker_data

#mcnemar tests comparing differnce in DX and biomarker.
#create cells by multiplying varibales 

bio.dx.data.PET$PtauPos_MCI_NPE=ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==0 & bio.dx.data.PET$PTAU.pos==0),0,
                                       ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==1 & bio.dx.data.PET$PTAU.pos==1),1,
                                              ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==1 & bio.dx.data.PET$PTAU.pos==0),2,
                                                     ifelse((bio.dx.data.PET$MCI_any_NONPE_Pet==0 & bio.dx.data.PET$PTAU.pos==1),3,-5))))

bio.dx.data.PET$PtauPos_MCI_PE=ifelse((bio.dx.data.PET$MCI_any_PE_Pet==0 & bio.dx.data.PET$PTAU.pos==0),0,
                                      ifelse((bio.dx.data.PET$MCI_any_PE_Pet==1 & bio.dx.data.PET$PTAU.pos==1),1,
                                             ifelse((bio.dx.data.PET$MCI_any_PE_Pet==1 & bio.dx.data.PET$PTAU.pos==0),2,
                                                    ifelse((bio.dx.data.PET$MCI_any_PE_Pet==0 & bio.dx.data.PET$PTAU.pos==1),3,-5))))

test=bio.dx.data

test=subset(test, PtauPos_MCI_NPE==3)

mcnemar.test(table(bio.dx.data.PET$PtauPos_MCI_NPE, bio.dx.data.PET$PTAU.pos))


sum(bio.dx.data.PET$PtauPos_MCI_NPE==0, na.rm=TRUE) #223 CN and neg
sum(bio.dx.data.PET$PtauPos_MCI_PE==0, na.rm=TRUE)  #205

#
sum(bio.dx.data.PET$PtauPos_MCI_NPE==1, na.rm=TRUE) #100 MCI and pos
sum(bio.dx.data.PET$PtauPos_MCI_PE==1, na.rm=TRUE)  #111

temp=bio.dx.data.PET
temp$test=ifelse(PtauPos_MCI_NPE==1)


#
sum(bio.dx.data.PET$PtauPos_MCI_NPE==2, na.rm=TRUE) #71 MCI and neg
sum(bio.dx.data.PET$PtauPos_MCI_PE==2, na.rm=TRUE)  #89
#
sum(bio.dx.data.PET$PtauPos_MCI_NPE==3, na.rm=TRUE) #124 CN and pos
sum(bio.dx.data.PET$PtauPos_MCI_PE==3, na.rm=TRUE)  #113
#

sum(bio.dx.data.PET$grouping==0,na.rm=TRUE)#456
sum(bio.dx.data.PET$grouping==1,na.rm=TRUE)#38
sum(bio.dx.data.PET$grouping==2,na.rm=TRUE)#228
sum(bio.dx.data.PET$grouping==3,na.rm=TRUE)# 0, sanity check. SHould be zero. 
sum(bio.dx.data.PET$grouping==(-5),na.rm=TRUE) #0, sanity check. 

bio.dx.data.PET$MCI_group_MCtest=ifelse(bio.dx.data.PET$grouping==1,0,
                                        ifelse(bio.dx.data.PET$grouping==2,1,NA))
    #--> MCI_group_MCtest=0 when PE only DX MCI. NonPE calls these peopel CNs. 
    #--> MCI_group_MCtest=1 when PE and NonPE call person MCI, when agree.
sum(bio.dx.data.PET$MCI_group_MCtest==1, na.rm = TRUE) 

temp=subset(bio.dx.data.PET, MCI_group_MCtest==1)
mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$PTAU.pos))
#McNemar's chi-squared = 42.451, df = 1, p-value = 7.247e-11

mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$TAU.pos))
#McNemar's chi-squared = 70.438, df = 1, p-value < 2.2e-16

mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$PTAU.ABETA.pos))
#McNemar's chi-squared = 57.642, df = 1, p-value = 3.144e-14

mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$TAU.ABETA.pos))
#McNemar's chi-squared = 55.51, df = 1, p-value = 9.297e-14

mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$ABETA.pos))
#McNemar's chi-squared = 48.674, df = 1, p-value = 3.023e-12

mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$AV45.pos))
#McNemar's chi-squared = 43.214, df = 1, p-value = 4.906e-11

mcnemar.test(table(bio.dx.data.PET$MCI_group_MCtest, bio.dx.data.PET$PIB.pos))
#NA
}
