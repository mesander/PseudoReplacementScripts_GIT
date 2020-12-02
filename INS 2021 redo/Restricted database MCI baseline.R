#restricted data set with subs that have ALL XE for dx in each domain, no NAs. 


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

#restrict data to people who have all data for each domain
New_DX_data_restricted=subset(New_DX_data, sum.ATEF.PE!='NA' & sum.memory.PE!='NA' & sum.fluency.PE!='NA' &
                                sum.ATEF.NOPE!='NA' & sum.memory.NOPE!='NA' & sum.fluency.NOPE!='NA')

#totals in Who stays MCI by PE and NOPE (some people are impaired in multiple domains)

New_DX_data_restricted$reverter_count_memory=ifelse(New_DX_data_restricted$mem_impaired_LM_baseline==1 & New_DX_data_restricted$sum.memory.NOPE==2,1,0)
New_DX_data_restricted$reverter_count_lang=ifelse(New_DX_data_restricted$lang_impaired_baseline==1 & New_DX_data_restricted$sum.fluency.NOPE==2,1,0)
New_DX_data_restricted$reverter_count_atef=ifelse(New_DX_data_restricted$ATEF_impaired_new_baseline ==1 & New_DX_data_restricted$sum.ATEF.NOPE ==2,1, 0)

sum(New_DX_data_restricted$reverter_count_memory==1, na.rm = TRUE) #126 no PE
sum(New_DX_data_restricted$reverter_count_lang==1, na.rm = TRUE)   #21
sum(New_DX_data_restricted$reverter_count_atef==1, na.rm = TRUE)   #22


New_DX_data_restricted$NOPE_reverter_count_memory=ifelse(New_DX_data_restricted$mem_impaired_LM_baseline==1 & New_DX_data_restricted$sum.memory.PE==2,1,0)
New_DX_data_restricted$NOPE_reverter_count_lang=ifelse(New_DX_data_restricted$lang_impaired_baseline==1 & New_DX_data_restricted$sum.fluency.PE==2,1,0)
New_DX_data_restricted$NOPE_reverter_count_atef=ifelse(New_DX_data_restricted$ATEF_impaired_new_baseline ==1 & New_DX_data_restricted$sum.ATEF.PE ==2,1, 0)

sum(New_DX_data_restricted$NOPE_reverter_count_memory==1, na.rm = TRUE) #131  with PE
sum(New_DX_data_restricted$NOPE_reverter_count_lang==1, na.rm = TRUE)   #31
sum(New_DX_data_restricted$NOPE_reverter_count_atef==1, na.rm = TRUE)   #31


#total differnce in MCI prevealnce within each domain (some people are impaired in multiple domains)

#memory prevelence [NO PE]
sum(New_DX_data_restricted$sum.memory.NOPE==2, na.rm=TRUE) #129
sum(New_DX_data_restricted$sum.memory.NOPE!=2, na.rm=TRUE) #66
sum(is.na(New_DX_data_restricted$sum.memory.NOPE))         #0
#fluency [NO PE]
sum(New_DX_data_restricted$sum.fluency.NOPE==2, na.rm=TRUE) #42
sum(New_DX_data_restricted$sum.fluency.NOPE!=2, na.rm=TRUE) #153
sum(is.na(New_DX_data_restricted$sum.fluency.NOPE))         #0 
#atef [NO PE]
sum(New_DX_data_restricted$sum.ATEF.NOPE==2, na.rm=TRUE) #26
sum(New_DX_data_restricted$sum.ATEF.NOPE!=2, na.rm=TRUE) #169
sum(is.na(New_DX_data_restricted$sum.ATEF.NOPE))         #0 


#memory prevelence
sum(New_DX_data_restricted$sum.memory.PE==2, na.rm=TRUE) #134
sum(New_DX_data_restricted$sum.memory.PE!=2, na.rm=TRUE) #61
sum(is.na(New_DX_data_restricted$sum.memory.PE))         #4 NAs. 343 total. -> 339 completed tests
#fluency
sum(New_DX_data_restricted$sum.fluency.PE==2, na.rm=TRUE) #43
sum(New_DX_data_restricted$sum.fluency.PE!=2, na.rm=TRUE) #152
sum(is.na(New_DX_data_restricted$sum.fluency.PE))         #145 NAs. 202 rows -> 57 completed tests. 
#atef
sum(New_DX_data_restricted$sum.ATEF.PE==2, na.rm=TRUE) #46
sum(New_DX_data_restricted$sum.ATEF.PE!=2, na.rm=TRUE) #149
sum(is.na(New_DX_data_restricted$sum.ATEF.PE))         #9 NAs. 338 rows -> 329 completed tests. 



#below gets total prevelance of MCI.  Did not exclude people who had incomplete tests in other domains.People can be multiple domain MCI, and will be counted in each domain. 

New_DX_data_restricted$totalMCI.PE=ifelse(New_DX_data_restricted$sum.memory.PE==2 |New_DX_data_restricted$sum.fluency.PE==2 | New_DX_data_restricted$sum.ATEF.PE==2,1,0)
New_DX_data_restricted$totalMCI.NOPE=ifelse(New_DX_data_restricted$sum.memory.NOPE==2 |New_DX_data_restricted$sum.fluency.NOPE==2 | New_DX_data_restricted$sum.ATEF.NOPE==2,1,0)

sum(New_DX_data_restricted$totalMCI.NOPE==1)
sum(New_DX_data_restricted$totalMCI.PE==1)

New_DX_data_restricted_TOTALCOUNTS=subset(New_DX_data_restricted, New_DX_data_restricted$totalMCI.PE!='NA' &New_DX_data_restricted$totalMCI.NOPE!='NA') #total of 291 people with a posisble DX. 

sum(New_DX_data_restricted_TOTALCOUNTS$totalMCI.NOPE==1)
sum(New_DX_data_restricted_TOTALCOUNTS$totalMCI.PE==1)


#below looks at who progresses from single domain to multil [green section of word doc]


#creates database for who is single domain at baseline
New_DX_data_restricted$SingleDomainBasline=ifelse(New_DX_data_restricted$lang_impaired_baseline==1 & New_DX_data_restricted$mem_impaired_LM_baseline==0 & New_DX_data_restricted$ATEF_impaired_new_baseline==0, 1,
                                       ifelse(New_DX_data_restricted$lang_impaired_baseline==0 & New_DX_data_restricted$mem_impaired_LM_baseline==1 & New_DX_data_restricted$ATEF_impaired_new_baseline==0, 1,
                                              ifelse(New_DX_data_restricted$lang_impaired_baseline==0 & New_DX_data_restricted$mem_impaired_LM_baseline==0 & New_DX_data_restricted$ATEF_impaired_new_baseline==1, 1, 0)))

sum(New_DX_data_restricted$SingleDomainBasline==1, na.rm=TRUE)
sum(is.na(New_DX_data_restricted$SingleDomainBasline)) #0

New_DX_data_restricted$SingleDomainBasline_lang=ifelse(New_DX_data_restricted$lang_impaired_baseline==1 & New_DX_data_restricted$mem_impaired_LM_baseline==0 & New_DX_data_restricted$ATEF_impaired_new_baseline==0, 1,0)
New_DX_data_restricted$SingleDomainBasline_memory=ifelse(New_DX_data_restricted$lang_impaired_baseline==0 & New_DX_data_restricted$mem_impaired_LM_baseline==1 & New_DX_data_restricted$ATEF_impaired_new_baseline==0, 1,0)
New_DX_data_restricted$SingleDomainBasline_atef=ifelse(New_DX_data_restricted$lang_impaired_baseline==0 & New_DX_data_restricted$mem_impaired_LM_baseline==0 & New_DX_data_restricted$ATEF_impaired_new_baseline==1, 1, 0)

sum(New_DX_data_restricted$SingleDomainBasline_memory==1, na.rm=TRUE)
sum(New_DX_data_restricted$SingleDomainBasline_lang==1, na.rm = TRUE)
sum(New_DX_data_restricted$SingleDomainBasline_atef==1, na.rm = TRUE)

#create IDs for those who were single domain at baseline, stayed that domain, and progressed to another domain. 
#no practice effects
New_DX_data_restricted$mem_progression=ifelse(New_DX_data_restricted$SingleDomainBasline_memory==1 & New_DX_data_restricted$sum.memory.NOPE==2 & (New_DX_data_restricted$sum.fluency.NOPE==2 | New_DX_data_restricted$sum.ATEF.NOPE==2),1,0)
New_DX_data_restricted$atef_progression=ifelse(New_DX_data_restricted$SingleDomainBasline_atef==1 & New_DX_data_restricted$sum.ATEF.NOPE==2 & (New_DX_data_restricted$sum.fluency.NOPE==2 | New_DX_data_restricted$sum.memory.NOPE==2),1,0)
New_DX_data_restricted$lang_progression=ifelse(New_DX_data_restricted$SingleDomainBasline_lang==1 & New_DX_data_restricted$sum.fluency.NOPE==2 & (New_DX_data_restricted$sum.memory.NOPE==2 | New_DX_data_restricted$sum.ATEF.NOPE==2),1,0)
#practice effects
New_DX_data_restricted$mem_progression_PE=ifelse(New_DX_data_restricted$SingleDomainBasline_memory==1 & New_DX_data_restricted$sum.memory.PE==2 & (New_DX_data_restricted$sum.fluency.PE==2 | New_DX_data_restricted$sum.ATEF.PE==2),1,0)
New_DX_data_restricted$atef_progression_PE=ifelse(New_DX_data_restricted$SingleDomainBasline_atef==1 & New_DX_data_restricted$sum.ATEF.PE==2 & (New_DX_data_restricted$sum.fluency.PE==2 | New_DX_data_restricted$sum.memory.PE==2),1,0)
New_DX_data_restricted$lang_progression_PE=ifelse(New_DX_data_restricted$SingleDomainBasline_lang==1 & New_DX_data_restricted$sum.fluency.PE==2 & (New_DX_data_restricted$sum.memory.PE==2 | New_DX_data_restricted$sum.ATEF.PE==2),1,0)


#memnory
sum(New_DX_data_restricted$mem_progression==1, na.rm=TRUE)     #9
sum(New_DX_data_restricted$mem_progression_PE==1, na.rm=TRUE)  #15

#ATEF
sum(New_DX_data_restricted$atef_progression==1, na.rm=TRUE)    #3
sum(New_DX_data_restricted$atef_progression_PE==1, na.rm=TRUE) #3

#lang
sum(New_DX_data_restricted$lang_progression==1, na.rm=TRUE)    #1
sum(New_DX_data_restricted$lang_progression_PE==1, na.rm=TRUE) #2


#below IDs people who are CN on all tests. 
New_DX_data_restricted$CN_NONPE=ifelse(New_DX_data_restricted$sum.ATEF.NOPE==4 &  New_DX_data_restricted$sum.memory.NOPE==4 &  New_DX_data_restricted$sum.fluency.NOPE==4, 1,0)
New_DX_data_restricted$CN_PE=ifelse(New_DX_data_restricted$sum.ATEF.PE==4 &  New_DX_data_restricted$sum.memory.PE==4 &  New_DX_data_restricted$sum.fluency.PE==4, 1,0)

sum(New_DX_data_restricted$CN_NONPE==1, na.rm = TRUE)
sum(New_DX_data_restricted$CN_PE==1, na.rm = TRUE)

#below creates new DX varlaibes with 1= ANY MCI, and 0 = normal. Using 1SD. 
New_DX_data_restricted$MCI_any_NONPE=ifelse(New_DX_data_restricted$sum.ATEF.NOPE==2 |  New_DX_data_restricted$sum.memory.NOPE==2 |  New_DX_data_restricted$sum.fluency.NOPE==2, 1,0)
New_DX_data_restricted$MCI_any_PE=ifelse(New_DX_data_restricted$sum.ATEF.PE==2 |  New_DX_data_restricted$sum.memory.PE==2 |  New_DX_data_restricted$sum.fluency.PE==2, 1,0)

#peterson any MCI variable. 
New_DX_data_restricted$MCI_any_NONPE_Pet=ifelse(New_DX_data_restricted$sum.ATEF.NOPE_PETERSON==1 |  New_DX_data_restricted$sum.memory.NOPE_PETERSON==1 |  New_DX_data_restricted$sum.fluency.NOPE_PETERSON==1, 1,0)
New_DX_data_restricted$MCI_any_PE_Pet=ifelse(New_DX_data_restricted$sum.ATEF.PE_PETERSON==1 |  New_DX_data_restricted$sum.memory.PE_PETERSON==1 |  New_DX_data_restricted$sum.fluency.PE_PETERSON==1, 1,0)


#below sums up DXs.
sum(New_DX_data_restricted$MCI_any_NONPE==1, na.rm = TRUE) #152
sum(New_DX_data_restricted$MCI_any_PE==1, na.rm = TRUE) #164
sum(is.na(New_DX_data_restricted$MCI_any_NONPE)) #0
sum(is.na(New_DX_data_restricted$MCI_any_PE))  #0

#sum(New_DX_data_restricted$MCI_any==1, na.rm = TRUE) #96

sum(New_DX_data_restricted$sum.ATEF.NOPE==2, na.rm = TRUE)      #26
sum(New_DX_data_restricted$sum.ATEF.PE==2, na.rm = TRUE)        #46

sum(New_DX_data_restricted$sum.memory.NOPE==2, na.rm = TRUE)    #129
sum(New_DX_data_restricted$sum.memory.PE==2, na.rm = TRUE)      #134

sum(New_DX_data_restricted$sum.fluency.NOPE==2, na.rm = TRUE)   #42
sum(New_DX_data_restricted$sum.fluency.PE==2, na.rm = TRUE)     #43




#DEMOGRAPHICS
demobaseline=subset(DXs.2, select = c('RID','Age','PTGENDER','PTEDUCAT', 'MCI_any'))
demoFU=subset(DXs, Visit==12, select = c('RID','Age','Visit', 'MCI_any'))

New_DX_data_restricted_demo=merge(New_DX_data_restricted, demobaseline, by = 'RID', all.x = TRUE)

New_DX_data_restricted_demo_FU=merge(New_DX_data_restricted, demoFU, by = 'RID', all.x = TRUE)


#total differnce in impaired DX (some people are impaired in multiple domains)
NONPE=(sum(New_DX_data_restricted$sum.ATEF.NOPE==2, na.rm = TRUE) +sum(New_DX_data_restricted$sum.memory.NOPE==2, na.rm = TRUE)+sum(New_DX_data_restricted$sum.fluency.NOPE==2, na.rm = TRUE)) #305
PE=(sum(New_DX_data_restricted$sum.ATEF.PE==2, na.rm = TRUE) +sum(New_DX_data_restricted$sum.memory.PE==2, na.rm = TRUE)+sum(New_DX_data_restricted$sum.fluency.PE==2, na.rm = TRUE)) #353

New_DX_data_restricted$MCI_any_PE
#NONPE_reverters=(sum(New_DX_data_restricted$sum.ATEF.NOPE==4, na.rm = TRUE) +sum(New_DX_data_restricted$sum.memory.NOPE==4, na.rm = TRUE)+sum(New_DX_data_restricted$sum.fluency.NOPE==4, na.rm = TRUE)) #314
#PE_reverters=(sum(New_DX_data_restricted$sum.ATEF.PE==4, na.rm = TRUE) +sum(New_DX_data_restricted$sum.memory.PE==4, na.rm = TRUE)+sum(New_DX_data_restricted$sum.fluency.PE==4, na.rm = TRUE)) #353
#Total_change_reverters=PE_reverters-NONPE_reverters #28



#biomarker anlayses with restricted sample

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
  bio.dx.data=merge(New_DX_data_restricted, Prog_data.3, by='RID', all=TRUE)
  
  #remove people who are missing PE or NPE diagnoses Jak/bondi. 
  bio.dx.data=subset(bio.dx.data, bio.dx.data$MCI_any_PE!='NA')
  bio.dx.data=subset(bio.dx.data, bio.dx.data$MCI_any_NONPE!='NA')
  
  #creates grouping variable to seperate who is MCI by which dataset. Jak Bondi. 
  bio.dx.data$grouping=ifelse((bio.dx.data$MCI_any_NONPE==0 & bio.dx.data$MCI_any_PE==0),0,
                              ifelse((bio.dx.data$MCI_any_NONPE==0 & bio.dx.data$MCI_any_PE==1), 1,
                                     ifelse((bio.dx.data$MCI_any_NONPE==1 & bio.dx.data$MCI_any_PE==1),2,
                                            ifelse((bio.dx.data$MCI_any_NONPE==1 & bio.dx.data$MCI_any_PE==0),3,-5))))
  sum(bio.dx.data$grouping==0,na.rm=TRUE)#31, normals by both datasets 599
  sum(bio.dx.data$grouping==1,na.rm=TRUE)#12, MCI by PE and not NPE
  sum(bio.dx.data$grouping==2,na.rm=TRUE)#152, MCI by both datasets
  sum(bio.dx.data$grouping==3,na.rm=TRUE)#0, sanity check. SHould be zero. 
  sum(bio.dx.data$grouping==(-5),na.rm=TRUE) #0, sanity check. 
  
  
  #### looks at converstion to AD based on PE or not PE. 
  library(psych)
  describe(bio.dx.data$Month_bl) #gives total months to ad DX for total sample n=89
  
  
  CN_NPE=subset(bio.dx.data, MCI_any_NONPE==0, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #n=43, number FU cognitively normal by NPE
  describe(CN_NPE$Month_bl)# n=14,  mean 47.95, median 30.02, SD 31.45 [17.64, 107.34] (n=14, convert to AD)
  
  CN_PE=subset(bio.dx.data, MCI_any_PE==0, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #31, number FU cognitively normal by PE
  describe(CN_PE$Month_bl)#n=7 mean 66.18, median 71.64, SD 31.83 [17.67, 133.54] (n=7, convert to AD)
  
        # the above numbers say that adjust for PE means catch more people on the track to AD: with CN_NPE, 14 of the FU CNS (who were MCI at baseline) convert to AD, mean time =48 months. 
            # after adjust for PE, only 7 convert to AD (mean time =66 months) meaning adj for PE caught 7 of those on path to AD. 
  
  MCI_NPE=subset(bio.dx.data, MCI_any_NONPE==1, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #152, number FU MCI by NPE
  MCI_NPE$Month_bl=ifelse(MCI_NPE$Month_bl<12.5, NA, MCI_NPE$Month_bl) #removes anyone with dementia dx at <12 months
  describe(MCI_NPE$Month_bl) #
            #  vars  n      mean    sd       median trimmed      mad     min     max     range       skew kurtosis   se
            #  X1    96     35.88     21.44  25.31        32.03   11.25   17.54   137.67      120.13 1.97     4.94 2.19
  
  MCI_PE=subset(bio.dx.data, MCI_any_PE==1, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) #164, number FU MCI by PE
  MCI_PE$Month_bl=ifelse(MCI_PE$Month_bl<12.5, NA, MCI_PE$Month_bl) #removes anyone with dementia dx at <12 months
  describe(MCI_PE$Month_bl) #
        #vars   n     mean      sd      median  trimmed   mad     min       max     range     skew      kurtosis  se
        #X1     103   35.46     21.25   24.89   31.52     10.6    17.54     137.67  120.13    1.98      4.92      2.09
  
          #the above notes that adj for PE increases the number of people catch who convert later to AD. NOte that there isn't much difference between the NPE and PE months to AD
            # this makes sense cause we're saying that the people missed by NPE really are on path to AD. 
  
  prog_dif=subset(bio.dx.data, grouping==1) #7, number of MCI dx by PE but NOT by NPE who convert to AD 
  describe(prog_dif$Month_bl)#  mean: 29.71;  SD=18.83  median=23.67  [17.64-72.1]
  
  
  #mcnemar comparisons between who converts to AD and who doesn't
  
  ***********************************************************************
  NEED TO FIGURE THIS PART OUT NOT SURE WHY IT'S NOT WORKING YET'
  
  *********************************************************************
  
  bio.dx.data$AD_Progress_CNs=if_else((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_NONPE==0),0,
                                  ifelse((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_PE==0), 1, NA))
  
  bio.dx.data$AD_Progress_MCIs=if_else((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_NONPE==1),0,
                                      ifelse((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_PE==1), 1, NA))
  
  bio.dx.data$AD_Progress_PE=if_else((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_PE==0),0,
                                      ifelse((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_PE==1), 1, 99))
  
  bio.dx.data$AD_Progress_NPE=if_else((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_NONPE==1),0,
                                       ifelse((bio.dx.data$Month_bl>12.5 & bio.dx.data$MCI_any_NONPE==1), 1, 99))
  
  
  
        #AD_Progress: 0= progress to AD and is CN at FU   [NPE] 
        #             1= progress to AD and is MCI at FU  [NPE]
        #             2= progress to AD and is CN at FU   [PE]
        #             3= progress to AD and is CN at FU   [PE]
        #             -5 means did not progress to MCI
  
  
  mcnemar.test(table(bio.dx.data$AD_Progress_PE==0, bio.dx.data$AD_Progress_NPE==0))
  
  
  sum(bio.dx.data$grouping==0,na.rm=TRUE)#31, normals by both datasets 599
  
  subset(bio.dx.data, MCI_any_NONPE==0, select = c('RID','Month_bl','MCI_any_PE','MCI_any_NONPE','Visit','VISCODE','DX','grouping')) 
  
  #### looks at converstion to AD based on PE or not PE. Peterson criteria
{  ##--not as good as Jak/Bondi. Not as large of group differnces. 
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
  
  
  } 
  ###########################################################
  
  ########################################
  
  {    #total number of people with each biomarker. Need for percentages
    bio.dx.data.count=subset(bio.dx.data, MCI_any_PE==1|MCI_any_PE==0)
    
    {    
      bio.dx.data.count.tau.abeta=subset(bio.dx.data.count, TAU.ABETA.pos==1|TAU.ABETA.pos==0)
      describe(bio.dx.data.count.tau.abeta$TAU.ABETA.pos) #102 subjects
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
  
  sum(bio.dx.data$PtauPos_MCI_NPE==0, na.rm=TRUE) #12
  sum(bio.dx.data$PtauPos_MCI_PE==0, na.rm=TRUE)  #7
  #PE has 5 less who CN and Bio neg
  sum(bio.dx.data$PtauPos_MCI_NPE==1, na.rm=TRUE) #57
  sum(bio.dx.data$PtauPos_MCI_PE==1, na.rm=TRUE)  #61
  #PE has 4 more who are MCI and bio pos
  sum(bio.dx.data$PtauPos_MCI_NPE==2, na.rm=TRUE) #18
  sum(bio.dx.data$PtauPos_MCI_PE==2, na.rm=TRUE)  #23
  #PE has 5 less who are MCI and bio neg. 
  sum(bio.dx.data$PtauPos_MCI_NPE==3, na.rm=TRUE) #15
  sum(bio.dx.data$PtauPos_MCI_PE==3, na.rm=TRUE)  #11
  #PE has 4 less who are CN and Bio pos
  
  #correct mcnemar tests based on talk with JE
  #below is repeat code, from grouping above. 
  sum(bio.dx.data$grouping==0,na.rm=TRUE)#31, normals by both datasets
  sum(bio.dx.data$grouping==1,na.rm=TRUE)#12, MCI by PE and not NPE
  sum(bio.dx.data$grouping==2,na.rm=TRUE)#152, MCI by both datasets
  sum(bio.dx.data$grouping==3,na.rm=TRUE)#0, sanity check. SHould be zero. 
  sum(bio.dx.data$grouping==(-5),na.rm=TRUE) #0, sanity check. 
  
  bio.dx.data$MCI_group_MCtest=ifelse(bio.dx.data$grouping==1,0,
                                      ifelse(bio.dx.data$grouping==2,1,NA))
  #MCI_group_MCtest: 0 = people who are MCI by PE only. 
  #                  1 = people who are MCI by both (so really = NPE)
  
  sum(bio.dx.data$MCI_group_MCtest==1, na.rm = TRUE) # 152
  
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




