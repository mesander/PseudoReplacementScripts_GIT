# PE logistic power anlasyes for Lancet Neurology Paper 1
# begun 5/4/20

#goal is to look at hypothetical drug effect on converstion to MCI
#use adj and unadj rates of MCI
#placebo group gets MCI rates in our sample
#treatment group gets MCI rate from our sample reduced by multipling drug effect
    #.10. .20. .50
#ideal outcome is smaller sample sizes necessary when using PE adj
#complete for total sample, biomarker negative, biomarker positive rates of MCI



#libraries
require(psych)
require(powerMediation)
require(ggplot2)


#analyses
  #p1 = event rate (conversion to MCI) when placebo; = .09 vs .11
  #p2 = event rate (conversion to mCI) when treatment; = .09-(.09*.2) vs etc.
###################

    ###############################

#test


percentU=0.0859375
percentA=0.1035156
DE=.5
prob2U=(percentU -(percentU*DE))
prob2A=(percentA -(percentA*DE))

SSizeLogisticBin(p1 = .2, p2 = .1, B = .5, alpha = .05, power=0.9936607)
powerLogisticBin(n=1000, p1 =.15, p2 = .15, B = .5, alpha = .05)

SSizeLogisticBin(p1 = .1, p2 = .053, B = .5, alpha = .05, power=0.80)
powerLogisticBin(n=1000, p1 =.1, p2 = .053, B = .5, alpha = .05)



#test#################

#Biomarker negative
{    ef=.1
    n=1
    results.biomeg=data.frame('effectSize'=NA,'N-unadj'=NA,'N-adj'=NA,'N-diff'=NA,
                       'Power1000-unadj'=NA,'Power1000-adj'=NA, 'Power1000-dif'=NA)
    
#calculate base rates [updated after sex mixup, 7/13/20]
    #abeta: 514 subjects total in sample. 313 with neg abeta
    #NPE: ABETA NEGATIVE MCI: 26
    #=> 0.08306709    26/313
    #PE biomarker negative MCI:  35
    #=>  0.1118211
    
while (ef<.41) {
  percentU=0.08306709
  percentA=0.1118211
  DE=ef
  prob2U=(percentU -(percentU*DE))
  prob2A=(percentA -(percentA*DE))
  
  Unadj_bioPos=SSizeLogisticBin(p1 = percentU, p2 = prob2U, B = .5, alpha = .05, power=.80)
  adj_bioPos=SSizeLogisticBin(p1 = percentA, p2 = prob2A, B = .5, alpha = .05, power=.80)
  sample_dif=Unadj_bioPos-adj_bioPos 
  unadj_power=powerLogisticBin(n=1000, p1 = percentU, p2 = prob2U, B = .5, alpha = .05)
  adj_power=powerLogisticBin(n=1000, p1 = percentA, p2 = prob2A, B = .5, alpha = .05)
  powerdif=adj_power-unadj_power
  
  results.biomeg[n,1]=DE*100
  results.biomeg[n,2]=Unadj_bioPos
  results.biomeg[n,3]=adj_bioPos
  results.biomeg[n,4]=sample_dif
  results.biomeg[n,5]=unadj_power
  results.biomeg[n,6]=adj_power
  results.biomeg[n,7]=powerdif
  ef=ef+.0001
  n=n+1
  
}  

    
    ggplot(results.biomeg) +
      geom_point(aes(x=effectSize, y=N.unadj), col="steelblue", size=2) +
      geom_point(aes(x=effectSize, y=N.adj), col= 'red', size=2) +
      
      ylab('Estimated Sample Size')+
      xlab('Effect Size: % reduction in MCI with treatment')+
      scale_x_continuous(breaks=seq(10,40, 2))+
      scale_y_continuous(breaks=seq(0,34000, 2000))+
      theme(axis.text=element_text(size=12, face='bold'),
            axis.title=element_text(size=12,face="bold"))
    
  





} #Biomarker negative
    


{
  
  ef=.1
  n=1
  results.biopos=data.frame('effectSize'=NA,'N-unadj'=NA,'N-adj'=NA,'N-diff'=NA,
                            'Power1000-unadj'=NA,'Power1000-adj'=NA, 'Power1000-dif'=NA)
  while (ef<.41) {
    percentU=0.2211055
    percentA=0.2663317
    DE=ef
    prob2U=(percentU -(percentU*DE))
    prob2A=(percentA -(percentA*DE))
    
    Unadj_bioPos=SSizeLogisticBin(p1 = percentU, p2 = prob2U, B = .5, alpha = .05, power=.80)
    adj_bioPos=SSizeLogisticBin(p1 = percentA, p2 = prob2A, B = .5, alpha = .05, power=.80)
    sample_dif=Unadj_bioPos-adj_bioPos 
    unadj_power=powerLogisticBin(n=1000, p1 = percentU, p2 = prob2U, B = .5, alpha = .05)
    adj_power=powerLogisticBin(n=1000, p1 = percentA, p2 = prob2A, B = .5, alpha = .05)
    powerdif=adj_power-unadj_power
    
    results.biopos[n,1]=DE*100
    results.biopos[n,2]=Unadj_bioPos
    results.biopos[n,3]=adj_bioPos
    results.biopos[n,4]=sample_dif
    results.biopos[n,5]=unadj_power
    results.biopos[n,6]=adj_power
    results.biopos[n,7]=powerdif
    ef=ef+.0001
    n=n+1
    
  }  
  
  
  results.biopos$percent_change=(results.biopos$N.diff/results.biopos$N.unadj)*100
  results.biopos$percent_pwr_change=(results.biopos$Power1000.dif/results.biopos$Power1000.unadj)*100
  results.biopos$PET_noPE=round((results.biopos$N.unadj*3.390778533635676))
  results.biopos$PET_PE=round((results.biopos$N.adj*3.390778533635676))
  results.biopos$PET_diff= results.biopos$PET_noPE - results.biopos$PET_PE
  results.biopos$screen_noPE=round((results.biopos$N.unadj*5.111866969009826))
  results.biopos$screen_PE=round((results.biopos$N.adj*5.111866969009826))
  results.biopos$screen_diff= results.biopos$screen_noPE - results.biopos$screen_PE
  results.biopos$PET_1000s_save= results.biopos$PET_diff*5
  
      #sample size differnce graph
  ggplot(results.biopos) +
    geom_point(aes(x=effectSize, y=N.unadj), col="steelblue", size=3) +
    geom_point(aes(x=effectSize, y=N.adj), col= 'red', size=3) +

    ylab('Estimated Sample Size')+
    xlab('Effect Size: % reduction in MCI with treatment')+
    scale_x_continuous(breaks=seq(28,30, 1))+
    scale_y_continuous(breaks=seq(700,1400, 100))+
    xlim(27,32)+
    ylim(700,1400)
  results.biopos$effectSize
    
    #power difference graphs
  ggplot(results.biopos) +
    geom_point(aes(x=effectSize, y=Power1000.unadj), col="steelblue",) +
    geom_point(aes(x=effectSize, y=Power1000.adj), col= 'red') +
    ylab('Power with 1000 subjects')+
    geom_hline(yintercept = .8, color='black')  +
    scale_x_continuous(breaks=seq(20,50, 2.5))
  
  ggplot(results.biopos) +
    geom_point(aes(x=N.diff, y=Power1000.dif), col="steelblue",) +
    geom_point(aes(x=N.diff, y=Power1000.dif), col= 'red') +
    ylab('Power with 1000 subjects')+
    scale_x_continuous(breaks=seq(0,500,25))
    
  
    #A4 specific graphs. Multiplication numbers come from the Sperling pape and ratios betwen final sub number and recruitment
  ggplot(results.biopos) +
    geom_point(aes(x=effectSize, y=PET_noPE), col="steelblue", size=3) +
    geom_point(aes(x=effectSize, y=PET_PE), col= 'red', size=3) +
    geom_point(aes(x=effectSize, y=PET_diff), col="black", size=3) +
    
    
    ylab('Estimated Sample Size for PET Screening')+
    xlab('Effect Size: % reduction in MCI with treatment')+
    scale_x_continuous(breaks=seq(20,50, 5))+
    scale_y_continuous(breaks=seq(000,8000, 800))
  
  
  ggplot(results.biopos) +
    geom_point(aes(x=effectSize, y=screen_noPE), col="steelblue", size=3) +
    geom_point(aes(x=effectSize, y=screen_PE), col= 'red', size=3) +
    geom_point(aes(x=effectSize, y=screen_diff), col="black", size=3) +
    
    ylab('Estimated Sample Size for Initial Screening')+
    xlab('Effect Size: % reduction in MCI with treatment')+
    scale_x_continuous(breaks=seq(20,50, 5))+
    xlim(20,50)+
    scale_y_continuous(breaks=seq(0,13000, 1000)) +
    ylim(0,13000)

  #full A4 graph
  ggplot(results.biopos) +
    geom_point(aes(x=effectSize, y=PET_diff), col="green", size=3) +
    geom_point(aes(x=effectSize, y=screen_diff), col="orange", size=3) +
    geom_point(aes(x=effectSize, y=N.diff), col="black", size=3) +
    
    
    ylab('Estimated Reduction in Sample Size')+
    xlab('Effect Size: % reduction in MCI with treatment') +
    
    scale_x_continuous(breaks=seq(10,40, 2)) +
    scale_y_continuous(breaks=seq(0,12000, 1000))+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"))
  
  
  
  
  
  
}  #Biomarker positive [updated after sex mixup, 7/13/20]

ggplot(results.all) +
  geom_point(aes(x=effectSize, y=N.unadj), col="steelblue", size=2) +
  geom_point(aes(x=effectSize, y=N.adj), col= 'red', size=2) +
  
  ylab('Estimated Sample Size')+
  xlab('Effect Size: % reduction in MCI with treatment')+
  scale_x_continuous(breaks=seq(10,40, 2))+
  scale_y_continuous(breaks=seq(0,34000, 2000))+
  theme(axis.text=element_text(size=12, face='bold'),
        axis.title=element_text(size=12,face="bold"))



    ###############################

# All subjects, regardless of biomarker status
{ef=.1
n=1
results.all=data.frame('effectSize'=NA,'N-unadj'=NA,'N-adj'=NA,'N-diff'=NA,
                          'Power1000-unadj'=NA,'Power1000-adj'=NA, 'Power1000-dif'=NA)
while (ef<.41) {
  percentU=0.1246537396121884
  percentA=0.1565096952908587
  DE=ef
  prob2U=(percentU -(percentU*DE))
  prob2A=(percentA -(percentA*DE))
  
  Unadj_bioPos=SSizeLogisticBin(p1 = percentU, p2 = prob2U, B = .5, alpha = .05, power=.80)
  adj_bioPos=SSizeLogisticBin(p1 = percentA, p2 = prob2A, B = .5, alpha = .05, power=.80)
  sample_dif=Unadj_bioPos-adj_bioPos 
  unadj_power=powerLogisticBin(n=1000, p1 = percentU, p2 = prob2U, B = .5, alpha = .05)
  adj_power=powerLogisticBin(n=1000, p1 = percentA, p2 = prob2A, B = .5, alpha = .05)
  powerdif=adj_power-unadj_power
  
  
  
  results.all$percent_change=(results.all$N.diff/results.all$N.unadj)*100
  results.all$percent_pwr_change=(results.all$Power1000.dif/results.all$Power1000.unadj)*100
  results.all$PET_noPE=round((results.all$N.unadj*3.390778533635676))
  results.all$PET_PE=round((results.all$N.adj*3.390778533635676))
  results.all$PET_diff= results.all$PET_noPE - results.all$PET_PE
  results.all$screen_noPE=round((results.all$N.unadj*5.111866969009826))
  results.all$screen_PE=round((results.all$N.adj*5.111866969009826))
  results.all$screen_diff= results.all$screen_noPE - results.all$screen_PE
  results.all$PET_1000s_save= results.all$PET_diff*5
  

  
  results.all[n,1]=DE*100
  results.all[n,2]=Unadj_bioPos
  results.all[n,3]=adj_bioPos
  results.all[n,4]=sample_dif
  results.all[n,5]=unadj_power
  results.all[n,6]=adj_power
  results.all[n,7]=powerdif
  ef=ef+.0001
  n=n+1
  

  
}  

#sample size differnce graph
ggplot(results.all) +
  geom_point(aes(x=effectSize, y=N.unadj), col="steelblue", size=3) +
  geom_point(aes(x=effectSize, y=N.adj), col= 'red', size=3) +
  
  ylab('Estimated Sample Size')+
  xlab('Effect Size: % reduction in MCI with treatment')+
  scale_x_continuous(breaks=seq(28,30, 1))+
  scale_y_continuous(breaks=seq(700,1400, 100))+
  xlim(27,32)+
  ylim(700,1400)

results.all$effectSize
ggplot(results.all) +
  geom_point(aes(x=effectSize, y=N.unadj), col="steelblue", size=2, ) +
  geom_point(aes(x=effectSize, y=N.adj), col= 'red', size=2) +
  geom_point(aes(x=effectSize, y=N.diff), col= 'black', size=2) +
  
  ylab('Estimated Sample Size')+
  xlab('Effect Size: % reduction in MCI with treatment')+
  scale_x_continuous(breaks=seq(10,40, 2))+
  scale_y_continuous(breaks=seq(0,18000, 1500))+
  theme(axis.text=element_text(size=12, face='bold'),
        axis.title=element_text(size=12,face="bold"))


    
} #all subjects, regardless of biomarker status


ggplot(results.all) +
  geom_point(aes(x=effectSize, y=Power1000.unadj), col="steelblue",) +
  geom_point(aes(x=effectSize, y=Power1000.adj), col= 'red') +
  geom_hline(yintercept = .8, color='black')    








#####################
percentU=.238
percentA=.282
DE=.3
prob2U=(percentU -(percentU*DE))
prob2A=(percentA -(percentA*DE))

Unadj_bioPos=SSizeLogisticBin(p1 = percentU, p2 = prob2U, B = .5, alpha = .05, power=.80)
adj_bioPos=SSizeLogisticBin(p1 = percentA, p2 = prob2A, B = .5, alpha = .05, power=.80)
sample_dif=Unadj_bioPos-adj_bioPos 
unadj_power=powerLogisticBin(n=1000, p1 = percentU, p2 = prob2U, B = .5, alpha = .05)
adj_power=powerLogisticBin(n=1000, p1 = percentA, p2 = prob2A, B = .5, alpha = .05)
powerdif=adj_power-unadj_power











##########################
    
    #Biomarker positive only [old]
    percentU=.092
    percentA=.109
    DE=.5
    prob2U=(percentU -(percentU*DE))
    prob2A=(percentA -(percentA*DE))
    
    Unadj_bioPos=SSizeLogisticBin(p1 = percentU, p2 = prob2U, B = .5, alpha = .05, power=.80)
    adj_bioPos=SSizeLogisticBin(p1 = percentA, p2 = prob2A, B = .5, alpha = .05, power=.80)
    Unadj_bioPos 
    adj_bioPos 
    Unadj_bioPos-adj_bioPos 
    
    unadj_power=powerLogisticBin(n=1000, p1 = percentU, p2 = prob2U, B = .5, alpha = .05)
    adj_power=powerLogisticBin(n=1000, p1 = percentA, p2 = prob2A, B = .5, alpha = .05)
    unadj_power 
    adj_power   
    adj_power-unadj_power 
    
    
    # drug effect = .1
    #Unadj_bioPos   = 29584
    # adj_bioPos    = 24528
    # Unadj_bioPos-adj_bioPos =5056
    # unadj_power   = 0.07421618
    # adj_power     =0.08157982
    # adj_power-unadj_power   =0.007363642
    
    
    # drug effect = .2
    #Unadj_bioPos   = 7041
    # adj_bioPos    = 5743
    # Unadj_bioPos-adj_bioPos =1198
    # unadj_power   =0.1827941
    # adj_power     =0.2113659
    # adj_power-unadj_power   =0.02857174
    
    #drug effect = .5
    # Unadj_bioPos  =951
    # adj_bioPos    =792
    # Unadj_bioPos-adj_bioPos = 159
    # unadj_power   =0.8194975
    # adj_power     =0.8831924
    # adj_power-unadj_power   =0.06369487
    