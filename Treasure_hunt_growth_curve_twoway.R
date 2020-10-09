#=============================================================================================#
# R script for Treasure Hunt game (Adam and Dorothy) - 
#
# 09/10/2020
# based on Paul Thompson - 07/10-2020
#=============================================================================================#
# This is an implementation of a longitudinal model using generalized linear mixed effect model 
# (Binomial family - proportion as dependent var, but simulated as binary response to individual items
# then converted to proportion correct by session).
# 
# The  model has three random effects: [Paul please check I have this right!!]
# R1) random intercepts of individuals - this assumes that all subjects start at slightly different levels, but the variance will be very small as we would expect little difference at the start.
# R2) random slopes of individuals - learning rate varies between individuals
# R3) random intercepts for items

# We first considered power to detect different learning rates by Group (DLD or Typical) and 
# Condition (Repeated or Novel); in effect this is a 3 way interaction: Group x Condition x Session.
# Power for this was very low for the kinds of sample size/item set that was plausible.

# This led us to adopt instead a staged approach. 
# We run a series of pre-planned two-way analyses, correcting the alpha level to account for the number of tests

# Analysis A. Collapse across Condition, to test for effect of Group, Session and Group x Session.
# We expect a strong effect of Session. This would confirm that learning does occur in the study.
# Because Groups are initially selected for similar levels of comprehension, we expect,
# like Hsu and Bishop, that the Group term will be nonsignificant. 
# We do not have strong predictions about Group x Session.

# Analysis B. Our principal question is whether learning rate is affected by Condition in the DLD group.
# This is tested in an analysis that excludes the TD group, and focuses just on Session x Condition.

# Analysis C. We do not predict an effect of condition on learning in the TD group. 
# This is tested in an analysis parallel to Analysis B, that excludes the DLD group.

# We thus have a total of 5 comparisons. To maintain overall alpha at .05, we use alpha of .01 
# for each comparison.





# 





# Use Dorothy's simulation of the data to make as close as possible. Then fit GLMM approach.

#==============================================================================#
# Load required R packages
library(lme4)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggeffects)
#==============================================================================#


#==============================================================================#

data_sims <- function(nItem, nsub, ncond, nsess, ngroup,allcondp,randsubs,randitems)
{
  #Now Vectorised!
  # nItem = Items per session per condition
  # nsub = subjects per group
  # ncond  = 2 conditions (repeated and non)
  # nsess  = 4 sessions
  # ngroup = 2 groups
  # randsubs = term that sets range for random subjects effect
  # randitems = term that sets range for rand items effect
  
  
  #We increment the probability of correct for each item of each type.
  #Rate of increase can vary by item type and group
  
  groupcode <- c('D','T') #group-1 is DLD ; group1 is TD
  condcode <-c(0,1) #cond0 is novel ; cond1 is repeated
  simdat<-expand.grid(1:nsub,groupcode,1:nsess,1:nItem,condcode)
  colnames(simdat) <- c('Subject','Group','Session','Item','Condition')
  w<-which(simdat$Group=='T')
  simdat$Subject[w] <- simdat$Subject[w]+nsub #codes must be different for subs in 2 groups, so add offset
  
  simdat$allorder<-simdat$Item + (simdat$Session-1)*nItem #order rather than item ID determines avg learning
  w<-which(simdat$Condition==1)
  simdat$Item[w] <- simdat$Item[w]+nItem #codes must be different for items in 2 conditions, so add offset - this is item identity, for random effects. We *could* make this more constant for the Repeated items, but because they do vary in distractors etc, better to make distinct for each one
  simdat$GroupCond <- simdat$Condition+1 #we need to set numeric codes for each combination in interaction in order to use vectorised formula. For 2 x 2, codes are 1,2,3,4
  w<-which(simdat$Group=='T')
  simdat$GroupCond[w]<-simdat$GroupCond[w]+2 #add 2 for group2, so get values 3-4
  
  simdat$Correct <- 1 #default response is correct
  myrands <- runif(nrow(simdat)) #uniform random numbers 0-1 for each row
  subeff<-runif((nsub*2),-randsubs,randsubs) #one effect per subject - range specified by randsubs; e.g. if randsubs is .1, then each subject has offset ranging from -.1 to .1
  itemeff<-runif(nItem*2,-randitems,randitems) #one effect per item - range specified by randitems
  simdat$allcondrow<-simdat$allorder+(simdat$Session-1)*nItem #serial order of this stimulus across all sessions
  
  #We now use predetermined allcondp table to lookup cutoff probability for each row's GroupCond combination.  The allcondp table can be created to give nonlinear and range-restricted values
  for(j in 1:nrow(simdat)){ #can't work out how to vectorise this!
    simdat$criticalp[j]<-allcondp[simdat$allorder[j],simdat$GroupCond[j]]
  }
  simdat$criticalp <- simdat$criticalp+subeff[simdat$Subject]+itemeff[simdat$Item]
  #previous line adds noise to the criticalp from subject and item effects
  simdat$Correct[simdat$criticalp<myrands]<-0
  return(simdat)
}
#==============================================================================#


makeallcondp<-function(nItem,nsess){
  basep <- .6 #starting avg probability correct for all items/subjects
  #group 1 is DLD, group 2= TD
  #myN is novel, myR is repeated
  finNgroupD <- .79 #item final p correct for each item in N condition/group1
  finNgroupT <- .85 #item final p correct for each item in N condition/group2
  finRgroupD <- .91 #item final p correct for each item in R condition/group1
  finRgroupT <- .85 #item final p correct for each item in R condition/group2
  #We use these values to create probability correct matrix
  #Updated version, use an exponential function for learning.
  #This was derived by trial and error:see plots for how this looks in practice
  startseq<-seq(0,.7,length.out=nItem*nsess) #range selected to give plausible range
  de <-1.49-dexp(startseq) #this function goes from 60% correct at start to 99% by final item
  basex <-startseq[max(which(de<basep))] #this is value of startseq corresponding to basep
  baseDN <- startseq[max(which(de<finNgroupD))]
  baseTN <- startseq[max(which(de<finNgroupT))]
  baseDR <- startseq[max(which(de<finRgroupD))]
  baseTR <- startseq[max(which(de<finRgroupT))]
  DN_p <- 1.49-dexp(seq(basex,baseDN,length.out=nItem*nsess))
  TN_p <- 1.49-dexp(seq(basex,baseTN,length.out=nItem*nsess))
  DR_p <- 1.49-dexp(seq(basex,baseDR,length.out=nItem*nsess))
  TR_p <- 1.49-dexp(seq(basex,baseTR,length.out=nItem*nsess))
  #item by item probability correct by item type N or R, and by group (1 or 2)
  allcondp <- data.frame(cbind(DN_p,DR_p,TN_p,TR_p))
  return(allcondp)
}
#==============================================================================#
# Use simulated data and fit model to first simulated data set.
nItem <- 20
nsub <- 30 # 
ncond <- 2
nsess <- 4
ngroup <- 2
randsubs <- .1
randitems <-.01
nsim <- 100
allcondp <- makeallcondp(nItem,nsess)

expdat <- data_sims(nItem, nsub, ncond, nsess, ngroup,allcondp,randsubs,randitems)

#now plot means
expdat2 <- aggregate(expdat$Correct,by=list(expdat$Session,expdat$Subject,expdat$Group,expdat$Condition,expdat$GroupCond),FUN=mean)
colnames(expdat2)<-c('Session','Subject','Group','Condition','GroupCond','p.Correct')
expdat2$GroupCond<-as.factor(expdat2$GroupCond)
levels(expdat2$GroupCond)<-c('DLD_novel','DLD-rep','TYP_novel','TYP_rep')
mymeans<-aggregate(expdat2$p.Correct,by=list(expdat2$Session,expdat2$GroupCond),FUN=mean)
colnames(mymeans)<-c('Session','GroupCond','p.Correct')
mymeans %>%
  ggplot( aes(x=Session, y=p.Correct, group=GroupCond, color=GroupCond)) +
  ggtitle('Simulated data')+
  geom_line()


fit1 <- glmer(p.Correct ~ 1+Condition*Session*Group  + (0 + Session | Subject), family = binomial, data = expdat2,weights=rep(nItem,nrow(expdat2)))


#=========================================================================#
# Plot the simulated data and the predicted model fit to compare to Dorothy's proposed idea of what the data look like.
dat<-ggpredict(fit1,c("Session","Group","Condition"),type="re") #Extract predicted values for the proportions
colnames(dat)<-c("Session", "p.Correct","std.error", "conf.low", "conf.high",  "Group", "Condition")

ggplot(dat,aes(x=Session,y=p.Correct,colour=Group,linetype=Condition))+geom_line()+theme_bw()

#====================================================================================================================#
# Power calculation! 
# 
# This is a completely difference concept as the power is to detect a difference in whether the slopes change 
# according to group:Condition or just no random slope.
#====================================================================================================================# 


# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4579019/ #Supplementary info 1.

fitsim2 <- function(i,nItem,nsub) {
  
  expdat <- data_sims(nItem, nsub, ncond, nsess, ngroup,allcondp,randsubs,randitems)
  
  
  #now aggregate means
  expdat2 <- aggregate(expdat$Correct,by=list(expdat$Session,expdat$Subject,expdat$Group,expdat$Condition,expdat$GroupCond),FUN=mean)
  colnames(expdat2)<-c('Session','Subject','Group','Condition','GroupCond','p.Correct')
  expdat2$GroupCond<-as.factor(expdat2$GroupCond)
  levels(expdat2$GroupCond)<-c('DLD_novel','DLD-rep','TYP_novel','TYP_rep')
  
  fit1 <- glmer(p.Correct ~ 1+Condition*Session*Group + (0 + Session | Subject), family = binomial, data = expdat2,weights=rep(nItem,nrow(expdat2)),control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  sumfit<-summary(fit1)
  
  tt <- getME(fit1,"theta")
  ll <- getME(fit1,"lower")
  singular<-min(tt[ll==0])
  pval1<-sumfit$coefficients['Session',4]
  pval2<-sumfit$coefficients['GroupT',4]
  pval3<-sumfit$coefficients['Session:GroupT',4]
  return(c(pval1,pval2,pval3,singular))
}

#####################################################################

#function for one group only
fitsim3 <- function(i,Nitem,nsub) {
  #we proceed by simulating the full data for 2 groups but then split it
  expdat <- data_sims(nItem, nsub, ncond, nsess, ngroup,allcondp,randsubs,randitems)
  
  for (mygroup in c('D','T'))  {
    expdatA <- expdat[expdat$Group==mygroup,]
    w<-which(colnames(expdatA) %in% c('Group','GroupCond'))
    expdatA<-expdatA[,-w]
    #now aggregate means
    expdat2 <- aggregate(expdatA$Correct,by=list(expdatA$Session,expdatA$Subject,expdatA$Condition),FUN=mean)
    colnames(expdat2)<-c('Session','Subject','Condition','p.Correct')
    
    fit1D <- glmer(p.Correct ~ 1+Condition*Session+ (0 + Session | Subject),
                   family = binomial, data = expdat2,weights=rep(nItem,nrow(expdat2)),
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
    
    sumfit<-summary(fit1D)
    
    tt <- getME(fit1,"theta")
    ll <- getME(fit1,"lower")
    singular5<-min(tt[ll==0])
    
    pval5<-sumfit$coefficients['Condition:Session',4] 
    #assign to pval5 but copy to pval4 if group = D
    if (mygroup=='D'){
      pval4<-pval5
      singular4<-singular5
    }
  } #next group
  return(c(pval4,pval5,singular4,singular5)) #pvals for both groups
}
#####################################################################

allnsub<-c(25,30)
allnitem<-c(16,20)


powerdf<-expand.grid(allnsub,allnitem)
colnames(powerdf)<-c('nsub','nitem')
#initialise columns to hold power values for the 5 comparisons of interest
powerdf$powSess <-NA
powerdf$powGp <-NA
powerdf$powGp.Sess<- NA
powerdf$DLD.Cond.Sess<-NA
powerdf$TD.Cond.Sess<-NA

myp <- .01 #adjusted alpha to allow for 5 contrasts

thisrow<-0 #initialise row for powerdf
for (nsub in allnsub){
  for (Nitem in allnitem){
    thisrow <- thisrow+1
    #Get first 3 power values from fitsim1
    pvals<-laply(seq(nsim),function(i){fitsim2(i,nItem, nsub)})
    powerdf$powSess[thisrow] <- mean(pvals[,1] < myp)
    powerdf$powGp[thisrow] <- mean(pvals[,2] < myp)
    powerdf$powGp.Sess[thisrow] <- mean(pvals[,3] < myp)
    
    pvals<-laply(seq(nsim),function(i){fitsim3(i,nItem, nsub)})
    powerdf$DLD.Cond.Sess[thisrow] <- mean(pvals[,1] < myp)
    powerdf$TD.Cond.Sess[thisrow] <- mean(pvals[,2] < myp)
  }
}