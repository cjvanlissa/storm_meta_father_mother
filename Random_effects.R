
source("pool_correlation_matrices_robust_se.R")
set.seed(3990)
pool_correlation_matrices()

#to be able to run the following sPIipt, it is necessary to run the sPIipt "pool_correlation_matrices_robust_se.R" seperately for each moderator!"

#First, I load all the dataframes needed for the random effects analyses, and recode which_cor in a way that it is clear for which subgroup the effect size is.
load("pooled_Overall.RData")
dat<- (subgroups[["Main"]][["robust_estimates"]][["data"]])
dat$which_corR <- recode(dat$which_cor, "m_a" = "total:m_a", "f_a" = "total:f_a","m_f" = "total:m_f")
#For each parenting dimension
load("pooled_p_cluster.RData")
dat_c <- (subgroups[["control"]][["robust_estimates"]][["data"]])
dat_c$which_corR <- recode(dat_c$which_cor, "m_a" = "control_m_a", "f_a" = "control_f_a","m_f" = "control_m_f")
dat_w <- (subgroups[["positive"]][["robust_estimates"]][["data"]])
dat_w$which_corR <- recode(dat_w$which_cor, "m_a" = "warm_m_a", "f_a" = "warm_f_a","m_f" = "warm_m_f")
dat_h <- (subgroups[["negative"]][["robust_estimates"]][["data"]])
dat_h$which_corR <- recode(dat_h$which_cor, "m_a" = "harsh_m_a", "f_a" = "harsh_f_a","m_f" = "harsh_m_f")
#For each specific form of behavioral control (for Reviewer1)
load("pooled_p_control.RData")
dat_cb <- (subgroups[["behavioral"]][["robust_estimates"]][["data"]])
dat_cb$which_corR <- recode(dat_cb$which_cor, "m_a" = "bcontrol_m_a", "f_a" = "bcontrol_f_a","m_f" = "bcontrol_m_f")
dat_ci <- (subgroups[["inconsistent"]][["robust_estimates"]][["data"]])
dat_ci$which_corR <- recode(dat_ci$which_cor, "m_a" = "icontrol_m_a", "f_a" = "icontrol_f_a","m_f" = "icontrol_m_f")
dat_cp <- (subgroups[["psychological"]][["robust_estimates"]][["data"]])
dat_cp$which_corR <- recode(dat_cp$which_cor, "m_a" = "pcontrol_m_a", "f_a" = "pcontrol_f_a","m_f" = "pcontrol_m_f")
#For concurrent vs. predictive studies
load("pooled_same_wave.RData")
dat_concurrent <- (subgroups[["Yes"]][["robust_estimates"]][["data"]])
dat_concurrent$which_corR <- recode(dat_concurrent$which_cor, "m_a" = "concurrent_m_a", "f_a" = "concurrent_f_a","m_f" = "concurrent_m_f")
dat_predictive<- (subgroups[["No"]][["robust_estimates"]][["data"]])
dat_predictive$which_corR <- recode(dat_predictive$which_cor, "m_a" = "predictive_m_a", "f_a" = "predictive_f_a","m_f" = "predictive_m_f")
#for adolescent vs. pre-adolescent samples
load("pooled_age_group.RData")
dat_young<- (subgroups[["Young"]][["robust_estimates"]][["data"]])
dat_young$which_corR <- recode(dat_young$which_cor, "m_a" = "young_m_a", "f_a" = "young_f_a","m_f" = "young_m_f")
dat_old<- (subgroups[["Old"]][["robust_estimates"]][["data"]])
dat_old$which_corR <- recode(dat_old$which_cor, "m_a" = "old_m_a", "f_a" = "old_f_a","m_f" = "old_m_f")
#for young vs. med. vs old children (for reviewer 1)
load("pooled_age_group3.RData")
dat_3young<- (subgroups[["Young"]][["robust_estimates"]][["data"]])
dat_3young$which_corR <- recode(dat_3young$which_cor, "m_a" = "3young_m_a", "f_a" = "3young_f_a","m_f" = "3young_m_f")
dat_3med<- (subgroups[["Med"]][["robust_estimates"]][["data"]])
dat_3med$which_corR <- recode(dat_3med$which_cor, "m_a" = "3med_m_a", "f_a" = "3med_f_a","m_f" = "3med_m_f")
dat_3old<- (subgroups[["Old"]][["robust_estimates"]][["data"]])
dat_3old$which_corR <- recode(dat_3old$which_cor, "m_a" = "3old_m_a", "f_a" = "3old_f_a","m_f" = "3old_m_f")
#for US vs non-US samples (for reviewer 1)
load("pooled_country_US.RData")
dat_US<- (subgroups[["US"]][["robust_estimates"]][["data"]])
dat_US$which_corR <- recode(dat_US$which_cor, "m_a" = "US_m_a", "f_a" = "US_f_a","m_f" = "US_m_f")
dat_nonUS<- (subgroups[["NONUS"]][["robust_estimates"]][["data"]])
dat_nonUS$which_corR <- recode(dat_nonUS$which_cor, "m_a" = "nonUS_m_a", "f_a" = "nonUS_f_a","m_f" = "nonUS_m_f")

#Random effects model including prediction intervals for total sample, with moderator of parents' sex. 
t1<-data.frame(t(sapply(unique(dat$which_corR), function(x){ 
  res.REtotal <- rma(yi, vi, data=dat[dat$which_corR == x, ], digits=3)
  predict(res.REtotal)
})))
t2<-data.frame(t(sapply(unique(dat$which_corR),function(x){res.REtotal <- rma(yi, vi, data=dat[dat$which_corR == x, ], digits=3)})))
t<-cbind(t1[,"pred"], t1[,"se"], t2[,"pval"], t1[,"ci.lb"], t1[,"ci.ub"], t1[,"PI.lb"], t1[,"PI.ub"]) 
colnames(t) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
#heterogeneity:
Q1<- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat)
#Random effects model including prediction intervals for each parenting dimension, with moderator of parents' sex. 
# behavioral control (recoded to positive behavioral control): 
tc1<-data.frame(t(sapply(unique(dat_c$which_corR), function(x){ 
  res.REcontrol <- rma(yi, vi, data=dat_c[dat_c$which_corR == x, ], digits=3)
  predict(res.REcontrol)
})))
tc2<-data.frame(t(sapply(unique(dat_c$which_corR), function(x){ 
  res.REcontrol <- rma(yi, vi, data=dat_c[dat_c$which_corR == x, ], digits=3)
})))
tc<-cbind(tc1[,"pred"], tc1[,"se"], tc2[,"pval"], tc1[,"ci.lb"], tc1[,"ci.ub"], tc1[,"PI.lb"], tc1[,"PI.ub"]) 
colnames(tc) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q2a <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_c)

#parental warmth:
tw1<-data.frame(t(sapply(unique(dat_w$which_corR), function(x){ 
  res.REwarm <- rma(yi, vi, data=dat_w[dat_w$which_corR == x, ], digits=3)
  predict(res.REwarm)
})))
tw2<-data.frame(t(sapply(unique(dat_w$which_corR), function(x){ 
  res.REwarm <- rma(yi, vi, data=dat_w[dat_w$which_corR == x, ], digits=3)
})))
tw<-cbind(tw1[,"pred"], tw1[,"se"], tw2[,"pval"], tw1[,"ci.lb"], tw1[,"ci.ub"], tw1[,"PI.lb"], tw1[,"PI.ub"]) 
colnames(tw) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q2b <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_w)
#(low) harsh parenting:
th1<-data.frame(t(sapply(unique(dat_h$which_corR), function(x){ 
  res.REharsh <- rma(yi, vi, data=dat_h[dat_h$which_corR == x, ], digits=3)
  predict(res.REharsh)
})))
th2<-data.frame(t(sapply(unique(dat_h$which_corR), function(x){ 
  res.REharsh <- rma(yi, vi, data=dat_h[dat_h$which_corR == x, ], digits=3)
})))
th<-cbind(th1[,"pred"], th1[,"se"], th2[,"pval"], th1[,"ci.lb"], th1[,"ci.ub"], th1[,"PI.lb"], th1[,"PI.ub"]) 
colnames(th) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q2c <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_h)

#Random effects model for studies examining specific types parental control, moderator of parents' sex. 
#behavioral control
tcb1<-data.frame(t(sapply(unique(dat_cb$which_corR), function(x){ 
  res.REbcontrol <- rma(yi, vi, data=dat_cb[dat_cb$which_corR == x, ], digits=3)
  predict(res.REbcontrol)
})))
tcb2<-data.frame(t(sapply(unique(dat_cb$which_corR), function(x){ 
  res.REbcontrol <- rma(yi, vi, data=dat_cb[dat_cb$which_corR == x, ], digits=3)
})))
tcb<-cbind(tcb1[,"pred"], tcb1[,"se"], tcb2[,"pval"], tcb1[,"ci.lb"], tcb1[,"ci.ub"], tcb1[,"PI.lb"], tcb1[,"PI.ub"]) 
colnames(tcb) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q3a <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_cb)
#inconsistent control
tci1<-data.frame(t(sapply(unique(dat_ci$which_corR), function(x){ 
  res.REicontrol <- rma(yi, vi, data=dat_ci[dat_ci$which_corR == x, ], digits=3)
  predict(res.REicontrol)
})))
tci2<-data.frame(t(sapply(unique(dat_ci$which_corR), function(x){ 
  res.REicontrol <- rma(yi, vi, data=dat_ci[dat_ci$which_corR == x, ], digits=3)
})))
tci<-cbind(tci1[,"pred"], tci1[,"se"], tci2[,"pval"], tci1[,"ci.lb"], tci1[,"ci.ub"], tci1[,"PI.lb"], tci1[,"PI.ub"]) 
colnames(tci) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q3b <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_ci)
#psychological control
tcp1<-data.frame(t(sapply(unique(dat_cp$which_corR), function(x){ 
  res.REpcontrol <- rma(yi, vi, data=dat_cp[dat_cp$which_corR == x, ], digits=3)
  predict(res.REpcontrol)
})))
tcp2<-data.frame(t(sapply(unique(dat_cp$which_corR), function(x){ 
  res.REpcontrol <- rma(yi, vi, data=dat_cp[dat_cp$which_corR == x, ], digits=3)
})))
tcp<-cbind(tcp1[,"pred"], tcp1[,"se"], tcp2[,"pval"], tcp1[,"ci.lb"], tcp1[,"ci.ub"], tcp1[,"PI.lb"], tcp1[,"PI.ub"]) 
colnames(tcp) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q3c <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_cp)
#Random effects model for studies examining concurrent vs predictive associations, moderator of parents' sex. 
#concurrent
tconc1<-data.frame(t(sapply(unique(dat_concurrent$which_corR), function(x){ 
  res.REconcurrent <- rma(yi, vi, data=dat_concurrent[dat_concurrent$which_corR == x, ], digits=3)
  predict(res.REconcurrent)
})))
tconc2<-data.frame(t(sapply(unique(dat_concurrent$which_corR), function(x){ 
  res.REconcurrent <- rma(yi, vi, data=dat_concurrent[dat_concurrent$which_corR == x, ], digits=3)
})))
tconc<-cbind(tconc1[,"pred"], tconc1[,"se"], tconc2[,"pval"], tconc1[,"ci.lb"], tconc1[,"ci.ub"], tconc1[,"PI.lb"], tconc1[,"PI.ub"]) 
colnames(tconc) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q4a <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_concurrent)
#predictive
tpred1<-data.frame(t(sapply(unique(dat_predictive$which_corR), function(x){ 
  res.REpredictive <- rma(yi, vi, data=dat_predictive[dat_predictive$which_corR == x, ], digits=3)
  predict(res.REpredictive)
})))
tpred2<-data.frame(t(sapply(unique(dat_predictive$which_corR), function(x){ 
  res.REpredictive <- rma(yi, vi, data=dat_predictive[dat_predictive$which_corR == x, ], digits=3)
})))
tpred<-cbind(tpred1[,"pred"], tpred1[,"se"], tpred2[,"pval"], tpred1[,"ci.lb"], tpred1[,"ci.ub"], tpred1[,"PI.lb"], tpred1[,"PI.ub"]) 
colnames(tpred) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q4b <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_predictive)
#Random effects model for studies examining pre-adolescent vs adolescent children, moderator of parents' sex. 
#young (pre-adolescence)
tyoung1<-data.frame(t(sapply(unique(dat_young$which_corR), function(x){ 
  res.REyoung <- rma(yi, vi, data=dat_young[dat_young$which_corR == x, ], digits=3)
  predict(res.REyoung)
})))
tyoung2<-data.frame(t(sapply(unique(dat_young$which_corR), function(x){ 
  res.REyoung <- rma(yi, vi, data=dat_young[dat_young$which_corR == x, ], digits=3)
})))
tyoung<-cbind(tyoung1[,"pred"], tyoung1[,"se"], tyoung2[,"pval"], tyoung1[,"ci.lb"], tyoung1[,"ci.ub"], tyoung1[,"PI.lb"], tyoung1[,"PI.ub"]) 
colnames(tyoung) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q5a <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_young)
#old (adolescence)
told1<-data.frame(t(sapply(unique(dat_old$which_corR), function(x){ 
  res.REold <- rma(yi, vi, data=dat_old[dat_old$which_corR == x, ], digits=3)
  predict(res.REold)
})))
told2<-data.frame(t(sapply(unique(dat_old$which_corR), function(x){ 
  res.REold <- rma(yi, vi, data=dat_old[dat_old$which_corR == x, ], digits=3)
})))
told<-cbind(told1[,"pred"], told1[,"se"], told2[,"pval"], told1[,"ci.lb"], told1[,"ci.ub"], told1[,"PI.lb"], told1[,"PI.ub"]) 
colnames(told) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q5b <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_old)
#Random effects model for studies examining differentiation of 3 age-groups for reviewer 1, moderator of parents' sex. 
#young
t3young1<-data.frame(t(sapply(unique(dat_3young$which_corR), function(x){ 
  res.RE3young <- rma(yi, vi, data=dat_3young[dat_3young$which_corR == x, ], digits=3)
  predict(res.RE3young)
})))
t3young2<-data.frame(t(sapply(unique(dat_3young$which_corR), function(x){ 
  res.RE3young <- rma(yi, vi, data=dat_3young[dat_3young$which_corR == x, ], digits=3)
})))
t3young<-cbind(t3young1[,"pred"], t3young1[,"se"], t3young2[,"pval"], t3young1[,"ci.lb"], t3young1[,"ci.ub"], t3young1[,"PI.lb"], t3young1[,"PI.ub"]) 
colnames(t3young) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q6a <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_3young)
#medium age
t3med1<-data.frame(t(sapply(unique(dat_3med$which_corR), function(x){ 
  res.RE3med <- rma(yi, vi, data=dat_3med[dat_3med$which_corR == x, ], digits=3)
  predict(res.RE3med)
})))
t3med2<-data.frame(t(sapply(unique(dat_3med$which_corR), function(x){ 
  res.RE3med <- rma(yi, vi, data=dat_3med[dat_3med$which_corR == x, ], digits=3)
})))
t3med<-cbind(t3med1[,"pred"], t3med1[,"se"], t3med2[,"pval"], t3med1[,"ci.lb"], t3med1[,"ci.ub"], t3med1[,"PI.lb"], t3med1[,"PI.ub"]) 
colnames(t3med) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q6b <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_3med)
#old 
t3old1<-data.frame(t(sapply(unique(dat_3old$which_corR), function(x){ 
  res.RE3old <- rma(yi, vi, data=dat_3old[dat_3old$which_corR == x, ], digits=3)
  predict(res.RE3old)
})))
t3old2<-data.frame(t(sapply(unique(dat_3old$which_corR), function(x){ 
  res.RE3old <- rma(yi, vi, data=dat_3old[dat_3old$which_corR == x, ], digits=3)
})))
t3old<-cbind(t3old1[,"pred"], t3old1[,"se"], t3old2[,"pval"], t3old1[,"ci.lb"], t3old1[,"ci.ub"], t3old1[,"PI.lb"], t3old1[,"PI.ub"]) 
colnames(t3old) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q6c <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_3old)

#Random effects model for non-US vs. US (for Reviewer 1)
#US
tUS1<-data.frame(t(sapply(unique(dat_US$which_corR), function(x){ 
  res.REUS <- rma(yi, vi, data=dat_US[dat_US$which_corR == x, ], digits=3)
  predict(res.REUS)
})))
tUS2<-data.frame(t(sapply(unique(dat_US$which_corR), function(x){ 
  res.REUS <- rma(yi, vi, data=dat_US[dat_US$which_corR == x, ], digits=3)
})))
tUS<-cbind(tUS1[,"pred"], tUS1[,"se"], tUS2[,"pval"], tUS1[,"ci.lb"], tUS1[,"ci.ub"], tUS1[,"PI.lb"], tUS1[,"PI.ub"]) 
colnames(tUS) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q7a <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_US)
#non-US 
tnonUS1<-data.frame(t(sapply(unique(dat_nonUS$which_corR), function(x){ 
  res.REnonUS <- rma(yi, vi, data=dat_nonUS[dat_nonUS$which_corR == x, ], digits=3)
  predict(res.REnonUS)
})))
tnonUS2<-data.frame(t(sapply(unique(dat_nonUS$which_corR), function(x){ 
  res.REnonUS <- rma(yi, vi, data=dat_nonUS[dat_nonUS$which_corR == x, ], digits=3)
})))
tnonUS<-cbind(tnonUS1[,"pred"], tnonUS1[,"se"], tnonUS2[,"pval"], tnonUS1[,"ci.lb"], tnonUS1[,"ci.ub"], tnonUS1[,"PI.lb"], tnonUS1[,"PI.ub"]) 
colnames(tnonUS) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub")
Q7b <- rma(yi, vi, mods=~ factor(which_corR)-1, data=dat_nonUS)
#Merge al results table into one table
res.RE<-data.frame(rbind(t, tc, tw, th, tcb, tci, tcp, tconc, tpred, tyoung, told, t3young, t3med, t3old, tUS, tnonUS))
#remove unnecessary columns, could not come up with a better way;)
#Unlist, again, could not find a better way to do this
res.RE$Estimate<-unlist(res.RE$Estimate, recursive=TRUE, use.names=TRUE)
res.RE$SE<-unlist(res.RE$SE, recursive=TRUE, use.names=TRUE)
res.RE$P.value<-unlist(res.RE$P.value, recursive=TRUE, use.names=TRUE)
res.RE$CI.lb<-unlist(res.RE$CI.lb, recursive=TRUE, use.names=TRUE)
res.RE$CI.ub<-unlist(res.RE$CI.ub, recursive=TRUE, use.names=TRUE)
res.RE$PI.lb<-unlist(res.RE$PI.lb, recursive=TRUE, use.names=TRUE)
res.RE$PI.ub<-unlist(res.RE$PI.ub, recursive=TRUE, use.names=TRUE)
res.RE2 <- rapply(object = res.RE, f = round, classes = "numeric", how = "replace", digits = 3) 

write.csv(res.RE, "results_RE.csv", row.names = TRUE)

#Make table for heterogeneity tests per subgroup
Moderator <- c('total sample', 'parental control', "parental warmth", 'low harsh parenting', 'behavioral control', 'inconsistent control', 'psychological control', 'concurrent', 'predictive', 'young', 'old', '3young', '3med', '3old','US','non US')
QE <- c(Q1$QE, Q2a$QE, Q2b$QE, Q2c$QE, Q3a$QE, Q3b$QE, Q3c$QE, Q4a$QE, Q4b$QE, Q5a$QE, Q5b$QE, Q6a$QE, Q6b$QE, Q6c$QE, Q7a$QE, Q7b$QE)
#Pvalue <- c(Q1$QEp, Q2a$QEp, Q2b$QEp, Q2c$QEp, Q3a$QEp, Q3b$QEp, Q3c$QEp, Q4a$QEp, Q4b$QEp, Q5a$QEp, Q5b$QEp, Q6a$QEp, Q6b$QEp, Q6c$QEp, Q7a$QEp, Q7b$QEp)
  #as some p's are so small that they are reported as 0, I replaced it for <.0001. However everytime I change somehting to the syntax, we should check whether this is still the same!
Pvalue <- "< .0001"

QEresults <- data.frame(Moderator, QE, Pvalue, stringsAsFactors=FALSE)
QEresults2 <- rapply(object = QEresults, f = round, classes = "numeric", how = "replace", digits = 4) 
?rapply
?write.csv
write.csv(QEresults2, "results_QE.csv", row.names = FALSE)
