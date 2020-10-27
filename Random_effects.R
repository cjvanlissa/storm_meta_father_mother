library(dplyr)
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
t<-cbind(t1[,"pred"], t1[,"se"], t2[,"pval"], t1[,"ci.lb"], t1[,"ci.ub"], t1[,"cr.lb"], t1[,"cr.ub"], t2[,"tau2"], t2[,"I2"], t2[,"QE"], t2[, "QEp"]) 
colnames(t) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#Random effects model including prediction intervals for each parenting dimension, with moderator of parents' sex. 
# behavioral control (recoded to positive behavioral control): 
tc1<-data.frame(t(sapply(unique(dat_c$which_corR), function(x){ 
  res.REcontrol <- rma(yi, vi, data=dat_c[dat_c$which_corR == x, ], digits=3)
  predict(res.REcontrol)
})))
tc2<-data.frame(t(sapply(unique(dat_c$which_corR), function(x){ 
  res.REcontrol <- rma(yi, vi, data=dat_c[dat_c$which_corR == x, ], digits=3)
})))
tc<-cbind(tc1[,"pred"], tc1[,"se"], tc2[,"pval"], tc1[,"ci.lb"], tc1[,"ci.ub"], tc1[,"cr.lb"], tc1[,"cr.ub"], tc2[,"tau2"], tc2[,"I2"], tc2[,"QE"], tc2[, "QEp"]) 
colnames(tc) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#parental warmth:
tw1<-data.frame(t(sapply(unique(dat_w$which_corR), function(x){ 
  res.REwarm <- rma(yi, vi, data=dat_w[dat_w$which_corR == x, ], digits=3)
  predict(res.REwarm)
})))
tw2<-data.frame(t(sapply(unique(dat_w$which_corR), function(x){ 
  res.REwarm <- rma(yi, vi, data=dat_w[dat_w$which_corR == x, ], digits=3)
})))
tw<-cbind(tw1[,"pred"], tw1[,"se"], tw2[,"pval"], tw1[,"ci.lb"], tw1[,"ci.ub"], tw1[,"cr.lb"], tw1[,"cr.ub"], tw2[,"tau2"], tw2[,"I2"], tw2[,"QE"], tw2[, "QEp"]) 
colnames(tw) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")
#(low) harsh parenting:
th1<-data.frame(t(sapply(unique(dat_h$which_corR), function(x){ 
  res.REharsh <- rma(yi, vi, data=dat_h[dat_h$which_corR == x, ], digits=3)
  predict(res.REharsh)
})))
th2<-data.frame(t(sapply(unique(dat_h$which_corR), function(x){ 
  res.REharsh <- rma(yi, vi, data=dat_h[dat_h$which_corR == x, ], digits=3)
})))
th<-cbind(th1[,"pred"], th1[,"se"], th2[,"pval"], th1[,"ci.lb"], th1[,"ci.ub"], th1[,"cr.lb"], th1[,"cr.ub"], th2[,"tau2"], th2[,"I2"], th2[,"QE"], th2[, "QEp"]) 
colnames(th) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#Random effects model for studies examining concurrent vs predictive associations, moderator of parents' sex. 
#concurrent
tconc1<-data.frame(t(sapply(unique(dat_concurrent$which_corR), function(x){ 
  res.REconcurrent <- rma(yi, vi, data=dat_concurrent[dat_concurrent$which_corR == x, ], digits=3)
  predict(res.REconcurrent)
})))
tconc2<-data.frame(t(sapply(unique(dat_concurrent$which_corR), function(x){ 
  res.REconcurrent <- rma(yi, vi, data=dat_concurrent[dat_concurrent$which_corR == x, ], digits=3)
})))
tconc<-cbind(tconc1[,"pred"], tconc1[,"se"], tconc2[,"pval"], tconc1[,"ci.lb"], tconc1[,"ci.ub"], tconc1[,"cr.lb"], tconc1[,"cr.ub"], tconc2[,"tau2"], tconc2[,"I2"], tconc2[,"QE"], tconc2[, "QEp"]) 
colnames(tconc) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#predictive
tpred1<-data.frame(t(sapply(unique(dat_predictive$which_corR), function(x){ 
  res.REpredictive <- rma(yi, vi, data=dat_predictive[dat_predictive$which_corR == x, ], digits=3)
  predict(res.REpredictive)
})))
tpred2<-data.frame(t(sapply(unique(dat_predictive$which_corR), function(x){ 
  res.REpredictive <- rma(yi, vi, data=dat_predictive[dat_predictive$which_corR == x, ], digits=3)
})))
tpred<-cbind(tpred1[,"pred"], tpred1[,"se"], tpred2[,"pval"], tpred1[,"ci.lb"], tpred1[,"ci.ub"], tpred1[,"cr.lb"], tpred1[,"cr.ub"], tpred2[,"tau2"], tpred2[,"I2"], tpred2[,"QE"], tpred2[, "QEp"]) 
colnames(tpred) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#Random effects model for studies examining pre-adolescent vs adolescent children, moderator of parents' sex. 
#young (pre-adolescence)
tyoung1<-data.frame(t(sapply(unique(dat_young$which_corR), function(x){ 
  res.REyoung <- rma(yi, vi, data=dat_young[dat_young$which_corR == x, ], digits=3)
  predict(res.REyoung)
})))
tyoung2<-data.frame(t(sapply(unique(dat_young$which_corR), function(x){ 
  res.REyoung <- rma(yi, vi, data=dat_young[dat_young$which_corR == x, ], digits=3)
})))
tyoung<-cbind(tyoung1[,"pred"], tyoung1[,"se"], tyoung2[,"pval"], tyoung1[,"ci.lb"], tyoung1[,"ci.ub"], tyoung1[,"cr.lb"], tyoung1[,"cr.ub"], tyoung2[,"tau2"], tyoung2[,"I2"], tyoung2[,"QE"], tyoung2[, "QEp"]) 
colnames(tyoung) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#old (adolescence)
told1<-data.frame(t(sapply(unique(dat_old$which_corR), function(x){ 
  res.REold <- rma(yi, vi, data=dat_old[dat_old$which_corR == x, ], digits=3)
  predict(res.REold)
})))
told2<-data.frame(t(sapply(unique(dat_old$which_corR), function(x){ 
  res.REold <- rma(yi, vi, data=dat_old[dat_old$which_corR == x, ], digits=3)
})))
told<-cbind(told1[,"pred"], told1[,"se"], told2[,"pval"], told1[,"ci.lb"], told1[,"ci.ub"], told1[,"cr.lb"], told1[,"cr.ub"], told2[,"tau2"], told2[,"I2"], told2[,"QE"], told2[, "QEp"]) 
colnames(told) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#Random effects model for studies examining differentiation of 3 age-groups for reviewer 1, moderator of parents' sex. 
#young
t3young1<-data.frame(t(sapply(unique(dat_3young$which_corR), function(x){ 
  res.RE3young <- rma(yi, vi, data=dat_3young[dat_3young$which_corR == x, ], digits=3)
  predict(res.RE3young)
})))
t3young2<-data.frame(t(sapply(unique(dat_3young$which_corR), function(x){ 
  res.RE3young <- rma(yi, vi, data=dat_3young[dat_3young$which_corR == x, ], digits=3)
})))
t3young<-cbind(t3young1[,"pred"], t3young1[,"se"], t3young2[,"pval"], t3young1[,"ci.lb"], t3young1[,"ci.ub"], t3young1[,"cr.lb"], t3young1[,"cr.ub"], t3young2[,"tau2"], t3young2[,"I2"], t3young2[,"QE"], t3young2[, "QEp"]) 
colnames(t3young) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#medium age
t3med1<-data.frame(t(sapply(unique(dat_3med$which_corR), function(x){ 
  res.RE3med <- rma(yi, vi, data=dat_3med[dat_3med$which_corR == x, ], digits=3)
  predict(res.RE3med)
})))
t3med2<-data.frame(t(sapply(unique(dat_3med$which_corR), function(x){ 
  res.RE3med <- rma(yi, vi, data=dat_3med[dat_3med$which_corR == x, ], digits=3)
})))
t3med<-cbind(t3med1[,"pred"], t3med1[,"se"], t3med2[,"pval"], t3med1[,"ci.lb"], t3med1[,"ci.ub"], t3med1[,"cr.lb"], t3med1[,"cr.ub"], t3med2[,"tau2"], t3med2[,"I2"], t3med2[,"QE"], t3med2[, "QEp"]) 
colnames(t3med) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#old 
t3old1<-data.frame(t(sapply(unique(dat_3old$which_corR), function(x){ 
  res.RE3old <- rma(yi, vi, data=dat_3old[dat_3old$which_corR == x, ], digits=3)
  predict(res.RE3old)
})))
t3old2<-data.frame(t(sapply(unique(dat_3old$which_corR), function(x){ 
  res.RE3old <- rma(yi, vi, data=dat_3old[dat_3old$which_corR == x, ], digits=3)
})))
t3old<-cbind(t3old1[,"pred"], t3old1[,"se"], t3old2[,"pval"], t3old1[,"ci.lb"], t3old1[,"ci.ub"], t3old1[,"cr.lb"], t3old1[,"cr.ub"], t3old2[,"tau2"], t3old2[,"I2"], t3old2[,"QE"], t3old2[, "QEp"]) 
colnames(t3old) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")


#Random effects model for non-US vs. US (for Reviewer 1)
#US
tUS1<-data.frame(t(sapply(unique(dat_US$which_corR), function(x){ 
  res.REUS <- rma(yi, vi, data=dat_US[dat_US$which_corR == x, ], digits=3)
  predict(res.REUS)
})))
tUS2<-data.frame(t(sapply(unique(dat_US$which_corR), function(x){ 
  res.REUS <- rma(yi, vi, data=dat_US[dat_US$which_corR == x, ], digits=3)
})))
tUS<-cbind(tUS1[,"pred"], tUS1[,"se"], tUS2[,"pval"], tUS1[,"ci.lb"], tUS1[,"ci.ub"], tUS1[,"cr.lb"], tUS1[,"cr.ub"], tUS2[,"tau2"], tUS2[,"I2"], tUS2[,"QE"], tUS2[, "QEp"]) 
colnames(tUS) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#non-US 
tnonUS1<-data.frame(t(sapply(unique(dat_nonUS$which_corR), function(x){ 
  res.REnonUS <- rma(yi, vi, data=dat_nonUS[dat_nonUS$which_corR == x, ], digits=3)
  predict(res.REnonUS)
})))
tnonUS2<-data.frame(t(sapply(unique(dat_nonUS$which_corR), function(x){ 
  res.REnonUS <- rma(yi, vi, data=dat_nonUS[dat_nonUS$which_corR == x, ], digits=3)
})))
tnonUS<-cbind(tnonUS1[,"pred"], tnonUS1[,"se"], tnonUS2[,"pval"], tnonUS1[,"ci.lb"], tnonUS1[,"ci.ub"], tnonUS1[,"cr.lb"], tnonUS1[,"cr.ub"], tnonUS2[,"tau2"], tnonUS2[,"I2"], tnonUS2[,"QE"], tnonUS2[, "QEp"]) 
colnames(tnonUS) <- c("Estimate", "SE", "P value", "CI lb", "CI ub", "PI lb", "PI ub", "Tau2", "I2", "QE", "QEp")

#Merge al results table into one table (without three age subgroups US/nonUS as we don't publish this but only mention in response to reviewer letter)
res.RE<-data.frame(rbind(t, tc, tw, th, tconc, tpred, tyoung, told))
#remove unnecessary columns, could not come up with a better way;)
#Unlist, again, could not find a better way to do this
res.RE$Estimate<-unlist(res.RE$Estimate, recursive=TRUE, use.names=TRUE)
res.RE$SE<-unlist(res.RE$SE, recursive=TRUE, use.names=TRUE)
res.RE$P.value<-unlist(res.RE$P.value, recursive=TRUE, use.names=TRUE)
res.RE$CI.lb<-unlist(res.RE$CI.lb, recursive=TRUE, use.names=TRUE)
res.RE$CI.ub<-unlist(res.RE$CI.ub, recursive=TRUE, use.names=TRUE)
res.RE$PI.lb<-unlist(res.RE$PI.lb, recursive=TRUE, use.names=TRUE)
res.RE$PI.ub<-unlist(res.RE$PI.ub, recursive=TRUE, use.names=TRUE)
res.RE$Tau2<-unlist(res.RE$Tau2, recursive=TRUE, use.names=TRUE)
res.RE$I2<-unlist(res.RE$I2, recursive=TRUE, use.names=TRUE)
res.RE$QE<-unlist(res.RE$QE, recursive=TRUE, use.names=TRUE)
res.RE$QEp<-unlist(res.RE$QEp, recursive=TRUE, use.names=TRUE)
#res.RE2 <- rapply(object = res.RE, f = round, classes = "numeric", how = "replace", digits = 3) 

write.csv(res.RE, "results_RE.csv", row.names = TRUE)


