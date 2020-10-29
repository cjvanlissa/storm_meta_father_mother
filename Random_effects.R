library(dplyr)
source("pool_correlation_matrices_robust_se.R")
set.seed(3990)
pool_correlation_matrices()
#to be able to run the following script, it is necessary to run the script "pool_correlation_matrices_robust_se.R" seperately for each moderator!"

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


