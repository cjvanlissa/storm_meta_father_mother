
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
t<-data.frame(t(sapply(unique(dat$which_corR), function(x){ 
  res.REtotal <- rma(yi, vi, data=dat[dat$which_corR == x, ], digits=3)
  predict(res.REtotal)
})))

#Random effects model including prediction intervals for each parenting dimension, with moderator of parents' sex. 
# behavioral control (recoded to positive behavioral control): 
tc<-data.frame(t(sapply(unique(dat_c$which_corR), function(x){ 
  res.REcontrol <- rma(yi, vi, data=dat_c[dat_c$which_corR == x, ], digits=3)
  predict(res.REcontrol)
})))
#parental warmth:
tw<-data.frame(t(sapply(unique(dat_w$which_corR), function(x){ 
  res.REwarm <- rma(yi, vi, data=dat_w[dat_w$which_corR == x, ], digits=3)
  predict(res.REwarm)
})))
#(low) harsh parenting:
th<-data.frame(t(sapply(unique(dat_h$which_corR), function(x){ 
  res.REharsh <- rma(yi, vi, data=dat_h[dat_h$which_corR == x, ], digits=3)
  predict(res.REharsh)
})))

#Random effects model for studies examining specific types parental control, moderator of parents' sex. 
#behavioral control
tcb<-data.frame(t(sapply(unique(dat_cb$which_corR), function(x){ 
  res.REbcontrol <- rma(yi, vi, data=dat_cb[dat_cb$which_corR == x, ], digits=3)
  predict(res.REbcontrol)
})))
#inconsistent control
tci<-data.frame(t(sapply(unique(dat_ci$which_corR), function(x){ 
  res.REicontrol <- rma(yi, vi, data=dat_ci[dat_ci$which_corR == x, ], digits=3)
  predict(res.REicontrol)
})))
#psychological control
tcp<-data.frame(t(sapply(unique(dat_cp$which_corR), function(x){ 
  res.REpcontrol <- rma(yi, vi, data=dat_cp[dat_cp$which_corR == x, ], digits=3)
  predict(res.REpcontrol)
})))

#Random effects model for studies examining concurrent vs predictive associations, moderator of parents' sex. 
#concurrent
tconc<-data.frame(t(sapply(unique(dat_concurrent$which_corR), function(x){ 
  res.REconcurrent <- rma(yi, vi, data=dat_concurrent[dat_concurrent$which_corR == x, ], digits=3)
  predict(res.REconcurrent)
})))
#predictive
tpred<-data.frame(t(sapply(unique(dat_predictive$which_corR), function(x){ 
  res.REpredictive <- rma(yi, vi, data=dat_predictive[dat_predictive$which_corR == x, ], digits=3)
  predict(res.REpredictive)
})))

#Random effects model for studies examining pre-adolescent vs adolescent children, moderator of parents' sex. 
#young (pre-adolescence)
tyoung<-data.frame(t(sapply(unique(dat_young$which_corR), function(x){ 
  res.REyoung <- rma(yi, vi, data=dat_young[dat_young$which_corR == x, ], digits=3)
  predict(res.REyoung)
})))
#old (adolescence)
told<-data.frame(t(sapply(unique(dat_old$which_corR), function(x){ 
  res.REold <- rma(yi, vi, data=dat_old[dat_old$which_corR == x, ], digits=3)
  predict(res.REold)
})))

#Random effects model for studies examining differentiation of 3 age-groups for reviewer 1, moderator of parents' sex. 
#young
t3young<-data.frame(t(sapply(unique(dat_3young$which_corR), function(x){ 
  res.RE3young <- rma(yi, vi, data=dat_3young[dat_3young$which_corR == x, ], digits=3)
  predict(res.RE3young)
})))
#medium age
t3med<-data.frame(t(sapply(unique(dat_3med$which_corR), function(x){ 
  res.RE3med <- rma(yi, vi, data=dat_3med[dat_3med$which_corR == x, ], digits=3)
  predict(res.RE3med)
})))
#old 
t3old<-data.frame(t(sapply(unique(dat_3old$which_corR), function(x){ 
  res.RE3old <- rma(yi, vi, data=dat_3old[dat_3old$which_corR == x, ], digits=3)
  predict(res.RE3old)
})))

#Random effects model for non-US vs. US (for Reviewer 1)
#US
tUS<-data.frame(t(sapply(unique(dat_US$which_corR), function(x){ 
  res.REUS <- rma(yi, vi, data=dat_US[dat_US$which_corR == x, ], digits=3)
  predict(res.REUS)
})))
#non-US 
tnonUS<-data.frame(t(sapply(unique(dat_nonUS$which_corR), function(x){ 
  res.REnonUS <- rma(yi, vi, data=dat_nonUS[dat_nonUS$which_corR == x, ], digits=3)
  predict(res.REnonUS)
})))

#Merge al results table into one table
res.RE<-rbind(t, tc, tw, th, tcb, tci, tcp, tconc, tpred, tyoung, told, t3young, t3med, t3old, tUS, tnonUS)
#remove unnecessary columns, could not come up with a better way;)
dflist <- subset(res.RE, select=-c(slab, digits, method, transf))
#Unlist, again, could not find a better way to do this
dflist$pred<-unlist(dflist$pred, recursive=TRUE, use.names=TRUE)
dflist$se<-unlist(dflist$se, recursive=TRUE, use.names=TRUE)
dflist$ci.lb<-unlist(dflist$ci.lb, recursive=TRUE, use.names=TRUE)
dflist$ci.ub<-unlist(dflist$ci.ub, recursive=TRUE, use.names=TRUE)
dflist$cr.lb<-unlist(dflist$cr.lb, recursive=TRUE, use.names=TRUE)
dflist$cr.ub<-unlist(dflist$cr.ub, recursive=TRUE, use.names=TRUE)

write.csv(dflist, "results_RE.csv", row.names = TRUE)

