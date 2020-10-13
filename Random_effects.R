
source("pool_correlation_matrices_robust_se.R")
set.seed(3990)
pool_correlation_matrices()

#to be able to run the following script, it is necessary to run the script "pool_correlation_matrices_robust_se.R" seperately for each moderator!"

#Random effects model for total sample, taking fathers and mothers together 
#NOTE that this takes all three correlations: 
  #paternal parenting + child prosocial
  #maternal parenting + child prosocial
  #paternal parenting + maternal parenting. 
load("pooled_Overall.RData")
dat <- (subgroups[["Main"]][["robust_estimates"]][["data"]])
res.REtotal <- rma(yi, vi, data=dat, digits=3)
res.REtotal
predict(res.REtotal)

#Random effects model for total sample, with moderator of parents' sex. 
res.REtotalt <- rma(yi, vi, mods=~ factor(which_cor)-1, data=dat, digits=3)
res.REtotalt
predict(res.REtotalt)

#Random effects model for studies examining behavioral control (recoded to positive behavioral control), moderator of parents' sex. 
load("pooled_p_cluster.RData")
dat_c <- (subgroups[["control"]][["robust_estimates"]][["data"]])
res.REcontrol <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_c, digits=3)
res.REcontrol
predict(res.REcontrol)

#Random effects model for studies three forms of parental control, moderator of parents' sex. 
load("pooled_p_control.RData")
  #behavioral control
dat_cb <- (subgroups[["behavioral"]][["robust_estimates"]][["data"]])
res.REcontrolb <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_cb, digits=3)
res.REcontrolb
predict(res.REcontrolb)
  #inconsistent control
dat_ci <- (subgroups[["inconsistent"]][["robust_estimates"]][["data"]])
res.REcontroli <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_ci, digits=3)
res.REcontroli
predict(res.REcontroli)
  #psychological control
dat_cp <- (subgroups[["psychological"]][["robust_estimates"]][["data"]])
res.REcontrolp <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_cp, digits=3)
res.REcontrolp
predict(res.REcontrolp)

#Random effects model for studies examining parental warmth, moderator of parents' sex. 
dat_w <- (subgroups[["positive"]][["robust_estimates"]][["data"]])
res.REwarm <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_w, digits=3)
res.REwarm
predict(res.REwarm)

#Random effects model for studies examining (low) harsh parenting, moderator of parents' sex. 
dat_n <- (subgroups[["negative"]][["robust_estimates"]][["data"]])
res.REnegative <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_n, digits=3)
res.REnegative
predict(res.REnegative)

#Random effects model for studies examining concurrent associations, moderator of parents' sex. 
load("pooled_same_wave.RData")
dat_concurrent<- (subgroups[["Yes"]][["robust_estimates"]][["data"]])
res.REconcurrent <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_concurrent, digits=3)
res.REconcurrent
predict(res.REconcurrent)

#Random effects model for studies examining predictive associations, moderator of parents' sex. 
dat_predictive<- (subgroups[["No"]][["robust_estimates"]][["data"]])
res.REpredictive <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_predictive, digits=3)
res.REpredictive
predict(res.REpredictive)

#Random effects model for studies examining pre-adolescent children, moderator of parents' sex. 
load("pooled_age_group.RData")
dat_young<- (subgroups[["Young"]][["robust_estimates"]][["data"]])
res.REyoung <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_young, digits=3)
res.REyoungmaar 
predict(res.REyoung)

#Random effects model for studies examining adolescent children, moderator of parents' sex. 
dat_old<- (subgroups[["Old"]][["robust_estimates"]][["data"]])
res.REold <- rma(yi, vi, mods =~ factor(which_cor)-1, data=dat_old, digits=3)
res.REold
predict(res.REold)
