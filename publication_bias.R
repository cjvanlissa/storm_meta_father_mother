library(metafor)
library(ggplot2)
# See https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/smallstudyeffects.html for explanation of these tests for publication bias

# Publication bias analyses are conducted using random effects meta-analyses of the Fisher's r-to-Z transformed correlations, because these methods are not available for multilevel meta-analysis or cluster robust standard errors.

# Functions ---------------------------------------------------------------

funnel_plot <- function(models){
  tau2 <- sapply(models, `[[`, "tau2")
  dat <- data.frame(model = do.call(c, mapply(function(x, y){rep(x, y)}, x = 1:3, y = sapply(models, function(x){length(x$yi)}))), 
                    se = sqrt(do.call(c, lapply(models, `[[`, "vi"))),
                    yi = do.call(c, lapply(models, `[[`, "yi"))
                    )
  dat$weights <- 1 / (dat$se^2 + tau2[dat$model])
  
  estimate <- sapply(models, `[[`, "b")
  se <- sapply(models, `[[`, "se")
  
  se.seq = seq(0, max(dat$se), 0.001)
  
  #Compute vectors of the lower-limit and upper limit values for
  #the 95% CI region
  dfCI <- data.frame(model = do.call(c, mapply(function(x, y){rep(x, y)}, x = 1:3, y = length(se.seq), SIMPLIFY = FALSE)),
                     se.seq = rep(se.seq, 3))
  dfCI$ll95 <- estimate[dfCI$model] - (1.96 * rep(se.seq, 3))
  dfCI$ul95 <- estimate[dfCI$model] + (1.96 * rep(se.seq, 3))
  dfCI$ll99 <- estimate[dfCI$model] - (3.29 * rep(se.seq, 3))
  dfCI$ul99 <- estimate[dfCI$model] + (3.29 * rep(se.seq, 3))

  dfCI$meanll95 = estimate[dfCI$model] - (1.96 * se[dfCI$model])
  dfCI$meanul95 = estimate[dfCI$model] + (1.96 * se[dfCI$model])
  list(dfCI = dfCI, dat = dat)
}

#Draw Plot

df <- read.csv("data_cleaned_mods.csv")

names(df)[grep("^R", names(df))] <- c("R_m_a", "R_m_f", "R_f_a")
df$ID_sample <- gsub(" ", "", paste0(df$ID, df$ID_subgroup))
df$use_N <- apply(df[, grep("^N", names(df), value = TRUE), ], 1, min, na.rm = TRUE)

es <- lapply(c("R_m_a", "R_m_f", "R_f_a"), function(thiscor){
  out <- cbind(escalc(ri = df[[thiscor]], ni = df$use_N, measure = "ZCOR"), effect_id = 1:nrow(df), study_id = df$ID_sample)
  out[!is.na(out$yi), ]
})

models <- lapply(es, function(thises){
  rma(yi = yi, vi = vi, data = thises) 
})

plotdat <- funnel_plot(models)
dfCI <- plotdat$dfCI
dat <- plotdat$dat
dfCI$lhs <- c("Mother", "Mother", "Father")[dfCI$model]
dfCI$rhs <- c("Adolescent", "Father", "Adolescent")[dfCI$model]
dat$lhs <- c("Mother", "Mother", "Father")[dat$model]
dat$rhs <- c("Adolescent", "Father", "Adolescent")[dat$model]

fp <- ggplot(data = NULL) +
  geom_line(aes(x = ll95, y = se.seq), linetype = 'dotted', data = dfCI) +
  geom_line(aes(x = ul95, y = se.seq), linetype = 'dotted', data = dfCI) +
  geom_line(aes(x = ll99, y = se.seq), linetype = 'dashed', data = dfCI) +
  geom_line(aes(x = ul99, y = se.seq), linetype = 'dashed', data = dfCI) +
  geom_segment(aes(
    x = meanll95,
    y = min(se.seq),
    xend = meanll95,
    yend = max(se.seq)
  ), data = dfCI) +
  geom_segment(aes(
    x = meanul95,
    y = min(se.seq),
    xend = meanul95,
    yend = max(se.seq)
  ), data = dfCI) 

fp <- fp + geom_point(data = dat,
                      mapping = aes_string(x = "yi", y = "se"), alpha = .2) +
  ylab('Standard Error') + xlab('R') +
  scale_y_continuous(trans = "reverse",
                     limits = c(max(dat$se), 0))+#,
                     #expand = c(0, 0)) +
  scale_x_continuous(limits = c(min(dat$yi), max(dat$yi))) +
  theme_bw()+
  theme(legend.position="none") +facet_grid(lhs ~ rhs)

ggsave("combined_funnel.png", fp)

write.csv(cbind(Parameter = paste0("R(", c("mother, adolescent", "mother, father", "father, adolescent"), ")"), t(sapply(models, function(x){ regtest(x)[3:4]}))), "eggerstest.csv")

# Perhaps publication bias for mother/father correlations. However, funnel plot suggests that variance in studies with small SEs is larger than in studies with large SEs.
models[[2]]
m_taf <- trimfill(models[[2]])
funnel(m_taf)

# Trim-and-fill analysis also did not find any studies to remove in the outlying regions of the funnel plot. So our analyses are nonconclusive with regard to publication bias.