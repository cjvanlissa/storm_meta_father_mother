library(metaSEM)
library(robumeta)
library(clubSandwich)
library(metafor)
df_main <- read.csv("data_cleaned.csv")
which_mod <- NULL

names(df_main)[grep("^R", names(df_main))] <- c("R_m_a", "R_m_f", "R_f_a")
df_main$ID_sample <- gsub(" ", "", paste0(df_main$ID, df_main$ID_subgroup))
#table(df_main$ID_sample)

# Select effect size data
df_es_wide <-
  df_main[, c(
    "ID_sample",
    grep("^N", names(df_main), value = TRUE),
    grep("^R_\\w_\\w$", names(df_main), value = TRUE)
  )]

if(is.null(which_mod)){
  df_es_wide$moderator <-
    factor(rep("Main", nrow(df_es_wide)))
  which_mod <- "Overall"
} else {
  df_es_wide$moderator <-
    factor(df_main[[which_mod]])
}

subgroups <- lapply(levels(df_es_wide$moderator), function(x) {
  # x <- levels(df_es_wide$moderator)[1]
  df_tmp <-
    df_es_wide[df_es_wide$moderator == x, -ncol(df_es_wide)]
  
  # Since A, M, F all have different N, use the minimal known N per sample ID
  df_tmp$use_N <- do.call(c, tapply(apply(df_tmp[, grep("^N", names(df_tmp)), ], 1, min, na.rm = TRUE), factor(df_tmp$ID_sample), function(x){rep(min(x), length(x))}))
  
  N_total_unique <- sum(tapply(apply(df_tmp[, grep("^N", names(df_tmp)), ], 1, min, na.rm = TRUE), factor(df_tmp$ID_sample), min, na.rm = TRUE))
  # Rename data for melting
  names(df_tmp) <-
    gsub("^R_", "R\\.", names(df_tmp))
  df_tmp <- df_tmp[, !grepl("^N", names(df_tmp))]
  
  # Melt data.frame
  df_tmp <- reshape(
    df_tmp,
    direction = 'long',
    varying = grep("^R", names(df_tmp), value = TRUE),
    timevar = 'which_cor'
  )
  
  # Drop duplicated effect sizes
  #df_tmp <- df_tmp[!duplicated(df_tmp$ID_ES),]
  
  # Calculate variance of the effect size
  df_tmp <-
    escalc(
      measure = "COR",
      ri = R,
      ni = use_N,
      data = df_tmp
    )
  df_tmp <- df_tmp[!is.na(df_tmp$yi),-match("id", names(df_tmp))]
  

  # Estimate pooled correlation matrix using robust variance estimation with
  # bias-reduced linearization adjustment (Pustejovsky and Tipton, 2017)
  robust_estimates <- robu(
    yi ~ which_cor - 1,
    data = df_tmp,
    modelweights = "CORR",
    studynum = ID_sample,
    var.eff.size = vi
  )
  
  # Asymptotic vcov matrix of pooled correlation matrix
  aCov <-
    as.matrix(vcovCR(
      robust_estimates,
      cluster = df_tmp$ID_sample,
      type = "CR2"
    ))
  
  Cov <- vec2symMat(robust_estimates$reg_table$b.r, diag = F)
  observed_variables  <- rownames(aCov) <- colnames(aCov) <- rownames(Cov) <- colnames(Cov) <- c("A", "F", "M")
  
  list(
    N_min = df_tmp$use_N,
    N_total_unique = N_total_unique,
    robust_estimates = robust_estimates,
    Cov = Cov,
    aCov = aCov,
    observed_variables = observed_variables
  )
})
# Name the subgroups list
names(subgroups) <- levels(df_es_wide$moderator)
save(subgroups, file = paste0(which_mod, "pooled.RData"))
