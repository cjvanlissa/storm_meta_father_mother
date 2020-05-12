pool_correlation_matrices <- function(which_mod = NULL){
  library(metaSEM)
  library(robumeta)
  library(clubSandwich)
  library(metafor)
  df_main <- read.csv("data_cleaned_mods.csv")
  #which_mod <- NULL
  
  #TEST
  if(!file.exists("table23.csv")){
    write.table(t(c("Moderator", "Level", "k", "#ESm_a","#ESm_f", "#ESf_a", "N")), "table23.csv", row.names = FALSE, col.names = FALSE, sep = ",")
  }
  names(df_main)[grep("^R", names(df_main))] <- c("R_m_a", "R_m_f", "R_f_a")
  df_main$ID_sample <- gsub(" ", "", paste0(df_main$ID, df_main$ID_subgroup))
  #length(table(df_main$ID_sample))
  
  # Select effect size data
  df_es_wide <-
    df_main[, c(
      "ID_sample",
      c("N", "N_m", "N_f"),
      c("R_m_a", "R_m_f", "R_f_a")
    )]
  
  if(is.null(which_mod)){
    df_es_wide$moderator <-
      factor(rep("Main", nrow(df_es_wide)))
    which_mod <- "Overall"
  } else {
    df_es_wide$moderator <-
      factor(df_main[[which_mod]])
  }
  
  df_es_wide <- df_es_wide[!is.na(df_es_wide$moderator), ]
  
  subgroups <- lapply(levels(df_es_wide$moderator), function(x) {
    # x <- levels(df_es_wide$moderator)[1]
    df_tmp <-
      df_es_wide[df_es_wide$moderator == x, -ncol(df_es_wide)]
    k <- length(unique(df_tmp$ID_sample))
    
    # Since A, M, F all have different N, use the minimal known N per sample ID
    min_Ns <- tapply(apply(df_tmp[, grep("^N", names(df_tmp)), ], 1, min, na.rm = TRUE), factor(df_tmp$ID_sample), min)
    
    rles <- rle(df_tmp$ID_sample)
    rles$values <- min_Ns[match(rles$values, names(min_Ns))]
    inverse.rle(rles)
    
    df_tmp$use_N <- inverse.rle(rles)
    total_N <- sum(df_tmp$use_N[!duplicated(df_tmp$ID_sample)])
    # Rename data for melting
    names(df_tmp) <-
      gsub("^R_", "R\\.", names(df_tmp))
    df_tmp <- df_tmp[, !grepl("^N", names(df_tmp))]
    num_es <- colSums(!is.na(df_tmp[,-1]))
    
    # Melt data.frame
    df_tmp <- reshape(
      df_tmp,
      direction = 'long',
      varying = grep("^R", names(df_tmp), value = TRUE),
      timevar = 'which_cor'
    )
    
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
    write.table(t(c(which_mod, x, k, num_es[1:3], total_N)), "table23.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    
    list(
      N_min = df_tmp$use_N,
      robust_estimates = robust_estimates,
      Cov = Cov,
      aCov = aCov,
      observed_variables = observed_variables,
      total_N = total_N
    )
  })
  # Name the subgroups list
  names(subgroups) <- levels(df_es_wide$moderator)
  save(subgroups, file = paste0("pooled_", which_mod, ".RData"))
}