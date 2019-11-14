library(metaSEM)
load("p_2clusterpooled.RData")
#or load("p_clusterpooled.RData")

# Name the subgroups list
#names(subgroups) <- gsub("[- ]", "", names(subgroups))

# Specify model in lavaan notation and parse into OpenMX matrices
RAM1 <- lavaan2RAM(
  c("A ~ F", "A ~ M", "F ~~ M", "F~~1*F", "M~~1*M"),
  obs.variables = subgroups[[1]]$observed_variables,
  A.notation = "on",
  S.notation = "with"
)

# Adjust matrices to estimate indirect effects
A1_ind <- gsub("^0\\*", "0\\.2\\*", RAM1$A)
S1_ind <- RAM1$S
diag(S1_ind) <-
  gsub("\\*(\\w)with\\w$", "\\.2\\*Error_\\1", diag(RAM1$S))


# Create model for all subgroups
subgroup_fits <- lapply(names(subgroups), function(this_subgroup) {
  #this_subgroup <- names(subgroups)[1]
  attach(subgroups[[this_subgroup]])
  RAM1$A[grepl("(\\*\\w+)$", RAM1$A)] <- paste0(RAM1$A[grepl("(\\*\\w+)$", RAM1$A)], this_subgroup)
  RAM1$S[grepl("(\\*\\w+)$", RAM1$S)] <- paste0(RAM1$S[grepl("(\\*\\w+)$", RAM1$S)], this_subgroup)
  on.exit(detach(subgroups[[this_subgroup]]))
  append_this <- this_subgroup
  wls_model <- wls(
    Cov,
    aCov,
    N_min,
    Amatrix = RAM1$A,
    Smatrix = RAM1$S,
    intervals = "LB",
    run = FALSE,
    model.name = this_subgroup
  )
})
names(subgroup_fits) <- names(subgroups)

# Create multigroup model
args <- c(list(model = "multigroup_model"), subgroup_fits, list(mxFitFunctionMultigroup(names(subgroups))))
mx_multigroup <- do.call(mxModel, args)

# Estimate multigroup model
fit_multigroup <- mxRun(mx_multigroup, intervals = TRUE)
summary(fit_multigroup)
