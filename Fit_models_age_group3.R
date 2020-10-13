#dubbelcheck moderator analyses for three age groups (As suggested by Reviewer 1), however this is a subgroup analysis 
#comparing young (5 studies), medium (13 studies) and Old (10) studies, which is not evenly distributed, and the graph 
#shows clearly two peaks!
library(metaSEM)
library(tidySEM)
library(ggplot2)
source("pool_correlation_matrices_robust_se.R")
set.seed(623919)
pool_correlation_matrices("age_group3")
load("pooled_age_group3.RData")

# Name the subgroups list
names(subgroups) <- gsub("[- ]", "", names(subgroups))

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


# Create multigroup model
Args <- c(list(model = "multigroup_model"), subgroup_fits, 
          list(
            mxFitFunctionMultigroup(names(subgroups)),
            mxAlgebra(AonMYoung-AonFYoung, name = "D_Young"),
            mxAlgebra(AonMMed-AonFMed, name = "D_Med"),
            mxAlgebra(AonMOld-AonFOld, name = "D_Old"),
            mxAlgebra(AonMOld-AonMYoung, name = "D_MOld_Young"),
            mxAlgebra(AonMMed-AonMYoung, name = "D_MMed_Young"),
            mxAlgebra(AonMOld-AonMMed, name = "D_MOld_Med"),
            mxAlgebra(AonFOld-AonFYoung, name = "D_FOld_Young"),
            mxAlgebra(AonFMed-AonFYoung, name = "D_FMed_Young"),
            mxAlgebra(AonFOld-AonFMed, name = "D_FOld_Med"),
            mxCI(c("D_Young", "D_Med", "D_Old", "D_MOld_Young", "D_MMed_Young","D_MOld_Med", "D_FOld_Young", "D_FMed_Young","D_FOld_Med"))))
mx_multigroup_constraints <- do.call(mxModel, Args)

# Estimate multigroup model
fit_multigroup_constraints <- mxRun(mx_multigroup_constraints, intervals = TRUE)

results <- table_results(fit_multigroup_constraints, columns = NULL)[c("label", "est_sig", "se", "pvalue", "confint")]


replace_these <- is.na(results$se)
ses <- results$se[replace_these] <- sapply(results$label[replace_these], mxSE, model = fit_multigroup_constraints)

results$pvalue[replace_these] <- 2*pnorm(abs(as.numeric(results$est_sig[replace_these])/ses), lower.tail=FALSE)
results$est_sig[replace_these] <- tidySEM::est_sig(results$est_sig[replace_these], sig = results$pvalue[replace_these])

write.csv(results, "results_age3.csv", row.names = FALSE)

# Make graph --------------------------------------------------------------

lay <- get_layout("M", "",
                  "", "A",
                  "F", "", rows = 3)
nodes <- data.frame(name = c("A", "M", "F"),
                    label = c("Child", "Mother", "Father"))

edges <- results[grepl("^[AF]", results$label), c("label", "est_sig")]
edges$from <- gsub("^.(on|with)(\\w).*$", "\\2", edges$label)
edges$to <- gsub("^(.)(on|with)(\\w).*$", "\\1", edges$label)
edges$group <- gsub("^.+(Old|Med|Young)$", "\\1", edges$label)
edges$label <- edges$est_sig
edges$est_sig <- NULL

prep <- tidySEM::prepare_graph(edges = edges, nodes = nodes, layout = lay)
edges(prep)$arrow[seq(3, nrow(edges(prep)), by = 3)] <- "none"
edges(prep)$linetype <- rep(c(1,1,2), nrow(edges(prep))/3)

p <- plot(prep)

ggsave("figure_age3.png", p, width = 10/2, height = 2)