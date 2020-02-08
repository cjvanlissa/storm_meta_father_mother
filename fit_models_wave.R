library(metaSEM)
library(tidySEM)
load("same_wavepooled.RData")

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

Args <- c(list(model = "multigroup_model"), subgroup_fits, 
          list(
            mxFitFunctionMultigroup(names(subgroups)),
            mxAlgebra(AonMYes-AonFYes, name = "D_concurrent"),
            mxAlgebra(AonMNo-AonFNo, name = "D_predictive"),
            mxAlgebra(AonMYes-AonMNo, name = "D_Mcon_pred"),
            mxAlgebra(AonFYes-AonFNo, name = "D_Fcon_pred"),
            mxCI(c("D_concurrent", "D_predictive", "D_Mcon_pred", "D_Fcon_pred"))))
mx_multigroup_constraints <- do.call(mxModel, Args)

# Estimate multigroup model
fit_multigroup_constraints <- mxRun(mx_multigroup_constraints, intervals = TRUE)

results <- table_results(fit_multigroup_constraints, columns = NULL)[c("label", "est_sig", "se", "pvalue", "confint")]
results
write.csv(results, "results_wave.csv", row.names = FALSE)

# Make graph --------------------------------------------------------------
lay <- get_layout("M", "",
                  "", "A",
                  "F", "", rows = 3)
nodes <- data.frame(name = c("A", "M", "F"),
                    label = c("Child", "Mother", "Father"))

edges <- results[grepl("^[AF]", results$label), c("label", "est_sig")]
edges$from <- gsub("^.(on|with)(\\w).*$", "\\2", edges$label)
edges$to <- gsub("^(.)(on|with)(\\w).*$", "\\1", edges$label)
edges$group <- gsub("^.+(No|Yes)$", "\\1", edges$label)
edges$label <- edges$est_sig
edges$est_sig <- NULL

prep <- tidySEM::prepare_graph(edges = edges, nodes = nodes, layout = lay)
edges(prep)$arrow[seq(3, nrow(edges(prep)), by = 3)] <- "none"
edges(prep)$linetype <- rep(c(1,1,2), nrow(edges(prep))/3)

p <- plot(prep)

ggsave("figure_wave.png", p, width = 10/2, height = 2)