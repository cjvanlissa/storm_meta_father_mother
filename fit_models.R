library(metaSEM)
library(tidySEM)
source("pool_correlation_matrices_robust_se.R")
pool_correlation_matrices()
load("Overallpooled.RData")
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
  attach(subgroups[[this_subgroup]])
  on.exit(detach(subgroups[[this_subgroup]]))
  append_this <- this_subgroup
  wls_model <- wls(
    Cov,
    aCov,
    total_N,
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
            mxAlgebra(AonM-AonF, name = "D_parenting"),
            mxCI(c("D_parenting"))))
mx_multigroup_constraints <- do.call(mxModel, Args)

# Estimate multigroup model
fit_multigroup_constraints <- mxRun(mx_multigroup_constraints, intervals = TRUE)

results <- table_results(fit_multigroup_constraints, columns = NULL)[c("label", "est_sig", "se", "pvalue", "confint")]

replace_these <- is.na(results$se)
ses <- results$se[replace_these] <- sapply(results$label[replace_these], mxSE, model = fit_multigroup_constraints)

results$pvalue[replace_these] <- 2*pnorm(abs(as.numeric(results$est_sig[replace_these])/ses), lower.tail=FALSE)
results$est_sig[replace_these] <- tidySEM::est_sig(results$est_sig[replace_these], sig = results$pvalue[replace_these])

write.csv(results, "results_overall.csv", row.names = FALSE)

# Make graph --------------------------------------------------------------
# Ik weet niet of dit wel klopt, zo zonder groepen. 
lay <- get_layout("M", "",
                  "", "A",
                  "F", "", rows = 3)
nodes <- data.frame(name = c("A", "M", "F"),
                    label = c("Child", "Mother", "Father"))

edges <- results[grepl("^[AF]", results$label), c("label", "est_sig")]
edges$from <- gsub("^.(on|with)(\\w).*$", "\\2", edges$label)
edges$to <- gsub("^(.)(on|with)(\\w).*$", "\\1", edges$label)
#edges$group <- gsub("^.+(control|negative|positive)$", "\\1", edges$label)
edges$label <- edges$est_sig
edges$est_sig <- NULL
tidySEM::graph(edges = edges, nodes = nodes, layout = lay)
prep <- tidySEM::prepare_graph(edges = edges, nodes = nodes, layout = lay)
#edges(prep)$connect_from <- "right"
#edges(prep)$connect_to <- "left"
edges(prep)[3, c("arrow", "curvature")] <- c("none", NA)
edges(prep)$linetype <- c(1,1,2)

p <- plot(prep)
p

ggsave("figure_overall.png", p, width = 10/3, height = 2)

