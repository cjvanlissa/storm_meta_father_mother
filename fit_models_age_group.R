library(metaSEM)
load("age_grouppooled.RData")

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


# Create multigroup model
Args <- c(list(model = "multigroup_model"), subgroup_fits, 
          list(
            mxFitFunctionMultigroup(names(subgroups)),
            mxAlgebra(AonMYoung-AonFYoung, name = "D_Young"),
            mxAlgebra(AonMOld-AonFOld, name = "D_Old"),
            mxAlgebra(AonMOld-AonMYoung, name = "D_MOld_Young"),
            mxAlgebra(AonFOld-AonFYoung, name = "D_FOld_Young"),
            mxCI(c("D_Young", "D_Old", "D_MOld_Young", "D_FOld_Young"))))
mx_multigroup_constraints <- do.call(mxModel, Args)

# Estimate multigroup model
fit_multigroup_constraints <- mxRun(mx_multigroup_constraints, intervals = TRUE)

results <- table_results(fit_multigroup_constraints, all = TRUE)[c("label", "est_sig", "se", "pvalue", "confint")]
results
write.csv(results, "results.csv", row.names = FALSE)

# Make graph --------------------------------------------------------------
# Ik weet niet wat de nummers betekenen, dus waarschijnlijk klopt het nog niet helemaal.
lay <- get_layout("M", "",
                  "", "A",
                  "F", "", rows = 3)
nodes <- data.frame(name = rep(c("A", "M", "F"), 3),
                    label = rep(c("Child", "Mother", "Father"), 3),
                    group = rep(c("Young", "Old"), each = 2))

edges <- results[grepl("^[AF]", results$label), c("label", "est_sig")]
edges$from <- gsub("^.(on|with)(\\w).*$", "\\2", edges$label)
edges$to <- gsub("^(.)(on|with)(\\w).*$", "\\1", edges$label)
edges$group <- gsub("^.+(control|negative|positive)$", "\\1", edges$label)
edges$label <- edges$est_sig
edges$est_sig <- NULL


prep <- prepare_graph(edges, lay, nodes, angle = 0)
edges(prep)$connect_from <- "right"
edges(prep)$connect_to <- "left"
edges(prep)[7:9, c("arrow", "connector", "connect_from", "connect_to", "curvature")] <- rep(c("none", "curve", "left", "left", .1), each = 3)

p <- plot(prep)
p

ggsave("figure_age.png", p, width = 10, height = 2)
