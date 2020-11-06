library(batchtools)

load("datasets.RData")
reg = makeExperimentRegistry(file.dir = "fs", packages = "mlr")

for (i in seq_along(datasets)) {
  addProblem(name = names(datasets)[i], seed = i, data = datasets[[i]])
}

addAlgorithm(
  name = "stoch",
  fun = function(job, data, instance, fw.method, ntree) {
    ranks = rank(generateFilterValuesData(data, fw.method, num.trees = ntree)$data[[fw.method]])
    return(ranks)
  }
)

design = expand.grid(
  fw.method = c("ranger_permutation", "ranger_impurity"),
  ntree = c(1, 2, 5, 10, 20, 50, 100) * 100,
  stringsAsFactors = FALSE
)

addExperiments(
  algo.designs = list(stoch = design),
  repls = 10
)
