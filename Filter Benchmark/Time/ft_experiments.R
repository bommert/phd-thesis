library(batchtools)
load("dataset_ft.RData")

reg = makeExperimentRegistry(file.dir = "ft", packages = c("mlr", "microbenchmark"))

partTask = function(data, job, part.p, part.n) {
  p = round(part.p * getTaskNFeats(data))
  n = round(part.n * getTaskSize(data))
  newtask = subsetTask(data, subset = 1:n, features = 1:p)
  return(newtask)
}


addProblem(name = "part", seed = 1,
  data = dataset,
  fun = partTask
)


addAlgorithm(
  name = "filter",
  fun = function(job, data, instance, fw.method, fw.perc) {
    source("microarray_filters.R")
    nselect = round(fw.perc * getTaskNFeats(instance))

    mb = microbenchmark(times = 10,
      generateFilterValuesData(task = instance, method = fw.method, nselect = nselect)
    )

    return(mb$time)
  }
)


############################################################################################
# Designs

# filters that we want to use
lf = listFilterMethods(tasks = TRUE, features = TRUE)
lf = lf[lf$task.classif & lf$feature.numerics, ]
filters.all = as.character(lf$id)

# exclude filters that are too slow or have a faster implementation
exclude = c("permutation.importance", "FSelector_relief", "FSelector_gain.ratio",
  "FSelector_information.gain", "FSelector_symmetrical.uncertainty", "univariate.model.score",
  "randomForest_importance", "randomForestSRC_var.select", "randomForestSRC_importance",
  "party_cforest.importance")
filters = setdiff(filters.all, exclude)
filters = union(filters, c("limma", "sam"))

percs1 = 1:10 / 10
percs2 = 1:9 / 10

design.algo1 = expand.grid(
  fw.method = filters,
  fw.perc = percs1,
  stringsAsFactors = FALSE
)

design.algo2 = expand.grid(
  fw.method = filters,
  fw.perc = 1,
  stringsAsFactors = FALSE
)


design.prob1 = expand.grid(
  part.p = 1,
  part.n = 1
)

design.prob2 = expand.grid(
  part.p = percs2,
  part.n = 1
)

design.prob3 = expand.grid(
  part.p = 1,
  part.n = percs2
)



addExperiments(
  algo.designs = list(filter = design.algo1),
  prob.designs = list(part = design.prob1),
  repls = 100)

addExperiments(
  algo.designs = list(filter = design.algo2),
  prob.designs = list(part = design.prob2),
  repls = 100)

addExperiments(
  algo.designs = list(filter = design.algo2),
  prob.designs = list(part = design.prob3),
  repls = 100)

