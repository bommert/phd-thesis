library(batchtools)
library(dplyr)
library(BBmisc)
library(mlr)

load("rins.RData")
load("datasets.RData")
reg = makeExperimentRegistry(file.dir = "fb", packages = "mlr")

###################################################################################
#### Definition of problems

extractOuterIter = function(job, data, outer.iter) {
  list(
    task.test = data$rins$outer.test[[outer.iter]],
    task.train = data$rins$outer.train[[outer.iter]],
    rin.train = data$rins$inner[[outer.iter]]
  )
}

for (i in seq_along(rins)) {
  addProblem(name = names(rins)[i], seed = i,
    data = list(rins = rins[[i]], task.all = datasets[[i]]),
    fun = extractOuterIter
  )
}

####################################################################################
### Definition of algorithms


# filter + classification
addAlgorithm(
  name = "filter",
  fun = function(job, data, instance, fw.method) {

    source("make_lrn.R")
    source("resampling.R")
    source("microarray_filters.R")
    load("pars_values.RData")

    pars = pars.list[[job$repl]][[job$prob.pars$outer.iter]]
    lrn = makeLrn(pars)

    if (fw.method != "none") {
      lrn = makeFilterWrapper(learner = lrn, fw.method = fw.method, fw.perc = pars$fw.perc)
    }

    res = resampling(lrn = lrn, instance = instance)
    return(res)
  }
)


# filter scores only
addAlgorithm(
  name = "ranking",
  fun = function(job, data, instance, fw.method) {

    source("microarray_filters.R")

    v = generateFilterValuesData(task = data$task.all, method = fw.method)
    scores = v$data[[fw.method]]

    return(scores)
  }
)



#####################################################################################
### Definition of designs

### Design for problems
design.tasks = data.frame(
  outer.iter = 1:10
)

design.tasks.ranking = data.frame(
  outer.iter = 1
)

design.probs = lapply(seq_along(datasets), function(i) design.tasks)
names(design.probs) = names(datasets)

design.probs.ranking = lapply(seq_along(datasets), function(i) design.tasks.ranking)
names(design.probs.ranking) = names(datasets)


### Desgin for algorithms


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
filters = c(filters, "limma", "sam")


design.ranking = expand.grid(
  fw.method = filters,
  stringsAsFactors = FALSE
)


design.filter = expand.grid(
  fw.method = c(filters, "none"),
  stringsAsFactors = FALSE
)

#################################################################################
### Add Experiments

addExperiments(
  algo.designs = list(ranking = design.ranking),
  prob.designs = design.probs.ranking,
  repls = 1)


addExperiments(
  algo.designs = list(filter = design.filter),
  prob.designs = design.probs,
  repls = 100)


ids.ranking = findExperiments(algo.name = "ranking")
ids.filter = findExperiments(algo.name = "filter")
