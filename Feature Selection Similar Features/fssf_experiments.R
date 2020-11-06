library(batchtools)

makeExperimentRegistry(file.dir = "fssf", packages = c("mlr", "mvtnorm"),
  source = c("extractSelectedFeatures.R", "extractSelectedFeaturesInternal.R"))

addProblem(name = "normal", seed = 1,
  fun = function(job, data, n = 100, sd.e = 0.5) {

    mat = diag(15)
    mat[2:4, 2:4] = 1
    mat[2:4, 5] = mat[5, 2:4] = 0.999
    mat[2:4, 6] = mat[6, 2:4] = 0.9
    mat[2:4, 7] = mat[7, 2:4] = 0.75
    mat[2:4, 8] = mat[8, 2:4] = 0.5
    mat[5, 6] = mat[6, 5] = 0.9
    mat[5, 7] = mat[7, 5] = 0.75
    mat[5, 8] = mat[8, 5] = 0.5
    mat[6, 7] = mat[7, 6] = 0.7
    mat[6, 8] = mat[8, 6] = 0.5
    mat[7, 8] = mat[8, 7] = 0.3

    x = rmvnorm(n, sigma = mat)
    e = rnorm(n, sd = sd.e)
    y = as.factor(x[, 1] + x[, 2] + e > 0)

    data = cbind(as.data.frame(x), y)
    colnames(data) = c(paste0("V", 1:15), "y")

    task = makeClassifTask(data = data, target = "y")
    ctrl = makeTuneControlRandom(maxit = 50)
    rdesc = makeResampleDesc("CV", iters = 10)
    rin = makeResampleInstance(rdesc, task)
    return(list(task = task, ctrl = ctrl, rin = rin))
  }
)


algoLasso = function(job, data, instance) {
  source("helper.R")
  lrn = makeLearner("classif.glmnet", alpha = 1)
  ps = makeParamSet(
    makeNumericParam("s", lower = -15, upper = 15, trafo = function(x) 2^x)
  )
  ret = help_fun(instance, lrn, ps)
  return(ret)
}

algoLiblineaR = function(job, data, instance) {
  source("helper.R")
  lrn = makeLearner("classif.LiblineaRL1LogReg")
  ps = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x)
  )
  ret = help_fun(instance, lrn, ps)
  return(ret)
}

algoBoosting = function(job, data, instance) {
  source("helper.R")
  lrn = makeLearner("classif.glmboost")
  ps = makeParamSet(
    makeNumericParam("mstop", lower = 0, upper = 15, trafo = function(x) round(2^x))
  )
  ret = help_fun(instance, lrn, ps)
  return(ret)
}

algoRF = function(job, data, instance) {
  source("helper.R")
  lrn = makeLearner("classif.ranger", importance = "impurity")
  ps = makeParamSet(
    makeDiscreteParam("num.trees", values = c(1, 2, 5, 10, 20, 50) * 100),
    makeNumericParam("min.node.size", lower = 0, upper = 1,
      trafo = function(x) {
        n = getTaskSize(data$rins$task.train)
        r = round((n * 0.2) ^ x)
        max(r, 1)
      }),
    makeIntegerParam("mtry", lower = 1, upper = getTaskNFeats(instance$task))
  )
  ret = help_fun(instance, lrn, ps)
  return(ret)
}

algoL0 = function(job, data, instance) {
  source("helper.R")
  source("RLearner_classif_L0Learn.R")
  lrn = makeLearner("classif.L0Learn")
  ps = makeParamSet(
    makeIntegerParam("maxSuppSize", lower = 1, upper = 15)
  )
  ctrl = makeTuneControlGrid(resolution = 15)
  ret = help_fun(instance, lrn, ps, ctrl)
  return(ret)
}

addAlgorithm("Lasso", algoLasso)
addAlgorithm("Boosting", algoBoosting)
addAlgorithm("RF", algoRF)
addAlgorithm("LiblineaR", algoLiblineaR)
addAlgorithm("L0", algoL0)

addExperiments(repls = 1000)


