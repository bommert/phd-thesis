library(mlr)
library(Matrix)
library(BBmisc)

load("datasets.RData")

set.seed(2019)

iters = 10
rdesc.cv = makeResampleDesc("CV", iters = iters, stratify = TRUE)
rdesc.tt = makeResampleDesc("Holdout", split = 0.5, stratify = TRUE)

makeRins = function(task) {
  rin = makeResampleInstance(rdesc.tt, task)

  task.train = subsetTask(task, rin$train.inds[[1]])
  task.train = removeConstantFeatures(task.train)
  feats.train = getTaskFeatureNames(task.train)

  task.test = subsetTask(task, rin$test.inds[[1]])
  feats.test = getTaskFeatureNames(task.test)
  feats.diff = setdiff(feats.test, feats.train)
  if (length(feats.diff) > 0) {
    task.test = subsetTask(task.test, features = feats.train)
  }

  rin.train = makeResampleInstance(rdesc.cv, task.train)
  rin.test = makeResampleInstance(rdesc.cv, task.test)

  return(list(task.train = task.train, task.test = task.test,
    rin.train = rin.train, rin.test = rin.test))
}

rins = lapply(datasets, makeRins)

save(rins, file = "rins_halves.RData")



# list of sparse similarity matrices with threshold = 0.9
simMats = function(task, threshold = 0.9) {
  trafoIndex = function(index, nr) {
    i1 = ceiling(index / nr)
    i2 = index - (i1 - 1) * nr
    return(c(i1, i2))
  }

  dat = getTaskData(task, target.extra = TRUE)$data
  cc = abs(cor(dat))

  gt = which(cc >= threshold)
  gt.mat = convertListOfRowsToDataFrame(lapply(gt, trafoIndex, nr = nrow(cc)))
  w = which(gt.mat[, 1] >= gt.mat[, 2])

  sparse.mat = sparseMatrix(gt.mat[w, 1], gt.mat[w, 2], x = cc[gt[w]], symmetric = TRUE)

  colnames(sparse.mat) = rownames(sparse.mat) = getTaskFeatureNames(task)
  return(sparse.mat)
}

sim.mats = lapply(rins, function(r) {
  sim.mat.train = simMats(r$task.train)
  sim.mat.test = simMats(r$task.test)
  return(list(sim.mat.train = sim.mat.train, sim.mat.test = sim.mat.test))
})

save(sim.mats, file = "simmats_halves.RData")

