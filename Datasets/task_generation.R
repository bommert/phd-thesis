library(mlr)
library(Matrix)
library(BBmisc)

load("datasets.RData")

set.seed(2019)

# resample instances for nested 10 fold CV
iters = 10
rdesc = makeResampleDesc("CV", iters = iters, stratify = TRUE)

makeRins = function(task) {
  # outer CV
  rin = makeResampleInstance(rdesc, task)

  outer.test = lapply(1:iters, function(i) {
    subsetTask(task, subset = rin$test.inds[[i]])
  })

  outer.train = lapply(1:iters, function(i) {
    tk = subsetTask(task, subset = rin$train.inds[[i]])
    removeConstantFeatures(tk)
  })

  # inner CV
  inner = lapply(outer.train, function(tsk) {
    makeResampleInstance(rdesc, tsk)
  })

  return(list(outer.test = outer.test, outer.train = outer.train, inner = inner))
}

rins = lapply(datasets, makeRins)

save(rins, file = "rins.RData")



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

  nf = getTaskNFeats(task)
  sparse.mat = sparseMatrix(gt.mat[w, 1], gt.mat[w, 2], x = cc[gt[w]],
    symmetric = TRUE, dims = c(nf, nf))

  colnames(sparse.mat) = rownames(sparse.mat) = getTaskFeatureNames(task)
  return(sparse.mat)
}

sim.mats = lapply(rins, function(r) {
  lapply(r$outer.train, simMats)
})

save(sim.mats, file = "simmats.RData")
