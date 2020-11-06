library(BBmisc)
library(batchtools)
library(checkmate)
library(dplyr)

# enter directory of batchtools registry
# reg.dir = choose.dir()
# loadRegistry(reg.dir, writeable = TRUE, work.dir = getwd())

all = unwrap(getJobTable(findDone()))
ids.ranking = all[algorithm == "ranking", ]
ids.filter = all[algorithm == "filter", ]

#################################################################################
# filter
# reduce and aggregate in parts because of memory consumption

fun.filter = function(job, res) {
  data.table(
    algo.name = job$algo.name,
    prob.name = job$prob.name,
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    repl = job$repl,
    measures.tune = list(res$measures.tune),
    measures.test = list(res$measures.test)
  )
}

reduce.and.aggregate.filter = function(ids) {
  red.ids = findDone(ids)

  # for intermediate analyses while jobs are still running on cluster
  # exist.ids = file.exists(paste0(reg.dir, "/results/", red.ids$job.id, ".rds"))
  # red.ids = red.ids[exist.ids, ]

  res = reduceResultsDataTable(red.ids, fun = fun.filter)

  unwrap.cols = c("algo.pars", "prob.pars", "measures.test")

  res2 = lapply(res$result, function(r) {
    r2 = unwrap(r, cols = unwrap.cols, sep = ".")

    part = r2[, "measures.tune"][[1]][[1]]
    part = as.data.table(part)
    part = part[, list(mmce.tune = mmce, timetrain.tune = timetrain,
      timetrain.filter.tune = timetrain.filter, inner.iter = iter)]

    rest = r2[, -"measures.tune"]
    rests = lapply(seq_len(nrow(part)), function(i) rest)
    rest = bind_rows(rests)

    cbind(rest, part)
  })
  results = bind_rows(res2)


  # renaming
  colnames(results)[colnames(results) == "algo.pars.fw.method"] = "fw.method"
  colnames(results)[colnames(results) == "prob.pars.outer.iter"] = "outer.iter"
  colnames(results)[colnames(results) == "measures.test.mmce"] = "mmce.test"
  colnames(results)[colnames(results) == "measures.test.timetrain"] = "timetrain.test"
  colnames(results)[colnames(results) == "measures.test.timetrain.filter"] = "timetrain.filter.test"

  results = results[ , list(
    mmce.tune.mean = mean(mmce.tune),
    mmce.tune.median = median(mmce.tune),
    timetrain.tune.mean = mean(timetrain.tune),
    timetrain.tune.median = median(timetrain.tune),
    timetrain.filter.tune.mean = mean(timetrain.filter.tune),
    timetrain.filter.tune.median = median(timetrain.filter.tune),
    mmce.test = unique(mmce.test),
    timetrain.test = unique(timetrain.test),
    timetrain.filter.test = unique(timetrain.filter.test)
  ), by = c("algo.name", "prob.name", "fw.method", "repl", "outer.iter")]

  return(results)
}


tab.parts = ids.filter[, list(ids = list(job.id)), by = "problem"]

aggr.parts = lapply(1:nrow(tab.parts), function(i) {
  reduce.and.aggregate.filter(unlist(tab.parts$ids[i]))
})

results.filter = bind_rows(aggr.parts)


results.filter = results.filter[!fw.method %in% c("univariate.model.score", "party_cforest.importance"), ]

a = base::strsplit(results.filter$fw.method, split = "_")
a = lapply(a, function(x) x[length(x)])
results.filter$fw.method = unlist(a)

results.filter$fw.method[results.filter$fw.method == "symmetrical.uncertainty"] = "sym.uncert"
results.filter$fw.method[results.filter$fw.method == "information.gain"] = "info.gain"


save(results.filter, file = "results_filter.RData")


#################################################################################
# ranking

fun.ranking = function(job, res) {
  data.table(
    algo.name = job$algo.name,
    prob.name = job$prob.name,
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    scores = list(res)
  )
}

reduce.and.aggregate.ranking = function(ids) {
  ids = findDone(ids)
  res = reduceResultsDataTable(ids, fun = fun.ranking)
  res = bind_rows(res$result)
  results = unwrap(res , cols = c("algo.pars", "prob.pars"))
  results = results[, -"outer.iter"]
  return(results)
}

results.ranking = reduce.and.aggregate.ranking(ids.ranking)

results.ranking = results.ranking[!fw.method %in% c("univariate.model.score", "party_cforest.importance"), ]

a = base::strsplit(results.ranking$fw.method, split = "_")
a = lapply(a, function(x) x[length(x)])
results.ranking$fw.method = unlist(a)

results.ranking$fw.method[results.ranking$fw.method == "symmetrical.uncertainty"] = "sym.uncert"
results.ranking$fw.method[results.ranking$fw.method == "information.gain"] = "info.gain"


save(results.ranking, file = "results_ranking.RData")
