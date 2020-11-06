library(batchtools)

# reg.dir = choose.dir()
# loadRegistry(reg.dir, writeable = TRUE, work.dir = getwd())

res = reduceResultsList(findDone(), fun = function(job, res) {
  data.table(
    prob.name = job$prob.name,
    classif = job$algo.name,
    filter = job$pars$algo.pars$fw.method,
    repl = job$repl,
    mmce.train = res$mmce.train,
    mmce.test = res$mmce.test,
    mean.size.train = res$mean.size.train,
    mean.size.test = res$mean.size.test,
    stability.train.value = list(res$stability.train$values),
    stability.train.time = list(res$stability.train$times),
    stability.test.value = list(res$stability.test$values),
    stability.test.time = list(res$stability.test$times),
    pars = list(res$pars)
  )
})

result = dplyr::bind_rows(res)
save(result, file = "mf_results.RData")
