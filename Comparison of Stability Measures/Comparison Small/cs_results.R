res = reduceResultsList(findDone(), fun = function(job, res) {
  data.table(
    prob.name = job$prob.name,
    i = job$prob.pars$i,
    j = job$prob.pars$j,
    features = list(res$features),
    value = list(res$values)
    )
})

result = dplyr::bind_rows(res)
result = unwrap(result, cols = "value")


save(result, file = "cs_results.RData")
