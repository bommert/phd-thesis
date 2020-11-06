tab = unwrap(getJobTable())

res = reduceResultsList(findDone(), fun = function(job, res) {
  data.table(job.id = job$job.id, ranks = list(res))
})

result = dplyr::bind_rows(res)
result = merge(result, tab, by = "job.id")
result = result[, list(ranks, problem, fw.method, ntree, time.running)]
result$time.running = as.numeric(result$time.running, units = "secs")

save(result, file = "fs_results.RData")
