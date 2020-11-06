library(batchtools)

# reg.dir = choose.dir()
# loadRegistry(reg.dir, writeable = TRUE, work.dir = getwd())

fun = function(job, res) {
  data.table(
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    repl = job$repl,
    time = list(res / 1e9))
}

results.time = reduceResultsDataTable(findDone(), fun = fun)
results.time = dplyr::bind_rows(results.time$result)
results.time = unwrap(results.time, cols = c("algo.pars", "prob.pars"))

results.time = results.time[, list(time = list(unlist(time))),
  by = c("fw.method", "fw.perc", "part.n", "part.p")]

a = base::strsplit(results.time$fw.method, split = "_")
a = lapply(a, function(x) x[length(x)])
results.time$fw.method = unlist(a)

results.time$fw.method[results.time$fw.method == "symmetrical.uncertainty"] = "sym.uncert"
results.time$fw.method[results.time$fw.method == "information.gain"] = "info.gain"

save(results.time, file = "results_time.RData")
