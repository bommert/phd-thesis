library(batchtools)
makeRegistry("SMS_undefined")

findq = function(p, m) {
  cdiff0 = function(p, m, q) {
    cmin1 = q^2 - p * (q - q %% p) - (q %% p)^2
    cmin2 = p * q * (m - 1)
    cmax1 = (q %% m)^2 + q * (m - 1) - (q %% m) * m
    cmax2 = q * (m - 1)
    eq = all.equal(cmax1 / cmax2, cmin1 / cmin2)
    return(eq == TRUE)
  }

  qs = 1:(m*p)
  cd0s = sapply(qs, cdiff0, p = p, m = m)
  return(qs[cd0s])
}

grid = expand.grid(p = as.numeric(2:10000), m = as.numeric(2:100))
batchMap(fun = findq, p = grid$p, m = grid$m)


# submit jobs and run this code after all jobs have completed
res = reduceResultsDataTable(findDone())
result = merge(res, unwrap(getJobTable()), by = "job.id")
result = result[, c("p", "m", "result"), with = FALSE]
save(result, file = "result_SMS_undefined.RData")


# evaluation of job results
load("result_SMS_undefined.RData")
hyp = mapply(p = result$p, m = result$m, result = result$result, function(p, m, result) {
  all.equal(c(1, p*m - 1, p*m), result)
})
summary(hyp)

