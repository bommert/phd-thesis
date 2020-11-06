res = reduceResultsList(findDone(), fun = function(job, res) {
  data.table(
    learner = job$algo.name,
    repl = job$repl,
    V1 = res[1],
    V2 = res[2],
    V3 = res[3],
    V4 = res[4],
    V5 = res[5],
    V6 = res[6],
    V7 = res[7],
    V8 = res[8],
    V9 = res[9],
    V10 = res[10],
    V11 = res[11],
    V12 = res[12],
    V13 = res[13],
    V14 = res[14],
    V15 = res[15]
    )
})

result = dplyr::bind_rows(res)

save(result, file = "fssf_results.RData")
