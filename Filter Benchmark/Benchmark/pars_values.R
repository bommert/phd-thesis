library(mlr)
set.seed(123)
source("paramset.R")

n.repls = 100
n.outer.iters = 10

pars.list = lapply(1:n.repls, function(i) replicate(n.outer.iters, sampleValue(par = ps, trafo = TRUE), simplify = FALSE))
save(pars.list, file = "pars_values.RData")
