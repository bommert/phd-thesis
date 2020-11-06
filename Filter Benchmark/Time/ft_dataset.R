library(mlr)
load("datasets.RData")
set.seed(672894)

task = datasets$gina_agnostic
tgt = getTaskData(task, target.extra = TRUE)$target
n = getTaskNFeats(task)

inds.class.1 = which(tgt == tgt[1])
inds.class.2 = setdiff(seq_along(tgt), inds.class.1)

chosen.class.1 = sample(inds.class.1, n/2)
chosen.class.2 = sample(inds.class.2, n/2)
chosen = integer(n)
chosen[2 * (1:(n/2)) - 1] = chosen.class.1
chosen[2 * (1:(n/2))] = chosen.class.2

dataset = subsetTask(task, subset = chosen)
save(dataset, file = "dataset_ft.RData")
