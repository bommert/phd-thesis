library(dplyr)
library(BBmisc)
library(data.table)

load("results_filter.RData")

results.filter$fw.method = factor(results.filter$fw.method,
  levels = c("anova.test", "limma", "sam", "kruskal.test", "chi.squared", "auc",
    "oneR", "variance","permutation", "impurity", "info.gain", "gain.ratio",
    "sym.uncert", "MIM", "MRMR", "JMI", "JMIM", "DISR", "NJMIM", "CMIM", "none"))

results.filter$prob.name = factor(results.filter$prob.name,
  levels = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
    "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti"))

##########################################################################################
# tuning: select best configuration (repl) based on accuracy
# solve ties by selecting the one with fastest run time

mmce.tune.best = results.filter[, list(mmce.tune.mean = min(mmce.tune.mean)),
  by = c("prob.name", "fw.method", "outer.iter")]

results.tuned = merge(results.filter, mmce.tune.best,
  by = intersect(colnames(results.filter), colnames(mmce.tune.best)))

# detect and resolve ties
ties = results.tuned[, .N, by = c("prob.name", "fw.method", "outer.iter")]
ties = ties[N > 1, ]
if (nrow(ties) > 0) {
  remove.rows.all = numeric(0)
  remove.rows.filter = numeric(0)
  set.seed(1223)

  for (i in 1:nrow(ties)) {
    ids = which(
      results.tuned$fw.method == ties$fw.method[i] &
        results.tuned$prob.name == ties$prob.name[i] &
        results.tuned$outer.iter == ties$outer.iter[i]
    )
    part = results.tuned[ids, ]

    min.time.all = min(part$timetrain.tune.median)
    min.time.filter = min(part$timetrain.filter.tune.median)

    min.part.id.all = which(part$timetrain.tune.median == min.time.all)
    min.part.id.filter = which(part$timetrain.filter.tune.median == min.time.filter)

    if (length(min.part.id.all) > 1) {
      min.part.id.all = sample(min.part.id.all, 1)
    }
    if (length(min.part.id.filter) > 1) {
      min.part.id.filter = sample(min.part.id.filter, 1)
    }

    remove.new.all = ids[-min.part.id.all]
    remove.new.filter = ids[-min.part.id.filter]

    remove.rows.all = c(remove.rows.all, remove.new.all)
    remove.rows.filter = c(remove.rows.filter, remove.new.filter)
  }
  results.tuned.all = results.tuned[-remove.rows.all, ]
  results.tuned.filter = results.tuned[-remove.rows.filter, ]
}

# when all filter experiments are done, there should be a 10 in each entry
table(results.tuned.all[, c("fw.method", "prob.name")], useNA = "ifany")
table(results.tuned.filter[, c("fw.method", "prob.name")], useNA = "ifany")

# aggregate the outer performance measures for each method
results.tuned.aggr.all = results.tuned.all[, list(
  mmce.test.mean = mean(mmce.test),
  timetrain.test.median = median(timetrain.test)),
  by = c("prob.name", "fw.method")]
results.tuned.aggr.filter = results.tuned.filter[, list(
  mmce.test.mean = mean(mmce.test),
  timetrain.filter.test.median = median(timetrain.filter.test)),
  by = c("prob.name", "fw.method")]

save(results.tuned.all, results.tuned.filter, results.tuned.aggr.all, results.tuned.aggr.filter,
  file = "results_tuning.RData")
