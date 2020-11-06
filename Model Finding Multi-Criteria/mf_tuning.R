library(data.table)
library(mco)

load("mf_results.RData")


replaceNames = function(name) {
  if (name == "Davis") "SMD-0"
  else if (name == "Dice") "SMD"
  else if (name == "Hamming") "SMH"
  else if (name == "IntersectionCount") "SMA-Count"
  else if (name == "IntersectionGreedy") "SMA-Greedy"
  else if (name == "IntersectionMBM") "SMA-MBM"
  else if (name == "IntersectionMean") "SMA-Mean"
  else if (name == "Jaccard") "SMJ"
  else if (name == "Kappa") "SMK"
  else if (name == "Lustgarten") "SML"
  else if (name == "Nogueira") "SMN"
  else if (name == "Novovicova") "SME"
  else if (name == "Ochiai") "SMO"
  else if (name == "Phi") "SMP"
  else if (name == "Somol") "SMS"
  else if (name == "Unadjusted") "SMU"
  else if (name == "Wald") "SMW"
  else if (name == "Yu") "SMY"
  else if (name == "Zucknick") "SMZ"
  else NA_character_
}

renameStab = function(name) {
  nms = strsplit(name, ".", fixed = TRUE)
  sapply(nms, function(x) {
    sn = strsplit(x[4], "stability")[[1]][2]
    paste(replaceNames(sn), x[2], sep = ".")
  })
}

considered.measures = c("SMU", "SMA-Count", "SMJ", "SMH")

result1 = result[, c("prob.name", "classif", "filter", "pars", "repl", "mmce.train", "mmce.test",
  "mean.size.train", "mean.size.test", "stability.train.value", "stability.test.value"), with = FALSE]
result2 = batchtools::unwrap(result1, sep = ".", cols = c("stability.train.value", "stability.test.value"))
stab.cols = grep("stability", colnames(result2))
colnames(result2)[stab.cols] = renameStab(colnames(result2)[stab.cols])

considered.measures.cols = unlist(lapply(considered.measures, grep, x = colnames(result2)))
choose.cols = c(setdiff(1:ncol(result2), stab.cols), considered.measures.cols)
result3 = result2[, choose.cols, with = FALSE]
result3 = cbind(result3, ID = 1:nrow(result3))

po = function(dat) {
  dat[, 3] = -dat[, 3]
  dat = as.matrix(dat)
  opt = paretoFilter(dat[, 1:3, drop = FALSE])
  opt = unique(opt)
  opt2 = merge(opt, dat)
  res = data.table(ID = opt2[, 4])
  return(res)
}

result4 = lapply(considered.measures, function(cm) {
  r = result3[, po(.SD), by = "prob.name",
    .SDcols = c("mmce.train", "mean.size.train", paste(cm, "train", sep = "."), "ID")]
  res = merge(result3, r)

  filters = strsplit(res$filter, "_")
  filters = sapply(filters, function(f) ifelse(length(f) == 1, f, f[2]))
  res$filter = factor(filters, levels = c("JMI", "MIM", "impurity", "none"))

  res$classif = factor(res$classif, levels = c("SVM", "KNN", "Ridge", "Lasso", "RF", "Boosting"))

  res$prob.name = factor(res$prob.name,
    levels = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
      "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti"))

  return(res)
})
names(result4) = considered.measures



# single criteria tuning
min.mmces = result3[, list(mmce.train = min(mmce.train)), by = "prob.name"]
single.crit = merge(result3, min.mmces)

filters = strsplit(single.crit$filter, "_")
filters = sapply(filters, function(f) ifelse(length(f) == 1, f, f[2]))
single.crit$filter = factor(filters, levels = c("JMI", "MIM", "impurity", "none"))
single.crit$classif = factor(single.crit$classif, levels = c("SVM", "KNN", "Ridge", "Lasso", "RF", "Boosting"))
single.crit$prob.name = factor(single.crit$prob.name,
  levels = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
    "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti"))



save(result4, considered.measures, single.crit, file = "mf_results_tuned.RData")
