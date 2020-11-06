library(OpenML)
library(mlr)
library(data.table)
library(datamicroarray)
load("data_ids.RData")

###########################################################################
##################### Datasets and final table ############################
###########################################################################

chosen.oml = c("kc1-binary", "sonar", "tecator", "gina_agnostic", "har",
  "dilbert", "lsvt", "arcene", "eating")
l.chosen = l[name %in% chosen.oml, ]

datasets = lapply(as.numeric(l.chosen$data.id), getOMLDataSet)
datasets = lapply(datasets, convertOMLDataSetToMlr)
names(datasets) = chosen.oml

data.subset.classes = function(task, classes) {
  target = getTaskData(task, target.extra = TRUE)$target
  keep = target %in% classes
  data = getTaskData(task)
  data = data[keep, ]
  data[[getTaskTargetNames(task)]] = droplevels(data[[getTaskTargetNames(task)]])
  ret = makeClassifTask(id = getTaskId(task), data = data, target = getTaskTargetNames(task))
  return(ret)
}

datasets$har = data.subset.classes(datasets$har, c("4", "5"))
datasets$dilbert = data.subset.classes(datasets$dilbert, c("1", "3"))
datasets$eating = data.subset.classes(datasets$eating, c("Apple", "Nectarine"))

christensen = function() {
  data("christensen")
  y = christensen$y
  y[y == "placenta"] = "other"
  y = droplevels(y)
  y2 = factor(y, levels = c("blood", "other"))
  data = as.data.frame(christensen$x)
  colnames(data) = paste0("V", 1:ncol(data))
  data = cbind(data, data.frame(y = y))
  task = makeClassifTask(id = "christensen", data = data, target = "y")
  return(task)
}

gravier = function() {
  data("gravier")
  data = as.data.frame(gravier$x)
  colnames(data) = paste0("V", 1:ncol(data))
  data = cbind(data, data.frame(y = gravier$y))
  task = makeClassifTask(id = "gravier", data = data, target = "y")
  return(task)
}

chiaretti = function() {
  library(ALL)
  data("ALL")

  x = t(exprs(ALL))
  x = log(x)
  y = as.character(ALL$BT)
  y[grep("B", y)] = "B"
  y[grep("T", y)] = "T"

  data = as.data.frame(x)
  colnames(data) = paste0("V", 1:ncol(data))
  data = cbind(data, data.frame(y = y))
  task = makeClassifTask(id = "chiaretti", data = data, target = "y")
  return(task)
}

datasets$christensen = christensen()
datasets$gravier = gravier()
datasets$chiaretti = chiaretti()

datasets = lapply(datasets, removeConstantFeatures)

# rename classes
renameClasses = function(task, old.class.names, new.class.names) {
  dat = getTaskData(task)
  levs = levels(dat[[getTaskTargetNames(task)]])

  if (!identical(levs, old.class.names)) {
    if (identical(levs, old.class.names[2:1])) {
      new.class.names = new.class.names[2:1]
    } else {
      warning("Old class names did not match!")
      return(task)
    }
  }

  levels(dat[[getTaskTargetNames(task)]]) = new.class.names
  pos = new.class.names[which(levs == task$task.desc$positive)]
  task2 = makeClassifTask(id = getTaskId(task), data = dat,
    target = getTaskTargetNames(task), positive = pos)
  return(task2)
}

datasets$`kc1-binary` = renameClasses(datasets$`kc1-binary`, c("_TRUE", "FALSE"), c("Defective", "Working"))
datasets$tecator = renameClasses(datasets$tecator, c("P", "N"), c("Positive", "Negative"))
datasets$har = renameClasses(datasets$har, c("4", "5"), c("Sitting", "Standing"))
datasets$lsvt = renameClasses(datasets$lsvt, c("1", "2"), c("Acceptable", "Unacceptable"))
datasets$christensen = renameClasses(datasets$christensen, c("blood", "other"), c("Blood", "Other"))
datasets$gravier = renameClasses(datasets$gravier, c("good", "poor"), c("Good", "Poor"))
datasets$arcene = renameClasses(datasets$arcene, c("1", "2"), c("Cancer", "Control"))

# order
datasets = datasets[c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
  "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti")]


###############

info = lapply(datasets, function(d) {
  name = getTaskId(d)
  p = getTaskNFeats(d)
  n = getTaskSize(d)
  perc = max(getTaskDesc(d)$class.distribution) / n
  data = getTaskData(d, target.extra = TRUE)$data
  kor = abs(cor(data))
  diag(kor) = 0
  n.sim = apply(kor, 1, function(x) sum(x >= 0.9))
  res = list(name = name, p = p, n = n, perc = perc,
    median.n.sim = median(n.sim), mean.n.sim = mean(n.sim))
  return(res)
})
info = BBmisc::convertListOfRowsToDataFrame(info)
info = merge(info, l[, c("name", "data.id"), with = FALSE], sort = FALSE)

xtab = xtable::xtable(info)
xtable::print.xtable(xtab, include.rownames = FALSE)

save(datasets, info, file = "datasets.RData")

