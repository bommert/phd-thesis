library(OpenML)
library(data.table)
library(mlr)

criteria = function(l) {
  l = l[number.of.symbolic.features == 1, ]
  l[, majority.class.perc := majority.class.size / number.of.instances]
  l = l[majority.class.perc <= 0.75, ]

  d = lapply(l$data.id, getOMLDataSet)
  tsk = lapply(d, convertOMLDataSetToMlr)

  n.fv = sapply(tsk, function(tk) {
    data = getTaskData(tk, target.extra = TRUE)$data
    n.diff.vals = apply(data, 2, function(x) length(table(x)))
    sum(n.diff.vals <= 5)
  })
  l[, n.few.different.values := n.fv]

  mns = lapply(tsk, function(tk) {
    kor = abs(cor(getTaskData(tk, target.extra = TRUE)$data))
    diag(kor) = 0
    n.sim = apply(kor, 1, function(x) sum(x >= 0.9))
    list(median = median(n.sim, na.rm = TRUE), mean = mean(n.sim, na.rm = TRUE))
  })
  mns = BBmisc::convertListOfRowsToDataFrame(mns)
  l[, median.n.sim := mns$median]
  l[, mean.n.sim := mns$mean]

  l = l[n.few.different.values / number.of.features <= 0.5, ]

  return(l)
}

criteria2 = function(l) {
  l = l[number.of.symbolic.features == 1, ]

  d = lapply(l$data.id, getOMLDataSet)
  tsk = lapply(d, function(xd) {
    tk = convertOMLDataSetToMlr(xd)
    if (length(getTaskClassLevels(tk)) > 2) {
      data = getTaskData(tk)
      target = getTaskTargetNames(tk)
      tab = table(data[[target]])
      tab = sort(tab, decreasing = TRUE)
      data.part = data[data[[target]] %in% names(tab)[1:2], ]
      data.part[[target]] = droplevels(data.part[[target]])
      tk = makeClassifTask(id = getTaskId(tk), data = data.part, target = target)
      tk = removeConstantFeatures(tk)
    }
    return(tk)
  })

  ni = sapply(tsk, getTaskSize)
  l[, number.of.instances := ni]

  l[, majority.class.perc := majority.class.size / number.of.instances]
  l = l[majority.class.perc <= 0.75, ]

  n.fv = sapply(tsk, function(tk) {
    data = getTaskData(tk, target.extra = TRUE)$data
    n.diff.vals = apply(data, 2, function(x) length(table(x)))
    sum(n.diff.vals <= 5)
  })
  l[, n.few.different.values := n.fv]

  mns = lapply(tsk, function(tk) {
    kor = abs(cor(getTaskData(tk, target.extra = TRUE)$data))
    diag(kor) = 0
    n.sim = apply(kor, 1, function(x) sum(x >= 0.9))
    list(median = median(n.sim, na.rm = TRUE), mean = mean(n.sim, na.rm = TRUE))
  })
  mns = BBmisc::convertListOfRowsToDataFrame(mns)
  l[, median.n.sim := mns$median]
  l[, mean.n.sim := mns$mean]

  l = l[n.few.different.values / number.of.features <= 0.5, ]

  return(l)
}

###################################################################
##################### Scenario 1 ##################################
###################################################################

ld1 = listOMLDataSets(
  number.of.classes = 2,
  number.of.missing.values = 0,
  number.of.features = c(50, 500) + 1,
  number.of.instances = c(50, 3000)
)
l1 = as.data.table(ld1)
l1 = l1[-grep("fri_", name), ]

l1 = l1[number.of.instances >= number.of.features, ]
l1 = criteria(l1)

real.data = data.table(
  name = c("sonar", "spambase", "tecator", "analcatdata_authorship",
    "kc1-binary", "hill-valley", "madelon", "clean1", "madeline", "philippine"),
  # madeline and philippine come from automl challenge
  real = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, NA, NA)
)

l1 = merge(l1, real.data)
l1 = l1[is.na(real) | real == TRUE, ]

l1

###################################################################
##################### Scenario 2 ##################################
###################################################################

ld2 = listOMLDataSets(
  number.of.classes = 2,
  number.of.missing.values = 0,
  number.of.features = c(501, 5000) + 1,
  number.of.instances = c(502, 5000)
)

l2 = as.data.table(ld2)
l2 = l2[number.of.instances >= number.of.features, ]
l2 = criteria(l2)
l2

# take 2 largest classes
ld2a = listOMLDataSets(
  number.of.missing.values = 0,
  number.of.features = c(501, 5000) + 1
)

l2a = as.data.table(ld2a)
l2a = l2a[number.of.classes > 2, ]
l2a = l2a[number.of.instances >= number.of.features, ]
l2a = l2a[-grep(".wc", name), ] # no data

l2a = criteria2(l2a)
l2a = l2a[number.of.instances >= number.of.features & number.of.instances <= 5000, ]
l2a

###################################################################
##################### Scenario 3 ##################################
###################################################################

ld3 = listOMLDataSets(
  number.of.classes = 2,
  number.of.missing.values = 0,
  number.of.features = c(1, 3000) + 1,
  number.of.instances = c(101, 3000)
)

l3 = as.data.table(ld3)
l3 = l3[number.of.instances < number.of.features, ]
l3 = l3[-grep("fri_", name), ]
l3 = criteria(l3)

# remove dataset with spatial correlations
l3 = l3[name != "wind_correlations", ]
l3

# take 2 largest classes
ld3a = listOMLDataSets(
  number.of.missing.values = 0,
  number.of.features = c(1, 3000) + 1
)

l3a = as.data.table(ld3a)
l3a = l3a[number.of.classes > 2, ]
l3a = l3a[-grep(".wc", name), ] # no data
l3a = l3a[majority.class.size >= 50 & majority.class.size < number.of.features, ]

l3a = criteria2(l3a)
l3a = l3a[number.of.instances > 100, ]
l3a = l3a[number.of.features > number.of.instances, ]
l3a

###################################################################
##################### Scenario 4 ##################################
###################################################################

ld4 = listOMLDataSets(
  number.of.classes = 2,
  number.of.missing.values = 0,
  number.of.features = c(3001, 15000) + 1,
  number.of.instances = c(101, 15000)
)

l4 = as.data.table(ld4)
l4 = l4[number.of.instances < number.of.features, ]
l4 = criteria(l4)

l4

# take 2 largest classes
ld4a = listOMLDataSets(
  number.of.missing.values = 0,
  number.of.features = c(3001, 15000) + 1
)

l4a = as.data.table(ld4a)
l4a = l4a[number.of.classes > 2, ]
l4a = l4a[-grep(".wc", name), ] # no data
l4a = l4a[majority.class.size >= 50 & majority.class.size < number.of.features, ]

l4a = criteria2(l4a)
l4a = l4a[number.of.instances > 100, ]
l4a = l4a[number.of.features > number.of.instances, ]
l4a

##################################################################

library(datamicroarray)

# data from package datamicroarray
# we cannot rely on the information of describe_data() for the moment
dd = describe_data()
ds = as.character(dd$author)
data(list = ds)

ds.info = lapply(ds, function(d) {
  x = get(d)
  tab = table(x$y)
  missing.values = apply(x$x, 1, function(a) sum(is.na(a)))

  majority.class.size = max(tab)
  second.class.size = sort(tab, decreasing = TRUE)[2]

  # take only the two largest classes
  list(
    name = d,
    number.of.classes = length(tab),
    number.of.features = ncol(x$x),
    number.of.instances = majority.class.size + second.class.size,
    number.of.instances.with.missing.values = sum(missing.values > 0),
    number.of.missing.values = sum(missing.values),
    majority.class.perc = majority.class.size / (majority.class.size + second.class.size),
    max = max(x$x),
    min = min(x$x)
  )
})

ds.info = BBmisc::convertListOfRowsToDataFrame(ds.info, strings.as.factors = FALSE)
l5 = as.data.table(ds.info)

l5 = l5[number.of.features <= 15000, ]
l5 = l5[number.of.instances > 100, ]
l5 = l5[majority.class.perc <= 0.75, ]
l5 = l5[max < 15 | min > 0, ]

mns = lapply(l5$name, function(d) {
  x = get(d)$x
  y = get(d)$y
  tab = table(y)
  tab = sort(tab, decreasing = TRUE)
  part = which(y %in% names(tab)[1:2])
  dat = as.matrix(x[part, ])

  kor = abs(cor(dat))
  diag(kor) = 0
  n.sim = apply(kor, 1, function(x) sum(x >= 0.9))
  list(median = median(n.sim), mean = mean(n.sim))
})
mns = BBmisc::convertListOfRowsToDataFrame(mns)
l5[, median.n.sim := mns$median]
l5[, mean.n.sim := mns$mean]

n.fv = sapply(l5$name, function(d) {
  x = get(d)$x
  y = get(d)$y
  tab = table(y)
  tab = sort(tab, decreasing = TRUE)
  part = which(y %in% names(tab)[1:2])
  data = as.matrix(x[part, ])

  n.diff.vals = apply(data, 2, function(x) length(table(x)))
  sum(n.diff.vals <= 5)
})
l5[, n.few.different.values := n.fv]

l5



###########################################################################
########################## Tables #########################################
###########################################################################

l = dplyr::bind_rows(l1, l2, l2a, l3, l3a, l4, l4a, l5)
l = l[, c("name", "data.id", "number.of.classes", "number.of.features",
  "number.of.instances", "majority.class.perc", "n.few.different.values",
  "median.n.sim", "mean.n.sim"), with = FALSE]
l$data.id[is.na(l$data.id)] = "datamicroarray"

xtab = xtable::xtable(l)
xtable::print.xtable(xtab, include.rownames = FALSE)

chosen = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
  "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti")
l = l[name %in% chosen, ]

save(l, file = "data_ids.RData")



############################################################################
####################### Transformations ####################################
############################################################################

dma = c("christensen", "gravier", "chiaretti")

data.x = lapply(dma, function(d) {
  x = get(d)$x
  y = get(d)$y
  tab = table(y)
  tab = sort(tab, decreasing = TRUE)
  part = which(y %in% names(tab)[1:2])
  as.matrix(x[part, ])
})

par(mfcol = c(2, 3))
for (i in seq_along(dma)) {
  hist(data.x[[i]], main = paste(dma[i], "(original scale)"))
  hist(log(data.x[[i]]), main = paste(dma[i], "(log scale)"))
}

# log: chiaretti


