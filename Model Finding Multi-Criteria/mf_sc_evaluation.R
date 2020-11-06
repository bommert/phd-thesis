library(data.table)
library(ggplot2)

load("mf_results.RData")
load("datasets.RData")

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
  else if (name == "Sechidis") "SMES"
  else if (name == "Somol") "SMS"
  else if (name == "Unadjusted") "SMU"
  else if (name == "Wald") "SMW"
  else if (name == "Yu") "SMY"
  else if (name == "Zucknick") "SMZ"
  else NA_character_
}

n.feats = lapply(datasets, mlr::getTaskNFeats)
n.feats = data.table(prob.name = names(n.feats), n.feats = unlist(n.feats))

result2 = result[, c("prob.name", "classif", "filter", "mmce.train", "mean.size.train",
  "stability.train.time", "stability.train.value", "repl"), with = FALSE]
result2 = merge(result2, n.feats, by = "prob.name")
result2[, mean.perc.train := mean.size.train / n.feats]
result2$prob.name = factor(result2$prob.name,
  levels = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
    "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti"))

result3 = batchtools::unwrap(result2, sep = ".")
result4 = melt(result3, id.vars = c("prob.name", "classif", "filter", "repl", "n.feats", "mean.perc.train"),
  variable.factor = FALSE)


####################### correlations ###############################
res = split(result3, result3$prob.name)
res = lapply(res, function(r) {
  ret = r[, grep("value", colnames(r)), with = FALSE]
  tmp = sapply(strsplit(colnames(ret), ".stability"), function(x) x[2])
  colnames(ret) = sapply(tmp, replaceNames)
  return(ret)
})


cors = lapply(res, cor, use = "pairwise.complete.obs")

mean.cors = Reduce("+", cors) / length(cors)
o = hclust(as.dist(1 - mean.cors), method = "average")$order
cn = colnames(cors[[1]])
levs = cn[o]

sort(rowMeans(mean.cors[c("SME", "SMD", "SMO", "SMD-0", "SMJ", "SMZ"),
  c("SME", "SMD", "SMO", "SMD-0", "SMJ", "SMZ")]))


plot_mat = function(dat, names, title = "") {
  colnames(dat) = rownames(dat) = names
  plt.data = cbind(measure = names, as.data.frame(dat))
  plt.data = reshape2::melt(plt.data, id.vars = "measure")
  plt.data$measure = factor(plt.data$measure, levels = names)
  plt.data$variable = factor(plt.data$variable, levels = names)

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "darkblue", mid = "white",
      high = "darkred", limits = c(-1, 1), name = "Similarity") +
    theme_grey() +
    labs(x = element_blank(), y = element_blank()) +
    scale_x_discrete(expand = c(0, 0), labels = names) +
    scale_y_discrete(expand = c(0, 0), labels = names) +
    theme(axis.ticks = element_blank()) +
    coord_equal(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12))

  if (title != "") {
    gg = gg +
      ggtitle(title) +
      theme(title = element_text(size = 13))
  } else {
    gg = gg + geom_text(mapping = aes(color = abs(value) >= 0.75,
      label = sprintf("%.2f", round(value, 2))),
      size = 2.5) +
      scale_color_manual(guide = FALSE, values = c("black", "white"))
  }

  return(gg)
}

gg.cors = lapply(seq_along(cors), function(i) {
  plot_mat(cors[[i]][o, o], names = levs, title = names(cors)[i])
})



pdf("measures_cors_cr.pdf", height = 6, width = 7)
tmp = lapply(gg.cors, print)
dev.off()

pdf("measures_cors_mean_cr.pdf", height = 6, width = 7)
plot_mat(mean.cors[o, o], names = levs, title = "")
dev.off()


###################### stability size ###############################
result.value = result4[grep("value", result4$variable), ]
result.value$variable = sapply(strsplit(result.value$variable, ".stability"), function(x) x[2])
result.value$variable = sapply(result.value$variable, replaceNames)
result.value$variable = factor(result.value$variable, levels = levs)

png("measures_size_cr.png", height = 9.5, width = 8, units = "in", res = 1000)
ggplot(data = result.value, mapping = aes(x = mean.perc.train, y = value)) +
  geom_point() +
  facet_wrap("variable", scales = "free_y", ncol = 4) +
  ylab("Stability Value") +
  xlab("Mean Proportion of Selected Features") +
  theme_bw()
dev.off()

#################### scatter plots ####################################
result.value2 = result.value
colnames(result.value2)[ncol(result.value2) - 1:0] = c("variable2", "value2")
result.value.m = merge(result.value, result.value2,
  by = intersect(colnames(result.value), colnames(result.value2)), allow.cartesian = TRUE)

result.value.m$variable = factor(result.value.m$variable, levels = levs)
result.value.m$variable2 = factor(result.value.m$variable2, levels = levs)
result.vms = split(result.value.m, result.value.m$prob.name)

interesting.measures = c("SMJ", "SMH", "SMA-Count", "SMU")

gg.scatter.fun = function(dat, title = FALSE) {
  gg = ggplot(data = dat, mapping = aes(x = value, y = value2, color = mean.perc.train)) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1.1) +
    geom_point() +
    facet_grid(variable2 ~ variable, scales = "free") +
    theme_bw() +
    xlab("Stability Value") + ylab("Stability Value") +
    theme(legend.position = "bottom", legend.key.width = grid::unit(1, "cm")) +
    scale_color_gradient(name = "Mean Proportion of Selected Features") +
    scale_x_continuous(labels = function(x) sprintf("%.1f", x))
  if (title) {
    gg = gg +  ggtitle(dat$prob.name[1])
  }
  return(gg)
}

gg.scatter = lapply(result.vms, function(r) {
  r = r[variable %in% interesting.measures & variable2 %in% interesting.measures, ]
  gg.scatter.fun(r, title = TRUE)
})

pdf("measures_scatter_cr.pdf", height = 8, width = 8)
tmp = lapply(gg.scatter, print)
dev.off()

result.value.m2 = result.value.m[variable %in% interesting.measures & variable2 %in% interesting.measures, ]
gg.scatter.all = gg.scatter.fun(result.value.m2)

png("measures_scatter_all_cr.png", height = 8, width = 8, units = "in", res = 1000)
print(gg.scatter.all)
dev.off()


########################### run times ############################
result.time = result4[grep("time", result4$variable), ]
result.time$variable = sapply(strsplit(result.time$variable, ".stability"), function(x) x[2])
result.time$variable = sapply(result.time$variable, replaceNames)
result.time$variable = factor(result.time$variable, levels = levs)

result.time.adjusted = result.time[variable %in% c("SMY", "SMA-Greedy", "SMA-MBM", "SMA-Count", "SMA-Mean"), ]

gg.rt1 = ggplot(data = result.time, mapping = aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap("prob.name", scales = "free_y", ncol = 3) +
  scale_y_log10() +
  xlab("Stability Measure") +
  ylab("Run Time (in Seconds)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

gg.rt2 = ggplot(data = result.time.adjusted, mapping = aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap("prob.name", scales = "free_y", ncol = 3) +
  xlab("Stability Measure") +
  ylab("Run Time (in Seconds)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


pdf("measures_runtimes.pdf", height = 10, width = 8)
print(gg.rt1)
print(gg.rt2)
dev.off()
