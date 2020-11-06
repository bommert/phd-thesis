library(data.table)
library(ggplot2)
library(cowplot)

load("mf_results_tuned.RData")

result5 = dplyr::bind_rows(lapply(seq_along(considered.measures), function(i) {
  r = result4[[i]][, c("prob.name", "classif", "filter", "ID", "mmce.train", "mmce.test",
    "mean.size.train", "mean.size.test",
    paste(considered.measures[i], c("train", "test"), sep = ".")), with = FALSE]
  colnames(r)[ncol(r) - 1:0] = c("stability.train", "stability.test")
  cbind(r, measure = considered.measures[i])
}))
result5$measure = factor(result5$measure, levels = considered.measures)

min.mmces = result5[, list(mmce.train.min = min(mmce.train)), by = "prob.name"]
result5a = merge(result5, min.mmces)

# PA-best configurations per measure
result6 = result5a[mmce.train <= mmce.train.min + 0.05, ]
result6s = split(result6, result6$measure)

result7 = result6[, opt := ifelse(mmce.train == mmce.train.min,
  "PA-best \nand A-best",
  "PA-best \n(not A-best)")]
result7 = result7[, accuracy.test := 1 - mmce.test]
result7 = result7[, accuracy.train := 1 - mmce.train]
result7s = split(result7, result7$prob.name)

# add A-best methods that are not among PA-best
single.crit = single.crit[, accuracy.train := 1 - mmce.train]
single.crit = single.crit[, accuracy.test := 1 - mmce.test]

result7sa = split(result7, result7$measure)

result7sb = lapply(result7sa, function(r) {
  miss = setdiff(single.crit$ID, r$ID)
  if (length(miss) > 0) {
    sc = single.crit
    m = r$measure[1]
    colnames(sc)[colnames(sc) == paste0(m, ".train")] = "stability.train"
    colnames(sc)[colnames(sc) == paste0(m, ".test")] = "stability.test"
    sc = sc[, c("prob.name", "classif", "filter", "ID",  "mean.size.train", "mean.size.test",
      "stability.train", "stability.test", "accuracy.train", "accuracy.test"), with = FALSE]
    sc = sc[, measure := m]
    sc = sc[, opt := "A-best\n(not PA-best)"]
    r = rbind(r, sc[ID %in% miss, ], fill = TRUE)
  }
  return(r)
})

result7a = dplyr::bind_rows(result7sb)
result7a$opt = factor(result7a$opt,
  levels = c("PA-best \n(not A-best)",
    "PA-best \nand A-best",
    "A-best\n(not PA-best)"))
result7as = split(result7a, result7a$prob.name)

result7as2 = split(result7a, result7a$opt)
pa.best = rbind(result7as2$`PA-best \n(not A-best)`,
  result7as2$`PA-best \nand A-best`)
pa.best$opt = "PA-best"

a.best = rbind(result7as2$`A-best\n(not PA-best)`,
  result7as2$`PA-best \nand A-best`)
# remove duplicates
a.best = a.best[measure == considered.measures[1], ]
a.best$measure = NA_character_
a.best$opt = "A-best"

result7b = rbind(pa.best, a.best)
result7b$opt = droplevels(result7b$opt)


result8 = result7b[, accuracy.diff := accuracy.test - accuracy.train]
result8 = result8[, stability.diff := stability.test - stability.train]
result8 = result8[, mean.size.diff := mean.size.test - mean.size.train]

result9 = melt(result8, id.vars = c("prob.name", "classif", "filter", "measure", "opt"),
  measure.vars = c("accuracy.diff", "stability.diff", "mean.size.diff"))
result9a = result9[, opt2 := paste0(opt, "\n(", measure, ")")]
result9a[opt == "A-best"]$opt2 = "A-best"
result9a$opt2 = factor(result9a$opt2,
  levels = c(paste0("PA-best\n(", considered.measures, ")"), "A-best"))


##############################################################################################

# comparison of optimal configurations between measures
plot_bpm = function(config.list, title) {
  config.list = rev(config.list)
  all.configs = Reduce(union, config.list)

  mat = Reduce(rbind, lapply(config.list, function(f) all.configs %in% f))
  colnames(mat) = NULL
  rownames(mat) = NULL

  if (length(all.configs) > 1) {
    d.configs = dist(t(mat), method = "manhattan")
    hc.configs = hclust(d.configs, method = "average")
    o.configs = hc.configs$order
  } else {
    o.configs = 1
  }

  mat = mat[, o.configs, drop = FALSE]
  colnames(mat) = paste0("V", all.configs[o.configs])
  rownames(mat) = paste0("S", 1:length(config.list))

  mat.data = reshape2::melt(mat, id.vars = rownames(mat))
  colnames(mat.data) = c("repl", "feature", "selected")
  mat.data$selected = factor(ifelse(mat.data$selected, "Yes", "No"), levels = c("No", "Yes"))

  gg = ggplot(mat.data, aes_string(x = "feature", y = "repl")) +
    geom_tile(aes_string(fill = "selected"), color = "white") +
    scale_fill_grey(name = "PA-best", start = 0.9, end = 0.2, drop = FALSE) +
    theme_void() +
    labs(x = "Configurations", title = title) +
    scale_y_discrete(expand = c(0, 0), labels = names(config.list)) +
    theme(axis.ticks = element_blank(),
      legend.position = "none",
      axis.title = element_text(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(hjust = 1),
      axis.text.x = element_blank()) +
    guides(fill = guide_legend(override.aes = list(colour = "black")))

  return(gg)
}

gg.bpm = lapply(result7s, function(r) {
  r2 = split(r, r$measure)
  l = lapply(r2, function(x) x$ID)
  names(l) = names(result4)
  l = l[considered.measures]
  plot_bpm(l, r$prob.name[1])
})


gg.bpm.legend = get_legend(gg.bpm[[1]] + theme(legend.position = "bottom"))
gg.bpm.cow1 = plot_grid(plotlist = gg.bpm, ncol = 2, scale = 0.95)
gg.bpm.cow2 = plot_grid(gg.bpm.cow1, gg.bpm.legend, nrow = 2, rel_heights = c(0.95, 0.05))

pdf("best_per_measure.pdf", height = 11, width = 8)
print(gg.bpm.cow2)
dev.off()


# classification and filter methods of optimal configurations
gg.methods = lapply(seq_along(result6s), function(i) {
  ggplot(data = result6s[[i]],
    mapping = aes(x = classif, fill = filter)) +
    geom_bar() +
    facet_wrap("prob.name", scales = "free_y", ncol = 3) +
    theme_bw() +
    theme(legend.position = "bottom") +
    ggtitle(names(result6s)[i]) +
    xlab("Classification Method") +
    ylab("Number of PA-best Configurations") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_fill_discrete(name = "Filter")
})

gg.methods.single.crit = ggplot(data = single.crit,
  mapping = aes(x = classif, fill = filter)) +
  geom_bar() +
  facet_wrap("prob.name", scales = "free_y", ncol = 3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Single-Criteria Tuning") +
  xlab("Classification Method") +
  ylab("Number of A-best Configurations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_discrete(name = "Filter")

pdf("best_methods.pdf", width = 7, height = 8.5)
tmp = lapply(gg.methods, print)
print(gg.methods.single.crit)
dev.off()



# size stability plots for best configurations
gg.best = lapply(result7as, function(r) {
  r2 = r[order(as.numeric(r$opt), decreasing = TRUE), ]
  ggplot(data = r2, mapping = aes(x = mean.size.test, y = stability.test, color = accuracy.test, shape = opt)) +
    geom_point() +
    theme_bw() +
    scale_color_gradient2(high = "forestgreen", mid = "yellow", low = "darkred",
      midpoint = mean(range(r$accuracy.test)), name = "Mean Classification\nAccuracy (Test)",
      labels = function(x) sprintf("%.2f", x)) +
    scale_shape_manual(values = c(17, 19, 8), name = "", drop = FALSE) +
    guides(shape = guide_legend(ncol = 1, order = 2),
      color = guide_colorbar(order = 1)) +
    facet_wrap("measure", scales = "free_y") +
    ggtitle(r$prob.name[1]) +
    ylab("Stability (Test)") +
    xlab("Mean Number of Selected Features (Test)") +
    theme(legend.position = "bottom", legend.key.width = grid::unit(0.875, "cm"))
})

pdf("best_configurations.pdf", width = 5.5, height = 4.75)
tmp = lapply(gg.best, print)
dev.off()



# differences in performance estimates on train and test data
gg_diff = function(diff.var, ylab) {
  gg = ggplot(data = result9[variable == diff.var & opt == "PA-best", ],
    mapping = aes(x = measure, y = value)) +
    geom_abline(slope = 0, intercept = 0, linetype = "dotted") +
    geom_boxplot() +
    facet_wrap("prob.name", ncol = 3) +
    xlab("Stability Measure for Assessing the PA-best Configurations") +
    ylab(ylab) +
    theme_bw()
  return(gg)
}

gg.diff.acc = gg_diff("accuracy.diff", "Mean Classification Accuracy (Test) - Mean Classification Accuracy (Train)")
gg.diff.size = gg_diff("mean.size.diff", "Mean Number of Selected Features (Test) - Mean Number of Selected Features (Train)")
gg.diff.stab = gg_diff("stability.diff", "Stability (Test) - Stability (Train)")


gg.diff.acc.opt = ggplot(data = result9a[variable == "accuracy.diff", ],
  mapping = aes(x = opt2, y = value)) +
  geom_abline(slope = 0, intercept = 0, linetype = "dotted") +
  geom_boxplot() +
  facet_wrap("prob.name", ncol = 3) +
  xlab("Approach for Assessing the Best Configurations") +
  ylab("Mean Classification Accuracy (Test) - Mean Classification Accuracy (Train)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6))


pdf("performance_differences.pdf", width = 7, height = 9)
print(gg.diff.acc)
print(gg.diff.size)
print(gg.diff.stab)
print(gg.diff.acc.opt)
dev.off()



# table: number of Paerto optimal configurations
tab1 = result5[, .N, by = c("prob.name", "measure")]
tab1 = dcast(tab1, prob.name ~ measure, value.var = "N")
colnames(tab1)[1 + seq_along(considered.measures)] =
  paste(colnames(tab1)[1 + seq_along(considered.measures)], "(all)")

tab2 = result6[, .N, by = c("prob.name", "measure")]
tab2 = dcast(tab2, prob.name ~ measure, value.var = "N")
colnames(tab2)[1 + seq_along(considered.measures)] =
  paste(colnames(tab2)[1 + seq_along(considered.measures)], "(PA)")

tab3 = merge(tab1, tab2)
colnames(tab3)[1] = "Data Set"

tab.single = table(single.crit$prob.name)
tab.single2 = as.data.table(tab.single)
colnames(tab.single2) = c("Data Set", "A-best")

tab = merge(tab3, tab.single2, sort = FALSE)
xtab = xtable::xtable(tab)
print(xtab, include.rownames = FALSE)
