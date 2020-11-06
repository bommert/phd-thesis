library(data.table)
library(ggplot2)

load("fs_results.RData")

result[, ntree.fac := factor(result$ntree, levels = c(1, 2, 5, 10, 20, 50, 100) * 100)]

result = result[fw.method %in% c("ranger_permutation", "ranger_impurity"), ]
a = strsplit(result$fw.method, "_", fixed = TRUE)
result$fw.method = unlist(lapply(a, function(x) x[2]))

result$problem = factor(result$problem,
  levels = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
    "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti"))


assessVar = function(r) {
  var.fun = function(x) quantile(x, 0.75, type = 2) - quantile(x, 0.25, type = 2)
  r = BBmisc::convertListOfRowsToDataFrame(r)
  p = ncol(r)
  vars = apply(r, 2, var.fun)
  means = apply(r, 2, median) / p
  o = order(vars)
  res = list(vars = vars[o], means = means[o], i = (1:p)/p)
  return(res)
}

result2 = result[, assessVar(ranks), by = c("problem", "fw.method", "ntree", "ntree.fac")]

result2s = split(result2, result2$fw.method)

ggs = lapply(result2s, function(r) {
  ggplot(data = r, mapping = aes(x = i, y = vars, group = ntree.fac, color = ntree.fac)) +
    geom_line() +
    facet_wrap("problem", scales = "free_y", ncol = 3) +
    theme_bw() +
    labs(color = "Number of Trees") +
    xlab("Proportion of Features (Sorted by Interquartile Range of Ranks)") +
    ylab("Interquartile Range of Ranks per Feature") +
    ggtitle(paste("Filter Method:", r$fw.method[1])) +
    theme(legend.position = "bottom")
})

pdf("fs_iqr.pdf", height = 10, width = 8)
tmp = lapply(ggs, print)
dev.off()


result3 = result[, list(median.time = median(time.running)), by = c("fw.method", "problem", "ntree")]

gg.time = ggplot(data = result3, mapping = aes(x = ntree, y = median.time,
  group = fw.method, linetype = fw.method)) +
  geom_line() +
  facet_wrap("problem", scales = "free_y", ncol = 3) +
  theme_bw() +
  xlab("Number of Trees") +
  ylab("Median Run Time (in Seconds)") +
  labs(linetype = "Filter Method") +
  theme(legend.position = "bottom")

pdf("fs_time.pdf", height = 10, width = 8)
print(gg.time)
dev.off()
