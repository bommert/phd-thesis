library(ggplot2)
library(data.table)

load("results_time.RData")

############################################################################################################

time.aggr = results.time[, list(time.median = median(unlist(time))),
  by = c("fw.method", "fw.perc", "part.n", "part.p")]

time.aggr$fw.method = factor(time.aggr$fw.method,
  levels = c("anova.test", "limma", "sam", "kruskal.test", "chi.squared", "auc",
    "oneR", "variance","permutation", "impurity", "info.gain", "gain.ratio",
    "sym.uncert", "MIM", "MRMR", "JMI", "JMIM", "DISR", "NJMIM", "CMIM"))


gg1 = ggplot(data = time.aggr[fw.perc == 1 & part.p == 1, ], mapping = aes(y = time.median, x = part.n * 100)) +
  xlab("Percentage of Observations of Original Data Set") +
  geom_line() +
  ylab("Median Run Time (in Seconds)") +
  facet_wrap("fw.method", scales = "free_y", ncol = 4) +
  theme_bw()

gg2 = ggplot(data = time.aggr[fw.perc == 1 & part.n == 1, ], mapping = aes(y = time.median, x = part.p * 100)) +
  xlab("Percentage of Features of Original Data Set") +
  geom_line() +
  ylab("Median Run Time (in seconds)") +
  facet_wrap("fw.method", scales = "free_y", ncol = 4) +
  theme_bw()

gg3 = ggplot(data = time.aggr[part.p == 1 & part.n == 1, ], mapping = aes(y = time.median, x = fw.perc * 100)) +
  xlab("Percentage of Features to Select") +
  geom_line() +
  ylab("Median Run Time (in Seconds)") +
  facet_wrap("fw.method", scales = "free_y", ncol = 4) +
  theme_bw()


pdf("filter_time.pdf", width = 8, height = 10)
print(gg1)
print(gg2)
print(gg3)
dev.off()


filters = unique(time.aggr$fw.method)
linetypes = c("solid", "dashed", "dotted", "twodash", "longdash")
lts = rep(linetypes, ceiling(length(filters) / length(linetypes)))[seq_along(filters)]


time.n = time.aggr[fw.perc == 1 & part.p == 1, ]
time.n.min = time.n[part.n == 0.1, list(fw.method = fw.method, time.min = time.median)]
time.n2 = merge(time.n, time.n.min, by = "fw.method")
time.n2[, time.rel := time.median / time.min]

gg1a = ggplot(data = time.n2, mapping = aes(y = time.rel, x = part.n * 100,
  color = fw.method, linetype = fw.method)) +
  xlab("Percentage of Observations of Original Data Set") +
  geom_line() +
  ylab("Relative Median Run Time") +
  theme_bw() +
  scale_linetype_manual(values = lts, drop = FALSE, name = "Filter Method") +
  scale_color_discrete(drop = FALSE, name = "Filter Method") +
  theme(legend.position = "bottom", legend.key.width = unit(2.5, "line"))


time.p = time.aggr[fw.perc == 1 & part.n == 1, ]
time.p.min = time.p[part.p == 0.1, list(fw.method = fw.method, time.min = time.median)]
time.p2 = merge(time.p, time.p.min, by = "fw.method")
time.p2[, time.rel := time.median / time.min]

gg2a = ggplot(data = time.p2, mapping = aes(y = time.rel, x = part.p * 100,
  color = fw.method, linetype = fw.method)) +
  xlab("Percentage of Features of Original Data Set") +
  geom_line() +
  ylab("Relative Median Run Time") +
  theme_bw() +
  scale_linetype_manual(values = lts, drop = FALSE, name = "Filter Method") +
  scale_color_discrete(drop = FALSE, name = "Filter Method") +
  theme(legend.position = "bottom", legend.key.width = unit(2.5, "line"))



time.fw = time.aggr[part.n == 1 & part.p == 1, ]
time.fw.min = time.fw[fw.perc == 0.1, list(fw.method = fw.method, time.min = time.median)]
time.fw2 = merge(time.fw, time.fw.min, by = "fw.method")
time.fw2[, time.rel := time.median / time.min]

gg3a = ggplot(data = time.fw2, mapping = aes(y = time.rel, x = fw.perc * 100,
  color = fw.method, linetype = fw.method)) +
  xlab("Percentage of Features to Select") +
  geom_line() +
  ylab("Relative Median Run Time") +
  theme_bw() +
  scale_linetype_manual(values = lts, drop = FALSE, name = "Filter Method") +
  scale_color_discrete(drop = FALSE, name = "Filter Method") +
  theme(legend.position = "bottom", legend.key.width = unit(2.5, "line"))


pdf("filter_time_rel.pdf", width = 8, height = 6)
print(gg1a)
print(gg2a)
print(gg3a)
dev.off()

