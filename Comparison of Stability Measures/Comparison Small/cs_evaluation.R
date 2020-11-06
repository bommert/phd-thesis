library(data.table)
library(ggplot2)
library(xtable)

load("cs_results.RData")

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

# number of missing values
tm = apply(result[, -(1:4)], 2, function(x) sum(is.na(x)))
ntm = sapply(strsplit(names(tm), "stability"), function(x) x[2])
names(tm) = sapply(ntm, replaceNames)
sort(tm)

result2 = result[, -c("features", "prob.name")]
result2 = melt(result2, id.vars = c("i", "j"), variable.factor = FALSE)

# scatter plots
result.value = result2[grep("stability", result2$variable), ]
result.value$variable = sapply(strsplit(result.value$variable, "stability"), function(x) x[2])
result.value$variable = sapply(result.value$variable, replaceNames)

result.value2 = result.value
colnames(result.value2) = c("i", "j", "variable2", "value2")
result.value.m = merge(result.value, result.value2, by = c("i", "j"), allow.cartesian = TRUE)
result.value.m2 = result.value.m[, .N, by = c("variable", "value", "variable2", "value2")]

result.value3 = result.value[, list(values = list(value)), by = "variable"]
result.value3 = batchtools::unwrap(result.value3)
pca.data = result.value3[, -1]
rownames(pca.data) = result.value3$variable
nas = apply(pca.data, 2, function(x) any(is.na(x)))
pca.data = pca.data[, !nas, with = FALSE]
pca.data = as.matrix(scale(pca.data, scale = FALSE))
pca = prcomp(pca.data, scale = FALSE)
pcs = pca.data %*% pca$rotation
pc1 = pcs[, 1]
o = order(pc1)

levs = unique(result.value.m2$variable)[o]
result.value.m2$variable = factor(result.value.m2$variable, levels = levs)
result.value.m2$variable2 = factor(result.value.m2$variable2, levels = levs)

gg.scatter = ggplot(data = result.value.m2, mapping = aes(x = value, y = value2)) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.1) +
  geom_point() +
  facet_grid(variable2 ~ variable, scales = "free") +
  theme_bw() +
  xlab("Stability Value") + ylab("Stability Value") +
  theme(axis.text.x = element_text(size = 7),
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x))

pdf("measures_scatter_cs.pdf", height = 19, width = 19)
print(gg.scatter)
dev.off()


result.value[, length(table(value)), by = "variable"]
result.value[, round(mean(value, na.rm = TRUE), 3), by = "variable"]
tmp = merge(result.value.m[variable == "SMN" & variable2 == "SMU" & value2 - value > 0.5, ],
  result[, list(i, j, features)], by = c("i", "j"))
max(tmp$value2)

tmp = merge(result.value.m[variable == "SMN" & variable2 == "SMP" & value2 - value > 0.5, ],
  result[, list(i, j, features)], by = c("i", "j"))

tmp = merge(result.value.m[variable == "SMY" & variable2 == "SMA-Count" & value - value2 > 0.5, ],
  result[, list(i, j, features)], by = c("i", "j"))

# explain scatter plots of SMES
t.smes = result[i == 24 & j %in% c(14, 29),]
t.smes2 = grep("stability", colnames(t.smes))
t.smes3 = sapply(strsplit(colnames(t.smes)[t.smes2], "stability"), function(x) x[2])
t.smes4 = sapply(t.smes3, replaceNames)
colnames(t.smes)[t.smes2] = t.smes4
t.smes5 = batchtools::unwrap(t.smes)
t.smes6 = subset(t.smes5,
  select = c("features.1", "features.2", "SMA-Mean", "SMA-Count", "SMA-Greedy", "SMA-MBM", "SMY", "SMES"))
t.smes6
xtab.smes = xtable(t.smes6[, -(1:2)], digits = 4)
print(xtab.smes, include.rownames = FALSE)



# connection to number of chosen features
n.feats = result[, list(n1 = lengths(features[[1]])[1], n2 = lengths(features[[1]])[2]), by = c("i", "j")]
result.value.nf = merge(result.value, n.feats)
result.value.nf1 = result.value.nf[, mean.size := (n1 + n2) / 2]
result.value.nf2 = result.value.nf1[, .N, by = c("variable", "value", "mean.size")]
result.value.nfm = result.value.nf1[, list(mean.value = mean(value, na.rm = TRUE)),
  by = c("variable", "mean.size")]

result.value.nf2$variable = factor(result.value.nf2$variable, levels = levs)
result.value.nfm$variable = factor(result.value.nfm$variable, levels = levs)

gg.size = ggplot(data = result.value.nf2, mapping = aes(x = mean.size, y = value, color = N)) +
  geom_point() +
  geom_line(data = result.value.nfm, mapping = aes(x = mean.size, y = mean.value), color = "red") +
  facet_wrap("variable", scales = "free_y", ncol = 4) +
  xlab("Mean Number of Selected Features") +
  ylab("Stability Value") +
  theme_bw() +
  theme(legend.position = "bottom", legend.key.width = grid::unit(1, "cm")) +
  scale_color_gradient("Number of Combinations with the Same Stability Value")

pdf("measures_size_cs.pdf", width = 8, height = 10)
print(gg.size)
dev.off()



# visualization of similarity matrix
sim.mat = matrix(0.1, ncol = 7, nrow = 7)
sim.mat[1:3, 1:3] = 0.95
sim.mat[4:5, 4:5] = 0.95
sim.mat[6:7, 6:7] = 0.95
diag(sim.mat) = 1

m = reshape2::melt(sim.mat)
m = cbind(m, similar = ifelse(m$value >= 0.9, "Yes", "No"))
m$value = round(m$value, 3)
m$Var1 = paste("X", m$Var1)
m$Var2 = paste("X", m$Var2)
leg.title = expression(Similarity >= 0.9)


gg.mat = ggplot(data = m, mapping = aes(x = Var1, y = Var2, label = value)) +
  geom_tile(mapping = aes(fill = similar), color = "white") +
  geom_text() +
  theme_bw() +
  labs(x = element_blank(), y = element_blank()) +
  scale_discrete_manual(values = c("grey95", "grey70"), aesthetics = "fill") +
  theme(legend.position = "bottom", legend.title.align = 1,
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 13, hjust = 0.25),
    axis.text.y = element_text(size = 13)) +
  guides(fill = guide_legend(title = leg.title, override.aes = list(colour = "black"))) +
  scale_x_discrete(expand = c(0, 0), labels = parse(text = paste0("X[", 1:7, "]"))) +
  scale_y_discrete(expand = c(0, 0), labels = parse(text = paste0("X[", 1:7, "]")))

pdf("simmat.pdf", height = 4, width = 4)
print(gg.mat)
dev.off()



