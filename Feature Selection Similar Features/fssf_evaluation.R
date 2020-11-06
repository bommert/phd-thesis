library(data.table)
library(ggplot2)


lp = function(labels) {
  list(list("GLM Boosting", expression(L[0]), "Lasso: glmnet", "Lasso: LiblineaR", "Random Forest"))
}

load("fssf_results.RData")
colnames(result)[colnames(result) %in% paste0("V", 1:15)] = paste0("X", 1:15)

results = result[, lapply(.SD, mean), by = "learner", .SDcols = paste0("X", 1:15)]

results2 = melt(results, id.vars = "learner")

pdf("percentage_times_chosen.pdf", height = 5, width = 8)
ggplot(data = results2, mapping = aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(labels = parse(text = paste0("X[", 1:15, "]"))) +
  facet_wrap("learner", scales = "free_x", labeller = lp) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.25, size = 7)) +
  xlab("Feature") +
  ylab("Relative Selection Frequency")
dev.off()


numbers = result
numbers[, n4 := X2 + X3 + X4]
numbers[, n5 := X2 + X3 + X4 + X5]
numbers[, n6 := X2 + X3 + X4 + X5 + X6]
numbers[, n7 := X2 + X3 + X4 + X5 + X6 + X7]
numbers[, n8 := X2 + X3 + X4 + X5 + X6 + X7 + X8]


numbers2 = melt(numbers, id.vars = c("learner", "repl"),
  measure.vars = c("n4", "n5", "n6", "n7", "n8"), variable.factor = FALSE)

line_dat = data.table(
  variable = rep(paste0("n", 4:8), 2),
  value = c(3:7, rep(1, 5)),
  note = c(rep("maximal", 5), rep("optimal", 5))
)

lp2 = function(labels) {
  list(list(expression(paste("Out of ", X[2], " - ", X[4])),
    expression(paste("Out of ", X[2], " - ", X[5])),
    expression(paste("Out of ", X[2], " - ", X[6])),
    expression(paste("Out of ", X[2], " - ", X[7])),
    expression(paste("Out of ", X[2], " - ", X[8]))))
}

pdf("ncf_box.pdf", height = 4.5, width = 8)
ggplot(data = numbers2, mapping = aes(x = learner, y = value)) +
  geom_boxplot() +
  facet_grid( ~ variable, labeller = lp2) +
  geom_hline(data = line_dat, mapping = aes(yintercept = value, linetype = note)) +
  scale_linetype_manual(values = c("dashed", "dotted"),
    name = "Number of Selected Features") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    legend.position = "bottom") +
  xlab("Feature Selection Method") +
  ylab("Number of Selected Features") +
  scale_x_discrete(labels = c("GLM Boosting", expression(L[0]), "Lasso: glmnet", "Lasso: LiblineaR", "Random Forest"))
dev.off()


numbers3 = numbers2[, list(ncf = as.double(median(value))),
  by = c("learner", "variable")]
numbers3$variable = as.numeric(sapply(strsplit(numbers3$variable, "n"), function(x) x[2]))
numbers4 = rbind(numbers3, data.table(
  learner = rep("Optimal", 5),
  variable = 4:8,
  ncf = rep(1, 5)
))
numbers4$learner = factor(numbers4$learner,
  levels = c("Boosting", "L0", "Lasso", "LiblineaR", "RF", "Optimal"))


gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


pdf("ncf_median.pdf", height = 4, width = 7)
ggplot(data = numbers4, mapping = aes(x = variable, y = ncf,
  group = learner, color = learner, linetype = learner)) +
  geom_line(size = 1) +
  scale_linetype_manual(name = "Feature Selection Method",
    values = c("longdash", "dashed", "dotted", "longdash", "dashed", "dotted"),
    labels = c("GLM Boosting", expression(L[0]), "Lasso: glmnet", "Lasso: LiblineaR", "Random Forest", "Optimal")) +
  scale_color_manual(values = c(gg_color_hue(5), "black"),
    name = "Feature Selection Method",
    labels = c("GLM Boosting", expression(L[0]), "Lasso: glmnet", "Lasso: LiblineaR", "Random Forest", "Optimal")) +
  theme_bw() +
  theme(legend.text = element_text(size = 11, hjust = 0),
    axis.text = element_text(size = 11)) +
  xlab(expression(paste("Out of Features ", X[2], " - ", X[i]))) +
  ylab("Median Number of Selected Features")
dev.off()




mat = diag(15)
mat[2:4, 2:4] = 1
mat[2:4, 5] = mat[5, 2:4] = 0.999
mat[2:4, 6] = mat[6, 2:4] = 0.9
mat[2:4, 7] = mat[7, 2:4] = 0.75
mat[2:4, 8] = mat[8, 2:4] = 0.5
mat[5, 6] = mat[6, 5] = 0.9
mat[5, 7] = mat[7, 5] = 0.75
mat[5, 8] = mat[8, 5] = 0.5
mat[6, 7] = mat[7, 6] = 0.7
mat[6, 8] = mat[8, 6] = 0.5
mat[7, 8] = mat[8, 7] = 0.3

m = reshape2::melt(mat)
m$Var1 = factor(paste0("X", m$Var1), levels = paste0("X", 1:15))
m$Var2 = factor(paste0("X", m$Var2), levels = paste0("X", 1:15))

pdf("mat_fssf.pdf", height = 3.75, width = 8)
ggplot(data = m, mapping = aes(x = Var1, y = Var2, label = value, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(color = value >= 0.5)) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 13, hjust = 0.25),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 13)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  scale_fill_gradient(low = "white", high = "black", name = "Correlation",
    guide = guide_colorbar(frame.colour = "black")) +
  scale_x_discrete(labels = parse(text = paste0("X[", 1:15, "]"))) +
  scale_y_discrete(labels = parse(text = paste0("X[", 1:15, "]")))
dev.off()
