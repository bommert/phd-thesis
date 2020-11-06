library(ggplot2)
library(data.table)
load("sd_results_tuned.RData")

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

results$measure = factor(results$measure,
  levels = c("intersectionCount", "unadjusted", "acc", "stabilitySelectionL0", "truth"))
levels(results$measure) = c("adj", "unadj", "acc", "stabs", "truth")
results$matrix.type[results$matrix.type == "exponential"] = "Toeplitz"
results$matrix.type[results$matrix.type == "blocks"] = "Blocks"

results.orig = results

results$block.size = paste("# Similar Other Features:", results$block.size - 1)
results$block.size = factor(results$block.size,
  levels = paste("# Similar Other Features:", c(0, 4, 14, 24)))

colnames(results)[colnames(results) == "acc.test"] = "Test Accuracy"
colnames(results)[colnames(results) == "false.positive"] = "False Positive"
colnames(results)[colnames(results) == "false.negative"] = "False Negative"

results.melt = melt(results, measure.vars = c("Test Accuracy", "False Positive", "False Negative"))

setup = results[, .N, by = c("p", "n", "matrix.type")]

ggs = lapply(1:nrow(setup), function(i) {
  dat = results.melt[p == setup$p[i] & n == setup$n[i] & matrix.type == setup$matrix.type[i], ]
  gg = ggplot(data = dat,
    mapping = aes(x = measure, y = value, color = measure)) +
    geom_boxplot(lwd = 0.75) +
    facet_grid(variable ~ block.size, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    xlab("Approach") + ylab(element_blank()) +
    ggtitle(paste0("Matrix Type: ", setup$matrix.type[i], ", n = ", setup$n[i], ", p = ", setup$p[i]))

  if (length(unique(dat$measure)) == 4) {
    gg = gg + scale_color_manual(values = gg_color_hue(5)[c(1:3, 5)])
  }
  return(gg)
})

pdf("sd_results.pdf", height = 4.5, width = 8)
a = lapply(ggs, print)
dev.off()


######################################

# How do the results vary between the scenarios?

results2 = results.orig
results2$p = factor(paste("p =", results2$p), levels = c("p = 200", "p = 2000", "p = 10000"))
results2$n = factor(paste("n =", results2$n), levels = c("n = 100", "n = 1000"))
results2$block.size = factor(as.character(results2$block.size - 1), levels = c("0", "4", "14", "24"))
results3 = results2[measure != "truth", ]

results2m = split(results2, results2$matrix.type)
results3m = split(results3, results3$matrix.type)

ggs.acc = lapply(results2m, function(r) {
  ggplot(data = r,
    mapping = aes(x = block.size, y = acc.test)) +
    geom_boxplot() +
    facet_grid(measure ~ p + n, scales = "free") +
    theme_bw() +
    xlab("Number of Similar Other Features") +
    ylab("Test Accuracy") +
    ggtitle(paste("Matrix Type:", r$matrix.type[1])) +
    theme(strip.text = element_text(size = 10))
})

ggs.fp = lapply(results3m, function(r) {
  ggplot(data = r,
    mapping = aes(x = block.size, y = false.positive)) +
    geom_boxplot() +
    facet_grid(measure ~ p + n, scales = "free") +
    theme_bw() +
    xlab("Number of Similar Other Features") +
    ylab("Number of False Positive Features") +
    ggtitle(paste("Matrix Type:", r$matrix.type[1])) +
    theme(strip.text = element_text(size = 10))
})

ggs.fn = lapply(results3m, function(r) {
  ggplot(data = r,
    mapping = aes(x = block.size, y = false.negative)) +
    geom_boxplot() +
    facet_grid(measure ~ p + n, scales = "free") +
    theme_bw() +
    xlab("Number of Similar Other Features") +
    ylab("Number of False Negative Features") +
    ggtitle(paste("Matrix Type:", r$matrix.type[1])) +
    theme(strip.text = element_text(size = 10))
})

pdf("sd_results_acc.pdf", height = 10, width = 8)
tmp = lapply(ggs.acc, print)
dev.off()

pdf("sd_results_fp.pdf", height = 8, width = 8)
tmp = lapply(ggs.fp, print)
dev.off()

pdf("sd_results_fn.pdf", height = 8, width = 8)
tmp = lapply(ggs.fn, print)
dev.off()


###########################################################################################
# Run times

load("sd_runtimes.RData")

tab = tab[, c("algorithm", "data.seed", "n", "p", "block.size", "matrix.type", "time.running"), with = FALSE]
tab = tab[algorithm %in% c("tuning.acc", "tuning.unadj", "tuning.adj", "stabilitySelL0", "truth"), ]

tab$algorithm = factor(tab$algorithm,
  levels = c("tuning.adj", "tuning.unadj", "tuning.acc", "stabilitySelL0", "truth"))
levels(tab$algorithm) = c("adj", "unadj", "acc", "stabs", "truth")
tab$matrix.type[tab$matrix.type == "exponential"] = "Matrix Type: Toeplitz"
tab$matrix.type[tab$matrix.type == "blocks"] = "Matrix Type: Blocks"
tab$n = paste0("n = ", tab$n)
tab$n = factor(tab$n, levels = paste0("n = ", c(100, 1000)))
tab$p = paste0("p = ", tab$p)
tab$p = factor(tab$p, levels = paste0("p = ", c(200, 2000, 10000)))


rt.tmp = tab[, list(time = sum(as.numeric(time.running, units = "days"))),
  by = c("algorithm", "n", "p", "block.size", "matrix.type", "data.seed")]
rt = rt.tmp[, list(time = mean(time)),
  by = c("algorithm", "n", "p", "block.size", "matrix.type")]

rt = rt[rt$algorithm != "truth", ]
rt$algorithm = droplevels(rt$algorithm)


gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg.time.fun = function(data) {
  ggplot(data = data, mapping = aes(x = algorithm, y = time,
    shape = as.factor(block.size - 1), color = algorithm)) +
    geom_point() +
    facet_grid(p + n ~ matrix.type, scales = "free_y") +
    theme_bw() +
    xlab("Approach") +
    ylab("Average Sequential Run Time in Days") +
    scale_shape_discrete(name = "Number of Similar Other Features") +
    theme(legend.position = "bottom") +
    guides(color = FALSE)
}


gg = gg.time.fun(rt) + scale_y_log10() + scale_color_manual(values = gg_color_hue(5)[1:4])

pdf("sd_run_times.pdf", height = 7, width = 5.5)
print(gg)
dev.off()
