library(stabm)
library(data.table)
library(ggplot2)

form = function(m, p, k) {
  vf = m * log2(m)
  h = 1:m
  a = h * log2(h) * choose(m, h) * (k / p)^(h - 1) * (1 - k/p)^(m - h)
  sum(a) / vf
}

e.values = function(m, p) {
  e = sapply(1:p, form, m = m, p = p)
  data.table(m = m, p = p, k = 1:p, e = e)
}

grid = expand.grid(m = c(2, 5, 10, 20, 40, 100), p = 1000)
res = mapply(e.values, m = grid$m, p = grid$p, SIMPLIFY = FALSE)
dat = dplyr::bind_rows(res)

pdf("E_SME.pdf", height = 4, width = 6)
ggplot(data = dat, mapping = aes(x = k/p, y = e,
  group = as.factor(m), color = as.factor(m))) +
  geom_line() +
  theme_bw() +
  ylab("E(SME)") +
  scale_color_discrete(name = "m")
dev.off()

