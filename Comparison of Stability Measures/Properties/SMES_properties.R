library(data.table)
library(ggplot2)
library(stabm)

# no minimum value
small.values = function(p, sim) {
  sim.mat = matrix(sim, nrow = p, ncol = p)
  sim.mat[1:2, 1:p] = 0
  sim.mat[1:p, 1:2] = 0
  diag(sim.mat) = 1
  stabilitySechidis(list(1, 2), sim.mat)
}

p = 2:100
sims = seq(0.9, 1, length.out = 11)
grid = expand.grid(sims = sims, p = p)

vals.min = mapply(p = grid$p, sim = grid$sim, small.values)
data.min = data.table(p = grid$p, sim = grid$sims, value = vals.min)

gg.min = ggplot(data = data.min, mapping = aes(x = p, y = value, group = sim, color = as.factor(sim))) +
  geom_line() +
  ylab("SMES") +
  scale_color_discrete(name = expression(tau)) +
  theme_bw() +
  ggtitle("Lower Bound")+
  theme(plot.title = element_text(size = 10), axis.title = element_text(size = 10))



# no maximum value
large.values = function(p, sim) {
  p1 = ceiling(p/2)
  m = matrix(0, ncol = p, nrow = p)
  m[1:p1, (p1+1):p] = m[(p1+1):p, 1:p1] = sim
  diag(m) = 1
  stabilitySechidis(list(1:p1, (p1+1):p), m)
}

vals.max = mapply(p = grid$p, sim = grid$sim,large.values)
data.max = data.table(p = grid$p, sim = grid$sims, value = vals.max)

gg.max = ggplot(data = data.max, mapping = aes(x = p, y = value, group = sim, color = as.factor(sim))) +
  geom_line() +
  ylab("SMES") +
  scale_color_discrete(name = expression(tau)) +
  theme_bw() +
  ggtitle("Upper Bound") +
  theme(plot.title = element_text(size = 10), axis.title = element_text(size = 10))


pdf("SMES_unbounded.pdf", height = 4, width = 6)
print(gg.min)
print(gg.max)
dev.off()



# not corrected for chance
p2 = 2
combs = lapply(0:p2, function(i) combn(p2, i, simplify = FALSE))
combs = Reduce(c, combs)

sim.mat = mat = matrix(0, ncol = p2, nrow = p2)
sim.mat[1:2, 1:2] = 0.95
diag(sim.mat) = 1

grid = expand.grid(i = seq_along(combs), j = seq_along(combs))
vals = mapply(i = grid$i, j = grid$j, function(i, j) stabilitySechidis(combs[c(i,j)], sim.mat = sim.mat))
n1 = sapply(grid$i, function(index) length(combs[[index]]))
n2 = sapply(grid$j, function(index) length(combs[[index]]))
nm = (n1 + n2)/2
dt = data.table(value = vals, n1 = n1, n2 = n2, nm = nm)

dt[, mean(value), by = c("n1", "n2")]
dt[n1 == 1 & n2 == 1, ]
dt[n1 == 1 & n2 == 2, ]
