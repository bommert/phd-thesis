library(ggplot2)
library(mlr)
library(cowplot)

calcPca = function(task, scale) {
  data = getTaskData(task, target.extra = TRUE)

  if (scale) {
    m = scale(data$data)
  } else {
    m = as.matrix(data$data)
  }

  pca = prcomp(m, scale = FALSE)

  exp = pca$sdev[1:2]^2 / sum(pca$sdev^2)
  pcs = m %*% pca$rotation

  pcs_part = cbind(as.data.frame(pcs[, 1:2]), Class = data$target)
  return(list(pcs_part = pcs_part, exp = exp))
}

plotBasic = function(task, scale, pcs) {
  lower = min(pcs$pcs_part[, 1:2])
  upper = max(pcs$pcs_part[, 1:2])

  pcgg = ggplot(pcs$pcs_part, aes(x = PC1, y = PC2, color = Class, shape = Class)) +
    geom_point(size = 3) +
    coord_equal(ratio = 1, xlim = c(lower, upper), ylim = c(lower, upper), expand = TRUE) +
    labs(x = paste0("PC1 (", round(pcs$exp[1] * 100, 2), "% of Data Variation)"),
      y = paste0("PC2 (", round(pcs$exp[2] * 100, 2), "% of Data Variation)")) +
    theme_bw() +
    theme(legend.title = element_text(size = 19),
      legend.text = element_text(size = 19),
      axis.title = element_text(size = 19),
      axis.text = element_text(size = 15),
      title = element_text(size = 19),
      legend.position = "bottom")
  return(pcgg)
}

plotPca = function(task, scale = FALSE) {
  pcs = calcPca(task, scale)
  pcgg = plotBasic(task, scale, pcs)

  pcgg = pcgg + ggtitle(getTaskId(task))
  return(pcgg)
}

plotPcaOriginal = function(task, scale) {
  pcs = calcPca(task, scale)
  pcgg = plotBasic(task, scale, pcs)

  n.classes = length(table(getTaskData(task, target.extra = TRUE)$target))
  tit = getTaskId(task)
  if (scale) tit = paste(tit, "(scaled)")
  else tit = paste(tit, "(unscaled)")

  pcgg = pcgg +
    scale_shape_manual(values = (1:14)[1:n.classes]) +
    guides(shape = guide_legend(nrow = 2)) +
    ggtitle(tit)
  return(pcgg)
}

load("datasets.RData")
plots = lapply(datasets, plotPca)
plots.scaled = lapply(datasets, plotPca, scale = TRUE)

pdf("pca_plots_grid.pdf", height = 23, width = 17.25)
plot_grid(plotlist = plots, ncol = 3)
plot_grid(plotlist = plots.scaled, ncol = 3)
dev.off()



###########################################################

# explain structure of christensen

getChristensen = function() {
  download.file(url = "ftp://ftp.ebi.ac.uk/pub/databases/microarray/data/experiment/GEOD/E-GEOD-19434/E-GEOD-19434.processed.1.zip", destfile = "christensen.zip")
  unzip("christensen.zip", exdir = "christensen")
  download.file(url = "http://www.ebi.ac.uk/arrayexpress/files/E-GEOD-19434/E-GEOD-19434.sdrf.txt", destfile = "additional_info.txt")

  # Methylation Data Set from Christensen et al. (2009)
  library('plyr')

  temp <- read.table("additional_info.txt", header = TRUE, sep ="\t", stringsAsFactors = FALSE, comment.char = "")
  temp <- temp[,c(1,8)]
  names(temp) <- c("subject_id", "labels")

  # Removes the extraneous " 1" from each subject's ID.
  temp$subject_id <- apply(temp, 1, function(subject) {
    unlist(strsplit(subject[1], " "))[1]
  })

  # The paper considers the three groups: "blood", "placenta", and "other"
  # temp[which(temp$labels == "guthrie blood"),]$labels <- "blood"
  # temp[temp$labels != "blood" & temp$labels != "placenta",]$labels <- "other"

  subjects_files <- dir("christensen")
  christensen <- ldply(subjects_files, function(subject_file) {
    subject_id <- unlist(strsplit(subject_file, "_"))[1]
    subject_data <- read.table(paste("christensen/", subject_file, sep = ""), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    subject_df <- rbind.data.frame(subject_data[,2])
    subject_df <- cbind.data.frame(temp[which(temp[,1] == subject_id), 2], subject_df)
    names(subject_df) <- c("labels", subject_data[,1])
    subject_df
  }, .progress = "text")
  christensen <- list(
    x = subset(christensen, select=-labels),
    y = factor(christensen$labels)
  )

  # Removes the downloaded, compressed ZIP file along with the meta data.
  file.remove("additional_info.txt")
  file.remove("christensen.zip")

  # Removes the folder 'christensen' that contained the decompressed data
  unlink("christensen", recursive = TRUE)

  return(christensen)
}

dat = getChristensen()
colnames(dat$x) = paste0("V", 1:ncol(dat$x))
dat = cbind(data.frame(y = dat$y), as.data.frame(dat$x))
tk = makeClassifTask(id = "christensen", data = dat, target = "y")

plots.christensen = lapply(c(FALSE, TRUE), plotPcaOriginal, task = tk)
plots.christensen = lapply(plots.christensen, function(gg) {
  gg + theme(legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    title = element_text(size = 13))
})

pdf("pca_christensen_original.pdf", height = 5, width = 10)
prow = plot_grid(plots.christensen[[1]] + theme(legend.position = "none"),
  plots.christensen[[2]] + theme(legend.position = "none"), nrow = 1)
leg = get_legend(plots.christensen[[1]])
plot_grid(prow, leg, rel_heights = c(1, 0.2), ncol = 1)
dev.off()
