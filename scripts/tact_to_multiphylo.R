args <- commandArgs(TRUE)

library(ape)

# Parsed directory name from shell script
tree_distribution_dir <- args[1]

# Set it as working directory
setwd(tree_distribution_dir)

# Get names of tacted trees
tree_names <- list.files(pattern = ".tre")

# Read trees
trees <- sapply(tree_names, read.tree, simplify = FALSE)

# Remove whitespaces around species names needed for dendropy dependency of TACT
for (i in 1:length(trees)) {
  trees[[i]]$tip.label <- gsub("'", "", trees[[i]]$tip.label)
}

# Convert to multiphylo
trees <- .compressTipLabel(trees)

# Write multiphlyo as rds object
saveRDS(trees, "trees.rds")