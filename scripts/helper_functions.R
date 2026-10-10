#' Plot confusion matrix
#' @param conf_mat. data.frame with four columns: evaluated label,
#' predicted label, count, and proportion
#' @param font_size. Text size for grid cells
gg_confusion <- function(conf_mat, font_size = 4) {
  ggplot(conf_mat,
       aes(x = predicted, y = evaluated, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = count),
            color = "grey80", size = font_size, fontface = "bold") +
  scale_fill_viridis_c(labels = scales::percent,
                       limits = c(0, 0.9),
                       name = "Proportion (%)") +
  theme_minimal() +
  labs(x = "Inferred category", y = "IUCN category") + 
  theme(
    axis.text.x = element_text(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )
}

#' Plot side or top bar with row/colsums of confusion matrix
#' @param conf_bar. data.frame with two columns: name of the threat
#' category, counts
#' @param color_keys. Vector mapping threat categories to colors
#' @param side. Boolean, should this be a side (TRUE) or top bar (FALSE)
#' @param font_size. Text size for bar cells
gg_bar <- function(conf_bar, color_keys, side = TRUE, font_size = 4) {
  x <- "sum"
  y <- conf_bar[, 1]
  if (!side) {
    y <- "sum"
    x <- conf_bar[, 1]
  }
  
  ggplot(conf_bar,
         aes(x = x, y = y, fill = conf_bar[, 1])) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = sum), fontface = "bold", size = font_size) +
    scale_fill_manual(values = color_keys, guide = "none") +
    theme_void() +
    theme(axis.text.y = element_blank())
}

#' Create label for panel in a multipanel figure
#' @param panel_tag. Label for panel. e.g. (a)
#' @param font_size. Text size for panel tag
gg_paneltag <- function(panel_tag, font_size = 4) {
  ggplot() + 
    annotate("text", x = 1, y = 1,
             label = panel_tag, fontface = "bold", size = font_size) + 
    theme_void()
}

#' Fork of plot.iucn_featureimportance to change colors
# plot_iucnn_featureimportance <- function(x, ...) {
#   bp <- barplot(height = x$feat_imp_mean,
#                 names.arg = x$feature_block,
#                 ylim = c(0, max(x$feat_imp_mean) + max(x$feat_imp_std)),
#                 ...)
#   
#   segments(bp,
#            x$feat_imp_mean - x$feat_imp_std,
#            bp,
#            x$feat_imp_mean + x$feat_imp_std,
#            lwd = 1.5)
# }

# plot_iucnn_featimp <- function(x, ylab, panel_tag, mar) {
#   par(las = 2, mar = mar)
#   bp <- barplot(height = x$feat_imp_mean,
#                 names.arg = x$feature_block,
#                 ylim = c(0, max(x$feat_imp_mean) + max(x$feat_imp_std)),
#                 ylab = ylab)
#   segments(bp,
#            x$feat_imp_mean - x$feat_imp_std,
#            bp,
#            x$feat_imp_mean + x$feat_imp_std,
#            lwd = 1.5)
#   mtext(panel_tag, side = 3, line = -1.0, adj = -0.1,
#         font = 2, cex = 1.1, las = 1)
# }


plot_iucnn_featimp <- function(x, ylab = expression(paste(Delta, "Accuracy")),
                               ylim = NULL, panel_tag = NULL, mar = NULL) {
  par(las = 2)
  if (!is.null(mar)) {
    par(mar = mar)
  }
  
  if (is.null(ylim)) {
    ylim <- c(0, max(x$feat_imp_mean) + max(x$feat_imp_std))
  }
  
  plot(0, 0, type = "n", xlim = c(0, length(x$feature_block) + 2.5), ylim = ylim,
       xlab = "", ylab = ylab, axes = FALSE, xaxs = "i")
  abline(h = seq(0, 0.15, by = 0.03), col = "grey")
  axis(side = 2, at = seq(0, 0.15, by = 0.03), tick = FALSE)
  
  # Mean Delta accuracy as barplots
  bp <- barplot(height = x$feat_imp_mean,
                names.arg = x$feature_block,
                ylim = ylim, ylab = ylab, xaxt = "n", axes = FALSE,
                add = TRUE)
  
  # Standard deviations
  segments(bp,
           ifelse(x$feat_imp_mean - x$feat_imp_std < 0,
                  0,
                  x$feat_imp_mean - x$feat_imp_std),
           bp,
           x$feat_imp_mean + x$feat_imp_std,
           lwd = 1.5)
  # Axis labels rotated by 45 degree
  text(x = bp, y = -0.03 * diff(par("usr")[3:4]),
       labels = x$feature_block, 
       srt = 45,                # Rotation angle in degrees
       adj = 1,                 # Align text end to the tick mark
       xpd = TRUE,              # Allow drawing text outside plot region
       cex = 1.0)
  # Tag e.g. (a)
  if (!is.null(panel_tag)) {
    mtext(panel_tag, side = 3,
          line = -1.0,  # up/down
          adj = -0.16, # left/right
          font = 2, cex = 1.1, las = 1)
  }
}

gg_featimp <- function(x, panel_tag, font_size = 7, x_lab_just = 1.1) {
  # Strip the pdp class and force to data.frame
  x_df <- as.data.frame(unclass(x))
  
  ggplot(x_df,
         # reorder() to ensure plot in descending importance
         aes(x = reorder(feature_block, -feat_imp_mean), y = feat_imp_mean)) +
    # Bars
    geom_col(fill = "grey80", color = "black", width = 0.7) +
    # Error bars
    geom_errorbar(aes(ymin = ifelse(feat_imp_mean - feat_imp_std < 0,
                                    0,
                                    feat_imp_mean - feat_imp_std),
                      ymax = feat_imp_mean + feat_imp_std),
                  width = 0.2, linewidth = 0.6) +
    # Horizontal lines; the same for 5 and 2-class
    scale_y_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, by = 0.03),
      expand = c(0, 0)
    ) +
    coord_cartesian(clip = "off") +
    # Axis and panel labeling
    labs(y = expression(paste(Delta, "Accuracy")), x = "", title = panel_tag) +
    theme_minimal() +
    theme(
      # Rotate x-axis text
      axis.text.x = element_text(angle = 45, vjust = x_lab_just, hjust = 1,
                                 color = "black", size = font_size),
      axis.text.y = element_text(color = "black", size = font_size),
      axis.title.y = element_text(size = 1.1 * font_size, face = "bold"),
      
      # Clean up panel structure
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, face = "bold",
                                hjust = -0.17, vjust = -3),
      plot.margin = margin(t = -30, r = 0, b = -30, l = 0, unit = "pt")
    )
}


#' Reverse z-scaling
#' @param pdp. Output of iucnn_get_pdp function
#' @param stats. data.frame with mean and standard deviation for z-scaling
#' in rows
backscale_pdp_features <- function(pdp, stats) {
  feature_names <- colnames(stats)
  for (fn in feature_names) {
    if (fn %in% names(pdp)) {
      pdp[[fn]]$feature <- (pdp[[fn]]$feature * feature_stats[2, fn]) +
        feature_stats[1, fn]
    }
  }
  return(pdp)
}


rename_pdp_cat <- function(pdp) {
  
  if ("systematics" %in% names(pdp)) {
    pdp$systematics$feature[
      pdp$systematics$feature == "Pontobelgrandiellinae_Bullaregiinae"] <-
      "Pontobelgrandiellinae\nand Bullaregiinae"
    pdp$systematics$feature[
      pdp$systematics$feature == "Islamiinae_Navarrielinae"] <-
      "Islamiinae\nand Navarrielinae"
    pdp$systematics$feature[
      pdp$systematics$feature == "Pyrgulinae_Shadiniinae"] <-
      "Pyrgulinae\nand Shadiniinae"
    pdp$systematics$feature[
      pdp$systematics$feature == "Unnamed_Subfamily"] <-
      "Unnamed\nSubfamily"
  }

  if ("habitat" %in% names(pdp)) {
    pdp$habitat$feature[
      pdp$habitat$feature == "rivers"] <- "Rivers"
    pdp$habitat$feature[
      pdp$habitat$feature == "brackish_lakes"] <- "Brackish\nLakes"
    pdp$habitat$feature[
      pdp$habitat$feature == "brackish_pools"] <- "Brackish\nPools"
    pdp$habitat$feature[
      pdp$habitat$feature == "subterranean_systems"] <- "Subterranean\nSystems"
    pdp$habitat$feature[
      pdp$habitat$feature == "freshwater_lakes"] <- "Freshwater\nLakes"
    pdp$habitat$feature[
      pdp$habitat$feature == "springs"] <- "Springs"
  }

  
  return(pdp)
}

#' Order categorical features in the 5-class PDP as they are in the 2-class PDP
order_as_pdp_2 <- function(pdp_5, pdp_2, name) {
  ord_in_pdp_2 <- order(pdp_2[[name]]$pdp[, 1])
  pdp_5[[name]]$feature <- pdp_5[[name]]$feature[ord_in_pdp_2, , drop = FALSE]
  pdp_5[[name]]$pdp <- pdp_5[[name]]$pdp[ord_in_pdp_2, ]
  pdp_5[[name]]$lwr <- pdp_5[[name]]$lwr[ord_in_pdp_2, ]
  pdp_5[[name]]$upr <- pdp_5[[name]]$upr[ord_in_pdp_2, ]
  return(pdp_5)
}

#' Convert raw predictions to threat categories
#' @param raw_pred. Raw prediction output obtained from 
#' iucnn_predict_status(..., return_raw = TRUE)
raw_pred_to_class <- function(pred) {
  raw_pred <- pred$raw_predictions
  if (class(raw_pred) == "list") {
    # Flatten list of raw predictions when using cross-validation
    cv_fold <- length(raw_pred)
    d <- dim(raw_pred[[1]])
    raw_pred_merge <- array(NA_real_, dim = c(cv_fold * d[1], d[2], d[3]))
    for (i in 1:cv_fold) {
      raw_pred_merge[((i - 1) * d[1] + 1):(i * d[1]), ,] <- raw_pred[[i]]
    }
    raw_pred <- raw_pred_merge
  }
  
  # Number of dropout repeat, species, and whether this is 5/2 class prediction
  raw_pred_dims <- dim(raw_pred)
  dropout_reps <- raw_pred_dims[1]
  num_species <- raw_pred_dims[2]
  num_cats <- raw_pred_dims[3]
  
  # Names of threat categories
  names_cats <- c("LC", "NT", "VU", "EN", "CR")
  if (num_cats == 2) {
    names_cats <- c("Not Threatened", "Threatened")
  }
  
  # Convert raw prediction to threat categories
  class_pred <- matrix(NA_character_, nrow = dropout_reps, ncol = num_species)
  colnames(class_pred) <- pred$names
  for (i in 1:dropout_reps) {
    class_pred[i, ] <- names_cats[apply(raw_pred[i, ,], 1, which.max)]
  }
  
  return(class_pred)
}


# Original code by Rikki Gumbs
# Modified by Ruth Brown rcb22@ic.ac.uk
# Adapted by Matilda Brown m.brown2@kew.org
# Adapted to run with predictions by Rikki Gumbs and Felix Forest

GE.2.calc <- function(pext, n_species = 10000) {
  iucn <- sample(1:5, size = n_species, replace = TRUE)
  data <- data.frame(species = 1:n_species, pext = pext[iucn])
  data <- data[order(data$pext), ]
  data$rank <- seq_len(nrow(data))
  rank <- c(0, with(data, tapply(rank, pext, median)))
  pext <- c(0, pext)
  rank.sq <- rank^2
  rank.cub <- rank^3
  rank.qu <- rank^4
  # rank.quu <- rank^5 # Not used
  # Why Gumbs et al was not using a glm(..., family = "quasibinomial)?
  model <- lm(pext ~ rank + rank.sq + rank.cub + rank.qu)
  data$rank.sq <- data$rank^2
  data$rank.cub <- data$rank^3
  data$rank.qu <- data$rank^4
  # data$rank.quu <- data$rank^5 # Not used
  data$rank.pext <- predict(model, data)
  data$rank.pext[data$rank.pext <= 0] <- 0.0001
  data$rank.pext[data$rank.pext >= 1] <- 0.9999
  pext.LC <- data.frame(RL.cat = "LC", pext = data$rank.pext[data$pext == pext[2]])
  pext.NT <- data.frame(RL.cat = "NT", pext = data$rank.pext[data$pext == pext[3]])
  pext.VU <- data.frame(RL.cat = "VU", pext = data$rank.pext[data$pext == pext[4]])
  pext.EN <- data.frame(RL.cat = "EN", pext = data$rank.pext[data$pext == pext[5]])
  pext.CR <- data.frame(RL.cat = "CR", pext = data$rank.pext[data$pext == pext[6]])
  return(rbind(pext.CR,pext.EN, pext.VU, pext.NT, pext.LC))
}


# remove excess pext, and reorders to same order as tree$tip.label
into_order <- function(tree, pext){
  new_pext <- pext[match(tree$tip.label, pext$species),]
  return (new_pext)
}


# order tree components
reorder_tree <- function(tree, ordering){
  tree@edge.length <- tree@edge.length[ordering]
  tree@edge <- tree@edge[ordering,]
  return(tree)
}


# EDGE2 Function
EDGE2_mod <- function(tree, pext) {
  
  names(pext) <- c("species", "pext")
  
  N_species <- length(tree$tip.label)
  N_nodes <- tree$Nnode
  N_tot <- N_species + N_nodes
  
  # ensure extinction probabilities are given in same order as tree$tip.label.
  if (!identical(tree$tip.label, pext$species)){
    pext <- into_order(tree, pext)
  }
  
  if (!class(tree) == "phylo") {
    tree <- as(tree, "phylo")
  }
  
  tree_dat <- data.frame(Species = as.character(tree$tip.label),
                         TBL = NA, 
                         pext = pext$pext, ED = NA, EDGE = NA)
  ePD.dat <- data.frame(PD = sum(tree$edge.length),ePDloss = NA)
  
  tree <- as(tree, "phylo4")
  root <- rootNode(tree)
  nodes <- c(root, descendants(tree, root, "all"))
  
  # reorder tree components more instinctively, such that nodes are easier to find
  ord <- order(nodes)
  tree <- reorder_tree(tree, ord)
  nodes <- nodes[ord]
  
  tree_dat$TBL <- tree@edge.length[1:N_species]
  
  node_data <- data.frame(Node = 1:N_tot, Pext = rep(1, N_tot), Edge_Sum = NA)
  node_data[1:length(tree@label), 2] <- pext[,2]
  
  # assign the product of its descendant tips to each node
  for (i in c(1:length(tree@label), N_tot:(root+1))) {      # for each node, beginning with tips
    anc <- tree@edge[i, 1]                                  # find ancestor of node
    node_data[anc, 2] <- node_data[anc, 2] * node_data[i,2] # muliply ancestor value by node "pext" 
  }
  
  # multiply each edge.length by each pext caluclated above
  for (i in 1:length(nodes)) {
    tree@edge.length[i] <- tree@edge.length[i]*node_data[i, 2]
  }
  
  if (is.na(tree@edge.length[root])) {
    tree@edge.length[root] <- 0
  }
  node_data$Edge_Sum[root] <- tree@edge.length[root]
  
  # for each internal node, summate ancesteral edgelengths
  for (i in (root+1):N_tot) {
    ans <- tree@edge[i,1]
    node_data$Edge_Sum[i] <- node_data$Edge_Sum[ans] + tree@edge.length[i]
  }
  
  # for each tip, summate ancesteral edgelengths to find EDGE2 score
  for (i in 1:N_species) {
    ans <- tree@edge[i, 1]
    tree_dat$EDGE[i] <- node_data$Edge_Sum[ans] + tree@edge.length[i]
  }  
  
  tree_dat$ED <- tree_dat$EDGE / tree_dat$pext
  # reorder tree
  tree <- reorder_tree(tree, order(ord))
  
  tree <- as(tree, "phylo")
  ePD.dat$ePDloss <- sum(tree$edge.length)
  edge.res <- list(tree_dat, tree,ePD.dat)
  return(edge.res)
}
