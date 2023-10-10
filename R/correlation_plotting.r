

#' @description
#' Normalize SNF matrix and plot heatmap. 

#' @param W similarity matrix from SNF 
#' @param group_cluster cluster assignment
#' @param top_annotation annotation to be displayed above the heatmap output
#' @paramm left_annotation annotation to be displayed on the left of the heatmap output

suppressPackageStartupMessages(library(ComplexHeatmap))
displayClustersHeatmap <- function(W, 
                                   group_cluster, 
                                   top_annotation=NULL, 
                                   left_annotation = NULL){
    
    # clean matrix
    normalize <- function(X) X/rowSums(X)
    ind = sort(as.vector(group_cluster), index.return = TRUE)
    ind = ind$ix # index after arranged by cluster

    diag(W) = median(as.vector(W))
    W = normalize(W)
    W = W + t(W)
    
    if(is.null(top_annotation) & is.null(left_annotation)){
        Heatmap(W[ind,ind], cluster_rows=FALSE, cluster_columns=FALSE, 
                show_row_names = FALSE, show_column_names = FALSE, 
                heatmap_legend_param = list(color_bar = 'continuous',
                                     title = "Similarity"), ...)
        }
    else{
        Heatmap(W[ind, ind], 
                top_annotation=top_annotation, left_annotation = left_annotation,
                cluster_rows=FALSE, 
                cluster_columns=FALSE, 
                show_row_names = FALSE, 
                show_column_names = FALSE,
                show_heatmap_legend = TRUE, 
                col = NULL,
                heatmap_legend_param = list(color_bar = 'continuous',
                                     title = "Similarity"))
        }
    }



#' Functions to calculate correlation between cluster assignment to outcome variables and visualize to find meaningful clusters with Manhattan plot

#' @description
#'  Calculate correlation of clusters to each outcome using chi-squared (categorical outcome) and/or kruskal-wallis test (continuous outcome) in each data set that were integrated using SNF, and then generates long format data input for ClustersToOutcomeManhattan 

#' @param df a dataframe of samples with cluster_column and outcomes columns.
#' @param cluster_column the column with cluster assignment from df
#' @param outcomes one or more outcomes of interest from df
#' @param method correlation test method ("chi-squared" or "kruskal")
#' @param datatype name of the SNF integration datatype

clusterToOutcomeCorr <- function(df, cluster_column, outcomes, method, datatype){

    out = data.frame()
    for (outcome in outcomes){
        print(outcome)

        table = df[, c(cluster_column, outcome)] %>% table %>% as.data.frame.matrix
        if (method=="chi-squared"){
            result = table %>% chisq.test(simulate.p.value=TRUE)
            }
        else if (method == "kruskal"){
            result = kruskal.test(df[,outcome] ~ cluster, data=df)
            }
        row = data.frame(outcome, result$p.value, result$statistic, datatype)
        out = rbind(out, row)
        }
    return(out)
    }


#' @description
#' Manhattan plot outputs the Correlation of Clusters from spectral clustering with the Outcomes (cco), colored by data types, dot size represents sample size.
#' @param outcomes column list of outcomes
#' @param pvalue column list of correlation p-values
#' @param datatype dataset the outcome comes from
#' @param size sample size
#' @param levels optional argument to re-arrange outcome display on x-axis

clusterToOutcomeManhattan <- function(outcomes, 
                                       pvalue, 
                                       datatype, 
                                       size, 
                                       levels=NULL){
    # use levels to rearrange sequence of the outcomes
    
    log_pvalue = -log10(pvalue)
    cco = data.frame(outcomes=outcomes, log_pvalue=log_pvalue, datatype=datatype, size=size)
    if (is.null(levels)){
        levels=unique(cco$outcomes)
        }
    else {
        levels=levels
        }

    plot <- ggplot(cco, 
           aes(x = factor({{outcomes}}, level = levels),
               y = {{log_pvalue}}, color = factor(datatype))) +
        geom_point(alpha = 1, aes(size=size)) +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
        geom_hline(yintercept = -log10(0.05/nlevels(factor(cco$outcomes))), linetype = "dashed", color = "black") +
        labs(x = "Outcome", y = "-log10(p-value)", 
             color = "Data type", 
             title = "Correlation p-value of SNF clusters versus Outcomes") +
        ylim(c(0,5)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5))
            
    return(plot)
    
    }




############ Display correlation between predictors-outcome, outcomes-outcomes ###############

suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))

#' @description
#' Generate correlation heatmap (need more generalization. Only tested outcomes-outcomes correlation)

#' @param a matrix of outcomes-outcomes correlation p_values
#' @param outcome_label_color optional argument to specify outcome color labels

corrHeatmap <- function(corr, outcome_label_color=NULL){
    
    # Calculate the log-10 p-value of the correlation coefficient significance
    corr_log <- log10(corr + 1)
     
    # Color bars
    outcome_heatmap_color <- colorRamp2(c(0, 0.0005, 0.005, 0.05, 1), c("navy", "blue", "royalblue", "steelblue2", "white"))

    
    # Add color for row and column labels
    if (is.null(outcome_label_color)){
        outcome_label_color <- c(rep("black", ncol(corr_log)) )
        names(outcome_label_color) <- colnames(corr_log)
        }
    else {
        outcome_label_color = outcome_label_color
        }
    
    hm <- Heatmap(as.matrix(corr_log),
                                 name = "Outcomes and Descriptors", 
                                 cluster_rows = TRUE, cluster_columns = TRUE,
                                 cell_fun = function(j, i, x, y, width, height, fill) {
                                   flag <- 0
                                   if(corr[i, j] < 0.0001) {
                                     grid.text("***", x, y, hjust = 0.5, vjust = 0.5, 
                                               gp = gpar(fontsize = 12, col = "white"))
                                     flag <- 1
                                   } 
                                   if(flag == 0 & corr[i, j] < 0.001) {
                                     grid.text("**", x, y, hjust = 0.5, vjust = 0.5, 
                                               gp = gpar(fontsize = 12, col = "white") )
                                     flag <- 1
                                   } 
                                   if (flag == 0 & corr[i, j] < 0.01) {
                                     grid.text("*", x, y, hjust = 0.5, vjust = 0.5, 
                                               gp = gpar(fontsize = 12, col = "white") )
                                     flag <- 1
                                   }
                                 },
                                 column_names_gp = grid::gpar(fontsize = 9, col = outcome_label_color),
                                 row_names_gp = grid::gpar(fontsize = 9, col = outcome_label_color),
                                 row_km = 5, column_km = 5,
                                 column_dend_height = unit(2, "cm"),
                                 row_dend_width = unit(2, "cm"),
                                 heatmap_legend_param = list(title = expression(paste(log[10], "(p-value)")),
                                                             title_gp = gpar(fontsize = 15, fontface = "bold"),
                                                             labels_gp = gpar(fontsize = 10, fontface = "bold"),
                                                             legend_height = unit(6, "cm"),
                                                             legend_width = unit(2, "cm") ),
                                 col = outcome_heatmap_color,
                                 show_heatmap_legend = FALSE)
                                     
    return(hm)
    }


#' @description
#' Generate legend for correlation heatmap. 

#' @param legend graph path to be saved to 
#' @param legend_outcome_labels optional argument to specify outcome label names
#' @param legend_outcome_labels_color optional argument to specify outcome label name colors


corrHeatmap_legend <- function(legend_name,
                          legend_outcome_labels=NULL,
                          legend_outcome_labels_color=NULL){
        
        # Legend for the significant p-values
        lgd_sig_01 = Legend(pch = "*", type = "points", labels = "< 0.01", labels_gp = gpar(fontsize = 10))
        lgd_sig_001 = Legend(pch = "**", type = "points", labels = "< 0.001", labels_gp = gpar(fontsize = 10))
        lgd_sig_0001 = Legend(pch = "***", type = "points", labels = "< 0.0001", labels_gp = gpar(fontsize = 10))
        
        # Color bars
        outcome_heatmap_color <- colorRamp2(c(0, 0.0005, 0.005, 0.05, 1), c("navy", "blue", "royalblue", "steelblue2", "white"))

        legend_outcome_labels = legend_outcome_labels
        legend_outcome_labels_color = legend_outcome_labels_color
        
        outcome_label_color_scheme <- data.frame(matrix(nrow = length(outcome_labels), ncol = 2))
        colnames(outcome_label_color_scheme) <- c("Outcomes", "Color")
        outcome_label_color_scheme$Outcomes <- outcome_labels
        outcome_label_color_scheme$Color <- outcome_labels_color
        
        # Create a legend
        outcome_name_legend <- Legend(labels = outcome_labels,
                                legend_gp = grid::gpar(fill = outcome_labels_color),
                                title = "Outcomes and Descriptors",
                                labels_gp = gpar(fontsize = 12,
                                                 col = outcome_labels_color),
                                title_gp = gpar(fontsize = 15, fontface = "bold"))
        
        outcome_heatmap_lgd = Legend(title = expression(paste(log[10], "(p-value)")), 
                             col_fun = outcome_heatmap_color, 
                             at = c(0, 0.0005, 0.005, 0.05, 1), 
                             labels = c("0", "0.0005", "0.005", "0.05", "1"),
                             break_dist = c(1, 1, 1, 3),
                             legend_height = unit(6, "cm"),
                             legend_width = unit(2, "cm"),
                             title_gp = gpar(fontsize = 15, fontface = "bold"))
        pd = packLegend(list = list(outcome_heatmap_lgd, 
                            lgd_sig_01, lgd_sig_001, lgd_sig_0001,
                            outcome_name_legend))
    
        png(legend_name, width = 5, height = 10, units = "in", res = 500, bg = "white")

        draw(pd)
        dev.off()
        
        return(pd)
        }
    



#' @description
#' Manhattan plot showing predictor correlations to an outcome

#' @param df_export is a dataframe with features in rownames, and columns:
#'  "p.value": from correlation test, 
#'  "n": number of samples,
#'  "Group": datatype name,
#'  "Group_index": sequence of datatypes to be displayed
#' @param outcome name the correlations were computed against. To be displayed in plot title

CorrManhattan <- function(df_export, outcome){
    # Prepare data for manhattan plot
    df_manhattan <- df_export %>% 

      # Compute chromosome size
      group_by(Group_Index) %>% 
      summarise(chr_len = 1) %>% 


      # Calculate cumulative position of each chromosome
      mutate(tot = cumsum(chr_len) - chr_len) %>%
      select(-chr_len) %>%

      # Add this info to the initial dataset
      left_join(df_export, ., by=c("Group_Index" = "Group_Index")) %>%

      # Add a cumulative position of each SNP
      arrange(Group_Index, `p.value`) %>%
      mutate( BPcum = `p.value` + tot)

    # Define the x-axis
    df_axis = df_manhattan %>% 
            group_by(Group_Index) %>% 
            summarize(center = ( max(BPcum) + min(BPcum) ) / 2 ) 

    # Prepare the plot
    plot <- ggplot(df_manhattan, aes(x = BPcum, y = -log10(`p.value`))) +

              # Show all points
              geom_point( aes(color = as.factor(Group_Index)), alpha = 0.5, size = 3) +
              scale_color_manual(values = rep(c("black", "orange"), 22 )) +

              # custom X axis:
              #scale_x_continuous( label = x_axis_ticks, breaks = df_axis$center ) +
              #scale_y_continuous(expand = c(0, 0), limits = c(0, 14) ) +     # remove space between plot area and x axis

              # Add a line at p = 0.05
              geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +

              # Add the plot title
              ggtitle(label = paste("Correlation p-value of Predictors versus ", outcome)) +
              ylab(label = expression(paste(-log[10], "(p.value)"))) +

              # Custom the theme:
              theme_bw() +
              theme( 
                legend.position = "none",
                panel.border = element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 15),
                #axis.line = element_line(linewidth = 1, colour = "black"),
                plot.title = element_text(hjust = 0.5, size = 20)
      ) 
    return(plot)
    }

