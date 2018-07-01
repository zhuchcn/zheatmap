################################################################################
#' @title Make an annotation side bar plot
#' @description This function makes a barplot to put on the side of the heatmap
#' as annotation. Should only be called internally
#' @param x a character vector for annotation. It should have the same order
#' as the input data in the very begining.
#' @param col.id a integer vector to reorder the x vector so it matches the
#' heatmap.
#' @param legend.text.size numeric indicates the legend text size.
#' @keywords internal
sider_barplot = function(x, col.id, legend.text.size){

    data = data.frame(group = x)
    data$x = 1:length(data$group)
    data$y = 1

    p = ggplot(data, aes(x,y)) +
        geom_tile(aes(fill=group), color="white") +
        scale_y_continuous(expand = c(0,0)) +
        scale_x_continuous(expand=c(0,0)) +
        theme(
            # axis
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            # legend
            legend.title = element_blank(),
            legend.text = element_text(size=legend.text.size),
            # margin
            plot.margin = margin(0,0,0,0)
        )
    return(p)
}
################################################################################
#' @title Extract the legend from a ggplot
#' @description This function extracts the legend out of a ggplot object. It is
#' used to separate the legend from the sider_barplot in the zheatmap funciton.
#' Should only be called internally.
#' @param a.gplot a ggplto object
#' @keywords internal
get_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
