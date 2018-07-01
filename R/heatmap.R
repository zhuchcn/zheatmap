################################################################################
#' @keywords internal
'%+%' = function(a, b) paste0(a, b)
################################################################################
#' @title Clustered Heatmap
#' @description
#' A flexible heatmap function with dendrograms on both sides.
#' @param data a data.frame object
#' @param colSideBar a character or factor vector to annotate on the column
#' side between the heatmap ans column-wize dendrogram
#' @param Rowv a logical value whether to show the row-wise dendrogram
#' @param Colv a logical value whether to show the column-wise dendrogram
#' @param colors a character vector off colors to use. The default uses
#' \code{\link{viridis}}(n=256, alpha = 1, begin = 0, end = 1,
#' option = "viridis")
#' @param seriate character indicates the method to use to calculate the dist.
#' Usful methods include "OLO", "GW", and "HC". See \code{\link{seriate}} for
#' more detail
#' @param scaled A character value indicates whether the data should be scaled
#' column wise or row wise, or not scaled.
#' @param scale.fun the scale function to use. When using "scale" the
#' \code{\link{scale}} from base package will be used. When using
#' "absolute_scale" each row or column will be forced between -1 and 1.
#' @param xtext logic whether to show x axis labels
#' @param ytext logic whether to show y axis labels
#' @param xtext.angle numeric
#' @param ytext.angle numeric
#' @param text.size numeric
#' @param xtext.hjust numeric
#' @param xtext.vjust numeric
#' @param ytext.hjust numeric
#' @param ytext.vjust numeric
#' @param heights numeric vector with length of 2
#' @param width numeric vector with length of 2
#' @param print logic, whether to print the heatmap out or not. Default is TRUE and if FALSE, a grob object will be returned
#' @author Chenghao Zhu
#' @import ggplot2
#' @import ggdendro
#' @import grid
#' @import seriation
#' @import viridisLite
#' @import dplyr
#' @import tibble
#' @import reshape2
#' @importFrom magrittr "%>%"
#' @importFrom gridExtra "arrangeGrob"
#' @export
#' @examples
#' # mtcars
#' zheatmap(mtcars)
#'
#' # show x axis texts and rotate them 90 degree
#' zheatmap(t(mtcars), scale = "row", xtext.angle = 90)
#'
#' # change the color
#' library(RColorBrewer)
#' my_colors = colorRampPalette(rev(brewer.pal(11, "RdBu")))(256)
#' zheatmap(mtcars, colors = my_colors)
#'
#' # use hierachical clustring method
#' zheatmap(mtcars, seriate = "HC")
#'
#' # Don't show the column wise dendrogram
#' zheatmap(mtcars, Colv = F)
zheatmap = function(data,
                    colSideBar       = NULL,
                    Rowv             = TRUE,
                    Colv             = TRUE,
                    colors           = viridis(n = 256, alpha = 1, begin = 0,
                                          end = 1, option = "viridis"),
                    seriate          = "OLO",
                    scale            = c("column", "row", "none"),
                    scale.fun        = c("scale", "absolute_scale"),
                    xtext            = FALSE,
                    ytext            = TRUE,
                    text.size        = 12,
                    xtext.angle      = 0,
                    ytext.angle      = 0,
                    xtext.hjust      = 1,
                    xtext.vjust      = 0,
                    ytext.hjust      = 1,
                    ytext.vjust      = 0,
                    legend.text.size = 7,
                    heights,
                    widths,
                    print            = TRUE){
    if(missing(data)) stop("Data is missing", call. = FALSE)

    scale     = match.arg(scale)
    scale.fun = match.arg(scale.fun)

    scale_fun = function(x){
        if(x == "scale"){
            return(function(x){
                scale(x)
            })
        }else if(x == "absolute_scale"){
            return(function(x){
                ((x-min(x)) / (max(x) - min(x)) - 0.5 ) * 2
            })
        }
        stop("scale.fun " %+% scale.fun %+% " in valid", call. = FALSE)
    }

    # scaling
    if(scale == "column"){
        rownames = rownames(data); colnames = colnames(data)
        data = apply(data, 2, function(x) scale_fun(scale.fun)(x)) %>%
            as.data.frame
        rownames(data) = rownames; colnames(data) = colnames
    }else if(scale == "row"){
        rownames = rownames(data); colnames = colnames(data)
        data = apply(data, 1, function(x) scale_fun(scale.fun)(x)) %>%
            t %>% as.data.frame
        rownames(data) = rownames; colnames(data) = colnames
    }else if(scale != "none") stop("scale input invalid")

    # color range
    data.max = max(as.matrix(data))
    data.min = min(as.matrix(data))
    max.real = max(abs(data.max), abs(data.min))
    color.range = c(- max.real, max.real)

    # make the basic dendrogram
    if(Rowv){
        dend.row = dendrogram_basic(data, row.wise = TRUE,  seriate = seriate)
        rowInd = order.dendrogram(dend.row)
        # row wise dendrogram
        p.Rowv = dendro_plot(dend.row, rotate = T, x.expand = 1/(length(rowInd) * 2))
        g.Rowv = ggplotGrob(p.Rowv)
    }else{
        rowInd = 1:nrow(data)
        p.Rowv = NULL; g.Rowv = NULL
    }

    if(Colv){
        dend.col = dendrogram_basic(data, row.wise = FALSE, seriate = seriate)
        colInd = order.dendrogram(dend.col)
        # column wise dendrogram
        p.Colv = dendro_plot(dend.col, x.expand = 1/(length(colInd) * 2))
        g.Colv = ggplotGrob(p.Colv)
    }else{
        colInd = 1:ncol(data)
        p.Colv = NULL; g.Colv = NULL
    }

    ## Subplots
    # main heatmap
    p.hm = heatmap_main(data        = data,
                        row.id      = rowInd,
                        col.id      = colInd,
                        color.range = color.range,
                        colors      = colors,
                        xtext       = xtext,
                        ytext       = ytext,
                        text.size   = text.size,
                        xtext.angle = xtext.angle,
                        ytext.angle = ytext.angle,
                        xtext.hjust = xtext.hjust,
                        xtext.vjust = xtext.vjust,
                        ytext.hjust = ytext.hjust,
                        ytext.vjust = ytext.vjust)
    g.hm = ggplotGrob(p.hm)

    # column side barplot for annotation
    if(!is.null(colSideBar)){
        p.colSideBar = sider_barplot(x = colSideBar, col.id = colInd,
                                     legend.text.size = legend.text.size)
        # get the legend
        p.colbar.legend = get_legend(p.colSideBar)
        # remove the legend from the side bar
        p.colSideBar = p.colSideBar + theme(legend.position = "none")
        g.colbar = ggplotGrob(p.colSideBar)
    }else{
        g.colbar = NULL
        p.colbar.legend = NULL
    }

    # color key plot
    g.colorkey = plot_colorkey(color.range = color.range,
                               colors      = colors,
                               text.size   = text.size)

    # g.ph is a empty place holder
    g.ph = ggplotGrob(ggplot() + theme_classic())

    # align subplots
    col_list = list(g.hm$widths, g.Colv$widths, g.colbar$widths)
    maxWidth = do.call(unit.pmax, col_list)

    row_list = list(g.hm$heights, g.Rowv$heights)
    maxHeight = do.call(unit.pmax, row_list)

    g.hm$heights = maxHeight
    g.hm$widths = maxWidth
    if(Colv){
        g.Colv$widths = maxWidth
    }
    if(Rowv){
        g.Rowv$heights = maxHeight
    }
    if(!is.null(colSideBar)){
        g.colbar$widths = maxWidth
    }

    # heights and widths
    if(missing(heights)) heights = c( max(nrow(data)/6, 3), nrow(data) )
    if(missing(widths))  widths  = c( ncol(data), max(ncol(data)/6, 3) )

    if(!Rowv) g.Rowv = g.ph

    if(Colv){
        if(!is.null(colSideBar)){
            top.panel = arrangeGrob(
                grobs = list(g.Colv, g.colbar),
                heights = c(heights[1], heights[2]/nrow(data))
            )
        }else{
            top.panel = g.Colv
            p.colbar.legend = g.ph
        }
        # left panel
        left.panel = arrangeGrob(
            grobs = list(top.panel, g.hm),
            heights = heights
        )
        # corner panel
        corner.panel = arrangeGrob(
            grobs = list(g.colorkey, p.colbar.legend),
            heights = c(1,1)
        )
        # right.panel
        right.panel = arrangeGrob(
            grobs = list(corner.panel, g.Rowv),
            heights = heights
        )
    }else{
        #left panel
        left.panel = arrangeGrob(
            grobs = list(g.ph, g.colorkey, g.hm),
            heights = c(3, nrow(data)),
            widths = c(2, 1),
            layout_matrix = rbind(c(1,2), c(3,3))
        )
        #right panel
        right.panel = arrangeGrob(
            grobs = list(g.ph, g.Rowv),
            heights = c(3, nrow(data))
        )
    }

    p = arrangeGrob(
        grobs = list(left.panel, right.panel),
        widths = widths
    )

    if(!print) return(p)
    grid.draw(p)
}
