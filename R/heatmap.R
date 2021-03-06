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
#' @param scale A character value indicates whether the data should be scaled
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
#' @param show.legend.scale logic value whether to show scales on color key. Default is TRUE.
#' @param show.value bolean, whether show values on heatmap
#' @param value.cutoff numeric, length bust be 2. All values beyond this range
#' @param value.size numeric, the size of values to display
#' will displayed as "< ..." or "> ..."
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
#' @examples
#' # using the mtcars data
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
#' # hide the column wise dendrogram
#' zheatmap(mtcars, Colv = F)
#'
#' # use color side bar to annotate
#' zheatmap(mtcars[,-2], rowSideBar = factor(mtcars$cyl))
#'
#' @export
zheatmap = function(data,
                    colSideBar        = NULL,
                    rowSideBar        = NULL,
                    Rowv              = TRUE,
                    Colv              = TRUE,
                    colors            = viridis(n = 256, alpha = 1, begin = 0,
                                          end = 1, option = "viridis"),
                    line.color        = "white",
                    line.size         = 0,
                    seriate           = "OLO",
                    scale             = c("column", "row", "none"),
                    scale.fun         = c("scale", "absolute_scale"),
                    xtext             = FALSE,
                    ytext             = TRUE,
                    text.size         = 12,
                    xtext.angle       = 0,
                    ytext.angle       = 0,
                    xtext.hjust       = 1,
                    xtext.vjust       = 0,
                    ytext.hjust       = 1,
                    ytext.vjust       = 0,
                    legend.text.size  = 9,
                    show.legend.scale = FALSE,
                    show.value        = FALSE,
                    text.data         = NULL,
                    text.value.size   = text.size,
                    text.value.cutoff = c(-Inf, Inf),
                    heights,
                    widths,
                    print             = TRUE){
    if(missing(data)) stop("Data is missing", call. = FALSE)
    data = as.data.frame(data)

    ## ---------- Step 1. Scale ----------------------------------

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

    ## ---------- Step 2. Subplots -------------------------------

    # color range
    data.max = max(as.matrix(data))
    data.min = min(as.matrix(data))
    max.real = max(abs(data.max), abs(data.min))
    color.range = c(- max.real, max.real)

    # Basic dendrogram
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
                        line.color  = line.color,
                        line.size   = line.size,
                        xtext       = xtext,
                        ytext       = ytext,
                        text.size   = text.size,
                        xtext.angle = xtext.angle,
                        ytext.angle = ytext.angle,
                        xtext.hjust = xtext.hjust,
                        xtext.vjust = xtext.vjust,
                        ytext.hjust = ytext.hjust,
                        ytext.vjust = ytext.vjust,
                        show.value = show.value,
                        text.data  = text.data,
                        text.value.size = text.value.size,
                        text.value.cutoff = text.value.cutoff)
    if(!Colv)
        p.hm = p.hm + theme(plot.margin = margin(t=3))

    g.hm = ggplotGrob(p.hm)

    # column side barplot
    if(!is.null(colSideBar)){
        p.colSideBar = side_barplot(x = colSideBar, id = colInd,
                                     vertical = FALSE,
                                     legend.text.size = legend.text.size)
        # get the legend
        g.colbar.legend = get_legend(p.colSideBar)
        # remove the legend from the side bar
        p.colSideBar = p.colSideBar + theme(legend.position = "none")
        g.colbar = ggplotGrob(p.colSideBar)
    }else{
        g.colbar = NULL
        g.colbar.legend = NULL
    }

    # row side barplot
    if(!is.null(rowSideBar)){
        p.rowSideBar = side_barplot(x = rowSideBar, id = rowInd,
                                    vertical = TRUE,
                                    legend.text.size = legend.text.size)
        # get the legend
        g.rowbar.legend = get_legend(p.rowSideBar)
        # remove the legend from the side bar
        p.rowSideBar = p.rowSideBar + theme(legend.position = "none")
        g.rowbar = ggplotGrob(p.rowSideBar)
    }else{
        g.rowbar = NULL
        g.rowbar.legend = NULL
    }

    # color key plot
    g.colorkey = plot_colorkey(color.range       = color.range,
                               colors            = colors,
                               text.size         = legend.text.size,
                               show.legend.scale = show.legend.scale)

    # g.ph is a empty place holder
    g.ph = grob()

    ## ---------- Step 3. Align ----------------------------------

    # align subplots
    col_list = list(g.hm$widths, g.Colv$widths, g.colbar$widths)
    maxWidth = do.call(unit.pmax, col_list)

    row_list  = list(g.hm$heights, g.Rowv$heights, g.rowbar$heights)
    maxHeight = do.call(unit.pmax, row_list)

    g.hm$heights = maxHeight
    g.hm$widths  = maxWidth

    # align legends
    legend.col_list = list(g.colorkey$widths,
                           g.rowbar.legend$widths,
                           g.colbar.legend$widths)
    legend.widths = do.call(unit.pmax, legend.col_list)

    ## ---------- Step 4. Arrange --------------------------------

    # heights and widths
    if(missing(heights)) heights = c( max(nrow(data)/6, 3), nrow(data) )
    if(missing(widths))  widths  = c( ncol(data), max(ncol(data)/6, 3) )

    legend.list = list()
    g.colorkey$widths = legend.widths
    legend.list$colorkey = g.colorkey

    if(Colv){
        g.Colv$widths    = maxWidth

        if(!is.null(colSideBar)){
            g.colbar$widths  = maxWidth
            top.panel = arrangeGrob(
                grobs = list(g.Colv, g.colbar),
                heights = c(heights[1], heights[2]/nrow(data))
            )
            g.colbar.legend$widths = legend.widths
            legend.list$colbar = g.colbar.legend
        }else{
            top.panel = g.Colv
        }
        # main panel
        main.panel = arrangeGrob(
            grobs = list(top.panel, g.hm),
            heights = heights
        )
    }else{
        # main panel
        main.panel = g.hm
        legend.list = list(g.colorkey)
    }

    if(Rowv){
        g.Rowv$heights   = maxHeight

        if(!is.null(rowSideBar)){
            g.rowbar$heights = maxHeight
            row.panel = arrangeGrob(
                grobs = list(g.rowbar, g.Rowv),
                widths = c(widths[1]/ncol(data), widths[2])
            )
            g.rowbar.legend$widths = legend.widths
            legend.list$rowbar = g.rowbar.legend
        }else{
            row.panel = g.Rowv
        }
        p = arrangeGrob(
            grobs = list(main.panel, g.ph, row.panel),
            heights = heights,
            widths = widths,
            layout_matrix = cbind(c(1,1),
                                  c(2,3))
        )
    }else{
        p = main.panel
    }

    legend.panel = arrangeGrob(
        grobs = legend.list
    )

    p = arrangeGrob(
        grobs = list(p, legend.panel),
        widths = c(sum(widths), max(sum(widths)/6,2)),
        vp = viewport(width = unit(0.95, "npc"), height = unit(0.95, "npc"))
    )

    if(!print) return(p)
    grid.newpage()
    grid.draw(p)
}
