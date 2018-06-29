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
#' @param seriate character. See \code{\link{seriate}}
#' @param scaled A character value indicates whether the data should be scaled
#' column wise or row wise, or not scaled.
#' @param scale.fun the scale function to use. When using "scale" the
#' \code{\link{scale}} from base package will be used. When using
#' "absolute_scale" each row or column will be forced between -1 and 1.
#' @param xtext logic whether to show x axis labels
#' @param ytext logic whether to show y axis labels
#' @param xtext.angle numeric
#' @param xtext.angle numeric
#' @param text.size numeric
#' @param xtext.hjust numeric
#' @param xtext.vjust numeric
#' @param ytext.hjust numeric
#' @param ytext.vjust numeric
#' @param heights numeric vector with length of 2
#' @param width numeric vector with length of 2
#' @author Chenghao Zhu
#' @import ggplot2
#' @import ggdendro
#' @import grid
#' @import gridExtra
#' @import seriation
#' @import viridisLite
#' @importFrom magrittr "%>%"
#' @export
zheatmap = function(data,
                    colSideBar,
                    Rowv        = TRUE,
                    Colv        = TRUE,
                    colors      = viridis(n = 256, alpha = 1, begin = 0,
                                          end = 1, option = "viridis"),
                    seriate     = "OLO",
                    scale       = c("column", "row", "none"),
                    scale.fun   = c("scale", "absolute_scale"),
                    xtext       = FALSE,
                    ytext       = TRUE,
                    text.size   = 12,
                    xtext.angle = 0,
                    ytext.angle = 0,
                    xtext.hjust = 0,
                    xtext.vjust = 0,
                    ytext.hjust = 0,
                    ytext.vjust = 0,
                    heights,
                    widths){
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
        p.Rowv = dendro.plot(dend.row, rotate = T, x.expand = 1/(length(rowInd) * 2))
        g.Rowv = ggplotGrob(p.Rowv)
    }else{
        rowInd = 1:nrow(data)
        p.Rowv = NULL; g.Rowv = NULL
    }

    if(Colv){
        dend.col = dendrogram_basic(data, row.wise = FALSE, seriate = seriate)
        colInd = order.dendrogram(dend.col)
        # column wise dendrogram
        p.Colv = dendro.plot(dend.col, x.expand = 1/(length(colInd) * 2))
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
    if(!missing(colSideBar)){
        p.colSideBar = sider_barplot(x = colSideBar, col.id = colInd)
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
    p.colorkey = plot_colorkey(color.range = color.range,
                               colors      = colors,
                               text.size   = text.size)
    g.colorkey = ggplotGrob(p.colorkey)

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
    if(!missing(colSideBar)){
        g.colbar$widths = maxWidth
    }

    # heights and widths
    if(missing(heights)) heights = c( max(nrow(data)/6, 3), nrow(data) )
    if(missing(widths))  widths  = c( ncol(data), max(ncol(data)/6, 3) )

    if(!Rowv) g.Rowv = g.ph

    if(Colv){
        if(!missing(colSideBar)){
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
            heights = c(2, nrow(data)),
            widths = c(ncol(data), ncol(data)/5),
            layout_matrix = rbind(c(1,2), c(3,3))
        )
        #right panel
        right.panel = arrangeGrob(
            grobs = list(g.ph, g.Rowv),
            heights = c(2, nrow(data))
        )
    }

    grid.arrange(
        grobs = list(left.panel, right.panel),
        widths = widths
    )

}
################################################################################
#' @keywords internal
dendrogram_basic = function(data, row.wise = TRUE, seriate = "OLO"){
    if(!row.wise){
        data = as.data.frame(t(data))
    }
    dist = dist(data)
    hc = hclust(dist)
    o = seriation::seriate(dist, method = seriate, control = list(hclust=hc))
    dend = as.dendrogram(hc)
    dend = dendextend::rotate(dend, order = rev(labels(dist)[seriation::get_order(o)]))
    return(dend)
}

################################################################################
#' @keywords internal
heatmap_main = function(data,
                        row.id,
                        col.id,
                        color.range,
                        colors,
                        xtext = FALSE,
                        ytext = TRUE,
                        text.size = 12,
                        xtext.angle = 0,
                        ytext.angle = 0,
                        xtext.hjust = 0,
                        xtext.vjust = 0,
                        ytext.hjust = 0,
                        ytext.vjust = 0
                        ){

    col.pal = colorRampPalette(colors)(256)

    df = data[rowInd, colInd] %>%
        rownames_to_column("Feature") %>%
        melt(id.var = "Feature",
             variable.name = "Sample",
             value.name = "Abundance") %>%
        mutate(Sample = factor(Sample, levels = colnames(data)[rev(col.id)]),
               Feature = factor(Feature, levels=rownames(df)[rev(row.id)]))

    p = ggplot(df, aes(Sample, Feature)) +
        geom_tile(aes(fill=Abundance)) +
        scale_x_discrete(expand = c(0,0)) +
        scale_y_discrete(expand = c(0,0)) +
        scale_fill_gradientn(
            colours = colorRampPalette(colors)(256),
            limits = color.range
        ) +
        labs(x="",y="") +
        theme(
            # legend:
            legend.position = "none",
            # axis:
            axis.title = element_blank(),
            axis.line = element_blank(),
            # margin
            plot.margin = margin(t = 0, r = 0)
        )

    if(xtext){
        p = p + theme(
            axis.text.x = element_text(size = text.size, angle = xtext.angle,
                                       hjust = xtext.hjust, vjust = xtext.vjust)
        )
    }else{
        p = p + theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
    }

    if(ytext){
        p = p + theme(
            axis.text.y = element_text(size = text.size, angle = ytext.angle,
                                       hjust = ytext.hjust, vjust = ytext.vjust)
        )
    }else{
        p = p + theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
    }
    return(p)
}
################################################################################
#' @keywords internal
sider_barplot = function(x, col.id){

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
            legend.text = element_text(size=15),
            # margin
            plot.margin = margin(0,0,0,0)
        )
    return(p)
}
################################################################################
#' @keywords internal
dendro.plot = function(dend, rotate = FALSE, x.expand){
    ggdendro::ggdendrogram(dend, labels=F, rotate = rotate, theme_dendro=T) +
        scale_y_continuous(expand=c(0,0)) +
        scale_x_continuous(expand = c(x.expand,0)) +
        labs(x="",y="") +
        theme(
            # axis
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #margin
            plot.margin = margin(0,0,0,0)
        )
}
################################################################################
#' @keywords internal
plot_colorkey = function(color.range, colors, text.size){
    key.mat = data.frame(
        x = seq(color.range[1], color.range[2], length.out = 256),
        y = rep(1,256)
    )

    p_colorkey = ggplot(key.mat,aes(x,y)) +
        geom_tile(aes(fill=x)) +
        scale_fill_gradientn(colours = colors,
                             limits = color.range) +
        scale_x_continuous(name="",breaks=c(color.range[1],0,color.range[2]),
                           labels = c("low", 0, "high")) +
        labs(x="",y="") +
        theme_bw() +
        theme(
            # panel
            panel.border = element_blank(),
            panel.grid = element_blank(),
            # legend
            legend.position = "none",
            # axis
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size=text.size)
        )
}
################################################################################
#' @keywords internal
get_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
