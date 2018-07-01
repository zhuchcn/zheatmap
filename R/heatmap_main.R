################################################################################
#' @title Make the main part of the heatmap
#' @description This function makes the major part of the heatmap. Should only
#' be called from internal.
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

    df = data[row.id, col.id] %>%
        rownames_to_column("Feature") %>%
        melt(id.var = "Feature",
             variable.name = "Sample",
             value.name = "Abundance") %>%
        mutate(Sample = factor(Sample, levels = colnames(data)[rev(col.id)]),
               Feature = factor(Feature, levels=rownames(data)[rev(row.id)]))

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
