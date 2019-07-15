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
                        line.color = "white",
                        line.size = 0,
                        xtext = FALSE,
                        ytext = TRUE,
                        text.size = 12,
                        xtext.angle = 0,
                        ytext.angle = 0,
                        xtext.hjust = 0,
                        xtext.vjust = 0,
                        ytext.hjust = 0,
                        ytext.vjust = 0,
                        show.value = FALSE,
                        text.data  = NULL,
                        text.value.size = text.size,
                        text.value.cutoff = c(-Inf, Inf)
){

    col.pal = colorRampPalette(colors)(256)

    df = data[row.id, col.id] %>%
        rownames_to_column("Feature") %>%
        melt(id.var = "Feature",
             variable.name = "Sample",
             value.name = "value") %>%
        mutate(Sample = factor(Sample, levels = colnames(data)[col.id]),
               Feature = factor(Feature, levels=rownames(data)[rev(row.id)]))

    if(show.value){
        if(is.null(text.data)) text.data = data

        text.df = text.data[row.id, col.id] %>%
            rownames_to_column("Feature") %>%
            melt(id.var = "Feature",
                 variable.name = "Sample",
                 value.name = "value") %>%
            mutate(Sample = factor(Sample, levels = colnames(data)[col.id]),
                   Feature = factor(Feature, levels=rownames(data)[rev(row.id)]))

        formatValue = function(x, min, max){
            if(is.na(x)){
                return("")
            } else if(x < min){
                return("<" %+% min)
            } else if(x > max){
                return(">" %+% max)
            } else if (between(x, -100, 100)) {
                return(formatC(round(x, 2), digits = 2, format = "f"))
            } else {
                return(formatC(x, digits = 1, format = "e"))
            }
        }
        text.df = mutate(text.df, value = sapply(value, function(x){
            formatValue(x, text.value.cutoff[1], text.value.cutoff[2])
        }))
    }

    p = ggplot(df, aes(Sample, Feature)) +
        geom_tile(aes(fill=value), color = line.color, size = line.size) +
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

    # add value to heatmap
    if(show.value){
        p = p + geom_text(
            data = text.df,
            aes(label = value),
            size = text.value.size/3
        )
    }

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
