################################################################################
#' @title Make a color key plot
#' @description This function makes a color key plot for the heatmap. Should
#' only be called internally
#' @param color.range numeric vector that indicates the range of x axis for the
#' color key
#' @param colors character indicates the colors to use
#' @param text.size the size of the x axis labels.
#' @param show.legend.scale logic value whether to show scales on color key.
#' @keywords internal
plot_colorkey = function(color.range, colors, text.size, show.legend.scale){
    key.mat = data.frame(
        x = seq(color.range[1], color.range[2], length.out = 256),
        y = rep(1,256)
    )

    p = ggplot(key.mat,aes(x,y)) +
        geom_tile(aes(fill=x))
    if(!show.legend.scale){
        p = p +
            scale_fill_gradientn(colours = colors, limits = color.range,
                                 name="",breaks=c(color.range[1],color.range[2]),
                                 labels = c("low", "high"),
                                 guide = guide_colorbar(
                                     label.theme = element_text(size = text.size)
                                 ))
    }else{
        p = p +
            scale_fill_gradientn(colours = colors, limits = color.range,name="",
                                 guide = guide_colorbar(
                                     label.theme = element_text(size = text.size)
                                 ))
    }

    g = get_legend(p)
    return(g)
}
