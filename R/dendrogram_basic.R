################################################################################
#' @title Make a basic dendrogram
#' @description This function makes a basic dendrogram using the seriation
#' methods provided by the seriation package
#'
#' This function should be only called from internal.
#'
#' @param data the data.
#' @param row.wise logic value. If true the row wise dendrogram will be returned.
#' @param seriate character. Seriate method to use. "OLO" seems to find the
#' order of samples (rows or columns) that makes the heatmap with a more clear
#' pattern. "HC" is the regular hierarchical clustring.
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
