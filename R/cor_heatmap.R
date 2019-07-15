cor_heatmap = function(X, Y,
                       show.value = FALSE,
                       method = c("pearson", "spearman", "kendall"),
                       cutoff.by = c("r", "p"),
                       cutoff,
                       scale = "none",
                       xtext = TRUE,
                       show.legend.scale = TRUE,
                       ...){

    method = match.arg(method)
    cutoff.by = match.arg(cutoff.by)
    if(missing(Y)) Y = X

    X = as.data.frame(X)
    Y = as.data.frame(Y)

    if(cutoff.by == "p"){
        res = lapply(X, function(x){
            lapply(Y, function(y){
                cor = cor.test(x, y, method = method)
                res = c(cor$estimate, cor$p.value)
                names(res) = c("r", "p")
                return(res)
            })

        })

        # get the matrix of r values
        rmat = matrix(nrow = ncol(X), ncol = ncol(Y))
        dimnames(rmat) = list(colnames(X), colnames(Y))
        pmat = rmat
        for(i in seq_len(nrow(rmat))){
            for(j in seq_len(ncol(rmat))){
                rmat[i, j] = res[[i]][[j]]["r"]
            }
        }

        # get the matrix of p values
        for(i in seq_len(nrow(pmat))){
            for(j in seq_len(ncol(pmat))){
                pmat[i, j] = res[[i]][[j]]["p"]
            }
        }

        anno.data = rmat
        anno.data[pmat > cutoff] = NA

    } else {
        rmat = cor(X, Y)
        anno.data = rmat
        anno.data[rmat < cutoff] = NA
    }

    zheatmap(rmat,
             show.value = T,
             text.data = as.data.frame(anno.data),
             scale = scale,
             xtext = xtext,
             show.legend.scale = show.legend.scale, ...)

}

