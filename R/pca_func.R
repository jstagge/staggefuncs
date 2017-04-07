# *------------------------------------------------------------------
# | FUNCTION NAME: pca_calc
# | FILE NAME: pca_func.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       data - a matrix with input data
# |
# |     Out:      pca_list -  Output list, with loadings, center, scaling, and 
# |					importance.
# | 
# |     Desc:      Performs Principal Component Analysis (PCA) with centering and scaling
# |                
# *------------------------------------------------------------------

pca_calc <- function(data) {

###  Perform PCA with scaling using SVD
pca_svd <- prcomp (data, center=TRUE, scale=TRUE)

###  Show the loadings to two decimal places
pc_importance <- summary (pca_svd , loadings=TRUE, digits=2)

### Extract loadings
pca_loadings <- pca_svd$rotation
pca_colnames <- colnames(pca_loadings)

### Fix sign of loadings, setting the largest absolute value to be positive
pc_max_load <- apply(pca_loadings,2,function(x){x[which.max(abs(x))]})
#pc_max_load  <- apply(pca_loadings,2,median)
### Determine the sign of the maximum loading and rescale to be positive
pc_sign_max <- sign(pc_max_load)
pca_loadings <- pca_loadings %*% diag(pc_sign_max)

### Reset column names
colnames(pca_loadings) <- pca_colnames

pca_list <- list(loadings=pca_loadings, center = pca_svd$center, scale=pca_svd$scale, importance=pc_importance$importance)

### Return results
return(pca_list)
}


# *------------------------------------------------------------------
# | FUNCTION NAME: pca_reconstruct
# | FILE NAME: pca_func.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       data - a matrix with input data
# |					pca_fit  -  a PCA fit from the function pca_calc
# |
# |     Out:      x -  A time series of PCA scores.
# | 
# |     Desc:      Calculates Principal Component Analysis (PCA) scores
# |					from a loading.
# |                
# *------------------------------------------------------------------

pca_reconstruct <- function (data, pca_fit) {
### Extract needed variables
loading <- pca_fit$loading
center <- pca_fit$center
scale <- pca_fit$scale

## Recalculate time series
x <- sweep(data, 2, center)
x <- as.matrix(x) %*% diag(1/scale)
x <- x %*% loading
return(x)
}