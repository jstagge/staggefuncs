# *------------------------------------------------------------------
# | FUNCTION NAME: area_lonlat
# | FILE NAME: area_bnds.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        dims
# |                
# |     Out:       
# | 
# |     Desc:      Calculates the area of a grid cell given lat lon box
# |                
# *------------------------------------------------------------------
area_lonlat <- function(dims) {
	require(SDMTools)
    names.dims <- names(dims)
    lat.cent <- dims[names.dims=="Lat"]
    lat.width <- dims[names.dims=="Lat_width"]
    lon.width <- dims[names.dims=="Lat_width"]
    grid.data <- grid.info(lats=lat.cent, c(lat.width, lon.width))
    return(grid.data$area)
  }


# *------------------------------------------------------------------
# | FUNCTION NAME: land_area_bnds
# | FILE NAME: area_bnds.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        dims
# |                
# |     Out:       
# | 
# |     Desc:      
# |                
# *------------------------------------------------------------------
land_area_bnds <- function(lon_vec, lat_vec, parallel=TRUE) {

### Re-sort lon and lat vec
lon_vec <- sort(lon_vec)
lat_vec <- sort(lat_vec)

### Assumes point is in center of grid-cell
### Calculate edges of grid cell for latitude
lat_low <- lat_vec - c(head(diff(lat_vec),1), diff(lat_vec))/2
lat_high <- lat_vec + c(diff(lat_vec),tail(diff(lat_vec),1))/2
lat_bnds <- as.matrix(data.frame(Low=lat_low, High=lat_high) ) 
lat_width <- lat_bnds[,2] - lat_bnds[,1]

### Calculate edges of grid cell for longitude
lon_low <- lon_vec - c(head(diff(lon_vec),1), diff(lon_vec))/2
lon_high <- lon_vec + c(diff(lon_vec),tail(diff(lon_vec),1))/2
lon_bnds <- as.matrix(data.frame(Low=lon_low, High=lon_high) ) 
lon_width <- lon_bnds[,2] - lon_bnds[,1]

### Create a dataframe with all possible combinations
### showing the gridpoint center and width
land_df <- data.frame(Lon= rep(lon_vec, length(lat_vec)), Lon_width= rep(lon_width, length(lat_vec)), Lat=rep(lat_vec, each=length(lon_vec)), Lat_width=rep(lat_width, each=length(lon_vec)))


if (parallel == TRUE) {

### Create parallel clusters
require(doParallel)
cores <- detectCores()
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

### Calculate grid area for each cell
area_mat <- parApply(cl, land_df,1,area_lonlat)
### Stop clusters
stopCluster(cl)

} else {
### Calculate grid area for each cell
area_mat <- apply(land_df,1,area_lonlat)
}

land_df$area <- area_mat

### Reformat area matrix to matrix format
area_mat <- matrix( area_mat, nrow=length(lon_vec), ncol=length(lat_vec))
rownames(area_mat) <- lon_vec
colnames(area_mat) <- lat_vec


return(list(lon_bnds=lon_bnds, lat_bnds=lat_bnds, area_mat=area_mat, land_df =land_df))


}



