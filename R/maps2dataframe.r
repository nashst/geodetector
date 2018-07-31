#' maps2dataframe
#'
#' This function transforms the information of varialbles from shipfiles to  dataframe.
#' @param y_shp The shipfile( polygons or points) containing explained varialble in its attribute table.
#' @param x_shp_n Shipfiles( polygons or points) containing explained varialble in their attribute tables.
#' @param namescolomn Field names which represent explained variable and explanatory variables, respectively. The order correspond with y_shp and x_shp_n.
#' @return dataframe transformed from shape files.
#' If input data is shapefile format, the function named geoDetector can be used to transform from shapefile map to table format. Please note that, these shapefile layers should have the same projected coordinate system.
#' @keywords transform shipfile to table or dataframe.
#' @export
#' @examples
#' library(maptools)
#'
#' data(DiseaseData)
#' data(SoilType)
#' data(Watershed)
#' data(Elevation)
#' CollectData2<-maps2dataframe(DiseaseData,c(SoilType,Watershed, Elevation),
#'                              namescolomn= c('incidence', 'type', 'region', 'level'))
#'
#' factor_detector("incidence","type",CollectData2)
#' factor_detector(1,2,CollectData2)
#' factor_detector (1,c(2,3,4),CollectData2)
#' rst <- factor_detector ("incidence",c("type","region"),CollectData2)
#
#' interaction_detector("incidence",c("type","region"),CollectData2)
#' interaction_detector("incidence",c("type","region","level"),CollectData2)
#'
#' risk_detector("incidence","type",CollectData2)
#' risk_detector(1,2,CollectData2)
#' risk_detector(1,c(2,3,4),CollectData2)
#' risk_detector("incidence",c("type"),CollectData2)
#'
#' ecological_detector("incidence",c("type","region"),CollectData2)
#' ecological_detector("incidence",c("type","region","level"),CollectData2)
#' @importFrom stats na.omit
#' @importFrom sp coordinates SpatialPointsDataFrame over

# Load needed library.



maps2dataframe <- function(y_shp,x_shp_n,namescolomn){

  #data type check

  yspaclass <- c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")
  classy <- class(y_shp)

  if(classy%in%yspaclass ==FALSE ){

    #dealing &break
    stop("The class of spatial data for Y varialble is wrong( correct classes: spatial polygons or points).")
  }

  xspaclass <- c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")
  classx <- class(x_shp_n)

  if(classx%in%xspaclass ==FALSE ){

    for (i in 1:length(x_shp_n)){

      classx <- class(x_shp_n[[i]])
      if(classx%in%xspaclass ==FALSE){

        #dealing &break
        stop("The class of spatial data for X varialble is wrong( correct classes: spatial polygons or points).")
      }

    }
    x_shp_n=c(x_shp_n)
  }


  disease<-list()
  tryCatch({

    Pnt <- as.data.frame(coordinates(y_shp))

    names(Pnt) <- c("x","y")

    Pnt <- SpatialPointsDataFrame(Pnt, data.frame(ID=1:nrow(Pnt)))


    ls_col = namescolomn
    ls = c(y_shp, x_shp_n)


    disease_merge = over(Pnt, ls[[1]])[ls_col[1]]
    disease_merge$id = row.names(disease_merge)

    #disease_merge <- over(Pnt ,ls[[1]],fn = mean)

    #
    for(i in 2:length(ls)) {
      l = ls[[i]]
      l_col = ls_col[i]
      inter = over(Pnt, l)[l_col]
      inter$id = row.names(inter)
      disease_merge = merge(disease_merge,inter,by='id')
    }

  },error=function(e){cat("ERROR",conditionMessage(e),"\n")},finally={disease=na.omit(disease_merge)})


  disease=na.omit(disease_merge)

  return(disease[,namescolomn]) #return(disease) revised to return(disease[,namescolomn]) 20180605 xucd


}



