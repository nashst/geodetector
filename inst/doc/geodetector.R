## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(error = TRUE, warning=FALSE, message=FALSE, fig.path='figs/')
library("geodetector")
data(CollectData)

## ----image1, echo = FALSE, out.width='90%', fig.cap="Figure 1. Principle of GeoDetector"----
knitr::include_graphics("./figs/fig-1.jpg")

## ----table1 ,echo = FALSE------------------------------------------------
  knitr::kable(
  head(CollectData[, 1:4], 10), booktabs = TRUE,
  caption = 'Table 1. Demo data in table format'
)

## ----image2, echo = FALSE, out.width='100%', fig.cap="Figure 2. Demo data in GIS format (a)NTDs prevalence Y, (b)Elevation X1, (c)Soil types X2, (d)Watersheds X3"----
knitr::include_graphics("./figs/fig-2.jpg")

## ---- eval=F-------------------------------------------------------------
#  install.packages("./Geodetector/geodetector_1.0-1.tar.gz",
#                   repos=NULL, type="source")

## ------------------------------------------------------------------------
library("geodetector")

## ------------------------------------------------------------------------
data(CollectData)

## ------------------------------------------------------------------------
class(CollectData)

## ------------------------------------------------------------------------
names(CollectData)

## ------------------------------------------------------------------------
factor_detector("incidence","type",CollectData) 

## ------------------------------------------------------------------------
factor_detector(1,2, CollectData)

## ---- eval=F-------------------------------------------------------------
#  factor_detector ("incidence",c("type","region","level"),CollectData)

## ------------------------------------------------------------------------
factor_detector (1,c(2,3,4), CollectData)

## ------------------------------------------------------------------------
interaction_detector("incidence",c("type","region","level"),CollectData)

## ------------------------------------------------------------------------
risk_detector("incidence","type",CollectData)

## ------------------------------------------------------------------------
risk_detector(1,2, CollectData)

## ---- eval=F-------------------------------------------------------------
#  risk_detector("incidence",c("type","region","level"),CollectData)

## ------------------------------------------------------------------------
risk_detector(1,c(2,3,4), CollectData)

## ------------------------------------------------------------------------
ecological_detector("incidence",c("type","region"),CollectData)

## ------------------------------------------------------------------------
ecological_detector("incidence",c("type","region","level"),CollectData)

## ------------------------------------------------------------------------
library(sp)
library(rgeos)
library(maptools)

## ------------------------------------------------------------------------
data(DiseaseData)
data(SoilType)
data(Watershed)
data(Elevation)

## ------------------------------------------------------------------------
CollectData2 <- maps2dataframe(DiseaseData,c(SoilType,Watershed, Elevation),
                            namescolomn= c('incidence', 'type', 'region', 'level'))
head(CollectData)

## ------------------------------------------------------------------------
risk_detector("incidence","type",CollectData2)
risk_detector("incidence",c("type"),CollectData2)

risk_detector(1,2,CollectData2)
risk_detector(1,c(2,3,4),CollectData2)

## ------------------------------------------------------------------------
factor_detector("incidence","type",CollectData2)
factor_detector ("incidence",c("type","region"),CollectData2)

factor_detector(1,2,CollectData2)
factor_detector (1,c(2,3,4),CollectData2)

## ------------------------------------------------------------------------
ecological_detector("incidence",c("type","region"),CollectData2)
ecological_detector("incidence",c("type","region","level"),CollectData2)

## ------------------------------------------------------------------------
interaction_detector("incidence",c("type","region"),CollectData2)
interaction_detector("incidence",c("type","region","level"),CollectData2)

## ---- eval=F-------------------------------------------------------------
#  Result <- factor_detector ("incidence",c("type","region"),CollectData)
#  write.csv(Result [[1]],'./Geodetector_R/ q_value.csv')
#  write.csv(Result [[2]],'./Geodetector_R/ p_value.csv')

