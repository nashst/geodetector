#' factor detector
#'
#'The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y, or the determinant power of a covariate X of Y.
#' @param y_column The index or field name of explained variable in input dataset.
#' @param x_column_nn The index or the field name(s) of explanatory variable(s) in input dataset.
#' @param tabledata The dataset (dataframe) contains fields of explained variable and explanatory variables.
#' @return  Results of factor detector include q statistic and the corresponding p value.
#' @keywords factor detector
#' @export
#' @examples
#' data(CollectData)
#' factor_detector("incidence","soiltype",CollectData)
#' factor_detector(1,2,CollectData)
#' factor_detector (1,c(2,3,4),CollectData)
#' factor_detector ("incidence",c("soiltype","watershed"),CollectData)
#' @importFrom stats var pf

factor_detector <- function(y_column,x_column_nn,tabledata)
{

  n_x<-length(  x_column_nn)

  #test the name of dataframe index ----not achieve


  ###parameter test

  #test Y&X column is exist in data
  error1<-try({tabledata[y_column]},silent=TRUE)
  if('try-error' %in% class(error1))
  {
    #dealing &break
    stop("undefined columns selected in data as parameter.")
  }

  for (num  in 1: n_x)
  {

    x_column <- x_column_nn[num]
    error1<-try({tabledata[x_column]},silent=TRUE)
    if('try-error' %in% class(error1))
    {
      #dealing &break
      stop("undefined columns selected in data as parameter.")
    }

  }


  #test X column is not the same as Y
  #find index
  if(is.character(y_column))
  {
    y_colname<-y_column
    y_column<-which(names(tabledata) == y_colname)
  }
  y_colname<-names(tabledata)[y_column]

  x_column_n <- vector()
  for(num in 1:n_x)
  {

    x_column <- x_column_nn[num]

    if(is.character(x_column))
    {
      x_colname<-x_column
      x_column<-which(names(tabledata) == x_colname)
    }
    #x_column_n[num]<-x_column
    x_column_n <- rbind(x_column_n,c(x_column))

    if(x_column==y_column)
    {
      #dealing &break
      stop("Y variable and X variables should be the different data.")
    }
  }


  ###data test

  #test data is null or not
  lgnull<-is.null(tabledata)
  num_null=sum(lgnull)
  if(num_null > 0)
  {
    #dealing &break
    stop("data hava some objects with value NULL")

  }


  ###test data is 'Not Available' / Missing Values or not
  long=length(tabledata[[y_column]])

  ###test NA
  Na_check <- vector()

  #find all NA in Y column -true
  for(i in 1:long)
  {
    if(is.na(tabledata[[y_column]][i]))
    {

      Na_check <-rbind(Na_check,c(y_column,as.character(i)))

    }
  }

  #find all NA in X columns-true
  for (num  in 1: n_x)
  {
    x_column <- x_column_n[num]
    for(i in 1:long)
    {
      if(is.na(tabledata[[x_column]][i]))
      {
        Na_check <-rbind(Na_check,c(x_column,as.character(i)))
      }
    }
  }

  #test "" in X data (when data is character type,or convert  to factor type through input ,NA will convert to "".
  #                  Y can't in character type ,Only Need to add the test of "" in X data.)
  for (num  in 1: n_x)
  {
    x_column <- x_column_n[num]
    if((class(tabledata[[x_column]])=="factor")|(class(tabledata[[x_column]])=="character") )
    {
      for(i in 1:long){
        if(tabledata[[x_column]][i]=="")
        {

          Na_check <-rbind(Na_check,c(x_column,as.character(i)))

        }
      }
    }
  }

  if(length(Na_check)!=0)
  {

    #dealing &break
    mes=""
    for(i in 1:length(Na_check[,1])){
      mes=paste(mes,"data hava NA in column: ",Na_check[i,1]," ,at row: ",Na_check[i,2],"\n")

    }
    stop(mes)
  }


  #test Y is ‘Not a Number’-true
  #(These apply to numeric values and real and imaginary parts of complex values but not to values of integer vectors.)

  for(i in 1:long)
  {
    if(class(tabledata[[y_column]][i])=="character")
    {
      #dealing &break
      stop("the data type of Y variable can not be character ,in column :",y_column)

    }
  }


  #test Y is infinite or not
  lginfi<-is.infinite(tabledata[[y_column]])
  num_infi=sum(lginfi)
  if(num_infi > 0)
  {
    #dealing &break
    stop("Y variable data hava some objects with value Not finite")

  }



  #test "more than 2" or not
  for (num  in 1: n_x)
  {

    x_column <- x_column_n[num]

    #test dispersed : the number of types(groups) in a X variable should < 1/2*the length of data
    uni_x=unique(tabledata[x_column])
    long2=long/2
    if(length(uni_x[[1]])> long2)
    {
      stop("For column ",x_column,":data should be dispersed.")
    }

    #the number of types(groups) in a X variable should >1
    if(length(uni_x[[1]]) < 2)
    {
      stop("For column ",x_column,":the number of types(or groups) in a x variable should be more than 1.")
    }
    ##test "more than 2" :not need test and error feedback ,ignore them when caculate


  }



  #begin calculate
  Result_factorDetector_n<-list()

  for (num  in 1: n_x)
  {
    x_column <- x_column_n[num]
    x_colname<-names(tabledata)[x_column]

    vec <- tabledata[,x_column]
    vec.sort <- sort(vec)
    vec.unique <- unique(vec.sort)

    #number of pupulation
    N_popu <- nrow(tabledata)
    #number of strata
    N_stra <- length(vec.unique)
    #variance of all samples
    N_var <- var(tabledata[,y_column])
    # 	N_var <- var(tabledata[,y_column])*(N_popu-1)/N_popu


    #Q value
    strataVarSum <- 0

    lamda_1st_sum <- 0
    lamda_2nd_sum <- 0

    for(i in vec.unique)
    {
      LenInter <- length(which(vec == i))
      strataVar <- 0

      lamda_1st <- 0
      lamda_2nd <- 0

      if(LenInter <= 1)
      {
        strataVar <- 0

        lamda_1st <- (tabledata[which(vec == i),y_column])^2
        lamda_2nd <- tabledata[which(vec == i),y_column]
      }else
      {
        strataVar <- (LenInter-1) * var(tabledata[which(vec == i),y_column])
        # 		    strataVar <- LenInter * var(tabledata[which(vec == i),y_column])*(LenInter-1)/LenInter

        lamda_1st <- (mean(tabledata[which(vec == i),y_column]))^2
        lamda_2nd <- sqrt(LenInter) * mean(tabledata[which(vec == i),y_column])
      }
      strataVarSum <- strataVarSum + strataVar;

      lamda_1st_sum <- lamda_1st_sum + lamda_1st
      lamda_2nd_sum <- lamda_2nd_sum + lamda_2nd
    }

    TotalVar <- (nrow(tabledata)-1)*N_var
    # 	TotalVar <- nrow(tabledata)*N_var
    #Q value
    pd <- 1 - strataVarSum/TotalVar;

    #lamda value
    lamda <- (lamda_1st_sum - lamda_2nd_sum^2 / N_popu) / N_var

    # F value
    F_value <- (N_popu - N_stra)* pd / ((N_stra - 1)* (1 - pd))

    #p value
    p_value <- pf(F_value,df1= N_stra - 1, df2= N_popu - N_stra, ncp=lamda, lower.tail = F)


	#Create Result
    Result_factorDetector <- data.frame(pd,p_value)
    colnames(Result_factorDetector) <- c("q-statistic","p-value")
    rownames(Result_factorDetector) <- c(x_colname)
    Result_factorDetector_n[num]<-list(Result_factorDetector)
  }
  return(Result_factorDetector_n)
}

