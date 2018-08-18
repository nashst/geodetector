#' risk detector
#'
#' This function calculates the average values in each stratum of explanatory variable (X), and presents if there exists difference between two strata.
#' @param y_column The index or field name of explained variable in input dataset.
#' @param x_column_nn The index or field name of explanatory variable(s) in input dataset.
#' @param tabledata The dataset (dataframe) contains fields of explained variable and explanatory variables.
#' @return Results of risk detector include the means of explained variable in each stratum derived from an explanatory variable and the t-test for difference between two strata.
#' @keywords risk detector
#' @export
#' @examples
#' data(CollectData)
#' risk_detector("incidence","soiltype",CollectData)
#' risk_detector(1,2,CollectData)
#' risk_detector(1,c(2,3,4),CollectData)
#' risk_detector("incidence",c("soiltype","watershed","elevation"),CollectData)
#' @importFrom stats var qt


risk_detector <- function(y_column,x_column_nn,tabledata){


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
    if((class(tabledata[[x_column]])=="factor")|(class(tabledata[[x_column]])=="character")  )
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
  Result_riskDetector_n<-list()

  for (num  in 1: n_x)
  {
    x_column <- x_column_n[num]
    x_colname<-names(tabledata)[x_column]

    vec <- tabledata[,x_column]

    vec.sort <- sort(vec)
    vec.unique <- unique(vec.sort)

    #unique zone(class)
    stratum.len <- length(vec.unique)
    MeanRisk <- rep(0,length(vec.unique))

    strataVarSum <- 0;


    for(L in 1:stratum.len)
    {
      MeanRisk[L] <- mean(tabledata[which(vec == vec.unique[L]),y_column])
    }
    ###result of MeanRisk
    Risk_Output <-  data.frame(vec.unique,MeanRisk)
    colnames(Risk_Output) <- c(x_colname,"Mean of explained variable")

    #T test result
    T_Result <- vector()
    for(i in 1:(stratum.len-1))
    {
      SratSampleCnt_i <- length(which(tabledata[,x_column]==vec.unique[i]))
      SratSampleVar_i <- var(tabledata[which(tabledata[,x_column]==vec.unique[i]),y_column])
      for(j in (i+1):stratum.len)
      {
        SratSampleCnt_j <- length(which(tabledata[,x_column]==vec.unique[j]))
        SratSampleVar_j <- var(tabledata[which(tabledata[,x_column]==vec.unique[j]),y_column])

        #if samples in one zone less than 2,stop caculate
        if(SratSampleCnt_i == 1 | SratSampleCnt_j==1) next

        #caculate t
        t_numerator <- abs(MeanRisk[i]-MeanRisk[j])
        t_denominator <- sqrt(SratSampleVar_i/SratSampleCnt_i + SratSampleVar_j/SratSampleCnt_j)

        if(t_denominator == 0) {next;}

        t_value <- t_numerator / t_denominator


        #caculate t_df
        df_numerator <- (SratSampleVar_i/SratSampleCnt_i + SratSampleVar_j/SratSampleCnt_j)^2
        df_denominator <- (SratSampleVar_i/SratSampleCnt_i )^2/(SratSampleCnt_i - 1) +
          (SratSampleVar_j/SratSampleCnt_j )^2/(SratSampleCnt_j - 1)

        # 			If (abs(df_denominator == 0) next
        t_df = round(df_numerator / df_denominator)

        #if sig
        t_sig <- as.character(t_value > qt(0.975, t_df))

        T_Result <- rbind(T_Result,c(vec.unique[i],vec.unique[j],t_sig))

      }
    }



    ###Create Result convert to data.frame
    T_Result<- as.data.frame(T_Result)
    colnames(T_Result) <- c("stratium_A","stratium_B","t-test result")

    Ttest<-reshapeMatrix(T_Result)

    #reshap.Ttest <- as.data.frame(Ttest)


    Result_riskDetector<-list('Risk Detector'=Risk_Output,'Significance t-test:0.05'=Ttest)
    Result_riskDetector_n[[num]]<-Result_riskDetector
  }
  return(Result_riskDetector_n)
}

reshapeMatrix <- function(dataset)
{
  if(class(dataset) ==  "character") dataset = t(as.matrix(dataset))

  fldName1 <- as.vector(dataset[,1])
  fldName2 <- as.vector(dataset[,2])

  fldName <- unique(c(fldName1,fldName2))
  lenFld <- length(fldName)

  CreatMat <- matrix(nrow =lenFld, ncol = lenFld,  dimnames = list(fldName, fldName))

  lenDt <- nrow(dataset)


  for(i in 1:lenDt)
  {
    fld1 <- fldName1[i]
    fld2 <- fldName2[i]
    CreatMat[fld1, fld2] <- as.vector(dataset[i,3])	 #up triangle
    CreatMat[fld2, fld1] <- as.vector(dataset[i,3])  #down triangle
  }
  # 	CreatMat[is.na(CreatMat)]  <- " "

  #diagonal line is all 'False'
  for (i in 1:length(CreatMat[,1]))
  {
    CreatMat[i,i] <- FALSE
  }

  ###convert to data.frame ,so "TRUE"->TRUE
  T_CreatMat<- as.data.frame(CreatMat)
  return(T_CreatMat)
}
