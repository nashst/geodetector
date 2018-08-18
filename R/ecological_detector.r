#' ecological detector
#'
#' This function identifies the impact differences between two factors X1 ~ X2.
#' @param y_column The index or field name of explained variable column in input dataset.
#' @param x_column_nn The index or field name of explanatory variable(s)in input dataset.
#' @param tabledata The dataset (dataframe) contains fields of explained variable and explanatory variables.
#' @return Results of ecological detector is the significance test of impact difference between two explanatory variables.
#' @keywords ecological detector
#' @export
#' @examples
#' data(CollectData)
#' ecological_detector("incidence",c("soiltype","watershed"),CollectData)
#' ecological_detector("incidence",c("soiltype","watershed","elevation"),CollectData)
#' @importFrom stats qf

ecological_detector <- function(y_column,x_column_nn,tabledata)
{
  #parameter test

  if(length(x_column_nn)<2)
  {
	#dealing &break
    stop("X variables input should be more than 1.")
  }

  #the number of all X input
  n_x<-length(x_column_nn)


  #test Y&X column is exist in data
  error1<-try({tabledata[y_column]},silent=TRUE)
  if('try-error' %in% class(error1)){
    stop("undefined columns selected in data as parameter.")
  }
  for (num  in 1: n_x)
  {

    x_column <- x_column_nn[num]
    error1<-try({tabledata[x_column]},silent=TRUE)
    if('try-error' %in% class(error1)){
      stop("undefined columns selected in data as parameter.")
    }

  }
  #test X column is not the same as Y
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



  ###combination  for X1,X2...
  n<-1
  x1_column_n<-vector()  #list() is ok
  x2_column_n<-vector()
  for (i in seq(from=1 , to=length(x_column_n)-1  , by=1))
  {

	  for (j in seq(from=i+1 , to=length(x_column_n)  , by=1))
	  {
		x1_column_n[n]<-x_column_n[i]
		x2_column_n[n]<-x_column_n[j]
		n=n+1
	  }

  }

  #the number of different pairs of X
  n_x_x<-length(x1_column_n)




  ###data test

  #test data is null or not
  lgnull<-is.null(tabledata)
  num_null=sum(lgnull)
  if(num_null > 0)
  {
	#dealing &break
    stop("data hava some objects with value NULL")

  }


  #test data is 'Not Available' / Missing Values or not
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

  #test "" in X data (when data is character type, or convert  to factor type through input ,NA will convert to "".
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

  if(length(Na_check)!=0){

	#dealing &break
	mes=""
	for(i in 1:length(Na_check[,1])){
		mes=paste(mes,"data hava NA in column: ",Na_check[i,1]," ,at row: ",Na_check[i,2],"\n")

	}
	stop(mes)
  }


  #test Y is ‘Not a Number’-true
  #(These apply to numeric values and real and imaginary parts of complex values but not to values of integer vectors.)

  for(i in 1:long){
	if(class(tabledata[[y_column]][i])=="character")
	{
		#dealing &break
		stop("data hava character in column :",y_column)

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
  #for X

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
  Result_ecologicalDetector_n<-list()
  mun<-1
  F_Result <- vector()
  for (num  in 1: n_x_x)
  {
    x1_column <- x1_column_n[num]
    x2_column <- x2_column_n[num]
    x1_colname<-names(tabledata)[x1_column]
    x2_colname<-names(tabledata)[x2_column]

    ##whether x1_colname > x2_colname:
    f_numerator <- factor_detector(y_column,x2_column,tabledata)[[1]][1]
    f_denominator <- factor_detector(y_column,x1_column,tabledata)[[1]][1]

    df_numerator = nrow(tabledata)-1
    df_denominator = df_numerator

    f_value = f_numerator / f_denominator

    #x1_colname > x2_colname wheather significanc,
    f_sig = f_value[1,1]  > qf(0.9, df_numerator, df_denominator)

	if(f_sig==TRUE){

	}else{
			##whether x1_colname < x2_colname:
			f_numerator <- factor_detector(y_column,x1_column,tabledata)[[1]][1]
			f_denominator <- factor_detector(y_column,x2_column,tabledata)[[1]][1]

			df_numerator = nrow(tabledata)-1
			df_denominator = df_numerator

			f_value = f_numerator / f_denominator

			#x1_colname < x2_colname  wheather significanc

			f_sig = f_value[1,1]  > qf(0.9, df_numerator, df_denominator)
		}

	F_Result<- rbind(F_Result,c(x1_colname,x2_colname,f_sig))
    #Result_ecologicalDetector <- data.frame(x1_colname,x2_colname,f_sig)
    #colnames(Result_ecologicalDetector) <- c("FactorA","FactorB","F Test")



  }




  F_Result<- as.data.frame(F_Result)
  F_Result<- reshapeMatrix(F_Result)

  #colnames(Result_ecologicalDetector) <- c('F-test for relationship(Significance:0.05)',"True/False")
  Result_ecologicalDetector_n<-list('Significance.F-test:0.05'=F_Result)


  return(Result_ecologicalDetector_n)

}
