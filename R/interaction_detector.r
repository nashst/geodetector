#' interaction detector
#'
#' This function reveals whether the risk factors X1 and X2 (and more X) have an interactive influence on a disease Y.  
#' @param y_column The index or field name of explained variable in input dataset.
#' @param x_column_nn The index or field name of explanatory variable(s) in input dataset.
#' @param tabledata The dataset (dataframe) contains fields of explained variable and explanatory variables.
#' @return Results of interaction detector include the interactive q satistic.
#' @keywords interaction detector
#' @export 
#' @examples
#' data(CollectData) 
#' interaction_detector("incidence",c("type","region"),CollectData)
#' interaction_detector("incidence",c("type","region","level"),CollectData)


interaction_detector<- function(y_column,x_column_nn,tabledata)
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
    if((class(tabledata[[x_column]])=="factor")|(class(tabledata[[x_column]])=="character"))
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
  
  
  #test Y is 'Not a Number'-true
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
  Result_interactionDetector_n<-list()
  Intr_Result_q <- vector()
  Intr_Result_r <- vector()
  for (num  in 1: n_x_x)
  {
    x1_column <- x1_column_n[num]
    x2_column <- x2_column_n[num]
    x1_colname<-names(tabledata)[x1_column]
    x2_colname<-names(tabledata)[x2_column]
    
    vec_1 <- tabledata[,x1_column]
    vec_2 <- tabledata[,x2_column]
    
    vec_inter <- paste(vec_1,vec_2,sep='_')
    
    tabledata <- cbind(tabledata,vec_inter)
    
    inter_index <- length(names(tabledata))
    
    #q-statistic
    interValue<-as.numeric(factor_detector(y_column,inter_index,tabledata)[[1]][1])
    Intr_Result_q<- rbind(Intr_Result_q,c(x1_colname,x2_colname,interValue))
    
    X1val<-as.numeric(factor_detector(y_column,x1_column,tabledata)[[1]][1])
    X2val<-as.numeric(factor_detector(y_column,x2_column,tabledata)[[1]][1])
    
    Intr_Result_q<- rbind(Intr_Result_q,c(x1_colname,x1_colname,X1val))
    Intr_Result_q<- rbind(Intr_Result_q,c(x2_colname,x2_colname,X2val))
    
    #reletionship of Interaction
    
    nonL <- F
    if(interValue < X1val & interValue < X2val)
    {
      outputRls <- "Weaken, nonlinear"
      description <- "q(Var1 intersect Var2) < Min(q(Var1),q(Var2))"
    }
    
    if(interValue < max(X1val, X2val) & interValue > min(X1val, X2val))
    {
      outputRls <- "Weaken, uni-"
      description <- "Min(q(Var1),q(Var2)) < q(Var1 intersect Var2) < Max(q(Var1)),q(Var2))"
    }	
    if(interValue == X1val+ X2val)
    {
      outputRls <- "Independent"
      description <- "q(Var1 intersect Var2) = Max(q(Var1),q(Var2))"
    }
    
    if(interValue > X1val+ X2val) 
    {
      outputRls <- "Enhance, nonlinear"
      description <- "q(Var1 intersect Var2) > q(Var1) + q(Var2)"
      nonL <- T
    }
    if( !nonL & interValue > max(X1val, X2val))
    {
      outputRls <- "Enhance, bi-"		
      description <- "q(Var1 intersect Var2) > Max(q(Var1),q(Var2))"
    }		
    
    interName <- paste(x1_colname,x2_colname,sep=" intersect ")
    interResult <- paste("The interaction reletionship for",x1_colname,"and",x2_colname,"is:",outputRls,";",description, sep = " ")
    Intr_Result_r <- rbind(Intr_Result_r,c(interResult))
    
    #Result_interactionDetector <- data.frame(interName,interValue,outputRls,description)
    
    #colnames(Result_interactionDetector) <- c("interaction variables","q-statistic","relationship","description")
    #rownames(Result_interactionDetector) <- NULL
    #Result_interactionDetector_n[[num]]<- Result_interactionDetector
  }
  
  Intr_Result_r <- as.data.frame(Intr_Result_r)
  colnames(Intr_Result_r)=c("Description")
  Intr_Result_q <- reshapeMatrix_numeric(Intr_Result_q)
  
  Result_interactionDetector_n<-list('Interaction q-statistic'=Intr_Result_q,"Interaction Reletionship"=Intr_Result_r)
  
  
  # return(Result_interactionDetector_n)
  return(Intr_Result_q)  #20180609 xucd
}


reshapeMatrix_numeric <- function(dataset)
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
  
  
  ###convert to data.frame ,so "TRUE"->TRUE
  T_CreatMat<- as.data.frame(CreatMat)
  return(T_CreatMat)
}