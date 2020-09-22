#' @title Simulates Data sets by given a Confirmatory Factor Analysis model.
#' @description Based on a given Confirmatory Factor Analysis model, this function simulates data sets.
#' In each data file, the first column shows sample numbers. The second and other columns show actual simulated data sets for each item.
#' If the model have 2 factor and each factor as 3 items, for example, column names will be something like "ID, F1_x1, F1_x2,F1_x3,F2_x1,F2_x2,F2_x3".
#' On the other hand, number of rows shows the sample number of the data. Besides, there will be two more files saved in the folder.
#' First of them is "Model_Info.dat". This file includes factor correlation and factor loading matrices, a vector showing nonnormal items and values of B, C and D for Fleishman's power method.
#' The second is "Data_List.dat". The file includes names of the data sets which were generated.
#'
#' @author Fatih Orçan
#' @importFrom Matrix chol
#' @importFrom stats rnorm
#' @importFrom utils write.table
#' @param nd Number of data set, an integer.
#' @param ss Sample Size, an integer and larger than 10.
#' @param fcors Factor correlation matrix, a symmetric matrix. If one factor model is used this should be matrix(1,1,1).
#' @param loading Factor loading matrix. Column represents number of factors and non zero row represents number of items under each factor.
#' @param nonnormal vector of 0 and 1s. 0 indicates normal, 1 indicates non-normal data generation. If nonnormal is not indicated a normal data will be generated.
#' @param Fleishman B, C and D values from Fleishman's power method. A = -C.
#' @param f.loc File location. Generated data sets will be saved at the user defined location.
#' @export
#' @examples
#' fc<-fcors.value(nf=3, cors=c(1,.5,.6,.5,1,.4,.6,.4,1))
#' fl<-loading.value(nf=3, fl.loads=c(.5,.5,.5,0,0,0,0,0,0,0,0,.3,.3,.3,0,0,0,0,0,0,0,0,.4,.4))
#' ifN<-c(1,1,1,0,0,0,0,0)
#' fleis<-c(1.0174852, .190995, -.018577) # The values for skewness=1 and kurtosis=1
#'
#' sim.skewed(nd=10, ss=1000, fcors=fc,loading=fl, nonnormal = ifN, Fleishman = fleis, f.loc=tempdir())

sim.skewed<-function(nd=10, ss=100, fcors, loading, nonnormal=NULL, Fleishman=NULL, f.loc){
  nd<-as.integer(nd)
  if (is.na(nd)==TRUE | nd < 1) {
    message("Error: The number of dataset should be a positive integer.")
    stop()}
  ss<-as.integer(ss)
  if (is.na(ss)==TRUE | ss < 10) {
    message("Error: The sample size should be an integer larger than 10.")
    stop()}

  if (all(is.numeric(fcors))!= TRUE | any(fcors>1) | any(fcors< -1)) {
    message("Error: Correlation values should be between -1 and +1.")
    stop()}

  nf<-dim(fcors)[1]

  if (all(is.numeric(loading))!= TRUE | any(loading>=1) | any(loading< 0)) {
    message("Error: Factor loadings should be smaller than 1 and larger than 0.")
    message("Note: The loading matrix should only not include cross loading.")
    stop()}

  if(is.null(nonnormal)==TRUE){nonnormal<-rep(0,dim(loading)[1])}

  U<-chol(fcors) #Cholesky decomposition
  f.names<-vector(mode = "character", length=nd)
  nfloads<-colSums(loading!=0)
  nitem<-dim(loading)[1]

  if(dim(loading)[2]!=nf){
    message("Error: Number of factors entered is not matching earlier values. Please check your parameters.")
    stop()}

  e.loading<-loading**2
  e.loading<-sqrt(1-apply(e.loading,1,sum))

  for(n.d in 1:nd){
    # Generation for Factor Scores
    f.scores<-matrix(NA,ss,nf)
    escores<-matrix(NA,ss,ncol=nitem)
    for(i in 1:nf){
      f.scores[,i]<-rnorm(ss,0,1)}
    for(i in 1:nitem){
      escores[,i]<-rnorm(ss,0,1)}
    cor.f.scores<-f.scores%*%U

    item.scores<-matrix(0,nrow = ss,ncol= nitem)
    for(j in 1:ss){
      item.scores[j,]<-loading%*%cor.f.scores[j,]+e.loading*escores[j,]
      f.names[n.d]<-c(paste("Data_",n.d,".dat", sep = ""))
    }

    item.scores<-cbind(c(1:ss),item.scores)

    # So far normal data were generated. After here non-normality will be added.
    nnitem.scores<-item.scores
    for(j in 1:nitem){
      if(nonnormal[j]==1){
        b<-Fleishman[1]
        c<-Fleishman[2]
        d<-Fleishman[3]}
      else if(nonnormal[j]==0){
        b<-1
        c<-0
        d<-0}
      else {stop("Please use only 0s or 1s to indicated nonnormality.")}
      a<- -c
      nnitem.scores[,j+1]<-a+b*item.scores[,j+1]+c*item.scores[,j+1]^2+d*item.scores[,j+1]^3
    }

    message(paste(round(100*n.d/nd, 4), "% of simulation is completed...", sep = " "))
    write.table(nnitem.scores, file= paste(f.loc, "/Data_",n.d,".dat", sep=""), sep = "\t", col.names = FALSE, row.names = FALSE,quote = FALSE)

  }

  write.table(f.names, file= paste(f.loc,"/Data_List.dat", sep = ""), col.names = FALSE, row.names = FALSE, quote = FALSE)
  message("Data generation has been completed...")

  sink(paste(f.loc,"/Model_Info.dat", sep = ""))
  print("Factor correlation matrix:")
  print(fcors)
  print("Factor loading matrix:")
  print(loading)
  print("Non-normal items:")
  print(nonnormal)
  print("Fleishman's B, C and D values (if applicable).")
  print(Fleishman)
  sink()

}
