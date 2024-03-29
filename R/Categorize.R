#' @title This function uses pre-given data sets to create categorical data sets for given thresholds.
#' @description Previously simulated data sets are utilized to create categorical data sets by the given thresholds.
#'
#' @author Fatih Orçan
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @param f.loc File location. Generated data sets will be saved at the user-defined location.
#' @param threshold The threshold values.
#' @param dataList List of the names of data sets generated earlier either with the package functions or any other software.
#' @export
#' @examples
#' tres<-c(-Inf, -1.645, -.643, .643, 1.645, Inf) # five categories
#' categorize(f.loc=tempdir(), threshold = tres)

categorize<-function( f.loc, threshold, dataList="Data_List.dat"){
  DataList<-read.table(file=paste(f.loc,"/", dataList, sep=""), header = FALSE)
  n.d<-dim(DataList)[1]
  thr<-length(threshold)-1
  f.names<-vector(mode = "character", length=n.d)
  for(r in 1: n.d){
    DataN<-DataList[r,1]
    item.scores<-read.table(file=paste(f.loc,"/",DataN, sep=""))[,-1]
  # Data were read, from here categories will be created
    cat.scores<-item.scores
    ss<-dim(item.scores)[1]
    nitem<-dim(item.scores)[2]

    for(i in 1:ss){
      for(j in 1:nitem){
        for(t in 1:thr){
          if(item.scores[i,j]>threshold[t] & item.scores[i,j]<=threshold[t+1]){cat.scores[i,j]<-t}
        }

      }}

    f.names[r]<-paste("C",thr,"_Data_",r,".dat", sep="")
    item.scores<-cbind(c(1:ss),cat.scores)
    message(paste("Data", r, " is completed...", sep = " "))
    write.table(item.scores, file=paste(f.loc,"/C",thr,"_Data_",r,".dat", sep=""), sep = "\t", col.names = FALSE, row.names = FALSE,quote = FALSE)
  }

write.table(f.names,file=paste(f.loc,"/Data_List_C",thr,".dat", sep = ""), col.names = FALSE, row.names = FALSE, quote = FALSE)
}
