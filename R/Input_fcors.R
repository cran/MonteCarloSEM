#' @title This function specifies the correlation matrix between the factors.
#' @description The user specifies the correlation matrix between the factors. The values entered should be between -1 and +1.
#' The values can be given by column or row but should be given in order. Please see the example for a correlation among three factors.
#' In case there is only one factor following line should be entered
#' "cors.value(nf=1, cors=c(1,1,1))"
#'
#' @author Fatih Orcan
#' @param nf the number of factor/s.
#' @param cors vector of the correlations.
#' @return The function returns the factor correlation matrix. This is a symmetric matrix, which shows the correlation values among the factors in the model.
#' @export
#' @examples
#' # This example represents a three-factor CFA model
#' #
#' fcors.value(nf=3, cors=c(1,.5,.6,.5,1,.4,.6,.4,1))

fcors.value <-function(nf, cors){
  if(nf==1){fcors<-matrix(1,1,1)}
  if(any(cors > 1) | any(cors < -1 )){stop("Values of the correlation should be between -1 and +1.
                                           Please re-run the function.")}
  if(nf>1){fcors<-matrix(ncol = nf, cors)}
  return(fcors)
}
