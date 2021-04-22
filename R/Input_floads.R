#' @title This function specifies factor loading values.
#' @description The user specifies the factor loadings as a matrix. The values should be given by column for each factor.
#' Columns represent factors and rows represent items.
#' The values entered should be larger than 0 and smaller then 1.
#' Please see the example for a loading matrix for three-factor model.
#' @author Fatih Orçan
#' @param nf the number of factor/s.
#' @param fl.loads vector of factor loadings
#' @return The function returns factor loading matrix. The number of column shows the number of factor in the model. The rows show number of items
#' @export
#' @examples
#' # This example represents a three-factor CFA model
#' #  where the factors are indicated by 3, 3 and 2 items respectively.
#' #
#' loading.value(nf=3, fl.loads=c(.6,.6,.6,0,0,0,0,0,0,0,0,.7,.7,.7,0,0,0,0,0,0,0,0,.8,.8))

loading.value <-function(nf, fl.loads){

  fl.matx<-matrix(ncol = nf, fl.loads)
  colSums(fl.matx !=0)
  if(nf==1 & length(fl.loads)<=2){stop("Note: One-factor models should have at least 3 items.
                                      Please re-run the function.")}
  if(nf > 1 & any(colSums(fl.matx !=0)<2)){stop("Note: Mutiple-factor models should have at least 2 items under each factor.
                                      Please re-run the function.")}
  if (any(fl.loads >= 1) | any(fl.loads < 0 )) {stop("Values of the factor loadings should be between 0 and 1.
                                      Please re-run the function.")}
    return(fl.matx)
}
