#' @title Predict method for KRR Model
#' @description Computes predicted values for new data using a fitted KRR model.
#'
#' @param model An object of class 'krr'.
#' @param xnew Matrix or data frame. New input data for prediction.
#'
#' @return A vector (or matrix) of predicted values
#' @export
predict.krr = function(model, xnew, ...){
  x = model$x
  n = nrow(x)

  nnew = nrow(xnew)
  knew = matrix(0,ncol=n, nrow = nnew)

  for (i in 1:nnew){
    for (j in 1:n){
      knew[i,j] = GaussianKernel(xnew[i,],x[j,], rho = model$rho)
    }
  }

  fhat = knew %*% model$alpha
  return(fhat)
}
