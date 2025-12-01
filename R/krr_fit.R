#' @title Fit KRR Model
#' @description Fits a Kernel Ridge Regression (KRR) model to the training data.
#'
#' @param x Matrix or data frame. training data.
#' @param y Vector. The response data.
#' @param rho Numeric. Parameter for the Gaussian kernel.
#' @param lambda Numeric. Regularization parameter.
#'
#' @return An object of class 'krr' containing the estimated coefficients.
#' @export
krr = function(x, y, rho = 1, lambda = 0.0001){
  n = nrow(x)
  K = matrix(0,ncol = n, nrow = n)

  for (i in 1:n){
    for (j in 1:n){
      K[i,j] = GaussianKernel(x[i,],x[j,],rho = rho)
    }
  }

  alpha = solve(K + diag(lambda ,n), y)

  model = list('alpha' = alpha, "rho" = rho, "x" = x, "y" = y, 'K' = K)
  class(model) = "krr"
  return(model)
}
