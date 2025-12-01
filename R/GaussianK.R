GaussianKernel = function(x,z,rho = 1){
  return(exp(-rho * sum((x - z)^2))) #K[i,j]
}
