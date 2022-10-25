A <- matrix(c(4,-1,1,
              -1,4.25,2.75,
              1,2.75,3.50), nrow = 3, ncol = 3, byrow = T)

A
cholesky <- function(A)
{
  n <- nrow(A)
  L <- matrix(c(0),n,n)
  #step1
  L[1,1] <- sqrt(A[1,1])
  #step2

  L[2:n,1] <- A[2:n,1] / L[1,1]
  
  #step3
  for(i in 2:(n-1))
  {
    #step4
    L[i,i] <- sqrt((A[i,i]-sum(L[i,1:(i-1)]^2)))
    #step5
    for(j in (i+1):n)
    {
      L[j,i] <- (A[j,i]-sum(L[j,1:(i-1)]*L[i,1:(i-1)]))/L[i,i]
    }
  }
  #step6
  L[n,n] <- sqrt((A[n,n]-sum(L[n,1:(n-1)]^2)))
  
  
  
  
  list(L = L) 
}
cholesky(A)
