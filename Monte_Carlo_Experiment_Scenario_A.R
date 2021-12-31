#Scenario A

#Number of players. The probability doesn't
#depend on n, it is only going to define the maximum k 
n = 30

#Matrix which is going to store the values
#of K and its probabilities.
M <- matrix(0, ncol=2,nrow=n-1)
colnames(M) <- c("K", "Prob")

#Your number, 2 <= k <= n
for (k in 2:n){
  #Runs
  r <- 10000
  
  #Number of runs taking the present from another
  #player is better
  f <- 0
  
  for (i in 1:r){
    #The array of the gifts
    pre <- sample(1:n, n, replace=FALSE)
    #If the present taken is greater than
    #the one picked from the pile.
    if (max(pre[1:k-1]) > pre[k]){
      f <- f +1
    }
  }
  M[k-1,1] <- k
  M[k-1,2] <- f/r
}

M