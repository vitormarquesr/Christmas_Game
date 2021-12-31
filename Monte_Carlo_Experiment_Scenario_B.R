#Scenario B

#In this scenario, the probabilities
#are going to depend on n, so the program
#will simulate from 3 to nmax.
nmax = 30

#Matrix which will store the results
M <- matrix(0, ncol=3,nrow=(2+nmax-1)*(nmax-2)/2)
colnames(M) <- c("N", "K", "Prob")

#Line counter
y <- 0 

#Number of participants
for (n in 3:nmax){
  #Your number
  for (k in 2:n){
    #Number of runs (a number greater than 1000 would
    #slow down the program too much)
    r <- 1000
    
    #Number of runs taking someone else's gift is
    #better
    f <- 0
    
    #Experiment
    for (i in 1:r){
      #Array of gifts
      pre <- sample(1:n, n, replace=FALSE)
      
      #1 - Scenario in which you keep the present
      #assigned to you (You pick from the pile)
      pre2 <- pre
      if (k < n){
        #Going through the next players' turn
        for (t in (k+1):n){
          #The current player is going to take the best present
          #revealed so far. The participant who had his gift
          #taken will get the present which would have been 
          #given to the current player. We can use this trick
          #to help the coding  without loss of generality.
          
          i_max = which(pre2[1:t-1]==max(pre2[1:t-1]))
          v_max = max(pre2[1:t-1])
          pre2[i_max] = pre2[t]
          pre2[t] = v_max
        }
      }
      
      #2 - Scenario in which you take the (n-k+1) best gift if possible.
      #If not, you take the worst.
      if (n-k+1 >= k){
        #2.1 - You take the worst, it can be
        #stolen.
        v <- min(pre[1:k-1])
        i_min = which(pre[1:k-1]==v)
        pre[i_min] = pre[k]
        pre[k] = v
        for (t in (k+1):n){
          i_max = which(pre[1:t-1]==max(pre[1:t-1]))
          v_max = max(pre[1:t-1])
          pre[i_max] = pre[t]
          pre[t] = v_max
        }
        #The gift you end up with
        v <- pre[k]
        
      }else{
        #2.2 - Case in which you take the (n-k+1) best,
        #it is guaranteed no one will take it from you
        v <- sort(pre[1:k-1], decreasing=TRUE)[n-k+1]
      }
      #If the present you end up with using the strategy
      #is better than picking from the pile, increments f.
      if (v > pre2[k]){
        f <- f + 1
      }
    }
    y <- y + 1
    M[y,1] <- n
    M[y,2] <- k
    M[y,3] <- f/r
  }
}

