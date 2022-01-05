Christmas Game Analysis
================

# Prerequisite

``` r
library(tidyverse)
```

# Statement

A family decided to play a different kind of Secret Santa on Christmas.
The rules are as follows:

-   Each participant buys a present, wraps it so nobody can see the
    content and places it together with the other presents.
-   A distinct integer number is given to each player indicating their
    turn.
-   On his turn, the player has two possibilities, to pick a gift from
    the pile, reveal its content and keep it, or to take a gift from one
    of the previous players. The participant who had his gift taken is
    obligated to pick another present from the pile.
-   The game keeps going until the player who was assigned the last
    number has played.

Suppose there are n people playing and your number is k.

**Constraints: n &gt; 3, 2 &lt;= k &lt;= n**

Consider two scenarios:

## 1 - Simplified Scenario

In this case nobody is going to take presents from you, which means the
present you pick from the pile or take from somebody will remain with
you until the end. Your strategy will be to take the best gift revealed
so far.

**Calculate the probability this strategy gives you a better present
than picking one from the pile.**

### 1.1 - Experiment

``` r
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
```

### 1.2 - Analysis

``` r
M %>% as_tibble %>%
  ggplot(aes(x=K, y=Prob))+
  geom_point()+
  geom_hline(yintercept = 0.5, colour="red")+
  ylim(c(0,1))
```

![](Analysis_Notebook_files/figure-gfm/simplified%20analysis-1.png)<!-- -->

The probabilities don’t depend on n. It is possible to notice the
probabilities of getting a better present taking from another player is
always greater or equal to 50%, which means it is a good strategy. Given
k=1, the probability is exactly 50%, result which can be proven
mathematically. Given k &gt; 1, the odds are always greater than 50%,
increasing with the increase of k.

------------------------------------------------------------------------

## 2 - Real Scenario

In this scenario players can take gifts from you and from each other.
Their plan will always be to take the best gift revealed so far. Knowing
their strategy, you reshaped yours. You will choose the best present
that you are sure nobody will take away, if that is not possible, you
will just take the worst present.

For instance:

-   Suppose n = 10 and k = 7. It is your turn; six players have played,
    there are still three more to go. If you take the best gift so far,
    one of the three players can take it from you, same goes for the
    second best and the third best. However, taking the fourth best you
    are sure you’ll keep it till the end, because the participants left
    will, in the worst-case scenario, take until the third best.
-   Suppose now that n = 10 and k = 3. It is your turn; two players have
    played, there are still seven left. You can’t use the strategy
    explained on the previous example, so you take the worst present so
    far, which means, the second best.

**Calculate the probability this strategy gives you a better present
than picking one from the pile.**

## 2.1 - Experiment

``` r
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
```

## 2.2 - Analysis

``` r
M %>% as_tibble() %>%
  filter(N %in% c(10,20,30)) %>%
  mutate(N=factor(N)) %>%
  ggplot(aes(x=K, y=Prob, colour=N, shape=N))+
  geom_point()+
  geom_hline(yintercept = 0.5,colour="red")+
  ylim(c(0,1))
```

![](Analysis_Notebook_files/figure-gfm/real%20analysis-1.png)<!-- -->

In this scenario the odds do depend on n. For a given n, the
probabilities decrease from 1 to n-k+1, when the strategy is to take the
worst present. From n-k+1 the odds start to increase due to the fact
that is possible to take the best present which can’t be taken from you.
These probabilities increase alongside k, because there are fewer and
fewer players, so it is possible to take better presents.
