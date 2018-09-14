#STUDENT NAME: Muhammad Daud                                                                                                   *#
#LIUID: muhda846

euclidean <- function(a, b)
{
  rk_1 <- a;
  rk_2 <- b;
  # Recurrence Formula:  r_k =  r_k-1 modulo r_k-2
  # Increment k until r_k-2 == 0 
  while(rk_2 != 0) {
    rk      <- rk_1%%rk_2; # remainder
    rk_1    <- rk_2;       # proceed in recurrence
    rk_2    <- rk;
  }
  return(rk_1)
}
