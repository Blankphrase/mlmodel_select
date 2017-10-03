## benckmark for combine the output results using both for and foreach, C code and apply functions later. 


# All matrix will be turned into data frame since creating a data frame is my interest.

# using foreach combine function for row combine
# n columns, m rows.
foreach_native_rcombine=function(n,m){
  
  library(foreach)
  
  result=foreach(i=1:m,.combine=rbind)%do%{
    return(rnorm(n))
  }
  
  result=as.data.frame(result)
  return(result)
}

# using foreach combine function for column combine. 
# n columns, m rows. 
foreach_native_ccombine=function(n,m){
  library(foreach)
  result=foreach(i=1:n,.combine =cbind )%do%{
    return(rnorm(m))
  }
  result=as.data.frame(result)
  return(result)
}


for_subscript_rcombine=function(n,m){
  library(foreach)
  result=matrix(NA,nrow = m,ncol = n)
  for(i in 1:m){
    result[i,]=rnorm(n)
  }
  result=as.data.frame(result)
  return(result)
}



for_subscript_ccombine=function(n,m){
  library(foreach)
  result=matrix(NA,nrow = m,ncol = n)
  for(i in 1:n){
    result[,i]=rnorm(m)
  }
  result=as.data.frame(result)
  return(result)
}

## There are three types of testing, when n << m or n >> m or n=m. 

## We will be using the microbenchmark package for benchmarking different sizes of data frame. 

library(microbenchmark)

library(magrittr)
foreach_native_ccombine(n=10,m=10)%>%dim
foreach_native_rcombine(n=10,m=10)%>%dim
for_subscript_rcombine(n=10,m=10)%>%dim
for_subscript_ccombine(n=10,m=10)%>%dim

# When n=m
nm10=microbenchmark(foreach_native_ccombine(n=10,m=10),foreach_native_rcombine(n=10,m=10),for_subscript_rcombine(n=10,m=10),for_subscript_ccombine(n=10,m=10),times=100)
nm100=microbenchmark(foreach_native_ccombine(n=100,m=100),foreach_native_rcombine(n=100,m=100),for_subscript_rcombine(n=100,m=100),for_subscript_ccombine(n=100,m=100),times=100)
nm1000=microbenchmark(foreach_native_ccombine(n=1000,m=1000),foreach_native_rcombine(n=1000,m=1000),for_subscript_rcombine(n=1000,m=1000),for_subscript_ccombine(n=1000,m=1000),times=100)
nm2000=microbenchmark(foreach_native_ccombine(n=2000,m=2000),foreach_native_rcombine(n=2000,m=2000),for_subscript_rcombine(n=2000,m=2000),for_subscript_ccombine(n=2000,m=2000),times=100)

# When n >> m
n1000m1=microbenchmark(foreach_native_ccombine(n=1000,m=1),foreach_native_rcombine(n=1000,m=1),for_subscript_rcombine(n=1000,m=1),for_subscript_ccombine(n=1000,m=1),times=100)
n1000m10=microbenchmark(foreach_native_ccombine(n=1000,m=10),foreach_native_rcombine(n=1000,m=10),for_subscript_rcombine(n=1000,m=10),for_subscript_ccombine(n=1000,m=10),times=100)
n1000m100=microbenchmark(foreach_native_ccombine(n=1000,m=100),foreach_native_rcombine(n=1000,m=100),for_subscript_rcombine(n=1000,m=100),for_subscript_ccombine(n=1000,m=100),times=100)
n2000m100=microbenchmark(foreach_native_ccombine(n=2000,m=100),foreach_native_rcombine(n=2000,m=100),for_subscript_rcombine(n=2000,m=100),for_subscript_ccombine(n=2000,m=100),times=100)

# when n << m 
n1m1000=microbenchmark(foreach_native_ccombine(m=1000,n=1),foreach_native_rcombine(m=1000,n=1),for_subscript_rcombine(m=1000,n=1),for_subscript_ccombine(m=1000,n=1),times=100)
n10m1000=microbenchmark(foreach_native_ccombine(m=1000,n=10),foreach_native_rcombine(m=1000,n=10),for_subscript_rcombine(m=1000,n=10),for_subscript_ccombine(m=1000,n=10),times=100)
n100m1000=microbenchmark(foreach_native_ccombine(m=1000,n=100),foreach_native_rcombine(m=1000,n=100),for_subscript_rcombine(m=1000,n=100),for_subscript_ccombine(m=1000,n=100),times=100)
n100m2000=microbenchmark(foreach_native_ccombine(m=2000,n=100),foreach_native_rcombine(m=2000,n=100),for_subscript_rcombine(m=2000,n=100),for_subscript_ccombine(m=2000,n=100),times=100)
