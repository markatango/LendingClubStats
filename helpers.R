allAreNA <- function(x){
  length(x) == sum(is.na(x)*1)
}

correctForAllAreNA <- function(x){
  res <- x
  if(allAreNA(x)){
    res <- rep(0,length(x))
  }
  res 
}

mode.1 <- function(x){
  if(anyNA(x)){
    x <- sapply(x,function(y){
      res <- y
      if(is.na(res)) res <- 0
      as.numeric(res)
    })
    #stop("mode.1: x contains an NA")
  }
  if(!(length(x)>0)){
    stop(paste("mode.1: input vector length = 0"))
  }
  if(!(class(x)=="numeric")){
    stop(paste("mode.1: input vector is not numeric"))
  }
  
  temp <- table(as.vector(x))
  res <- names(temp)[temp == max(correctForAllAreNA(temp), na.rm=TRUE)][1]
  out <- ""
  if (length(res)!=1){
    stop(paste("mode.1 returned result length != 1; length = ",length(res)))
  }
  list(res,out)
}

mode.2 <- function(x){
  if(!allAreNA(x) && !length(x)==0){
    temp <- table(x)
    tryCatch(
      m <- max(temp),
      error = function(e){print(paste0("x: ",x))}
    )
    names(temp)[temp==max(temp)[1]]
  } else {
    "JAN1970"
  }
  
}

ficoSlopes2 <- function(n){  
  if(!is.numeric(n)) stop("ficoSlopes2: n input not numeric")
  if(length(n) != 1) stop("ficoSlopes2: n input length != 1")
  if (n < 1 || n > 100) stop("ficoSlopes2: n not in [1,100]")
  
  res <-rep(0,n)
  cum <-rep(0,n)
  
  function(x){
    if(!is.numeric(x)) stop("ficoSlopes2: x input is not numeric")
    xLen <- length(x)
    if(xLen > 1) {
      
      res[1] <<- x[1]
      cum[1] <<- 0
      for (i in 2:min(n,xLen)){
        res[i] <<- ifelse(is.na(x[i]-x[i-1]),0,x[i]-x[i-1])
        cum[i] <<- cum[i-1]+res[i]
      }
      if(n>xLen){
        for(i in xLen:n){
          res[i] <<- res[xLen]
          cum[i] <<- cum[xLen]
        }
      }
    } 
    res <<- res[2:n]
    cum <<- cum[2:n]

    res <<- fixNA(res)
    cum <<- fixNA(cum)
    
    names(res) <- paste0("dFico",1:length(res))
    names(cum) <- paste0("cumFico",1:length(cum))
    list(res=res, cum=cum)
  }
}


# count contiguous OK months from start of loan
countContOK <- function(x){
  ccOK <- 0
  x.len <- length(x)
  i <- 1 
  while( (x[i]=="Current" || x[i]=="Fully Paid") && i < x.len){
    i <- i+1
    ccOK <- ccOK+1
  }
  ccOK
}


# fix NAs with Zeros
fixNA <- function(x){
  
  if(is.numeric(x)){
    ret <- na.roughfix(x)
  } else {ret <- x}
  ret
}
