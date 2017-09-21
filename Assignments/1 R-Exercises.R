
#EXCERCISE --- 1

#Q1
(1:20)
(20:1)
c(1:20,19:1)
tmp <- c(4,6,3)
rep(tmp,10)
rep(tmp,l=31)
rep(tmp,c(10,20,30))

#Q2
tmp <- seq(3,6,by=0.1)
exp(tmp)*cos(tmp)

#Q3
(0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3))
(2^seq(1,25,by=1))/seq(1,25,by=1)

#Q4
sum(((seq(10,100,by=1))^3)+(4*(seq(10,100,by=1))^2))
sum(((2^(1:25))/(1:25))+((3^(1:25))/((1:25)^2)))

#Q5
paste("label",1:30,sep=" ")
paste("fn",1:30,sep="")

#Q6
x <- sample(0:999,250,replace = T)
y <- sample(0:999,250,replace = T)
y[2:length(x)]-x[1:length(x)-1]

(sin(y[1:(length(x)-1)]))/(cos(x[2:length(x)]))

x[1:(length(x)-2)] + 2*(x[2:(length(x)-1)]) - x[3:(length(x))]df


sum((exp(-x[(1:(length(x)-1))+1]))/(x[(1:(length(x)-1))]+10))



#Q7

y[y>600]

match(c(y[y>600]),y)

x[y>600]

sqrt(abs(x[1:length(x)]-mean(x)))

length(y[y>(max(y)-200)])

sum(x %% 2 == 0)

x[order(y)]

y[seq(1,length(y),by=3)]



#Q8

1+sum(cumprod(seq(2,38,by=2)/seq(3,39,by=2)))

##############################################################################################



#EXERCISES  ----- 2

#Q1
A <- matrix(c(1,1,3,5,2,6,-2,-1,-3),nr=3,byrow = TRUE)

A%*%A%*%A

A[3, ] = A[2,]+A[3,]

#Q2

B <0- matrix(rep(c(10,-10,10),15),nc=3,byrow = TRUE)
crossprod(B)

#Q3

C <- matrix(0,nr = 6,nc=6,byrow = TRUE)
C[ abs(col(C)-row(C))==1 ] <- 1
C

#Q4

outer(0:4,0:4,"+")

#Q5

outer(0:4,0:4,FUN = "+")%%5

outer(0:9,0:9,FUN="+")%%10

outer(0:8,0:8,FUN = "-")%%9

#Q6

y <- c(7,-1,-3,5,17)
A <- matrix(0,nr=5,nc=5,byrow = TRUE)
A <- abs(col(A)-row(A))+1
x <- solve(A,y)
A%*%x

#Q7

set.seed(75)
A <- matrix(sample(10,size = 60, replace = T),nr=6)

apply(A, 1, function(x){sum(x>4)})

which( apply(A,1,function(x){sum(x==7)==2}) )

cbind(rep(1:10,rep(10,10)),rep(1:10,10)) [outer(colSums(A),colSums(A),"+")>75]

ABC <- outer(colSums(A),colSums(A),"+")>75
ABC[lower.tri(ABC,diag=T)] <- F
which(ABC, arr.ind=T)

#Q8

sum((1:20)^4)*sum((1/(3+(1:5))))

sum((1:20)^4/(3+outer(1:20,1:5,"*")))

sum( outer(1:10,1:10,function(i,j){ (i>=j)*i^4/(3+i*j) }) )

##############################################################################################




#EXERCISE ----- 3

#Q1

x <- c(1:10)

tmpFn1 <- function(x)
{
  x^(1:length(x))
}
tmpFn1(x)


tmpFn2 <- function(x)
{
  (x^(1:length(x)))/(1:length(x))
}
tmpFn2(x)



tmpFn3 <- function(x,n)
{
  1+sum((x^(1:n))/(1:n))
}
tmpFn3(3,3)




#Q2

x[-c(1,2)]
x <- c(1:6)
n <- length(x)

tmpFn4 <- function(x)
{
  n <- length(x)
  ( x[ -c(n-1,n) ] + x[ -c(1,n) ] + x[ -c(1,2) ] )/3
}
tmpFn4(c(1:5,6:1))




tmpFn5 <- function(x)
{
  if(x<0)
  {
    (x^2) + (2*x) + 3
  }else
  if(x==0 || x>0 && x<2)
  {
    x+3
  }else
  if(x==2 || x>2)
  {
    (x^2) + (4*x) - 7
    
  }
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn5(tmp), type="l")


#Q4


tmpFn6 <- function(mat)
{
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat
}

asd <- matrix(c(1,1,3,5,2,6,-2,-1,-3),nr=3,byrow = TRUE)
tmpFn6(asd)



#Q5


tmp <- diag(2, nr = 5)
tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
tmp

tmpFn7 <- function(n,k)
{
  tmp <- diag(k,nr=n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}
tmpFn7(5,2)




#Q6


quadrant <- function(alpha)
{
  1 + (alpha%%360)%/%90
}

quadrant(24)


#Q7

#(a)
tmpFn8 <- function(d,m,y)
{
  flag <- m <=2  
  m <- m -2 + 12*flag
  y <- y - flag
  cc <- y %/% 100
  y <- y %% 100
  tmp <- floor(2.6*m -0.2)+d+y+y%/%4+cc%/%4-2*cc 
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") [1+tmp%%7]
}
tmpFn8(02,10,1988)
c( tmpFn8(27,2,1997), tmpFn8(18,2,1940), tmpFn8(21,1,1963) )
  
#(b)
#yes


#Q8

#(a)

testloop <- function(n)
{
  if (n<4)
    stop("This argument n must be an integer which is at least 4. \n")
  x <- rep(NA,n-1)
  x[1]<-1
  x[2]<-2
  for(j in 3:(n-1))
    x[j] <- x[j-1] + (2/(x[j-1]))
  x
}
testloop(3)

#(b)
y <- (1:10)
y
testloop2 <- function(y)
{
  n <- length(y)
  sum(exp(seq(along=y)))
}

testloop2(y)


#Q9

#(a)

quadmap <- function(start, rho, niter)
{
  x <- rep(NA,niter)
  x[1] <- start
  for (i in 1:(niter-1))
  {
    x[i+1] <- rho*x[i] * (1-x[i])
  }
  x
}
tmp <- quadmap(0.95,2.99,500)

plot(tmp[300:500], type="l")

#(b)

quad2 <- function(start, rho, eps = 0.02)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho*x1*(1 - x1)
    niter <- niter + 1
  }
  niter
}

quad2(0.95,2.99)


#Q10

#(a)

tmpAcf <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}

xVec <- seq(2,56,by=3)
xVec
tmpAcf(xVec)


#(b)

tmpAcf <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}
  
tmpAcf(xVec,8)





####################################################################################################



#EXERCISE  -----------  4    Harder functions

#Q1

#(a)

q1a <- function(x,y)
{
  colSums(outer(y,x,"<"))
}
x <- 1:20
Y <- 21:30
x
q1a(x,y)


#(b)

q1b <- function(xVec, yVec){
  rowSums( sapply(yVec, FUN=function(y){y < xVec}) )
}

q1b(x,y)

#(c)

q1c <- function(xVec, yVec){
  rowSums( vapply(yVec, FUN=function(y){y<xVec}, FUN.VALUE=seq(along=xVec)) )
}

q1c(x,y)

#(d)

q1d <- function(xVec,yVec)
{
  leny <- length(yVec)
  mat <- matrix(rep(xVec,leny), byrow=T, nrow=leny)
  apply( yVec<mat, 2, sum )
}
q1d(x,y)
x <- c()
x
y <-c()
y

#Both q1b and q1d fail if either xVec or yVec has length 0; but at least they do not give incorrect
#answers which would be far worse. Both q1a and q1d fail if x and y are matrices.


#(e)

rjr1 <- rnorm(10000)
rjr2 <- rnorm(12000)
system.time(q1a(rjr1,rjr2))
system.time(q1b(rjr1,rjr2))
system.time(q1c(rjr1,rjr2))
system.time(q1d(rjr1,rjr2))


#Q2

#(a)

tmpFn <- function(mat){
  mat[, !apply(is.na(mat), 2, any), drop = F]
}

#(b)

tmpFn2 <- function(mat){
  mat[!apply(is.na(mat), 1, any), !apply(is.na(mat), 2, any), drop = F]
}


#Q3

#(a)
empCopula <- function( u, v, xVec, yVec )
{
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  sum( (rVecN <= u) & (sVecN <= v) ) /n
}


#(b)

empCopula1 <- function( u, v, xVec, yVec )
{
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  valuesN <- mapply( FUN=function(u1,v1){ sum((rVecN<=u1)*(sVecN<=v1)) }, u, v )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}



#Q4

#(a)

funA <- function (n)
{
  su <- 0
  for(r in 1:n)
  {
    for(s in 1:r)
      su <- su+s^2/(10+4*r^3)
  }
  su
}


#(b)


funB <- function (n)
{
  mat <- matrix(0, ncol=n, nrow=n)
  sum( (col(mat)^2)/(10+4*row(mat)^3)*(col(mat)<=row(mat)) )
}


#(C)
funC <- function (n)
{
  sum( outer(1:n,1:n,FUN=function(r,s){ (s<=r)*(s^2)/(10+4*r^3) }) )
}


#(d)
funD <- function (n)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(sapply(1:n, FUN=tmpfn))
}
funE <- function (n)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(unlist(lapply(1:n, FUN=tmpfn)))
}

#(e)

funF <- function (n)
{
  tmpf <- function(s,r){(s^2)/(10+4*r^3)*(s<=r)}
  sum(mapply(tmpf, rep(1:n, times=rep(n,n)), 1:n))
}
#The fastest are funE and funD, but funB and funC are also quite fast. The function funA is much slower
#and funF is even slower!
 
#Q5


#(a)
queue2 <- function(n, aRate, sRate)
{
  w <- 0
  s <- rexp(n, sRate)
  a <- rexp(n, aRate)
  for(i in 1:n){
    w <- max(0, w+s[i]-a[i])
  }
  w
}

#(b)
queueRep1 <- function (nReps, n, aRate, sRate)
{
  wVec <- rep(NA, nReps)
  for(j in 1:nReps)
    wVec[j] <- queue2(n, aRate, sRate)
  wVec
}
queueRep2 <- function (nReps, n, aRate, sRate)
{
  sapply( rep(n,nReps), queue2, aRate, sRate )
}

#(c)

queueRep3 <- function (nReps, n, aRate, sRate)
{
  w <- rep(0, nReps)
  s <- matrix(rexp(n*nReps, sRate), ncol=nReps)
  a <- matrix(rexp(n*nReps, aRate), ncol=nReps)
  for(i in 1:n){
    w <- pmax(0, w+s[i,]-a[i,])
  }
  w
}


#Q6

#(a)

rwalk <- function(n)
{
  c( 0, cumsum(sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))) )
}

#(b)

rwalkPos <- function(n)
{
  rw <- cumsum(c(0, sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))))
  sum( (rw[-(n+1)] + rw[-1]) > 0 )
}

#(c)
rwalkPos1 <- function(nReps, n)
{
  results <- rep(NA, nReps)
  for(i in 1:nReps)
    results[i]<-rwalkPos(n)
  results
}
rwalkPos2 <- function(nReps, n)
{
  replicate( nReps, rwalkPos(n) )
}
  
  
#(d)
rwalkPos3 <- function(nReps, n)
{
  stepWalks <- matrix( sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5)), nr=nReps )
  for(j in 2:n)
    stepWalks[,j] <- stepWalks[,j] + stepWalks[,j-1]
  stepWalks <- cbind(0, stepWalks)
  rowSums( stepWalks[,1:n] + stepWalks[,2:(n+1)]>0 )
}


###############################################################################################


#EXERCISE -------------  5 Dataframe

#Q1
#(a)
tsEwma <- function( tsDat, m0=0, delta=0.7)
{
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=start(tsDat), frequency=frequency(tsDat))
}


#(b)

tsEwma2 <- function( tsDat, m0=0, delta=0.7)
{
  tsPars <- tsp(tsDat)
  tsDat <- c(tsDat)
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=tsPars[1], frequency=tsPars[3])
}

#Q2

#(a)

myListFn <- function(n)
{
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}

#(b)

myList <- lapply( rep(10,4), myListFn )

myMatrix <- sapply( rep(10,4), myListFn )


#(C)
myList <- lapply( rep(10,1000), myListFn )
lapply(myList, FUN=function(x){x[[2]]})
lapply(myList, FUN="[[", 2)
lapply(myList, FUN="[[", "yVec")


# (d)

sapply(myList, FUN="[[", 2)
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), 2)
sapply(myList, FUN=function(x){x[[2]]})
vapply(myList, FUN=function(x){x[[2]]}, FUN.VALUE=rep(0,10))
sapply(mList, FUN="[[", "yVec")
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), "yVec")


#(e)

myList2 <- lapply(myList, function(x){list(xVec=x$xVec, yVec=x$yVec)})

#(f)
which( unlist( lapply(myList, function(x){x[[3]]>2}) ) )
myList[which( unlist(lapply( myList, function(x){x[[3]]>2} )) )]


#Q3

#(a)
partA <- sapply(myList, function(x){ sum(x$xVec*(1:10))/sum(x$yVec*(1:10)) })


#(b)

myMat <- t(sapply( myList, function(x){x$xVec-x$yVec}))
myMat2 <- matrix( unlist( lapply(myList, FUN="[[",1) ) -
                    unlist( lapply(myList, FUN="[[",2) ), nc=10, by=T )
myMat3 <- matrix( unlist(lapply(myList, function(x){x$xVec-x$yVec})), nc=10, by=T )

#(C)

sum(sapply(myList, function(x){x$xVec[2]})*(1:1000)) /
  sum(sapply(myList, function(x){x$yVec[2]})*sapply(myList, function(x){x$count}))


#Q4

#(a)

testFn2 <- function(xArray)
{
  wArray <- sweep(testArray, c(2,3), apply(testArray, c(2,3), min))
  zArray <- apply(testArray, c(2,3), FUN=function(x){ sum(x) - max(x)})
  list(wArray=wArray, zArray=zArray)
}


#(b)

testFn <- function( xArray)
{
  apply( apply(xArray, c(1,2), FUN=function(x){x^(1:length(x))}), c(3,1), sum )
}


#Q5

drawA <- function(X)
{
  lines(X[1:3,1], X[1:3,2])
  lines(X[4:5,1], X[4:5,2])
}
plot(c(-10,10), c(-10,10), ann=F, type='n')
#(a)

shift <- function(X,a,b){
  X[,1] <- X[,1] + a
  X[,2] <- X[,2] + b
  X
}

#(b)

rotate <- function(X,r){
  X%*%matrix(c(cos(r), -sin(r), sin(r), cos(r)), nrow = 2)
}

A <- cbind(c(0,1,2,4/9,14/9), c(0,3,0,4/3,4/3))

#(c)

arrayA<-vapply(1:25,
               FUN=function(i){
                 rotate(A,2*pi*(i-1)/24)
               },
               matrix(0,nrow=5, ncol=2)
)

plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
  


plot(arrayA[2,1,], arrayA[2,2,])


plot(1:25, arrayA[2,1,])



#(d)

scale <- function(X,a,b){
  X%*%matrix(c(a,0,0,b), nrow=2)
}
arAscaled <- vapply(1:25,
                    FUN=function(i){
                      scale(arrayA[,,i],2,3)
                    },
                    matrix(0,nrow=5, ncol=2)
)
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
invisible(sapply( 1:25, FUN=function(i){ drawA(arAscaled[,,i]) } ))


#(e)

arArandom <- array(0, dim=c(5,2,25))
arArandom[,,1] <- A
for(i in 2:25){
  arArandom[,,i] <-
    shift(
      rotate(  
        scale(arArandom[,,i-1], runif(1,0.5,1.5),runif(1,0.5,1.5)),
        2*pi*runif(1,-1,1)
      ),
      runif(1,-1,1), runif(1,-1,1)
    )
}
oopt = ani.options(interval = 0.2, nmax = 25)
for (i in 1:ani.options("nmax")){
  plot(c(-10,10), c(-10,10), ann=F)
  drawA(arArandom[,,i])
  ani.pause()
}



###################################################################################################################


