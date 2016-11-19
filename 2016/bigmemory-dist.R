createDistanceMatrix <- function(Inventory, blockSize = 20, Directory= getwd(), filename="distance.bin"){
  # based on code by Nick Stokes
  
  require(bigmemory)
  options(bigmemory.allow.dimnames=TRUE)
  if(!grepl("\\.bin",filename)) stop("please use a bin extension")
  columnsPresent <- intersect(colnames(Inventory),c("Id","Lon","Lat"))
  if(length(columnsPresent) != 3) stop("missing the correct column names")
  descName <- sub("bin","desc",filename)
  if(class(Inventory) == "matrix") Inventory <- as.data.frame(Inventory)
  
  L <- cbind(Inventory$Lat,Inventory$Lon)*(pi/180)
  s <- cbind(cos(L),sin(L))
  # z is the array of 3D coords of stations. All unit vecs
  z <- cbind(s[,1]*s[,2],s[,1]*s[,4],s[,3])
  
  z2 <- z/sqrt(2) # Better to mult now than after outer prod
  D <- 6371*2 #Diam of Earth
  
  n <- nrow(L)
  if(n <= blockSize)stop("BlockSize must be less than rows in Inventory")
  blocks <- n %/% blockSize
  if((n %% blockSize) > 0)blocks <- blocks + 1
  dex <- 1:blockSize
  
  BM <- filebacked.big.matrix(nrow = n , ncol = n,
                              dimnames = list(as.list(Inventory$Id),NULL),
                              init = NA,
                              backingpath = Directory, 
                              backingfile = filename, 
                              descriptorfile = descName, 
                              type = "double")
  startTime <- Sys.time()
  
  for(i in 1:blocks){
    
    p <- dex + (i-1)*blockSize
    p <- p[p<= n]
    for(j in 1:blocks){
      q <- dex +(j-1)*blockSize
      q <- q[q<=n]
      x <- 0.5000000001-z2[p,]%*%t(z2[q,])
      
      x <- D * asin(sqrt(x)) 
      if(identical(p,q))diag(x)<-0
      BM[p,q]<- x
      
    }
  }
  
  print(Sys.time()-startTime)
  return(BM)
}

