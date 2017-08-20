avgInterval <- function(d, per, pername){
  #根据avg计算各区间点的值
  # per <- c(0.125,0.25,0.5,0.75,1,1.25,1.5,2,4)   #avg倍数
  # pername <- c("u0125","u025","u05","u075","u1","u125","u15","u2","u4")  #对应列明

  for(i in 1:nrow(d)){
    a <- round(d$mean[i]*per,3)
    a <- matrix(a,nrow = 1, ncol = length(a))
    colnames(a) <- pername
    a <- as.data.frame(a)
    a$min <- d$min[i]
    a$max <- d$max[i]
    if(i==1){
      avgd <- a
    }else{
      avgd <- rbind(avgd,a)
    }
  }
  
  avgd <- avgd[,c(10,1:9,11)]
  
  intervalName <- matrix(0, nrow = nrow(avgd), ncol = ncol(avgd)-1)
    
  for(i in 1:nrow(avgd)){
    for(j in 2: ncol(avgd)){
      if(avgd[i,j-1]<10){
        a <- paste0("a",avgd[i,j-1],"~",avgd[i,j])
      }else if(avgd[i,j-1]<20){
        a <- paste0("b",avgd[i,j-1],"~",avgd[i,j])
      }else{
        a <- paste0("c",avgd[i,j-1],"~",avgd[i,j])
      }
      
      intervalName[i,j-1] <- a
    }
  }
  res <- list(interval=avgd, intervalName=intervalName)
  return(res)
}
