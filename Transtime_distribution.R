transtime_dis <- function(xlsxname, n, fpath){
  ########给定固定文件大小区间，统计trans_time分布占比,根据percent评分
  ###注释
    #fname ： xlsx数据文件 
    #k ：节点个数
    
  
   
  # library(openxlsx)
  # library(dplyr)
  # library(plyr)
  # library(cluster)
  # library(randomForest)
  
  fpath <- "E:/Rfile/170818"
  xlsxname <- "zj_5M_10M_0904.xlsx"
  n=4
  
  # setwd(fpath)
  D <- list()
  for (i in 1:n){
  
    d <- read.xlsx(paste0("data/",xlsxname),i)[,c(2,5:32)]
    colnames(d) <- c("time","monitor","monitorIp","visit",
                     "file_size","trans_time",colnames(d)[7:29])
    
    d$group <- substr(d$time, 1, 2)
    
    # a <- floor(as.numeric(substr(d$time, 4, 5)) / 6)  #Group by hour (6hours)
    # d$group <- paste0(substr(d$time,1,3),a)
    d <- d[,c(1,30,2:29)]
    
    ###set filesize: delete NA
    d <- d[which(d$file_size == 5123.25),]
   
    d$trans_time <- round(as.numeric(d$trans_time),3)
    t <- which(is.na(d$trans_time))
    if(length(t)>=1){
      d <- d[-t,]
    }  
    
    t <- which(d$trans_time==0)
    if(length(t)>=1){
      d <- d[-t,]
    } 
    
    d$retrans_rate <- round(d$retrans_bytes/d$trans_bytes,3)
    d <- d[,c(1:10,31,11:30)]
    D <- c(D,list(d))
  }
  
  load(paste0(fpath,"/rf_model.Rdata"))  #加载预测模型
  Pdata <- list()
  for(i in 1:n){
    test <- data.frame(D[i])
    
    pred_y <- round(predict(model.rf, test),3)
    pred <- data.frame(time=test$time, group=test$group, visit=test$visit, 
                       file_size=test$file_size, real_y=test$trans_time, pred_y=pred_y)
    if(i==1){
      DP <- pred
    }else{
      DP <- rbind(DP, pred)
    }
    # assign(paste0("pred", i), pred)
    Pdata <- c(Pdata,list(pred))
  }
  
  
  ############################################################################################
  ###################################################
  
    
  ##count percent by group on pred
  a=0.1
  p <- aggregate(DP$pred,list(DP$group),function(x){
    return(quantile(x,a))
  })
  
  for (j in 2:9) {
    a=a+0.1
    p[,j+1] <- aggregate(DP$pred,list(DP$group),function(x){
      return(quantile(x,a))
    })[,2]
    
  }
  names(p) <- c("groupH","10%","20%","30%","40%","50%","60%","70%","80%","90%")
  
  
  ##count percent by group on real
  a=0.1
  r <- aggregate(DP$real,list(DP$group),function(x){
    return(quantile(x,a))
  })
  
  for (j in 2:9) {
    a=a+0.1
    r[,j+1] <- aggregate(DP$real,list(DP$group),function(x){
      return(quantile(x,a))
    })[,2]
    
  }
  names(r) <- c("groupH","10%","20%","30%","40%","50%","60%","70%","80%","90%")


  #####transtime -> score
  score <- c(10:1)
  for (i in 1:n){
    d <- as.data.frame(Pdata[i])
    d$score1 <- 1
    d$score2 <- 1
    groups <- unique(d$group)
    for(j in 1: length(groups)){  # nrow(r) : 时间组个数
      #  根据时间组和传输时间范围评分
      t <-  d[which(d$group==groups[j]),]  #把同一时间组的取出来
      for(jj in 2:ncol(r)){  #ncol(r)：trans_time 区间划分个数
        if(jj==2){
          tr <- which(t$real_y < r[j,jj]) #实际值情况
          tp <- which(t$pred_y < p[j,jj]) #预测值情况
          
          t$score1[tr] <- score[1]
          t$score2[tp] <- score[1]
        }else{
          tr <- which(t$real_y>=r[j,(jj-1)] & t$real_y < r[j,jj])
          tp <- which(t$pred_y>=p[j,(jj-1)] & t$pred_y < p[j,jj])
          
          t$score1[tr] <- score[jj-1]
          t$score2[tp] <- score[jj-1]
        }
      }
      d[which(d$group==groups[j]),] <- t
    }
    Pdata[i] <- list(d)
  }
  
  ######
  

}

