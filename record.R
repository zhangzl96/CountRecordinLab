rm(list = ls())
options(stringsAsFactors = F, digits = 4)

## 从文件中获得姓名
GetName <- function(ifile){
  mydata <- read.csv(ifile, header = TRUE)
  name <- unique(mydata$X4)
}

## 对单个名字进行统计
CountTime <- function(name, ifile){
  xm <- name
  mydata <- read.csv(ifile, header = TRUE)
  sign <- data.frame(mydata[grep(xm, mydata$X4), (5)])
  colnames(sign) <- "ttime"
  
  ## 将日期时间拆分成日期和时间
  day_time <- strsplit(sign$ttime, " ")
  day  <- sapply(day_time, "[", 1)
  time <- sapply(day_time, "[", 2)
  
  ## 统计日期
  uday <- unique(day)
  gzts <- length(uday)
  fd <- as.character(as.Date(uday[1]))
  ld <- as.character(as.Date(uday[length(uday)]))
  
  ## 将打卡记录拆分成列表，每个值表示某一天的打卡记录数据框
  cday <- c()
  for (i in seq(1:length(uday))){
    cday[i] <- data.frame(sign[grep(uday[i], sign$ttime), ])
  }
  gzr <- c("星期一","星期二","星期三","星期四","星期五")
  counttime <- c()
  cd = 0
  ## 计算每天的打卡时长，生成含每天时长的数据向量
  for (i in seq(1:length(cday))){
    x <- as.POSIXct(cday[[i]][1])
    y <- as.POSIXct(cday[[i]][length(cday[[i]])])
    t <- gsub(":", "", strsplit(cday[[i]][1], split = " ")[[1]][2])
    counttime[i] <- difftime(y, x, units = "mins")
    if (format(as.Date(uday[i]), format = "%A") %in% gzr) {
      if (t > 830) cd = cd + 1  # 工作日迟到天数
    }
  }
  zsc <- sum(counttime)/60
  yxsc <- zsc - cd*3
  countresult <- data.frame(xm, fd, ld, gzts, zsc, cd, yxsc)
}

## 统计文件中所有人的打卡情况
Record <- function(inputfile){
  name <- GetName(ifile = inputfile)
  record <- data.frame()
  for (i in 1:length(name)){
    line <- CountTime(name = name[i], ifile = "sign.CSV")
    colnames(line) <- c("姓名", "第一天", "最后一天", "工作天数", "总时长", "迟到次数", "有效时长")
    record <- rbind(record, line)
  }
  write.table(record, file = "record.csv", sep = ",",row.names = FALSE)
}

Record(inputfile = "sign.CSV")