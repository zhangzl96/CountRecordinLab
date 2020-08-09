rm(list = ls())
options(stringsAsFactors = F, digits = 4)

## ���ļ��л������
GetName <- function(ifile){
  mydata <- read.csv(ifile, header = TRUE)
  name <- unique(mydata$X4)
}

## �Ե������ֽ���ͳ��
CountTime <- function(name, ifile){
  xm <- name
  mydata <- read.csv(ifile, header = TRUE)
  sign <- data.frame(mydata[grep(xm, mydata$X4), (5)])
  colnames(sign) <- "ttime"
  
  ## ������ʱ���ֳ����ں�ʱ��
  day_time <- strsplit(sign$ttime, " ")
  day  <- sapply(day_time, "[", 1)
  time <- sapply(day_time, "[", 2)
  
  ## ͳ������
  uday <- unique(day)
  gzts <- length(uday)
  fd <- as.character(as.Date(uday[1]))
  ld <- as.character(as.Date(uday[length(uday)]))
  
  ## ���򿨼�¼��ֳ��б���ÿ��ֵ��ʾĳһ��Ĵ򿨼�¼���ݿ�
  cday <- c()
  for (i in seq(1:length(uday))){
    cday[i] <- data.frame(sign[grep(uday[i], sign$ttime), ])
  }
  gzr <- c("����һ","���ڶ�","������","������","������")
  counttime <- c()
  cd = 0
  ## ����ÿ��Ĵ�ʱ�������ɺ�ÿ��ʱ������������
  for (i in seq(1:length(cday))){
    x <- as.POSIXct(cday[[i]][1])
    y <- as.POSIXct(cday[[i]][length(cday[[i]])])
    t <- gsub(":", "", strsplit(cday[[i]][1], split = " ")[[1]][2])
    counttime[i] <- difftime(y, x, units = "mins")
    if (format(as.Date(uday[i]), format = "%A") %in% gzr) {
      if (t > 830) cd = cd + 1  # �����ճٵ�����
    }
  }
  zsc <- sum(counttime)/60
  yxsc <- zsc - cd*3
  countresult <- data.frame(xm, fd, ld, gzts, zsc, cd, yxsc)
}

## ͳ���ļ��������˵Ĵ����
Record <- function(inputfile){
  name <- GetName(ifile = inputfile)
  record <- data.frame()
  for (i in 1:length(name)){
    line <- CountTime(name = name[i], ifile = "sign.CSV")
    colnames(line) <- c("����", "��һ��", "���һ��", "��������", "��ʱ��", "�ٵ�����", "��Чʱ��")
    record <- rbind(record, line)
  }
  write.table(record, file = "record.csv", sep = ",",row.names = FALSE)
}

Record(inputfile = "sign.CSV")