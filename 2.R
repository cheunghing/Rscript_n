library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)
mainpath<-"D:/Rworkplace"##存储路径
date<-seq(from='20181012',to='20181018',by=1)
result<-data.frame()
for (i in as.character(date)) {
  result <- rbind(result,read.csv(paste(mainpath,"/hist_data/",format(as.Date(i,format='%Y%m%d'),format='%Y%m%d'),"-",format(as.Date(i,format='%Y%m%d')+1,format='%Y%m%d'),"-1.csv",sep = '')
,stringsAsFactors=FALSE))

}

result<-subset(result,result$status!='PROCESSING')

result[which(result$channelName == '微信JS支付'), 16] <- '微信JS'
result[which(result$channelName == '微信JS支付'), 14] <- 'weixinJS'
unique_bank_list <-result[!duplicated(result$bankCode), c(14, 16)]##银行名称合并之生成银行唯一列表

result <-  ##银行名称合并之关联银行唯一列表
  merge(
    result,
    unique_bank_list,
    by.x = c('bankCode'),
    by.y = c('bankCode'),
    all.x = T
  )


result$crea_time <-
  as.POSIXct(as.numeric(result$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$req_time <-
  as.POSIXct(as.numeric(result$requestDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$noti_time <-
  as.POSIXct(as.numeric(result$notifyTime) / 1000, origin = "1970-01-01 00:00:00")
result$noti_time[which(!result$status %in% c('SUCCESS', 'FAIL'))] <-
  Sys.time()
result$take_time <- result$noti_time - result$req_time

for (i in normal_fail) {#####定义正常失败
  result[grepl(i, result$returnInfo) &
           result$status == 'FAIL', 33] <- '失败(余额不足)'
  
}

result[result$status == 'FAIL', 33] <- '失败(异常)'
result[result$status == 'SUCCESS', 33] <- '成功'
result[result$status == 'PROCESSING', 33] <- '处理中'



result<-subset(result,result$transType%in%c('1'))#####再次筛选实时:1 or批扣:4

result <-
  result[format(result$crea_time, format = '%Y%m%d') %in% format(as.Date(as.character(date[1]), format =
                                                                           '%Y%m%d'), format = '%Y%m%d'):format(as.Date(as.character(date[length(date)]), format =
                                                                                                                                     '%Y%m%d'), format = '%Y%m%d'), ]


result[as.numeric(gsub('-','',substr(result$crea_time,1,10)))>=20180929,]$notifyUrl<-'NA'
result[is.na(result$notifyUrl),]$notifyUrl<-'NA'
result<-subset(result,result$notifyUrl=='NA')###把混入的批扣交易筛去

cat('原始数据整理完毕\n')

res<-aggregate(result$transSeqno,list(substr(result$crea_time,1,10),result$status),length)
res_1<-dcast(res,Group.1~Group.2,value.var ='x')
res_1$总数<-res_1$成功+res_1$`失败(异常)`+res_1$`失败(余额不足)`
res_1$成功率<-res_1$成功/res_1$总数
res_1$`失败(余额不足)率`<-res_1$`失败(余额不足)`/res_1$总数
res_1$`失败(异常)率`<-res_1$`失败(异常)`/res_1$总数
res_2<-melt(res_1,id.vars=c("Group.1","成功","失败(异常)","失败(余额不足)",'总数'))

png(filename = paste(mainpath,"/pic/",date[length(date)],"交易量趋势.png",sep=''),width = 600*1.35,
    height = 400*1.35)
ggplot()+geom_histogram(data=result,aes(x=substr(result$crea_time,1,10),fill=reorder(result$status, rep(1, length(result$status)), sum)),stat = 'count',alpha=0.9)+labs(title = paste('交易量', date[1], '至', date[length(date)]),
  x = '日期',
  y = '笔数',
  fill = '结果')+ theme(text = element_text(family = 'STXihei', size = 19)) 
dev.off()


png(filename = paste(mainpath,"/pic/",date[length(date)],"指标趋势.png",sep=''),width = 600*1.35,
    height = 400*1.35)
ggplot(data = res_2,aes(
  x = res_2$Group.1,
  y = res_2$value
  
)) + geom_line(
  
  aes(
   
    group = res_2$variable,
    
    color = res_2$variable
    
  ),
  size = 1
) + labs(
  title = paste('支付指标趋势', date[1], '至', date[length(date)]),
  x = '日期',
  y = '比率',
  color = '指标'
)+geom_text(data=res_2 ,aes(label = paste(100*round(res_2$value,4),'%')), vjust = 1.5, colour = "black", position = position_dodge(.9), size = 5)+ theme(text = element_text(family = 'STXihei', size = 16)) 
dev.off()

