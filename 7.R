rm(list = ls())
library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)
setwd("D:/Rworkplace")

start_time <- '2018-10-24+00:00:00'
end_time <-
  '2018-10-25+00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
loop_time <- NA
start_num <- 0
length <- 5000
draw <- '1'
result <- data.frame()
handle <-
  getCurlHandle(
    httpheader = list(
      Accept = 'application/json, text/javascript, */*; q=0.01',
      'Accept-Ecoding' = 'gzip, deflate',
      'Accept-Language' = 'zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=5F888C1F8A9FC704CCC6FF4860A8CD6D; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=dbd747f4766357ee4c44871c56b4fbf4; userId=s00580; userType=CBUSER',
      Referer = 'http://172.18.32.14:8080/pcs-oms-new/payment/transOrderInfo/list?token=3cf075a6f585a7e414a08e794d777509&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )

split <- ##耗时分区
  c(
    -Inf,
    3,
    10,
    30,
    60,
    60 * 3,
    60 * 5,
    60 * 10,
    60 * 20,
    60 * 30,
    60 * 60,
    3600 * 2,
    3600 * 3,
    3600 * 6,
    3600 * 12,
    3600 * 24,
    +Inf
  )
label <-
  c(
    '3s以下',
    '大于3s小于10s',
    '大于10s小于30s',
    '大于30s小于1min',
    '大于1min小于3min',
    '大于3min小于5min',
    '大于5min小于10min',
    '大于10min小于20min',
    '大于20min小于30min',
    '大于30min小于1hour',
    '大于1hour小于2hour',
    '大于2hour小于3hour',
    '大于3hour小于6hour',
    '大于6hour小于12hour',
    '大于12hour小于1day',
    '大于1day'
  )

split_amt <- ##交易金额分区
  c(-Inf,
    100,
    600,
    1000,
    3000,
    5000,
    10000,
    15000,+Inf)
label_amt <-
  c(
    '[0,100]',
    '[100,600]',
    '[600,1000]',
    '[1000,3000]',
    '[3000,5000]',
    '[5000,10000]',
    '[10000,15000]',
    '[15000,+Inf]'
  )
normal_fail <-
  c('.*余额.*不足.*', '.*额度不足.*', '订单关闭成功', '订单未支付', '订单已关闭')
getdata <-
  function(s_time,
           e_time,
           start_num,
           length,
           handle,
           draw) {
    form <- c(
      'columns[0][data]' = 'transSeqno',
      'columns[0][name]' = '',
      'columns[0][orderable]' = 'TRUE',
      'columns[0][search][regex]' = 'FALSE',
      'columns[0][search][value]' = '',
      'columns[0][searchable]' = 'TRUE',
      'columns[1][data]' = 'custNo',
      'columns[1][name]' = '',
      'columns[1][orderable]' = 'FALSE',
      'columns[1][search][regex]' = 'FALSE',
      'columns[1][search][value]' = '',
      'columns[1][searchable]' = 'TRUE',
      'columns[10][data]' = 'returnCode',
      'columns[10][name]' = '',
      'columns[10][orderable]' = 'FALSE',
      'columns[10][search][regex]' = 'FALSE',
      'columns[10][search][value]' = '',
      'columns[10][searchable]' = 'TRUE',
      'columns[11][data]' = 'returnInfo',
      'columns[11][name]' = '',
      'columns[11][orderable]' = 'TRUE',
      'columns[11][search][regex]' = 'FALSE',
      'columns[11][search][value]' = '',
      'columns[11][searchable]' = 'TRUE',
      'columns[12][data]' = 'standardCode',
      'columns[12][name]' = '',
      'columns[12][orderable]' = 'TRUE',
      'columns[12][search][regex]' = 'FALSE',
      'columns[12][search][value]' = '',
      'columns[12][searchable]' = 'TRUE',
      'columns[13][data]' = 'createDatetime',
      'columns[13][name]' = '',
      'columns[13][orderable]' = 'TRUE',
      'columns[13][search][regex]' = 'FALSE',
      'columns[13][search][value]' = '',
      'columns[13][searchable]' = 'TRUE',
      'columns[14][data]' = 'updateDatetime',
      'columns[14][name]' = '',
      'columns[14][orderable]' = 'TRUE',
      'columns[14][search][regex]' = 'FALSE',
      'columns[14][search][value]' = '',
      'columns[14][searchable]' = 'TRUE',
      'columns[2][data]' = 'custName',
      'columns[2][name]' = '',
      'columns[2][orderable]' = 'FALSE',
      'columns[2][search][regex]' = 'FALSE',
      'columns[2][search][value]' = '',
      'columns[2][searchable]' = 'TRUE',
      'columns[3][data]' = 'bankCardNo',
      'columns[3][name]' = '',
      'columns[3][orderable]' = 'FALSE',
      'columns[3][search][regex]' = 'FALSE',
      'columns[3][search][value]' = '',
      'columns[3][searchable]' = 'TRUE',
      'columns[4][data]' = 'bankName',
      'columns[4][name]' = '',
      'columns[4][orderable]' = 'FALSE',
      'columns[4][search][regex]' = 'FALSE',
      'columns[4][search][value]' = '',
      'columns[4][searchable]' = 'TRUE',
      'columns[5][data]' = 'transAmt',
      'columns[5][name]' = '',
      'columns[5][orderable]' = 'FALSE',
      'columns[5][search][regex]' = 'FALSE',
      'columns[5][search][value]' = '',
      'columns[5][searchable]' = 'TRUE',
      'columns[6][data]' = 'transType',
      'columns[6][name]' = '',
      'columns[6][orderable]' = 'FALSE',
      'columns[6][search][regex]' = 'FALSE',
      'columns[6][search][value]' = '',
      'columns[6][searchable]' = 'TRUE',
      'columns[7][data]' = 'channelName',
      'columns[7][name]' = '',
      'columns[7][orderable]' = 'FALSE',
      'columns[7][search][regex]' = 'FALSE',
      'columns[7][search][value]' = '',
      'columns[7][searchable]' = 'TRUE',
      'columns[8][data]' = 'status',
      'columns[8][name]' = '',
      'columns[8][orderable]' = 'FALSE',
      'columns[8][search][regex]' = 'FALSE',
      'columns[8][search][value]' = '',
      'columns[8][searchable]' = 'TRUE',
      'columns[9][data]' = 'stepStatus',
      'columns[9][name]' = '',
      'columns[9][orderable]' = 'FALSE',
      'columns[9][search][regex]' = 'FALSE',
      'columns[9][search][value]' = '',
      'columns[9][searchable]' = 'TRUE',
      'draw' = draw,
      'endUpdateDatetime' = e_time,
      'length' = length,
      'order[0][column]' = '13',
      'order[0][dir]' = 'asc',
      'search[regex]' = 'FALSE',
      'search[value]' = '',
      'start' = start_num,
      'startUpdateDatetime' = s_time
    )
    res <- postForm(
      uri = 'http://172.18.32.14:8080/pcs-oms-new/payment/transOrderInfo/listPage',
      style = 'POST',
      curl = handle,
      .params = form
    )
    return(res)
  }

get_res <-
  getdata(start_time,
          end_time,
          start_num,
          length,
          handle,
          draw)

json_result <- fromJSON(get_res)
response_code <- json_result$code
total <- json_result$total
tmp <- json_result$rows
result <- rbind(result, tmp)
loop_time <- total %/% length + ifelse(total %% length > 0, 1, 0)
cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次')
if (loop_time > 1) {
  for (i in 1:(loop_time - 1)) {
    draw <- as.character(i)
    start_num <- i * length
    get_res <-
      getdata(start_time,
              end_time,
              start_num,
              length,
              handle,
              draw)
    json_result <- fromJSON(get_res)
    response_code <- json_result$code
    total <- json_result$total
    tmp <- json_result$rows
    result <- rbind(result, tmp)
    cat('共', total, '条数据，分', loop_time, '次完成，目前为第', i + 1, '次\n')
    Sys.sleep(1)
  }
}
cat('数据下载完成\n')
result_backup <- result###backup
write.table(
  result,
  file = paste(
    'D:/Rworkplace/hist_data_all/',
    gsub('-', '', substr(start_time, 1, 10)),
    '-',
    gsub('-', '', substr(end_time, 1, 10)),
    '-',
    ".csv",
    sep = ''
  ),
  sep = ','
)
cat('数据保存本地完成\n')

###数据下载完成！！！！！！

for (i in dir(path = "D:/Rworkplace/hist_data_all")) {
  cat(i,'\n')
  result <- rbind(result,read.csv(paste("D:/Rworkplace/hist_data_all/",i,sep = '')
                                  ,stringsAsFactors=FALSE))
  
}


split <- ##耗时分区
  c(
    -Inf,
    3,
    10,
    30,
    60,
    60 * 3,
    60 * 5,
    60 * 10,
    60 * 20,
    60 * 30,
    60 * 60,
    3600 * 2,
    3600 * 3,
    3600 * 6,
    3600 * 12,
    3600 * 24,
    +Inf
  )
label <-
  c(
    '3s以下',
    '大于3s小于10s',
    '大于10s小于30s',
    '大于30s小于1min',
    '大于1min小于3min',
    '大于3min小于5min',
    '大于5min小于10min',
    '大于10min小于20min',
    '大于20min小于30min',
    '大于30min小于1hour',
    '大于1hour小于2hour',
    '大于2hour小于3hour',
    '大于3hour小于6hour',
    '大于6hour小于12hour',
    '大于12hour小于1day',
    '大于1day'
  )

split_amt <- ##交易金额分区
  c(-Inf,
    100,
    600,
    1000,
    3000,
    5000,
    10000,
    15000,+Inf)
label_amt <-
  c(
    '[0,100]',
    '[100,600]',
    '[600,1000]',
    '[1000,3000]',
    '[3000,5000]',
    '[5000,10000]',
    '[10000,15000]',
    '[15000,+Inf]'
  )
normal_fail <-
  c('.*余额.*不足.*', '.*额度不足.*', '订单关闭成功', '订单未支付', '订单已关闭')



result[which(result$channelName == '微信JS支付'), 16] <- '微信JS'
result[which(result$channelName == '微信JS支付'), 14] <- 'weixinJS'
unique_bank_list <-
  result[!duplicated(result$bankCode), c(14, 16)]##银行名称合并之生成银行唯一列表

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
result<-subset(result,substr(result$crea_time,1,10)!='2018-08-31')###时间去头去尾
result<-subset(result,result$transType=='1')###筛选实时交易
for (i in normal_fail) {
  #####定义正常失败
  result[grepl(i, result$returnInfo) &
           result$status == 'FAIL', 33] <- '失败(余额不足)'
  
}

result[result$status == 'FAIL', 33] <- '失败(异常)'
result[result$status == 'SUCCESS', 33] <- '成功'
result[result$status == 'PROCESSING', 33] <- '处理中'
result$group <-
  cut(as.numeric(result$take_time), split, label)##划分耗时区间
result$group_amt <-
  cut(result$transAmt, split_amt, label_amt)##划分耗时区间
result[as.numeric(gsub('-','',substr(result$crea_time,1,10)))>=20180929,]$notifyUrl<-'NA'
result[is.na(result$notifyUrl),]$notifyUrl<-'NA'
result<-subset(result,result$notifyUrl=='NA')###把混入的批扣交易筛去

res<-aggregate(result$transSeqno,list(substr(result$crea_time,1,10),result$status),length)
res_1<-dcast(res,Group.1~Group.2,value.var ='x')
res_1$总数<-res_1$成功+res_1$`失败(异常)`+res_1$`失败(余额不足)`
res_1$成功率<-res_1$成功/res_1$总数
res_1$`失败(余额不足)率`<-res_1$`失败(余额不足)`/res_1$总数
res_1$`失败(异常)率`<-res_1$`失败(异常)`/res_1$总数
res_2<-melt(res_1,id.vars=c("Group.1","成功","失败(异常)","失败(余额不足)",'总数'))

ggplot(data = res_2,aes(
  x = substr(res_2$Group.1,6,10),
  y = res_2$value
  
)) + geom_line(
  
  aes(
    
    group = res_2$variable,
    
    color = res_2$variable
    
  ),
  size = 1
) + labs(
  x = '日期',
  y = '比率',
  color = '指标'
)+geom_text(data=res_2 ,aes(label = paste(100*round(res_2$value,4),'%')), vjust = 1.5, colour = "black", position = position_dodge(.9), size = 4)+ theme(text = element_text(family = 'STXihei', size = 16)) 
