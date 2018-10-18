setwd("D:/Rworkplace")
rm(list = ls())
library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)

#############
start_num <- 0
length <- 5000
uri_credit_report <- 'http://172.18.32.14:8080/ncc-oms/pbcapply/list'


handle <-
  getCurlHandle(
    httpheader = list(
      Accept = 'application/json, text/javascript, */*; q=0.01',
      'Accept-Ecoding' = 'gzip, deflate',
      'Accept-Language' = 'zh-CN,zh;q=0.9',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=E0DA7481724BF3E34F6008D6FB1781AB; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=98c3cc5b50c9f0359ce566792e22baf2; userId=s00580; userType=CBUSER',
      Referer = 'http://172.18.32.14:8080/ncc-oms/pbcapply/pbcApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )
#############


getdata <- function(start_num, length, uri, handle) {
  form_credit_report <- c(
    'draw' = '1',
    'columns[0][data]' = 'id',
    'columns[0][name]' = '',
    'columns[0][searchable]' = 'TRUE',
    'columns[0][orderable]' = 'FALSE',
    'columns[0][search][value]' = '',
    'columns[0][search][regex]' = 'FALSE',
    'columns[1][data]' = 'custNo',
    'columns[1][name]' = '',
    'columns[1][searchable]' = 'TRUE',
    'columns[1][orderable]' = 'FALSE',
    'columns[1][search][value]' = '',
    'columns[1][search][regex]' = 'FALSE',
    'columns[2][data]' = 'custName',
    'columns[2][name]' = '',
    'columns[2][searchable]' = 'TRUE',
    'columns[2][orderable]' = 'FALSE',
    'columns[2][search][value]' = '',
    'columns[2][search][regex]' = 'FALSE',
    'columns[3][data]' = 'capCode',
    'columns[3][name]' = '',
    'columns[3][searchable]' = 'TRUE',
    'columns[3][orderable]' = 'FALSE',
    'columns[3][search][value]' = '',
    'columns[3][search][regex]' = 'FALSE',
    'columns[4][data]' = 'pbcApplyType',
    'columns[4][name]' = '',
    'columns[4][searchable]' = 'TRUE',
    'columns[4][orderable]' = 'FALSE',
    'columns[4][search][value]' = '',
    'columns[4][search][regex]' = 'FALSE',
    'columns[5][data]' = 'pbcReportUrl',
    'columns[5][name]' = '',
    'columns[5][searchable]' = 'TRUE',
    'columns[5][orderable]' = 'FALSE',
    'columns[5][search][value]' = '',
    'columns[5][search][regex]' = 'FALSE',
    'columns[6][data]' = 'status',
    'columns[6][name]' = '',
    'columns[6][searchable]' = 'TRUE',
    'columns[6][orderable]' = 'FALSE',
    'columns[6][search][value]' = '',
    'columns[6][search][regex]' = 'FALSE',
    'columns[7][data]' = 'stepStatus',
    'columns[7][name]' = '',
    'columns[7][searchable]' = 'TRUE',
    'columns[7][orderable]' = 'FALSE',
    'columns[7][search][value]' = '',
    'columns[7][search][regex]' = 'FALSE',
    'columns[8][data]' = 'responseInfo',
    'columns[8][name]' = '',
    'columns[8][searchable]' = 'TRUE',
    'columns[8][orderable]' = 'FALSE',
    'columns[8][search][value]' = '',
    'columns[8][search][regex]' = 'FALSE',
    'columns[9][data]' = 'createDatetime',
    'columns[9][name]' = '',
    'columns[9][searchable]' = 'TRUE',
    'columns[9][orderable]' = 'TRUE',
    'columns[9][search][value]' = '',
    'columns[9][search][regex]' = 'FALSE',
    'columns[10][data]' = 'requestDatetime',
    'columns[10][name]' = '',
    'columns[10][searchable]' = 'TRUE',
    'columns[10][orderable]' = 'TRUE',
    'columns[10][search][value]' = '',
    'columns[10][search][regex]' = 'FALSE',
    'columns[11][data]' = 'resultResponseDatetime',
    'columns[11][name]' = '',
    'columns[11][searchable]' = 'TRUE',
    'columns[11][orderable]' = 'TRUE',
    'columns[11][search][value]' = '',
    'columns[11][search][regex]' = 'FALSE',
    'columns[12][data]' = 'updateDatetime',
    'columns[12][name]' = '',
    'columns[12][searchable]' = 'TRUE',
    'columns[12][orderable]' = 'TRUE',
    'columns[12][search][value]' = '',
    'columns[12][search][regex]' = 'FALSE',
    'columns[13][data]' = 'requestId',
    'columns[13][name]' = '',
    'columns[13][searchable]' = 'TRUE',
    'columns[13][orderable]' = 'FALSE',
    'columns[13][search][value]' = '',
    'columns[13][search][regex]' = 'FALSE',
    'columns[14][data]' = 'requestBusinessId',
    'columns[14][name]' = '',
    'columns[14][searchable]' = 'TRUE',
    'columns[14][orderable]' = 'TRUE',
    'columns[14][search][value]' = '',
    'columns[14][search][regex]' = 'FALSE',
    'columns[15][data]' = 'remark',
    'columns[15][name]' = '',
    'columns[15][searchable]' = 'TRUE',
    'columns[15][orderable]' = 'TRUE',
    'columns[15][search][value]' = '',
    'columns[15][search][regex]' = 'FALSE',
    'order[0][column]' = '0',
    'order[0][dir]' = 'asc',
    'start' = start_num,
    'length' = length,
    'search[value]' = '',
    'search[regex]' = 'FALSE',
    'id' = '',
    'remark' = '',
    'responseInfo' = '',
    'status' = '',
    'stepStatus' = '',
    'capCode' = '',
    'custNo' = '',
    'pbcApplyType' = '',
    'startCreateDatetime' = '',
    'endCreateDatetime' = '',
    'startRequestDatetime' = '2018-09-30 00:00:00',
    'endRequestDatetime' = '2018-09-30 23:59:59',
    'startResultResponseDatetime' = '',
    'endResultResponseDatetime' = ''
  )
  res <- postForm(
    uri = uri,
    style = 'POST',
    curl = handle,
    .params = form_credit_report
  )
  return(res)
}



credit_report <- data.frame()

get_res <- getdata(start_num, length, uri_credit_report, handle)
json_result <- fromJSON(get_res)
total <- json_result$total
tmp <- json_result$rows
credit_report <- rbind(credit_report, tmp)
loop_time <- total %/% 5000 + ifelse(total %% 5000 > 0, 1, 0)
cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次')
if (loop_time > 1) {
  for (i in 1:(loop_time - 1)) {
    start_num <- i * length
    get_res <-
      getdata(start_num, length, uri_credit_report,  handle)
    json_result <- fromJSON(get_res)
    response_code <- json_result$code
    total <- json_result$total
    tmp <- json_result$rows
    credit_report <- rbind(credit_report, tmp)
    cat('共', total, '条数据，分', loop_time, '次完成，目前为第', i + 1, '次\n')
    Sys.sleep(1)
  }
}
cat('数据下载完成\n')

#_________________________________________________________________________#
credit_report_bac<-credit_report
credit_report[!credit_report$status%in%c('80','90'),]$status<-'未结案'
status <-
  aggregate(
    credit_report$requestId,
    list(credit_report$capCode, credit_report$status),
    length
  )
names(status) <- c('渠道', '状态', '笔数')

s_1 <- subset(status, !status$状态 %in% c('80','90'))
s_2 <- subset(status, status$状态 == '90')
s_3 <- subset(status, status$状态 == '80')

status <- merge(s_1, s_2, by = c('渠道'),all = T)[, c(1, 3, 5)]
status<-merge(status,s_3,by = c('渠道'),all = T)[, c(1, 2,3, 5)]


names(status) <- c('渠道', '未结案', '成功','失败')
status$总数 <- status$未结案+status$成功


report_10 <- subset(credit_report, credit_report$status != '90')
status_10 <-
  aggregate(report_10$requestId,
            list(report_10$capCode, report_10$responseInfo),
            length)
names(status_10) <- c('渠道', 'responseInfo', '笔数')

credit_report$crea_time <-
  as.POSIXct(as.numeric(credit_report$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
credit_report$req_time <-
  as.POSIXct(as.numeric(credit_report$requestDatetime) / 1000, origin = "1970-01-01 00:00:00")
credit_report$resul_time <-
  as.POSIXct(as.numeric(credit_report$resultResponseDatetime) / 1000,
             origin = "1970-01-01 00:00:00")
credit_report$take_time <-
  as.numeric(credit_report$resultResponseDatetime) / 1000 - as.numeric(credit_report$requestDatetime) /
  1000
credit_report$group <-
  cut(
    credit_report$take_time,
    c(-Inf, 30, 60, 60 * 3, 60 * 5, 60 * 10, 60 * 30, 60 * 60, 60 * 60 * 3, Inf),
    labels = c(
      '小于30S',
      '大于30S小于60S',
      '大于1MIN小于3MIN',
      '大于3MIN小于5MIN',
      '大于5MIN小于10MIN',
      '大于10MIN小于30MIN',
      '大于30MIN小于60MIN',
      '大于1HOUR小于3HOUR',
      '大于3HOUR'
    )
  )

credit_report_ynxt<-subset(credit_report,credit_report$capCode=='YNXT'&credit_report$status=='90')
time<-aggregate(credit_report_ynxt$requestId,list(credit_report_ynxt$group),length)
names(time)<-c('区间','数量')






uri_cust_info <- 'http://172.18.32.14:8080/cif-oms-new/client/queryOmsCustMainInfo'
tmp_req <- function(cust_no,uri, handle) {
  form_cust_info <- c(
    'queryValue'=cust_no,
    'queryType'='custNo'
  )
  res <- postForm(
    uri = uri,
    style = 'POST',
    curl = handle,
    .params = form_cust_info
  )
  return(fromJSON(res)$channerlCode)
}
result$from<-NA
for (i in 1:length(result$custNo)) {
  result[i,]$from<-tmp_req(result[i,]$custNo,uri_cust_info,handle)
  cat(i,'\n')
}




