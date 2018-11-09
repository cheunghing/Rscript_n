setwd("D:/Rworkplace")
library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)

#############
mainpath<-"D:/Rworkplace"##存储路径

result <- data.frame()


start_time<-'2018-10-21 00:00:00'
end_time<-'2018-10-22 00:00:00'
loop_time <- NA
start_num <- 0
length <- 5000
draw <- '1'

oldpayurl <- 'http://172.18.32.14:8080/ncc-oms/repayapply/list'


handle <-
  getCurlHandle(
    httpheader = list(
      Accept = 'application/json, text/javascript, *; q=0.01',
      'Accept-Ecoding' = 'gzip, deflate',
      'Accept-Language' = 'zh-CN,zh;q=0.9',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=7D40B76937630CB770195BB7DE386E72; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=ad231f357ccb0f3f25eaf959b87cddbd; userId=s00580; userType=CBUSER',
      Referer = 'http://172.18.32.14:8080/ncc-oms/pbcapply/pbcApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )
#############


getdata <- function(s_time,
                    e_time,
                    start_num,
                    length,
                    uri,
                    handle
                    ) {
  oldpayform <- c('draw'='1',
                 'columns[0][data]'='repayId',
                 'columns[0][name]'='',
                 'columns[0][searchable]'='TRUE',
                 'columns[0][orderable]'='FALSE',
                 'columns[0][search][value]'='',
                 'columns[0][search][regex]'='FALSE',
                 'columns[1][data]'='custNo',
                 'columns[1][name]'='',
                 'columns[1][searchable]'='TRUE',
                 'columns[1][orderable]'='FALSE',
                 'columns[1][search][value]'='',
                 'columns[1][search][regex]'='FALSE',
                 'columns[2][data]'='custName',
                 'columns[2][name]'='',
                 'columns[2][searchable]'='TRUE',
                 'columns[2][orderable]'='FALSE',
                 'columns[2][search][value]'='',
                 'columns[2][search][regex]'='FALSE',
                 'columns[3][data]'='capitalCode',
                 'columns[3][name]'='',
                 'columns[3][searchable]'='TRUE',
                 'columns[3][orderable]'='FALSE',
                 'columns[3][search][value]'='',
                 'columns[3][search][regex]'='FALSE',
                 'columns[4][data]'='transAmt',
                 'columns[4][name]'='',
                 'columns[4][searchable]'='TRUE',
                 'columns[4][orderable]'='FALSE',
                 'columns[4][search][value]'='',
                 'columns[4][search][regex]'='FALSE',
                 'columns[5][data]'='bankCardNo',
                 'columns[5][name]'='',
                 'columns[5][searchable]'='TRUE',
                 'columns[5][orderable]'='FALSE',
                 'columns[5][search][value]'='',
                 'columns[5][search][regex]'='FALSE',
                 'columns[6][data]'='openBankName',
                 'columns[6][name]'='',
                 'columns[6][searchable]'='TRUE',
                 'columns[6][orderable]'='FALSE',
                 'columns[6][search][value]'='',
                 'columns[6][search][regex]'='FALSE',
                 'columns[7][data]'='notifyCtsFlag',
                 'columns[7][name]'='',
                 'columns[7][searchable]'='TRUE',
                 'columns[7][orderable]'='FALSE',
                 'columns[7][search][value]'='',
                 'columns[7][search][regex]'='FALSE',
                 'columns[8][data]'='status',
                 'columns[8][name]'='',
                 'columns[8][searchable]'='TRUE',
                 'columns[8][orderable]'='FALSE',
                 'columns[8][search][value]'='',
                 'columns[8][search][regex]'='FALSE',
                 'columns[9][data]'='stepStatus',
                 'columns[9][name]'='',
                 'columns[9][searchable]'='TRUE',
                 'columns[9][orderable]'='FALSE',
                 'columns[9][search][value]'='',
                 'columns[9][search][regex]'='FALSE',
                 'columns[10][data]'='createDatetime',
                 'columns[10][name]'='',
                 'columns[10][searchable]'='TRUE',
                 'columns[10][orderable]'='TRUE',
                 'columns[10][search][value]'='',
                 'columns[10][search][regex]'='FALSE',
                 'columns[11][data]'='requestDatetime',
                 'columns[11][name]'='',
                 'columns[11][searchable]'='TRUE',
                 'columns[11][orderable]'='TRUE',
                 'columns[11][search][value]'='',
                 'columns[11][search][regex]'='FALSE',
                 'columns[12][data]'='resultResponseDatetime',
                 'columns[12][name]'='',
                 'columns[12][searchable]'='TRUE',
                 'columns[12][orderable]'='TRUE',
                 'columns[12][search][value]'='',
                 'columns[12][search][regex]'='FALSE',
                 'columns[13][data]'='updateDatetime',
                 'columns[13][name]'='',
                 'columns[13][searchable]'='TRUE',
                 'columns[13][orderable]'='TRUE',
                 'columns[13][search][value]'='',
                 'columns[13][search][regex]'='FALSE',
                 'columns[14][data]'='remark',
                 'columns[14][name]'='',
                 'columns[14][searchable]'='TRUE',
                 'columns[14][orderable]'='FALSE',
                 'columns[14][search][value]'='',
                 'columns[14][search][regex]'='FALSE',
                 'columns[15][data]'='succTxTimesSum',
                 'columns[15][name]'='',
                 'columns[15][searchable]'='TRUE',
                 'columns[15][orderable]'='FALSE',
                 'columns[15][search][value]'='',
                 'columns[15][search][regex]'='FALSE',
                 'order[0][column]'='11',
                 'order[0][dir]'='desc',
                 'order[1][column]'='12',
                 'order[1][dir]'='desc',
                 'order[2][column]'='13',
                 'order[2][dir]'='desc',
                 'order[3][column]'='14',
                 'order[3][dir]'='desc',
                 'start'=start_num,
                 'length'=length,
                 'search[value]'='',
                 'search[regex]'='FALSE',
                 'repayId'='',
                 'custNo'='',
                 'capitalCode'='',
                 'capSubCode'='',
                 'status'='',
                 'stepStatus'='',
                 'startCreateDatetime'=s_time,
                 'endCreateDatetime'=e_time,
                 'startRequestDatetime'='',
                 'endRequestDatetime'='',
                 'startResultResponseDatetime'='',
                 'endResultResponseDatetime'=''
  )
  res <- postForm(
    uri = uri,
    style = 'POST',
    curl = handle,
    .params = oldpayform
  )
  return(res)
}
get_res<-getdata(start_time,end_time,start_num,length,oldpayurl,handle)
json_result <- fromJSON(get_res)
total <- json_result$total
tmp <- json_result$rows
result <- rbind(result, tmp)
loop_time <- total %/% length + ifelse(total %% length > 0, 1, 0)
cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次')
if (loop_time > 1) {
  for (i in 1:(loop_time - 1)) {
    start_num <- i * length
    get_res <-
      getdata(start_time,
              end_time,
              start_num,
              length,
              oldpayurl,
              handle)
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
result<-subset(result,is.na(result$batchId))
result$crea_time <-
  as.POSIXct(as.numeric(result$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$req_time <-
  as.POSIXct(as.numeric(result$requestDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$noti_time <-
  as.POSIXct(as.numeric(result$updateDatetime) / 1000, origin = "1970-01-01 00:00:00")
result<-subset(result,result$status%in%c('80','90','91'))
result[which(result$status=='91'),]$status<-'90'

res_ag<-aggregate(result$transAmt,list(result$openBankCode,result$capitalCode,result$status),sum)
res_ag<-dcast(res_ag,Group.1+Group.2~Group.3,value.var = 'x')
res_ag[is.na(res_ag$`80`), ]$`80` <- 0
res_ag[is.na(res_ag$`90`), ]$`90` <- 0
res_ag$total <-  res_ag$`80` + res_ag$`90`
res_ag$rate <- res_ag$`90` / res_ag$total
res_ag[res_ag$Group.2 == 'BSB', ]$Group.2 <- '包商'
res_ag[res_ag$Group.2 == 'MSXF', ]$Group.2 <- '马上消费'
res_ag[res_ag$Group.2 == 'YCWD', ]$Group.2 <- '粤财网贷'
res_ag[res_ag$Group.2 == 'YCXT', ]$Group.2 <- '粤财信托'
res_ag[res_ag$Group.2 == 'ZWXD', ]$Group.2 <- '众网小贷'
res_ag[res_ag$Group.2 == 'ZXXT', ]$Group.2 <- '中信信托'
res_ag[res_ag$Group.2 == 'ZABX', ]$Group.2 <- '众安保险'
res_ag[res_ag$Group.2 == 'XYXJ', ]$Group.2 <- '兴业消金'
res_ag[res_ag$Group.2 == 'BXBK', ]$Group.2 <- '百信银行'
res_ag[res_ag$Group.2 == 'FOTIC', ]$Group.2 <- '外贸信托'
res_ag[res_ag$Group.2 == 'SZYH', ]$Group.2 <- '苏州银行'
res_ag[res_ag$Group.2 == 'YNXT', ]$Group.2 <- '云南信托'
res_ag[res_ag$Group.2 == 'HYXF', ]$Group.2 <- '杭银消费'



res_ag_c<-aggregate(result$transAmt,list(result$openBankCode,result$capitalCode,result$status),length)
res_ag_c<-dcast(res_ag_c,Group.1+Group.2~Group.3,value.var = 'x')
res_ag_c[is.na(res_ag_c$`80`), ]$`80` <- 0
res_ag_c[is.na(res_ag_c$`90`), ]$`90` <- 0
res_ag_c$total <-  res_ag_c$`80` + res_ag_c$`90`
res_ag_c[res_ag_c$Group.2 == 'BSB', ]$Group.2 <- '包商'
res_ag_c[res_ag_c$Group.2 == 'MSXF', ]$Group.2 <- '马上消费'
res_ag_c[res_ag_c$Group.2 == 'YCWD', ]$Group.2 <- '粤财网贷'
res_ag_c[res_ag_c$Group.2 == 'YCXT', ]$Group.2 <- '粤财信托'
res_ag_c[res_ag_c$Group.2 == 'ZWXD', ]$Group.2 <- '众网小贷'
res_ag_c[res_ag_c$Group.2 == 'ZXXT', ]$Group.2 <- '中信信托'
res_ag_c[res_ag_c$Group.2 == 'ZABX', ]$Group.2 <- '众安保险'
res_ag_c[res_ag_c$Group.2 == 'XYXJ', ]$Group.2 <- '兴业消金'
res_ag_c[res_ag_c$Group.2 == 'BXBK', ]$Group.2 <- '百信银行'
res_ag_c[res_ag_c$Group.2 == 'FOTIC', ]$Group.2 <- '外贸信托'
res_ag_c[res_ag_c$Group.2 == 'SZYH', ]$Group.2 <- '苏州银行'
res_ag_c[res_ag_c$Group.2 == 'YNXT', ]$Group.2 <- '云南信托'
res_ag_c[res_ag_c$Group.2 == 'HYXF', ]$Group.2 <- '杭银消费'

res_ag_total<-merge(res_ag,res_ag_c,by=c('Group.1','Group.2'),all=T)
res_ag_total <- res_ag_total[, c(1, 2, 8,4,9,5,6)]
names(res_ag_total)<-c('银行','渠道','成功数','成功金额','总数','总金额','成功率')

##res_ag_total <- res_ag_total[, c(1, 2, 8,4,7,3,9,5,6)]
##names(res_ag_total)<-c('银行','渠道','成功数','成功金额','失败数','失败金额','总数','总金额','成功率')
res_ag_total_2<-rbind(res_ag_total_1,res_ag_total)
res_ag_total_2<-aggregate(res_ag_total_2[,3:6],list(res_ag_total_2$银行,res_ag_total_2$渠道),sum)




