library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)
mainpath<-"D:/Rworkplace"##存储路径
start_time <- '2018-11-16 00:00:00'
end_time <-
  '2018-11-17 00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
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
      'Accept-Language' = 'zh-CN,zh;q=0.9',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=1D0C2256A70BD1F90CBF40900D7361F8; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=66d46edcecb7a82aa5eb1cc44f170a67; userId=s00580; userType=CBUSER',
      Referer = 'http://172.18.32.14:8080/ncc-oms/repayapply/repayApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )



getdata <-
  function(s_time,
           e_time,
           start_num,
           length,
           handle,
           draw) {
    form <- c(
      'draw'=draw,
      'columns[0][data]'='id',
      'columns[0][name]'='',
      'columns[0][searchable]'='TRUE',
      'columns[0][orderable]'='FALSE',
      'columns[0][search][value]'='',
      'columns[0][search][regex]'='FALSE',
      'columns[1][data]'='capitalCode',
      'columns[1][name]'='',
      'columns[1][searchable]'='TRUE',
      'columns[1][orderable]'='FALSE',
      'columns[1][search][value]'='',
      'columns[1][search][regex]'='FALSE',
      'columns[2][data]'='capSubCode',
      'columns[2][name]'='',
      'columns[2][searchable]'='TRUE',
      'columns[2][orderable]'='FALSE',
      'columns[2][search][value]'='',
      'columns[2][search][regex]'='FALSE',
      'columns[3][data]'='custNo',
      'columns[3][name]'='',
      'columns[3][searchable]'='TRUE',
      'columns[3][orderable]'='FALSE',
      'columns[3][search][value]'='',
      'columns[3][search][regex]'='FALSE',
      'columns[4][data]'='loanId',
      'columns[4][name]'='',
      'columns[4][searchable]'='TRUE',
      'columns[4][orderable]'='FALSE',
      'columns[4][search][value]'='',
      'columns[4][search][regex]'='FALSE',
      'columns[5][data]'='capitalLoanId',
      'columns[5][name]'='',
      'columns[5][searchable]'='TRUE',
      'columns[5][orderable]'='FALSE',
      'columns[5][search][value]'='',
      'columns[5][search][regex]'='FALSE',
      'columns[6][data]'='period',
      'columns[6][name]'='',
      'columns[6][searchable]'='TRUE',
      'columns[6][orderable]'='FALSE',
      'columns[6][search][value]'='',
      'columns[6][search][regex]'='FALSE',
      'columns[7][data]'='scheduleTotal',
      'columns[7][name]'='',
      'columns[7][searchable]'='TRUE',
      'columns[7][orderable]'='FALSE',
      'columns[7][search][value]'='',
      'columns[7][search][regex]'='FALSE',
      'columns[8][data]'='repayCapital',
      'columns[8][name]'='',
      'columns[8][searchable]'='TRUE',
      'columns[8][orderable]'='FALSE',
      'columns[8][search][value]'='',
      'columns[8][search][regex]'='FALSE',
      'columns[9][data]'='repayInterest',
      'columns[9][name]'='',
      'columns[9][searchable]'='TRUE',
      'columns[9][orderable]'='FALSE',
      'columns[9][search][value]'='',
      'columns[9][search][regex]'='FALSE',
      'columns[10][data]'='repayAmt',
      'columns[10][name]'='',
      'columns[10][searchable]'='TRUE',
      'columns[10][orderable]'='FALSE',
      'columns[10][search][value]'='',
      'columns[10][search][regex]'='FALSE',
      'columns[11][data]'='payOffDate',
      'columns[11][name]'='',
      'columns[11][searchable]'='TRUE',
      'columns[11][orderable]'='FALSE',
      'columns[11][search][value]'='',
      'columns[11][search][regex]'='FALSE',
      'columns[12][data]'='scheduleDate',
      'columns[12][name]'='',
      'columns[12][searchable]'='TRUE',
      'columns[12][orderable]'='FALSE',
      'columns[12][search][value]'='',
      'columns[12][search][regex]'='FALSE',
      'columns[13][data]'='status',
      'columns[13][name]'='',
      'columns[13][searchable]'='TRUE',
      'columns[13][orderable]'='FALSE',
      'columns[13][search][value]'='',
      'columns[13][search][regex]'='FALSE',
      'columns[14][data]'='stepStatus',
      'columns[14][name]'='',
      'columns[14][searchable]'='TRUE',
      'columns[14][orderable]'='FALSE',
      'columns[14][search][value]'='',
      'columns[14][search][regex]'='FALSE',
      'columns[15][data]'='statusRemark',
      'columns[15][name]'='',
      'columns[15][searchable]'='TRUE',
      'columns[15][orderable]'='FALSE',
      'columns[15][search][value]'='',
      'columns[15][search][regex]'='FALSE',
      'columns[16][data]'='responseInfo',
      'columns[16][name]'='',
      'columns[16][searchable]'='TRUE',
      'columns[16][orderable]'='FALSE',
      'columns[16][search][value]'='',
      'columns[16][search][regex]'='FALSE',
      'columns[17][data]'='repayType',
      'columns[17][name]'='',
      'columns[17][searchable]'='TRUE',
      'columns[17][orderable]'='FALSE',
      'columns[17][search][value]'='',
      'columns[17][search][regex]'='FALSE',
      'columns[18][data]'='createDatetime',
      'columns[18][name]'='',
      'columns[18][searchable]'='TRUE',
      'columns[18][orderable]'='TRUE',
      'columns[18][search][value]'='',
      'columns[18][search][regex]'='FALSE',
      'columns[19][data]'='requestDatetime',
      'columns[19][name]'='',
      'columns[19][searchable]'='TRUE',
      'columns[19][orderable]'='TRUE',
      'columns[19][search][value]'='',
      'columns[19][search][regex]'='FALSE',
      'columns[20][data]'='resultResponseDatetime',
      'columns[20][name]'='',
      'columns[20][searchable]'='TRUE',
      'columns[20][orderable]'='TRUE',
      'columns[20][search][value]'='',
      'columns[20][search][regex]'='FALSE',
      'columns[21][data]'='updateDatetime',
      'columns[21][name]'='',
      'columns[21][searchable]'='TRUE',
      'columns[21][orderable]'='TRUE',
      'columns[21][search][value]'='',
      'columns[21][search][regex]'='FALSE',
      'columns[22][data]'='remark',
      'columns[22][name]'='',
      'columns[22][searchable]'='TRUE',
      'columns[22][orderable]'='FALSE',
      'columns[22][search][value]'='',
      'columns[22][search][regex]'='FALSE',
      'order[0][column]'='18',
      'order[0][dir]'='desc',
      'order[1][column]'='19',
      'order[1][dir]'='desc',
      'order[2][column]'='20',
      'order[2][dir]'='desc',
      'order[3][column]'='21',
      'order[3][dir]'='desc',
      'start'=start_num,
      'length'=length,
      'search[value]'='',
      'search[regex]'='FALSE',
      'id'='',
      'custNo'='',
      'loanId'='',
      'status'='',
      'stepStatus'='',
      'repayType'='',
      'capitalCode'='',
      'capSubCode'='',
      'responseInfo'='',
      'remark'='',
      'startCreateDatetime'=s_time,
      'endCreateDatetime'=e_time,
      'startRequestDatetime'='',
      'endRequestDatetime'='',
      'startResultResponseDatetime'='',
      'endResultResponseDatetime'='',
      'batchId'='',
      'transferDatetime'='',
      'batchOper'='FALSE'
    )
    res <- postForm(
      uri = 'http://172.18.32.14:8080/ncc-oms//repay/queryRepayNoticeInfoByPage',
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

result_merge<-aggregate(result$repayAmt,list(result$capitalCode,result$repayType),sum)
result_merge<-dcast(result_merge,Group.1~ Group.2,value.var = "x")
result_merge[is.na(result_merge$`00`), ]$`00` <- 0
result_merge[is.na(result_merge$`01`), ]$`01` <- 0

result_merge$资金方金额占比<-result_merge$`00`/(result_merge$`00`+result_merge$`01`)
result_merge$萨摩耶金额占比<-result_merge$`01`/(result_merge$`00`+result_merge$`01`)
names(result_merge)[1:3]<-c('资金方','资金方金额','萨摩耶金额')



result_cou<-result
result_cou$flag<- paste(result_cou$custNo,result_cou$capitalCode,result_cou$payOffDate,result_cou$payOffTime,sep='')
result_cou<-result_cou[!duplicated(result_cou$flag),]
result_merge_cou<-aggregate(result_cou$repayAmt,list(result_cou$capitalCode,result_cou$repayType),length)
result_merge_cou<-dcast(result_merge_cou,Group.1~ Group.2,value.var = "x")
result_merge_cou[is.na(result_merge_cou$`00`), ]$`00` <- 0
result_merge_cou[is.na(result_merge_cou$`01`), ]$`01` <- 0
result_merge_cou$资金方笔数占比<-result_merge_cou$`00`/(result_merge_cou$`00`+result_merge_cou$`01`)
result_merge_cou$萨摩耶笔数占比<-result_merge_cou$`01`/(result_merge_cou$`00`+result_merge_cou$`01`)
names(result_merge_cou)[1:3]<-c('资金方','资金方笔数','萨摩耶笔数')

result_merge<-merge(result_merge,result_merge_cou,by=c('资金方'))
result_merge<-result_merge[,c(1,2,4,3,5,6,8,7,9)]
result_merge[result_merge$资金方 == 'BSB', ]$资金方 <- '包商'
result_merge[result_merge$资金方 == 'MSXF', ]$资金方 <- '马上消费'
result_merge[result_merge$资金方 == 'YCWD', ]$资金方 <- '粤财网贷'
result_merge[result_merge$资金方 == 'YCXT', ]$资金方 <- '粤财信托'
result_merge[result_merge$资金方 == 'ZWXD', ]$资金方 <- '众网小贷'
result_merge[result_merge$资金方 == 'BXBK', ]$资金方 <- '百信银行'
result_merge[result_merge$资金方 == 'FOTIC', ]$资金方 <- '外贸信托'
result_merge[result_merge$资金方 == 'HBCF', ]$资金方 <- '哈银消费'
result_merge[result_merge$资金方 == 'HYXF', ]$资金方 <- '杭银消费'
result_merge[result_merge$资金方 == 'SZYH', ]$资金方 <- '苏州银行'
result_merge[result_merge$资金方 == 'ZXXT', ]$资金方 <- '中信信托'
result_merge[result_merge$资金方 == 'ZYXF', ]$资金方 <- '中原消费'
result_merge[result_merge$资金方 == 'XYXJ', ]$资金方 <- '兴业消金'
result_merge[result_merge$资金方 == 'YNXT', ]$资金方 <- '云南信托'
result_merge[result_merge$资金方 == 'ZABX', ]$资金方 <- '众安保险'

result_merge<-arrange(result_merge,result_merge$萨摩耶笔数占比)
result_merge <-
  rbind(result_merge,
        list(
          '总计',
          sum(result_merge$资金方金额),
          sum(result_merge$资金方金额)/(sum(result_merge$萨摩耶金额)+sum(result_merge$资金方金额)),
          sum(result_merge$萨摩耶金额),
          sum(result_merge$萨摩耶金额)/(sum(result_merge$萨摩耶金额)+sum(result_merge$资金方金额)),
          
          sum(result_merge$资金方笔数),
          sum(result_merge$资金方笔数)/(sum(result_merge$资金方笔数)+sum(result_merge$萨摩耶笔数)),
          
          sum(result_merge$萨摩耶笔数),
          sum(result_merge$萨摩耶笔数)/(sum(result_merge$资金方笔数)+sum(result_merge$萨摩耶笔数))
          
          )
        )
write.table(
  result_merge,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "资金方VS萨摩耶占比.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)

