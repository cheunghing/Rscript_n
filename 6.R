library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)
mainpath<-"D:/Rworkplace"##存储路径
start_time <- '2018-12-04 00:00:00'
end_time <-
  '2018-12-05 00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
loop_time <- NA
start_num <- 0
length <- 5000
draw <- '1'
result <- data.frame()
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

handle <-
  getCurlHandle(
    httpheader = list(
      Accept = 'application/json, text/javascript, */*; q=0.01',
      'Accept-Ecoding' = 'gzip, deflate',
      'Accept-Language' = 'zh-CN,zh;q=0.9',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=D6AD011322E8CD114E605D036B92D447; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=d84561838c24ec961a75570bcbd87f88; userId=s00580; userType=CBUSER',
      Referer = 'http://172.18.32.14:8080/ncc-oms/repayapply/repayApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )
get_reroute_data <-
  function(s_time,
           e_time,
           start_num,
           length,
           handle,
           draw) {
    form <- c(
      'draw' = draw,
      'columns[0][data]' = 'cApplyId',
      'columns[0][name]' = '',
      'columns[0][searchable]' = 'true',
      'columns[0][orderable]' = 'false',
      'columns[0][search][value]' = '',
      'columns[0][search][regex]' = 'false',
      'columns[1][data]' = 'repayReqId',
      'columns[1][name]' = '',
      'columns[1][searchable]' = 'true',
      'columns[1][orderable]' = 'true',
      'columns[1][search][value]' = '',
      'columns[1][search][regex]' = 'false',
      'columns[2][data]' = 'custNo',
      'columns[2][name]' = '',
      'columns[2][searchable]' = 'true',
      'columns[2][orderable]' = 'false',
      'columns[2][search][value]' = '',
      'columns[2][search][regex]' = 'false',
      'columns[3][data]' = 'custName',
      'columns[3][name]' = '',
      'columns[3][searchable]' = 'true',
      'columns[3][orderable]' = 'false',
      'columns[3][search][value]' = '',
      'columns[3][search][regex]' = 'false',
      'columns[4][data]' = 'idType',
      'columns[4][name]' = '',
      'columns[4][searchable]' = 'true',
      'columns[4][orderable]' = 'true',
      'columns[4][search][value]' = '',
      'columns[4][search][regex]' = 'false',
      'columns[5][data]' = 'idNo',
      'columns[5][name]' = '',
      'columns[5][searchable]' = 'true',
      'columns[5][orderable]' = 'false',
      'columns[5][search][value]' = '',
      'columns[5][search][regex]' = 'false',
      'columns[6][data]' = 'repayType',
      'columns[6][name]' = '',
      'columns[6][searchable]' = 'true',
      'columns[6][orderable]' = 'true',
      'columns[6][search][value]' = '',
      'columns[6][search][regex]' = 'false',
      'columns[7][data]' = 'repayMethod',
      'columns[7][name]' = '',
      'columns[7][searchable]' = 'true',
      'columns[7][orderable]' = 'true',
      'columns[7][search][value]' = '',
      'columns[7][search][regex]' = 'false',
      'columns[8][data]' = 'channelId',
      'columns[8][name]' = '',
      'columns[8][searchable]' = 'true',
      'columns[8][orderable]' = 'true',
      'columns[8][search][value]' = '',
      'columns[8][search][regex]' = 'false',
      'columns[9][data]' = 'bankCardNo',
      'columns[9][name]' = '',
      'columns[9][searchable]' = 'true',
      'columns[9][orderable]' = 'false',
      'columns[9][search][value]' = '',
      'columns[9][search][regex]' = 'false',
      'columns[10][data]' = 'mobile',
      'columns[10][name]' = '',
      'columns[10][searchable]' = 'true',
      'columns[10][orderable]' = 'false',
      'columns[10][search][value]' = '',
      'columns[10][search][regex]' = 'false',
      'columns[11][data]' = 'bankCode',
      'columns[11][name]' = '',
      'columns[11][searchable]' = 'true',
      'columns[11][orderable]' = 'false',
      'columns[11][search][value]' = '',
      'columns[11][search][regex]' = 'false',
      'columns[12][data]' = 'bankName',
      'columns[12][name]' = '',
      'columns[12][searchable]' = 'true',
      'columns[12][orderable]' = 'false',
      'columns[12][search][value]' = '',
      'columns[12][search][regex]' = 'false',
      'columns[13][data]' = 'totalAmt',
      'columns[13][name]' = '',
      'columns[13][searchable]' = 'true',
      'columns[13][orderable]' = 'false',
      'columns[13][search][value]' = '',
      'columns[13][search][regex]' = 'false',
      'columns[14][data]' = 'totalPeriod',
      'columns[14][name]' = '',
      'columns[14][searchable]' = 'true',
      'columns[14][orderable]' = 'false',
      'columns[14][search][value]' = '',
      'columns[14][search][regex]' = 'false',
      'columns[15][data]' = 'status',
      'columns[15][name]' = '',
      'columns[15][searchable]' = 'true',
      'columns[15][orderable]' = 'true',
      'columns[15][search][value]' = '',
      'columns[15][search][regex]' = 'false',
      'columns[16][data]' = 'statusRemark',
      'columns[16][name]' = '',
      'columns[16][searchable]' = 'true',
      'columns[16][orderable]' = 'false',
      'columns[16][search][value]' = '',
      'columns[16][search][regex]' = 'false',
      'columns[17][data]' = 'stepStatus',
      'columns[17][name]' = '',
      'columns[17][searchable]' = 'true',
      'columns[17][orderable]' = 'true',
      'columns[17][search][value]' = '',
      'columns[17][search][regex]' = 'false',
      'columns[18][data]' = 'repayStatus',
      'columns[18][name]' = '',
      'columns[18][searchable]' = 'true',
      'columns[18][orderable]' = 'true',
      'columns[18][search][value]' = '',
      'columns[18][search][regex]' = 'false',
      'columns[19][data]' = 'channelReqId',
      'columns[19][name]' = '',
      'columns[19][searchable]' = 'true',
      'columns[19][orderable]' = 'false',
      'columns[19][search][value]' = '',
      'columns[19][search][regex]' = 'false',
      'columns[20][data]' = 'channelRepayId',
      'columns[20][name]' = '',
      'columns[20][searchable]' = 'true',
      'columns[20][orderable]' = 'false',
      'columns[20][search][value]' = '',
      'columns[20][search][regex]' = 'false',
      'columns[21][data]' = 'channelRspId',
      'columns[21][name]' = '',
      'columns[21][searchable]' = 'true',
      'columns[21][orderable]' = 'false',
      'columns[21][search][value]' = '',
      'columns[21][search][regex]' = 'false',
      'columns[22][data]' = 'repayDate',
      'columns[22][name]' = '',
      'columns[22][searchable]' = 'true',
      'columns[22][orderable]' = 'true',
      'columns[22][search][value]' = '',
      'columns[22][search][regex]' = 'false',
      'columns[23][data]' = 'transDate',
      'columns[23][name]' = '',
      'columns[23][searchable]' = 'true',
      'columns[23][orderable]' = 'true',
      'columns[23][search][value]' = '',
      'columns[23][search][regex]' = 'false',
      'columns[24][data]' = 'excProcessCnt',
      'columns[24][name]' = '',
      'columns[24][searchable]' = 'true',
      'columns[24][orderable]' = 'true',
      'columns[24][search][value]' = '',
      'columns[24][search][regex]' = 'false',
      'columns[25][data]' = 'responseCode',
      'columns[25][name]' = '',
      'columns[25][searchable]' = 'true',
      'columns[25][orderable]' = 'false',
      'columns[25][search][value]' = '',
      'columns[25][search][regex]' = 'false',
      'columns[26][data]' = 'responseInfo',
      'columns[26][name]' = '',
      'columns[26][searchable]' = 'true',
      'columns[26][orderable]' = 'false',
      'columns[26][search][value]' = '',
      'columns[26][search][regex]' = 'false',
      'columns[27][data]' = 'transSeqno',
      'columns[27][name]' = '',
      'columns[27][searchable]' = 'true',
      'columns[27][orderable]' = 'true',
      'columns[27][search][value]' = '',
      'columns[27][search][regex]' = 'false',
      'columns[28][data]' = 'requestDatetime',
      'columns[28][name]' = '',
      'columns[28][searchable]' = 'true',
      'columns[28][orderable]' = 'true',
      'columns[28][search][value]' = '',
      'columns[28][search][regex]' = 'false',
      'columns[29][data]' = 'responseDatetime',
      'columns[29][name]' = '',
      'columns[29][searchable]' = 'true',
      'columns[29][orderable]' = 'true',
      'columns[29][search][value]' = '',
      'columns[29][search][regex]' = 'false',
      'columns[30][data]' = 'version',
      'columns[30][name]' = '',
      'columns[30][searchable]' = 'true',
      'columns[30][orderable]' = 'false',
      'columns[30][search][value]' = '',
      'columns[30][search][regex]' = 'false',
      'columns[31][data]' = 'remark',
      'columns[31][name]' = '',
      'columns[31][searchable]' = 'true',
      'columns[31][orderable]' = 'false',
      'columns[31][search][value]' = '',
      'columns[31][search][regex]' = 'false',
      'columns[32][data]' = 'createDatetime',
      'columns[32][name]' = '',
      'columns[32][searchable]' = 'true',
      'columns[32][orderable]' = 'true',
      'columns[32][search][value]' = '',
      'columns[32][search][regex]' = 'false',
      'columns[33][data]' = 'updateDatetime',
      'columns[33][name]' = '',
      'columns[33][searchable]' = 'true',
      'columns[33][orderable]' = 'true',
      'columns[33][search][value]' = '',
      'columns[33][search][regex]' = 'false',
      'order[0][column]' = '33',
      'order[0][dir]' = 'desc',
      'start' = start_num,
      'length' = length,
      'search[value]' = '',
      'search[regex]' = 'false',
      'startCreateDatetime' = s_time,
      'endCreateDatetime' = e_time
    )
    res <- postForm(
      uri = 'http://172.18.32.14:8080/ncc-oms/repay_reform/repayChannelInfoRouteLog/listPage',
      style = 'POST',
      curl = handle,
      .params = form
    )
    return(res)
  }

get_res <-
  get_reroute_data(start_time,
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
      get_reroute_data(start_time,
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
result_reroute_list<-result
result_reroute_list$version<-'reroute_ori'
result<-data.frame()

getdata <-
  function(s_time,
           e_time,
           start_num,
           length,
           handle,
           draw) {
    form <- c(
      'draw' = draw,
      'columns[0][data]' = 'cApplyId',
      'columns[0][name]' = '',
      'columns[0][searchable]' = 'true',
      'columns[0][orderable]' = 'false',
      'columns[0][search][value]' = '',
      'columns[0][search][regex]' = 'false',
      'columns[1][data]' = 'repayReqId',
      'columns[1][name]' = '',
      'columns[1][searchable]' = 'true',
      'columns[1][orderable]' = 'true',
      'columns[1][search][value]' = '',
      'columns[1][search][regex]' = 'false',
      'columns[2][data]' = 'custNo',
      'columns[2][name]' = '',
      'columns[2][searchable]' = 'true',
      'columns[2][orderable]' = 'false',
      'columns[2][search][value]' = '',
      'columns[2][search][regex]' = 'false',
      'columns[3][data]' = 'custName',
      'columns[3][name]' = '',
      'columns[3][searchable]' = 'true',
      'columns[3][orderable]' = 'false',
      'columns[3][search][value]' = '',
      'columns[3][search][regex]' = 'false',
      'columns[4][data]' = 'idType',
      'columns[4][name]' = '',
      'columns[4][searchable]' = 'true',
      'columns[4][orderable]' = 'true',
      'columns[4][search][value]' = '',
      'columns[4][search][regex]' = 'false',
      'columns[5][data]' = 'idNo',
      'columns[5][name]' = '',
      'columns[5][searchable]' = 'true',
      'columns[5][orderable]' = 'false',
      'columns[5][search][value]' = '',
      'columns[5][search][regex]' = 'false',
      'columns[6][data]' = 'repayType',
      'columns[6][name]' = '',
      'columns[6][searchable]' = 'true',
      'columns[6][orderable]' = 'true',
      'columns[6][search][value]' = '',
      'columns[6][search][regex]' = 'false',
      'columns[7][data]' = 'repayMethod',
      'columns[7][name]' = '',
      'columns[7][searchable]' = 'true',
      'columns[7][orderable]' = 'true',
      'columns[7][search][value]' = '',
      'columns[7][search][regex]' = 'false',
      'columns[8][data]' = 'channelId',
      'columns[8][name]' = '',
      'columns[8][searchable]' = 'true',
      'columns[8][orderable]' = 'true',
      'columns[8][search][value]' = '',
      'columns[8][search][regex]' = 'false',
      'columns[9][data]' = 'bankCardNo',
      'columns[9][name]' = '',
      'columns[9][searchable]' = 'true',
      'columns[9][orderable]' = 'false',
      'columns[9][search][value]' = '',
      'columns[9][search][regex]' = 'false',
      'columns[10][data]' = 'mobile',
      'columns[10][name]' = '',
      'columns[10][searchable]' = 'true',
      'columns[10][orderable]' = 'false',
      'columns[10][search][value]' = '',
      'columns[10][search][regex]' = 'false',
      'columns[11][data]' = 'bankCode',
      'columns[11][name]' = '',
      'columns[11][searchable]' = 'true',
      'columns[11][orderable]' = 'false',
      'columns[11][search][value]' = '',
      'columns[11][search][regex]' = 'false',
      'columns[12][data]' = 'bankName',
      'columns[12][name]' = '',
      'columns[12][searchable]' = 'true',
      'columns[12][orderable]' = 'false',
      'columns[12][search][value]' = '',
      'columns[12][search][regex]' = 'false',
      'columns[13][data]' = 'totalAmt',
      'columns[13][name]' = '',
      'columns[13][searchable]' = 'true',
      'columns[13][orderable]' = 'false',
      'columns[13][search][value]' = '',
      'columns[13][search][regex]' = 'false',
      'columns[14][data]' = 'totalPeriod',
      'columns[14][name]' = '',
      'columns[14][searchable]' = 'true',
      'columns[14][orderable]' = 'false',
      'columns[14][search][value]' = '',
      'columns[14][search][regex]' = 'false',
      'columns[15][data]' = 'status',
      'columns[15][name]' = '',
      'columns[15][searchable]' = 'true',
      'columns[15][orderable]' = 'true',
      'columns[15][search][value]' = '',
      'columns[15][search][regex]' = 'false',
      'columns[16][data]' = 'statusRemark',
      'columns[16][name]' = '',
      'columns[16][searchable]' = 'true',
      'columns[16][orderable]' = 'false',
      'columns[16][search][value]' = '',
      'columns[16][search][regex]' = 'false',
      'columns[17][data]' = 'stepStatus',
      'columns[17][name]' = '',
      'columns[17][searchable]' = 'true',
      'columns[17][orderable]' = 'true',
      'columns[17][search][value]' = '',
      'columns[17][search][regex]' = 'false',
      'columns[18][data]' = 'repayStatus',
      'columns[18][name]' = '',
      'columns[18][searchable]' = 'true',
      'columns[18][orderable]' = 'true',
      'columns[18][search][value]' = '',
      'columns[18][search][regex]' = 'false',
      'columns[19][data]' = 'channelReqId',
      'columns[19][name]' = '',
      'columns[19][searchable]' = 'true',
      'columns[19][orderable]' = 'false',
      'columns[19][search][value]' = '',
      'columns[19][search][regex]' = 'false',
      'columns[20][data]' = 'channelRepayId',
      'columns[20][name]' = '',
      'columns[20][searchable]' = 'true',
      'columns[20][orderable]' = 'false',
      'columns[20][search][value]' = '',
      'columns[20][search][regex]' = 'false',
      'columns[21][data]' = 'channelRspId',
      'columns[21][name]' = '',
      'columns[21][searchable]' = 'true',
      'columns[21][orderable]' = 'false',
      'columns[21][search][value]' = '',
      'columns[21][search][regex]' = 'false',
      'columns[22][data]' = 'repayDate',
      'columns[22][name]' = '',
      'columns[22][searchable]' = 'true',
      'columns[22][orderable]' = 'true',
      'columns[22][search][value]' = '',
      'columns[22][search][regex]' = 'false',
      'columns[23][data]' = 'transDate',
      'columns[23][name]' = '',
      'columns[23][searchable]' = 'true',
      'columns[23][orderable]' = 'true',
      'columns[23][search][value]' = '',
      'columns[23][search][regex]' = 'false',
      'columns[24][data]' = 'excProcessCnt',
      'columns[24][name]' = '',
      'columns[24][searchable]' = 'true',
      'columns[24][orderable]' = 'true',
      'columns[24][search][value]' = '',
      'columns[24][search][regex]' = 'false',
      'columns[25][data]' = 'responseCode',
      'columns[25][name]' = '',
      'columns[25][searchable]' = 'true',
      'columns[25][orderable]' = 'false',
      'columns[25][search][value]' = '',
      'columns[25][search][regex]' = 'false',
      'columns[26][data]' = 'responseInfo',
      'columns[26][name]' = '',
      'columns[26][searchable]' = 'true',
      'columns[26][orderable]' = 'false',
      'columns[26][search][value]' = '',
      'columns[26][search][regex]' = 'false',
      'columns[27][data]' = 'transSeqno',
      'columns[27][name]' = '',
      'columns[27][searchable]' = 'true',
      'columns[27][orderable]' = 'true',
      'columns[27][search][value]' = '',
      'columns[27][search][regex]' = 'false',
      'columns[28][data]' = 'requestDatetime',
      'columns[28][name]' = '',
      'columns[28][searchable]' = 'true',
      'columns[28][orderable]' = 'true',
      'columns[28][search][value]' = '',
      'columns[28][search][regex]' = 'false',
      'columns[29][data]' = 'responseDatetime',
      'columns[29][name]' = '',
      'columns[29][searchable]' = 'true',
      'columns[29][orderable]' = 'true',
      'columns[29][search][value]' = '',
      'columns[29][search][regex]' = 'false',
      'columns[30][data]' = 'version',
      'columns[30][name]' = '',
      'columns[30][searchable]' = 'true',
      'columns[30][orderable]' = 'false',
      'columns[30][search][value]' = '',
      'columns[30][search][regex]' = 'false',
      'columns[31][data]' = 'remark',
      'columns[31][name]' = '',
      'columns[31][searchable]' = 'true',
      'columns[31][orderable]' = 'false',
      'columns[31][search][value]' = '',
      'columns[31][search][regex]' = 'false',
      'columns[32][data]' = 'createDatetime',
      'columns[32][name]' = '',
      'columns[32][searchable]' = 'true',
      'columns[32][orderable]' = 'true',
      'columns[32][search][value]' = '',
      'columns[32][search][regex]' = 'false',
      'columns[33][data]' = 'updateDatetime',
      'columns[33][name]' = '',
      'columns[33][searchable]' = 'true',
      'columns[33][orderable]' = 'true',
      'columns[33][search][value]' = '',
      'columns[33][search][regex]' = 'false',
      'order[0][column]' = '33',
      'order[0][dir]' = 'desc',
      'start' = start_num,
      'length' = length,
      'repayType'='RT01',
      'search[value]' = '',
      'search[regex]' = 'false',
      'startCreateDatetime' = s_time,
      'endCreateDatetime' = e_time
    )
    res <- postForm(
      uri = 'http://172.18.32.14:8080/ncc-oms/repay_reform/repayChannelInfo/listPage',
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
# unique_bank_list <-result[!duplicated(result$bankCode), c(12, 13)]##银行名称合并之生成银行唯一列表
# result <-  ##银行名称合并之关联银行唯一列表
#   merge(
#     result,
#     unique_bank_list,
#     by.x = c('bankCode'),
#     by.y = c('bankCode'),
#     all.x = T
#   )
result[which(result$cApplyId%in%result_reroute_list$cApplyId),]$version<-'reroute'

result<-rbind(result,result_reroute_list[,names(result_reroute_list)[names(result_reroute_list)%in%names(result)]])
result$bankName_uni<-result$bankName
result[grep('.*建设.*',result$bankName),]$bankName_uni<-'建设银行'
result[grep('.*工商*',result$bankName),]$bankName_uni<-'工商银行'
result[grep('.*浦东*',result$bankName),]$bankName_uni<-'浦发银行'
result[grep('.*农业*',result$bankName),]$bankName_uni<-'农业银行'
result[grep('.*交通*',result$bankName),]$bankName_uni<-'交通银行'
result[grep('.*民生*',result$bankName),]$bankName_uni<-'民生银行'
result[grep('.*邮政*',result$bankName),]$bankName_uni<-'邮储银行'
result[grep('.*广东发展.*',result$bankName),]$bankName_uni<-'广发银行'
result[grep('.*光大*',result$bankName),]$bankName_uni<-'光大银行'
#++++++++++++++++++++++++++#
result[result$channelId == 'BSB', ]$channelId <- '包商'
result[result$channelId == 'MSXF', ]$channelId <- '马上消费'
result[result$channelId == 'YCWD', ]$channelId <- '粤财网贷'
result[result$channelId == 'YCXT', ]$channelId <- '粤财信托'
result[result$channelId == 'ZWXD', ]$channelId <- '众网小贷'
result[result$channelId == 'ZXXT', ]$channelId <- '中信信托'
result[result$channelId == 'ZABX', ]$channelId <- '众安保险'
result[result$channelId == 'XYXJ', ]$channelId <- '兴业消金'
result[result$channelId == 'BXBK', ]$channelId <- '百信银行'
result[result$channelId == 'FOTIC', ]$channelId <- '外贸信托'
result[result$channelId == 'SZYH', ]$channelId <- '苏州银行'
result[result$channelId == 'YNXT', ]$channelId <- '云南信托'
result[result$channelId == 'HYXF', ]$channelId <- '杭银消费'



result$crea_time <-
  as.POSIXct(as.numeric(result$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$req_time <-
  as.POSIXct(as.numeric(result$requestDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$respon_time <-
  as.POSIXct(as.numeric(result$responseDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$respon_time[which(!result$status %in% c('80', '90'))] <-
  Sys.time()
result$take_time <- result$respon_time - result$req_time

# result$noti_time <-
#   as.POSIXct(paste(result$transDate,result$transTime,sep=''), origin = "1970-01-01 00:00:00")
# result$noti_time <-
#   paste(result$transDate,result$transTime,sep='')
result<-subset(result,!result$status%in%c('00','10','20'))
result$group <-
  cut(as.numeric(result$take_time), split, label)##划分耗时区间
result_reroute<-subset(result,result$cApplyId%in%result_reroute_list$cApplyId&result$version!='reroute_ori')##只选择重路由后的部分！！！
result <-
  subset(result, result$repayType %in% c('RT01'))#####再一次校验选实时:RT01 or批扣::RT02
result_for_16<-subset(result,result$version!='reroute_ori')##选择重路由后的+未重路由的！！！
result<-subset(result,!result$cApplyId%in%result_reroute_list$cApplyId|result$version=='reroute_ori')##选择重路由前的+未重路由的！！！
result_for_16<-merge(result_for_16,result[which(result$version=='reroute_ori'),c('cApplyId','channelId')],by='cApplyId',all.x=T)
names(result_for_16)[which(names(result_for_16)=='channelId.y')]<-'capcode'
names(result_for_16)[which(names(result_for_16)=='channelId.x')]<-'channelId'
result_for_16[which(result_for_16$version!='reroute'),]$capcode<-result_for_16[which(result_for_16$version!='reroute'),]$channelId
result <-
  subset(result, !result$channelId %in% c('PCS'))#####筛选不为PCS代扣的
res_ag <-
  aggregate(result$idNo,
            list(result$bankName_uni, result$channelId, result$status),
            length)
res_ag <- dcast(res_ag, Group.1 + Group.2 ~ Group.3, value.var = "x")
if(!'80'%in%names(res_ag)){
  res_ag$`80`<-0
  
}
##res_ag[is.na(res_ag$`10`), ]$`10` <- 0
res_ag[is.na(res_ag$`80`), ]$`80` <- 0
res_ag[is.na(res_ag$`90`), ]$`90` <- 0
res_ag$total <-  res_ag$`80` + res_ag$`90`
res_ag$rate <- res_ag$`90` / res_ag$total
res_ag <- res_ag[, c('Group.1', 'Group.2', '90', 'total', 'rate')]
# res_ag[res_ag$Group.2 == 'BSB', ]$Group.2 <- '包商'
# res_ag[res_ag$Group.2 == 'MSXF', ]$Group.2 <- '马上消费'
# res_ag[res_ag$Group.2 == 'YCWD', ]$Group.2 <- '粤财网贷'
# res_ag[res_ag$Group.2 == 'YCXT', ]$Group.2 <- '粤财信托'
# res_ag[res_ag$Group.2 == 'ZWXD', ]$Group.2 <- '众网小贷'
# res_ag[res_ag$Group.2 == 'ZXXT', ]$Group.2 <- '中信信托'
# res_ag[res_ag$Group.2 == 'ZABX', ]$Group.2 <- '众安保险'
# res_ag[res_ag$Group.2 == 'XYXJ', ]$Group.2 <- '兴业消金'
# res_ag[res_ag$Group.2 == 'BXBK', ]$Group.2 <- '百信银行'
# res_ag[res_ag$Group.2 == 'FOTIC', ]$Group.2 <- '外贸信托'
# res_ag[res_ag$Group.2 == 'SZYH', ]$Group.2 <- '苏州银行'
# res_ag[res_ag$Group.2 == 'YNXT', ]$Group.2 <- '云南信托'
# res_ag[res_ag$Group.2 == 'HYXF', ]$Group.2 <- '杭银消费'






names(res_ag) <- c('银行', '渠道', '成功数', '总数', '成功率')
#------------------------#
res_ag_c<-
  aggregate(result$totalAmt,
            list(result$bankName_uni, result$channelId, result$status),
            sum)
res_ag_c <- dcast(res_ag_c, Group.1 + Group.2 ~ Group.3, value.var = "x")
##res_ag_c[is.na(res_ag_c$`10`), ]$`10` <- 0
if(!'80'%in%names(res_ag_c)){
  res_ag_c$`80`<-0
}
res_ag_c[is.na(res_ag_c$`80`), ]$`80` <- 0
res_ag_c[is.na(res_ag_c$`90`), ]$`90` <- 0
res_ag_c$total <-  res_ag_c$`80` + res_ag_c$`90`
res_ag_c<-res_ag_c[,c('Group.1', 'Group.2', '90', 'total')]
# res_ag_c[res_ag_c$Group.2 == 'BSB', ]$Group.2 <- '包商'
# res_ag_c[res_ag_c$Group.2 == 'MSXF', ]$Group.2 <- '马上消费'
# res_ag_c[res_ag_c$Group.2 == 'YCWD', ]$Group.2 <- '粤财网贷'
# res_ag_c[res_ag_c$Group.2 == 'YCXT', ]$Group.2 <- '粤财信托'
# res_ag_c[res_ag_c$Group.2 == 'ZWXD', ]$Group.2 <- '众网小贷'
# res_ag_c[res_ag_c$Group.2 == 'ZXXT', ]$Group.2 <- '中信信托'
# res_ag_c[res_ag_c$Group.2 == 'ZABX', ]$Group.2 <- '众安保险'
# res_ag_c[res_ag_c$Group.2 == 'XYXJ', ]$Group.2 <- '兴业消金'
# res_ag_c[res_ag_c$Group.2 == 'BXBK', ]$Group.2 <- '百信银行'
# res_ag_c[res_ag_c$Group.2 == 'FOTIC', ]$Group.2 <- '外贸信托'
# res_ag_c[res_ag_c$Group.2 == 'SZYH', ]$Group.2 <- '苏州银行'
# res_ag_c[res_ag_c$Group.2 == 'YNXT', ]$Group.2 <- '云南信托'
# res_ag_c[res_ag_c$Group.2 == 'HYXF', ]$Group.2 <- '杭银消费'






names(res_ag_c) <- c('银行', '渠道', '成功金额', '总金额')
res_ag_total<- merge(res_ag,res_ag_c,by=c('银行', '渠道'),all=T)
res_ag_total<-res_ag_total[,c(1,2,3,6,4,7,5)]




bb_name_short <- bb_name[, c(1, 2, 3, 4)]
bb_name_short$channel <- '萨摩耶代收'
bb_name_short$total <-
  bb_name_short$成功数+bb_name_short$`失败(异常)数` + bb_name_short$`失败(余额不足)数`
bb_name_short$rate <- bb_name_short$成功数 / bb_name_short$total
bb_name_short <- bb_name_short[-dim(bb_name_short)[1], c(1, 5, 2, 6, 7)]
names(bb_name_short) <- c('银行', '渠道', '成功数', '总数', '成功率')


res_ag_total <- rbind(res_ag_total, for_6_merge)
res_ag_total$银行 <- reorder(res_ag_total$银行, res_ag_total$总数, sum)
res_ag_total <- arrange(res_ag_total, desc(res_ag_total$银行),-res_ag_total$成功率)
res_ag_total <-
  rbind(res_ag_total,
        list(
          '总计',
          '总计',
          sum(res_ag_total$成功数),
          sum(res_ag_total$成功金额),
          sum(res_ag_total$总数),
          sum(res_ag_total$总金额),
          
          
          
          sum(res_ag_total$成功数) / sum(res_ag_total$总数)
        )) ##会报错，但无影响
res_ag_total$失败数 <- res_ag_total$总数-res_ag_total$成功数
res_ag_total$失败金额 <- res_ag_total$总金额-res_ag_total$成功金额

res_ag_total <- res_ag_total[, c(1, 2, 3, 4,8,9 ,5,6,7)]
res_ag_total_short <-
  aggregate(res_ag_total[, c(3, 4, 5,6,7,8)], list(res_ag_total$渠道), sum)
names(res_ag_total_short)[1] <- c('渠道')
res_ag_total_short$成功率 <- res_ag_total_short$成功数 / res_ag_total_short$总数
res_ag_total_short <-
  rbind(arrange(res_ag_total_short[-dim(res_ag_total_short)[1], ], -res_ag_total_short[-dim(res_ag_total_short)[1], ]$总数), res_ag_total_short[dim(res_ag_total_short)[1], ])
write.table(
  res_ag_total_short,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "总体代收(请求维度).csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)
bb_name_short <-
  bb_name_short[bb_name_short$银行 %in% res_ag$银行, ]###将资金方没有的银行剔除

png(
 filename = paste(
   mainpath,"/pic/",
   gsub('-', '', substr(start_time, 1, 10)),
   "各资金方成功率图.png",
   sep = ''
 ),
 ###生成银行图
 width = 1400 ,
 height = 600
)

ggplot(data = res_ag) +geom_line(
  data = bb_name_short,
  
  aes(
    x = bb_name_short$银行,
    y = bb_name_short$成功率,
    group = 1
  ),
  alpha = 1,
  size = 1.5,
  colour = 'green'
  
) + geom_text(
  data = bb_name_short,
  aes(x = bb_name_short$银行, y = bb_name_short$成功率),
  label = '萨摩耶成功率',
  size = 4.5
) + geom_point(
  aes(
    x = res_ag$银行,
    y = res_ag$成功率,
    fill = res_ag$渠道,
    group = res_ag$渠道
  ),
  shape=21,
  alpha = 0.3,
  size = 11
  ##position = position_jitter(0.2)   去除扰动
) + geom_text(
  aes(
    x = res_ag$银行,
    y = res_ag$成功率,
    label = res_ag$总数,
    
    group = res_ag$渠道
  ),
  ##position = position_jitter(0.25),   去除扰动
  size = 4.3
) + scale_y_continuous(limits = c(0, 1.05), breaks = c(seq(0, 1, 0.05))) +
  theme(text = element_text(family = 'STXihei', size = 18)) + labs(
    title = paste('各资金方成功率'),
    x = '银行',
    y = '成功率',
    fill = '资金方'
  )+scale_fill_brewer(palette = 'Set3')
dev.off()
result$bankName_uni<-reorder(result$bankName_uni, rep(-1, length(result$bankName_uni)), sum)
result$group<-factor(result$group, levels = label[seq(16, 1)], ordered = T)
png(
  filename = paste(
    mainpath,"/pic/",
    gsub('-', '', substr(start_time, 1, 10)),
    "各资金方耗时图.png",
    sep = ''
  ),
  ###生成银行图
  width = 1400 ,
  height = 700
)
ggplot(data = result,aes(
  x = bankName_uni,
  fill = group
)) + geom_bar(position = 'fill') + scale_fill_manual(
  values = c(
    '#001107',
    '#88001B',
    '#FF0033',
    '#FF6600',
    '#FF0099',
    '#FF00FF',
    '#CC00FF',
    '#9900FF',
    '#6600FF',
    '#0033FF',
    '#0099FF',
    '#00CCFF',
    '#00FFFF',
    '#00FFCC',
    '#F1F141',
    '#99FF00'
  )
) + theme(text = element_text(family = 'STXihei', size = 15))+facet_grid(channelId~.)
dev.off()

res_reroute_agg<-merge(dcast(aggregate(result_reroute$idNo,list(result_reroute$bankName_uni,result_reroute$status),length),Group.1~Group.2,value.var = 'x'),dcast(aggregate(result_reroute$totalAmt,list(result_reroute$bankName_uni,result_reroute$status),sum),Group.1~Group.2,value.var = 'x'),all = T,by='Group.1')
res_reroute_agg<-res_reroute_agg[,c(1,2,4,3,5)]
res_reroute_agg[is.na(res_reroute_agg)]<-0
res_reroute_agg<-arrange(res_reroute_agg,-res_reroute_agg$`90.x`)
res_reroute_agg<-rbind( res_reroute_agg,cbind(list(Group.1='总计'),summarise(res_reroute_agg,`80.x`=sum(`80.x`),`80.y`=sum(`80.y`),`90.x`=sum(`90.x`),`90.y`=sum(`90.y`))))
res_reroute_agg$rate<-res_reroute_agg$`90.x`/(res_reroute_agg$`90.x`+res_reroute_agg$`80.x`)
names(res_reroute_agg)<-c('银行','失败数','失败金额','成功数','成功金额','成功率')
write.table(
  res_reroute_agg,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "重路由结果.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)