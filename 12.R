start_time <- '2018-10-18+00:00:00'
end_time <-
  '2018-10-19+00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
loop_time_reroute <- NA
start_num <- 0
length <- 5000
result_reroute <- data.frame()
handle <-
  getCurlHandle(
    httpheader = list(
      Accept = 'application/json, text/javascript, */*; q=0.01',
      'Accept-Ecoding' = 'gzip, deflate',
      'Accept-Language' = 'zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=BF99069F7503730A06436D6F856494F7; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=2819b3c20fcbadfea58b5f4a0b8c66fd; userId=s00580; userType=CBUSER',
      Referer = 'http://172.18.32.14:8080/pcs-oms-new/payment/transOrderInfo/list?token=3cf075a6f585a7e414a08e794d777509&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )

get_reroute_data <- function(s_time, e_time, start_num, length, handle) {
  form<-c(
    'draw'='1',
    'columns[0][data]'='transSeqno',
    'columns[0][name]'='',
    'columns[0][searchable]'='true',
    'columns[0][orderable]'='true',
    'columns[0][search][value]'='',
    'columns[0][search][regex]'='false',
    'columns[1][data]'='custNo',
    'columns[1][name]'='',
    'columns[1][searchable]'='true',
    'columns[1][orderable]'='false',
    'columns[1][search][value]'='',
    'columns[1][search][regex]'='false',
    'columns[2][data]'='custName',
    'columns[2][name]'='',
    'columns[2][searchable]'='true',
    'columns[2][orderable]'='false',
    'columns[2][search][value]'='',
    'columns[2][search][regex]'='false',
    'columns[3][data]'='bankName',
    'columns[3][name]'='',
    'columns[3][searchable]'='true',
    'columns[3][orderable]'='false',
    'columns[3][search][value]'='',
    'columns[3][search][regex]'='false',
    'columns[4][data]'='bankCardNo',
    'columns[4][name]'='',
    'columns[4][searchable]'='true',
    'columns[4][orderable]'='false',
    'columns[4][search][value]'='',
    'columns[4][search][regex]'='false',
    'columns[5][data]'='transAmt',
    'columns[5][name]'='',
    'columns[5][searchable]'='true',
    'columns[5][orderable]'='false',
    'columns[5][search][value]'='',
    'columns[5][search][regex]'='false',
    'columns[6][data]'='transType',
    'columns[6][name]'='',
    'columns[6][searchable]'='true',
    'columns[6][orderable]'='false',
    'columns[6][search][value]'='',
    'columns[6][search][regex]'='false',
    'columns[7][data]'='channelName',
    'columns[7][name]'='',
    'columns[7][searchable]'='true',
    'columns[7][orderable]'='false',
    'columns[7][search][value]'='',
    'columns[7][search][regex]'='false',
    'columns[8][data]'='rerouteChannelId',
    'columns[8][name]'='',
    'columns[8][searchable]'='true',
    'columns[8][orderable]'='false',
    'columns[8][search][value]'='',
    'columns[8][search][regex]'='false',
    'columns[9][data]'='returnCode',
    'columns[9][name]'='',
    'columns[9][searchable]'='true',
    'columns[9][orderable]'='false',
    'columns[9][search][value]'='',
    'columns[9][search][regex]'='false',
    'columns[10][data]'='returnInfo',
    'columns[10][name]'='',
    'columns[10][searchable]'='true',
    'columns[10][orderable]'='true',
    'columns[10][search][value]'='',
    'columns[10][search][regex]'='false',
    'columns[11][data]'='createDatetime',
    'columns[11][name]'='',
    'columns[11][searchable]'='true',
    'columns[11][orderable]'='true',
    'columns[11][search][value]'='',
    'columns[11][search][regex]'='false',
    'columns[12][data]'='updateDatetime',
    'columns[12][name]'='',
    'columns[12][searchable]'='true',
    'columns[12][orderable]'='true',
    'columns[12][search][value]'='',
    'columns[12][search][regex]'='false',
    'order[0][column]'='11',
    'order[0][dir]'='desc',
    'start'=start_num,
    'length'=length,
    'search[value]'='',
    'search[regex]'='false',
    'startUpdateDatetime'=s_time,
    'endUpdateDatetime'=e_time,
    'transType'='1'
    
  )
  
  
  res <- postForm(
    uri = 'http://172.18.32.14:8080/pcs-oms-new/payment/transOrderLog/listPage',
    style = 'POST',
    curl = handle,
    .params = form
  )
  return(res)
}


get_res <- get_reroute_data(start_time, end_time, start_num, length, handle)
json_result <- fromJSON(get_res)
response_code <- json_result$code
total <- json_result$total
tmp <- json_result$rows
result_reroute <- rbind(result_reroute, tmp)
loop_time_reroute <- total %/% length + ifelse(total %% length > 0, 1, 0)
cat('共', total, '条数据，分', loop_time_reroute, '次完成，目前为第1次')
if (loop_time_reroute > 1) {
  for (i in 1:(loop_time_reroute - 1)) {
    start_num <- i * length
    get_res <-
      get_reroute_data(start_time, end_time, start_num, length, handle)
    json_result <- fromJSON(get_res)
    response_code <- json_result$code
    total <- json_result$total
    tmp <- json_result$rows
    result_reroute <- rbind(result_reroute, tmp)
    cat('共', total, '条数据，分', loop_time_reroute, '次完成，目前为第', i + 1, '次\n')
  }
}
cat('数据下载完成\n')
result_reroute$crea_time <-
  as.POSIXct(as.numeric(result_reroute$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
result$route_time<-0
result$last_time<-result$crea_time
result$last_channel<-NA


for (i in 1:dim(result_reroute)[1]) {
  cat(i, '\n')
  result[which(result$transSeqno == result_reroute[i,]$transSeqno),]$route_time <-
    result[which(result$transSeqno == result_reroute[i,]$transSeqno),]$route_time +
    1
  result[which(result$transSeqno == result_reroute[i,]$transSeqno),]$last_time <-
    as.POSIXct(as.numeric(
      ifelse(
        result[which(result$transSeqno == result_reroute[i,]$transSeqno),]$last_time <
          result_reroute[i,]$crea_time,
        result_reroute[i,]$crea_time,
        result[which(result$transSeqno == result_reroute[i,]$transSeqno),]$last_time
      )
    ) , origin = "1970-01-01 00:00:00")
  result[which(result$transSeqno == result_reroute[i, ]$transSeqno), ]$last_channel <-
    ifelse(
      result[which(result$transSeqno == result_reroute[i, ]$transSeqno), ]$last_time ==
        result_reroute[i, ]$crea_time,
      result_reroute[i, ]$channelName,
      result[which(result$transSeqno == result_reroute[i, ]$transSeqno), ]$last_channel
    )
}

