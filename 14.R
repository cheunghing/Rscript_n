
####方法：获得导致逾期的期次，返回df
get_repay_plan<-function(the_date,cookie){
  result<-data.frame()
  loop_time <- NA
  start_num <- 0
  length <- 5000
  draw <- '1'
  date<-the_date
  normal_fail <-
    c('.*金额.*不足.*','.*余额.*不足.*','.*额度不足.*')
  handle <-
    getCurlHandle(
      httpheader = list(
        Accept = 'application/json, text/javascript, */*; q=0.01',
        'Accept-Ecoding' = 'gzip, deflate',
        'Accept-Language' = 'zh-CN,zh;q=0.9',
        Connection = 'keep-alive',
        # 'Content-Length' = '',
        'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
        Cookie = cookie,
        Referer = 'http://172.18.32.14:8080/ncc-oms/repayapply/repayApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
        Host = '172.18.32.14:8080',
        'X-Requested-With' = 'XMLHttpRequest'
      )
    )
  
  
  
  getdata <-
    function(start_num,length,date,draw,handle) {
      form <- c(
        'draw'=draw,
        'columns[0][data]'='repayReqDetailId',
        'columns[0][name]'='',
        'columns[0][searchable]'='TRUE',
        'columns[0][orderable]'='TRUE',
        'columns[0][search][value]'='',
        'columns[0][search][regex]'='FALSE',
        'columns[1][data]'='repayReqId',
        'columns[1][name]'='',
        'columns[1][searchable]'='TRUE',
        'columns[1][orderable]'='TRUE',
        'columns[1][search][value]'='',
        'columns[1][search][regex]'='FALSE',
        'columns[2][data]'='cApplyId',
        'columns[2][name]'='',
        'columns[2][searchable]'='TRUE',
        'columns[2][orderable]'='TRUE',
        'columns[2][search][value]'='',
        'columns[2][search][regex]'='FALSE',
        'columns[3][data]'='capCode',
        'columns[3][name]'='',
        'columns[3][searchable]'='TRUE',
        'columns[3][orderable]'='FALSE',
        'columns[3][search][value]'='',
        'columns[3][search][regex]'='FALSE',
        'columns[4][data]'='capSubCode',
        'columns[4][name]'='',
        'columns[4][searchable]'='TRUE',
        'columns[4][orderable]'='FALSE',
        'columns[4][search][value]'='',
        'columns[4][search][regex]'='FALSE',
        'columns[5][data]'='loanId',
        'columns[5][name]'='',
        'columns[5][searchable]'='TRUE',
        'columns[5][orderable]'='TRUE',
        'columns[5][search][value]'='',
        'columns[5][search][regex]'='FALSE',
        'columns[6][data]'='period',
        'columns[6][name]'='',
        'columns[6][searchable]'='TRUE',
        'columns[6][orderable]'='TRUE',
        'columns[6][search][value]'='',
        'columns[6][search][regex]'='FALSE',
        'columns[7][data]'='scheduleDate',
        'columns[7][name]'='',
        'columns[7][searchable]'='TRUE',
        'columns[7][orderable]'='TRUE',
        'columns[7][search][value]'='',
        'columns[7][search][regex]'='FALSE',
        'columns[8][data]'='delayPhase',
        'columns[8][name]'='',
        'columns[8][searchable]'='TRUE',
        'columns[8][orderable]'='TRUE',
        'columns[8][search][value]'='',
        'columns[8][search][regex]'='FALSE',
        'columns[9][data]'='repayStatus',
        'columns[9][name]'='',
        'columns[9][searchable]'='TRUE',
        'columns[9][orderable]'='TRUE',
        'columns[9][search][value]'='',
        'columns[9][search][regex]'='FALSE',
        'columns[10][data]'='routeResult',
        'columns[10][name]'='',
        'columns[10][searchable]'='TRUE',
        'columns[10][orderable]'='TRUE',
        'columns[10][search][value]'='',
        'columns[10][search][regex]'='FALSE',
        'columns[11][data]'='smyTransAmt',
        'columns[11][name]'='',
        'columns[11][searchable]'='TRUE',
        'columns[11][orderable]'='FALSE',
        'columns[11][search][value]'='',
        'columns[11][search][regex]'='FALSE',
        'columns[12][data]'='smyPrincipal',
        'columns[12][name]'='',
        'columns[12][searchable]'='TRUE',
        'columns[12][orderable]'='FALSE',
        'columns[12][search][value]'='',
        'columns[12][search][regex]'='FALSE',
        'columns[13][data]'='smyFee',
        'columns[13][name]'='',
        'columns[13][searchable]'='TRUE',
        'columns[13][orderable]'='FALSE',
        'columns[13][search][value]'='',
        'columns[13][search][regex]'='FALSE',
        'columns[14][data]'='smyServiceFee',
        'columns[14][name]'='',
        'columns[14][searchable]'='TRUE',
        'columns[14][orderable]'='FALSE',
        'columns[14][search][value]'='',
        'columns[14][search][regex]'='FALSE',
        'columns[15][data]'='smyOverdueInterest',
        'columns[15][name]'='',
        'columns[15][searchable]'='TRUE',
        'columns[15][orderable]'='FALSE',
        'columns[15][search][value]'='',
        'columns[15][search][regex]'='FALSE',
        'columns[16][data]'='smyOverdueFee',
        'columns[16][name]'='',
        'columns[16][searchable]'='TRUE',
        'columns[16][orderable]'='FALSE',
        'columns[16][search][value]'='',
        'columns[16][search][regex]'='FALSE',
        'columns[17][data]'='dayInterest',
        'columns[17][name]'='',
        'columns[17][searchable]'='TRUE',
        'columns[17][orderable]'='FALSE',
        'columns[17][search][value]'='',
        'columns[17][search][regex]'='FALSE',
        'columns[18][data]'='capTransAmt',
        'columns[18][name]'='',
        'columns[18][searchable]'='TRUE',
        'columns[18][orderable]'='FALSE',
        'columns[18][search][value]'='',
        'columns[18][search][regex]'='FALSE',
        'columns[19][data]'='capPrincipal',
        'columns[19][name]'='',
        'columns[19][searchable]'='TRUE',
        'columns[19][orderable]'='FALSE',
        'columns[19][search][value]'='',
        'columns[19][search][regex]'='FALSE',
        'columns[20][data]'='capFee',
        'columns[20][name]'='',
        'columns[20][searchable]'='TRUE',
        'columns[20][orderable]'='FALSE',
        'columns[20][search][value]'='',
        'columns[20][search][regex]'='FALSE',
        'columns[21][data]'='capServiceFee',
        'columns[21][name]'='',
        'columns[21][searchable]'='TRUE',
        'columns[21][orderable]'='FALSE',
        'columns[21][search][value]'='',
        'columns[21][search][regex]'='FALSE',
        'columns[22][data]'='capOverdueInterest',
        'columns[22][name]'='',
        'columns[22][searchable]'='TRUE',
        'columns[22][orderable]'='FALSE',
        'columns[22][search][value]'='',
        'columns[22][search][regex]'='FALSE',
        'columns[23][data]'='repayResultInfo',
        'columns[23][name]'='',
        'columns[23][searchable]'='TRUE',
        'columns[23][orderable]'='FALSE',
        'columns[23][search][value]'='',
        'columns[23][search][regex]'='FALSE',
        'columns[24][data]'='createDatetime',
        'columns[24][name]'='',
        'columns[24][searchable]'='TRUE',
        'columns[24][orderable]'='TRUE',
        'columns[24][search][value]'='',
        'columns[24][search][regex]'='FALSE',
        'columns[25][data]'='updateDatetime',
        'columns[25][name]'='',
        'columns[25][searchable]'='TRUE',
        'columns[25][orderable]'='TRUE',
        'columns[25][search][value]'='',
        'columns[25][search][regex]'='FALSE',
        'order[0][column]'='25',
        'order[0][dir]'='desc',
        'start'=start_num,
        'length'=length,
        'search[value]'='',
        'search[regex]'='FALSE',
        'scheduleDate'=date,
        'routeResult'='NCC'
      )
      res <- postForm(
        uri = 'http://172.18.32.14:8080/ncc-oms/repay_reform/repayPeriodInfo/listPage',
        style = 'POST',
        curl = handle,
        .params = form
      )
      return(res)
    }
  get_res<-getdata(start_num,length,date,draw,handle)
  json_result <- fromJSON(get_res)
  response_code <- json_result$code
  total <- json_result$total
  tmp <- json_result$rows
  result <- rbind(result, tmp)
  loop_time <- total %/% length + ifelse(total %% length > 0, 1, 0)
  cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次\n')
  if (loop_time > 1) {
    for (i in 1:(loop_time - 1)) {
      draw <- as.character(i)
      start_num <- i * length
      get_res <-
        getdata(
          start_num,
          length,
          date,draw,
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
  result$crea_time <-
    as.POSIXct(as.numeric(result$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
  result$up_time <-
    as.POSIXct(as.numeric(result$updateDatetime) / 1000, origin = "1970-01-01 00:00:00")
  result$enoughflag<-'N'
  for (i in normal_fail) {
    #####定义正常失败
    result[grepl(i, result$repayResultInfo) &
             result$repayStatus == '02',]$enoughflag <- 'Y'
    cat(i,'\n')
    
  }
  result[grepl('.*限制.*',result$repayResultInfo),]$enoughflag<-'N'
  
  result_90<-subset(result,result$repayStatus=='01')##选择成功的期次
  result_99<-subset(result,result$repayStatus!='01'&result$enoughflag=='N')##选择不成功且非余额不足的期次
  result_99_real_0<-subset(result_99,!result_99$loanId%in%result_90$loanId)##选择后续资金方也未能收回的期次
  result_99_real_1<-subset(result_99_real_0,substr(result_99_real_0$crea_time,1,10)==as.Date(date,format = '%Y%m%d')-3)##选择还款日等于应还日期的期次
  result_99_real_2<-result_99_real_1[!duplicated(result_99_real_1$loanId),]##去重
  return(result_99_real_2)
}


######方法：获得还款流水,输出df
get_repay_trx<-function(start_time,end_time,cookie){
  start_time <- paste(paste(substr(start_time,1,4),substr(start_time,5,6),substr(start_time,7,8),sep='-'),'00:00:00',sep=' ') ##'2018-11-09 00:00:00'
  end_time <- paste(paste(substr(end_time,1,4),substr(end_time,5,6),substr(end_time,7,8),sep='-'),'00:00:00',sep=' ')
  ##'2018-11-10 00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
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
        Cookie = cookie,
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
  cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次\n')
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
  return(result)
  
}



# ######
# result<-data.frame()
# loop_time <- NA
# start_num <- 0
# length <- 5000
# draw <- '1'
# date<-'20181114'
# normal_fail <-
#   c('.*金额.*不足.*','.*余额.*不足.*','.*额度不足.*')
# handle <-
#   getCurlHandle(
#     httpheader = list(
#       Accept = 'application/json, text/javascript, */*; q=0.01',
#       'Accept-Ecoding' = 'gzip, deflate',
#       'Accept-Language' = 'zh-CN,zh;q=0.9',
#       Connection = 'keep-alive',
#       # 'Content-Length' = '',
#       'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
#       Cookie = 'JSESSIONID=03E583EF37759464669E6EBDA76994C6; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=c37223fe710d5093af1b653c398052c5; userId=s00580; userType=CBUSER',
#       Referer = 'http://172.18.32.14:8080/ncc-oms/repayapply/repayApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
#       Host = '172.18.32.14:8080',
#       'X-Requested-With' = 'XMLHttpRequest'
#     )
#   )
# 
# 
# 
# getdata <-
#   function(start_num,length,date,draw,handle) {
#     form <- c(
#       'draw'=draw,
#       'columns[0][data]'='repayReqDetailId',
#       'columns[0][name]'='',
#       'columns[0][searchable]'='TRUE',
#       'columns[0][orderable]'='TRUE',
#       'columns[0][search][value]'='',
#       'columns[0][search][regex]'='FALSE',
#       'columns[1][data]'='repayReqId',
#       'columns[1][name]'='',
#       'columns[1][searchable]'='TRUE',
#       'columns[1][orderable]'='TRUE',
#       'columns[1][search][value]'='',
#       'columns[1][search][regex]'='FALSE',
#       'columns[2][data]'='cApplyId',
#       'columns[2][name]'='',
#       'columns[2][searchable]'='TRUE',
#       'columns[2][orderable]'='TRUE',
#       'columns[2][search][value]'='',
#       'columns[2][search][regex]'='FALSE',
#       'columns[3][data]'='capCode',
#       'columns[3][name]'='',
#       'columns[3][searchable]'='TRUE',
#       'columns[3][orderable]'='FALSE',
#       'columns[3][search][value]'='',
#       'columns[3][search][regex]'='FALSE',
#       'columns[4][data]'='capSubCode',
#       'columns[4][name]'='',
#       'columns[4][searchable]'='TRUE',
#       'columns[4][orderable]'='FALSE',
#       'columns[4][search][value]'='',
#       'columns[4][search][regex]'='FALSE',
#       'columns[5][data]'='loanId',
#       'columns[5][name]'='',
#       'columns[5][searchable]'='TRUE',
#       'columns[5][orderable]'='TRUE',
#       'columns[5][search][value]'='',
#       'columns[5][search][regex]'='FALSE',
#       'columns[6][data]'='period',
#       'columns[6][name]'='',
#       'columns[6][searchable]'='TRUE',
#       'columns[6][orderable]'='TRUE',
#       'columns[6][search][value]'='',
#       'columns[6][search][regex]'='FALSE',
#       'columns[7][data]'='scheduleDate',
#       'columns[7][name]'='',
#       'columns[7][searchable]'='TRUE',
#       'columns[7][orderable]'='TRUE',
#       'columns[7][search][value]'='',
#       'columns[7][search][regex]'='FALSE',
#       'columns[8][data]'='delayPhase',
#       'columns[8][name]'='',
#       'columns[8][searchable]'='TRUE',
#       'columns[8][orderable]'='TRUE',
#       'columns[8][search][value]'='',
#       'columns[8][search][regex]'='FALSE',
#       'columns[9][data]'='repayStatus',
#       'columns[9][name]'='',
#       'columns[9][searchable]'='TRUE',
#       'columns[9][orderable]'='TRUE',
#       'columns[9][search][value]'='',
#       'columns[9][search][regex]'='FALSE',
#       'columns[10][data]'='routeResult',
#       'columns[10][name]'='',
#       'columns[10][searchable]'='TRUE',
#       'columns[10][orderable]'='TRUE',
#       'columns[10][search][value]'='',
#       'columns[10][search][regex]'='FALSE',
#       'columns[11][data]'='smyTransAmt',
#       'columns[11][name]'='',
#       'columns[11][searchable]'='TRUE',
#       'columns[11][orderable]'='FALSE',
#       'columns[11][search][value]'='',
#       'columns[11][search][regex]'='FALSE',
#       'columns[12][data]'='smyPrincipal',
#       'columns[12][name]'='',
#       'columns[12][searchable]'='TRUE',
#       'columns[12][orderable]'='FALSE',
#       'columns[12][search][value]'='',
#       'columns[12][search][regex]'='FALSE',
#       'columns[13][data]'='smyFee',
#       'columns[13][name]'='',
#       'columns[13][searchable]'='TRUE',
#       'columns[13][orderable]'='FALSE',
#       'columns[13][search][value]'='',
#       'columns[13][search][regex]'='FALSE',
#       'columns[14][data]'='smyServiceFee',
#       'columns[14][name]'='',
#       'columns[14][searchable]'='TRUE',
#       'columns[14][orderable]'='FALSE',
#       'columns[14][search][value]'='',
#       'columns[14][search][regex]'='FALSE',
#       'columns[15][data]'='smyOverdueInterest',
#       'columns[15][name]'='',
#       'columns[15][searchable]'='TRUE',
#       'columns[15][orderable]'='FALSE',
#       'columns[15][search][value]'='',
#       'columns[15][search][regex]'='FALSE',
#       'columns[16][data]'='smyOverdueFee',
#       'columns[16][name]'='',
#       'columns[16][searchable]'='TRUE',
#       'columns[16][orderable]'='FALSE',
#       'columns[16][search][value]'='',
#       'columns[16][search][regex]'='FALSE',
#       'columns[17][data]'='dayInterest',
#       'columns[17][name]'='',
#       'columns[17][searchable]'='TRUE',
#       'columns[17][orderable]'='FALSE',
#       'columns[17][search][value]'='',
#       'columns[17][search][regex]'='FALSE',
#       'columns[18][data]'='capTransAmt',
#       'columns[18][name]'='',
#       'columns[18][searchable]'='TRUE',
#       'columns[18][orderable]'='FALSE',
#       'columns[18][search][value]'='',
#       'columns[18][search][regex]'='FALSE',
#       'columns[19][data]'='capPrincipal',
#       'columns[19][name]'='',
#       'columns[19][searchable]'='TRUE',
#       'columns[19][orderable]'='FALSE',
#       'columns[19][search][value]'='',
#       'columns[19][search][regex]'='FALSE',
#       'columns[20][data]'='capFee',
#       'columns[20][name]'='',
#       'columns[20][searchable]'='TRUE',
#       'columns[20][orderable]'='FALSE',
#       'columns[20][search][value]'='',
#       'columns[20][search][regex]'='FALSE',
#       'columns[21][data]'='capServiceFee',
#       'columns[21][name]'='',
#       'columns[21][searchable]'='TRUE',
#       'columns[21][orderable]'='FALSE',
#       'columns[21][search][value]'='',
#       'columns[21][search][regex]'='FALSE',
#       'columns[22][data]'='capOverdueInterest',
#       'columns[22][name]'='',
#       'columns[22][searchable]'='TRUE',
#       'columns[22][orderable]'='FALSE',
#       'columns[22][search][value]'='',
#       'columns[22][search][regex]'='FALSE',
#       'columns[23][data]'='repayResultInfo',
#       'columns[23][name]'='',
#       'columns[23][searchable]'='TRUE',
#       'columns[23][orderable]'='FALSE',
#       'columns[23][search][value]'='',
#       'columns[23][search][regex]'='FALSE',
#       'columns[24][data]'='createDatetime',
#       'columns[24][name]'='',
#       'columns[24][searchable]'='TRUE',
#       'columns[24][orderable]'='TRUE',
#       'columns[24][search][value]'='',
#       'columns[24][search][regex]'='FALSE',
#       'columns[25][data]'='updateDatetime',
#       'columns[25][name]'='',
#       'columns[25][searchable]'='TRUE',
#       'columns[25][orderable]'='TRUE',
#       'columns[25][search][value]'='',
#       'columns[25][search][regex]'='FALSE',
#       'order[0][column]'='25',
#       'order[0][dir]'='desc',
#       'start'=start_num,
#       'length'=length,
#       'search[value]'='',
#       'search[regex]'='FALSE',
#       'scheduleDate'=date,
#       'routeResult'='NCC'
#       )
#     res <- postForm(
#       uri = 'http://172.18.32.14:8080/ncc-oms/repay_reform/repayPeriodInfo/listPage',
#       style = 'POST',
#       curl = handle,
#       .params = form
#     )
#     return(res)
#   }
# get_res<-getdata(start_num,length,date,draw,handle)
# json_result <- fromJSON(get_res)
# response_code <- json_result$code
# total <- json_result$total
# tmp <- json_result$rows
# result <- rbind(result, tmp)
# loop_time <- total %/% length + ifelse(total %% length > 0, 1, 0)
# cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次')
# if (loop_time > 1) {
#   for (i in 1:(loop_time - 1)) {
#     draw <- as.character(i)
#     start_num <- i * length
#     get_res <-
#       getdata(
#               start_num,
#               length,
#               date,draw,
#               handle)
#     json_result <- fromJSON(get_res)
#     response_code <- json_result$code
#     total <- json_result$total
#     tmp <- json_result$rows
#     result <- rbind(result, tmp)
#     cat('共', total, '条数据，分', loop_time, '次完成，目前为第', i + 1, '次\n')
#     Sys.sleep(1)
#   }
# }
# cat('数据下载完成\n')
# result$crea_time <-
#   as.POSIXct(as.numeric(result$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
# result$up_time <-
#   as.POSIXct(as.numeric(result$updateDatetime) / 1000, origin = "1970-01-01 00:00:00")
# result$enoughflag<-'N'
# for (i in normal_fail) {
#   #####定义正常失败
#   result[grepl(i, result$repayResultInfo) &
#            result$repayStatus == '02',]$enoughflag <- 'Y'
#   cat(i,'\n')
#   
# }
# result[grepl('.*限制.*',result$repayResultInfo),]$enoughflag<-'N'
# 
# result_90<-subset(result,result$repayStatus=='01')##选择成功的期次
# result_99<-subset(result,result$repayStatus!='01'&result$enoughflag=='N')##选择不成功的期次
# result_99_real_0<-subset(result_99,!result_99$loanId%in%result_90$loanId)##选择后续资金方也未能收回的期次
# result_99_real_1<-subset(result_99_real_0,substr(result_99_real_0$crea_time,1,10)==as.Date(date,format = '%Y%m%d')-3)##选择还款日等于应还日期的期次
# result_99_real_2<-result_99_real_1[!duplicated(result_99_real_1$loanId),]##去重






# res_repay_plan<-data.frame()
# for (i in 1:dim(result_99)[1]) {
#   res_repay_plan<-rbind(res_repay_plan, fromJSON(getdata(result_99[i,]$cApplyId,handle))$rows)
#   cat(i,'finished\n')
# }
# res_repay_plan$crea_time <-
#   as.POSIXct(as.numeric(res_repay_plan$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
# res_repay_plan$up_time <-
#   as.POSIXct(as.numeric(res_repay_plan$updateDatetime) / 1000, origin = "1970-01-01 00:00:00")
# 
# 


