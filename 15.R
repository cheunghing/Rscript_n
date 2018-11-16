######方法：获取NCC还款流水
get_repay_trx<-function(s_time,cookie){
  start_time <- paste(paste(substr(s_time,1,4),substr(s_time,5,6),substr(s_time,7,8),sep='-'),'00:00:00',sep=' ') ##'2018-11-09 00:00:00'
  end_time <- paste(paste(substr(format(as.Date(s_time,format = '%Y%m%d')+1,'%Y%m%d'),1,4),substr(format(as.Date(s_time,format = '%Y%m%d')+1,'%Y%m%d'),5,6),substr(format(as.Date(s_time,format = '%Y%m%d')+1,'%Y%m%d'),7,8),sep='-'),'00:00:00',sep=' ')
  cat(start_time,'\n')
  cat(end_time,'\n')
  ##'2018-11-10 00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
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
get_notify_info<-function(s_time,cookie){
  start_time <- paste(paste(substr(s_time,1,4),substr(s_time,5,6),substr(s_time,7,8),sep='-'),'00:00:00',sep=' ') ##'2018-11-09 00:00:00'
  end_time <- paste(paste(substr(format(as.Date(s_time,format = '%Y%m%d')+1,'%Y%m%d'),1,4),substr(format(as.Date(s_time,format = '%Y%m%d')+1,'%Y%m%d'),5,6),substr(format(as.Date(s_time,format = '%Y%m%d')+1,'%Y%m%d'),7,8),sep='-'),'00:00:00',sep=' ')
  cat(start_time,'\n')
  cat(end_time,'\n')
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
  return(result)
  cat('数据下载完成\n')
  
}


date<-as.character(format(seq(from=as.Date('20181110','%Y%m%d'),to=as.Date('20181111','%Y%m%d'),by='day'),'%Y%m%d'))##输入起始日期
cookie<-'JSESSIONID=D05601800E12A4BA24E027EC91BE47B9; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=66d46edcecb7a82aa5eb1cc44f170a67; userId=s00580; userType=CBUSER'##输入登录token
result_ncc_trx<-data.frame()
result_notify_info<-data.frame()

begin_time<-Sys.time()
for (i in 1:length(date)) {###遍历日期调用get_repay_trx()
  result_ncc_trx<-rbind(result_ncc_trx,get_repay_trx(date[i],cookie))
  cat('完成ncc_trx',date[i],'\n')
}
for (i in 1:length(date)) {###遍历日期调用get_notify_info()
  result_notify_info<-rbind(result_notify_info,get_notify_info(date[i],cookie))
  cat('完成notify_info',date[i],'\n')
}
cat('所有数据下载完成',difftime(Sys.time(),begin_time,units = 'secs'))

result_ncc_trx<-subset(result_ncc_trx,result_ncc_trx$status%in%c('80','90'))##只选取成功&失败
result_ncc_trx<-subset(result_ncc_trx,result_ncc_trx$channelId!='PCS')##取非pcs的
result_ncc_trx$crea_time <-
  as.POSIXct(as.numeric(result_ncc_trx$createDatetime) / 1000, origin = "1970-01-01 00:00:00")
result_ncc_trx$req_time <-
  as.POSIXct(as.numeric(result_ncc_trx$requestDatetime) / 1000, origin = "1970-01-01 00:00:00")
result_ncc_trx$noti_time <-
  as.POSIXct(as.numeric(result_ncc_trx$updateDatetime) / 1000, origin = "1970-01-01 00:00:00")
result_ncc_trx$noti_time[which(!result_ncc_trx$status %in% c('80', '90'))] <-
  Sys.time()
result_ncc_trx$take_time <- result_ncc_trx$noti_time - result_ncc_trx$crea_time
split <- ##耗时分区
  c(
    -Inf,
    60,
    60 * 5,
    60 * 10,
    60 * 30,
    60 * 60,
    3600 * 3,
    3600 * 6,
    3600 * 12,
    3600 * 24,
    +Inf
  )
label <-
  c(
    '小于1min',
    '大于1min小于5min',
    '大于5min小于10min',
    '大于10min小于30min',
    '大于30min小于1hour',
    '大于1hour小于3hour',
    '大于3hour小于6hour',
    '大于6hour小于12hour',
    '大于12hour小于1day',
    '大于1day'
  )
result_ncc_trx$group <-
  cut(as.numeric(result_ncc_trx$take_time), split, label)##划分耗时区间
result_ncc_trx$bankName_uni<-result_ncc_trx$bankName
result_ncc_trx[grep('.*建设.*',result_ncc_trx$bankName),]$bankName_uni<-'建设银行'
result_ncc_trx[grep('.*工商*',result_ncc_trx$bankName),]$bankName_uni<-'工商银行'
result_ncc_trx[grep('.*浦东*',result_ncc_trx$bankName),]$bankName_uni<-'浦发银行'
result_ncc_trx[grep('.*农业*',result_ncc_trx$bankName),]$bankName_uni<-'农业银行'
result_ncc_trx[grep('.*交通*',result_ncc_trx$bankName),]$bankName_uni<-'交通银行'
result_ncc_trx[grep('.*民生*',result_ncc_trx$bankName),]$bankName_uni<-'民生银行'
result_ncc_trx[grep('.*邮政*',result_ncc_trx$bankName),]$bankName_uni<-'邮储银行'
result_ncc_trx[grep('.*广东发展.*',result_ncc_trx$bankName),]$bankName_uni<-'广发银行'
result_ncc_trx[grep('.*光大*',result_ncc_trx$bankName),]$bankName_uni<-'光大银行'
#++++++++++++++++++++++++++#
result_ncc_trx$channelId_c<-NA
result_ncc_trx[result_ncc_trx$channelId == 'BSB', ]$channelId_c <- '包商'
result_ncc_trx[result_ncc_trx$channelId == 'MSXF', ]$channelId_c <- '马上消费'
result_ncc_trx[result_ncc_trx$channelId == 'YCWD', ]$channelId_c <- '粤财网贷'
result_ncc_trx[result_ncc_trx$channelId == 'YCXT', ]$channelId_c <- '粤财信托'
result_ncc_trx[result_ncc_trx$channelId == 'ZWXD', ]$channelId_c <- '众网小贷'
result_ncc_trx[result_ncc_trx$channelId == 'ZXXT', ]$channelId_c <- '中信信托'
result_ncc_trx[result_ncc_trx$channelId == 'ZABX', ]$channelId_c <- '众安保险'
result_ncc_trx[result_ncc_trx$channelId == 'XYXJ', ]$channelId_c <- '兴业消金'
result_ncc_trx[result_ncc_trx$channelId == 'BXBK', ]$channelId_c <- '百信银行'
result_ncc_trx[result_ncc_trx$channelId == 'FOTIC', ]$channelId_c <- '外贸信托'
result_ncc_trx[result_ncc_trx$channelId == 'SZYH', ]$channelId_c <- '苏州银行'
result_ncc_trx[result_ncc_trx$channelId == 'YNXT', ]$channelId_c <- '云南信托'
result_ncc_trx[result_ncc_trx$channelId == 'HYXF', ]$channelId_c <- '杭银消费'
result_ncc_trx$enoughflag<-'N'
result_ncc_trx[grepl('.*金额.*不足.*', result_ncc_trx$responseInfo)&result_ncc_trx$status == '80',]$enoughflag <- 'Y'
result_ncc_trx[grepl('.*余额.*不足.*', result_ncc_trx$responseInfo)&result_ncc_trx$status == '80',]$enoughflag <- 'Y'
result_ncc_trx[grepl('.*额度不足.*', result_ncc_trx$responseInfo)&result_ncc_trx$status == '80',]$enoughflag <- 'Y'
result_ncc_trx[grepl('.*限制.*',result_ncc_trx$responseInfo),]$enoughflag<-'N'
result_ncc_trx[which(result_ncc_trx$status=='80'&result_ncc_trx$enoughflag=='Y'),]$status<-'81'
cap_list<-names(table(result_ncc_trx$channelId))
res<-list()
for (i in 1:length(cap_list)) {
  cat('开始处理：',cap_list[i])
  eval(parse(text =paste("temp_df<-subset(result_ncc_trx,result_ncc_trx$channelId=='",cap_list[i],"')",sep = '')))
  temp_df_1<-dcast(aggregate(temp_df$idNo,list(temp_df$bankName_uni,temp_df$status),length),Group.1~Group.2,value.var = 'x')
  if(!'81'%in%names(temp_df_1)){
    temp_df_1$`81`<-0
  }
  temp_df_1[is.na(temp_df_1)]<-0
  
  temp_df_1$`80_rate`<-temp_df_1$`80`/(temp_df_1$`80`+temp_df_1$`81`+temp_df_1$`90`)
  temp_df_1$`81_rate`<-temp_df_1$`81`/(temp_df_1$`80`+temp_df_1$`81`+temp_df_1$`90`)
  temp_df_1$`90_rate`<-temp_df_1$`90`/(temp_df_1$`80`+temp_df_1$`81`+temp_df_1$`90`)
  temp_df_1<-temp_df_1[,c('Group.1','80','80_rate','81','81_rate','90','90_rate')]
  names(temp_df_1)<-c('银行','异常数','异常率','余额不足数','余额不足率','成功数','成功率')
  temp_df_1<-arrange(temp_df_1,-temp_df_1$成功数)
  temp_df_1<-rbind(temp_df_1,list('总计',sum(temp_df_1$异常数),sum(temp_df_1$异常数)/(sum(temp_df_1$异常数)+sum(temp_df_1$余额不足数)+sum(temp_df_1$成功数)),sum(temp_df_1$余额不足数),sum(temp_df_1$异常数)/(sum(temp_df_1$异常数)+sum(temp_df_1$余额不足数)+sum(temp_df_1$成功数)),sum(temp_df_1$成功数),sum(temp_df_1$成功数)/(sum(temp_df_1$异常数)+sum(temp_df_1$余额不足数)+sum(temp_df_1$成功数))))
  #####
  temp_df_2<-subset(temp_df,temp_df$status=='80')
  temp_df_2<-aggregate(temp_df_2$idNo,list(temp_df_2$responseInfo),length)
  temp_df_2$rate<-temp_df_2$x/sum(temp_df_2$x)
  temp_df_2<-arrange(temp_df_2,-temp_df_2$rate)
  #####
  temp_df_3<-aggregate(temp_df$idNo,list(temp_df$group),length)
  temp_df_3$rate<-temp_df_3$x/sum(temp_df_3$x)
  temp_df_3<-arrange(temp_df_3,-temp_df_3$rate)
  #####
  temp_df_4<-dcast(aggregate(temp_df$idNo,list(temp_df$bankName_uni,temp_df$group),length),Group.1~Group.2,value.var = 'x')
  temp_df_4[is.na(temp_df_4)]<-0
  temp_df_4$sum<-0
  for (j in 1:dim(temp_df_4)[1]) {
    temp_df_4[j,]$sum<-sum(temp_df_4[j,2:dim(temp_df_4)[2]])
  }
  for (k in 2:(dim(temp_df_4)[2]-1)) {
    temp_df_4[[as.character(k-1)]]<-temp_df_4[,k]/temp_df_4$sum
    
  }
  temp_df_4<-temp_df_4[,-which(names(temp_df_4)=='sum')]
  names(temp_df_4)[(2:((dim(temp_df_4)[2]-1)/2+1)+(dim(temp_df_4)[2]-1)/2)]<- paste( names(temp_df_4) [2:((dim(temp_df_4)[2]-1)/2+1)],'占比')
  
  tmp<-1
  for (l in 1:length(2:((dim(temp_df_4)[2]-1)/2+1))) {
    tmp<-c(tmp,(2:((dim(temp_df_4)[2]-1)/2+1))[l])
    tmp<-c(tmp,(2:((dim(temp_df_4)[2]-1)/2+1)+(dim(temp_df_4)[2]-1)/2)[l])
  }
  temp_df_4<-temp_df_4[,tmp]
  names(temp_df_4)[1]<-'银行'
  ###以下是还款通知统计占比
  eval(parse(text =paste("temp_df_noti<-subset(result_notify_info,result_notify_info$capitalCode=='",cap_list[i],"')",sep = '')))
  temp_df_5_1<-aggregate(temp_df_noti$repayAmt,list(temp_df_noti$capitalCode,temp_df_noti$repayType),sum)
  temp_df_5_1<-dcast(temp_df_5_1,Group.1~ Group.2,value.var = "x")
  temp_df_5_2[is.na(temp_df_5_2)]<-0
  
  
  temp_df_5_1$资金方金额占比<-temp_df_5_1$`00`/(temp_df_5_1$`00`+temp_df_5_1$`01`)
  temp_df_5_1$萨摩耶金额占比<-temp_df_5_1$`01`/(temp_df_5_1$`00`+temp_df_5_1$`01`)
  names(temp_df_5_1)[1:3]<-c('资金方','资金方金额','萨摩耶金额')

  temp_df_noti$flag<- paste(temp_df_noti$custNo,temp_df_noti$capitalCode,temp_df_noti$payOffDate,temp_df_noti$payOffTime,sep='')
  temp_df_noti<-temp_df_noti[!duplicated(temp_df_noti$flag),]
  temp_df_5_2<-aggregate(temp_df_noti$repayAmt,list(temp_df_noti$capitalCode,temp_df_noti$repayType),length)
  temp_df_5_2<-dcast(temp_df_5_2,Group.1~ Group.2,value.var = "x")
  temp_df_5_2[is.na(temp_df_5_2)]<-0
  temp_df_5_2$资金方笔数占比<-temp_df_5_2$`00`/(temp_df_5_2$`00`+temp_df_5_2$`01`)
  temp_df_5_2$萨摩耶笔数占比<-temp_df_5_2$`01`/(temp_df_5_2$`00`+temp_df_5_2$`01`)
  names(temp_df_5_2)[1:3]<-c('资金方','资金方笔数','萨摩耶笔数')
  
  temp_df_5<-merge(temp_df_5_1,temp_df_5_2,by=c('资金方'))
  temp_df_5<-temp_df_5[,c(1,2,4,3,5,6,8,7,9)]
  eval(parse(text =paste("res$",cap_list[i],"<-list(temp_df_1,temp_df_2,temp_df_3,temp_df_4,temp_df_5)",sep = '')))
  cat('处理完成：',cap_list[i],'i值为：',i,'\n')
}

