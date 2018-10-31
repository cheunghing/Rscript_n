rm(list = ls())
library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)
mainpath<-"D:/Rworkplace"##存储路径
setwd(mainpath)
trans_type <- '1'  ##1实时 4批量
start_time <- '2018-10-31+00:00:00'
end_time <-
  '2018-11-01+00:00:00'  ####format(Sys.time(), format = '%Y-%m-%d+%H:%M:%S')  #######
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
      Cookie = 'JSESSIONID=749F8B640628A4F114B1175A3448C938; theme=theme_base; userName=%E6%9D%8E%E9%95%BF%E5%85%B4; token=a9acde0ff6754a2f5ee6d240323af7c0; userId=s00580; userType=CBUSER',
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
           draw,
           trans_type) {
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
      'startUpdateDatetime' = s_time,
      'transType' = trans_type
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
          draw,
          trans_type)

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
              draw,
              trans_type)
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
    mainpath,'/hist_data/',
    gsub('-', '', substr(start_time, 1, 10)),
    '-',
    gsub('-', '', substr(end_time, 1, 10)),
    '-',
    trans_type,
    ".csv",
    sep = ''
  ),
  sep = ','
)
cat('数据保存本地完成\n')
#————————————————————————————————数据提取完毕——————————————————————————————————#
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

for (i in normal_fail) {
  #####定义正常失败
  result[grepl(i, result$returnInfo) &
           result$status == 'FAIL', 33] <- '失败(余额不足)'
  
}

result[result$status == 'FAIL', 33] <- '失败(异常)'
result[result$status == 'SUCCESS', 33] <- '成功'
result[result$status == 'PROCESSING', 33] <- '处理中'



result <-
  subset(result, result$transType %in% c('1'))#####再次筛选实时:1 or批扣:4

result <-
  result[format(result$crea_time, format = '%Y%m%d') %in% format(as.Date(start_time, format =
                                                                           '%Y-%m-%d+%H:%M:%S'), format = '%Y%m%d'):format(as.Date(end_time, format =
                                                                                                                                     '%Y-%m-%d+%H:%M:%S'), format = '%Y%m%d'),]
result$group <-
  cut(as.numeric(result$take_time), split, label)##划分耗时区间
result$group_amt <-
  cut(result$transAmt, split_amt, label_amt)##划分amt区间

if (trans_type == '1') {
  result[as.numeric(gsub('-', '', substr(result$crea_time, 1, 10))) >= 20180929, ]$notifyUrl <-
    'NA'
  result[is.na(result$notifyUrl), ]$notifyUrl <- 'NA'
  result <- subset(result, result$notifyUrl == 'NA')###把混入的批扣交易筛去
}
cat('原始数据整理完毕\n')


#——————————————————原始数据整理完毕————————————————#
aggregate_res <-
  aggregate(
    result$transSeqno,
    list(result$status, result$channelName, result$bankCode),
    length
  )
aggregate_res <-
  merge(
    aggregate_res,
    unique_bank_list,
    by.x = c('Group.3'),
    by.y = c('bankCode'),
    all.x = T
  )
aggregate_res[1] <- aggregate_res[5]
aggregate_res <- aggregate_res[-5]
names(aggregate_res) <- c('bank', 'status', 'channel', 'count')
aggregate_res <-
  subset(aggregate_res, select = c('status', 'bank', 'channel', 'count'))
succ <-
  subset(aggregate_res,
         aggregate_res$status == '成功',
         select = c('bank', 'channel', 'count'))
fail <-
  subset(aggregate_res,
         aggregate_res$status == '失败(异常)',
         select = c('bank', 'channel', 'count'))
fail_n <-
  subset(
    aggregate_res,
    aggregate_res$status == '失败(余额不足)',
    select = c('bank', 'channel', 'count')
  )
proc <-
  subset(aggregate_res,
         aggregate_res$status == '处理中',
         select = c('bank', 'channel', 'count'))
aa <-  ####银行&渠道
  merge(
    merge(
      merge(succ, fail, by = c('bank', 'channel'), all = T),
      fail_n,
      by = c('bank', 'channel'),
      all = T
    ),
    proc,
    by = c('bank', 'channel'),
    all = T
  )
names(aa) <-
  c('bank',
    'channel',
    'succ',
    'fail_abnor',
    'fail_nor',
    'processing')
for (i in 3:dim(aa)[2]) {
  aa[is.na(aa[, i]), i] <- 0
}
aa$fail_abnor_rate <-
  paste(round(aa$fail_abnor / (aa$succ + aa$fail_abnor + aa$fail_nor) * 100, 2), '%')
aa <-
  arrange(aa,-aa$fail_abnor / (aa$succ + aa$fail_abnor + aa$fail_nor) * 100)


aggregate.a <-
  aggregate(aggregate_res$count,
            list(aggregate_res$status,
                 aggregate_res$bank),
            sum)
names(aggregate.a) <- c('status', 'bank', 'count')


succ_1 <-
  subset(aggregate.a,
         aggregate.a$status == '成功',
         select = c('bank',  'count'))
fail_1 <-
  subset(aggregate.a,
         aggregate.a$status == '失败(异常)',
         select = c('bank',  'count'))
fail_n_1 <-
  subset(aggregate.a,
         aggregate.a$status == '失败(余额不足)',
         select = c('bank',  'count'))
proc_1 <-
  subset(aggregate.a,
         aggregate.a$status == '处理中',
         select = c('bank', 'count'))
bb <-
  merge(merge(
    merge(succ_1, fail_1, by = c('bank'), all = T),
    fail_n_1,
    by = c('bank'),
    all = T
  ),
  proc_1,
  by = c('bank'),
  all = T)
names(bb) <-
  c('bank',
    'succ',
    'fail_abnor',
    'fail_nor',
    'processing')
for (i in 2:dim(bb)[2]) {
  bb[is.na(bb[, i]), i] <- 0
}
bb$`失败(余额不足)率`   <-
  round(bb$fail_nor / (bb$succ + bb$fail_abnor + bb$fail_nor) , 4)
bb$成功率    <-
  round(bb$succ / (bb$succ + bb$fail_abnor + bb$fail_nor) , 4)
bb$`失败(异常)率`   <-
  round(bb$fail_abnor / (bb$succ + bb$fail_abnor + bb$fail_nor) , 4)
bb$处理中占比    <-
  round(bb$processing / (bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing) ,
        4)
bb$fail_abnor_rate_per <-
  paste(round(bb$fail_abnor / (bb$succ + bb$fail_abnor + bb$fail_nor) * 100, 4), '%')
bb <- ###银行
  arrange(bb,-bb$fail_abnor / (bb$succ + bb$fail_abnor + bb$fail_nor) * 100)
bb_melt <-
  melt(bb[, -10],
       id = c("bank", "succ", "fail_abnor", "fail_nor", "processing"))



# cc<-aggregate(result$transSeqno,list(result$group,result$bankCode),length)
# ccc<-aggregate(result$transSeqno,list(result$bankCode),length)
# cccc<-merge(cc,ccc,by.x = c('Group.2'),by.y = c('Group.1'))
# cccc$rate<-round(cccc$x.x/cccc$x.y,4)
# cccc<-merge(cccc,unique_bank_list,by.x = c('Group.2'),by.y = c('bankCode'),all.x = T)

aggregate.b <-
  aggregate(aggregate_res$count,
            list(aggregate_res$status,
                 aggregate_res$channel),
            sum)
names(aggregate.b) <- c('status', 'channel', 'count')

succ_2 <-
  subset(aggregate.b,
         aggregate.b$status == '成功',
         select = c('channel',  'count'))
fail_2 <-
  subset(aggregate.b,
         aggregate.b$status == '失败(异常)',
         select = c('channel',  'count'))
fail_n_2 <-
  subset(aggregate.b,
         aggregate.b$status == '失败(余额不足)',
         select = c('channel',  'count'))
proc_2 <-
  subset(aggregate.b,
         aggregate.b$status == '处理中',
         select = c('channel', 'count'))
cc <-
  merge(
    merge(
      merge(succ_2, fail_2, by = c('channel'), all = T),
      fail_n_2,
      by = c('channel'),
      all = T
    ),
    proc_2,
    by = c('channel'),
    all = T
  )
names(cc) <-
  c('channel',
    'succ',
    'fail_abnor',
    'fail_nor',
    'processing')
for (i in 2:dim(cc)[2]) {
  cc[is.na(cc[, i]), i] <- 0
}
cc$余额不足率    <-
  round(cc$fail_nor / (cc$succ + cc$fail_abnor + cc$fail_nor) , 4)
cc$成功率    <-
  round(cc$succ / (cc$succ + cc$fail_abnor + cc$fail_nor) , 4)
cc$异常率    <-
  round(cc$fail_abnor / (cc$succ + cc$fail_abnor + cc$fail_nor) , 4)
cc$处理中占比    <-
  round(cc$processing / (cc$succ + cc$fail_abnor + cc$fail_nor + cc$processing) ,
        4)
cc$fail_abnor_rate_per <-
  paste(round(cc$fail_abnor / (cc$succ + cc$fail_abnor + cc$fail_nor) * 100, 4), '%')
cc <- ###渠道
  arrange(cc,-cc$fail_abnor / (cc$succ + cc$fail_abnor + cc$fail_nor) * 100)
cc_melt <-
  melt(cc[, -10],
       id = c("channel", "succ", "fail_abnor", "fail_nor", "processing"))
fail_detail <- subset(result, result$status == '失败(异常)')
fail_detail <-
  aggregate(
    fail_detail$transSeqno,
    list(
      fail_detail$bankName.y,
      fail_detail$channelName,
      fail_detail$returnInfo
    ),
    length
  )
fail_detail <- subset(fail_detail, fail_detail$x > 0)
fail_detail <- arrange(fail_detail, -fail_detail$x)
fail_detail$rate <- fail_detail$x / sum(fail_detail$x)
names(fail_detail) <- c('银行', '渠道', '报错信息', '笔数', '占比')
sum(aa$succ) / (sum(aa$succ) + sum(aa$fail_abnor) + sum(aa$fail_nor))###总体成功率


write.table(
  fail_detail,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "异常汇总.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)
bb_name <- bb
names(bb_name) <-
  c(
    '银行',
    '成功数',
    '失败(异常)数',
    '失败(余额不足)数',
    '处理中数',
    '失败(余额不足)率',
    '成功率',
    '失败(异常)率',
    '处理中占比',
    '失败(异常)率%'
  )
bb_name <- bb_name[, c(1, 2, 3, 4, 6, 7, 8)]
bb_name <-
  rbind(bb_name,
        list(
          '总计',
          sum(bb_name$成功数),
          sum(bb_name$`失败(异常)数`),
          sum(bb_name$`失败(余额不足)数`),
          
          sum(bb_name$`失败(余额不足)数`) / (
            sum(bb_name$成功数) + sum(bb_name$`失败(异常)数`) + sum(bb_name$`失败(余额不足)数`)
          ),
          sum(bb_name$成功数) / (
            sum(bb_name$成功数) + sum(bb_name$`失败(异常)数`) + sum(bb_name$`失败(余额不足)数`)
          ),
          sum(bb_name$`失败(异常)数`) / (
            sum(bb_name$成功数) + sum(bb_name$`失败(异常)数`) + sum(bb_name$`失败(余额不足)数`)
          )
        ))
write.table(
  bb_name,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "各银行交易情况.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)

channel_amt <-
  aggregate(result$transAmt, list(result$channelName, result$status), sum)
channel_amt <-
  subset(channel_amt,
         channel_amt$Group.2 == '成功',
         select = c('Group.1', 'x'))
channel_partion <- cc[, c(1, 2)]
channel_partion$rate <- channel_partion$succ / sum(channel_partion$succ)
channel_partion <-
  merge(
    channel_partion,
    channel_amt,
    by.x = 'channel',
    by.y = 'Group.1',
    all = T
  )
channel_partion$rate_1 <- channel_partion$x / sum(channel_partion$x)
channel_partion <- arrange(channel_partion, -(channel_partion$succ))
names(channel_partion) <- c('渠道', '成功笔数', '成功笔数占比', '成功金额', '成功金额占比')
write.table(
  channel_partion,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "各渠道交易占比.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)

bank_amt <-
  aggregate(result$transAmt, list(result$bankName.y, result$status), sum)
bank_amt <-
  subset(bank_amt, bank_amt$Group.2 == '成功', select = c('Group.1', 'x'))
bank_partion <- bb[, c(1, 2)]
bank_partion$rate <- bank_partion$succ / sum(bank_partion$succ)
bank_partion <-
  merge(bank_partion,
        bank_amt,
        by.x = 'bank',
        by.y = 'Group.1',
        all = T)
bank_partion$rate_1 <- bank_partion$x / sum(bank_partion$x)
bank_partion <- arrange(bank_partion, -(bank_partion$succ))
names(bank_partion) <- c('银行', '成功笔数', '成功笔数占比', '成功金额', '成功金额占比')
write.table(
  bank_partion,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "各银行交易占比.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)
###iam mac haha 
a<-123








done_time <- aggregate(result$transSeqno, list(result$group), length)
done_time$rate <- round(done_time$x / sum(done_time$x), 4)
names(done_time) <- c('交易耗时分布', '笔数', '占比')
write.table(
  done_time,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "时间分布占比.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)

group_amt_agr <-
  aggregate(result$transSeqno, list(result$group_amt), length)
#——————————————————详情表生成完毕————————————————#
png(
  filename = paste(mainpath,"/pic/", gsub('-', '', substr(start_time, 1, 10)), "银行图.png", sep =
                     ''),
  ###生成银行图
  width = 1580 * 2,
  height = 780
)
ggplot(data = result) + geom_bar(aes(
  x = reorder(result$bankName.y, rep(-1, length(result$bankName.y)), sum),
  fill = factor(result$group, levels = label[seq(16, 1)], ordered = T)
), position = 'fill') + scale_fill_manual(
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
) + theme(text = element_text(family = 'STXihei', size = 30)) + geom_line(
  data = bb_melt,
  aes(
    x = bb_melt$bank,
    y = bb_melt$value,
    colour = bb_melt$variable,
    group = bb_melt$variable
  ),
  size = 1.5
) + scale_colour_manual(values =  c(
  成功率   = '#1B6D1B',
  `失败(余额不足)率`  = 'blue',
  `失败(异常)率`  = 'red',
  处理中占比   = 'purple'
)) + labs(
  title = paste('各银行交易耗时', start_time, '至', end_time),
  x = '银行',
  y = '比率',
  fill = '耗时区间',
  colour = '指标'
) + geom_area(
  data = bb,
  aes(
    x = bb$bank,
    y = rescale(
      bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing,
      c(0, 1),
      c(0, max(
        bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing
      ))
    ),
    group = 1
  ),
  alpha = 0.2,
  fill = 'red'
) + scale_y_continuous(breaks = c(seq(0, 1, 0.05)),
                       sec.axis = sec_axis(
                         ~ . * max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing),
                         breaks = seq(
                           0,
                           ceiling(
                             max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing) / 10 ^ (nchar(as.character(
                               max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing)
                             )) - 1)
                           ) * 10 ^ (nchar(as.character(
                             max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing)
                           )) - 1),
                           ceiling(
                             max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing) / 10 ^ (nchar(as.character(
                               max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing)
                             )) - 1)
                           ) * 10 ^ (nchar(as.character(
                             max(bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing)
                           )) - 1) / 10
                         )
                       ))

dev.off()

png(
  filename = paste(
    mainpath,"/pic/",
    gsub('-', '', substr(start_time, 1, 10)),
    "渠道交易金额区间图.png",
    sep = ''
  ),
  ###生成银行图
  width = 1580 / 1.5,
  height = 780 / 1.8
)
ggplot(data = result) + geom_bar(aes(
  x = result$group_amt,
  fill = reorder(result$channelName  , rep(-1, length(result$channelName)), sum)
), position = 'fill')  + theme(text = element_text(family = 'STXihei', size = 12)) + labs(
  title = paste('各交易区间各渠道占比', start_time, '至', end_time),
  x = '金额区间',
  y = '比率',
  fill = '渠道'
) + geom_area(
  data = group_amt_agr,
  aes(
    x = group_amt_agr$Group.1,
    y = rescale(group_amt_agr$x,
                c(0, 1),
                c(0, max(group_amt_agr$x))),
    group = 1
  ),
  alpha = 0.3,
  fill = 'white',
  size = 1.2
  
) + scale_y_continuous(sec.axis = sec_axis(~ . * max(group_amt_agr$x)))###+scale_fill_brewer(palette = 'RdBu')
dev.off()

png(
  filename = paste(
    mainpath,"/pic/",
    gsub('-', '', substr(start_time, 1, 10)),
    "各银行渠道分布图.png",
    sep = ''
  ),
  ###生成银行图
  width = 1580 / 1.5,
  height = 780 / 1.8
)
ggplot(data = result) + geom_bar(aes(
  x = reorder(result$bankName.y, rep(-1, length(result$bankName.y)), sum),
  fill = reorder(result$channelName, rep(-1, length(result$channelName)), sum)
), position = 'fill')  + theme(text = element_text(family = 'STXihei', size = 12)) + labs(
  title = paste('各银行渠道分布', start_time, '至', end_time),
  x = '银行',
  y = '比率',
  fill = '渠道'
) + geom_line(
  data = bb,
  aes(
    x = bb$bank,
    y = rescale(
      bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing,
      c(0, 1),
      c(0, max(
        bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing
      ))
    ),
    group = 1
  ),
  alpha = 0.3,
  fill = 'red',
  size = 1.2
) + scale_y_continuous(sec.axis = sec_axis(~ . * max(
  bb$succ + bb$fail_abnor + bb$fail_nor + bb$processing
)))
dev.off()



png(filename = "渠道图.png",
    ###生成渠道图
    width = 3080,
    height = 1480)
ggplot(data = result) + geom_bar(aes(
  x = reorder(result$channelName, rep(-1, length(result$channelName)), sum),
  fill = factor(result$group, levels = label[seq(16, 1)], ordered = T)
), position = 'fill') + scale_fill_manual(
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
) + theme(text = element_text(family = 'STXihei', size = 30)) + geom_line(
  data = cc_melt,
  aes(
    x = cc_melt$channel,
    y = cc_melt$value,
    colour = cc_melt$variable,
    group = cc_melt$variable
  ),
  size = 1.5
) + scale_colour_manual(values =  c(
  成功率   = '#1B6D1B',
  余额不足率   = 'blue',
  异常率   = 'red',
  处理中占比   = 'purple'
)) + labs(
  title = paste('【按渠道】', start_time, '至', end_time),
  x = '银行',
  y = '比率',
  fill = '耗时区间',
  colour = '指标'
) + geom_area(
  data = cc,
  aes(
    x = cc$channel,
    y = rescale(
      cc$succ + cc$fail_abnor + cc$fail_nor + cc$processing,
      c(0, 1),
      c(0, max(
        cc$succ + cc$fail_abnor + cc$fail_nor + cc$processing
      ))
    ),
    group = 1
  ),
  alpha = 0.2,
  fill = 'red'
) + scale_y_continuous(sec.axis = sec_axis(~ . * max(
  cc$succ + cc$fail_abnor + cc$fail_nor + cc$processing
)))

dev.off()
# cc$rate<-round(cc$x/sum(cc$x),4)


getstandard_code <- function(channel) {
  ####爬取现有标准码配置
  form <- c(
    'channelId' = channel,
    'columns[0][data]' = 'channelName',
    'columns[0][name]' = '',
    'columns[0][orderable]' = 'FALSE',
    'columns[0][search][regex]' = 'FALSE',
    'columns[0][search][value]' = '',
    'columns[0][searchable]' = 'TRUE',
    'columns[1][data]' = 'channelId',
    'columns[1][name]' = '',
    'columns[1][orderable]' = 'TRUE',
    'columns[1][search][regex]' = 'FALSE',
    'columns[1][search][value]' = '',
    'columns[1][searchable]' = 'TRUE',
    'columns[2][data]' = 'standardName',
    'columns[2][name]' = '',
    'columns[2][orderable]' = 'FALSE',
    'columns[2][search][regex]' = 'FALSE',
    'columns[2][search][value]' = '',
    'columns[2][searchable]' = 'TRUE',
    'columns[3][data]' = 'standardCode',
    'columns[3][name]' = '',
    'columns[3][orderable]' = 'FALSE',
    'columns[3][search][regex]' = 'FALSE',
    'columns[3][search][value]' = '',
    'columns[3][searchable]' = 'TRUE',
    'columns[4][data]' = 'returnCode',
    'columns[4][name]' = '',
    'columns[4][orderable]' = 'TRUE',
    'columns[4][search][regex]' = 'FALSE',
    'columns[4][search][value]' = '',
    'columns[4][searchable]' = 'TRUE',
    'columns[5][data]' = 'returnName',
    'columns[5][name]' = '',
    'columns[5][orderable]' = 'FALSE',
    'columns[5][search][regex]' = 'FALSE',
    'columns[5][search][value]' = '',
    'columns[5][searchable]' = 'TRUE',
    'columns[6][data]' = 'createDatetime',
    'columns[6][name]' = '',
    'columns[6][orderable]' = 'TRUE',
    'columns[6][search][regex]' = 'FALSE',
    'columns[6][search][value]' = '',
    'columns[6][searchable]' = 'TRUE',
    'columns[7][data]' = 'updateDatetime',
    'columns[7][name]' = '',
    'columns[7][orderable]' = 'TRUE',
    'columns[7][search][regex]' = 'FALSE',
    'columns[7][search][value]' = '',
    'columns[7][searchable]' = 'TRUE',
    'draw' = '2',
    'length' = '100',
    'order[0][column]' = '1',
    'order[0][dir]' = 'asc',
    'search[regex]' = 'FALSE',
    'search[value]' = '',
    'start' = '0'
  )
  
  
  res <- postForm(
    uri = 'http://172.18.32.14:8080/pcs-oms-new/payment/channelStandardConfig/listPage',
    style = 'POST',
    curl = handle,
    .params = form
  )
  return(res)
}
temp_standard_code <- data.frame()
for (i in names(table(result$channelId))) {
  temp_standard_code <-
    rbind(temp_standard_code, fromJSON(getstandard_code(i))$rows)
  
}
standard_no_reroute <-
  merge(
    result,
    temp_standard_code[, c(2, 4, 6)],
    by.x = c('channelId', 'returnCode'),
    by.y = c('channelId', 'returnCode'),
    all.x = T
  )
standard_no_reroute <-
  subset(
    standard_no_reroute,
    standard_no_reroute$status %in% c('失败(异常)', 'PROCESSING') &
      (
        standard_no_reroute$standardCode.y != 'REROUTE' |
          is.na(standard_no_reroute$standardCode.y)
      )
  )
standard_no_reroute <-
  subset(standard_no_reroute,
         standard_no_reroute$channelName != '微信JS支付')

####时间分布图
png(
  filename = paste(
    mainpath,"/pic/",
    gsub('-', '', substr(start_time, 1, 10)),
    "交易时间分布.png",
    sep = ''
  ),
  width = 600 * 1.35,
  height = 400 * 1.35
)
ggplot(data = result) + geom_histogram(
  aes(
    x = result$crea_time,
    fill = reorder(result$status, rep(1, length(result$status)), sum)
  ),
  color = 'black',
  bins = 2 * length(seq.POSIXt(
    from = as.POSIXlt(start_time, format = '%Y-%m-%d+%H:%M:%S'),
    to = as.POSIXlt(end_time, format = '%Y-%m-%d+%H:%M:%S'),
    by = 'hour'
  ))
) + scale_x_datetime(
  breaks = seq.POSIXt(
    from = as.POSIXlt(start_time, format = '%Y-%m-%d+%H:%M:%S'),
    to = as.POSIXlt(end_time, format = '%Y-%m-%d+%H:%M:%S'),
    by = 'hour'
  ),
  labels = seq(from = 0, to = 24, by = 1)
) + labs(
  title = paste('交易时间分布', start_time, '至', end_time,sep=''),
  x = '时',
  y = '数量',
  fill = '结果'
)
dev.off()

cacu_fee <- function(df_t) {
  cat(1)
  df <- df_t
  df$fee<-NA
  df<-rbind(df,NA)
  df[dim(df)[1],]$channelId<-'TAT'
  ###在这里写基础费率
  df[which(df$channelId == 'BAOFOO' |df$channelId == 'TAT'), ]$fee <- 1.1
  df[which(df$channelId == 'ALLINPAY' |df$channelId == 'TAT'), ]$fee <- 1.5
  df[which(df$channelId == 'ALLINPAY2'|df$channelId=='TAT'),]$fee <-
    df[which(df$channelId == 'ALLINPAY2'|df$channelId=='TAT'),]$transAmt * 0.002
  df[which(df$channelId == 'ALLINPAYQUICK' |df$channelId == 'TAT'), ]$fee <-
    df[which(df$channelId == 'ALLINPAYQUICK' |df$channelId == 'TAT'), ]$transAmt * 0.0021
  df[which(df$channelId == 'BAOFOOQUICK' |df$channelId == 'TAT'), ]$fee <-
    df[which(df$channelId == 'BAOFOOQUICK' |df$channelId == 'TAT'), ]$transAmt * 0.0023
  df[which(df$channelId == 'JDPAY' | df$channelId == 'TAT'), ]$fee <-
    df[which(df$channelId == 'JDPAY' |df$channelId == 'TAT'), ]$transAmt * 0.002
  df[which(df$channelId == 'CPCNQUICK' | df$channelId == 'TAT'), ]$fee <-df[which(df$channelId == 'CPCNQUICK' |df$channelId == 'TAT'), ]$transAmt * 0.002
  df[which(df$channelId == 'BILL99QUICK' |df$channelId == 'TAT'), ]$fee <-
    df[which(df$channelId == 'BILL99QUICK' |df$channelId == 'TAT'), ]$transAmt * 0.002
  df[which(df$channelId == 'WEIXIN' | df$channelId == 'TAT'), ]$fee <-df[which(df$channelId == 'WEIXIN' |df$channelId == 'TAT'), ]$transAmt * 0.002
  df[which(df$channelId == 'CPCN' | df$channelId == 'TAT'), ]$fee <-1.2
  df[which(df$channelId == 'YEEPAY' |df$channelId == 'TAT'), ]$fee <- 1.8
  df[which(df$channelId == 'BILL99' |df$channelId == 'TAT'), ]$fee <- 0.8
  cat(2)
  ###在这里写分支费率
  df[which(df$channelId == 'BAOFOO' &df$bankCode != '03080000' &df$transAmt > 1000 | df$channelId == 'TAT'), ]$fee <- 1.3
  df[which(df$channelId == 'BAOFOO' &df$bankCode == '03080000' &df$transAmt < 5000 | df$channelId == 'TAT'), ]$fee <- 1.3
  df[which(df$channelId == 'BAOFOO' &df$bankCode == '03080000' &df$transAmt >= 5000 | df$channelId == 'TAT'), ]$fee <- 1.7
  df[which(df$channelId == 'CPCN' &df$bankCode == '03080000' | df$channelId == 'TAT'), ]$fee <-df[which(df$channelId == 'CPCN' &
                                                                                                          df$bankCode == '03080000' |df$channelId == 'TAT'), ]$transAmt * 0.002
  df[which(df$channelId == 'BILL99' &df$bankCode == '01050000' &df$transAmt < 5000 | df$channelId == 'TAT'), ]$fee <- 1.4
  df[which(df$channelId == 'BILL99' &df$bankCode == '01050000' &df$transAmt >= 5000 | df$channelId == 'TAT'), ]$fee <- 1.8
  df[which(df$channelId == 'BILL99' &df$bankCode != '01050000' &df$transAmt >= 1000 & df$transAmt < 5000| df$channelId == 'TAT'), ]$fee <- 1.4
  df[which(df$channelId == 'BILL99' &df$bankCode != '01050000' &df$transAmt >= 5000 | df$channelId == 'TAT'), ]$fee <- 1.8
  df<-subset(df,df$channelId!='TAT')
  return(df)
}

result_for_6<-result
result_for_6<-subset(result_for_6,result_for_6$status!='处理中')
result_for_6[which(result_for_6$status=='失败(余额不足)'),]$status<-'80'
result_for_6[which(result_for_6$status=='失败(异常)'),]$status<-'80'
result_for_6[which(result_for_6$status=='成功'),]$status<-'90'


for_6_sum<-aggregate(result_for_6$transAmt,list(result_for_6$bankName.y,result_for_6$status),sum)
for_6_count<-aggregate(result_for_6$transAmt,list(result_for_6$bankName.y,result_for_6$status),length)
for_6_sum <- dcast(for_6_sum, Group.1  ~ Group.2, value.var = "x")
for_6_count <- dcast(for_6_count, Group.1  ~ Group.2, value.var = "x")
for_6_merge<- merge(for_6_count,for_6_sum,by=c('Group.1'),all=T)
for_6_merge$渠道<-'萨摩耶代收'
for_6_merge[is.na(for_6_merge$`80.x`), ]$`80.x` <- 0
for_6_merge[is.na(for_6_merge$`90.x`), ]$`90.x` <- 0
for_6_merge[is.na(for_6_merge$`80.y`), ]$`80.y` <- 0
for_6_merge[is.na(for_6_merge$`90.y`), ]$`90.y` <- 0
for_6_merge$总数<-for_6_merge$`80.x`+for_6_merge$`90.x`
for_6_merge$总金额<-for_6_merge$`80.y`+for_6_merge$`90.y`
for_6_merge$成功率<-for_6_merge$`90.x`/for_6_merge$总数
for_6_merge<-for_6_merge[,c(1,6,3,5,7,8,9)]
names(for_6_merge)<- c('银行', '渠道','成功数', '成功金额', '总数','总金额','成功率')




