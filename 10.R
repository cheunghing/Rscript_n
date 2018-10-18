result<-data.frame()
for (i in dir(path = "D:/Rworkplace/hist_data_all")[37]) {
  cat(i,'\n')
  result <- rbind(result,read.csv(paste("D:/Rworkplace/hist_data_all/",i,sep = '')
                                  ,stringsAsFactors=FALSE))
}
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
result$group_amt <-
  cut(result$transAmt, split_amt, label_amt)##划分amt区间

result<-cacu_fee(result)
result<-subset(result,result$status=='成功')
result_1_1<-aggregate(result$transAmt,list(result$group_amt,result$bankCode),sum)
result_1_2<-aggregate(result$transAmt,list(result$group_amt,result$bankCode),length)
result_1_3<-aggregate(result$fee,list(result$group_amt,result$bankCode),sum)
merge_1<-merge(result_1_1,result_1_2,by=c('Group.1','Group.2'),all=T)
merge_1<-merge(merge_1,result_1_3,by=c('Group.1','Group.2'),all=T)
merge_1$笔均成本<-merge_1$x/merge_1$x.y
merge_1$千元成本<-merge_1$x.y/merge_1$x.x*1000
names(merge_1)<-c('金额区间','银行','金额','笔数','费用','笔均成本','千元成本')
##merge_2<-merge_1 改改改！！
merge_res<-merge(merge_2,merge_1,by=c('金额区间','银行'),all=T)
merge_res <-  ##银行名称合并之关联银行唯一列表
  merge(
    merge_res,
    unique_bank_list,
    by.x = c('银行'),
    by.y = c('bankCode'),
    all.x = T
  )
merge_res<-merge_res[,c(13,2:12)]
merge_res$笔均成本gap<-round(merge_res$笔均成本.y-merge_res$笔均成本.x,2)
merge_res$千元成本gap<-round(merge_res$千元成本.y-merge_res$千元成本.x,2)
