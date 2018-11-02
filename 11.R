setwd("D:/Rworkplace")
rm(list = ls())
library(ggplot2)
library(RCurl)
library(plyr)
library(jsonlite)
library(reshape2)
library(scales)

#############
mainpath<-"D:/Rworkplace"##存储路径

result <- data.frame()


start_time<-'2018-11-01'
end_time<-'2018-11-01'

page <- 1
row <- 5000
bindurl <- 'http://172.18.32.14:8080/pcs-oms-new/card/listBindingData'


handle <-
  getCurlHandle(
    httpheader = list(
      Accept = 'application/json, text/javascript, *; q=0.01',
      'Accept-Ecoding' = 'gzip, deflate',
      'Accept-Language' = 'zh-CN,zh;q=0.9',
      Connection = 'keep-alive',
      # 'Content-Length' = '',
      'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
      Cookie = 'JSESSIONID=68C10CAF7DC52F047EAD451CF7616BB2; theme=theme_base; token=e94e3f77df07b79f8f205b0f0f19f27a; userId=s00580; userType=CBUSER; userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Referer = 'http://172.18.32.14:8080/ncc-oms/pbcapply/pbcApplyPage?token=a25b085949531e494c422dccc17638b6&userId=s00580&userType=CBUSER&userName=%E6%9D%8E%E9%95%BF%E5%85%B4',
      Host = '172.18.32.14:8080',
      'X-Requested-With' = 'XMLHttpRequest'
    )
  )
#############


getdata <- function(s_time,e_time,page, row, uri, handle) {
  form_bind <- c(
    'dateStart'= s_time,
    'dateEnd'= e_time,
    'thirdPartyCode'='',
      'custNO'='' ,
      'custName'='',
      'bankCardNo'='' ,
      'idNo'='' ,
      'mobileNo'='' ,
      'issuerCode'='' ,
      'validStatus'='' ,
      'page'= page,
    'rows'= row
  )
  res <- postForm(
    uri = uri,
    style = 'POST',
    curl = handle,
    .params = form_bind
  )
  return(res)
}
get_res<-getdata(start_time,end_time,page,row,bindurl,handle)
json_result <- fromJSON(get_res)
total <- json_result$total
tmp <- json_result$rows
result <- rbind(result, tmp)
loop_time <- total %/% row + ifelse(total %% row > 0, 1, 0)
cat('共', total, '条数据，分', loop_time, '次完成，目前为第1次')
if (loop_time > 1) {
  for (i in 1:(loop_time - 1)) {
    page <- page+1
    get_res <-
      getdata(start_time,
              end_time,
              page,
              row,
              bindurl,
              handle)
    json_result <- fromJSON(get_res)
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
result[result$validStatus=='02',]$validStatus='99'
result[result$thirdPartyCode=='00',]$thirdPartyCode="默认值"
result[result$thirdPartyCode=='01',]$thirdPartyCode="诺亚"
result[result$thirdPartyCode=='02',]$thirdPartyCode="快钱"
result[result$thirdPartyCode=='03',]$thirdPartyCode="中金"
result[result$thirdPartyCode=='04',]$thirdPartyCode="联动优势"
result[result$thirdPartyCode=='05',]$thirdPartyCode="宝付支付"
result[result$thirdPartyCode=='06',]$thirdPartyCode="易宝支付"
result[result$thirdPartyCode=='07',]$thirdPartyCode="京东支付"
result[result$thirdPartyCode=='08',]$thirdPartyCode="微信支付"
result[result$thirdPartyCode=='09',]$thirdPartyCode="北京银联"
result[result$thirdPartyCode=='10',]$thirdPartyCode="华付金科"
result[result$thirdPartyCode=='11',]$thirdPartyCode="银盛支付"
result[result$thirdPartyCode=='12',]$thirdPartyCode="合数科技"
result[result$thirdPartyCode=='13',]$thirdPartyCode="快钱代扣"
result[result$thirdPartyCode=='14',]$thirdPartyCode="吉信支付"
result[result$thirdPartyCode=='16',]$thirdPartyCode="通联支付"
result[result$thirdPartyCode=='17',]$thirdPartyCode="通联快捷支付"
result[result$thirdPartyCode=='18',]$thirdPartyCode="宝付快捷支付"
result[result$thirdPartyCode=='19',]$thirdPartyCode="包商快捷支付"
result[result$thirdPartyCode=='20',]$thirdPartyCode="中金协议支付"
result[result$thirdPartyCode=='21',]$thirdPartyCode="快钱协议支付"
result[result$thirdPartyCode=='98',]$thirdPartyCode="虚拟第三方机构"
result[result$thirdPartyCode=='99',]$thirdPartyCode="外部渠道绑卡"

result[result$bankCardType=='1',]$bankCardType="借记卡"
result[result$bankCardType=='2',]$bankCardType="信用卡"


bind_res<-aggregate(result$certId,list(result$bankName,result$bankCardType,result$thirdPartyCode,result$validStatus),length)
bind_res<-dcast(bind_res,Group.1+Group.2+Group.3~Group.4,value.var = 'x')
bind_res[is.na(bind_res$`00`), ]$`00` <- 0
bind_res[is.na(bind_res$`01`), ]$`01` <- 0
bind_res[is.na(bind_res$`99`), ]$`99` <- 0
bind_res$succ_rate<-bind_res$`01`/(bind_res$`00`+bind_res$`01`+bind_res$`99`)
bind_res$sms_rate<-bind_res$`00`/(bind_res$`00`+bind_res$`01`+bind_res$`99`)
bind_res$fail_rate<-bind_res$`99`/(bind_res$`00`+bind_res$`01`+bind_res$`99`)
bind_res_cre<-subset(bind_res,bind_res$Group.2=='信用卡')
bind_res_deb<-subset(bind_res,bind_res$Group.2=='借记卡')
bind_res_cre$Group.1<- reorder(bind_res_cre$Group.1  , -bind_res_cre$`01`, sum)
bind_res_deb$Group.1<- reorder(bind_res_deb$Group.1  , -bind_res_deb$`01`, sum)

bind_res_cre<-arrange(bind_res_cre,bind_res_cre$Group.1,-bind_res_cre$succ_rate)
bind_res_deb<-arrange(bind_res_deb,bind_res_deb$Group.1,-bind_res_deb$succ_rate)

bind_res_cre <-
  rbind(bind_res_cre,
        list(
          '总计',"","",
          sum(bind_res_cre$`00`),
          sum(bind_res_cre$`01`),
          sum(bind_res_cre$`99`),
          sum(bind_res_cre$`01`) / (
            sum(bind_res_cre$`00`) + sum(bind_res_cre$`01`) + sum(bind_res_cre$`99`)
          ),
          sum(bind_res_cre$`00`) / (
            sum(bind_res_cre$`00`) + sum(bind_res_cre$`01`) + sum(bind_res_cre$`99`)
          ), sum(bind_res_cre$`99`) / (
            sum(bind_res_cre$`00`) + sum(bind_res_cre$`01`) + sum(bind_res_cre$`99`)
          )
        ))
names(bind_res_cre)<-c('银行','卡类型','验卡渠道','要素验证成功（未绑定）','要素验证成功（已绑定）','验证失败','要素验证成功(已绑定)率','要素验证成功(未绑定)率','验证失败率')
bind_res_deb <-
  rbind(bind_res_deb,
        list(
          '总计',"","",
          sum(bind_res_deb$`00`),
          sum(bind_res_deb$`01`),
          sum(bind_res_deb$`99`),
          sum(bind_res_deb$`01`) / (
            sum(bind_res_deb$`00`) + sum(bind_res_deb$`01`) + sum(bind_res_deb$`99`)
          ),
          sum(bind_res_deb$`00`) / (
            sum(bind_res_deb$`00`) + sum(bind_res_deb$`01`) + sum(bind_res_deb$`99`)
          ), sum(bind_res_deb$`99`) / (
            sum(bind_res_deb$`00`) + sum(bind_res_deb$`01`) + sum(bind_res_deb$`99`)
          )
        ))
names(bind_res_deb)<-c('银行','卡类型','验卡渠道','要素验证成功（未绑定）','要素验证成功（已绑定）','验证失败','要素验证成功(已绑定)率','要素验证成功(未绑定)率','验证失败率')

write.table(
  bind_res_cre,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "绑卡-信用卡.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)
write.table(
  bind_res_deb,
  file = paste(
    mainpath,"/table/",
    gsub('-', '', substr(start_time, 1, 10)),
    "绑卡-借记卡.csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)





