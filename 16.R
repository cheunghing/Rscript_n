result_cap_status<-subset(result_for_16,result_for_16$capcode!='PCS')
result_for_16 <-
  subset(result_for_16, !result_for_16$channelId %in% c('PCS'))#####筛选不为PCS代扣的

res_ag <-
  aggregate(result_for_16$idNo,
            list(result_for_16$bankName_uni, result_for_16$channelId, result_for_16$status),
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
  aggregate(result_for_16$totalAmt,
            list(result_for_16$bankName_uni, result_for_16$channelId, result_for_16$status),
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
    "总体代收(订单维度).csv",
    sep = ''
  ),
  sep = ',',
  row.names = F
)


result_cap_status<-subset(result_cap_status,result_for_16$capcode!='PCS')
result_cap_status$cap_status<-NA
result_cap_status[which(result_cap_status$version!='reroute'),]$cap_status<-'资金方直接成功'
result_cap_status[which(result_cap_status$version=='reroute'&result_cap_status$status=='90'),]$cap_status<-'重路由至PCS成功'
result_cap_status[which(result_cap_status$version=='reroute'&result_cap_status$status=='80'),]$cap_status<-'重路由至PCS失败'
result_cap_status$bankName_uni<-reorder(result_cap_status$bankName_uni, rep(-1, length(result_cap_status$bankName_uni)), sum)
result_cap_status$cap_status<-factor(result_cap_status$cap_status,levels=c('重路由至PCS失败','重路由至PCS成功','资金方直接成功'))
png(
  filename = paste(
    mainpath,"/pic/",
    gsub('-', '', substr(start_time, 1, 10)),
    "各资金方重路由效果图.png",
    sep = ''
  ),
  ###生成银行图
  width = 1400 ,
  height = 700
)
ggplot(data = result_cap_status,aes(
  x = bankName_uni,
  fill = cap_status
)) + geom_bar(position = 'fill') + theme(text = element_text(family = 'STXihei', size = 15))+facet_grid(capcode~.)+labs(
  title = paste('各资金方重路由效果', start_time, '至', end_time),
  x = '银行',
  y = '占比',
  fill = '类型'
)+scale_fill_brewer(palette = 'Set2')
dev.off()

