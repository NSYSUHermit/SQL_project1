# 資料庫專案資料更新程式碼
rm(list = ls());gc()
library(data.table)
library(zoo)
library(lubridate)
library(RMySQL)
library(tidyverse)

# 設定要傳到資料庫檔案的檔案存放路徑
filePath <- "C:/Users/Su-Yen-Ting/Desktop/dbProject/"   

# 是否啟用回溯
refreshAllData <- T

# 設定資料起始日
dataStartDate <- ifelse(refreshAllData == T, 
                        as.numeric(gsub("-", "", Sys.Date()-years(3))), 
                        as.numeric(gsub("-", "", Sys.Date()-years(1))))
# dataStartDate <- 20140101

############################  db連線 ##################################################
dbHost <- "140.117.75.69"
dbUser <- "shiny"
dbPassword <- "shiny2019"

channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPassword)
dbSendQuery(channel, "set names big5;")

queryString <- paste0("select code, name, date, open, high, low, close, trade_value, trade_volume ",
                      "from stock_market.stock_price_data where date >= ", dataStartDate, " order by code, date;")
res <- dbSendQuery(channel, queryString)
stock_price_data <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

dbDisconnect(channel)

#######################  整理所需表格 #####################
stock_price_data <- stock_price_data %>%
  group_by(code) %>%
  arrange(code,date) %>%
  mutate(ret = (close/lag(close)-1),
         direction = ifelse(close <= open, "decreasing", "increasing"), 
         ma5 = rollapplyr(close, 5, FUN = "mean", fill = NA),
         ma20 = rollapplyr(close, 20, FUN = "mean", fill = NA),
         ma60 = rollapplyr(close, 60, FUN = "mean", fill = NA),
         recent_5days_max = rollapplyr(close, 5, FUN = "max", fill = NA),
         recent_20days_max = rollapplyr(close, 20, FUN = "max", fill = NA),
         recent_60days_max = rollapplyr(close, 60, FUN = "max", fill = NA),
         recent_5days_min = rollapplyr(close, 5, FUN = "min", fill = NA),
         recent_20days_min = rollapplyr(close, 20, FUN = "min", fill = NA),
         recent_60days_min = rollapplyr(close, 60, FUN = "min", fill = NA))

condition_table <- stock_price_data %>%
  group_by(code) %>%
  arrange(code,date) %>%
  mutate(price_more_ma5 = ifelse(close > ma5, 1, 0),
         price_more_ma20 = ifelse(close > ma20 , 1, 0),
         price_more_ma60 = ifelse(close > ma60 , 1, 0),
         price_less_ma5 = ifelse(close < ma5 , 1, 0),
         price_less_ma20 = ifelse(close < ma20 , 1, 0),
         price_less_ma60 = ifelse(close < ma60 , 1, 0),
         break_recent_5days_max = ifelse(close>lag(recent_5days_max,1), 1, 0),
         break_recent_20days_max = ifelse(close>lag(recent_20days_max,1), 1, 0),
         break_recent_60days_max = ifelse(close>lag(recent_60days_max,1), 1, 0),
         break_recent_5days_min = ifelse(close>lag(recent_5days_min,1), 1, 0),
         break_recent_20days_min = ifelse(close>lag(recent_20days_min,1), 1, 0),
         break_recent_60days_min = ifelse(close>lag(recent_60days_min,1), 1, 0),
         long_position = ifelse((close > ma5) & (ma5 > ma20) & (ma20 > ma60), 1, 0),
         short_position = ifelse((close< ma5) & (ma5 < ma20) & (ma20 < ma60), 1, 0)) %>%
  select(code, name, date, price_more_ma5:short_position) 
condition_table[is.na(condition_table)] <- 0
condition_table <- condition_table %>% gather(indicator,cond_match,price_more_ma5:short_position)

stock_price_data <- stock_price_data %>% select(code:ma60)

indicator <- unique(condition_table$indicator) %>% as.tibble()
unique(indicator$value)
indicator_name <- c("收盤價高於MA5","收盤價高於MA20","收盤價高於MA60","收盤價低於MA5",
                    "收盤價低於MA20","收盤價低於MA60","收盤價創5日新高","收盤價創20日新高",
                    "收盤價創60日新高","收盤價創5日新低","收盤價創20日新低","收盤價創60日新低","多頭排列","空頭排列") %>% as.tibble()
condition_direction <- c("bull","bull","bull","bear","bear","bear","bull","bull","bull","bear","bear","bear","bull","bear") %>% as.tibble()
indicator <- indicator %>% bind_cols(indicator_name,condition_direction)
colnames(indicator) <- c("indicator","indicator_name","condition_direction")


################################# 使用者登入資訊 #################################
channel <- dbConnect(dbDriver("MySQL"), dbname = "shiny", host = dbHost, user = dbUser, password = dbPassword)

if(refreshAllData == T){
  
  # 更新stock
  stock <- stock_price_data %>% distinct(code, name)
  write.table(stock, paste0(filePath, "stock.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE, quote = FALSE, fileEncoding ="utf8")
  dbSendQuery(channel, "truncate shiny.stock;")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "stock.txt' into table shiny.stock LINES TERMINATED BY '\r\n';"))
  
  # 更新stock_price_data
  write.table(stock_price_data, paste0(filePath, "stock_price_data.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE, quote = FALSE, fileEncoding ="utf8")
  dbSendQuery(channel, "truncate shiny.stock_price_data;")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "stock_price_data.txt' into table shiny.stock_price_data LINES TERMINATED BY '\r\n';"))
  
  # 更新condition_table
  write.table(condition_table, paste0(filePath, "condition_table.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE, quote = FALSE, fileEncoding ="utf8")
  dbSendQuery(channel, "truncate shiny.condition_table;")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "condition_table.txt' into table shiny.condition_table LINES TERMINATED BY '\r\n';"))
  # 更新indicator
  write.table(indicator, paste0(filePath, "indicator.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE, quote = FALSE, fileEncoding ="utf8")
  dbSendQuery(channel, "truncate shiny.indicator;")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "indicator.txt' into table shiny.indicator LINES TERMINATED BY '\r\n';"))
  # 更新member
  member <- t(c("admin", "admin", "T", "管理猿")) %>% as.tibble()
  colnames(member) <- c("user","password","admin","nick")
  write.table(member, paste0(filePath, "member.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE, fileEncoding ="utf8")
  dbSendQuery(channel, "truncate shiny.member;")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "member.txt' into table shiny.member LINES TERMINATED BY '\r\n';"))
  
  # 清空資料表
  dbSendQuery(channel, "truncate shiny.browser;")
  dbSendQuery(channel, "truncate shiny.record;")
  
}else{
  
  # 更新stock
  stock <- stock_price_data %>% distinct(code, name)
  write.table(stock, paste0(filePath, "stock.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE, quote = FALSE, fileEncoding ="utf8")
  dbSendQuery(channel, "truncate shiny.stock;")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "stock.txt' into table shiny.stock LINES TERMINATED BY '\r\n';"))
  
  # 查看stock_price_data 的最後更新日期
  queryString <- paste0("select max(date) from shiny.stock_price_data ;")
  res <- dbSendQuery(channel, queryString)
  date_list_max <- fetch(res, n = -1) %>% pull()

  # 更新stock_price_data
  stock_price_data <- stock_price_data %>% filter(date > date_list_max)
  write.table(stock_price_data, paste0(filePath, "stock_price_data.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE,fileEncoding ="utf8")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "stock_price_data.txt' into table shiny.stock_price_data LINES TERMINATED BY '\r\n';"))
  
  # 查看condition_table 的最後更新日期
  queryString <- paste0("select max(date) from shiny.condition_table ;")
  res <- dbSendQuery(channel, queryString)
  date_list_max <- fetch(res, n = -1) %>% pull()
  
  # 更新condition_table
  condition_table <- condition_table %>% filter(date > date_list_max)
  write.table(condition_table, paste0(filePath, "condition_table.txt"), 
              sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE,fileEncoding ="utf8")
  dbSendQuery(channel, paste0("load data local infile '", filePath, 
                              "condition_table.txt' into table shiny.condition_table LINES TERMINATED BY '\r\n';"))
}

dbDisconnect(channel)

