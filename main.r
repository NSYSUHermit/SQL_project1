rm(list=ls());gc()
library(shiny)
library(shinymanager)
library(shinythemes)
library(shinyjs)
library(RMySQL)
library(lubridate)
library(plotly)
library(DT)
library(stringr)
library(tidyverse)


################################# 前台畫面 #################################
ui <- navbarPage("技術分析簡易回測系統", id = "tabs",
                 
  # 主題設定
  theme = shinytheme("flatly"),

  
  ################################# 回測選股分頁內容 #################################
  tabPanel("回測選股", value = "回測選股",
    
    titlePanel("選股條件設定"),
    
    # 側邊+主面板版型
    sidebarLayout(
      
      # 側邊面板設定
      sidebarPanel(
        
        # 輸入股票代碼
        helpText("請輸入要回測的股票:"),
        textInput("stockCode", label = "股票代碼", placeholder = "請輸入股票代碼"),
        
        # 勾選進場條件
        helpText("請選擇要回測的進場條件(可複選，皆符合所選條件即進場):"),
        selectizeInput("inCondition", 
                       label = "進場條件", 
                       choices = NULL,        # 此處選擇條件由後端給
                       multiple = TRUE),
        
        # 勾選出場條件
        helpText("請選擇要回測的出場條件(可複選，皆符合所選條件即出場):"),
        selectizeInput("outCondition", 
                       label = "出場條件", 
                       choices = NULL,        # 此處選擇條件由後端給
                       multiple = TRUE),
        
        # 選擇回測期間
        helpText("請選擇要回測的期間(限近2年):"),
        dateRangeInput("backtestDateRange",
                       label = "回測期間範圍",
                       start = Sys.Date() - years(2),
                       end = Sys.Date(),
                       min = Sys.Date() - years(2),
                       max = Sys.Date()),
        hr(),
        
        # 送出按鈕
        actionButton("action", label = "啟動回測")
      ),
      
      # 主畫面設定
      mainPanel(
        
        # 錯誤訊息提示
        span(textOutput(outputId = "errorMessage"), style = "color:red; font-size:20px"),
        
        # 顯示績效表
        dataTableOutput("performanceTable"),
        
        hr(),
        
        # 顯示繪製圖形
        plotlyOutput(outputId = "techFigure", width = "100%", height = "600px")
        
      )
    )
  ),
  
  
  ################################# 我的回測紀錄分頁內容 #################################
  tabPanel("我的回測紀錄", value = "我的回測紀錄",
           
           span(textOutput("userName", inline = T), "您好! 下表為您使用本服務的歷史回測記錄。"),
           tags$head(tags$style("#userName{font-size: 20px;}")),
           hr(),
           dataTableOutput("myBacktestRecord")
           
  ),
  
  
  ################################# 回測績效排行榜分頁內容 #################################
  tabPanel("回測績效排行榜", value = "回測績效排行榜",
           
           p("此處紀錄各使用者回測績效最好前20名的投資標的、回測期間與進出場條件。"),
           hr(),
           dataTableOutput("bestRecord")
  ),
  
  
  ################################# 後台管理分頁內容 #################################
  tabPanel("後台管理", value = "後台管理",
           
           tabsetPanel(id = "manageTabs",
             tabPanel(title = "註冊新使用者",
                      
                      titlePanel("註冊新使用者"),
                      column(5,
                             wellPanel(
                               textInput("registerUser", "請輸入使用者帳號", placeholder = "請輸入使用者帳號"),
                               passwordInput("registerPw", "請輸入使用者密碼", placeholder = "請輸入使用者密碼"),
                               passwordInput("registerPwCheck", "請再次輸入使用者密碼", placeholder = "請再次輸入使用者密碼"),
                               textInput("registerName", "請輸入使用者名稱", placeholder = "請輸入使用者名稱"),
                               radioButtons("registerAdmin",
                                            label = "請問是否要給予後台管理權限?",
                                            choices = list("是" = "T", 
                                                           "否" = "F"),
                                            selected = c("F")),
                               actionButton("registerAction", label = "確認送出"),
                               hr(),
                               htmlOutput("registerErrorInfo", style = "color:red; font-size:18px"),
                               htmlOutput("registerSuccessInfo", style = "color:blue; font-size:18px")
                             )
                      )
             ),
             
             tabPanel(title = "修改使用者資訊",
             
                      titlePanel("修改使用者資訊"),
                      column(5,
                             wellPanel(
                               selectizeInput("reviseUser", "請選擇要修改資訊的使用者", choices = NULL, multiple = FALSE),
                               passwordInput("revisePw", "請輸入新密碼", placeholder = "請輸入新密碼"),
                               passwordInput("revisePwCheck", "請再次輸入新密碼", 
                                             placeholder = "請再次輸入新密碼"),
                               textInput("reviseName", "請輸入新使用者名稱", placeholder = "請輸入新使用者名稱"),
                               radioButtons("reviseAdmin",
                                            label = "請問是否要給予後台管理權限?",
                                            choices = list("是" = "T", 
                                                           "否" = "F")),
                               actionButton("reviseAction", label = "確認送出"),
                               hr(),
                               htmlOutput("reviseErrorInfo", style = "color:red; font-size:18px"),
                               htmlOutput("reviseSuccessInfo", style = "color:blue; font-size:18px")
                             )
                      )
             ),
             
             tabPanel(title = "刪除使用者",
                      
                      titlePanel("刪除使用者"),
                      column(5,
                             wellPanel(
                               selectizeInput("delUserUser", "請選擇要刪除的使用者(可多選)", choices = NULL, multiple = TRUE),
                               actionButton("delUserAction", label = "確認送出"),
                               htmlOutput("delUserSuccessInfo", style = "color:blue; font-size:18px")
                             )
                      )
             )
             
             
           )
  ),
  
  
  ################################# 字體設定:使用CSS語法 #################################
  tags$head(
    tags$style(HTML("
                @import url('//fonts.googleapis.com/css?family=Noto+Sans+TC');
                
                body, h2 {
                    font-family: 'Noto Sans TC', sans-serif;
                }")))
)


################################# 登入畫面 #################################
# Wrap your UI with secure_app
ui <- secure_app(ui, enable_admin = T, theme = shinytheme("flatly"))


################################# 後台系統 #################################
server <- function(input, output, session) {
  
  
  ################################# 資料庫設定 #################################
  dbHost <- "140.117.75.69"
  dbUser <- "shiny"
  dbPasword <- "shiny2019"
  
  
  ################################# 讀取登入資訊 #################################
  # 連入資料庫
  channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
  dbSendQuery(channel, "set names big5;")
  
  # 自資料庫取得登入名單
  query <- "select * from shiny.member;"
  res <- dbSendQuery(channel, query)
  credentials <- fetch(res, n = -1) %>% as_tibble()
  
  # 自資料庫取得條件資訊
  query <- "select * from shiny.indicator;"
  res <- dbSendQuery(channel, query)
  indicatorTable <- fetch(res, n = -1) %>% as_tibble()
  
  dbDisconnect(channel)
  
  # 傳送回測選單進出場條件選項
  inCondition <- indicatorTable %>% 
    filter(cond_direction == 'bull')
  inConditionList <- lapply(seq_len(nrow(inCondition)), function(i) inCondition$indicator[i])
  names(inConditionList) <- inCondition$indicator_name
  
  outCondition <- indicatorTable %>% 
    filter(cond_direction == 'bear')
  outConditionList <- lapply(seq_len(nrow(outCondition)), function(i) outCondition$indicator[i])
  names(outConditionList) <- outCondition$indicator_name
  
  updateSelectInput(session, "inCondition", choices = inConditionList)
  updateSelectInput(session, "outCondition", choices = outConditionList)

  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # 目前使用者資訊
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # 目前使用者名稱
  output$userName <- renderText(credentials$nick[which(credentials$user == isolate(res_auth$user))])
  
  # 控制後台管理頁面給有管理權限者
  userAdmin <- reactive(credentials$admin[which(credentials$user == res_auth$user)])
  observe({
    if(length(userAdmin()) > 0){
      if(userAdmin() == "F"){
        hideTab(inputId = "tabs", target = "後台管理")
      }
    }
  })
  

  ################################# 新增使用者 #################################
  observeEvent(input$registerAction, {
    
    # 清空訊息
    registerErrorInfo <- registerSuccessInfo <- NULL
    output$registerErrorInfo <- renderText({registerErrorInfo})
    output$registerSuccessInfo <- renderText({registerSuccessInfo})
    
    # 驗證申請帳號是否符合規則
    if(nchar(input$registerUser) == 0){
      registerErrorInfo <- paste0(registerErrorInfo, "> 帳號填寫有誤，請確認!<br/>")
    }else if(length(which(credentials$user == input$registerUser)) >= 1){
      registerErrorInfo <- paste0(registerErrorInfo, "> 此帳號已被申請，請重新輸入!<br/>")
    }

    # 驗證申請密碼是否符合規則
    if(nchar(input$registerPw) == 0){
      registerErrorInfo <- paste0(registerErrorInfo, "> 密碼填寫有誤，請確認!<br/>")
    }else if(input$registerPw != input$registerPwCheck){
      registerErrorInfo <- paste0(registerErrorInfo, "> 密碼與再次確認密碼不符合，請重新輸入!<br/>")
    }
    
    # 驗證使用者名稱是否符合規則
    if(nchar(input$registerName) == 0){
      registerErrorInfo <- paste0(registerErrorInfo, "> 使用者名稱填寫有誤，請確認!<br/>")
    }
    
    # 通過規則設定 
    if(is.null(registerErrorInfo)){
      
      # 執行匯入帳號資訊
      channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
      dbSendQuery(channel, "set names utf8;")
      query <- paste0("INSERT INTO shiny.member ( user, password, admin, nick) VALUES ",
                      "( '", input$registerUser, "', '", input$registerPw, "', '", 
                      input$registerAdmin, "', '", input$registerName, "' );")
      dbSendQuery(channel, query)
      dbDisconnect(channel)
      
      # 返回成功語句
      registerSuccessInfo <- paste0("> ", input$registerUser, " 註冊申請通過!<br/>")
      
      # 紀錄至帳號表
      credentials <<- credentials %>%
        bind_rows(tibble = c(user = input$registerUser,
                             password = input$registerPw,
                             admin = input$registerAdmin,
                             nick = input$registerName))
      
      # 重設使用者選單與填寫值
      updateSelectInput(session, "reviseUser", choices = credentials$user)
      userUserList <- credentials$user[which(!(credentials$user %in% 
                                                 c("admin", credentials$user[which(credentials$user == isolate(res_auth$user))])))]
      updateSelectInput(session, "delUserUser", choices = userUserList)
      updateTextInput(session, "registerUser", value = "")
      updateTextInput(session, "registerPw", value = "")
      updateTextInput(session, "registerPwCheck", value = "")
      updateTextInput(session, "registerName", value = "")
    }
      
    # 返回訊息
    output$registerErrorInfo <- renderText({registerErrorInfo})
    output$registerSuccessInfo <- renderText({registerSuccessInfo})
  })
  
  
  ################################# 修改使用者資訊 #################################
  # 產生管理後台頁面修改使用者默認值
  updateSelectInput(session, "reviseUser", choices = credentials$user)
  observeEvent(input$reviseUser, {
    updateTextInput(session, "reviseName", value = credentials$nick[which(credentials$user == input$reviseUser)])
    updateRadioButtons(session, "reviseAdmin", selected = credentials$admin[which(credentials$user == input$reviseUser)])
  })
  
  observeEvent(input$reviseAction, {
    
    # 清空訊息
    reviseErrorInfo <- reviseSuccessInfo <- NULL
    output$reviseErrorInfo <- renderText({reviseErrorInfo})
    output$reviseSuccessInfo <- renderText({reviseSuccessInfo})
    
    # 調閱此使用者原本的設定
    matchSite <- which(credentials$user == input$reviseUser)
    originPw <- credentials$password[matchSite]
    originName <- credentials$nick[matchSite]
    originAdmin <- credentials$admin[matchSite]
    
    # 驗證修改密碼是否符合規則 若符合則直接修改密碼
    if((originPw != input$revisePw) & (nchar(input$revisePw) != 0)){
      if(input$revisePw != input$revisePwCheck){
        reviseErrorInfo <- paste0(reviseErrorInfo, "> 密碼與再次確認密碼不符合，請重新輸入!<br/>")
      }else{
        
        # 修改使用者密碼
        channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
        dbSendQuery(channel, "set names utf8;")
        query <- "set sql_safe_updates = 0;"
        dbSendQuery(channel, query)
        query <- paste0("update shiny.member set password = '", input$revisePw,"' where user = '", input$reviseUser,"';")
        dbSendQuery(channel, query)
        dbDisconnect(channel)
        
        # 修改帳號表
        credentials$password[matchSite] <<- input$revisePw
        
        # 返回成功語句
        reviseSuccessInfo <- paste0("> ", input$reviseUser, " 密碼已修改!<br/>")
      }
    }
    
    
    # 驗證修改使用者名稱是否符合規則
    if(originName != input$reviseName){
      if(nchar(input$reviseName) == 0){
        reviseErrorInfo <- paste0(reviseErrorInfo, "> 使用者名稱填寫有誤，請確認!<br/>")
      }else{
        
        # 修改使用者名稱
        channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
        dbSendQuery(channel, "set names utf8;")
        query <- "set sql_safe_updates = 0;"
        dbSendQuery(channel, query)
        query <- paste0("update shiny.member set nick = '", input$reviseName,"' where user = '",input$reviseUser,"';")
        dbSendQuery(channel, query)
        dbDisconnect(channel)
        
        # 修改帳號表
        credentials$nick[matchSite] <<- input$reviseName
        
        # 返回成功語句
        reviseSuccessInfo <- paste0("> ", input$reviseUser, " 使用者名稱已修改!<br/>")
      }
    }
    
    
    # 修改使用者權限
    if(originAdmin != input$reviseAdmin){
      channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
      dbSendQuery(channel, "set names utf8;")
      query <- "set sql_safe_updates = 0;"
      dbSendQuery(channel, query)
      query <- paste0("update shiny.member set admin = '", input$reviseAdmin,"' where user = '", input$reviseUser,"';")
      dbSendQuery(channel, query)
      dbDisconnect(channel)
      
      # 修改帳號表
      credentials$admin[matchSite] <<- input$reviseAdmin
      
      # 返回成功語句
      reviseSuccessInfo <- paste0("> ", input$reviseUser, " 使用者權限已修改!<br/>")
    }

    # 重設使用者資訊
    updateTextInput(session, "revisePw", value = "")
    updateTextInput(session, "revisePwCheck", value = "")
    updateTextInput(session, "reviseName", value = credentials$nick[which(credentials$user == input$reviseUser)])
    
    # 返回訊息
    output$reviseErrorInfo <- renderText({reviseErrorInfo})
    output$reviseSuccessInfo <- renderText({reviseSuccessInfo})
  })
  
  
  ################################# 刪除使用者 #################################
  # 產生管理後台頁面刪除使用者選項
  # 20191106 此處有Bug無法解決: 1. 無法排除當前使用者帳號 意即可以刪自己帳號
  #                             2. 新增或刪除使用者後 表單無法立即更新
  userUserList <- credentials$user[which(!(credentials$user %in% 
                                             c("admin", credentials$user[which(credentials$user == isolate(res_auth$user))])))]
  updateSelectInput(session, "delUserUser", choices = userUserList)
  
  observeEvent(input$delUserAction, {

    # 自資料庫刪除使用者
    channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
    dbSendQuery(channel, "set names utf8;")
    query <- "set sql_safe_updates = 0;"
    dbSendQuery(channel, query)
    query <- paste0("delete from shiny.member where user in (", paste0("'", input$delUserUser, "'", collapse = ","), ");")
    dbSendQuery(channel, query)
    dbDisconnect(channel)
    
    # 回傳資訊
    output$delUserSuccessInfo <- renderText({
      paste0("已刪除使用者: <br/>", paste0(isolate(input$delUserUser), collapse = "<br/>"))
    })
    
    # 重設使用者選單與填寫值
    credentials <<- credentials %>% filter(!(user %in% input$delUserUser))
    updateSelectInput(session, "reviseUser", choices = credentials$user)
    userUserList <- credentials$user[which(!(credentials$user %in% 
                                               c("admin", credentials$user[which(credentials$user == isolate(res_auth$user))])))]
    updateSelectInput(session, "delUserUser", selected = NULL, choices = userUserList)
  })
  
  
  ################################# 當使用者按下啟動回測按鈕時行為 #################################
  observeEvent(input$action, {
    
    withProgress(message = "回測程式運作中", value = 0, {
      
      # 進度列
      incProgress(10/100, detail = "下載股票資料...")
      
      ################################# 下載股票資料 #################################
      # 使用者選定之日期區間
      backtestDateRange <- as.numeric(gsub("-", "", input$backtestDateRange))
      
      # 連線至資料庫取資料
      channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
      dbSendQuery(channel, "set names big5;")
      
      # 查詢使用者選定之股票資料
      query <- paste0("select code, name, date, open, high, low, close, trade_volume, direction, ma5, ma20, ma60 ",
                      "from shiny.stock_price_data where code = ", input$stockCode," and date >= ", backtestDateRange[1], 
                      " and date <= ", backtestDateRange[2], " order by date;")
      res <- dbSendQuery(channel, query)
      stockData <- fetch(res, n = -1) %>% 
        as_tibble() %>% 
        mutate(date = ymd(date)) %>%  # 更改日期格式 方便繪圖
        na.omit()
      
      # 關閉資料庫
      dbDisconnect(channel)
      
      # 判斷該檔股票是否存在 設計防呆機制
      if(nrow(stockData) == 0){
        
        # 若該檔股票無資料 代表是使用者輸入錯誤 顯示錯誤提示訊息
        output$techFigure <- renderPlotly({})
        output$performanceTable <- renderDataTable({})
        output$errorMessage <- renderText({
          stockCode <- isolate(input$stockCode)
          paste0("查無股票代號: ", stockCode,"，請確認股票代碼後再查詢!")
        })
        
      }else{
        
        # 清除錯誤提示訊息
        output$errorMessage <- renderText({})
        
        ################################# 計算回測績效 #################################
        # 連線至資料庫取資料
        channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
        dbSendQuery(channel, "set names big5;")
        
        # 進度列
        incProgress(20/100, detail = "正在整理符合進場條件日期...")
        
        # 查詢使用者選定股票符合進場條件之日期和收盤價
        query <- paste0("select date, close as in_price from shiny.stock_price_data ",
                        "where code = ", input$stockCode," and date in (",
                        " select date ", 
                        " from( ",
                        "   select date, sum(cond_match) as match_nums",
                        "   from shiny.condition_table", 
                        "   where code = ", input$stockCode," and cond_match = 1",
                        "   and date >= ", backtestDateRange[1], " and date <= ", backtestDateRange[2], 
                        "   and indicator in (", paste0("'", input$inCondition, "'", collapse = ","),")",
                        "   group by date) as a",
                        " where match_nums = ", length(input$inCondition),
                        " ) order by date;")
        res <- dbSendQuery(channel, query)
        inCondition <- fetch(res, n = -1) %>% as_tibble()
        
        # 進度列
        incProgress(30/100, detail = "正在整理符合出場條件日期...")
        
        # 查詢使用者選定股票符合出場條件之日期和收盤價
        query <- paste0("select date, close as out_price from shiny.stock_price_data ",
                        "where code = ", input$stockCode," and date in (",
                        " select date ",
                        " from( ",
                        "   select date, sum(cond_match) as match_nums",
                        "   from shiny.condition_table", 
                        "   where code = ", input$stockCode," and cond_match = 1",
                        "   and date >= ", backtestDateRange[1], " and date <= ", backtestDateRange[2], 
                        "   and indicator in (", paste0("'", input$outCondition, "'", collapse = ","),")",
                        "   group by date) as a",
                        " where match_nums = ", length(input$outCondition),
                        " ) order by date;")
        res <- dbSendQuery(channel, query)
        outCondition <- fetch(res, n = -1) %>% as_tibble()
        
        # 關閉資料庫
        dbDisconnect(channel)
        
        # 進度列
        incProgress(50/100, detail = "正在計算回測績效...")
        
        # 迴圈計算進場及出場績效
        tradeRetTable <- tibble(inDate = numeric(), outDate = numeric(), ret = numeric(), holdDays = numeric())
        nextInSite <- 1
        while(1){
          
          # 進場日期及價格
          inDate <- inCondition$date[nextInSite]
          inPrice <- inCondition$in_price[nextInSite]
          
          # 出場日期及價格
          outSite <- which(outCondition$date > inDate)[1]
          if(is.na(outSite)) break
          outDate <- outCondition$date[outSite]
          outPrice <- outCondition$out_price[outSite]
          
          # 紀錄報酬率及持有天數
          tradeRetTable <- tradeRetTable %>%
            bind_rows(tibble(inDate = inDate,
                             outDate = outDate,
                             ret = outPrice/inPrice-1,
                             holdDays = as.numeric(ymd(outDate) - ymd(inDate))))
          
          # 尋找下個位置
          nextInSite <- which(inCondition$date > outDate)[1]
          if(is.na(nextInSite)) break
        }
        
        # 計算績效
        performanceTable <- tibble(`交易總次數` = paste0(nrow(tradeRetTable), " 次"),
                                   `平均報酬率` = paste0(round(mean(tradeRetTable$ret)*100, 2), "%"),
                                   `報酬率標準差` = paste0(round(sd(tradeRetTable$ret)*100, 2), "%"),
                                   `平均持有天數` = paste0(round(mean(tradeRetTable$holdDays), 2), " 天"))
        output$performanceTable <- renderDataTable(datatable(performanceTable))
        
        # 紀錄使用者本次回測資訊
        # 進場條件對應名稱
        inConditionName <- indicatorTable$indicator_name[which(indicatorTable$indicator %in% input$inCondition)] 
        # 進場條件對應名稱
        outConditionName <- indicatorTable$indicator_name[which(indicatorTable$indicator %in% input$outCondition)]  
        
        channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
        dbSendQuery(channel, "set names big5;")
        query <- paste0("INSERT INTO shiny.record (user, exec_date, code, name, start_date, end_date, ",
                        "in_condition, out_condition, trade_nums, mean_ret, sd_ret, mean_hold_days) ",
                        "VALUES ('", isolate(res_auth$user),"', '", Sys.time(),"', '", last(unique(stockData$code)),"', '",
                        last(unique(stockData$name)),"', '", input$backtestDateRange[1],"', '",
                        input$backtestDateRange[2],"', '", paste0(inConditionName, collapse = ","),
                        "', '", paste0(outConditionName, collapse = ","),"', '",
                        paste0(nrow(tradeRetTable), " 次"),"', '", paste0(round(mean(tradeRetTable$ret)*100, 2), "%"),
                        "', '", paste0(round(sd(tradeRetTable$ret)*100, 2), "%"),"', '", 
                        paste0(round(mean(tradeRetTable$holdDays), 2), " 天"),"');")
        dbSendQuery(channel, query)
        dbDisconnect(channel)
        
        ################################# 繪製股票技術分析圖形 #################################
        # 進度列
        incProgress(80/100, detail = "正在繪製進出場圖片...")
        
        output$techFigure <- renderPlotly({
          
          # 併入進場及出場位置
          stockData <- stockData %>%
            left_join(tradeRetTable %>% 
                        transmute(date = ymd(inDate),
                                  in_site = 1), 
                      by = c("date" = "date")) %>%
            left_join(tradeRetTable %>% 
                        transmute(date = ymd(outDate),
                                  out_site = 1), 
                      by = c("date" = "date")) %>%
            mutate(in_site = in_site * low * (1-0.015),
                   out_site = out_site * high * (1+0.015))
          
          # 股票代碼及名稱
          plotCode <- last(unique(stockData$code))
          plotCodeName <- last(unique(stockData$name))
          
          # 定義上漲下跌顏色
          increaseColor <- list(line = list(color = "#ff0000"))   # 定義上漲顏色
          decreaseColor <- list(line = list(color = "#008000"))   # 定義下跌顏色
          
          # 繪製技術分析圖形
          p1 <- plot_ly(data = stockData, x = ~ date, type = "candlestick",
                        open = ~ open, close = ~ close,
                        high = ~ high, low = ~ low,
                        increasing = increaseColor, decreasing = decreaseColor, name = "K棒") %>%
            add_lines(x = ~ date, y = ~ ma5, name = "5日均線",
                      line = list(color = "#0000ff", width = 1.2),
                      hoverinfo = "none", inherit = F) %>%
            add_lines(x = ~ date, y = ~ ma20, name = "20日均線",
                      line = list(color = "#ffa500", width = 1.2),
                      hoverinfo = "none", inherit = F) %>%
            add_lines(x = ~ date, y = ~ ma60, name = "60日均線",
                      line = list(color = "#00ff00", width = 1.2),
                      hoverinfo = "none", inherit = F) %>%
            add_markers(x = ~ date, y = ~ in_site, name = "進場點", 
                        marker = list(color = "red", size = 10, line = list(color = "black", width = 2))) %>%
            add_markers(x = ~ date, y = ~ out_site, name = "出場點", 
                        marker = list(color = "green", size = 10, line = list(color = "black", width = 2))) %>%
            layout(yaxis = list(title = "價格"),
                   xaxis = list(title = "日期", rangeslider = list(visible = F), showlegend = T)) 
          
          # 繪製成交量圖形
          p2 <- plot_ly(data = stockData, x = ~ date, y = ~ trade_volume, type = "bar",
                        color = ~ direction, colors = c("#008000", "#ff0000")) %>%
            layout(yaxis = list(title = "成交量"),
                   xaxis = list(title = "日期"),
                   showlegend = F)
          
          # 設定區間按鈕
          rs <- list(visible = TRUE, x = 0.5, y = -0.15,
                     xanchor = "center", yref = "paper",
                     font = list(size = 12),
                     buttons = list(
                       list(count = 1,
                            label = "全部期間",
                            step = "all"),
                       list(count = 1,
                            label = "近1年",
                            step = "year",
                            stepmode = "backward"),
                       list(count = 3,
                            label = "近3個月",
                            step = "month",
                            stepmode = "backward"),
                       list(count = 1,
                            label = "近1個月",
                            step = "month",
                            stepmode = "backward")))
          
          # 繪製主圖形
          p <- subplot(p1, p2, heights = c(0.8, 0.2), nrows = 2,
                       shareX = TRUE, titleY = TRUE) %>%
            layout(title = list(text = paste0(plotCode, " ", plotCodeName, " 技術分析圖形"),
                                font = list(size = 16)),
                   xaxis = list(title = "日期", rangeselector = rs),
                   font = list(family = "Microsoft JhengHei"),
                   legend = list(orientation = 'h', x = 0.5, y = 1,
                                 xanchor = 'center', yref = 'paper',
                                 font = list(size = 10),
                                 bgcolor = 'transparent'),
                   showlegend = TRUE)
          
          return(p)
        })
      }
    })
  })
  
  
  ################################# 我的回測紀錄 #################################
  observeEvent(input$tabs, {
    output$myBacktestRecord <- renderDataTable({
      
      channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
      dbSendQuery(channel, "set names big5;")
      query <- paste0("select exec_date, code, name, start_date, end_date, in_condition, out_condition, trade_nums, ",
                      "mean_ret, sd_ret, mean_hold_days from shiny.record where user = '", isolate(res_auth$user),"' order by exec_date desc;")
      res <- dbSendQuery(channel, query)
      myBacktestRecord <- fetch(res, n = -1) %>% as_tibble()
      dbDisconnect(channel)
      colnames(myBacktestRecord) <- c("執行回測時間", "股票代碼", "股票名稱", "回測起始日期", "回測結束日期", "進場條件", "出場條件", 
                                      "總交易次數", "平均報酬率", "報酬率標準差", "平均持有日數")
      
      return(datatable(myBacktestRecord))
    })
  })
  
  
  ################################# 回測績效排行榜 #################################
  observeEvent(input$tabs, {
    output$bestRecord <- renderDataTable({
      
      channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
      dbSendQuery(channel, "set names big5;")
      query <- paste0("select b.nick, a.exec_date, a.code, a.name, a.start_date, a.end_date, a.in_condition, a.out_condition, a.trade_nums, ",
                      "a.mean_ret, a.sd_ret, a.mean_hold_days from shiny.record as a ",
                      "left join (select user, nick from shiny.member) as b on a.user = b.user ",
                      "order by a.mean_ret desc limit 20;")
      res <- dbSendQuery(channel, query)
      bestRecord <- fetch(res, n = -1) %>% as_tibble()
      dbDisconnect(channel)
      colnames(bestRecord) <- c("使用者名稱", "執行回測時間", "股票代碼", "股票名稱", "回測起始日期", "回測結束日期", "進場條件", "出場條件", 
                                "總交易次數", "平均報酬率", "報酬率標準差", "平均持有日數")
      
      return(datatable(bestRecord))
    })
  })
  
  
  
  ################################# 紀錄瀏覽者行為 #################################
  # 紀錄使用者資訊
  observeEvent(input$tabs, {
    
    channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPasword)
    dbSendQuery(channel, "set names utf8;")
    query <- paste0("INSERT INTO shiny.browser (user, record_time, action) VALUES ",
                    "( '", isolate(res_auth$user), "', '", Sys.time(), "', '", isolate(input$tabs), "');")
    dbSendQuery(channel, query)
    dbDisconnect(channel)
  })
    
}


# 部署Shiny
shinyApp(ui, server)

