#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)


dateInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }" ), 
   # Application title
   titlePanel("Зарплата v0.1"),
   
  dateInput2("date", label = h3("Date input"), value = Sys.Date(),
             format = "mm-yyyy", minview = "months", startview = "year",daysofweekdisabled = seq(1:8)),
  sidebarPanel( #designates location of following items
     htmlOutput("operator_selector")
   ),
   tableOutput("table"),
   sidebarPanel(width=12, #designates location of following items
     fluidRow(
       column(4,htmlOutput("project_selector1"),htmlOutput("project_selector1_main"),htmlOutput("sick1"),htmlOutput("quality1")),
       column(4,htmlOutput("project_selector2"),htmlOutput("project_selector2_main"),htmlOutput("sick2"),htmlOutput("quality2")),
       column(4,htmlOutput("project_selector3"),htmlOutput("project_selector3_main"),htmlOutput("sick3"),htmlOutput("quality3"))
     )
   ),
   tableOutput("table_union"),
   rHandsontableOutput("table_pm"),
   tableOutput("table1"),
    sidebarPanel(width=12, fluidRow(
      column(6,htmlOutput("prophylaxis"),htmlOutput("minus")),
      column(6,htmlOutput("ss"),htmlOutput("komment"))
      )
    ),
   hr(),
   fluidRow(column(4, verbatimTextOutput("value")))
      
   
)

server <- function(input, output) {
  indicators <- read.csv2("projects.csv")
    
    observe({
    day01 <- as.character(format(input$date, "%d.%m.%Y"))
    dayend <- paste0(as.character(numberOfDays(input$date)),
                     as.character(format(input$date, ".%m.%Y")))
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "xxx.xxx.xxx.xxx",
                     Database = "db_name",
                     UID = "name",
                     PWD = "password",
                     Port = port)
    
    query_online <- paste("WITH user_ontask AS  (
                          SELECT *
                          FROM (SELECT utm.user_id, utm.action_code, utm.task_id, 
                          utm.action_date AS dt_start,
                          CASE WHEN LAG(utm.action_date, 1, GETDATE()) OVER (ORDER BY utm.user_id, utm.task_id, action_date DESC) <utm.action_date
                          OR LAG(utm.user_id, 1) OVER (ORDER BY utm.task_id, utm.user_id, action_date DESC)<>utm.user_id
                          THEN GETDATE() ELSE
                          LAG(utm.action_date, 1, GETDATE()) OVER (ORDER BY utm.user_id, utm.task_id, action_date DESC)
                          END AS dt_end
                          FROM user_task_movement utm
                          ) prepare
                          WHERE prepare.action_code = 1 --постановка на задачу
    )
                          
                          SELECT u.user_name, SUM(DATEDIFF(SECOND, online_begin, online_end ))/3600.0 AS duration, task.task_name
                          FROM (SELECT ual.user_id AS user_id,
                          --начало онлайна
                          CASE WHEN ual.dt_begin < '",day01,"'
                          THEN '",day01,"' 
                          ELSE ual.dt_begin END AS online_begin,
                          
                          --конец онлайна
                          CASE WHEN COALESCE(ual.dt_end, GETDATE()) > '",dayend,"' 
                          THEN '",dayend,"' 
                          ELSE COALESCE(ual.dt_end, GETDATE()) END AS online_end                                                 
                          FROM user_activity_log ual
                          WHERE (ual.dt_begin BETWEEN '",day01,"' AND '",dayend,"'
                          OR COALESCE(ual.dt_end, GETDATE()) BETWEEN '",day01,"' AND '",dayend,"'
                          OR (ual.dt_begin < '",day01,"' AND COALESCE(ual.dt_end, GETDATE()) > '",dayend,"')
                          )AND ual.status_id=16
                          AND ual.dt_begin IS NOT NULL ) user_online
                          INNER JOIN users u ON u.user_id=user_online.user_id
                          LEFT JOIN user_ontask t ON online_end BETWEEN t.dt_start AND t.dt_end AND t.user_id=user_online.user_id
                          INNER JOIN task ON t.task_id=task.task_id GROUP BY u.user_name, task.task_name")
    
    
    result <- dbSendQuery(con, query_online)
    
    duration_spb2 <- dbFetch(result)
    dbClearResult(result)
    
    query_result <- paste("SELECT t.task_name, u.user_name,
                          SUM(DATEDIFF(SECOND, ch.timestart_abon, ch.timestop_abon))/3600.0 AS totalsec_abon , 
                          SUM(CASE WHEN DATEDIFF(SECOND, ch.timestop_abon, ch.dt_form_closed) BETWEEN -3 AND 3 THEN 0 
                          ELSE DATEDIFF(SECOND, ch.timestop_abon, ch.dt_form_closed) END) AS time_kart,
                          COUNT(DATEDIFF(SECOND, ch.timestop_abon, ch.dt_form_closed)) AS kolvo_kart
                          FROM dbo.call_info ci 
                          INNER JOIN task t ON ci.task_id = t.task_id
                          INNER JOIN dbo.call_history ch ON ci.call_info_id = ch.call_info_id
                          FULL OUTER JOIN dbo.users u ON ch.user_id = u.user_id
                          WHERE ch.dt_create >='",day01,"' AND ch.dt_create<'",dayend,"'
                          GROUP BY t.task_name, u.user_name")
    
    
    result <- dbSendQuery(con, query_result)
    
    result_spb2 <- dbFetch(result)
    dbClearResult(result)
    
    spb2 <- merge(duration_spb2, result_spb2, by=c("user_name", "task_name"))
    output$operator_selector = renderUI({
      selectInput(inputId = "operator", #name of input
                  label = "Выберите оператора:", #label displayed in ui
                  choices = unique(spb2$user_name),
                  selected=unique(spb2$user_name)[1])
})
    output$table <- renderTable({spb2[spb2$user_name==input$operator,]})
    output$project_selector1 = renderUI({
      checkboxGroupInput("Group1", label = h5("Группа проектов 1"), 
                         choices = unique(spb2$task_name[spb2$user_name==input$operator]))
    })
    output$project_selector1_main = renderUI({
      selectInput(inputId = "project_selector1_main", #name of input
                  label = "Выберите основной проект:", 
                         choices = unique(spb2$task_name[spb2$user_name==input$operator]))
    })
    output$sick1 = renderUI({
      checkboxInput(inputId = "sick1", #name of input
                  label = "Больничный/отпуск:", 
                  value = FALSE)
    })
    output$quality1 = renderUI({
      numericInput(inputId = "quality1", #name of input
                    label = "Коэффициент качества:", 
                    value = 100)
    })
    output$project_selector2 = renderUI({ 
      checkboxGroupInput("Group2", label = h5("Группа проектов 2"), 
                         choices = unique(spb2$task_name[spb2$user_name==input$operator]))
    })
    output$project_selector2_main = renderUI({
      selectInput(inputId = "project_selector2_main", #name of input
                  label = "Выберите основной проект:", 
                  choices = unique(spb2$task_name[spb2$user_name==input$operator]))
    })
    output$sick2 = renderUI({
      checkboxInput(inputId = "sick2", #name of input
                    label = "Больничный/отпуск:", 
                    value = FALSE)
    })
    output$quality2 = renderUI({
      numericInput(inputId = "quality2", #name of input
                   label = "Коэффициент качества:", 
                   value = 100)
    })
    output$project_selector3 = renderUI({
      checkboxGroupInput("Group3", label = h5("Группа проектов 3"), 
                         choices = unique(spb2$task_name[spb2$user_name==input$operator]))
    })
    output$project_selector3_main = renderUI({
      selectInput(inputId = "project_selector3_main", #name of input
                  label = "Выберите основной проект:", 
                  choices = unique(spb2$task_name[spb2$user_name==input$operator]))
    })
    output$sick3 = renderUI({
      checkboxInput(inputId = "sick3", #name of input
                    label = "Больничный/отпуск:", 
                    value = FALSE)
    })
    output$quality3 = renderUI({
      numericInput(inputId = "quality3", #name of input
                   label = "Коэффициент качества:", 
                   value = 100)
    })
    table_with_group_final <-  reactive({
      table_with_group1 <- spb2[spb2$user_name==input$operator & spb2$task_name %in% input$Group1,]
      table_with_group1 <- aggregate(table_with_group1[3:6], by=list(user_name=table_with_group1$user_name), sum)
      if(length(input$Group2)!=0){
        table_with_group2 <- spb2[spb2$user_name==input$operator & spb2$task_name %in% input$Group2,]
        table_with_group2 <- aggregate(table_with_group2[3:6], by=list(user_name=table_with_group2$user_name), sum)
      }
      else {table_with_group2 <- data.frame(user_name=c(0), duration=c(0), totalsec_abon=c(0), time_kart=c(0), kolvo_kart=c(0))}
      if(length(input$Group3)!=0){
        table_with_group3 <- spb2[spb2$user_name==input$operator & spb2$task_name %in% input$Group3,]
        table_with_group3 <- aggregate(table_with_group3[3:6], by=list(user_name=table_with_group3$user_name), sum)
      }
      else {table_with_group3 <- data.frame(user_name=c(0), duration=c(0), totalsec_abon=c(0), time_kart=c(0), kolvo_kart=c(0))}
      table_with_group_final <- data.frame(user_name=c(table_with_group1$user_name, table_with_group2$user_name,table_with_group3$user_name),
                                           duration=c(table_with_group1$duration,table_with_group2$duration,table_with_group3$duration),
                                           kart = c(table_with_group1$time_kart/table_with_group1$kolvo_kart, table_with_group2$time_kart/table_with_group2$kolvo_kart,table_with_group3$time_kart/table_with_group3$kolvo_kart),
                                           produkt = c(table_with_group1$totalsec_abon/table_with_group1$duration*100,table_with_group2$totalsec_abon/table_with_group2$duration*100,table_with_group3$totalsec_abon/table_with_group3$duration*100), 
                                           task_name=c(input$project_selector1_main,input$project_selector2_main,input$project_selector3_main))
      table_with_group_final$stavka <- c(indicators$stavka[indicators$task_name==input$project_selector1_main], indicators$stavka[indicators$task_name==input$project_selector2_main], indicators$stavka[indicators$task_name==input$project_selector3_main])
      
      criterion1_group1 <- ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_1_5_min[indicators$task_name==input$project_selector1_main] & 
                                    table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_1_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_1_4_min[indicators$task_name==input$project_selector1_main] & 
                                           table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_1_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_1_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_1_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_1_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_1_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_1_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                table_with_group_final$kart[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_1_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],0)))))
      criterion2_group1 <- ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_2_5_min[indicators$task_name==input$project_selector1_main] & 
                                    table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_2_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_2_4_min[indicators$task_name==input$project_selector1_main] & 
                                           table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_2_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_2_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_2_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_2_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_2_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]>=indicators$criterion_2_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector1_main]<indicators$criterion_2_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],0)))))
      criterion3_group1 <- ifelse(input$quality1>=indicators$criterion_3_5_min[indicators$task_name==input$project_selector1_main] & 
                                    input$quality1<indicators$criterion_3_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(input$quality1>=indicators$criterion_3_4_min[indicators$task_name==input$project_selector1_main] & 
                                           input$quality1<indicators$criterion_3_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(input$quality1>=indicators$criterion_3_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  input$quality1<indicators$criterion_3_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(input$quality1>=indicators$criterion_3_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         input$quality1<indicators$criterion_3_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(input$quality1>=indicators$criterion_3_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                input$quality1<indicators$criterion_3_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],0)))))
      
      criterion1_group2 <- ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_1_5_min[indicators$task_name==input$project_selector2_main] & 
                                    table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_1_5_max[indicators$task_name==input$project_selector2_main],5*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_1_4_min[indicators$task_name==input$project_selector2_main] & 
                                           table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_1_4_max[indicators$task_name==input$project_selector2_main],4*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_1_3_min[indicators$task_name==input$project_selector2_main] & 
                                                  table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_1_3_max[indicators$task_name==input$project_selector2_main],3*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_1_2_min[indicators$task_name==input$project_selector2_main] & 
                                                         table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_1_2_max[indicators$task_name==input$project_selector2_main],2*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_1_1_min[indicators$task_name==input$project_selector2_main] & 
                                                                table_with_group_final$kart[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_1_1_max[indicators$task_name==input$project_selector2_main],1*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],0)))))
      criterion2_group2 <- ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_2_5_min[indicators$task_name==input$project_selector2_main] & 
                                    table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_2_5_max[indicators$task_name==input$project_selector2_main],5*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_2_4_min[indicators$task_name==input$project_selector2_main] & 
                                           table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_2_4_max[indicators$task_name==input$project_selector2_main],4*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_2_3_min[indicators$task_name==input$project_selector2_main] & 
                                                  table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_2_3_max[indicators$task_name==input$project_selector2_main],3*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_2_2_min[indicators$task_name==input$project_selector2_main] & 
                                                         table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_2_2_max[indicators$task_name==input$project_selector2_main],2*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]>indicators$criterion_2_1_min[indicators$task_name==input$project_selector2_main] & 
                                                                table_with_group_final$produkt[table_with_group_final$task_name==input$project_selector2_main]<indicators$criterion_2_1_max[indicators$task_name==input$project_selector2_main],1*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],0)))))
      
      criterion3_group2 <- ifelse(input$quality2>indicators$criterion_3_5_min[indicators$task_name==input$project_selector1_main] & 
                                    input$quality2<indicators$criterion_3_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(input$quality2>indicators$criterion_3_4_min[indicators$task_name==input$project_selector1_main] & 
                                           input$quality2<indicators$criterion_3_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(input$quality2>indicators$criterion_3_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  input$quality2<indicators$criterion_3_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(input$quality2>indicators$criterion_3_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         input$quality2<indicators$criterion_3_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(input$quality2>indicators$criterion_3_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                input$quality2<indicators$criterion_3_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],0)))))
      
      
      table_with_group_final$criterion <- c(criterion1_group1[1]+criterion2_group1[1]+criterion3_group1[1], criterion1_group2[1]+criterion2_group2[1]+criterion3_group2[1], criterion1_group2[1]+criterion2_group2[1]+criterion3_group2[1])
      table_with_group_final$sick <- c(input$sick1, input$sick2, input$sick3)
      table_with_group_final <- table_with_group_final[!apply(table_with_group_final,1,anyNA),]
      table_with_group_final$SalaryForHour <- table_with_group_final$duration*table_with_group_final$stavka
      
      table_with_group_final$PersonalBonus <- ifelse((table_with_group_final$duration>=110 | table_with_group_final$sick==TRUE), ifelse(table_with_group_final$duration>168,700,table_with_group_final$duration*700/168),0)
      table_with_group_final$PersonalBonusQuality <- table_with_group_final$PersonalBonus/5*table_with_group_final$criterion
      table_with_group_final$Itogo <- table_with_group_final$SalaryForHour+table_with_group_final$PersonalBonusQuality
      
      table_with_group_final 
      
    })
    output$table_union <- renderTable({
      table_with_group_final()
    })
    
    output$table_pm <- renderRHandsontable({
      df <- table_with_group_final()
      rhandsontable(df[1:4]) # converts the R dataframe to rhandsontable object
    })
    
    output$table1 <- renderTable({
      table_pm <- hot_to_r(input$table_pm)
      table <- table_with_group_final()
      table_final <- table_pm
      table_final$task_name <- table$task_name
      table_final$stavka <- table$stavka
      
      criterion1_group1 <- ifelse(table_final$kart[table_final$task_name==input$project_selector1_main]>=indicators$criterion_1_5_min[indicators$task_name==input$project_selector1_main] & 
                                    table_final$kart[table_final$task_name==input$project_selector1_main]<indicators$criterion_1_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_final$kart[table_final$task_name==input$project_selector1_main]>=indicators$criterion_1_4_min[indicators$task_name==input$project_selector1_main] & 
                                           table_final$kart[table_final$task_name==input$project_selector1_main]<indicators$criterion_1_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_final$kart[table_final$task_name==input$project_selector1_main]>=indicators$criterion_1_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  table_final$kart[table_final$task_name==input$project_selector1_main]<indicators$criterion_1_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_final$kart[table_final$task_name==input$project_selector1_main]>=indicators$criterion_1_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         table_final$kart[table_final$task_name==input$project_selector1_main]<indicators$criterion_1_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_final$kart[table_final$task_name==input$project_selector1_main]>=indicators$criterion_1_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                table_final$kart[table_final$task_name==input$project_selector1_main]<indicators$criterion_1_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],0)))))
      criterion2_group1 <- ifelse(table_final$produkt[table_final$task_name==input$project_selector1_main]>=indicators$criterion_2_5_min[indicators$task_name==input$project_selector1_main] & 
                                    table_final$produkt[table_final$task_name==input$project_selector1_main]<indicators$criterion_2_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_final$produkt[table_final$task_name==input$project_selector1_main]>=indicators$criterion_2_4_min[indicators$task_name==input$project_selector1_main] & 
                                           table_final$produkt[table_final$task_name==input$project_selector1_main]<indicators$criterion_2_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_final$produkt[table_final$task_name==input$project_selector1_main]>=indicators$criterion_2_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  table_final$produkt[table_final$task_name==input$project_selector1_main]<indicators$criterion_2_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_final$produkt[table_final$task_name==input$project_selector1_main]>=indicators$criterion_2_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         table_final$produkt[table_final$task_name==input$project_selector1_main]<indicators$criterion_2_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_final$produkt[table_final$task_name==input$project_selector1_main]>=indicators$criterion_2_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                table_final$produkt[table_final$task_name==input$project_selector1_main]<indicators$criterion_2_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],0)))))
      criterion3_group1 <- ifelse(input$quality1>=indicators$criterion_3_5_min[indicators$task_name==input$project_selector1_main] & 
                                    input$quality1<indicators$criterion_3_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(input$quality1>=indicators$criterion_3_4_min[indicators$task_name==input$project_selector1_main] & 
                                           input$quality1<indicators$criterion_3_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(input$quality1>=indicators$criterion_3_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  input$quality1<indicators$criterion_3_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(input$quality1>=indicators$criterion_3_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         input$quality1<indicators$criterion_3_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(input$quality1>=indicators$criterion_3_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                input$quality1<indicators$criterion_3_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],0)))))
      
      criterion1_group2 <- ifelse(table_final$kart[table_final$task_name==input$project_selector2_main]>indicators$criterion_1_5_min[indicators$task_name==input$project_selector2_main] & 
                                    table_final$kart[table_final$task_name==input$project_selector2_main]<indicators$criterion_1_5_max[indicators$task_name==input$project_selector2_main],5*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_final$kart[table_final$task_name==input$project_selector2_main]>indicators$criterion_1_4_min[indicators$task_name==input$project_selector2_main] & 
                                           table_final$kart[table_final$task_name==input$project_selector2_main]<indicators$criterion_1_4_max[indicators$task_name==input$project_selector2_main],4*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_final$kart[table_final$task_name==input$project_selector2_main]>indicators$criterion_1_3_min[indicators$task_name==input$project_selector2_main] & 
                                                  table_final$kart[table_final$task_name==input$project_selector2_main]<indicators$criterion_1_3_max[indicators$task_name==input$project_selector2_main],3*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_final$kart[table_final$task_name==input$project_selector2_main]>indicators$criterion_1_2_min[indicators$task_name==input$project_selector2_main] & 
                                                         table_final$kart[table_final$task_name==input$project_selector2_main]<indicators$criterion_1_2_max[indicators$task_name==input$project_selector2_main],2*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_final$kart[table_final$task_name==input$project_selector2_main]>indicators$criterion_1_1_min[indicators$task_name==input$project_selector2_main] & 
                                                                table_final$kart[table_final$task_name==input$project_selector2_main]<indicators$criterion_1_1_max[indicators$task_name==input$project_selector2_main],1*indicators$criterion_1_weight[indicators$task_name==input$project_selector1_main],0)))))
      criterion2_group2 <- ifelse(table_final$produkt[table_final$task_name==input$project_selector2_main]>indicators$criterion_2_5_min[indicators$task_name==input$project_selector2_main] & 
                                    table_final$produkt[table_final$task_name==input$project_selector2_main]<indicators$criterion_2_5_max[indicators$task_name==input$project_selector2_main],5*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(table_final$produkt[table_final$task_name==input$project_selector2_main]>indicators$criterion_2_4_min[indicators$task_name==input$project_selector2_main] & 
                                           table_final$produkt[table_final$task_name==input$project_selector2_main]<indicators$criterion_2_4_max[indicators$task_name==input$project_selector2_main],4*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(table_final$produkt[table_final$task_name==input$project_selector2_main]>indicators$criterion_2_3_min[indicators$task_name==input$project_selector2_main] & 
                                                  table_final$produkt[table_final$task_name==input$project_selector2_main]<indicators$criterion_2_3_max[indicators$task_name==input$project_selector2_main],3*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(table_final$produkt[table_final$task_name==input$project_selector2_main]>indicators$criterion_2_2_min[indicators$task_name==input$project_selector2_main] & 
                                                         table_final$produkt[table_final$task_name==input$project_selector2_main]<indicators$criterion_2_2_max[indicators$task_name==input$project_selector2_main],2*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(table_final$produkt[table_final$task_name==input$project_selector2_main]>indicators$criterion_2_1_min[indicators$task_name==input$project_selector2_main] & 
                                                                table_final$produkt[table_final$task_name==input$project_selector2_main]<indicators$criterion_2_1_max[indicators$task_name==input$project_selector2_main],1*indicators$criterion_2_weight[indicators$task_name==input$project_selector1_main],0)))))
      
      criterion3_group2 <- ifelse(input$quality2>indicators$criterion_3_5_min[indicators$task_name==input$project_selector1_main] & 
                                    input$quality2<indicators$criterion_3_5_max[indicators$task_name==input$project_selector1_main],5*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                  ifelse(input$quality2>indicators$criterion_3_4_min[indicators$task_name==input$project_selector1_main] & 
                                           input$quality2<indicators$criterion_3_4_max[indicators$task_name==input$project_selector1_main],4*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                         ifelse(input$quality2>indicators$criterion_3_3_min[indicators$task_name==input$project_selector1_main] & 
                                                  input$quality2<indicators$criterion_3_3_max[indicators$task_name==input$project_selector1_main],3*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                ifelse(input$quality2>indicators$criterion_3_2_min[indicators$task_name==input$project_selector1_main] & 
                                                         input$quality2<indicators$criterion_3_2_max[indicators$task_name==input$project_selector1_main],2*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],
                                                       ifelse(input$quality2>indicators$criterion_3_1_min[indicators$task_name==input$project_selector1_main] & 
                                                                input$quality2<indicators$criterion_3_1_max[indicators$task_name==input$project_selector1_main],1*indicators$criterion_3_weight[indicators$task_name==input$project_selector1_main],0)))))
      
      if(length(input$Group3)!=0){
        table_final$criterion <- c(criterion1_group1[1]+criterion2_group1[1]+criterion3_group1[1], criterion1_group2[1]+criterion2_group2[1]+criterion3_group2[1], criterion1_group2[1]+criterion2_group2[1]+criterion3_group2[1])
      }
      else{
        if(length(input$Group2)!=0){
        table_final$criterion <- c(criterion1_group1[1]+criterion2_group1[1]+criterion3_group1[1], criterion1_group2[1]+criterion2_group2[1]+criterion3_group2[1])
        }
        else{   
          table_final$criterion <- c(criterion1_group1[1]+criterion2_group1[1]+criterion3_group1[1])
        }
      }
      
      table_final$sick <- table$sick
      table_final$SalaryForHour <- table_final$duration*table_final$stavka
      table_final$PersonalBonus <- ifelse((table_final$duration>=110 | table_final$sick==TRUE), ifelse(table_final$duration>168,700,table_final$duration*700/168),0)
      table_final$PersonalBonusQuality <- table_final$PersonalBonus/5*table_final$criterion
      table_final$Itogo <- table_final$SalaryForHour+table_final$PersonalBonusQuality
      
      table_final
    })
    output$prophylaxis = renderUI({
      numericInput(inputId = "prophylaxis", #name of input
                   label = "Введите количество часов профилактики:", 
                   value = 0)
    })
    output$ss = renderUI({
      numericInput(inputId = "ss", #name of input
                   label = "Введите количество часов старшим смены:", 
                   value = 0)
    })
    output$minus = renderUI({
      numericInput(inputId = "minus", #name of input
                   label = "Введите величину штрафа в рублях:", 
                   value = 0)
    })
    output$komment = renderUI({
      textInput(inputId = "komment", #name of input
                   label = "Оставьте комментарий к рассчёту:")
    })
    
    

    })

}

# Run the application 
shinyApp(ui = ui, server = server)

