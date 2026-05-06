library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(shinyWidgets)

# ---------------- LOAD DATA ----------------
df_raw <- read_csv("smart_subscription_dataset_updated.csv")

df_raw <- df_raw %>%
  mutate(
    Start_Date = as.Date(Start_Date),
    Renewal_Date = as.Date(Renewal_Date, origin = "1899-12-30"),
    Category = as.character(Category),
    Status = as.character(Status),
    Monthly_Cost = as.numeric(Monthly_Cost)
  )

# ---------------- UI ----------------
ui <- fluidPage(
  
  tags$head(
    
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap"),
    
    tags$style(HTML("
      
      body {
        background-color: #f4f7fb;
        font-family: 'Poppins', sans-serif;
        color: #1f2d3d;
      }

      /* ---------------- TITLE ---------------- */
      .dashboard-title {
        font-size: 30px;
        font-weight: 700;
        color: #0f172a;
        padding: 14px 0 20px 0;
        position: relative;
      }

      .dashboard-title::after {
        content: '';
        display: block;
        width: 90px;
        height: 4px;
        margin-top: 6px;
        border-radius: 10px;
        background: linear-gradient(90deg, #ff5f8f, #42a5f5, #00bfa5);
      }

      /* ---------------- CARDS ---------------- */
      .card {
        background: white;
        padding: 12px;
        border-radius: 14px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.06);
        margin-bottom: 12px;
      }

      h4 {
        font-size: 14px;
        font-weight: 600;
      }

      /* ---------------- KPI ---------------- */
      .kpi-box {
        display: flex;
        gap: 12px;
        margin-bottom: 15px;
      }

      .kpi {
        flex: 1;
        padding: 18px;
        border-radius: 16px;
        color: white;
      }

      .pink { background: linear-gradient(135deg,#ff5f8f,#ff8fb1); }
      .teal { background: linear-gradient(135deg,#00bfa5,#5eead4); }
      .blue { background: linear-gradient(135deg,#42a5f5,#90caf9); }

      /* ---------------- STATUS ---------------- */
      .status-wrap {
        display: flex;
        gap: 12px;
      }

      .status-card {
        flex: 1;
        padding: 16px;
        border-radius: 14px;
        text-align: center;
        color: white;
      }

      /* ---------------- TOP 5 ---------------- */
      .top5-card table {
        width: 100%;
        border-collapse: collapse;
        font-size: 13px;
      }

      .top5-card th {
        background: linear-gradient(135deg, #6366f1, #a855f7);
        color: white;
        padding: 12px;
      }

      .top5-card td {
        padding: 10px;
        border-bottom: 1px solid #eef2f7;
      }

      /* ---------------- STATUS BADGES ---------------- */
      .status-active {
        color: #166534;
        background: #dcfce7;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
      }

      .status-inactive {
        color: #991b1b;
        background: #fee2e2;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
      }

      /* ---------------- ✨ NEW RENEWAL COLORS (PASTEL) ---------------- */
      .renewal-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 14px 16px;
        margin-bottom: 10px;
        border-radius: 14px;
        background: white;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        border-left: 5px solid transparent;
      }

      /* Soft pastel system */
      .critical {
        border-left-color: #fb7185;
        background: #fde2e4;
      }

      .upcoming {
        border-left-color: #f59e0b;
        background: #fff1c1;
      }

      .planned {
        border-left-color: #60a5fa;
        background: #dbeafe;
      }

      .renewal-title {
        font-weight: 600;
        font-size: 13px;
      }

      .renewal-sub {
        font-size: 12px;
        color: #64748b;
      }

      .renewal-right {
        font-weight: 600;
        font-size: 12px;
        opacity: 0.75;
      }

    "))
  ),
  
  # ---------------- TITLE ----------------
  div(class="dashboard-title", "Smart Subscription Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(class="card",
          h4("Filters"),
          pickerInput("category","Category",
                      choices=unique(df_raw$Category),
                      selected=unique(df_raw$Category),
                      multiple=TRUE,
                      options=list(`actions-box`=TRUE,`live-search`=TRUE)),
          
          radioButtons("status_filter","Status",
                       choices=c("All","Active","Inactive"),
                       selected="All")
      ),
      
      div(class="card",
          h4("Spending"),
          sliderInput("cost","Monthly Spend Range",
                      min=floor(min(df_raw$Monthly_Cost)/10)*10,
                      max=ceiling(max(df_raw$Monthly_Cost)/10)*10,
                      value=c(0,200), step=10),
          numericInput("budget","Monthly Budget",1600)
      ),
      
      div(class="card",
          h4("Renewals"),
          dateInput("today","Today",Sys.Date()),
          numericInput("renewal_days","Show Next (Days)",90)
      )
    ),
    
    mainPanel(
      
      div(class="kpi-box",
          div(class="kpi pink", div("Total Spend"), div(textOutput("total_spend"))),
          div(class="kpi teal", div("Average Cost"), div(textOutput("avg_spend"))),
          div(class="kpi blue", div("Budget Status"), uiOutput("budget_status"))
      ),
      
      div(class="card",
          h4("Subscription Status Overview"),
          div(class="status-wrap",
              div(class="status-card", style="background:#2563eb",
                  div("Active"), div(textOutput("active_count"))),
              div(class="status-card", style="background:#f43f5e",
                  div("Inactive"), div(textOutput("inactive_count")))
          )
      ),
      
      div(class="card top5-card",
          h4("Top 5 Highest Cost Subscriptions"),
          tableOutput("top5")
      ),
      
      div(class="card",
          h4("Upcoming Renewals"),
          uiOutput("renewals_list")
      ),
      
      div(class="card",
          h4("Spend by Status"),
          plotlyOutput("donut")
      ),
      
      div(class="card",
          h4("Spend by Category"),
          plotlyOutput("bar")
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- df_raw
    
    if(!is.null(input$category))
      d <- d %>% filter(Category %in% input$category)
    
    if(input$status_filter=="Active")
      d <- d %>% filter(Status=="Active")
    else if(input$status_filter=="Inactive")
      d <- d %>% filter(Status=="Inactive")
    
    d %>% filter(
      Monthly_Cost >= input$cost[1],
      Monthly_Cost <= input$cost[2]
    )
  })
  
  output$total_spend <- renderText({
    paste0("$", round(sum(filtered_data()$Monthly_Cost),2))
  })
  
  output$avg_spend <- renderText({
    paste0("$", round(mean(filtered_data()$Monthly_Cost),2))
  })
  
  output$budget_status <- renderUI({
    diff <- sum(filtered_data()$Monthly_Cost) - input$budget
    
    if(diff>0)
      div(style="background:#ffeaea;color:#ff3b3b;padding:8px;border-radius:10px;",
          paste0("Over by $", round(diff,2)))
    else
      div(style="background:#eafff1;color:#00a86b;padding:8px;border-radius:10px;",
          paste0("Under by $", round(abs(diff),2)))
  })
  
  output$active_count <- renderText({
    sum(filtered_data()$Status=="Active")
  })
  
  output$inactive_count <- renderText({
    sum(filtered_data()$Status=="Inactive")
  })
  
  output$top5 <- renderTable({
    filtered_data() %>%
      arrange(desc(Monthly_Cost)) %>%
      head(5) %>%
      mutate(
        Monthly_Cost=paste0("$",round(Monthly_Cost,2)),
        Status=ifelse(Status=="Active",
                      "<span class='status-active'>Active</span>",
                      "<span class='status-inactive'>Inactive</span>")
      ) %>%
      select(Subscription_Name,Category,Monthly_Cost,Status)
  }, sanitize.text.function=function(x)x)
  
  output$renewals_list <- renderUI({
    
    df <- filtered_data() %>%
      filter(!is.na(Renewal_Date)) %>%
      mutate(Days = as.integer(Renewal_Date - as.Date(input$today))) %>%
      filter(Days >= 0, Days <= input$renewal_days) %>%
      arrange(Days) %>%
      head(8)
    
    div(
      lapply(seq_len(nrow(df)), function(i){
        
        cls <- if(df$Days[i] <= 7) "critical"
        else if(df$Days[i] <= 30) "upcoming"
        else "planned"
        
        div(class=paste("renewal-item", cls),
            
            div(
              div(class="renewal-title", df$Category[i]),
              div(class="renewal-sub",
                  format(df$Renewal_Date[i], "%b %d, %Y"))
            ),
            
            div(class="renewal-right",
                paste0(df$Days[i], " days"))
        )
      })
    )
  })
  
  output$donut <- renderPlotly({
    d <- filtered_data() %>% group_by(Status) %>% summarise(Total=sum(Monthly_Cost))
    
    plot_ly(d,labels=~Status,values=~Total,type="pie",hole=0.6,
            marker=list(colors=c("#ff5f8f","#00bfa5")))
  })
  
  output$bar <- renderPlotly({
    d <- filtered_data() %>% group_by(Category) %>% summarise(Total=sum(Monthly_Cost))
    
    plot_ly(d,x=~Category,y=~Total,type="bar",
            marker=list(color="#42a5f5"))
  })
}

shinyApp(ui,server)