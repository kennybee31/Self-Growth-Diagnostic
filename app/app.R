library(shiny)
library(bslib)
library(ggplot2)
library(psychTools)
library(janitor)
library(viridis)

# 1. 權威數據載入 (SAPA Project)
data(bfi)
soul_data <- bfi |> 
  as.data.frame() |> 
  clean_names() |> 
  subset(select = c(c1,c2,c3,c4,c5,n1,n2,n3,n4,n5)) |>
  na.omit()

soul_data$Discipline <- rowMeans(soul_data[,1:5])
soul_data$Stability  <- 7 - rowMeans(soul_data[,6:10])

# 2. 雙語與法律免責字典
i18n <- list(
  en = list(
    title = "EBDS Self-Growth Diagnostic",
    disclaimer = "⚠️ DISCLAIMER: For reference only. Not a clinical diagnosis. Consult a professional for health concerns.",
    engine_label = "Discipline (1=Low, 6=High)",
    brake_label = "Stability (1=Low, 6=High)",
    btn_label = "Run Audit",
    map_title = "Global Soul Map (N=2,800)",
    status_title = "Strategic Audit Report",
    safe = "✅ Status: Balanced. Systems operational.",
    fragile = "⚠️ Warning: Fragility Peak. Cooling required.",
    holy_grail = "🌟 Excellence: High performance, high stability."
  ),
  zh = list(
    title = "EBDS 自我成長診斷",
    disclaimer = "⚠️ 法律聲明：本結果僅供參考，不具臨床診斷效力。若有身心健康疑慮，請諮詢專業醫療人員。",
    engine_label = "執行力 (1=低, 6=高)",
    brake_label = "穩定性 (1=低, 6=高)",
    btn_label = "開始數據審計",
    map_title = "2,800 顆靈魂的全球地圖",
    status_title = "精算師診斷報告",
    safe = "✅ 狀態：平衡成長。目前系統運作穩健。",
    fragile = "⚠️ 警告：玻璃強者。引擎過熱，建議補強韌性。",
    holy_grail = "🌟 卓越：聖杯象限。極高產出且情緒穩健。"
  )
)

# 3. UI 介面 (手機版優化)
ui <- page_fixed(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # 頂部語言切換與標題
  div(style = "padding: 15px; background: #2c3e50; color: white; border-radius: 0 0 15px 15px;",
      selectInput("lang", NULL, choices = c("繁體中文" = "zh", "English" = "en"), width = "120px"),
      uiOutput("header_ui")
  ),
  
  # 法律警示語 (ISO 42001 必備)
  div(style = "padding: 10px; color: #e74c3c; font-size: 0.85rem; font-weight: bold; background: #fdf2f2; margin: 10px 0; border-radius: 5px;",
      uiOutput("disclaimer_ui")
  ),
  
  layout_column_wrap(
    width = 1,
    card(
      uiOutput("input_ui"),
      actionButton("calc", "Run Diagnostic", class = "btn-primary w-100", style = "margin-top: 10px;")
    ),
    
    card(
      plotOutput("distPlot", height = "320px")
    ),
    
    card(
      uiOutput("report_ui")
    )
  )
)

# 4. Server 邏輯
server <- function(input, output, session) {
  
  output$header_ui <- renderUI({ h3(i18n[[input$lang]]$title, style = "margin: 0; font-weight: bold;") })
  output$disclaimer_ui <- renderUI({ i18n[[input$lang]]$disclaimer })
  
  # 精簡後的刻度：step 改為 0.5，減少視覺雜訊
  output$input_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(
      sliderInput("u_disc", L$engine_label, min = 1, max = 6, value = 3.5, step = 0.5, ticks = FALSE),
      sliderInput("u_stab", L$brake_label, min = 1, max = 6, value = 3.5, step = 0.5, ticks = FALSE)
    )
  })
  
  output$report_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(card_header(L$status_title), textOutput("diag_text"))
  })
  
  # 繪圖邏輯：修正錯誤 (明確指定 ggplot2::annotate)
  output$distPlot <- renderPlot({
    L <- i18n[[input$lang]]
    
    ggplot(soul_data, aes(x = Discipline, y = Stability)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
      scale_fill_viridis_c(option = "plasma") +
      # 標註用戶位置 (綠色鑽石)
      geom_point(aes(x = input$u_disc, y = input$u_stab), color = "#2ecc71", size = 8, shape = 18) +
      # 關鍵修復：使用 ggplot2::annotate 並明確指定類型
      ggplot2::annotate("rect", xmin = 4.5, xmax = 6, ymin = 1, ymax = 2.5, 
                        alpha = 0.2, fill = "white", color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = L$map_title, x = NULL, y = NULL) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })
  
  output$diag_text <- renderText({
    L <- i18n[[input$lang]]
    if (input$u_disc > 4.5 & input$u_stab < 2.5) return(L$fragile)
    if (input$u_disc > 4.5 & input$u_stab > 4.5) return(L$holy_grail)
    return(L$safe)
  })
}

shinyApp(ui, server)