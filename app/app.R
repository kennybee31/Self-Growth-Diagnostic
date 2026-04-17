# ==============================================================================
# 專案名稱：EBDS 自我成長診斷 (The Discipline Paradox Audit)
# 技術標準：ISO 42001 數據透明度與穩健性規範
# 數據來源：SAPA Project (N=2,800 真實受訪者)
# ==============================================================================

# 1. 核心依賴套件 (明確載入所有依賴，防止 WebR/Shinylive 崩潰)
library(shiny)
library(bslib)
library(ggplot2)
library(psychTools)
library(janitor)
library(viridis)
library(scales)   
library(mgcv)     
library(farver)   

# 2. 數據血統精算 (載入 2800 顆真實靈魂資料)
data(bfi)
soul_data <- bfi |> 
  as.data.frame() |> 
  clean_names() |> 
  subset(select = c(c1, c2, c3, c4, c5, n1, n2, n3, n4, n5)) |>
  na.omit()

# 計算特質均分 (Discipline: 執行引擎; Stability: 情緒冷卻系統)
soul_data$Discipline <- rowMeans(soul_data[, 1:5])
soul_data$Stability  <- 7 - rowMeans(soul_data[, 6:10])

# 3. 雙語字典與合規聲明
i18n <- list(
  en = list(
    title = "The Discipline Paradox Audit",
    disclaimer = "⚠️ DISCLAIMER: For reference only. Not a clinical diagnosis. Consult a professional for health concerns.",
    engine_label = "Discipline Intensity (The Engine)",
    brake_label = "Emotional Stability (The Brake)",
    btn_label = "Execute Strategic Audit",
    map_title = "Global Soul Map (N=2,800)",
    status_title = "Actuarial Diagnostic Report",
    safe = "✅ STATUS: Balanced Growth. System thermal regulation is optimal.",
    fragile = "⚠️ WARNING: Fragility Peak. Engine redlining without sufficient cooling.",
    holy_grail = "🌟 EXCELLENCE: The Holy Grail. High output with perfect resilience."
  ),
  zh = list(
    title = "EBDS 自律悖論審計",
    disclaimer = "⚠️ 法律聲明：本結果基於統計趨勢，僅供參考，不具臨床診斷效力。若有身心健康疑慮，請諮詢醫療人員。",
    engine_label = "執行力強度 (自律引擎)",
    brake_label = "穩定性強度 (冷卻系統)",
    btn_label = "開始數據審計",
    map_title = "2,800 顆靈魂的全球地圖",
    status_title = "精算師診斷報告",
    safe = "✅ 狀態：平衡成長。你目前的散熱系統運作良好。",
    fragile = "⚠️ 警告：玻璃強者。引擎轉速極高但缺乏冷卻，隨時可能燒毀。",
    holy_grail = "🌟 卓越：聖杯象限。具備極高產出且情緒防線穩固。"
  )
)

# 4. UI 介面設計
ui <- page_fixed(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  div(style = "padding: 15px; background: #2c3e50; color: white; border-radius: 0 0 15px 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
      selectInput("lang", NULL, choices = c("繁體中文" = "zh", "English" = "en"), width = "120px"),
      uiOutput("header_ui")
  ),
  
  div(style = "padding: 10px; color: #e74c3c; font-size: 0.82rem; font-weight: bold; background: #fff5f5; border: 1px solid #ffcccc; margin: 15px 0; border-radius: 8px;",
      uiOutput("disclaimer_ui")
  ),
  
  layout_column_wrap(
    width = 1,
    card(
      uiOutput("input_ui"),
      actionButton("calc", "Diagnostic", class = "btn-primary w-100", style = "margin-top: 10px; font-weight: bold;")
    ),
    
    card(
      plotOutput("distPlot", height = "320px")
    ),
    
    card(
      uiOutput("report_ui")
    )
  )
)

# 5. Server 邏輯處理
server <- function(input, output, session) {
  
  output$header_ui <- renderUI({ h3(i18n[[input$lang]]$title, style = "margin: 0; font-weight: bold;") })
  output$disclaimer_ui <- renderUI({ i18n[[input$lang]]$disclaimer })
  
  output$input_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(
      sliderInput("u_disc", L$engine_label, min = 1, max = 6, value = 3.5, step = 0.5, ticks = FALSE),
      sliderInput("u_stab", L$brake_label, min = 1, max = 6, value = 3.5, step = 0.5, ticks = FALSE)
    )
  })
  
  output$report_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(card_header(L$status_title), strong(textOutput("diag_text")))
  })
  
  output$distPlot <- renderPlot({
    L <- i18n[[input$lang]]
    
    ggplot(soul_data, aes(x = Discipline, y = Stability)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
      scale_fill_viridis_c(option = "plasma") +
      geom_point(aes(x = input$u_disc, y = input$u_stab), color = "#2ecc71", size = 8, shape = 18) +
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

# ==============================================================================
# 終極核心：必須是檔案最後一行，確保系統能識別 App 物件
# ==============================================================================
shinyApp(ui = ui, server = server)