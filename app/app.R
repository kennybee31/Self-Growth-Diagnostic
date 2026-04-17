# ==============================================================================
# 專案名稱：自我成長與壓力檢測 (Growth & Stress Diagnostic)
# 數據來源：SAPA Project (N=2,800 真實受訪者)
# 技術修復：補齊 MASS 與色彩依賴鏈，並優化 WASM 渲染穩定性
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(psychTools)
# 強制載入所有底層依賴，防止 WASM 橋接失效
library(MASS)       # 處理密度計算
library(scales)     # 處理座標縮放
library(munsell)    # 處理色彩空間
library(colorspace) # 處理顏色轉換

# 1. 數據準備 (N=2,800)
data(bfi)
soul_data <- bfi[, c("C1", "C2", "C3", "C4", "C5", "N1", "N2", "N3", "N4", "N5")]
soul_data <- na.omit(soul_data)

# 計算特質均分 (Discipline: 自律引擎; Stability: 穩定冷卻系統)
soul_data$Discipline <- rowMeans(soul_data[, 1:5])
soul_data$Stability  <- 7 - rowMeans(soul_data[, 6:10])

# 2. 雙語字典與合規聲明
i18n <- list(
  en = list(
    title = "Growth & Stress Diagnostic",
    disclaimer = "⚠️ DISCLAIMER: For reference only. Not a clinical diagnosis.",
    engine_label = "Discipline Intensity (The Engine)",
    brake_label = "Emotional Stability (The Brake)",
    scale_info = "1 = Totally Inaccurate | 6 = Totally Accurate",
    map_title = "Global Sample Distribution (N=2,800)",
    burnout_text = "Burnout Zone",
    status_title = "Diagnostic Report",
    safe = "✅ STATUS: Balanced Growth.",
    fragile = "⚠️ WARNING: Fragility Peak (Engine Overheat).",
    holy_grail = "🌟 EXCELLENCE: High Resilience & Output."
  ),
  zh = list(
    title = "自我成長與壓力檢測",
    disclaimer = "⚠️ 法律聲明：本結果僅供參考，不具臨床診斷效力。",
    engine_label = "執行力強度 (自律引擎)",
    brake_label = "穩定性強度 (冷卻系統)",
    scale_info = "1分 = 極度不符合 | 6分 = 極度符合",
    map_title = "全球 2,800 筆真實樣本分佈圖",
    burnout_text = "高壓燒毀區",
    status_title = "診斷報告",
    safe = "✅ 狀態：平衡成長。目前散熱系統運作良好。",
    fragile = "⚠️ 警告：玻璃強者。引擎轉速極高但缺乏冷卻。",
    holy_grail = "🌟 卓越：聖杯象限。高產出且情緒穩健。"
  )
)

# 3. UI 介面設計
ui <- page_fixed(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  div(style = "padding: 15px; background: #2c3e50; color: white; border-radius: 0 0 15px 15px;",
      selectInput("lang", NULL, choices = c("繁體中文" = "zh", "English" = "en"), width = "120px"),
      uiOutput("header_ui")
  ),
  div(style = "padding: 10px; color: #e74c3c; font-size: 0.82rem; background: #fff5f5; margin: 15px 0; border-radius: 8px;",
      uiOutput("disclaimer_ui")
  ),
  layout_column_wrap(
    width = 1,
    card(uiOutput("input_ui"), p(uiOutput("scale_explanation"), style="font-size:0.8em; color:gray;")),
    card(plotOutput("distPlot", height = "320px")),
    card(uiOutput("report_ui"))
  )
)

# 4. Server 邏輯處理
server <- function(input, output, session) {
  output$header_ui <- renderUI({ h3(i18n[[input$lang]]$title, style = "margin: 0; font-weight: bold;") })
  output$disclaimer_ui <- renderUI({ i18n[[input$lang]]$disclaimer })
  output$scale_explanation <- renderUI({ i18n[[input$lang]]$scale_info })
  output$input_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(
      sliderInput("u_disc", L$engine_label, min = 1, max = 6, value = 4, step = 1, ticks = TRUE),
      sliderInput("u_stab", L$brake_label, min = 1, max = 6, value = 4, step = 1, ticks = TRUE)
    )
  })
  output$report_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(card_header(L$status_title), strong(textOutput("diag_text")))
  })
  
  output$distPlot <- renderPlot({
    L <- i18n[[input$lang]]
    ggplot(soul_data, aes(x = Discipline, y = Stability)) +
      # 使用穩定性更高的 geom_bin2d 代替 stat_density_2d
      geom_bin2d(bins = 25) + 
      scale_fill_gradientn(colors = c("#0d0887", "#cc4678", "#f0f921")) +
      geom_point(aes(x = input$u_disc, y = input$u_stab), color = "#00FF00", size = 8, shape = 18) +
      ggplot2::annotate("rect", xmin = 4.5, xmax = 6, ymin = 1, ymax = 2.5, alpha = 0.2, fill = "white", color = "red", linetype = "dashed") +
      ggplot2::annotate("text", x = 5.25, y = 1.75, label = L$burnout_text, color = "red", fontface = "bold", size = 5) +
      theme_minimal() +
      labs(title = L$map_title, x = NULL, y = NULL) +
      theme(legend.position = "none")
  })
  
  output$diag_text <- renderText({
    L <- i18n[[input$lang]]
    if (input$u_disc > 4.5 & input$u_stab < 2.5) return(L$fragile)
    if (input$u_disc > 4.5 & input$u_stab > 4.5) return(L$holy_grail)
    return(L$safe)
  })
}

shinyApp(ui = ui, server = server)