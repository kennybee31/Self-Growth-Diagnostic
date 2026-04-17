# ==============================================================================
# 專案名稱：自我成長與壓力檢測 (Growth & Stress Diagnostic) - 最終上線版
# 數據來源：SAPA Project (N=2,800 真實受訪者)
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(psychTools)

# 1. 數據血統精算 (N=2,800)
data(bfi)
soul_data <- bfi[, c("C1", "C2", "C3", "C4", "C5", "N1", "N2", "N3", "N4", "N5")]
soul_data <- na.omit(soul_data)

# 計算特質均分 (Discipline: 執行引擎; Stability: 情緒冷卻系統)
soul_data$Discipline <- rowMeans(soul_data[, 1:5])
soul_data$Stability  <- 7 - rowMeans(soul_data[, 6:10])

# 2. 雙語字典與合規聲明
i18n <- list(
  en = list(
    title = "Growth & Stress Diagnostic",
    disclaimer = "⚠️ DISCLAIMER: For reference only. Not a clinical diagnosis.",
    engine_label = "Discipline Intensity (The Engine)",
    brake_label = "Emotional Stability (The Brake)",
    scale_info = "<div style='font-size: 0.85em; color: #7f8c8d; margin-top: 10px; line-height: 1.5;'>
                  <b>Scale Definitions (No Neutral Zone):</b><br>
                  1 = Totally Inaccurate | 2 = Moderately Inaccurate | 3 = Slightly Inaccurate (Relaxed)<br>
                  4 = Slightly Accurate (Disciplined) | 5 = Moderately Accurate | 6 = Totally Accurate (Extreme)
                  </div>",
    map_title = "Global Sample Distribution (N=2,800)",
    burnout_text = "Burnout Zone",
    status_title = "Diagnostic Report",
    safe = "✅ STATUS: Balanced Growth. System thermal regulation is optimal.",
    fragile = "⚠️ WARNING: Fragility Peak. Engine redlining without sufficient cooling.",
    holy_grail = "🌟 EXCELLENCE: The Holy Grail. High output with perfect resilience."
  ),
  zh = list(
    title = "自我成長與壓力檢測",
    disclaimer = "⚠️ 法律聲明：本結果基於統計趨勢，僅供參考，不具臨床診斷效力。",
    engine_label = "執行力強度 (自律引擎)",
    brake_label = "穩定性強度 (冷卻系統)",
    scale_info = "<div style='font-size: 0.85em; color: #7f8c8d; margin-top: 10px; line-height: 1.5;'>
                  <b>1-6 分刻度定義 (無中立選項)：</b><br>
                  1分 = 極度不符合 | 2分 = 中度不符合 | 3分 = 輕度不符合 (偏向隨性/脆弱)<br>
                  4分 = 輕度符合 (偏向自律/穩定) | 5分 = 中度符合 | 6分 = 極度符合 (極致榨取/防禦)
                  </div>",
    map_title = "全球 2,800 筆真實樣本分佈圖",
    burnout_text = "高壓燒毀區",
    status_title = "診斷報告",
    safe = "✅ 狀態：平衡成長。你目前的散熱系統運作良好。",
    fragile = "⚠️ 警告：玻璃強者。引擎轉速極高但缺乏冷卻，隨時可能燒毀。",
    holy_grail = "🌟 卓越：聖杯象限。具備極高產出且情緒防線穩固。"
  )
)

# 3. UI 介面設計
ui <- page_fixed(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # 標題與語系切換
  div(style = "padding: 15px; background: #2c3e50; color: white; border-radius: 0 0 15px 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
      selectInput("lang", NULL, choices = c("繁體中文" = "zh", "English" = "en"), width = "120px"),
      uiOutput("header_ui")
  ),
  
  # 警示語
  div(style = "padding: 10px; color: #e74c3c; font-size: 0.82rem; font-weight: bold; background: #fff5f5; border: 1px solid #ffcccc; margin: 15px 0; border-radius: 8px;",
      uiOutput("disclaimer_ui")
  ),
  
  layout_column_wrap(
    width = 1,
    # 輸入區 (級距為 1，並包含刻度說明)
    card(
      uiOutput("input_ui"),
      uiOutput("scale_explanation")
    ),
    
    # 圖表顯示區
    card(
      plotOutput("distPlot", height = "320px")
    ),
    
    # 診斷報告區
    card(
      uiOutput("report_ui")
    )
  )
)

# 4. Server 邏輯處理
server <- function(input, output, session) {
  
  # 動態文字綁定
  output$header_ui <- renderUI({ h3(i18n[[input$lang]]$title, style = "margin: 0; font-weight: bold;") })
  output$disclaimer_ui <- renderUI({ i18n[[input$lang]]$disclaimer })
  output$scale_explanation <- renderUI({ HTML(i18n[[input$lang]]$scale_info) })
  
  # 刻度 1-6 之滑桿 (Step 改為 1，並顯示 Ticks 刻度線)
  output$input_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(
      sliderInput("u_disc", L$engine_label, min = 1, max = 6, value = 4, step = 1, ticks = TRUE),
      sliderInput("u_stab", L$brake_label, min = 1, max = 6, value = 4, step = 1, ticks = TRUE)
    )
  })
  
  # 診斷報告標題與內文
  output$report_ui <- renderUI({
    L <- i18n[[input$lang]]
    tagList(card_header(L$status_title), strong(textOutput("diag_text")))
  })
  
  # 圖表繪製：熱力圖與落點標示
  output$distPlot <- renderPlot({
    L <- i18n[[input$lang]]
    
    ggplot(soul_data, aes(x = Discipline, y = Stability)) +
      # 2D 核密度估計圖
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
      scale_fill_gradientn(colors = c("#0d0887", "#cc4678", "#f0f921")) +
      
      # 使用者即時落點 (綠色菱形)
      geom_point(aes(x = input$u_disc, y = input$u_stab), color = "#00FF00", size = 8, shape = 18) +
      
      # 高壓燒毀區：紅色虛線方框與標註文字
      ggplot2::annotate("rect", xmin = 4.5, xmax = 6, ymin = 1, ymax = 2.5, 
                        alpha = 0.2, fill = "white", color = "red", linetype = "dashed") +
      ggplot2::annotate("text", x = 5.25, y = 1.75, label = L$burnout_text, 
                        color = "red", fontface = "bold", size = 5) +
      
      theme_minimal() +
      labs(title = L$map_title, x = NULL, y = NULL) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })
  
  # 診斷報告文字邏輯
  output$diag_text <- renderText({
    L <- i18n[[input$lang]]
    if (input$u_disc > 4.5 & input$u_stab < 2.5) return(L$fragile)
    if (input$u_disc > 4.5 & input$u_stab > 4.5) return(L$holy_grail)
    return(L$safe)
  })
}

# ==============================================================================
# 啟動命令
# ==============================================================================
shinyApp(ui = ui, server = server)