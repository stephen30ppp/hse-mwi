# R/modules/map_module.R

library(shiny)
library(leaflet)

# 定义地图模块的 UI 函数
mapModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("map_legend")),
    br(),
    leafletOutput(ns("map"), height = 850)
  )
}

# 定义地图模块的服务器函数
mapModule <- function(input, output, session, data, fill_var, idx, ol, is_all, focus_info) {
  ns <- session$ns

  output$map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      us_proxy <- plot_map(
        fill = fill_var(),
        geodat = data(),
        idx = idx(),
        ol = ol,
        is_all = is_all()
      )

      if (focus_info$hl) {
        # 添加高亮显示的多边形
        us_proxy <- plot_map(
          fill = fill_var(),
          geodat = data(),
          idx = idx(),
          ol = ol,
          is_all = is_all(),
          add_poly = TRUE,
          us_proxy = us_proxy,
          zcta_choose = focus_info$ZCTA
        )
      }

      us_proxy
    })
  })

  output$map_legend <- renderUI({
    withProgress(message = "Rendering map legend", {
      # 生成地图图例的 HTML 内容
      # ... 根据您的实际情况填充
      HTML("<center><b>Map Legend</b></center>")
    })
  })
}
