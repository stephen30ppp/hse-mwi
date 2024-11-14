# R/modules/map_module.R

library(shiny)
library(leaflet)

# 定义地图模块的 UI 函数
mapModuleUI <- function(id) {
  ns <- NS(id)  # 创建命名空间
  tagList(
    # 您可以在这里添加其他 UI 元素，例如图例等
    uiOutput(ns("map_legend")),
    br(),
    leafletOutput(ns("map"), height = 850)
  )
}

# 定义地图模块的服务器函数
mapModule <- function(input, output, session, data, fill_var, idx, ol, is_all, focus_info) {
  ns <- session$ns  # 获取命名空间

  # 渲染地图
  output$map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      us_proxy <- plot_map(
        fill = fill_var,
        gd_map = data,
        idx = idx,
        ol = ol,
        is_all = is_all
      )

      if (focus_info$hl) {
        # 添加高亮显示的多边形
        us_proxy <- plot_map(
          fill = fill_var,
          gd_map = data,
          idx = idx,
          ol = ol,
          is_all = is_all,
          add_poly = TRUE,
          us_proxy = us_proxy,
          zcta_choose = focus_info$ZCTA
        )
      }

      us_proxy
    })
  })

  # 渲染地图图例（如果需要）
  output$map_legend <- renderUI({
    withProgress(message = "Rendering map legend", {
      HTML(paste0(
        "<center>",
        paste(
          sapply(1:length(meas_max_colors), function(x){
            if (names(meas_max_colors[x]) != "Mental Wellness Index"){
              paste0("<font color = ", meas_max_colors[x], " size = '3'><b>", 
                     names(meas_max_colors[x]), "</font></b>")
            } else {
              paste(
                sapply(1:nchar(names(meas_max_colors[x])), function(y){
                  bchar <- strsplit(names(meas_max_colors[x]), "")[[1]][y]
                  paste0(
                    "<font color = ", 
                    meas_colors_pal$`Mental Wellness Index`(nchar("Mental Wellness Index"))[y],
                    " size = '3'><b>", 
                    bchar, "</font></b>")
                }),
                collapse = ""
              )
            }
          }),
          collapse = "<font size = 3'><b> | </b></font>"
        ),
        "</center>"
      ))
    })
  })
}
