# R/modules/plot_module.R

library(shiny)
library(plotly)
library(ggplot2)
library(ggbeeswarm)

# 定义绘图模块的 UI 函数
plotModuleUI <- function(id) {
  ns <- NS(id)  # 创建命名空间
  tagList(
    uiOutput(ns("distr_title")),
    plotlyOutput(ns("plot"), height = 400)
  )
}

# 定义绘图模块的服务器函数
plotModule <- function(input, output, session, fill, st, mwi, idx, ol, is_all, hl, zcta_hl) {
  ns <- session$ns  # 获取命名空间

  # 渲染分布图标题
  output$distr_title <- renderUI({
    HTML(paste0(
      "<b><center><font size = '3'>",
      "Distribution of ", ol$measure_to_names[[idx]][fill],
      " for ",
      ifelse(idx != "black", "the ", ""),
      ifelse(idx == "black", "Black ", "Overall "),
      "Population",
      ifelse(idx == "black", "s", ""),
      " in ",
      st,
      "</b></center></font>"
    ))
  })

  # 渲染分布图
  output$plot <- renderPlotly({
    withProgress(message = "Rendering data distribution", {
      plot_bee_distr(
        fill = fill,
        st = st,
        mwi = mwi,
        idx = idx,
        ol = ol,
        is_all = is_all,
        hl = hl,
        zcta_hl = zcta_hl
      )
    })
  })
}
