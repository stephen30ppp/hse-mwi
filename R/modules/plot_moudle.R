# R/modules/plot_module.R

library(shiny)
library(plotly)
library(ggplot2)
library(ggbeeswarm)

# define the drawing moudle UI
plotModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot"), height = 400)
}

# 定义绘图模块的服务器逻辑define the service logic for the drawing moudle
plotModule <- function(input, output, session, data, fill_var, additional_params = list()) {
  output$plot <- renderPlotly({
    # 使用传入的数据和填充变量绘制分布图
    plot_bee_distr(fill_var, data, additional_params)
  })

  # 其他绘图相关的反应逻辑可以在这里添加
}
