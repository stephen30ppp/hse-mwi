# app.R

library(shiny)

# 加载全局变量和函数
source("global.R")
source("ui.R")
source("server.R")

# 启动 Shiny 应用
shinyApp(ui = ui, server = server)
