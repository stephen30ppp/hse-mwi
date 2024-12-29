# file: app.R

library(shiny)

# 从全局脚本加载包和关键数据/配置
source("global.R")

# 从 R/ 文件夹加载所有功能性脚本：
source("R/data_preprocessing.R")   # 数据预处理相关函数
source("R/plot_functions.R")       # 绘图/可视化相关函数
source("R/mod_explore_states.R")   # Explore States 模块
source("R/mod_explore_zipcodes.R") # Explore ZIP Codes 模块
source("R/mod_create_own_mwi.R")   # Create Your Own MWI 模块
source("R/mod_mwi_toolkit.R")      # MWI Toolkit 模块
source("R/server.R")               # 主 server 逻辑

# 注意：mod_mwi_toolkit.R 可以只是单纯封装若干 UI 面板

# 最终运行 Shiny 应用：
shinyApp(
  ui = main_ui,      # 在 server.R 或 mod_* 中我们会定义好 main_ui
  server = main_srv  # 在 server.R 定义好的 server 逻辑
)
