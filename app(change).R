# file: app.R
library(shiny)
source("D:\\software\\hse-mwi-main\\global.R")
# library(shiny)
# file: global.R

# 全局选项、库加载 ----
library(readxl)
library(writexl)
library(htmltools)
library(shiny)
library(tigris)
library(leaflet)
library(RColorBrewer)
library(sf)
library(plotly)
library(ggbeeswarm)
library(shinyWidgets)
library(sass)
library(shinycssloaders)
library(shinyBS)
library(DT)
library(dplyr)
library(tidycensus)
options(shiny.maxRequestSize = 300 * 1024^2)

# 如果有其他全局变量（例如 show_mitre, data_folder, index_types, territories 等等）也可在此定义
# 例如：
show_mitre <<- TRUE # 假设在 original code 里是从 app_config.R 读取，这里可直接设定或者 source("app_config.R")

data_folder <<- "D:/software/hse-mwi-main/Data"

index_types <<- c("Population" = "pop", "Black" = "black")
territories <<- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
cat("data_folder:", data_folder, "\n")
# 如果还有更多的全局配置，也可以放在这里

# 从全局脚本加载包和关键数据/配置
setwd("D:/software/hse-mwi-main")
source("D:/software/hse-mwi-main/global.R")


# 从 R/ 文件夹加载所有功能性脚本：
source("D:/software/hse-mwi-main/R/data_preprocessing.R") # 数据预处理相关函数
source("D:/software/hse-mwi-main/R/plot_functions.R") # 绘图/可视化相关函数
source("D:/software/hse-mwi-main/R/mod_explore_states.R") # Explore States 模块
source("D:/software/hse-mwi-main/R/mod_explore_zipcodes.R") # Explore ZIP Codes 模块
source("D:/software/hse-mwi-main/R/mod_create_own_mwi.R") # Create Your Own MWI 模块
source("D:/software/hse-mwi-main/R/mod_mwi_toolkit.R") # MWI Toolkit 模块
source("D:/software/hse-mwi-main/R/server.R") # 主 server 逻辑

# 注意：mod_mwi_toolkit.R 可以只是单纯封装若干 UI 面板

# 最终运行 Shiny 应用：
shinyApp(
  ui = main_ui, # 在 server.R 或 mod_* 中我们会定义好 main_ui
  server = main_srv # 在 server.R 定义好的 server 逻辑
)
