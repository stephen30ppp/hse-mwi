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

options(shiny.maxRequestSize = 300 * 1024^2)

# 如果有其他全局变量（例如 show_mitre, data_folder, index_types, territories 等等）也可在此定义
# 例如：
show_mitre <- TRUE  # 假设在 original code 里是从 app_config.R 读取，这里可直接设定或者 source("app_config.R")

data_folder <- file.path("Data")

index_types <- c("Population" = "pop", "Black" = "black")
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")

# 如果还有更多的全局配置，也可以放在这里
