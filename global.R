# global.R

# 加载必要的包
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(ggbeeswarm)

# 设置全局变量
data_folder <- "Data"
show_mitre <- TRUE

# 加载数据预处理脚本
source("R/data_preprocessing.R")

# 预处理数据
preprocessed_data <- load_and_preprocess_data(data_folder)

# 将预处理后的数据存储为全局变量
m_reg <- preprocessed_data$m_reg
info_dat <- preprocessed_data$info_dat
mwi <- preprocessed_data$mwi
geodat <- preprocessed_data$geodat
geopts <- preprocessed_data$geopts
measure_to_names <- preprocessed_data$measure_to_names
meas_col_to_type <- preprocessed_data$meas_col_to_type
meas_colors_pal <- preprocessed_data$meas_colors_pal
avail_measures <- preprocessed_data$avail_measures
avail_meas_list <- preprocessed_data$avail_meas_list
# ... 其他全局数据

# 加载自定义函数
source("R/utility_functions.R")
