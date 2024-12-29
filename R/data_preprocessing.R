# R/data_preprocessing.R

# 加载必要的包
library(readxl)
library(dplyr)
library(sf)
library(stringr)

# 定义数据预处理函数
load_and_preprocess_data <- function(data_folder) {
  # 加载 measure registry
  m_reg <- read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1) %>%
    filter(!is.na(Numerator)) %>%
    column_to_rownames("Numerator")
  
  # 加载其他数据，例如 info_dat、mwi、geodat、geopts 等
  # 这里需要根据您的数据实际情况进行调整
  
  # 示例：加载 info_dat
  info_dat <- read.csv(file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"), row.names = 1)
  
  # 加载 mwi 数据
  # 假设有两个类型：pop 和 black
  mwi <- list()
  mwi[["pop"]] <- read.csv(file.path(data_folder, "Cleaned", "MWI_pop.csv"))
  mwi[["black"]] <- read.csv(file.path(data_folder, "Cleaned", "MWI_black.csv"))
  
  # 加载地理数据
  geodat <- list()
  geopts <- list()
  
  # 加载地理数据，例如 ZCTA shapefile
  zcta_shapefile <- st_read(file.path(data_folder, "Shapefiles", "ZCTA.shp"))
  geodat[["pop"]] <- zcta_shapefile
  geodat[["black"]] <- zcta_shapefile
  
  # 计算地理中心点
  geopts[["pop"]] <- st_centroid(geodat[["pop"]])
  geopts[["black"]] <- st_centroid(geodat[["black"]])
  
  # 生成 measure_to_names、meas_col_to_type、meas_colors_pal 等
  # 这些需要根据您的实际情况进行计算
  # 示例：
  measure_to_names <- list()
  measure_to_names[["pop"]] <- setNames(m_reg$Measure, rownames(m_reg))
  measure_to_names[["black"]] <- setNames(m_reg$Measure, rownames(m_reg))
  
  # 返回预处理后的数据列表
  list(
    m_reg = m_reg,
    info_dat = info_dat,
    mwi = mwi,
    geodat = geodat,
    geopts = geopts,
    measure_to_names = measure_to_names,
    # ... 其他数据
  )
}
