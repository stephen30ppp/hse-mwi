# server.R

library(shiny)
library(leaflet)
library(plotly)
library(DT)

# 加载模块文件
source("R/modules/map_module.R")
source("R/modules/plot_module.R")

server <- function(input, output, session) {
  # 初始化反应式值
  focus_info <- reactiveValues(
    hl = FALSE,
    ZCTA = ""
  )
  
  st_sub <- reactiveValues(
    idx = "pop",
    st = "Virginia",
    geodat = geodat[["pop"]][geodat[["pop"]]$STATE_NAME == "Virginia", ],
    mwi = mwi[["pop"]][mwi[["pop"]]$STATE_NAME == "Virginia", ],
    us_map_fill = "Mental_Wellness_Index",
    is_all = FALSE
  )
  
  # 观察输入的变化，更新反应式值
  observeEvent(input$st_focus, {
    idx <- st_sub$idx
    st_sub$st <- input$st_focus
    if (input$st_focus == "All") {
      st_sub$geodat <- geopts[[idx]]
      st_sub$mwi <- mwi[[idx]]
      st_sub$is_all <- TRUE
    } else {
      st_sub$geodat <- geodat[[idx]][geodat[[idx]]$STATE_NAME == input$st_focus, ]
      st_sub$mwi <- mwi[[idx]][mwi[[idx]]$STATE_NAME == input$st_focus, ]
      st_sub$is_all <- FALSE
    }
  })
  
  # 调用地图模块
  callModule(
    module = mapModule,
    id = "us_map_module",
    data = reactive(st_sub$geodat),
    fill_var = reactive(st_sub$us_map_fill),
    idx = reactive(st_sub$idx),
    ol = list(
      measure_to_names = measure_to_names,
      meas_col_to_type = meas_col_to_type,
      meas_colors_pal = meas_colors_pal,
      # ... 其他必要的数据
    ),
    is_all = reactive(st_sub$is_all),
    focus_info = focus_info
  )
  
  # 调用绘图模块
  callModule(
    module = plotModule,
    id = "us_distr_module",
    fill = reactive(st_sub$us_map_fill),
    st = reactive(st_sub$st),
    mwi = reactive(st_sub$mwi),
    idx = reactive(st_sub$idx),
    ol = list(
      measure_to_names = measure_to_names,
      # ... 其他必要的数据
    ),
    is_all = reactive(st_sub$is_all),
    hl = reactive(focus_info$hl),
    zcta_hl = reactive(focus_info$ZCTA)
  )
  
  # ... 其他服务器逻辑（如处理用户交互、数据上传等）
}
