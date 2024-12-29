# file: R/mod_mwi_toolkit.R
# -----------------------------------------------------------------------------
# 模块化 MWI Toolkit 页面
# -----------------------------------------------------------------------------

mod_mwi_toolkit_ui <- function(id){
  ns <- NS(id)
  
  # 这里原先在app.R里是 navbarMenu("MWI Toolkit", ...)
  # 也可直接写成一个navbarMenu，也可把 toolkit_order 里的每个生成一个tabPanel
  # 此处演示直接返回 navbarMenu:
  
  navbarMenu(
    "MWI Toolkit",
    lapply(
      mwi_toolkit_order,
      function(x){
        tabPanel(
          id = tolower(x),
          title = div(gsub("_", " ", x), class = "about"),
          htmltools::tags$iframe(
            src = paste0("mwi-toolkit/", x, ".html"),
            class = "about-panel",
            frameborder = 0,
            scrolling = "auto"
          )
        )
      }
    )
  )
}

mod_mwi_toolkit_server <- function(id){
  moduleServer(id, function(input, output, session){
    # 这里几乎无需任何 server 逻辑
  })
}
