# file: R/server.R
# -----------------------------------------------------------------------------
# 主 UI 与 主 server
# -----------------------------------------------------------------------------

# 如果你需要 data_folder, index_types, show_mitre, overall 等
# 请在 global.R 里定义，并在 app.R 中先 source("global.R")

# 1) main_ui ----
main_ui <- navbarPage(
  title = "Mental Wellness Index Tool",
  collapsible = TRUE,
  theme = "stylesheets/app.css",

  # (A) Explore States
  tabPanel(
    "Explore States",
    mod_explore_states_ui("explore_states") # 这里同理: 里面是 UI
  ),

  # (B) Explore ZIP Codes
  tabPanel(
    "Explore ZIP Codes",
    mod_explore_zipcodes_ui("explore_zipcodes") # 这里嵌入
  ),

  # (C) Create Your Own MWI
  tabPanel(
    "Create Your Own MWI",
    mod_create_own_mwi_ui("create_own_mwi")
  ),

  # (D) MWI Toolkit
  tabPanel(
    "MWI Toolkit",
    mod_mwi_toolkit_ui("mwi_toolkit")
  ),
  footer = if (show_mitre) {
    HTML(paste0(
      "<span class='copyright-footer'>&copy; ",
      format(Sys.Date(), "%Y"),
      ", The MITRE Corporation</span>"
    ))
  }
)

# 2) main_srv ----
main_srv <- function(input, output, session) {
  # 创建 reactiveValues 用来存放 overall 的数据
  ol <- reactiveValues(
    geodat = data.frame(
      state_name = c("Virginia", "California"),
      longitude = c(-78.6569, -119.4179),
      latitude = c(37.4316, 36.7783)
    ), # 确保 `overall$geodat` 是数据框或列表

    mwi = overall$mwi
  )





  # 假设 'overall' 已在 data_preprocessing.R 里定义为全局
  # 也可在 global.R 里定义
  validate(
    need(exists("overall"), "Global data 'overall' is missing or not loaded.")
  )

  for (nm in names(overall)) {
    ol[[nm]] <- overall[[nm]]
  }

  # 调用各模块 server
  mod_explore_states_server("explore_states", ol)
  mod_explore_zipcodes_server("explore_zipcodes", ol)
  mod_create_own_mwi_server("create_own_mwi", ol)
  mod_mwi_toolkit_server("mwi_toolkit")

  # 可选：欢迎弹窗
  welcome_modal <- modalDialog(
    title = HTML("<b><center>Welcome to the Mental Wellness Index™!</center></b>"),
    size = "l",
    fluidRow(
      column(
        width = 10, offset = 1,
        HTML("<p align='center'><font size='3'>"),
        HTML("The <b>Mental Wellness Index (MWI)</b> ..."),
        HTML("</p></font>"),
        HTML("<center>"),
        img(src = file.path("media", "MWI Framework (Transparent Background).png"), width = "60%"),
        HTML("</center>"),
        HTML("<br>"),
        HTML("<center>To learn more, click <b>MWI Toolkit</b> above.</center>"),
        HTML("<center><font size='2'><i>Notes: best viewed on a desktop ...</i></font></center>")
      )
    ),
    footer = tagList(modalButton("Start Exploring!")),
    easyClose = TRUE
  )

  showModal(welcome_modal)
}

# END of server.R
