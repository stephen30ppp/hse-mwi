# file: R/server.R
# -----------------------------------------------------------------------------
# 主 UI 与 主 server
# -----------------------------------------------------------------------------

# 1) main_ui ----
# 这里我们把几个模块的 UI 按顺序放入 navbarPage, 并返回 main_ui
main_ui <- navbarPage(
  collapsible = TRUE,
  
  title = div(
    if (show_mitre) {
      tagList(
        a(
          href = "https://www.mitre.org/",
          img(src = "media/MITRE_logo.png", height = "30"),
          target = "blank"
        ),
        "Mental Wellness Index™ Tool"
      )
    } else {
      div("Mental Wellness Index™ Tool", style = "padding-top:5px")
    }
  ),
  
  theme = "stylesheets/app.css",
  
  # -- (A) Explore States
  mod_explore_states_ui("explore_states"),
  
  # -- (B) Explore ZIP Codes
  mod_explore_zipcodes_ui("explore_zipcodes"),
  
  # -- (C) Create Your Own MWI
  mod_create_own_mwi_ui("create_own_mwi"),
  
  # -- (D) MWI Toolkit
  mod_mwi_toolkit_ui("mwi_toolkit"),
  
  # 可选：底部copyright
  if (show_mitre){
    HTML(paste0(
      "<span class='copyright-footer'>&copy; ",
      format(Sys.Date(), "%Y"),
      ", The MITRE Corporation</span>"
    ))
  }
)


# 2) main_srv ----
# 组装各个模块的 server
main_srv <- function(input, output, session){
  
  # 这里可创建 reactiveValues() 以存放预处理数据(ol)
  # 或者直接使用 overall (已在 data_preprocessing.R 中读出)
  
  ol <- reactiveValues()
  for(nm in names(overall)){
    ol[[nm]] <- overall[[nm]]
  }
  
  # 调用各模块
  mod_explore_states_server("explore_states", ol)
  mod_explore_zipcodes_server("explore_zipcodes", ol)
  mod_create_own_mwi_server("create_own_mwi", ol)
  mod_mwi_toolkit_server("mwi_toolkit")
  
  # 如果有开屏 modal，可在这里写
  welcome_modal <- modalDialog(
    title = HTML("<b><center>Welcome to the Mental Wellness Index™!</center></b>"),
    size = "l",
    fluidRow(
      column(width = 1),
      column(width = 10,
        HTML("<p align='center'><font size='3'>"),
        HTML("The <b>Mental Wellness Index (MWI)</b> ..."),
        HTML("</p></font>"),
        HTML("<center>"),
        img(src = file.path("media", "MWI Framework (Transparent Background).png"), width = "60%"),
        HTML("</center>"),
        HTML("<br>"),
        HTML("<center>To learn more and view MWI videos, click <b>MWI Toolkit</b> in the top bar.</center>"),
        HTML("<center><font size='2'><i>Notes: This application is best viewed on a desktop ...</i></font></center>")
      )
    ),
    footer = tagList(
      HTML("<center>"),
      modalButton("Start Exploring!"),
      HTML("</center>")
    ),
    easyClose = TRUE
  )
  
  showModal(welcome_modal)
}

# END of server.R
