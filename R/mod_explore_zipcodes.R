# file: R/mod_explore_zipcodes.R
# -----------------------------------------------------------------------------
# 模块化 Explore ZIP Codes 页面及其服务器逻辑
# -----------------------------------------------------------------------------

# 如果你在 global.R 中已经定义 data_folder、index_types、overall 等，
# 可不必在此再次定义；若需特定常量，可在此声明。

mod_explore_zipcodes_ui <- function(id){
  ns <- NS(id)
  
  # 这里返回一个 tabPanel(...)，以便在 server.R 里可以：
  #   tabPanel("Explore ZIP Codes", mod_explore_zipcodes_ui("explore_zipcodes"))
  
  # 也可以直接返回 fluidPage(...)，然后在 server.R 做：
  #   tabPanel("Explore ZIP Codes", ... ) # 里嵌mod_explore_zipcodes_ui
  
  # 本示例直接写成可嵌入 tabPanel 方式
  tagList(  # 使用 tagList 让外层 tabPanel() 包含更多布局
    sidebarLayout(
      sidebarPanel(
        width = 3,
        bsCollapse(
          multiple = TRUE,
          open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),
          
          # 1) Exploration Options ----
          bsCollapsePanel(
            "Exploration Options",
            HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> Enter a ZIP Code below to get started.<p>"),
            
            # Which population
            radioButtons(
              inputId = ns("idx_type_com"),
              label = "Which population's MWI do you want to view?",
              choiceValues = unname(index_types),
              choiceNames = c("Overall", "Black"),
              inline = TRUE
            ),
            
            # measure selection
            selectInput(
              ns("com_map_fill"),
              "What would you like to explore?",
              choices = overall$avail_meas_list[["pop"]]
            ),
            
            # ZIP code input
            textInput(
              ns("zip_choose_com"),
              label = "Which ZIP Code would you like to focus on?",
              placeholder = "e.g. 35004, 00501, 20041, etc."
            )
          ),
          
          # 2) Custom MWI Upload ----
          bsCollapsePanel(
            "Custom MWI Upload",
            tagList(
              HTML("<font size = '2'><p>"),
              "To create the necessary custom MWI file, see the 'Create Your Own MWI' tab. Data is not saved after you leave the page.",
              HTML("</p></font>"),
              
              fileInput(
                ns("custom_data_com"),
                label = "Upload Custom MWI (.RData)",
                accept = ".RData"
              ),
              actionButton(ns("custom_data_load_com"), "Run Custom MWI"),
              actionButton(ns("custom_data_reset_com"), "Reset")
            )
          ),
          
          # 3) About the Mental Wellness Index ----
          bsCollapsePanel(
            "About the Mental Wellness Index",
            HTML("<center>"),
            img(
              src = file.path("media", "MWI Framework (Transparent Background).png"),
              align = "center",
              width = "90%"
            ),
            HTML("</center>"),
            HTML("<font size = '2'>"),
            HTML("The MWI is the weighted sum of 28 measure values... See 'MWI Toolkit' for more info."),
            HTML("</font>")
          )
        )
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          # (A) Explore ZCTA Maps
          tabPanel(
            "Explore ZCTA Maps",
            fluidRow(
              column(
                width = 8,
                uiOutput(ns("com_map_legend")),
                HTML("<br>"),
                withSpinner(
                  leafletOutput(ns("com_map"), height = 850),
                  type = 8, color = "#005B94", hide.ui = FALSE
                )
              ),
              column(
                width = 4,
                bsCollapse(
                  multiple = TRUE,
                  open = c("ZCTA Measure Results", "Selected Measure Interpretation", "About Selected Measure"),
                  
                  bsCollapsePanel(
                    "Selected Measure Interpretation",
                    uiOutput(ns("com_map_expl"))
                  ),
                  bsCollapsePanel(
                    "About Selected Measure",
                    uiOutput(ns("data_info_com")),
                    HTML("<font size='2'>For more info on data, see 'MWI Toolkit'.</font>")
                  ),
                  bsCollapsePanel(
                    "ZCTA Measure Results",
                    uiOutput(ns("com_map_report_card"))
                  )
                )
              )
            )
          ),
          
          # (B) Explore ZCTA Measures
          tabPanel(
            "Explore ZCTA Measures",
            bsCollapse(
              open = c("ZCTA Measure Results"),
              bsCollapsePanel(
                "ZCTA Measure Results",
                HTML("<p><i>Measures have ranks from 0 to 100...</i></p>"),
                uiOutput(ns("com_report_card_table_mwi")),
                HTML("<p></p>"),
                DTOutput(ns("com_report_card_table"))
              )
            )
          )
        )
      )
    )
  )
}


mod_explore_zipcodes_server <- function(id, ol){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ---------------------------------------------------------------
    # ReactiveValues for community data
    # ---------------------------------------------------------------
    com_sub <- reactiveValues(
      idx = "pop",
      ZCTA = "23936",
      
      # 初始化 geodat 和 mwi (based on overall)
      geodat = NULL,
      mwi = NULL,
      com_map_fill = "Mental_Wellness_Index"
    )
    
    # 初始化: 把默认 geodat, mwi 放到 observe() or isolate()
    observeEvent(TRUE, {
      isolate({
        # 需要从 ol$geopts$pop 中根据 ZCTA="23936" 计算 geodat
        # 并赋给 com_sub$geodat, com_sub$mwi
        # 例如:
        if (!is.null(ol$geopts$pop)) {
          zcta_coord <- st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936", ])
          all_coord <- st_coordinates(ol$geopts$pop)
          zcta_log <-
            all_coord[,1] >= zcta_coord[1] - 1 &
            all_coord[,1] <= zcta_coord[1] + 1 &
            all_coord[,2] >= zcta_coord[2] - 1 &
            all_coord[,2] <= zcta_coord[2] + 1
          
          com_sub$geodat <- ol$geodat[["pop"]][zcta_log, ]
          com_sub$mwi <- ol$mwi[["pop"]][
            ol$mwi[["pop"]]$ZCTA %in% com_sub$geodat$GEOID,
          ]
        }
      })
    }, once = TRUE)
    
    # ---------------------------------------------------------------
    # 1) 监听 idx_type_com
    # ---------------------------------------------------------------
    observeEvent(input$idx_type_com, {
      idx <- input$idx_type_com
      # 修正 measure fill
      fill0 <- com_sub$com_map_fill
      if (idx == "pop" && grepl("*_black$", fill0)){
        fill0 <- gsub("*_black$", "_pop", fill0)
      } else if (
        idx == "black" &&
        grepl("*_pop$", fill0) &&
        !fill0 %in% colnames(ol$mwi[["black"]])
      ){
        fill0 <- gsub("*_pop$", "_black", fill0)
      }
      
      com_sub$idx <- idx
      com_sub$com_map_fill <- fill0
      
      updateSelectInput(
        session,
        "com_map_fill",
        "What would you like to explore?",
        choices = ol$avail_meas_list[[idx]],
        selected = fill0
      )
    })
    
    # 2) 监听 zip_choose_com
    observeEvent(input$zip_choose_com, {
      zip_in <- input$zip_choose_com
      if (!is.null(zip_in) &&
          nchar(zip_in) == 5 &&
          !grepl("\\D", zip_in) &&  # digits only
          zip_in %in% names(zip_to_zcta)
      ){
        com_sub$ZCTA <- unname(zip_to_zcta[zip_in])
      }
    })
    
    # 3) 当 idx 或 ZCTA 改变时 -> 更新 geodat, mwi
    observeEvent(list(com_sub$idx, com_sub$ZCTA), {
      idx <- com_sub$idx
      ZC <- com_sub$ZCTA
      
      req(idx, ZC)  # 确保存在
      req(ol$geopts[[idx]], ol$geodat[[idx]], ol$mwi[[idx]])
      
      # 计算 bounding
      zcta_coord <- st_coordinates(ol$geopts[[idx]][ ol$geopts[[idx]]$GEOID == ZC, ])
      all_coord <- st_coordinates(ol$geopts[[idx]])
      
      if (length(zcta_coord) == 0) {
        # 说明没找到ZCTA
        com_sub$geodat <- NULL
        com_sub$mwi <- NULL
        return()
      }
      
      zcta_log <-
        all_coord[,1] >= zcta_coord[1] - 1 &
        all_coord[,1] <= zcta_coord[1] + 1 &
        all_coord[,2] >= zcta_coord[2] - 1 &
        all_coord[,2] <= zcta_coord[2] + 1
      
      if (sum(zcta_log) <= 1) {
        # 扩大到 ±3
        zcta_log <-
          all_coord[,1] >= zcta_coord[1] - 3 &
          all_coord[,1] <= zcta_coord[1] + 3 &
          all_coord[,2] >= zcta_coord[2] - 3 &
          all_coord[,2] <= zcta_coord[2] + 3
      }
      
      # 更新 geodat
      sub_geo <- ol$geodat[[idx]][zcta_log, ]
      sub_mwi <- ol$mwi[[idx]][ ol$mwi[[idx]]$ZCTA %in% sub_geo$GEOID, ]
      
      com_sub$geodat <- sub_geo
      com_sub$mwi <- sub_mwi
    })
    
    # 4) 当 measure fill 改变
    observeEvent(input$com_map_fill, {
      fill0 <- input$com_map_fill
      # 修正 measure fill
      if (com_sub$idx == "pop" && grepl("*_black$", fill0)) {
        fill0 <- gsub("*_black$", "_pop", fill0)
      } else if (
        com_sub$idx == "black" &&
        grepl("*_pop$", fill0) &&
        !fill0 %in% colnames(ol$mwi[["black"]])
      ){
        fill0 <- gsub("*_pop$", "_black", fill0)
      }
      com_sub$com_map_fill <- fill0
    })
    
    # 5) output$com_map
    output$com_map <- renderLeaflet({
      req(com_sub$geodat, com_sub$mwi)
      plot_map(
        fill = com_sub$com_map_fill,
        geodat = com_sub$geodat,
        idx = com_sub$idx,
        ol = ol,
        is_all = FALSE,
        is_com = TRUE,
        zcta_choose = com_sub$ZCTA
      )
    })
    
    # 6) output$com_map_legend
    output$com_map_legend <- renderUI({
      # ...同上
      HTML("... your legend code ...")
    })
    
    # 7) output$com_map_expl
    output$com_map_expl <- renderUI({
      # ... 需req一下 com_sub$mwi ...
      req(com_sub$mwi)
      # 进行 measure 解释
      HTML("... your measure explanation ...")
    })
    
    # (其余 output$xxx 同理，全部写成 renderUI / renderPlotly / renderDataTable / observeEvent)
    
    # 9) Custom MWI Upload
    observeEvent(input$custom_data_load_com, {
      req(input$custom_data_com)
      load(input$custom_data_com$datapath)  # expects overall_output
      for (ov in names(ol)){
        ol[[ov]] <- overall_output[[ov]]
      }
      # 之后同理重算 geodat / geopts ...
    })
    
    observeEvent(input$custom_data_reset_com, {
      req(input$custom_data_com)
      for (ov in names(ol)) {
        ol[[ov]] <- overall[[ov]]
      }
      # reset com_sub
      com_sub$idx <- "pop"
      com_sub$ZCTA <- "23936"
      com_sub$com_map_fill <- "Mental_Wellness_Index"
    })
  })
}
