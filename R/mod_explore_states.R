# 文件: R/mod_explore_states.R
# ------------------------------------------------------------------
# 模块: Explore States 页面
# ------------------------------------------------------------------

library(shinyBS)
library(leaflet)
library(plotly)

# UI 部分
mod_explore_states_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = div("Explore States", class = "explore"),
    class = "explore-panel",
    sidebarLayout(
      # --- 左侧侧边栏 ---
      sidebarPanel(
        width = 3,
        bsCollapse(
          multiple = TRUE,
          open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),

          # Exploration Options
          bsCollapsePanel(
            "Exploration Options",
            HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> Select any of the options below to get started."),
            radioButtons(
              inputId = ns("idx_type"),
              label = "Which population's MWI do you want to view?",
              choices = c("Overall", "Black"),
              inline = TRUE
            ),
            selectInput(
              inputId = ns("st_focus"),
              label = "Which state would you like to focus on?",
              choices = c("Virginia", "California", "All"),
              selected = "Virginia"
            ),
            selectInput(
              inputId = ns("us_map_fill"),
              label = "What would you like to explore?",
              choices = c("Mental Wellness Index", "Other Measure"),
              selected = "Mental Wellness Index"
            ),
            textInput(
              inputId = ns("zip_choose"),
              label = "Which ZIP Code would you like to focus on in the selected state?",
              placeholder = "e.g., 35004, 00501, 20041, etc."
            ),
            actionButton(
              inputId = ns("reset_zcta_click"),
              label = "Reset ZIP Code Focus"
            )
          ),

          # Custom MWI Upload
          bsCollapsePanel(
            "Custom MWI Upload",
            tagList(
              HTML("<font size='2'><p>To create the necessary custom Mental Wellness Index file, see the 'Create Your Own MWI' tab.</p></font>"),
              fileInput(
                inputId = ns("custom_data_st"),
                label = "Upload Custom Mental Wellness Index (.RData)",
                accept = ".RData"
              ),
              actionButton(ns("custom_data_load_st"), "Run Custom MWI"),
              actionButton(ns("custom_data_reset_st"), "Reset")
            )
          ),

          # About the Mental Wellness Index
          bsCollapsePanel(
            "About the Mental Wellness Index",
            HTML("<center><img src='media/MWI Framework (Transparent Background).png' width='90%'></center>"),
            HTML("<font size='2'>The Mental Wellness Index is the weighted sum of 28 measures that quantify facilitators and barriers to mental wellness.</font>")
          )
        )
      ),

      # --- 右侧主界面 ---
      mainPanel(
        width = 9,

        # 插入地图和右侧内容
        column(
          width = 8,
          uiOutput(ns("us_map_legend")),
          HTML("<br>"),
          withSpinner(
            leafletOutput(ns("us_map"), height = 850),
            type = 8, color = "#005B94", hide.ui = FALSE
          )
        ),
        column(
          width = 4,
          uiOutput(ns("us_distr_title")),
          withSpinner(
            plotlyOutput(ns("us_distr"), height = 400),
            type = 8, color = "#005B94", hide.ui = FALSE
          ),
          bsCollapse(
            multiple = TRUE,
            open = c("Measure Interpretation", "About Selected Measure"),

            # Measure Interpretation
            bsCollapsePanel(
              "Measure Interpretation",
              conditionalPanel(
                condition = paste0("!output['", ns("focus_on"), "']"),
                uiOutput(ns("us_map_expl"))
              ),
              conditionalPanel(
                condition = paste0("output['", ns("focus_on"), "']"),
                uiOutput(ns("us_info"))
              )
            ),

            # About Selected Measure
            bsCollapsePanel(
              "About Selected Measure",
              uiOutput(ns("data_info")),
              HTML("<font size='2'>For more information on data and overall methodology, see the 'MWI Toolkit' page.</font>")
            )
          )
        )
      )
    )
  )
}

# 服务器逻辑部分
mod_explore_states_server <- function(id, ol) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #  observe({
    #    validate(
    #      need(!is.null(ol$geodat$pop), "Global data 'geodat$pop' is missing")
    #    )
    #    ol$geodat$pop <- extract_coordinates(ol$geodat$pop)
    #    print("Coordinates extracted and added to geodat$pop:")
    #    print(head(ol$geodat$pop))
    #  })
    # 初始化 reactiveValues
    observe({
      if (is.null(input$st_focus)) {
        print("Initializing st_focus to default value 'Virginia'")
        updateSelectInput(session, "st_focus", selected = "Virginia")
      }
    })
    reactive_data <- reactiveValues(
      filtered_geodat = NULL
    )
    ol$geodat <- data.frame(
      state_name = c("Virginia", "California", "Texas"),
      longitude = c(-78.6569, -119.4179, -99.9018),
      latitude = c(37.4316, 36.7783, 31.9686)
    )


    # 监听用户选择的州并筛选数据
    # observeEvent(input$st_focus, {
    # validate(
    #  need(!is.null(ol$geodat), "Global data 'geodat' is missing"),
    #  need("state_name" %in% colnames(ol$geodat), "'state_name' column is missing in 'geodat'"),
    # need("longitude" %in% colnames(ol$geodat), "'longitude' column is missing in 'geodat'"),
    #  need("latitude" %in% colnames(ol$geodat), "'latitude' column is missing in 'geodat'")
    # )
    # state_selected <- input$st_focus
    # print(paste("Selected state:", state_selected))
    # if (is.data.frame(ol$geodat) && "state_name" %in% colnames(ol$geodat)) {
    # if (state_selected == "All") {
    #  reactive_data$filtered_geodat <- ol$geodat
    # } else {
    #  reactive_data$filtered_geodat <- ol$geodat[ol$geodat$state_name == state_selected, ]
    # }}
    # print(paste("Filtered geodat for state:", state_selected))
    # })
    # 监听用户选择的州并筛选数据
    observeEvent(input$st_focus, {
      state_selected <- input$st_focus
      print(paste("Selected state:", state_selected))

      # 检查 ol$geodat 数据结构
      print("Structure of ol$geodat:")
      print(str(ol$geodat))

      # 检查 state_name 列是否存在
      if (!"state_name" %in% colnames(ol$geodat)) {
        stop("'state_name' column is missing in 'ol$geodat'")
      }

      # 检查 state_name 列的唯一值
      print("Unique state names in ol$geodat:")
      print(unique(ol$geodat$state_name))

      # 数据过滤
      if (state_selected == "All") {
        reactive_data$filtered_geodat <- ol$geodat
      } else {
        reactive_data$filtered_geodat <- ol$geodat[ol$geodat$state_name == state_selected, ]
      }

      # 检查过滤后的数据
      print("Filtered data:")
      print(head(reactive_data$filtered_geodat))

      if (nrow(reactive_data$filtered_geodat) == 0) {
        print("Filtered data is empty!")
      }
    })


    # 输出地图
    output$us_map <- renderLeaflet({
      req(reactive_data$filtered_geodat)
      validate(
        need(!is.null(reactive_data$filtered_geodat) > 0, "No data available for map rendering")
      )
      print("Rendering map with filtered data:")
      print(head(reactive_data$filtered_geodat))
      leaflet(data = reactive_data$filtered_geodat) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude, # 经度列名
          lat = ~latitude, # 纬度列名
          label = ~state_name, # 显示的标签
          radius = 5,
          color = "blue",
          fillOpacity = 0.7
        )
    })

    # 输出右侧内容: 数据分布标题
    output$us_distr_title <- renderUI({
      HTML(paste0(
        "<b><center>Distribution of MWI for the State of ", input$st_focus, "</center></b>"
      ))
    })

    # 输出右侧内容: 数据分布图（示例）
    output$us_distr <- renderPlotly({
      validate(
        need(!is.null(reactive_data$filtered_geodat) > 0, "No data available for plotting")
      )
      plot_ly(
        data = reactive_data$filtered_geodat,
        x = ~state_name,
        y = ~latitude, # 示例变量
        type = "bar"
      )
    })
  })
}
