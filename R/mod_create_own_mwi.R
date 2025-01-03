# file: R/mod_create_own_mwi.R
# -----------------------------------------------------------------------------
# 模块化 Create Your Own MWI 页面及其服务器逻辑
# -----------------------------------------------------------------------------

# 如果需要 data_folder, 可在 global.R 或这里定义:
# data_folder <- "D:/software/hse-mwi-assignment1_YANGUO-XU/Data"

mod_create_own_mwi_ui <- function(id){
  ns <- NS(id)
  
  # 注意：此处不再使用 navbarMenu(...),
  # 而是返回单个 tabPanel("Create Your Own MWI", ...),
  # 在内部用 tabsetPanel 组织 3 个子页面
  tabPanel(
    title = "Create Your Own MWI",  # 这是在主 navbarPage 中的标签名
    
    # 这里用一个 tabsetPanel 来承载之前的 3 个子页面
    tabsetPanel(
      type = "tabs",  # 或 "pills"
      
      # 1) Adjust MWI Weights and ZIP Codes ----
      tabPanel(
        title = "Adjust MWI Weights and ZIP Codes",
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            HTML("<center><h2>Change weights and ZIP Codes ...</h2></center>"),
            HTML("<p align='justify'>... （省略若干介绍性文本，可完整粘贴）</p>"),
            hr(),
            HTML("<center>"),
            tabsetPanel(
              # (A) Adjust MWI Weights
              tabPanel(
                "Adjust MWI Weights",
                DTOutput(ns("custom_mwi_weights"))
              ),
              # (B) Subset Zip Codes/ZCTAs
              tabPanel(
                "Subset Zip Codes/ZCTAs",
                br(),
                textAreaInput(
                  ns("custom_mwi_zips"),
                  "Enter ZIP Codes or ZCTAs ...",
                  placeholder = "12345\n10034\n19567\n...",
                  height = "200px",
                  width = "400px"
                ),
                switchInput(
                  ns("custom_mwi_zip_choice"),
                  onLabel = "ZIP Code",
                  offLabel = "ZCTA",
                  value = TRUE,
                  onStatus = "secondary",
                  offStatus = "secondary"
                )
              )
            ),
            HTML("<br><br>"),
            actionButton(ns("custom_mwi_go_weights"), "Create Custom MWI"),
            downloadButton(ns("download_custom_mwi_weights"), "Download Custom MWI"),
            HTML("<br><br>"),
            verbatimTextOutput(ns("custom_error_weights")),
            HTML("</center>")
          ),
          column(width = 2)
        )
      ),
      
      # 2) Add Local Data to MWI ----
      tabPanel(
        title = "Add Local Data to MWI",
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            HTML("<center><h2>Add Local Data to MWI</h2></center>"),
            HTML("<p align='justify'> ... （省略介绍） </p>"),
            tagList(
              hr(),
              HTML("<center>"),
              downloadButton(ns("download_metadata"), "Download Metadata.xlsx"),
              HTML("<br><br>"),
              fileInput(
                ns("custom_zip"),
                "Upload Custom Data Files (.xlsx, .csv)...",
                accept = c(".xlsx", ".csv"),
                multiple = TRUE
              ),
              actionButton(ns("custom_mwi_go"), "Create Custom MWI"),
              downloadButton(ns("download_custom_mwi"), "Download Custom MWI"),
              HTML("<br><br>"),
              verbatimTextOutput(ns("custom_error")),
              HTML("</center>")
            )
          ),
          column(width = 2)
        )
      ),
      
      # 3) Add Local Data to MWI on Your Computer ----
      tabPanel(
        title = "Add Local Data to MWI on Your Computer",
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            HTML("<center><h2>Add Local Data to MWI on Your Computer</h2></center>"),
            HTML("<p align='justify'> ... （同样省略介绍） </p>"),
            tagList(
              hr(),
              HTML("<center>"),
              downloadButton(ns("download_metadata_comp"), "Download Metadata.xlsx"),
              HTML("<br><br>"),
              fileInput(
                ns("custom_zip_comp"),
                "Upload Custom Data Files (.xlsx, .csv)...",
                accept = c(".xlsx", ".csv"),
                multiple = TRUE
              ),
              actionButton(ns("custom_mwi_go_comp"), "Create Custom MWI"),
              downloadButton(ns("download_custom_mwi_comp"), "Download Custom MWI"),
              HTML("<br><br>"),
              verbatimTextOutput(ns("custom_error_comp")),
              HTML("</center>")
            )
          ),
          column(width = 2)
        )
      )
    ) # end of tabsetPanel
  )
}


mod_create_own_mwi_server <- function(id, ol){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # -------------------------------------------
    # 自定义 MWI 相关的 server 逻辑
    # 包含 weight 调整, subset zip codes, pipeline 等
    # -------------------------------------------
    
    # 1) table of custom weights
    upd_weights <- reactiveVal(sub_m)  # sub_m 来自 global (subset of measure registry)
    
    output$custom_mwi_weights <- renderDT({
      datatable(
        upd_weights(),
        rownames = FALSE,
        options = list(pageLength = nrow(upd_weights())),
        editable = list(
          target = "cell",
          disable = list(columns = c(0:2))  # 允许编辑"Updated Weights"列
        )
      )
    })
    
    observeEvent(input$custom_mwi_weights_cell_edit, {
      df <- upd_weights()
      row <- input$custom_mwi_weights_cell_edit$row
      col <- input$custom_mwi_weights_cell_edit$col
      df[row, col + 1] <- input$custom_mwi_weights_cell_edit$value
      upd_weights(df)
    })
    
    # 2) download metadata
    output$download_metadata <- output$download_metadata_comp <- downloadHandler(
      filename = function(){
        "Metadata.xlsx"
      },
      content = function(file){
        desc <- as.data.frame(
          readxl::read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 2)
        )
        writexl::write_xlsx(
          list(
            "Measure Registry" = m_reg,
            "Column Descriptions" = desc
          ),
          file
        )
      }
    )
    
    # 3) Create custom MWI
    button_click <- reactiveValues(go = 0, weights = 0, comp = 0)
    overall_list <- reactiveVal()
    
    # 如果有 pipeline，可以 source 或在此写:
    observeEvent(c(input$custom_mwi_go, input$custom_mwi_go_comp, input$custom_mwi_go_weights), {
      press <- if (input$custom_mwi_go[1] > button_click$go){
        "go"
      } else if (input$custom_mwi_go_comp[1] > button_click$comp){
        "comp"
      } else {
        "weights"
      }
      button_click$go <- input$custom_mwi_go[1]
      button_click$comp <- input$custom_mwi_go_comp[1]
      button_click$weights <- input$custom_mwi_go_weights[1]
      
      withProgress(message = "Creating custom MWI!", detail = "Loading data...", {
        # 如果你有 pipeline_driver.R:
        # source(file.path("Processing_Pipeline", "pipeline_driver.R"))
        
        # 处理文件上传、读取、校验、产生 overall_out
        # ...
        
        # Example:
        # overall_list(overall_out)
        
        output$custom_error <- output$custom_error_comp <- output$custom_error_weights <- renderText({
          "Complete! Click 'Download Custom MWI' to download..."
        })
      })
    })
    
    # 4) download the custom MWI
    output$download_custom_mwi <- output$download_custom_mwi_comp <- output$download_custom_mwi_weights <-
      downloadHandler(
        filename = function(){
          paste0("Custom_MWI_", Sys.Date(), ".RData")
        },
        content = function(file){
          withProgress(message = "Creating Custom MWI file for download...", {
            overall_output <- overall_list()
            save(overall_output, file = file)
          })
        }
      )
    
  })
}
