# file: R/mod_explore_states.R
# -----------------------------------------------------------------------------
# 模块化 Explore States 页面及相关服务器逻辑
# -----------------------------------------------------------------------------

# 模块 UI ----
mod_explore_states_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(
    title = div("Explore States", class="explore"),
    class = "explore-panel",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        bsCollapse(
          multiple = TRUE,
          open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),
          
          # 1) Exploration Options ----
          bsCollapsePanel(
            "Exploration Options",
            HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> Select any of the options below to get started. <b>If you would like to focus on a specific ZIP Code*, click on it in the map to the right or select it from the list below.</b><p>"),
            
            # Population radio
            radioButtons(
              inputId = ns("idx_type"),
              label = "Which population's MWI do you want to view?",
              choiceValues = unname(index_types),
              choiceNames = c("Overall", "Black"),
              inline = TRUE
            ),
            
            # State
            selectInput(
              ns("st_focus"),
              "Which state would you like to focus on?",
              choices = c(unname(f_st), "All"),
              selected = "Virginia"
            ),
            
            # Measure selection
            selectInput(
              ns("us_map_fill"),
              "What would you like to explore?",
              # 注意：初始时可以先默认用 overall$avail_meas_list[["pop"]]
              choices = overall$avail_meas_list[["pop"]]
            ),
            
            # ZIP Code text
            textInput(
              ns("zip_choose"),
              label = "Which ZIP Code would you like to focus on in the selected state?",
              placeholder = "e.g. 35004, 00501, 20041, etc."
            ),
            
            # Reset
            actionButton(ns("reset_zcta_click"), "Reset ZIP Code Focus")
          ),
          
          # 2) Custom MWI Upload ----
          bsCollapsePanel(
            "Custom MWI Upload",
            tagList(
              HTML("<font size = '2'><p>"),
              "To create the necessary custom Mental Wellness Index file, please see the \"Create Your Own MWI\" tab. Note that data uploaded to this application is not kept -- it is deleted once you leave the page. However, if you would like to keep your data on your computer while viewing the MWI, please see the \"Add Local Data to MWI on Your Computer\" section.",
              HTML("<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>"),
              HTML("<p></font>"),
              
              fileInput(
                ns("custom_data_st"),
                label = "Upload Custom Mental Wellness Index (.RData)",
                accept = ".RData"
              ),
              actionButton(ns("custom_data_load_st"), "Run Custom MWI"),
              actionButton(ns("custom_data_reset_st"), "Reset")
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
            HTML(paste0(
              "The Mental Wellness Index is the weighted sum of 28 measure values, which quantify facilitators and barriers to mental wellness. For more information about the Mental Wellness Index, please see the 'MWI Toolkit' page.<p></p>"
            )),
            HTML(paste0(
              "All states are included. Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points.<p></p>"
            )),
            HTML(paste0(
              "* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>census.gov</a>.<p></p>"
            )),
            HTML("</font>")
          )
        )
      ),
      
      # 主面板 ----
      mainPanel(
        width = 9,
        tags$head(tags$script(src = "msg_api.js")),
        tags$head(tags$script(src = "web_content.js")),
        
        # 左侧 - 地图
        column(
          width = 8,
          uiOutput(ns("us_map_legend")),
          HTML("<br>"),
          withSpinner(
            leafletOutput(ns("us_map"), height = 850),
            type = 8, color = "#005B94", hide.ui = FALSE
          )
        ),
        
        # 右侧 - 分布图 & 展示
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
            
            # (A) Measure Interpretation ----
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
            
            # (B) About Selected Measure ----
            bsCollapsePanel(
              "About Selected Measure",
              uiOutput(ns("data_info")),
              HTML(paste0(
                "<font size = '2'>",
                "For more information on data and overall methodology, please see the \"MWI Toolkit\" page.",
                "</font>"
              ))
            )
          )
        )
      )
    )
  )
}

# 模块 Server ----
mod_explore_states_server <- function(id, ol){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ----------------------------------------------------------------------------
    # 在原app.R中，用于管理 Explore States 的所有逻辑
    # ----------------------------------------------------------------------------
    
    # 用于追踪地图点击 / ZIP Focus
    focus_info <- reactiveValues(
      hl = FALSE,
      ZCTA = ""
    )
    
    # 用于保存当前所选的 population (Overall / Black) + State + measure
    st_sub <- reactiveValues(
      idx = "pop",  # "pop" or "black"
      st = "Virginia",
      geodat = ol$geodat[["pop"]][ol$geodat[["pop"]]$STATE_NAME == "Virginia",],
      mwi = ol$mwi[["pop"]][ol$mwi[["pop"]]$STATE_NAME == "Virginia",],
      us_map_fill = "Mental_Wellness_Index",
      is_all = FALSE
    )
    
    # 准备一个 leafletProxy 方便更新地图
    us_proxy <- leafletProxy(ns("us_map"))
    
    # ----------------------------------------------------------------------------
    # 1) 当人口类别 (idx_type) 改变时，更新 measure 选择器
    # ----------------------------------------------------------------------------
    observeEvent(input$idx_type, {
      idx <- input$idx_type
      
      # 如果当前 measure 末尾是 _black，但是我们换到 population = pop，就要改回 *_pop；反之亦然
      fill <- if (idx == "pop" & grepl("*_black$", input$us_map_fill)){
        gsub("*_black$", "_pop", input$us_map_fill)
      } else if (idx == "black" & grepl("*_pop$", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(ol$mwi[["black"]]){
        gsub("*_pop$", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
      
      updateSelectInput(
        session = session,
        inputId = "us_map_fill",
        label = "What would you like to explore?",
        choices = ol$avail_meas_list[[idx]],
        selected = fill
      )
    })
    
    # ----------------------------------------------------------------------------
    # 2) 当 state 或 population 改变时，更新 st_sub 并重绘地图
    # ----------------------------------------------------------------------------
    observeEvent(c(input$st_focus, input$idx_type), {
      idx <- input$idx_type
      
      # 重置 focus
      focus_info$hl <- FALSE
      focus_info$ZCTA <- ""
      
      # 移除高亮
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
      
      st_sub$idx <- idx
      
      if (input$st_focus == "All"){
        st_sub$st <- "All"
        st_sub$geodat <- ol$geopts[[idx]]
        st_sub$mwi <- ol$mwi[[idx]]
        st_sub$is_all <- TRUE
      } else {
        st_sub$st <- input$st_focus
        # 这里包含了所在州(STATE_NAME == input$st_focus)及bordering
        st_sub$geodat <- ol$geodat[[idx]][
          ol$geodat[[idx]]$STATE_NAME == input$st_focus |
            (ol$geodat[[idx]]$STATE_2 == st_to_fips[input$st_focus] & 
             !is.na(ol$geodat[[idx]]$STATE_2)), 
        ]
        st_sub$mwi <- ol$mwi[[idx]][
          ol$mwi[[idx]]$STATE_NAME == input$st_focus |
            (ol$mwi[[idx]]$STATE_2 == st_to_fips[input$st_focus] &
             !is.na(ol$mwi[[idx]]$STATE_2)), 
        ]
        st_sub$is_all <- FALSE
      }
      
      # 修正 measure fill 名
      st_sub$us_map_fill <- if (st_sub$idx == "pop" & grepl("*_black$", input$us_map_fill)){
        gsub("*_black$", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("*_pop$", input$us_map_fill) &&
                 !input$us_map_fill %in% colnames(ol$mwi[["black"]]){
        gsub("*_pop$", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
    })
    
    # ----------------------------------------------------------------------------
    # 3) 当点击 "Reset ZIP Code Focus" 时，取消 focus
    # ----------------------------------------------------------------------------
    observeEvent(input$reset_zcta_click, {
      focus_info$hl <- FALSE
      focus_info$ZCTA <- ""
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
    })
    
    # ----------------------------------------------------------------------------
    # 4) 当 measure fill 改变时，更新 st_sub$us_map_fill
    # ----------------------------------------------------------------------------
    observeEvent(input$us_map_fill, {
      st_sub$us_map_fill <- if (st_sub$idx == "pop" & grepl("*_black$", input$us_map_fill)){
        gsub("*_black$", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("*_pop$", input$us_map_fill) &&
                 !input$us_map_fill %in% colnames(ol$mwi[["black"]]){
        gsub("*_pop$", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
    })
    
    # ----------------------------------------------------------------------------
    # 5) 地图点击事件 - 选定 ZCTA
    # ----------------------------------------------------------------------------
    observeEvent(input$us_map_shape_click, {
      click <- input$us_map_shape_click
      if (!is.null(click$id)){
        focus_info$hl <- TRUE
        focus_info$ZCTA <- click$id
        
        # 移除上一次的高亮
        if (!st_sub$is_all){
          us_proxy %>% removeShape("remove_me")
        } else {
          us_proxy %>% removeMarker("remove_me")
        }
        
        # 加高亮 polygon
        plot_map(
          fill = st_sub$us_map_fill, 
          geodat = st_sub$geodat,
          idx = st_sub$idx, 
          ol = ol,
          is_all = st_sub$is_all,
          add_poly = TRUE, 
          us_proxy = us_proxy, 
          zcta_choose = focus_info$ZCTA
        )
        
      } else {
        focus_info$hl <- FALSE
        focus_info$ZCTA <- ""
        
        if (!st_sub$is_all){
          us_proxy %>% removeShape("remove_me")
        } else {
          us_proxy %>% removeMarker("remove_me")
        }
      }
    })
    
    # ----------------------------------------------------------------------------
    # 6) ZIP text 输入
    # ----------------------------------------------------------------------------
    observeEvent(input$zip_choose, {
      zc <- input$zip_choose
      if (
        zc != "" && 
        nchar(zc) == 5 && 
        !grepl("\\D", zc) &&  # only digits
        (zc %in% names(zip_to_zcta)) && # valid
        (zip_to_zcta[zc] %in% st_sub$geodat$GEOID) # 该ZCTA在当前state内
      ){
        focus_info$hl <- TRUE
        focus_info$ZCTA <- unname(zip_to_zcta[zc])
        
        if (!st_sub$is_all){
          us_proxy %>% removeShape("remove_me")
        } else {
          us_proxy %>% removeMarker("remove_me")
        }
        
        # 高亮
        plot_map(
          fill = st_sub$us_map_fill,
          geodat = st_sub$geodat,
          idx = st_sub$idx,
          ol = ol,
          is_all = st_sub$is_all,
          add_poly = TRUE,
          us_proxy = us_proxy,
          zcta_choose = focus_info$ZCTA
        )
      } else {
        focus_info$hl <- FALSE
        focus_info$ZCTA <- ""
        if (!st_sub$is_all){
          us_proxy %>% removeShape("remove_me")
        } else {
          us_proxy %>% removeMarker("remove_me")
        }
      }
    })
    
    # ----------------------------------------------------------------------------
    # 7) focus_on - 是否高亮
    # ----------------------------------------------------------------------------
    output$focus_on <- reactive({
      focus_info$ZCTA != ""
    })
    outputOptions(output, "focus_on", suspendWhenHidden = FALSE)
    
    # ----------------------------------------------------------------------------
    # 8) data_info - UI 渲染 measure 数据信息
    # ----------------------------------------------------------------------------
    output$data_info <- renderUI({
      full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
      HTML(paste0(
        "<font size='2'>",
        "A variety of data sources are used for measures comprising the Mental Wellness Index. The data currently pictured for ",
        full_name, 
        " came from ",
        ifelse(full_name == "Mental Wellness Index",
               "",
               ol$info_dat[st_sub$us_map_fill, "Years"]
        ),
        " ",
        ifelse(full_name == "Mental Wellness Index",
               "various sources of",
               ol$info_dat[st_sub$us_map_fill, "Source"]
        ),
        " data.<p><p>",
        ifelse(full_name == "Mental Wellness Index",
          "",
          paste0(
            full_name, " Description: ",
            ol$info_dat[st_sub$us_map_fill, "Measure.Description"]
          )
        ),
        "<p>",
        "</font>"
      ))
    })
    
    # ----------------------------------------------------------------------------
    # 9) us_map - 绘制 Leaflet 地图
    # ----------------------------------------------------------------------------
    output$us_map <- renderLeaflet({
      withProgress(message = "Rendering map", {
        # 先画基本地图
        mp <- plot_map(
          fill = st_sub$us_map_fill,
          geodat = st_sub$geodat,
          idx = st_sub$idx,
          ol = ol,
          is_all = st_sub$is_all
        )
        # 如果focus，就加高亮
        if (focus_info$hl){
          mp <- plot_map(
            fill = st_sub$us_map_fill,
            geodat = st_sub$geodat,
            idx = st_sub$idx,
            ol = ol,
            is_all = st_sub$is_all,
            add_poly = TRUE,
            us_proxy = mp,
            zcta_choose = focus_info$ZCTA
          )
        }
        mp
      })
    })
    
    # ----------------------------------------------------------------------------
    # 10) 地图上方的“彩色标题” us_map_legend
    # ----------------------------------------------------------------------------
    output$us_map_legend <- renderUI({
      withProgress(message = "Rendering map legend", {
        HTML(paste0(
          "<center>",
          paste(
            sapply(1:length(meas_max_colors), function(x){
              if (names(meas_max_colors[x]) != "Mental Wellness Index"){
                paste0(
                  "<font color='", meas_max_colors[x], "' size='3'><b>",
                  names(meas_max_colors[x]), "</b></font>"
                )
              } else {
                # 针对"Mental Wellness Index"做渐变字符
                mwi_chars <- strsplit(names(meas_max_colors[x]), "")[[1]]
                paste(
                  sapply(seq_along(mwi_chars), function(y){
                    bchar <- mwi_chars[y]
                    color_for_char <- meas_colors_pal[["Mental Wellness Index"]](nchar("Mental Wellness Index"))[y]
                    paste0(
                      "<font color='", color_for_char, "' size='3'><b>", bchar, "</b></font>"
                    )
                  }),
                  collapse = ""
                )
              }
            }),
            collapse = "<font size='3'><b> | </b></font>"
          ),
          "</center>"
        ))
      })
    })
    
    # ----------------------------------------------------------------------------
    # 11) us_map_expl - 地图未选ZCTA时的文字
    # ----------------------------------------------------------------------------
    output$us_map_expl <- renderUI({
      withProgress(message = "Rendering map explanation", {
        full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
        mc <- meas_max_colors[ol$meas_col_to_type[full_name]]
        lc <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){
          meas_min_colors[ol$meas_col_to_type[full_name]]
        } else {
          mc
        }
        
        dir_val <- if (st_sub$us_map_fill != "Mental_Wellness_Index"){
          ol$info_dat[st_sub$us_map_fill, "Directionality"]
        } else {
          1
        }
        
        explanation_text <- ""
        if (st_sub$us_map_fill == "Mental_Wellness_Index"){
          explanation_text <- paste0(
            "A ", html_color(mc, "higher"), " value indicates more ",
            html_color(mc, "assets"), " supporting ",
            html_color(mc, "mental wellness"), ". ",
            "A ", html_color(lc, "lower"), " value indicates more ",
            html_color(lc, "obstacles"), " to ",
            html_color(lc, "mental wellness"), "."
          )
        } else {
          explanation_text <- paste0(
            "A ", html_color(mc, "higher"), " value indicates a ",
            html_color(mc, "higher"), " national ranking for ",
            html_color(mc, full_name), ". ",
            "</font></b><p></p><font size='2'><i>",
            ifelse(
              dir_val == -1, 
              "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
              ""
            ),
            "</i></font>"
          )
        }
        
        HTML(paste0(
          "<center><font size='3'><b>",
          explanation_text,
          "</b></font></center>"
        ))
      })
    })
    
    # ----------------------------------------------------------------------------
    # 12) us_distr_title - 分布图标题
    # ----------------------------------------------------------------------------
    output$us_distr_title <- renderUI({
      HTML(paste0(
        "<b><center><font size='3'>",
        "Distribution of ", ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill],
        " for ",
        ifelse(st_sub$idx != "black", "the ", ""),
        ifelse(st_sub$idx == "black", "Black ", "Overall "),
        "Population",
        ifelse(st_sub$idx == "black", "s", ""),
        " in ",
        st_sub$st,
        "</font></center></b>"
      ))
    })
    
    # ----------------------------------------------------------------------------
    # 13) us_distr - 分布图 (Plotly)
    # ----------------------------------------------------------------------------
    output$us_distr <- renderPlotly({
      withProgress(message = "Rendering ZCTA data distribution", {
        plot_bee_distr(
          fill = st_sub$us_map_fill,
          st = st_sub$st,
          mwi = st_sub$mwi,
          idx = st_sub$idx,
          ol = ol,
          is_all = st_sub$is_all,
          hl = focus_info$hl,
          zcta_hl = focus_info$ZCTA
        )
      })
    })
    
    # ----------------------------------------------------------------------------
    # 14) us_info - 如果选了ZCTA, 显示其排名/分位等
    # ----------------------------------------------------------------------------
    output$us_info <- renderUI({
      withProgress(message = "Rendering ZCTA data distribution explanation", {
        if (focus_info$ZCTA != ""){
          full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
          
          dir_val <- if (st_sub$us_map_fill != "Mental_Wellness_Index"){
            ol$info_dat[st_sub$us_map_fill, "Directionality"]
          } else {
            1
          }
          
          # dir_df
          dir_df <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){
            st_sub$mwi
          } else {
            ol$no_dir_perc_meas_df[st_sub$mwi$ZCTA,]
          }
          overall_df <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){
            ol$mwi[[st_sub$idx]]
          } else {
            ol$no_dir_perc_meas_df
          }
          
          f_val <- dir_df[dir_df$ZCTA == focus_info$ZCTA, st_sub$us_map_fill]
          all_st_val <- dir_df[, st_sub$us_map_fill]
          all_us_val <- overall_df[, st_sub$us_map_fill]
          
          # 决定颜色
          mc <- if (
            st_sub$us_map_fill == "Mental_Wellness_Index" &&
            !is.na(f_val) && f_val < 50
          ){
            meas_min_colors[ol$meas_col_to_type[full_name]]
          } else {
            meas_max_colors[ol$meas_col_to_type[full_name]]
          }
          
          if (!is.na(f_val)){
            st_perc <- trunc(ecdf(all_st_val)(f_val) * 100)
            us_perc <- trunc(ecdf(all_us_val)(f_val) * 100)
            st_comp <- quant_map(st_perc)
            us_comp <- quant_map(us_perc)
            
            HTML(paste0(
              "<center>",
              "<b><font size='3'>",
              "ZCTA ", html_color(mc, focus_info$ZCTA),
              " (ZIP Code",
              ifelse(nchar(zcta_to_zip[focus_info$ZCTA]) > 5, "s "," "),
              " ", html_color(mc, zcta_to_zip[focus_info$ZCTA]), ") ",
              "has a ", html_color(mc, full_name), " value of ",
              html_color(mc, trunc(f_val)), ".<br>",
              "This is in the ", html_color(mc, st_comp), " relative to ",
              st_sub$st, ", and in the ", html_color(mc, us_comp), " relative to the United States.",
              "</font></b><p></p>",
              "<font size='2'><i>A higher value indicates a higher national ",
              ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"),
              " for ", full_name, ". ",
              ifelse(
                dir_val == -1, 
                "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
                ""
              ),
              "</i></font></center>"
            ))
          } else {
            HTML(paste0(
              "<center><b><font size='3'>",
              "ZCTA ", html_color(mc, focus_info$ZCTA),
              " (ZIP Code",
              ifelse(nchar(zcta_to_zip[focus_info$ZCTA]) > 5, "s "," "),
              " ", html_color(mc, zcta_to_zip[focus_info$ZCTA]), ") ",
              "does not have a value for ", html_color(mc, full_name),
              ", indicating missing data or no population in this area.",
              "</font></b><p></p>",
              "<font size='2'><i>A higher value indicates a higher national ",
              ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"),
              " for ", full_name, ". ",
              ifelse(
                dir_val == -1, 
                "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
                ""
              ),
              "</i></font></center>"
            ))
          }
        }
      })
    })
    
    # ----------------------------------------------------------------------------
    # 15) Custom MWI Upload - Run/Reset - 针对 Explore States
    # ----------------------------------------------------------------------------
    observeEvent(input$custom_data_load_st, {
      withProgress(
        message = "Uploading custom Mental Wellness Index!", {
          if (!is.null(input$custom_data_st)){
            validate(need(endsWith(tolower(input$custom_data_st$datapath), ".rdata"),
                          "Must upload .RData File"))
            
            # load the RData file -- contains overall_output
            load(input$custom_data_st$datapath)  # expected to have overall_output
            
            # 将新数据写入 ol
            for (ov in names(ol)){
              ol[[ov]] <- overall_output[[ov]]
            }
            
            # 重新构造 geo data 以匹配新的 MWI
            geodat_new <- list(); geopts_new <- list()
            for (idx_nm in index_types){
              geo_sub <- st_drop_geometry(overall$geodat[[idx_nm]])[, "GEOID"] %in%
                overall_output$mwi[[idx_nm]][,"ZCTA"]
              
              geodat_new[[idx_nm]] <- dplyr::left_join(
                overall$geodat[[idx_nm]][geo_sub, 1:7],
                overall_output$mwi[[idx_nm]],
                by = c("GEOID" = "ZCTA")
              )
              
              geodat_new[[idx_nm]] <- geodat_new[[idx_nm]][order(
                geodat_new[[idx_nm]]$STATE,
                geodat_new[[idx_nm]]$GEOID
              ),]
              
              geopts_new[[idx_nm]] <- st_centroid(geodat_new[[idx_nm]])
            }
            ol$geodat <- geodat_new
            ol$geopts <- geopts_new
            
            # 更新选择框
            updateSelectInput(
              session = session,
              "st_focus",
              "Which state would you like to focus on?",
              choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"),
              selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1]
            )
            updateSelectInput(
              session = session,
              "us_map_fill",
              "What would you like to explore?",
              choices = ol$avail_meas_list[["pop"]]
            )
            
            # 重置 ZCTA focus
            focus_info$hl <- FALSE
            focus_info$ZCTA <- ""
            
            # 重置 st_sub
            st_sub$idx <- "pop"
            if (!is.null(ol$mwi$pop$STATE_NAME)){
              def_st <- ol$mwi$pop$STATE_NAME[1]
              st_sub$st <- def_st
              st_sub$geodat <- ol$geodat[["pop"]][ ol$geodat[["pop"]]$STATE_NAME == def_st, ]
              st_sub$mwi <- ol$mwi[["pop"]][ ol$mwi[["pop"]]$STATE_NAME == def_st, ]
            }
            st_sub$us_map_fill <- "Mental_Wellness_Index"
            st_sub$is_all <- FALSE
          }
        }
      )
    })
    
    # 点击 reset 回到原始
    observeEvent(input$custom_data_reset_st, {
      if (!is.null(input$custom_data_st)){
        # 将 ol 重置为 original overall
        for (ov in names(ol)){
          ol[[ov]] <- overall[[ov]]
        }
        
        # 重置
        focus_info$hl <- FALSE
        focus_info$ZCTA <- ""
        
        # 恢复选择框
        updateSelectInput(
          session = session,
          "st_focus",
          "Which state would you like to focus on?",
          choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"),
          selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1]
        )
        updateSelectInput(
          session = session,
          "us_map_fill",
          "What would you like to explore?",
          choices = ol$avail_meas_list[["pop"]]
        )
        
        # 重置 st_sub
        st_sub$idx <- "pop"
        st_sub$st <- "Virginia"
        st_sub$geodat <- ol$geodat[["pop"]][ol$geodat[["pop"]]$STATE_NAME == "Virginia",]
        st_sub$mwi <- ol$mwi[["pop"]][ol$mwi[["pop"]]$STATE_NAME == "Virginia",]
        st_sub$us_map_fill <- "Mental_Wellness_Index"
        st_sub$is_all <- FALSE
      }
    })
    
 
    
  })
}
