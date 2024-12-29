# file: R/mod_explore_zipcodes.R
# -----------------------------------------------------------------------------
# 模块化 Explore ZIP Codes 页面及其服务器逻辑
# -----------------------------------------------------------------------------

mod_explore_zipcodes_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(
    title = div("Explore ZIP Codes", class = "explore"),
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
            HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> To explore your community's outcomes, enter a specific ZIP Code* to get started.<p>"),
            
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
              "To create the necessary custom Mental Wellness Index file, please see the \"Create Your Own MWI\" tab. Note that data uploaded to this application is not kept -- it is deleted once you leave the page. However, if you would like to keep your data on your computer while viewing the MWI, please see the \"Add Local Data to MWI on Your Computer\" section.",
              HTML("<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>"),
              HTML("</p></font>"),
              
              fileInput(
                ns("custom_data_com"),
                label = "Upload Custom Mental Wellness Index (.RData)",
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
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          # ---- (A) Explore ZCTA Maps ----
          tabPanel(
            "Explore ZCTA Maps",
            p(),
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
                  HTML(paste0(
                    "<font size = '2'>",
                    "For more information on data and overall methodology, please see the \"MWI Toolkit\" page.",
                    "</font>"
                  ))
                ),
                bsCollapsePanel(
                  "ZCTA Measure Results",
                  uiOutput(ns("com_map_report_card"))
                )
              )
            )
          ),
          
          # ---- (B) Explore ZCTA Measures ----
          tabPanel(
            "Explore ZCTA Measures",
            p(),
            bsCollapse(
              open = c("ZCTA Measure Results"),
              bsCollapsePanel(
                "ZCTA Measure Results",
                HTML("<p><i>Measures have ranks from 0 to 100 ... For more information, please see `MWI Measures and Data`...</i></p>"),
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
    # 1) ReactiveValues for community data
    # ---------------------------------------------------------------
    com_sub <- reactiveValues(
      idx = "pop",
      ZCTA = "23936",
      
      # 初始化 geodat 和 mwi
      geodat = ol$geodat[["pop"]][ 
        st_coordinates(ol$geopts$pop)[,1] >=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[1] - 1 &
        st_coordinates(ol$geopts$pop)[,1] <=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[1] + 1 &
        st_coordinates(ol$geopts$pop)[,2] >=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[2] - 1 &
        st_coordinates(ol$geopts$pop)[,2] <=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[2] + 1,
      ],
      mwi = ol$mwi[["pop"]][
        ol$mwi[["pop"]]$ZCTA %in%
          ol$geodat[["pop"]]$GEOID[
            st_coordinates(ol$geopts$pop)[,1] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[1] - 1 &
            st_coordinates(ol$geopts$pop)[,1] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[1] + 1 &
            st_coordinates(ol$geopts$pop)[,2] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[2] - 1 &
            st_coordinates(ol$geopts$pop)[,2] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])[2] + 1
          ],
      ],
      
      com_map_fill = "Mental_Wellness_Index"
    )
    
    # ---------------------------------------------------------------
    # 2) 监听 idx_type_com & zip_choose_com
    # ---------------------------------------------------------------
    observeEvent(input$idx_type_com, {
      idx <- input$idx_type_com
      
      fill <- if (idx == "pop" & grepl("*_black$", input$com_map_fill)){
        gsub("*_black$", "_pop", input$com_map_fill)
      } else if (idx == "black" & grepl("*_pop$", input$com_map_fill) &&
                 !input$com_map_fill %in% colnames(ol$mwi[["black"]]){
        gsub("*_pop$", "_black", input$com_map_fill)
      } else {
        input$com_map_fill
      }
      
      updateSelectInput(
        session,
        "com_map_fill",
        "What would you like to explore?",
        choices = ol$avail_meas_list[[idx]],
        selected = fill
      )
    })
    
    # 监听 idx_type_com 与 zip_choose_com，更新 com_sub
    observeEvent(c(input$idx_type_com, input$zip_choose_com), {
      idx <- input$idx_type_com
      orig_zcta <- com_sub$ZCTA
      
      # 检查输入 ZIP
      if (
        input$zip_choose_com != "" &&
        nchar(input$zip_choose_com) == 5 &&
        !grepl("\\D", input$zip_choose_com) && # digits only
        input$zip_choose_com %in% names(zip_to_zcta)  # valid zcta
      ){
        com_sub$ZCTA <- unname(zip_to_zcta[input$zip_choose_com])
      }
      
      if (com_sub$idx != idx || com_sub$ZCTA != orig_zcta){
        com_sub$idx <- idx
        
        all_coord <- st_coordinates(ol$geopts[[idx]])
        zcta_coord <- st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])
        
        # 默认 ±1 但若不足则 ±3
        zcta_log <- 
          all_coord[,1] >= zcta_coord[1] - 1 &
          all_coord[,1] <= zcta_coord[1] + 1 &
          all_coord[,2] >= zcta_coord[2] - 1 &
          all_coord[,2] <= zcta_coord[2] + 1
        
        if (sum(zcta_log) <= 1){
          zcta_log <- 
            all_coord[,1] >= zcta_coord[1] - 3 &
            all_coord[,1] <= zcta_coord[1] + 3 &
            all_coord[,2] >= zcta_coord[2] - 3 &
            all_coord[,2] <= zcta_coord[2] + 3
        }
        
        com_sub$geodat <- ol$geodat[[idx]][zcta_log,]
        com_sub$mwi <- ol$mwi[[idx]][ ol$mwi[[idx]]$ZCTA %in% ol$geodat[[idx]]$GEOID, ]
        
        com_sub$com_map_fill <- if (com_sub$idx == "pop" & grepl("*_black$", input$com_map_fill)){
          gsub("*_black$", "_pop", input$com_map_fill)
        } else if (com_sub$idx == "black" & grepl("*_pop$", input$com_map_fill) &&
                   !input$com_map_fill %in% colnames(ol$mwi[["black"]]){
          gsub("*_pop$", "_black", input$com_map_fill)
        } else {
          input$com_map_fill
        }
      }
    })
    
    # ---------------------------------------------------------------
    # 3) 当 measure fill 改变时
    # ---------------------------------------------------------------
    observeEvent(input$com_map_fill, {
      com_sub$com_map_fill <- if (com_sub$idx == "pop" & grepl("*_black$", input$com_map_fill)){
        gsub("_black", "_pop", input$com_map_fill)
      } else if (com_sub$idx == "black" & grepl("_pop", input$com_map_fill) &&
                 !input$com_map_fill %in% colnames(ol$mwi[["black"]]){
        gsub("_pop", "_black", input$com_map_fill)
      } else {
        input$com_map_fill
      }
    })
    
    # ---------------------------------------------------------------
    # 4) Leaflet 地图 com_map
    # ---------------------------------------------------------------
    output$com_map <- renderLeaflet({
      withProgress(message = "Rendering map", {
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
    })
    
    # 地图上方 legend
    output$com_map_legend <- renderUI({
      withProgress(message = "Rendering map legend", {
        HTML(paste0(
          "<center>",
          paste(
            sapply(1:length(meas_max_colors), function(x){
              if (names(meas_max_colors[x]) != "Mental Wellness Index"){
                paste0("<font color='", meas_max_colors[x], "' size='3'><b>",
                       names(meas_max_colors[x]), "</b></font>")
              } else {
                # 对 MWI 做渐变
                chars <- strsplit(names(meas_max_colors[x]), "")[[1]]
                paste(
                  sapply(seq_along(chars), function(y){
                    bchar <- chars[y]
                    color_for_char <- meas_colors_pal$`Mental Wellness Index`(nchar("Mental Wellness Index"))[y]
                    paste0("<font color='", color_for_char, "' size='3'><b>", bchar, "</b></font>")
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
    
    # ---------------------------------------------------------------
    # 5) 解释文本 com_map_expl
    # ---------------------------------------------------------------
    output$com_map_expl <- renderUI({
      withProgress(message = "Rendering map explanation", {
        full_name <- ol$measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
        mc <- meas_max_colors[ol$meas_col_to_type[full_name]]
        lc <- meas_min_colors[ol$meas_col_to_type[full_name]]
        
        dir_val <- if (com_sub$com_map_fill != "Mental_Wellness_Index"){
          ol$info_dat[com_sub$com_map_fill, "Directionality"]
        } else {
          1
        }
        
        # 计算选中的ZCTA值
        dir_df <- if (com_sub$com_map_fill == "Mental_Wellness_Index"){
          com_sub$mwi
        } else {
          ol$no_dir_perc_meas_df[com_sub$mwi$ZCTA,]
        }
        overall_df <- if (com_sub$com_map_fill == "Mental_Wellness_Index"){
          ol$mwi[[com_sub$idx]]
        } else {
          ol$no_dir_perc_meas_df
        }
        
        f_val <- dir_df[dir_df$ZCTA == com_sub$ZCTA, com_sub$com_map_fill]
        all_com_val <- dir_df[, com_sub$com_map_fill]
        all_us_val <- overall_df[, com_sub$com_map_fill]
        
        # 如果选中的ZCTA有值
        if (!is.na(f_val)){
          com_perc <- trunc(ecdf(all_com_val)(f_val)*100)
          us_perc <- trunc(ecdf(all_us_val)(f_val)*100)
          com_comp <- quant_map(com_perc)
          us_comp <- quant_map(us_perc)
          
          HTML(paste0(
            "<center><b><font size='3'>",
            "ZCTA ", html_color(mc, com_sub$ZCTA),
            " (ZIP Code",
            ifelse(nchar(zcta_to_zip[com_sub$ZCTA]) > 5, "s "," "),
            html_color(mc, zcta_to_zip[com_sub$ZCTA]), ")",
            " has a ", html_color(mc, full_name), " value of ",
            html_color(mc, trunc(f_val)),
            " and is at the ",
            html_color(mc, com_perc), " percentile for the selected community. ",
            "This is in the ", html_color(mc, com_comp), " relative to the selected community, ",
            "and in the ", html_color(mc, us_comp), " relative to the United States.",
            "</b>",
            ifelse(full_name != "Mental Wellness Index",
              paste0(
                "<p></p><font size='2'><i>A higher value indicates a higher national ",
                ifelse(com_sub$com_map_fill != "Mental_Wellness_Index","ranking","value"),
                " for ", full_name, ". ",
                ifelse(dir_val == -1,
                  "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
                  ""
                ),
                "</i></font>"
              ), ""
            ),
            "</font></center>"
          ))
        } else {
          HTML(paste0(
            "<center><b><font size='3'>",
            "ZCTA ", html_color(mc, com_sub$ZCTA),
            " (ZIP Code",
            ifelse(nchar(zcta_to_zip[com_sub$ZCTA]) > 5, "s "," "),
            html_color(mc, zcta_to_zip[com_sub$ZCTA]), ")",
            " does not have a value for ",
            html_color(mc, full_name),
            ", indicating missing data or no population in this area.",
            "</b>",
            ifelse(full_name != "Mental Wellness Index",
              paste0(
                "<p></p><font size='2'><i>A higher value indicates a higher national ",
                ifelse(com_sub$com_map_fill != "Mental_Wellness_Index","ranking","value"),
                " for ", full_name, ". ",
                ifelse(dir_val == -1,
                  "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness. ",
                  ""
                ),
                "</i></font>"
              ), ""
            ),
            "</font></center>"
          ))
        }
      })
    })
    
    # ---------------------------------------------------------------
    # 6) com_map_report_card
    # ---------------------------------------------------------------
    output$com_map_report_card <- renderUI({
      mwi_zcta <- com_sub$mwi[com_sub$mwi$ZCTA == com_sub$ZCTA, , drop = FALSE]
      
      dn <- "Mental Wellness Index"
      mc <- meas_max_colors[dn]
      text_mwi <- paste0(
        "<b><font size='3'>",
        html_color(
          ifelse(trunc(mwi_zcta[1, "Mental_Wellness_Index"]) >= 50, mc, meas_min_colors[dn]),
          dn
        ),
        ": ", trunc(mwi_zcta[1, "Mental_Wellness_Index"]),
        "</b></font><br>"
      )
      
      # 构建 measure 的ranking
      text_meas <- ""
      mwi_zcta_no_dir <- ol$no_dir_perc_meas_df[com_sub$ZCTA, , drop = FALSE]
      
      for (cat_nm in names(ol$avail_meas_list[[com_sub$idx]])){
        if (cat_nm != "Mental Wellness Index"){
          cat_col <- meas_max_colors[cat_nm]
          # Category标题
          text_meas <- paste0(
            text_meas,
            "<b><font size='3'>", html_color(cat_col, cat_nm), "</b></font><br>"
          )
          # measures
          for (cn in ol$avail_meas_list[[com_sub$idx]][[cat_nm]]){
            dir_val <- ol$info_dat[cn, "Directionality"]
            val_rk <- trunc(mwi_zcta_no_dir[1, cn])
            text_meas <- paste0(
              text_meas,
              "<b>", html_color(cat_col, ol$measure_to_names[[com_sub$idx]][cn]),
              ifelse(dir_val == -1, "*", ""),
              ": </b>", val_rk, "<br>"
            )
          }
          text_meas <- paste0(text_meas, "<p>")
        }
      }
      
      HTML(paste0(
        "<font size='2'>",
        "<i>A higher MWI value indicates more assets supporting mental wellness...",
        "</i><p></p>",
        "<p><b><font size='3'>ZCTA: ", com_sub$ZCTA, "</b></font></p>",
        text_mwi,
        "<hr/>",
        "<font size='3'><b>Measure Rankings:</b></font>",
        "<i><p>Range from 0 to 100. ...</p></i>",
        "<p></p>",
        text_meas,
        "</font>"
      ))
    })
    
    # ---------------------------------------------------------------
    # 7) com_report_card_table_mwi
    # ---------------------------------------------------------------
    output$com_report_card_table_mwi <- renderUI({
      mwi_zcta <- com_sub$mwi[com_sub$mwi$ZCTA == com_sub$ZCTA, , drop = FALSE]
      
      dn <- "Mental Wellness Index"
      mc <- meas_max_colors[dn]
      text_mwi <- paste0(
        "<b><font size='3'>",
        html_color(
          ifelse(trunc(mwi_zcta[1, "Mental_Wellness_Index"]) >= 50, mc, meas_min_colors[dn]),
          dn
        ),
        ": ", trunc(mwi_zcta[1, "Mental_Wellness_Index"]),
        "</b></font><br>"
      )
      
      HTML(paste0(
        "<p><b><font size = '3'>",
        "ZCTA: ", com_sub$ZCTA,
        "</b></font></p>",
        text_mwi
      ))
    })
    
    # ---------------------------------------------------------------
    # 8) com_report_card_table - DT
    # ---------------------------------------------------------------
    output$com_report_card_table <- renderDataTable({
      reportcard <- ol$m_reg[, c("Measure", "Measure Description", "Category", "Directionality")]
      
      Rank <- t(ol$no_dir_perc_meas_df[com_sub$ZCTA, ol$avail_meas_list[[com_sub$idx]][["Mental Wellness Index"]] %>% 
                                         # 这行仅演示, 你需改成正确合并
                                         # or you can do:
                                         # colnames(ol$no_dir_perc_meas_df)
                                         # 但示例中我们只知道一部分信息
                                         # TODO: adjust for your real code
                                         # ...
                                         # 这里仅作示意
                                         colnames()])
      
      # 实际上在原代码中, 你可能直接做:
      # Rank <- t(ol$no_dir_perc_meas_df[com_sub$ZCTA,
      #           ol$avail_measures[[com_sub$idx]][-1]])
      # 并做相应的 rename.
      # 下略
      
      # -- 下面贴你原来的合并逻辑, 此处省略 ...
      # 例如：
      # rownames(Rank) <- gsub("_pop$", "", rownames(Rank))
      # ...
      
      # 你原本在 app.R 里对 DataTable 的处理
      # ...
      
      # 最终返回 datatable(...)
      datatable(
        reportcard, 
        rownames = FALSE,
        options = list(
          pageLength = nrow(reportcard)
        )
      )
    })
    
    # ---------------------------------------------------------------
    # 9) Custom MWI Upload (community)
    # ---------------------------------------------------------------
    observeEvent(input$custom_data_load_com, {
      withProgress(message = "Uploading custom MWI!", {
        if (!is.null(input$custom_data_com)){
          load(input$custom_data_com$datapath)  # overall_output
          
          for (ov in names(ol)){
            ol[[ov]] <- overall_output[[ov]]
          }
          
          # 重新生成 geodat geopts
          geodat_new <- list()
          geopts_new <- list()
          for (idx_ in index_types){
            geo_sub <- st_drop_geometry(overall$geodat[[idx_]])[, "GEOID"] %in%
              overall_output$mwi[[idx_]][,"ZCTA"]
            
            geodat_new[[idx_]] <- dplyr::left_join(
              overall$geodat[[idx_]][geo_sub, 1:7],
              overall_output$mwi[[idx_]],
              by = c("GEOID" = "ZCTA")
            )
            geodat_new[[idx_]] <- geodat_new[[idx_]][order(
              geodat_new[[idx_]]$STATE,
              geodat_new[[idx_]]$GEOID
            ),]
            
            geopts_new[[idx_]] <- st_centroid(geodat_new[[idx_]])
          }
          ol$geodat <- geodat_new
          ol$geopts <- geopts_new
          
          # 重置 com_sub
          com_sub$idx <- "pop"
          com_sub$ZCTA <- ol$mwi$pop$ZCTA[1]
          
          # 周边 ±1
          zcta_coord <- st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])
          all_coord <- st_coordinates(ol$geopts$pop)
          zcta_log <- 
            all_coord[,1] >= zcta_coord[1] - 1 &
            all_coord[,1] <= zcta_coord[1] + 1 &
            all_coord[,2] >= zcta_coord[2] - 1 &
            all_coord[,2] <= zcta_coord[2] + 1
          com_sub$geodat <- ol$geodat[["pop"]][zcta_log,]
          com_sub$mwi <- ol$mwi[["pop"]][ ol$mwi[["pop"]]$ZCTA %in% ol$geodat[["pop"]]$GEOID, ]
          com_sub$com_map_fill <- "Mental_Wellness_Index"
        }
      })
    })
    
    observeEvent(input$custom_data_reset_com, {
      if (!is.null(input$custom_data_com)){
        for (ov in names(ol)){
          ol[[ov]] <- overall[[ov]]
        }
        
        com_sub$idx <- "pop"
        com_sub$ZCTA <- "23936"
        zcta_coord <- st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == "23936",])
        all_coord <- st_coordinates(ol$geopts$pop)
        zcta_log <- 
          all_coord[,1] >= zcta_coord[1] - 1 &
          all_coord[,1] <= zcta_coord[1] + 1 &
          all_coord[,2] >= zcta_coord[2] - 1 &
          all_coord[,2] <= zcta_coord[2] + 1
        com_sub$geodat <- ol$geodat[["pop"]][zcta_log,]
        com_sub$mwi <- ol$mwi[["pop"]][ ol$mwi[["pop"]]$ZCTA %in% ol$geodat[["pop"]]$GEOID[zcta_log], ]
        com_sub$com_map_fill <- "Mental_Wellness_Index"
      }
    })
    
  })
}
