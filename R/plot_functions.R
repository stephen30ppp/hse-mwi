# file: R/plot_functions.R

# plot the overall map, filled by measure/score (LEAFLET)
plot_map <- function(fill, geodat, idx, ol, is_all = FALSE, is_com = FALSE,
                     fill_opacity = 0.7,
                     add_poly = FALSE, us_proxy = NA, zcta_choose = NA){
  gd_map <- geodat[,c(fill, "GEOID", "STATE", "STATE_NAME", "geometry")]
  colnames(gd_map)[1] <- "Fill"
  if (fill != "Mental_Wellness_Index"){
    gd_map$Fill <- ol$no_dir_perc_meas_df[gd_map$GEOID, fill]
  }
  gd_map[, colnames(all_pop_df)[-c(1:2)]] <- all_pop_df[gd_map$GEOID, -c(1:2)]
  
  gd_map <- gd_map[!is.na(gd_map$GEOID),]
  
  pal <- colorNumeric(
    palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
    domain = c(0, gd_map$Fill, 100),
    na.color = "transparent",
    reverse = ifelse(fill == "Score", TRUE, FALSE)
  )
  pal_wo_na <- colorNumeric(
    palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
    domain = c(0, gd_map$Fill, 100),
    na.color = rgb(0,0,0,0),
    reverse = ifelse(fill == "Score", TRUE, FALSE)
  )
  
  full_name <- ol$measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(full_name, " Ranking")
  }
  
  labels <- paste0(
    "State: ", gd_map$STATE_NAME, "<br>",
    "ZCTA: ", gd_map$GEOID, "<br>", 
    "ZIP Code: ", unname(zcta_to_zip[gd_map$GEOID]), "<br>", 
    "Population: ", as.data.frame(gd_map)[, paste0("total_",idx)], 
    "<br>",
    full_name, ": ", trunc(gd_map$Fill)
  ) %>% lapply(htmltools::HTML)
  
  bounds <- if (!is_com){
    unname(st_bbox(geodat))
  } else {
    unname(st_bbox(geodat[geodat$GEOID == zcta_choose,]))
  }
  
  if (!add_poly){
    mp <- if (!is_all){
      leaflet(data = gd_map) %>%
        addProviderTiles("CartoDB") %>%
        addPolygons(fillColor = ~pal(Fill),
                    weight = 1,
                    opacity = 1,
                    color = "#b2aeae",
                    dashArray = "",
                    fillOpacity = fill_opacity,
                    layerId = ~GEOID,
                    highlight = highlightOptions(weight = 2,
                                                 color = "#666",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = !is_com),
                    label = labels) %>%
        addLegend(pal = pal_wo_na,
                  values = ~c(0, Fill, 100), 
                  opacity = 0.7, 
                  position = "bottomright",
                  title = unname(full_name)) %>%
        fitBounds(
          lng1 = bounds[1],
          lng2 = bounds[3],
          lat1 = bounds[2],
          lat2 = bounds[4]
        )
    } else {
      leaflet(data = gd_map) %>%
        addProviderTiles("CartoDB") %>%
        addCircleMarkers(fillColor = ~pal(Fill),
                         weight = 1,
                         opacity = 1,
                         color = ~pal(Fill),
                         dashArray = "",
                         fillOpacity = fill_opacity,
                         layerId = ~GEOID,
                         label = labels,
                         radius = 5) %>%
        addLegend(pal = pal_wo_na,
                  values = ~c(0, Fill, 100), 
                  opacity = 0.7, 
                  position = "bottomright",
                  title = unname(full_name)) %>%
        fitBounds(
          lng1 = bounds[1],
          lng2 = bounds[3],
          lat1 = bounds[2],
          lat2 = bounds[4]
        )
    }
    
    if (is_com){
      zcta_select <- gd_map[gd_map$GEOID == zcta_choose,]
      mp <- mp %>% addPolygons(
        data = zcta_select,
        fillColor = ~pal(Fill),
        weight = 4,
        opacity = 1,
        color = "#000",
        dashArray = "",
        fillOpacity = fill_opacity,
        highlight = highlightOptions(weight = 4,
                                     color = "#000",
                                     dashArray = "",
                                     fillOpacity = 0.7,
                                     bringToFront = TRUE),
        label = labels[gd_map$GEOID == zcta_choose]
      )
    }
  } else {
    zcta_select <- gd_map[gd_map$GEOID == zcta_choose,]
    mp <- if (!is_all){
      us_proxy %>% addPolygons(
        data = zcta_select,
        fillColor = ~pal(Fill),
        weight = 4,
        opacity = 1,
        color = "#000",
        dashArray = "",
        fillOpacity = fill_opacity,
        layerId = "remove_me",
        highlight = highlightOptions(weight = 4,
                                     color = "#000",
                                     dashArray = "",
                                     fillOpacity = 0.7,
                                     bringToFront = TRUE),
        label = labels[gd_map$GEOID == zcta_choose]
      )
    } else {
      us_proxy %>% addCircleMarkers(
        data = zcta_select,
        fillColor = ~pal(Fill),
        weight = 4,
        opacity = 1,
        color = "#000",
        dashArray = "",
        fillOpacity = 1,
        layerId = "remove_me",
        label = labels[gd_map$GEOID == zcta_choose],
        radius = 7
      )
    }
  }
  
  return(mp)
}

# plot a distribution of the fill value using a beeswarm plot (PLOTLY)
plot_bee_distr <- function(fill, st, mwi, idx, ol, is_all = FALSE, hl = FALSE, zcta_hl = ""){
  bee.df <- data.frame(
    val = mwi[,fill],
    zcta = mwi$ZCTA,
    lab = rep("val", nrow(mwi)),
    focus = rep("val", nrow(mwi)),
    focus_alpha = rep(1, nrow(mwi))
  )
  bee.df <- bee.df[complete.cases(bee.df),]
  if (fill != "Mental_Wellness_Index"){
    bee.df$val <- ol$no_dir_perc_meas_df[bee.df$zcta, fill]
  }
  
  if (hl){
    row_hl <- which(bee.df$zcta == zcta_hl)
    bee.df$focus[row_hl] <- "Focus"
    bee.df$focus_alpha[-row_hl] <- 0.3
    
    pal <- colorNumeric(
      palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
      domain = c(0, bee.df$val, 100),
      na.color = "transparent"
    )
    hl_pal <- c("val" = "#e3e3e3", "Focus" = pal(bee.df[row_hl, "val"]))
    hl_size <- c("val" = 1.5, "Focus" = 3)
  }
  
  p <- if (hl){
    ggplot(bee.df, aes(lab, val, color = val, size = focus))+
      scale_color_gradientn(
        colors = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](100),
        limits = c(0, 100)
      )+
      scale_size_manual(values = hl_size)
  } else {
    ggplot(bee.df, aes(lab, val, color = val), size = 1.5)+
      scale_color_gradientn(
        colors = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](100),
        limits = c(0, 100)
      )
  }
  
  full_name <- ol$measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(full_name, " Ranking")
  }
  
  p <- suppressWarnings(
    p + 
      theme_bw()+
      ylab(full_name)+
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      )+
      ylim(-3, 103)+
      if (!is_all){
        geom_quasirandom(
          aes(text = paste0(
            "ZCTA: ", zcta, "\n",
            "ZIP Code: ", unname(zcta_to_zip[zcta]), "<br>", 
            full_name, ": ", trunc(val)
          )),
          dodge.width = NULL, alpha = bee.df$focus_alpha)
      } else {
        geom_violin(
          fill = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](3)[2],
          color = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](3)[2]
        )
      }
  )
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(margin = list(l = 0, r = 10, b = 0, t = 0)) %>%
    config(displayModeBar = FALSE)
}

quant_map <- function(perc){
  if (perc < 34) {
    "bottom third"
  } else if (perc < 67){
    "middle third"
  } else {
    "top third"
  }
}

html_color <- function(meas_color, text){
  paste0("<font color =", meas_color,">", text, "</font>")
}

# END of plot_functions.R
