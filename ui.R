# ui.R

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(DT)

source("R/modules/map_module.R")
source("R/modules/plot_module.R")

ui <- fluidPage(
  # Title panel sets text in the browser tab
  div(
    titlePanel(
      title="", 
      windowTitle=HTML(paste0("Mental Wellness Index™ Tool"))
    ),
    style="display:none"
  ),
   mainPanel(
          width = 9,
          tags$head(tags$script(src = "msg_api.js")),
          tags$head(tags$script(src = "web_content.js")),
          column(
            width = 8,
            uiOutput("us_map_legend"),
            HTML("<br>"),
            withSpinner(leafletOutput("us_map", height = 850),
                        type = 8, color = "#005B94", hide.ui = F)
          ),
          column(
            width = 4,
            # hr(),
            uiOutput("us_distr_title"),
            withSpinner(plotlyOutput("us_distr", height = 400),
                        type = 8, color = "#005B94", hide.ui = F),
            # hr(),
            bsCollapse(
              multiple = T,
              open = c("Measure Interpretation", "About Selected Measure"),
              bsCollapsePanel(
                "Measure Interpretation",
                conditionalPanel(
                  condition = "!output.focus_on",
                  # tableOutput("us_quantile"),
                  uiOutput("us_map_expl")
                ),
                conditionalPanel(
                  condition = "output.focus_on",
                  uiOutput("us_info")
                )
              ),
              bsCollapsePanel(
                "About Selected Measure",
                uiOutput("data_info"),
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
    ),
  navbarPage(
    collapsible = TRUE,
    title = if (show_mitre) {
      div(
        a(
          href="https://www.mitre.org/",
          img(src="media/MITRE_logo.png", height="30"),
          target="blank"
        ),
        HTML(paste0("Mental Wellness Index™ Tool"))
      )
    } else {
      div(
        HTML(paste0("Mental Wellness Index™ Tool")),
        "style" = "padding-top:5px"
      )
    },
    theme = "stylesheets/app.css",
    
    # ... (其他标签页内容保持不变)
    
    # Copyright footer
    if (show_mitre){
      HTML(paste0(
        "<span class = 'copyright-footer'>&copy; ",
        format(Sys.Date(), "%Y"),
        ", The MITRE Corporation",
        "</span>"
      ))
    }
  )
)
