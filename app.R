#########################################################################################
#########################################################################################
#########################################################################################
##############       Shiny app script for Selectivity indicators apps      ##############
##############                          UnCod project                      ##############
##############                      By Tiago Veiga Malta                   ##############
##############                     version 1.0 07/09/2023                  ##############
#########################################################################################
#########################################################################################
#########################################################################################


library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(data.table)
library(dplyr)

library(tidyverse)
library(shinyBS)
library(egg)


##### ggspider function #####
ggspider <- function(p_data,
                     polygon = TRUE,
                     scaled = FALSE,
                     draw_axis = TRUE,
                     n_labels = 5,
                     subset = NULL,
                     background_color = "gray99",
                     area_fill = TRUE,
                     fill_opacity = 0.05,
                     central_distance = 0.2,
                     axis_name_offset = 0.2,
                     digit_rounding = 2,
                     axis_label_font_size = NULL,
                     axis_label_font_face = NULL,
                     axis_name_font_size = NULL,
                     axis_name_font_face = NULL
){
  
  legend_title <- names(p_data)[[1]]
  p_data <- p_data %>% dplyr::rename(group = 1) %>% dplyr::mutate(group = factor(group))
  
  circle_coords <- function(r, n_axis = ifelse(polygon == TRUE, ncol(p_data) - 1, 100)){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble::tibble(x, y, r)
  }
  
  (step_1 <- purrr::map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_polygon(data = circle_coords(1 + central_distance), alpha = 1, fill = background_color, lty = 2) +
      ggplot2::geom_path(ggplot2::aes(group = r), lty = 2, alpha = 0.5) +
      ggplot2::theme_void())
  
  
  axis_coords <- function(n_axis){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)
    
    tibble::tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }
  
  text_data <- p_data %>%
    dplyr::select(-group) %>%
    purrr::map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 1/(n_labels - 1))) %>%
    dplyr::mutate(r = seq(0, 1, 1/(n_labels - 1))) %>%
    tidyr::pivot_longer(-r, names_to = "parameter", values_to = "value")
  
  text_coords <- function(r, n_axis = ncol(p_data) - 1){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble::tibble(x, y, r = r - central_distance)
  }
  
  labels_data <- purrr::map_df(seq(0, 1, 1/(n_labels - 1)) + central_distance, text_coords) %>%
    dplyr::bind_cols(text_data %>% dplyr::select(-r))
  
  
  rescaled_coords <- function(r, n_axis){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble::tibble(r, fi) %>% dplyr::mutate(x = r*cos(fi), y = r*sin(fi)) %>% dplyr::select(-fi)
  }
  
  rescaled_data <- p_data %>%
    dplyr::mutate(across(-group, scales::rescale)) %>%
    dplyr::mutate(copy = dplyr::pull(., 2)) %>% #da se moze geom_path spojiti opet na pocetnu tocku
    tidyr::pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
    tidyr::unnest(cols = c(coords))
  
  rescaled_data <- if(is.null(subset))  rescaled_data else rescaled_data %>% filter(group %in% subset)
  
  step_1 +
    {if(draw_axis == TRUE) ggplot2::geom_line(data = axis_coords(ncol(p_data) - 1), ggplot2::aes(x, y, group = id), alpha = 0.3)} +
    ggplot2::geom_point(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group), size = 3) +
    ggplot2::geom_path(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group), size = 1) +
    {if(area_fill == TRUE) ggplot2::geom_polygon(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group, fill = group), size = 1, alpha = fill_opacity, show.legend = FALSE)} +
    {if(scaled == TRUE){
      ggplot2::geom_text(data = labels_data %>% dplyr::filter(parameter == labels_data$parameter[[1]]), ggplot2::aes(x, y, label = r), alpha = 0.65,
                         family = theme_get()$text[["family"]],
                         size = ifelse(is.null(axis_label_font_size), theme_get()$text[["size"]]/2.75, axis_label_font_size),
                         fontface = ifelse(is.null(axis_label_font_face), "plain", axis_label_font_face))
    }
      else{
        ggplot2::geom_text(data = labels_data, ggplot2::aes(x, y, label = round(value, digit_rounding)), alpha = 0.65,
                           family = theme_get()$text[["family"]],
                           size = ifelse(is.null(axis_label_font_size), theme_get()$text[["size"]]/2.75, axis_label_font_size),
                           fontface = ifelse(is.null(axis_label_font_face), "plain", axis_label_font_face))
      }
    } +
    ggplot2::geom_text(data = text_coords(1 + central_distance + axis_name_offset), ggplot2::aes(x, y), label = labels_data$parameter[1:(ncol(p_data)-1)],
                       family = theme_get()$text[["family"]],
                       size = ifelse(is.null(axis_name_font_size), theme_get()$text[["size"]]/2.75, axis_name_font_size),
                       fontface = ifelse(is.null(axis_name_font_face), "plain", axis_name_font_face)) +
    ggplot2::labs(col = legend_title) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 12))
}
#####



DF_LegalGears <- fread("data/List of Legal Gears.csv", dec = ".")
DF_GearsAlt <- data.table()

LegalGears <- unique(DF_LegalGears$Gear)


ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                      title = "Kattegat TR2 fishery",
                      tags$li(
                        class = "dropdown",
                        div(
                          style = "font-size: 12px; color: white; line-height: 1; display: flex;",
                          div(
                            style = "flex-direction: column;",
                            HTML(
                              '<span style="align-self: flex-end;"> </span><br><span style="align-self: flex-end;">Project: UnCod (33113-I-20-170)</span><br><span style="align-self: flex-end;">Contact: Valentina Melli (vmel@aqua.dtu.dk)</span><br><span style="align-self: flex-end;">Developed by: Tiago Malta (timat@aqua.dtu.dk)</span>'
                            )
                          ),
                          img(src = 'EMFF-logo-bredformat-DK.jpg', align = "right", style = "width: 109px"),
                          img(src = 'Fiskeristyrelsen logo.png', align = "right", style = "width: 190px"),
                          img(src = 'DTU Aqua logo.jpg', align = "right", style = "width: 106px")
                        )
                      )
                    ),

  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Main panel", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Information", tabName = "Information", icon = icon("readme"))
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              #begining of main box on the left
              box(width = 6,
                  box(width = 6, height = "22vh",
                      checkboxGroupInput("ListOfLegalGears", "Legal gears:", 
                                         choiceNames = LegalGears,
                                         choiceValues = LegalGears,
                                         inline = FALSE,
                                         selected = LegalGears) %>%
                        shiny::tagAppendAttributes(style = 'height: 10vh;'), 
                      bsTooltip("ListOfLegalGears", "Additional information can be found in the dropdown menu in the upper left corner",
                                "top", trigger = "hover",
                                options = list(container = "body"))),
                  box(width = 6, height = "22vh",
                      h5(HTML(paste0("<b>","List of added gears: ","</b>"))),
                      htmlOutput("ListOfAlternativeGears")  %>%
                        shiny::tagAppendAttributes(style = 'height: 10vh;')
                  ),
                  box(width = 12, title = HTML("<b>Set selectivity parameters:</b>"),
                      column(width = 6,
                             fluidRow(
                               h4(HTML(paste0("Length at 50% retention (L",tags$sub("50"),")"))),
                               column(width = 4,
                                      numericInput(inputId = "L50_Codend",
                                                   label = HTML(paste0("Insert L",tags$sub("50")," value")),
                                                   value = 38) %>%
                                        shiny::tagAppendAttributes(style = 'height: 8vh;')),
                               column(width = 4,
                                      numericInput(inputId = "L50_Codend_lower",
                                                   label = HTML(paste0("Lower 95%CI")),
                                                   value = 35) %>%
                                        shiny::tagAppendAttributes(style = 'height: 8vh;')),
                               column(width = 4,
                                      numericInput(inputId = "L50_Codend_upper",
                                                   label = HTML(paste0("Upper 95%CI")),
                                                   value = 41) %>%
                                        shiny::tagAppendAttributes(style = 'height: 8vh;'))
                             ),
                             fluidRow(
                               h4(HTML(paste0("Selection range (SR)"))),
                               column(width = 4,
                                      numericInput(inputId = "SR_Codend",
                                                   label = HTML(paste0("Insert SR value")),
                                                   value = 9) %>%
                                        shiny::tagAppendAttributes(style = 'height: 8vh;')),
                               column(width = 4,
                                      numericInput(inputId = "SR_Codend_lower",
                                                   label = HTML(paste0("Lower 95%CI")),
                                                   value = 7) %>%
                                        shiny::tagAppendAttributes(style = 'height: 8vh;')),
                               column(width = 4,
                                      numericInput(inputId = "SR_Codend_upper",
                                                   label = HTML(paste0("Upper 95%CI")),
                                                   value = 11) %>%
                                        shiny::tagAppendAttributes(style = 'height: 8vh;'))
                               
                             )
                      ),
                      column(width = 6,
                             fluidRow(
                               h4("Add name of codend:"),
                               textInput(inputId = "NewCodendName",
                                         label = "Please avoid space in the gear name as shown in example below",
                                         placeholder = "e.g., 90mm_Diamond") %>%
                                 shiny::tagAppendAttributes(style = 'height: 8vh;')
                             ),

                             fluidRow(
                               h4("Select species:"),
                               
                               column(width = 9,

                                      p("", style = "margin-bottom: 5px;"),
                                      selectInput("SpeciesCodend", label = HTML("Please note: Food and Agriculture Organisation (FAO) <a href='https://www.fao.org/fishery/en/collection/asfis/en', target='_blank'> 3 letter code</a> is used in this app."), choices = c("COD", "NEP", "PLE")),
                               ),
                               column(width = 3,
                                      br(),
                                      br(),
                                      br(),
                                      p("", style = "margin-bottom: 5px;"),
                                      actionButton("AddCodend", "Upload" )
                               )
                               
                             )
                             
                      ), height = "40vh"
                      
                  ),
                  box(width = 12, height = "27vh",
                      title = "Upload selectivity curve file for new gear:",
                      column(width = 6,
                             h5("Max file size is 5 MB and it must be a .csv file.
                                The table must contain at least the columns: ", strong("Species")," (3 letter code in capital letters)," , strong("Gear"),",",strong("Length"),
                                ",", strong("Retention"),",", strong("Lower_CI"), "and", strong("Upper_CI"),". Please note the capital letters.", style = "margin-top: -10px;"),
                             fileInput("UploadedFile", label = "Upload the file", multiple = FALSE)
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 9,
                                      br(),
                                      br(),
                                      p("", style = "margin-bottom: 5px;"),
                                      radioButtons("DEC", "Decimal separator in file:", choices = c(Period = ".", Comma = ',')),
                               ),
                               column(width = 3,
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      p("", style = "margin-bottom: 5px;"),
                                      actionButton("AddGear", "Upload")
                               )
                               
                             )
                             )
                      
                      
                  )
              , height = "95vh"),#end of main box on the left
              #beginning of main box on the right
              box(width = 6,
                  column(12,
                         box(width = 12,
                                fluidRow(plotOutput("IndicatorsPlot", height = "40vh")),
                               height = "40vh"),
                         box(width= 12,
                             fluidRow(column(12, plotOutput("PopulationStructurePlot", height = "25vh")),
                             ),
                             fluidRow(
                               column(4,
                                      h5(HTML("<b>COD parameters:</b>"), 
                                         style="text-align:center"),
                                      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                                                visibility: hidden !important;}'))),
                                      sliderInput("MovePopMean_Cod",NULL,
                                                  min = -10,
                                                  max = 40,
                                                  value = 0, ticks = FALSE), bsTooltip("MovePopMean_Cod", HTML("Move slider left or right to change mean size of COD population"),
                                                                                       "right", trigger = "hover",
                                                                                       options = list(container = "body")),
                                      
                                      column(width = 6,
                                             numericInput("MCRS_Cod", label = "MCRS (cm)", value = 30, min = 1,max = 100, step = 1)
                                      ),
                                      
                                      column(width = 6,
                                             selectInput(inputId = "NumberCohorts_Cod", label = "Cohorts:",
                                                         choices = c(1,2,3), multiple = FALSE, selected = 2)
                                      )
                               ),
                               column(width = 4,
                                      h5(HTML("<b>NEP parameters:</b>"), 
                                         style="text-align:center"),
                                      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                                                visibility: hidden !important;}'))),
                                      sliderInput("MovePopMean_Nephrops",NULL,
                                                  min = -10,
                                                  max = 40,
                                                  value = 0, ticks = FALSE), bsTooltip("MovePopMean_Nephrops", HTML("Move slider left or right to change mean size of NEP population"),
                                                                                      "right", trigger = "hover",
                                                                                      options = list(container = "body")),
                                      column(width = 6,
                                             numericInput("MCRS_Nephrops", label = "MCRS (mm)", value = 32, min = 1,max = 100, step = 1)
                                      ),
                                      
                                      column(width = 6,
                                             selectInput(inputId = "NumberCohorts_Nephrops", label = "Cohorts:",
                                                         choices = c(1,2,3), multiple = FALSE, selected = 1)
                                      )
                                      
                                      
                               ),
                               column(width = 4,
                                      h5(HTML("<b>PLE parameters:</b>"), 
                                         style="text-align:center"),
                                      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                                                visibility: hidden !important;}'))),
                                      sliderInput("MovePopMean_Plaice",NULL,
                                                  min = -10,
                                                  max = 40,
                                                  value = 0, ticks = FALSE), bsTooltip("MovePopMean_Plaice", HTML("Move slider left or right to change mean size of PLE population"),
                                                                                       "right", trigger = "hover",
                                                                                       options = list(container = "body")),
                                      column(width = 6,
                                             numericInput("MCRS_Plaice", label = "MCRS (cm)", value = 27, min = 1,max = 100, step = 1)
                                      ),
                                      
                                      column(width = 6,
                                             selectInput(inputId = "NumberCohorts_Plaice", label = "Cohorts:",
                                                         choices = c(1,2,3), multiple = FALSE, selected = 2)
                                      )
                                      
                               )
                             ), height = "25vh")), height = "95vh"#,

                  )#end of main box on the right
      ),
      tabItem( tabName = "Information",
               HTML("<p><b>SELTRA180:</b> 90 mm diamond mesh codend with a 3m-long, 180 mm square mesh panel placed at 4 to 7 m from the codline. Unpublished data collected for advice purposes on a commercial vessel. Covered codend method, No. of hauls: 15, 18 and 2 for COD, NEP and PLE, respectively.</p>"),
               HTML("<p><b>SELTRA270K:</b> 90 mm diamond mesh codend with a 3m-long, 270 mm diamond mesh panel placed at 4 to 7 m from the codline. Kattegat joining ratio of 1:4 (mesh panel:meshes codend). Unpublished data collected for advice purposes on a commercial vessel. Covered codend method, No. of hauls: 17, 18 for COD and NEP, respectively. Due to lack of data for PLE, the covered codend data for the SELTRA270 Skagerrak version (joining ratio of 1:3) was used, based on Krag et al., 2016 (<a href='https://doi.org/10.1051/alr/2016028',  target='_blank'>https://doi.org/10.1051/alr/2016028</a>).</p>"),

               HTML("<p><b>SELTRA300_4to7:</b> 90 mm diamond mesh codend with a 3m-long, 300 mm square mesh panel placed at 4 to 7 m from the codline. Detailed gear description available at  <a href='https://orbit.dtu.dk/en/publications/testing-a-large-opening-as-an-alternative-to-the-seltra-300-panel', target='_blank'>https://orbit.dtu.dk/en/publications/testing-a-large-opening-as-an-alternative-to-the-seltra-300-panel</a> and <a href='https://www.aqua.dtu.dk/-/media/institutter/aqua/publikationer/rapporter-352-400/379-2020_testing-the-placement-of-a-seltra-300-panel.pdf', target='_blank'>https://www.aqua.dtu.dk/-/media/institutter/aqua/publikationer/rapporter-352-400/379-2020_testing-the-placement-of-a-seltra-300-panel.pdf</a>. Covered codend method, No. of hauls: 18, 17 and 9 for COD, NEP and PLE, respectively.</p>"),
               HTML("<p><b>GSF_SELTRA300:</b> 90 mm diamond mesh codend with a 3m-long, 300 mm square mesh panel placed at 4 to 7 m from the codline, and a small grid with scaring floats placed at the back end of the escape panel to obstruct fish passage and facilitate escape. The gear design was never tested in this configuration, but catch comparison data were collected with respect to the SELTRA270K (see Savina et al., 2022 <a href='https://doi.org/10.1016/j.ocecoaman.2022.106286', target='_blank'>https://doi.org/10.1016/j.ocecoaman.2022.106286</a>), showing no difference in selectivity for NEP, and reduction of both COD and PLE, similar to those obtained with the SELTRA300_4to7 vs SELTRA270K. Therefore, we adopted the selectivity for SELTRA270K for NEP, and that of SELTRA300_4to7 for COD and PLE.</p>"),
               HTML("<p><b>GSWE_GRID:</b> 70 mm square mesh codend with a Swedish Grid with 35 mm bar space. Detailed gear description available at Valentinsson and Ulmestrand, 2008 (<a href='https://doi.org/10.1016/j.fishres.2007.10.011', target='_blank'>https://doi.org/10.1016/j.fishres.2007.10.011</a>). Catch comparison data (9 hauls) where a SELTRA codend with 120 mm square mesh panel was used as reference gear.  The selection of the SWE_GRID was inferred by using the methodology described in Melli et al., 2020 (<a href='https://doi.org/10.1111/faf.12428', target='_blank'>https://doi.org/10.1111/faf.12428</a>) and the known selection of SELTRA120 obtained from Krag et al., 2013 (<a href='https://findit.dtu.dk/en/catalog/2389485905', target='_blank'>https://findit.dtu.dk/en/catalog/2389485905</a>). Data were only available for COD and NEP.</p>"),
               

      )
    ),
 
  )
) #end of UI



server <- shinyServer(function(input, output, session) {

  saveData <- function(data) {
    if (exists("DF_GearsAlt")) {
      DF_GearsAlt <<- rbind(DF_GearsAlt, data)
    } else {
      DF_GearsAlt <<- data
    }
  }


  observeEvent(input$AddCodend,{

    DF <- data.table(Gear = input$NewCodendName,
                     Species= input$SpeciesCodend,
                     Length = seq(0.5,100.5,1))
    DF$Retention <- (exp((log(9)/input$SR_Codend)*(DF$Length-input$L50_Codend)))/(1+(exp((log(9)/input$SR_Codend)*(DF$Length-input$L50_Codend))))
    DF$Lower_CI <- (exp((log(9)/input$SR_Codend_lower)*(DF$Length-input$L50_Codend_lower)))/(1+(exp((log(9)/input$SR_Codend_lower)*(DF$Length-input$L50_Codend_lower))))
    DF$Upper_CI <- (exp((log(9)/input$SR_Codend_upper)*(DF$Length-input$L50_Codend_upper)))/(1+(exp((log(9)/input$SR_Codend_upper)*(DF$Length-input$L50_Codend_upper))))

    DF$Gear <- as.character(DF$Gear)
    

    saveData(DF)

    output$ListOfAlternativeGears <- renderUI({
      tags$ul(lapply(unique(DF_GearsAlt$Gear),
                     tags$li)) })

  })

  
  
  observeEvent(input$AddGear,{

    infile <- input$UploadedFile # user input file upload
    req(infile)
    DF <- fread(infile$datapath, dec = input$DEC)
    DF$Gear <- as.character(DF$Gear)

    saveData(DF)

    output$ListOfAlternativeGears <- renderUI({
      tags$ul(lapply(unique(DF_GearsAlt$Gear),
                     tags$li)) })

  })

#### Setting species Population structures ####

  PopulationStructure_Cod <- reactive({
    input$AddCodend
    input$AddGear


    TEMP <- data.frame(Species="COD", Length=seq(0.5, 100.5, 1))
    if(input$NumberCohorts_Cod == 3){

      TEMP$Freq <- (((1/(3*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Cod-20.5)^2)/(2*3^2)))*0.55 +
        ((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Cod-38.5)^2)/(2*5^2)))*0.4 +
        ((1/(10*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Cod-63.5)^2)/(2*10^2)))*0.05)*1000000

    } else if (input$NumberCohorts_Cod == 2){

      TEMP$Freq <- (((1/(3*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Cod-20.5)^2)/(2*3^2)))*0.55 +
        ((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Cod-38.5)^2)/(2*5^2)))*0.45)*1000000

    } else{

      TEMP$Freq <- (((1/(3*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Cod-20.5)^2)/(2*3^2))))*1000000

    }


    return(TEMP)

  })

  PopulationStructure_Plaice <- reactive({
    input$AddCodend
    input$AddGear


    TEMP <- data.frame(Species="PLE", Length=seq(0.5, 100.5, 1))
    if(input$NumberCohorts_Plaice == 3){

      TEMP$Freq <- (((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Plaice-23.5)^2)/(2*5^2)))*0.55 +
                      ((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Plaice-40.5)^2)/(2*5^2)))*0.4 +
                      ((1/(10*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Plaice-63.5)^2)/(2*10^2)))*0.05)*1000000

    } else if (input$NumberCohorts_Plaice == 2){

      TEMP$Freq <- (((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Plaice-23.5)^2)/(2*5^2)))*0.60 +
                      ((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Plaice-40.5)^2)/(2*5^2)))*0.40)*1000000

    } else{

      TEMP$Freq <- (((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Plaice-23.5)^2)/(2*5^2))))*1000000

    }


    return(TEMP)

  })

  PopulationStructure_Nephrops <- reactive({
    input$AddCodend
    input$AddGear


    TEMP <- data.frame(Species="NEP", Length=seq(0.5, 100.5, 1))
    if(input$NumberCohorts_Nephrops == 3){

      TEMP$Freq <- (((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Nephrops-35.5)^2)/(2*5^2)))*0.80 +
                      ((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Nephrops-50.5)^2)/(2*5^2)))*0.15 +
                      ((1/(10*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Nephrops-65.5)^2)/(2*10^2)))*0.05)*1000000

    } else if (input$NumberCohorts_Nephrops == 2){

      TEMP$Freq <- (((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Nephrops-35.5)^2)/(2*5^2)))*0.95 +
                      ((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Nephrops-50.5)^2)/(2*5^2)))*0.05)*1000000

    } else{

      TEMP$Freq <- (((1/(5*sqrt(2*pi)))*exp((-(TEMP$Length-input$MovePopMean_Nephrops-35.5)^2)/(2*5^2))))*1000000

    }


    return(TEMP)

  })

  #### Population structure plots ####

  output$PopulationStructurePlot <- renderPlot({


    PopStructs1 <- PopulationStructure_Cod()
    PopStructs2 <- PopulationStructure_Plaice()
    PopStructs3 <- PopulationStructure_Nephrops()

    PopStructs <- rbind(PopStructs1, PopStructs2, PopStructs3)

    PopStructs$Length <- as.numeric(as.character(PopStructs$Length))
    ggplot(data = PopStructs, aes(x= Length, y=Freq/1000000, colour=Species))+
      geom_line(size=1) +
      theme_bw() +
      ylab("Relative frequency") +
      xlab("Length") +
      scale_y_continuous(limits = c(0,0.15))+
      scale_color_manual(values=c("royalblue3","red", "green2"))+
      theme(axis.line = element_line(colour = "black"),
            plot.title = element_text(color="black", size=18, face="bold", hjust=.5),
            text = element_text(size=14),
            axis.title.x = element_text(size=14, face="bold"),
            axis.title.y = element_text(size=14, face="bold"),
            axis.text.y = element_text(face="bold"),
            legend.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  })


  Indicators <- reactive({
    

    NewDF_LegalGears <- DF_LegalGears[DF_LegalGears$Gear %in% c(input$ListOfLegalGears),]

    if(dim(NewDF_LegalGears)[1]!=0){
      DF_AllGears <- rbind(NewDF_LegalGears, DF_GearsAlt)

    } else {
      DF_AllGears <- DF_GearsAlt
    }



    DF_AllGears[DF_AllGears$Lower_CI < 0,]$Lower_CI <- 0
    DF_AllGears[DF_AllGears$Upper > 1,]$Upper_CI <- 1


    GearsList <- unique(DF_AllGears$Gear)
    SpeciesList <- unique(DF_AllGears$Species)
    MCRS <- data.table(Species = c("COD", "PLE", "NEP"), MCRS = c(input$MCRS_Cod, input$MCRS_Plaice, input$MCRS_Nephrops))


    PopStructs1 <- PopulationStructure_Cod()
    PopStructs2 <- PopulationStructure_Plaice()
    PopStructs3 <- PopulationStructure_Nephrops()

    PopStructs <- rbind(PopStructs1, PopStructs2, PopStructs3)

    PopStructs$Length <- as.numeric(as.character(PopStructs$Length))


    Indicators <- NULL
    ListOfIndicators <- c("Total", "Commercial", "Undersized")

    for(sp in SpeciesList){

      for(gear in GearsList){
        TEMP <- merge(PopStructs[PopStructs$Species == sp,],DF_AllGears[DF_AllGears$Gear == gear & DF_AllGears$Species == sp,])
        TEMP$Retained <- TEMP$Freq*TEMP$Retention
        TEMP$Retained_Lower <- TEMP$Freq*TEMP$Lower_CI
        TEMP$Retained_Upper <- TEMP$Freq*TEMP$Upper_CI

        TEMP_Indicator <- NULL
        for(ind in ListOfIndicators){

          TEMP_Indicator$Species <- sp

          TEMP_Indicator$Gear <- gear

          TEMP_Indicator$Indicator <- ind

          if(ind == "Total"){
            TEMP_Indicator$Mean <- round((sum(TEMP$Retained)/sum(TEMP$Freq))*100,2)
            TEMP_Indicator$Lower_CI <- round((sum(TEMP$Retained_Lower)/sum(TEMP$Freq))*100,2)
            TEMP_Indicator$Upper_CI <- round((sum(TEMP$Retained_Upper)/sum(TEMP$Freq))*100,2)
            Indicators <- rbind(Indicators, TEMP_Indicator)

          } else if(ind == "Commercial"){
            TEMP_Indicator$Mean <- round((sum(TEMP[TEMP$Length > MCRS[Species==sp]$MCRS,]$Retained)/sum(TEMP[TEMP$Length > MCRS[Species==sp]$MCRS,]$Freq))*100,2)
            TEMP_Indicator$Lower_CI <- round((sum(TEMP[TEMP$Length > MCRS[Species==sp]$MCRS,]$Retained_Lower)/sum(TEMP[TEMP$Length > MCRS[Species==sp]$MCRS,]$Freq))*100,2)
            TEMP_Indicator$Upper_CI <- round((sum(TEMP[TEMP$Length > MCRS[Species==sp]$MCRS,]$Retained_Upper)/sum(TEMP[TEMP$Length > MCRS[Species==sp]$MCRS,]$Freq))*100,2)
            Indicators <- rbind(Indicators, TEMP_Indicator)

          } else {
            TEMP_Indicator$Mean <- round((sum(TEMP[TEMP$Length <= MCRS[Species==sp]$MCRS,]$Retained)/sum(TEMP[TEMP$Length <= MCRS[Species==sp]$MCRS,]$Freq))*100,2)
            TEMP_Indicator$Lower_CI <- round((sum(TEMP[TEMP$Length <= MCRS[Species==sp]$MCRS,]$Retained_Lower)/sum(TEMP[TEMP$Length <= MCRS[Species==sp]$MCRS,]$Freq))*100,2)
            TEMP_Indicator$Upper_CI <- round((sum(TEMP[TEMP$Length <= MCRS[Species==sp]$MCRS,]$Retained_Upper)/sum(TEMP[TEMP$Length <= MCRS[Species==sp]$MCRS,]$Freq))*100,2)
            Indicators <- rbind(Indicators, TEMP_Indicator)
          }


        }

      }

    }



    return(as.data.table(Indicators))

  })

    ## radar plot without plotly
    output$IndicatorsPlot <- renderPlot({
      
      Indicators <- Indicators()
      
      Indicators[] <- lapply(Indicators, unlist)

      if(length(Indicators)){
        data_wide <- dcast(Indicators,  Gear ~ Species+Indicator, value.var="Mean")
        
        data_wide <- data_wide[, c("Gear", "COD_Total", "PLE_Total", 
                                   "NEP_Total", "COD_Commercial", 
                                   "PLE_Commercial", "NEP_Commercial",
                                   "COD_Undersized", "PLE_Undersized",
                                   "NEP_Undersized")]
        data_wide <- data_wide %>% add_row(Gear="100% retention", COD_Total=100, PLE_Total=100, NEP_Total=100, COD_Commercial=100,
                                           PLE_Commercial=100, NEP_Commercial=100, COD_Undersized=100, PLE_Undersized=100, NEP_Undersized=100)
        data_wide <- data_wide %>% add_row(Gear="0% retention", COD_Total=0, PLE_Total=0, NEP_Total=0, COD_Commercial=0,
                                           PLE_Commercial=0, NEP_Commercial=0, COD_Undersized=0, PLE_Undersized=0, NEP_Undersized=0)

      } else {
        data_wide <- data.frame(Gear=NA, COD_Total=NA, PLE_Total=NA, 
                                NEP_Total=NA, COD_Commercial=NA, 
                                PLE_Commercial=NA, NEP_Commercial=NA,
                                COD_Undersized=NA, PLE_Undersized=NA,
                                NEP_Undersized=NA)
        data_wide <- data_wide %>% add_row(Gear="100% retention", COD_Total=100, PLE_Total=100, NEP_Total=100, COD_Commercial=100,
                                           PLE_Commercial=100, NEP_Commercial=100, COD_Undersized=100, PLE_Undersized=100, NEP_Undersized=100)
        data_wide <- data_wide %>% add_row(Gear="0% retention", COD_Total=0, PLE_Total=0, NEP_Total=0, COD_Commercial=0,
                                           PLE_Commercial=0, NEP_Commercial=0, COD_Undersized=0, PLE_Undersized=0, NEP_Undersized=0)
        data_wide <- data_wide[-1,]
      }
      
      ListOfGears <- unique(Indicators$Gear)
      IndPlot <- ggspider(data_wide, scaled = FALSE, 
                         polygon = FALSE, fill_opacity = 0, 
                         subset = ListOfGears, central_distance = 0.15,
                         axis_name_offset = 0.2)+
        theme(panel.grid.major = element_blank());IndPlot

      
    })
    
    output$IndicatorsTable <- renderTable({
      
      Indicators <- Indicators()
      
      Indicators[] <- lapply(Indicators, unlist)
      Indicators$Category <- "Alternative"
      Indicators[Indicators$Gear %in% LegalGears,]$Category <- "Legal"
      
      data_wide <- dcast(Indicators,  Gear ~ Indicator+Species, value.var="Mean")
      
      data_wide <- data_wide[, c("Gear", "RetentionTotal_COD", "RetentionTotal_PLE", 
                                 "RetentionTotal_NEP", "RetentionLarge_COD", 
                                 "RetentionLarge_PLE", "RetentionLarge_NEP",
                                 "RetentionSmall_COD", "RetentionSmall_PLE",
                                 "RetentionSmall_NEP")]

      data_wide <- data_wide %>% add_row(Gear="100% retention", RetentionTotal_COD=100, RetentionTotal_PLE=100, RetentionTotal_NEP=100, RetentionLarge_COD=100,
                            RetentionLarge_PLE=100, RetentionLarge_NEP=100, RetentionSmall_COD=100, RetentionSmall_PLE=100, RetentionSmall_NEP=100)
      data_wide
      
    })
    
})



shinyApp(ui, server)
