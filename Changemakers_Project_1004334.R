# Adam Moore, University of Wales Trinity St David, SN: 1004334 -----------

# Libraries Required for Application ----------------

library(ggthemr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(av)
library(scales)
library(radiant.data)
library(lubridate)
library(stringr)
library(utils)
library(shiny)
library(shinythemes)
library(plotly)
library(gapminder)
library(profvis)
library(ggthemes)


# UI Function for Shiny R ----------------

source("functions.r")

ui <- fluidPage(theme = shinytheme("spacelab"),
                
                navbarPage(
                    
                    "Covid-19 Animated Visualisations",
                    
                    tabPanel("Vaccinations",
                             
                             sidebarPanel(
                                
                                 tags$h3("Define parameters:"),
                                 
                                 radioButtons(inputId = "Selection", label = "Graph Type", choices = c("Doses Administered", "Vaccination Race", "Scatter Plot Relative", "Scatter Plot Absolute")),
                                 
                                 selectInput(inputId = "Theme", label = "Colour Theme", choices = c("chalk", "greyscale", "fresh", "grass", "grape", "pale", "copper", "flat")),

                                 dateInput(inputId = "Start", label = "Plot From", value = "2021-01-03", format = "yyyy-mm-dd"),
                                 
                                 dateInput(inputId = "End", label = "Plot Until", value = Sys.Date(), format = "yyyy-mm-dd"),
                                 
                                 sliderInput(inputId = "PopReq", label = "Country Minimum Population (millions)", min = 0, max = 10, value = 0),
                                  
                                 conditionalPanel(
  
                                 condition = "input.Selection == 'Vaccination Race'",
  
                                 sliderInput(inputId = "rank", label = "Remove Lowest x Ranked Countries", min = 5, max = 10, value = 5)
  
                                  ),
                                 
                                 actionButton(inputId = "go", label = "Generate Visualisation")
                                 
                             ),
                             
                             mainPanel(
                                 textOutput(outputId = "test"),
                                 imageOutput(outputId = "anim")
                                      )
                             
                    ),
                    
                    tabPanel("Cases and Mortality", "Coming Soon"),
                    tabPanel("Geographical", "Coming Soon")
                    
                )
                
)



# Server Function for Shiny R ----------------



server <- function(input, output){


    dfData <- eventReactive(input$go, {
        

        if(input$Selection == "Doses Administered"){
    
            progress <- shiny::Progress$new()
            
            on.exit(progress$close())
            
            ggthemr(input$Theme, type = 'outer', spacing = 3)
            
            progress$set(message = "Getting Data (ECDC Europa & UK Gov)", value = 0.2)
    
            CovidDataPLOT <- getData(CovidDataPLOT)
            
            if(input$End >= Sys.Date()-14){date <- Sys.Date()-14}
            
            else{date <- input$End}
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>input$Start,]
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date<date,]
            
            PopReq <- input$PopReq * 1000000
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Population>PopReq, ]
            
            CovidDataPLOT <-  CovidDataPLOT[order(-CovidDataPLOT$First_Dose_Given), ]
            
            CovidDataPLOT$Country <- factor(CovidDataPLOT$Country,
                                            levels = unique(CovidDataPLOT$Country)[order(CovidDataPLOT$First_Dose_Given)])
            
            # Rendering Gif ----------------------------
            
            progress$set(message = "Building & Rendering Visuals (This may take some time)", value = 0.4)
            
            vaccTemp <- ggplot(CovidDataPLOT, aes(x = Country, y = First_Dose_Given))
            
            VaccineGraph <- vaccTemp + geom_col(color = "black", aes(fill = Second_Dose_Given)) +
                coord_flip() +
                transition_time(Date) +
              ggtitle(' ',subtitle = 'Date: {frame_time} ; Frame {frame} of {nframes}') +  # title with the timestamp period
                theme(text = element_text(size=12)) +
                guides(col=guide_legend("Test")) +
                labs(title = "Vaccine Doses",
                     x = "Country",
                     y = "First Doses Administered (% of pop)",
                     fill = "Second Doses 
                     Administered (% of pop)",
                     caption = "Data Source: ECDC Europa and UK Gov")
            
            # anim_save("outfile.gif", animate(VaccineGraph, nframes= 400, fps=100, height = 720, width = 1080, end_pause = 200, res = 100))
            
            anim_save("outfile.gif", animate(VaccineGraph, nframes= 400, fps=100, height = 720, width = 1080, end_pause = 200, res = 100))
           
            output$anim <- renderImage({
                
                list(src = "outfile.gif",
                     contentType = 'image/gif',
                     alt = "Please generate a Visualisation"
                )}, deleteFile = TRUE)
            
            progress$set(message = "Complete", value = 1)
            
        } 
        
        # Vaccine Race -----------
        
        else if(input$Selection == "Vaccination Race"){
            
            progress <- shiny::Progress$new()
            
            on.exit(progress$close())
        
            ggthemr(input$Theme, type = 'outer', spacing = 3)
            
            progress$set(message = "Getting Data (ECDC Europa & UK Gov)", value = 0.2)
            
            CovidDataPLOT <- getData(CovidDataPLOT)
            
            CovidDataPLOT <-  CovidDataPLOT %>%
                group_by(Country) %>%
                filter(max(Cumulative_First_Dose) > 1)
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>'2021-01-03',]
            
            if(input$End >= Sys.Date()-14){date <- Sys.Date()-14} else{date <- input$End}
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>input$Start,]
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date<date,]
            
            CovidDataPLOT$Year <- year(ymd(CovidDataPLOT$Date))
            
            CovidDataPLOT$Month <- month(ymd(CovidDataPLOT$Date))
            
            CovidDataPLOT$Day <- day(ymd(CovidDataPLOT$Date))
            
            CovidDataPLOT <- CovidDataPLOT %>%
                group_by(Country) %>%
                arrange(Date) %>%
                mutate(First_Dose_Given = Cumulative_First_Dose/Population*100)
            
            CovidDataPLOT <- CovidDataPLOT %>%
                group_by(Country) %>%
                arrange(Date) %>%
                mutate(Second_Dose_Given = Cumulative_Second_Dose/Population*100)
            
            CovidDataPLOT <- CovidDataPLOT %>%
                arrange(Country)
            
            CovidDataPLOT <- CovidDataPLOT[order(-CovidDataPLOT$First_Dose_Given), ]
            
            PopReq <- input$PopReq * 1000000
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Population>PopReq, ]
            
            progress$set(message = "Generating Plot", value = 0.6)
            
            CovidDataPLOT <- CovidDataPLOT %>%
                group_by(Date) %>%
                mutate(Rank = rank(First_Dose_Given)) %>%
                filter(Rank >= input$rank)
            
            Race <- ggplot(CovidDataPLOT) +
                geom_col(aes(x=Rank, y=First_Dose_Given, group=Country, fill=Second_Dose_Given), width=0.85) +
                geom_text(aes(x=Rank, y=-10, label=Country, group=Country)) +
              theme(text = element_text(size=10)) +
                ylab('Percentage of Population to receive first dose of Covid-19 Vaccine') +
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.line.y.left = element_blank(),
                      plot.margin = unit(c(3,3,3,5), 'lines')) +
                      coord_flip(clip='off') +
                labs(y = "First Doses 
                     Administered (% of pop)",
                   fill = "Second Doses 
                   Administered 
                   (% of pop)",
                   caption = "Data Source: ECDC Europa and UK Gov") +
                ggtitle('Vaccination Race',subtitle = 'Now showing {closest_state} ; Frame {frame} of {nframes}') +  # title with the timestamp period
                transition_states(Date,
                                  transition_length = 1,
                                  state_length = 2) +
                exit_fly(x_loc = 0, y_loc = 0) +
                enter_fly(x_loc = 0, y_loc = 0)
            
            progress$set(message = "Rendering Animation", value = 0.8)
            
            anim_save("outfile.gif", animate(Race, nframes= 600, fps=100, height = 720, width = 1080, end_pause = 0, res = 100))
            
            output$anim <- renderImage({
                
                list(src = "outfile.gif",
                     contentType = 'image/gif',
                     alt = "Please generate a Visualisation"
                )}, deleteFile = FALSE)
            
            progress$set(message = "Complete", value = 1)
            
            # Selection Number 3; Scatter Plot ------------------
            
        }else if(input$Selection == "Scatter Plot Relative"){
            

            progress <- shiny::Progress$new()
            on.exit(progress$close())
            
            ggthemr(input$Theme, type = 'outer', spacing = 3)
            
            progress$set(message = "Getting Data (ECDC Europa & UK Gov)", value = 0.2)
            
            CovidDataPLOT <- getData(CovidDataPLOT)
                     
            CovidDataPLOT <- CovidDataPLOT %>%
                             arrange(Country)
            
            CovidDataPLOT <- CovidDataPLOT[order(-CovidDataPLOT$First_Dose_Given), ]
            
            if(input$End >= Sys.Date()-14){date <- Sys.Date()-14}
            else{date <- input$End}
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>'2021-01-03',]
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>input$Start,]
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date<date,]
            
            CovidDataPLOT <- CovidDataPLOT[CovidDataPLOT$Country %in% c("France", "Portugal", "Iceland", "Greece", "Germany", "Bulgaria", "Spain", "United Kingdom"), ]
            
            PopReq <- input$PopReq * 1000000
            
            CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Population>PopReq, ]
            
            progress$set(message = "Generating Plot", value = 0.6)
            
            Scat = CovidDataPLOT %>%
                ggplot(aes(x=First_Dose_Given, y=Second_Dose_Given, color=Country, size=Population)) +
                geom_point(alpha = 0.75, stroke = 0) +
                scale_size(range=c(3,13), guide="none") +
                labs(title = "Vaccine Doses",
                     x = "First Doses Administered (% of pop)",
                     y = "Second Doses Administered (% of pop)",
                     color = "Country",
                     caption = "Source: ECDC Europa and UK Gov") +
                scale_color_brewer(palette = "Set1")
            
            PlotAnim = Scat +
                transition_time(Date) +
                labs(subtitle = "Date: {frame_time}") +
                shadow_wake(wake_length = 0.06)
            
            progress$set(message = "Rendering Animation", value = 0.8)
            
            anim_save("outfile.gif", animate(PlotAnim, nframes= 720, fps=40, height = 600, width = 800, end_pause = 60, res = 100))
            
            output$anim <- renderImage({
                
                list(src = "outfile.gif",
                     
                     contentType = 'image/gif',
                     
                     alt = "Please generate a Visualisation"
                     
                )}, deleteFile = FALSE)
            
            
            progress$set(message = "Complete", value = 1)
            
       
            
        } else if(input$Selection == "Scatter Plot Absolute"){
          
          
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          
          ggthemr(input$Theme, type = 'outer', spacing = 3)
          
          progress$set(message = "Getting Data (ECDC Europa & UK Gov)", value = 0.2)
          
          CovidDataPLOT <- getData(CovidDataPLOT)
          
          CovidDataPLOT <- CovidDataPLOT %>%
            arrange(Country)
          
          CovidDataPLOT <- CovidDataPLOT[order(-CovidDataPLOT$First_Dose_Given), ]
          
          if(input$End >= Sys.Date()-14){date <- Sys.Date()-14}
          else{date <- input$End}
          
          CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>'2021-01-03',]
          
          CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>input$Start,]
          
          CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date<date,]
          
          PopReq <- input$PopReq * 1000000
          
          CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Population>PopReq, ]
          
          CovidDataPLOT <- CovidDataPLOT[CovidDataPLOT$Country %in% c("France", "Portugal", "Iceland", "Greece", "Germany", "Bulgaria", "Spain", "United Kingdom"), ]
          
          progress$set(message = "Generating Plot", value = 0.6)
          
          Scat = CovidDataPLOT %>%
            ggplot(aes(x=Cumulative_First_Dose, y=Cumulative_Second_Dose, color=Country, size=Population)) +
            geom_point(alpha = 0.75, stroke = 0) +
            scale_size(range=c(3,13), guide="none") +
            labs(title = "Vaccine Doses",
                 x = "First Doses Administered Total",
                 y = "Second Doses Administered Total",
                 color = "Country",
                 caption = "Source: ECDC Europa and UK Gov") +
            scale_color_brewer(palette = "Set1")
          
          PlotAnim = Scat +
            transition_time(Date) +
            labs(subtitle = "Date: {frame_time}") +
            shadow_wake(wake_length = 0.06)
          
          progress$set(message = "Rendering Animation", value = 0.8)
          
          anim_save("outfile.gif", animate(PlotAnim, nframes= 720, fps=40, height = 600, width = 800, end_pause = 60, res = 100))
          
          output$anim <- renderImage({
            
            list(src = "outfile.gif",
                 
                 contentType = 'image/gif',
                 
                 alt = "Please generate a Visualisation"
                 
            )}, deleteFile = FALSE)
          
          
          progress$set(message = "Complete", value = 1)
          
          
          
        }
        
        
        
    })
  
    ####################################################################
    
    observeEvent(input$go, {
        
        showModal(modalDialog(title = "Wrangling Data and Rendering Visualisation.", "This can take 1-3 minutes. 
                              Please also note, the most recent data will be 14-20 days old in order to align all data to the least frequently reported European data."))
        
        easyClose = TRUE
        
        dfData()
        
        removeModal()
        
    })
    
}


shinyApp(ui = ui, server = server)