library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(readxl)
library(ggpubr)
library(shiny)
library(htmltools)

 Post_Stats <- read_xlsx("C:/Users/student/Downloads/post_stats.xlsx")

 Post_Percentiles <- read_xlsx("C:/Users/student/Downloads/Post_Percentiles.xlsx")




# Start of the UI - Part 1 of App Structure

ui <- navbarPage(
  
  "2023 Postseason Pitch Statistics", theme = "flatly",
  
  tabPanel("Pitchers",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("Team", label = "Choose Team",
                           
                           choices = levels(as.factor(Post_Stats$Team))),
               
               selectInput("Pitcher", label = "Choose Pitcher",
                           
                           choices = levels(as.factor(Post_Stats$Pitcher))),
               
               dateRangeInput("Date", label = "Choose Date Range",
                              start = min(Post_Stats$Date),
                              end = max(Post_Stats$Date),
                              min = min(Post_Stats$Date),
                              max = max(Post_Stats$Date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               
               checkboxGroupInput("Pitch", label = "Choose Pitch Type(s)", 
                                  choices = levels(as.factor(Post_Stats$TaggedPitchType))),
               
               
               width = 2),
             
             
             mainPanel(
               
               fluidRow(plotOutput("Percentiles")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(DTOutput("Percentiles_Data")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Velo_Chart")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Strike_Zone")),
               
               
             )),
           
  ),
  
)



# Start of the Server - Part 2 of App Structure

server = function(input, output, session) {
  
  # Input Reactions -- Pitcher Tab
  
  
  # Pitchers Based on Team    
  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Pitcher", "Choose Pitcher",
                      choices = levels(factor(filter(Post_Stats,
                                                     Team == isolate(input$Team))$Pitcher))))
  
  
  # Date Range Based on When Pitcher Threw
  
  observeEvent(
    input$Pitcher,
    updateDateRangeInput(session,
                         "Date", "Choose Date Range",
                         start = min(Post_Stats$Date),
                         end = max(Post_Stats$Date)))
  
  
  # Pitch Types Based on Pitcher
  
  observeEvent(
    input$Pitcher,
    updateCheckboxGroupInput(session,
                             "Pitch", "Choose Pitch Type(s)",
                             choices = levels(factor(filter(Post_Stats,
                                                            Pitcher == isolate(input$Pitcher))$TaggedPitchType))))
  
  
  # Start of Outputs (Plots and Data Tables)
  
  
  
  # Percentiles
  
  output$Percentiles <- renderPlot({
    
    # FB Max Velo
    
    MaxVelo <- Post_Percentiles%>%
      filter(Team == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
      ggplot(Post_Percentiles, mapping = aes(x= MaxVelo_percentile, y= TaggedPitchType, colour = (MaxVelo_percentile))) +
      geom_line() + geom_point(size = 9)  +
      ggtitle("Max Velo") + xlim(0, 100) + ylim("Fastball") +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
      geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = MaxVelo_percentile, y = TaggedPitchType, fill = MaxVelo_percentile), pch = 21, color = "black", size = 10) +
      geom_text(aes(label=MaxVelo_percentile),hjust=.5, vjust=.4, color = "White",
                size = 5)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3",
                           na.value = "grey50") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    
    
    # FB Max Spin
    
    MaxSpin <- Post_Percentiles%>%
      filter(Team == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
      ggplot(Post_Percentiles, mapping = aes(x= MaxSpin_percentile, y= TaggedPitchType, colour = (MaxSpin_percentile))) +
      geom_line() + geom_point(size = 9)  +
      ggtitle("Max Spin") + xlim(0, 100) + ylim("Fastball") +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
      geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = MaxSpin_percentile, y = TaggedPitchType, fill = MaxSpin_percentile), pch = 21, color = "black", size = 10) +
      geom_text(aes(label=MaxSpin_percentile),hjust=.5, vjust=.4, color = "White",
                size = 5)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    ggarrange(MaxVelo, MaxSpin ,nrow = 1, ncol = 2)
    
    
  })
  
  
  
  
  # Percentiles_Data
  
  output$Percentiles_Data <- renderDT({
    
    Post_Percentiles <- Post_Percentiles%>%
      filter(TaggedPitchType == "Fastball",
             Team == input$Team,
             Pitcher == input$Pitcher)
 
    Post_Percentiles <- as.data.table(Post_Percentiles)
    
    datatable(Post_Percentiles, caption = htmltools::tags$caption( style = 'caption-side: top; 
                                                  text-align: center; color:black; font-size:200% ;',
                                                      'Fastball Percentiles'), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1), `border-left` = "solid 1px") %>% formatStyle(c(9), `border-right` = "solid 1px")
    
    
  })
  
  
  
  
  # Velo_Chart
  
  output$Velo_Chart <- renderPlot({
    
  
    Post_Stats%>%
      filter(Team == input$Team,
             Pitcher == input$Pitcher,
             (Date >= input$Date[1] & Date <= input$Date[2]),
             TaggedPitchType %in% input$Pitch) %>%
      ggplot(Post_Stats, mapping = aes(x=PitchNo, y= RelSpeed, colour = TaggedPitchType)) +
      geom_line() + geom_point() +
      scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "grey",
                                    Sweeper = "purple",
                                    Slurve = "yellow",
                                    KnuckleCurve = "pink")) +
      ggtitle("Velocity / Pitch") +
      xlab("Pitch") + ylab("Velocity") + theme(
        plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
      geom_hline(yintercept = seq(from=70, to=100, by = 5))
    
  })
  
  
  
  # Strike_Zone
  
  output$Strike_Zone <- renderPlot({
    
    Post_Stats%>%
      filter(Team == input$Team,
             Pitcher == input$Pitcher,
             (Date >= input$Date[1] & Date <= input$Date[2]),
             TaggedPitchType %in% input$Pitch) %>%
      ggplot(Team, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType),size = 3) +
      scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "grey",
                                    Sweeper = "purple", Slurve = "yellow",
                                    KnuckleCurve = "pink"))+
      geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
      geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
      geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
      geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
      
      
      geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
      
      
      geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
      geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none") + 
      xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Strike Zone")
    
    
  })
  
}


# ShinyApp - Part 3 of App Structure

shinyApp(ui = ui, server = server)