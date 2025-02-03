#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(lubridate)

# Import data and convert to knots

dat <- read_excel("data/LW historical wind full.xlsx") %>%
  rename_with(tolower) %>%
  mutate(hour=hour(time),
         knots= `wind_speed_10m (km/h)` / 1.852) %>%
  select(year, month, day, hour, knots)

# calculate mean and 90% confidence intervals for each hour
dat.summary <- dat %>%
  group_by(month, day, hour) %>%
  summarize(
    mean_knots=mean(knots),
    sd=sd(knots),
    lower=max(0,qnorm(0.05, mean=mean_knots, sd=sd)),
    upper=qnorm(0.95, mean=mean_knots, sd=sd),
    .groups = "drop"
  ) %>%
  mutate(day2=str_c(month, "-", str_pad(day, 2, pad = "0"))) # needed for proper sorting 

plot.order <- sort(unique(dat.summary$day2))
  
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lake Washington Historical Wind Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          dateRangeInput("date.range", 
                         "Select a range of dates.  The calendar shows 2025 but data for the last 15 years will be summarized and displayed",
                         start = "2025-07-01",
                         end = "2025-07-31"),
          sliderInput("time.range",
                      "Select the hours for display",
                      min=0,
                      max=24,
                      value = c(12, 21))
          ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("wind.plot"),
          plotOutput("wind.tile")
          
        ) # mainPanel
    )# sidebarLayout 
    
) #ui

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$wind.plot <- renderPlot({
      dat %>%
        filter(month >= month(input$date.range[[1]]) & day >= day(input$date.range[[1]]),
               month <= month(input$date.range[[2]]) & day <= day(input$date.range[[2]]),
               hour >= input$time.range[[1]],
               hour <= input$time.range[[2]]
        ) %>%
        ggplot(aes(x=hour, y = knots)) +
        geom_smooth(aes(group=str_c(month, "-", day)), se=FALSE, alpha=0.5, color="skyblue", method = "loess") +
        geom_smooth(se=FALSE, method="loess")
    })
    
    output$wind.tile <- renderPlot({
      dat.plot <- dat.summary %>%
        filter(month >= month(input$date.range[[1]]) & day >= day(input$date.range[[1]]),
               month <= month(input$date.range[[2]]) & day <= day(input$date.range[[2]]),
               hour >= input$time.range[[1]],
               hour <= input$time.range[[2]]
        )
        dat.plot %>% 
          ggplot(aes(x=hour, y=day2, fill=mean_knots)) +
          geom_raster() +
          scale_y_discrete(limits=sort(unique(dat.plot$day2), decreasing=TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
