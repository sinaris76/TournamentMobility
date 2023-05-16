
state_list <- c("Drop-Out", "No Mathematics", "Grade", "Alg I", "Math Models", "Geom", "Alg II", "Pre-Calc", "AP Stats", "AP Calc AB", "AP Calc BC")
## Desired state labels
state_labels <- c("Drop Out", "No Mathematics", "Grade Level/Basic", "Algebra I", "Models/Application", "Geometry", "Algebra II", "Pre-calculus", "AP Statistics", "Calculus AB", "Calculus BC")
## Desired numerical levels
state_levels <- c(-2, -1, 0,1,1.5,2,3,4,4.5,5,6)

paths_df <- read.csv("paths_df.csv")

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


get_corresponding_rows <- function(origin, destinations){
  filtered_df <- paths_df[paths_df$orig_math_state == origin & paths_df$dest_math_state %in% destinations, ]
  return(filtered_df)
}

get_disadvantageds <- function(percent_range) {
  percent_range <- percent_range / 100
  filtered_df <- subset(paths_df, mn_eds_g8 >= percent_range)
  print(filtered_df)
  return(filtered_df)
}

# Define UI ----
ui <- fluidPage(
  titlePanel("Tournament Mobility in Mathematics Course-Taking Pathways"),
  
  sidebarLayout(sidebarPanel( width = 4,
           radioButtons("OriginCourse", h3("Origin Course"),
                        choices = list("Grade" = 0, "Alg I" = 1, "Models/Application" = 1.5, 
                                       "Geometry" = 2, "Algebra II" = 3, "Pre-calculus" = 4, "AP Statistics" = 4.5, 
                                       "Calculus AB" = 5),
                                       selected = 0),
           checkboxGroupInput("DestGroup", 
                              h3("Destination Level"), 
                              choices = list("Alg I" = 1, "Models/Application" = 1.5, 
                                             "Geometry" = 2, "Algebra II" = 3, "Pre-calculus" = 4, "AP Statistics" = 4.5, 
                                             "Calculus AB" = 5, "Calculus BC"= 6),
                              selected = c(1, 1.5)),   
    sliderInput('Disadvantaged', 'Minimum Disadvantaged Percent', min = 18, max = 85, value = 18),
    ),
  mainPanel(
    "Let's investigate different paths students take during their mathematics course-taking pathway, 
    based on their economically disadvantaged status, for more information:",
    tags$a("Hanselman, P. (2020). Tournament Mobility in Mathematics Course-Taking Pathways. Socius.", href="https://doi.org/10.1177/2378023120927604"),
    plotOutput("plot", width = "80%"),
    plotOutput("plot2", width = "80%")
    ))
)

# Define server logic ----
server <- function(input, output) {
  
  # Using reactive makes it so the predict_x_cubed function will be run only the first time we 
  # run result() as long as none of the inputs have changed.
  # That way we can run predict_x_cubed only once for the plot AND computing the bias and variance
  # without needing to re-run the model.
  result <- reactive({
    get_corresponding_rows(origin = input$OriginCourse, destinations = input$DestGroup)
  })
  
  result2 <- reactive({
    get_disadvantageds(percent_range = input$Disadvantaged)
  })
  
  output$plot <- renderPlot({
    plot_df <- result()
    gg <- plot_df %>%
      filter(dest_math_state >= 0) %>%
      arrange(orig_year, n) %>%
      ggplot(aes(x=orig_year, y=orig_math_state)) +
      geom_segment(aes(xend = dest_year, yend = dest_math_state, size = n/1000, color = mn_eds_g8), alpha = .8, lineend = "round") +
      scale_size_area("Students\n(1000s)", max_size = 10) +
      scale_y_continuous("", limits = c(0, 6), minor_breaks = NULL, breaks = state_levels, labels = state_labels, position = "right") +
      scale_x_continuous("Grade", minor_breaks = NULL, breaks = 2009:2015, labels = 6:12) +
      scale_color_distiller("Economically\nDisadvantaged", labels = scales::percent, palette = "RdBu", direction = -1) +
      theme(legend.justification = c(0,1),
            legend.position = c(.03,.97),
            legend.box = "horizontal",
            legend.background = element_rect(linetype = "solid", size=.5,colour = "black"),
            text = element_text(size=16, family = "serif"),
            axis.text.y = element_text(color = c("black","black","gray50","black","black","black","gray50","black","black")),
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()
      ) 
      # coord_fixed(xlim = c(2008.75,2015))
    gg
  })
  
  
  output$plot2 <- renderPlot({
    plot_df <- result2()
    gg <- plot_df %>%
      filter(dest_math_state >= 0) %>%
      arrange(orig_year, n) %>%
      ggplot(aes(x=orig_year, y=orig_math_state)) +
      geom_segment(aes(xend = dest_year, yend = dest_math_state, size = n/1000, color = mn_eds_g8), alpha = .8, lineend = "round") +
      scale_size_area("Students\n(1000s)", max_size = 10) +
      scale_y_continuous("", limits = c(0, 6), minor_breaks = NULL, breaks = state_levels, labels = state_labels, position = "right") +
      scale_x_continuous("Grade", minor_breaks = NULL, breaks = 2009:2015, labels = 6:12) +
      scale_color_distiller("Economically\nDisadvantaged", labels = scales::percent, palette = "RdBu", direction = -1) +
      theme(legend.justification = c(0,1),
            legend.position = c(.03,.97),
            legend.box = "horizontal",
            legend.background = element_rect(linetype = "solid", size=.5,colour = "black"),
            text = element_text(size=16, family = "serif"),
            axis.text.y = element_text(color = c("black","black","gray50","black","black","black","gray50","black","black")),
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()
      ) 
      # coord_fixed(xlim = c(2008.75,2015))
    gg
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)