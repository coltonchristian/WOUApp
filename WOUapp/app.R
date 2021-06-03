library(shiny)
library(tidyverse)

minimaltheme =  
    theme(axis.line.x = element_line(),
          axis.line.y = element_line(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# Define UI for application that draws graphs

setwd("C:/Users/Colton/Desktop/WOUApp/WOUapp")

enrollment = read.csv("2010-2019 Enrollment.csv") %>%
    filter(race != "Nonresident alien")

demographics = read.csv("2010-2019 Demographics.csv") 

majors = read.csv("2010-2018 Majors.csv")

completions = read.csv("2012-2019 Completions.csv")

data_enrollmentgraph = enrollment %>%
    left_join(demographics, suffix = c(".enrollment", ".demographics"), by = c("race", "year")) %>%
    select(year, race, ends_with("enrollment"), ends_with("demographics"), -starts_with("X")) %>%
    pivot_longer(c(contains("value"), contains("total"), contains("percent")),
                 names_sep = "\\.", 
                 names_to = c("valuetype", "source"))

data_majorsgraph = majors %>%
    left_join(enrollment, suffix = c(".major", ".enrollment"), by = c("race", "year")) %>%
    select(year, race, ends_with("major"), ends_with("enrollment"), -starts_with("X")) %>%
    pivot_longer(c(contains("value"), contains("total"), contains("percent")),
                 names_sep = "\\.", 
                 names_to = c("valuetype", "source"))

data_completionsgraph = completions %>%
    left_join(enrollment, suffix = c(".completions", ".enrollment"), by = c("race", "year")) %>%
    select(year, race, ends_with("completions"), ends_with("enrollment"), -starts_with("X")) %>%
    pivot_longer(c(contains("value"), contains("total"), contains("percent")),
                 names_sep = "\\.", 
                 names_to = c("valuetype", "source"))

ui <- fluidPage(
    
    # Application title
titlePanel("WOU - Racial and Ethnic Diversity Across the Academic Pipeline"),
    
    # Sidebar with a slider input for number of bins 
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(      tabPanel("Intro", 
                                       "Data Dashboard:", tags$a(href="https://coltonchristian.shinyapps.io/wouapp/", "coltonchristian.shinyapps.io/wouapp"),
                                       "Code:", tags$a(href="https://github.com/coltonchristian/WOUapp", "github.com/coltonchristian/WOUapp"),
                                       "Slides:", tags$a(href="http://www.rstudio.com", "Data Dashboard"),
                                      
                                       h2("Background"),
                                       
                                       
"For this project, I set out to analyze the racial and ethnic diversity at Western Oregon University. I used publicly available data from the American Community Survey (produced by the US
Census Bureau) and from IPEDS. I created a data dashboard that plots and compares racial and ethnic diversity
at various steps in the academic pipeline for each of the last 10 years (where available). These steps include Fall Enrollment,
Majors, Completion, Graduations, and the diversity of faculty.",
br(),
br(),
"Depending on the step, a different comparison 
target is used. For example, for Fall Enrollment, I compared the racial and ethnic diversity of the students 
enrolled to the racial and ethnic diversity of the state of Oregon as a whole. For Majors, Completions, and Graduations,
I compared the racial and ethnic diversity of each indicator to the racial and ethnic diversity of students enrolled. 
Lastly, for faculty diversity, I compared the racial and ethnic diversity of faculty to the racial and ethnic diversity 
of the state of Oregon as a whole."), 
                              
                              tabPanel("Enrollment", "This tab depicts...",
                                       hr(),
                                       selectInput("Year",
                                                   "Year:",
                                       choices = seq(2010, 2019)),
                                       hr(),
                                       plotOutput("plot_enrollment")),
                              
                              tabPanel("Majors", "This tab depicts...",  
                                       hr(),
                                       selectInput("Major",
                                                   "Major:",
                                       choices = c("Education",                                      
                                                   "Biological Sciences Life Sciences",              
                                                   "Mathematics",                                   
                                                   "Physical Sciences",                              
                                                   "Business Management and Administrative Services")),
                                       selectInput("Year2",
                                                   "Year:",
                                                   choices = seq(2010, 2018, 2)),
                                       hr(),
                                       plotOutput("plot_majors")),
                              
                              tabPanel("Completions", "This tab depicts...",  
                                       hr(),
                                       selectInput("Year3",
                                                   "Year:",
                                                   choices = seq(2012, 2019, 1)),
                                       hr(),
                                       plotOutput("plot_completions")),
                              tabPanel("Graduations"),
                              tabPanel("Faculty")) 
                                       )
        )
        
        
    

# Define server logic required to draw a histogram
server <- function(input, output) {

#### Enrollment ####
    
     plot_reactive_enrollment = reactive({
        data_enrollmentgraph %>%
            filter(year == input$Year) %>%
            filter(valuetype == "percent") %>%
            mutate(percent = as.numeric(value)*100) %>%
            ggplot(., aes(x=percent, y= race, group = source)) + 
            geom_line(aes(group = race)) +
            geom_point(aes(color = source)) +
            scale_color_manual(values = c("blue", "black")) +
            scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
            theme_minimal() + 
            minimaltheme})
     
     output$plot_enrollment = renderPlot(plot_reactive_enrollment())
     
#### Majors ####
     
     plot_reactive_majors = reactive({
         data_majorsgraph %>%
             filter(year == input$Year2) %>%
             filter(major == input$Major) %>%
             filter(valuetype == "percent") %>%
             mutate(percent = as.numeric(value)*100) %>%
             ggplot(., aes(x=percent, y= race, group = source)) + 
             geom_line(aes(group = race)) +
             geom_point(aes(color = source)) +
             scale_color_manual(values = c("blue", "black")) +
             scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
             theme_minimal() + 
             minimaltheme})
     
     output$plot_majors = renderPlot(plot_reactive_majors())
     
#### Completions ####
     
     plot_reactive_completions = reactive({
         data_completionsgraph %>%
             filter(year == input$Year3) %>%
             filter(valuetype == "percent") %>%
             mutate(percent = as.numeric(value)*100) %>%
             ggplot(., aes(x=percent, y= race, group = source)) + 
             geom_line(aes(group = race)) +
             geom_point(aes(color = source)) +
             scale_color_manual(values = c("blue", "black")) +
             scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
             theme_minimal() + 
             minimaltheme})
     
     output$plot_completions = renderPlot(plot_reactive_completions())
    }

shinyApp(ui = ui, server = server)
