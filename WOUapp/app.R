library(shiny)
library(tidyverse)

minimaltheme =  
    theme(axis.line.x = element_line(),
          axis.line.y = element_line(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))

# Define UI for application that draws graphs

# setwd("C:/Users/Colton/Desktop/WOUApp/WOUapp")

enrollment = read.csv("2010-2019 Enrollment.csv") %>%
    filter(race != "Nonresident alien")

demographics = read.csv("2010-2019 Demographics.csv") 

majors = read.csv("2010-2018 Majors.csv")

completers = read.csv("2012-2019 Completers.csv")

graduates = read.csv("2011-2019 Graduates.csv")

faculty = read.csv("2012-2019 Faculty.csv")



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

data_completersgraph = completers %>%
    left_join(enrollment, suffix = c(".completers", ".enrollment"), by = c("race", "year")) %>%
    select(year, race, ends_with("completers"), ends_with("enrollment"), -starts_with("X")) %>%
    pivot_longer(c(contains("value"), contains("total"), contains("percent")),
                 names_sep = "\\.", 
                 names_to = c("valuetype", "source"))

data_graduatesgraph = graduates %>%
    left_join(enrollment, suffix = c(".graduates", ".enrollment"), by = c("race", "year")) %>%
    select(year, race, ends_with("graduates"), ends_with("enrollment"), -starts_with("X")) %>%
    pivot_longer(c(contains("value"), contains("total"), contains("percent")),
                 names_sep = "\\.", 
                 names_to = c("valuetype", "source"))

data_facultygraph = faculty %>%
    left_join(demographics, suffix = c(".faculty", ".demographics"), by = c("race", "year")) %>%
    select(year, race, rank, ends_with("faculty"), ends_with("demographics"), -starts_with("X")) %>%
    pivot_longer(c(contains("value"), contains("total"), contains("percent")),
                 names_sep = "\\.", 
                 names_to = c("valuetype", "source"))

ui <- fluidPage(
    
    # Application title
    titlePanel("WOU - Racial and Ethnic Diversity Across the Academic Pipeline"),
    
    
    mainPanel(
        tabsetPanel(      tabPanel("Intro", 
                                   "Data Dashboard:", tags$a(href="https://coltonchristian.shinyapps.io/wouapp/", "coltonchristian.shinyapps.io/wouapp"),
                                   br(), 
                                   "Code:", tags$a(href="https://github.com/coltonchristian/WOUapp", "github.com/coltonchristian/WOUapp"),
                                   br(),
                                   "Slides:", tags$a(href="http://www.rstudio.com", "Data Dashboard"),
                                   
                                   h2("Background"),
                                   
                                   
                                   "For this project, I set out to analyze the racial and ethnic diversity at Western Oregon University. I used publicly available data from the American Community Survey (produced by the US
Census Bureau) and from IPEDS. I created a data dashboard that plots and compares racial and ethnic diversity
at various steps in the academic pipeline for each of the last 10 years (where available). These steps include Fall Enrollment,
Majors, Completers, Graduates, and the diversity of faculty.",
                                   br(),
                                   br(),
                                   "Depending on the step, a different comparison 
target is used. For example, for Fall Enrollment, I compared the racial and ethnic diversity of the students 
enrolled to the racial and ethnic diversity of the state of Oregon as a whole. For Majors, Completers, and Graduates, I compared the racial and ethnic diversity of each indicator to the racial and ethnic diversity of students enrolled. 
Lastly, for faculty diversity, I compared the racial and ethnic diversity of faculty to the racial and ethnic diversity 
of the state of Oregon as a whole."), 
                          
                          tabPanel("Enrollment", 
                                   img(src='Enrollment.png', align = "center"),
                                   
                                   h2("Enrollment"),
                                   "To examine racial and ethnic diversity in enrollment, I began by downloading Fall Enrollment data from IPEDS.
Fall enrollment represents students enrolled for credit during the fall where credit means recognition of
attendance or performance in an instructional activity (course or program) that can be applied by a recipient
toward the requirements for a degree, diploma, certificate, or other formal award. Enrollment reported is of
the institution's official fall reporting date or October 15.",
                                   br(),
                                   br(),
                                   
                                   "Next, to provide for a benchmark comparison, I downloaded population data from the American Community Survey
for the state of Oregon. Pairing these two datasets will allow me to determine whether the racial and ethnic
diversity at Western Oregon University aligns with the racial and ethnic diversity of the state of Oregon or 
if there are groups that are under- or over-represented as compared to the population of Oregon. ",
                                   
                                   br(),
                                   br(),
                                   
                                   "Under-represented: When the population estimate is higher than the enrollment estimate, this suggests that
a racial or ethnic group may be under-represented at WOU.",
                                   
                                   br(),
                                   br(),
                                   
                                   "Equally-represented: When the population estimate is equal (i.e. largely overlapping) to the enrollment
estimate, this suggests that a racial or ethnic group is fairly equally represented at WOU.",
                                   br(),
                                   br(),
                                   "Over-represented: When the population estimate is lower than the enrollment estimate, this suggests that
a racial or ethnic group may be over-represented at WOU.",
                                   hr(),
                                   selectInput("Year",
                                               "Year:",
                                               choices = seq(2010, 2019)),
                                   hr(),
                                   downloadButton('downloadenrollmentData', 'Download Enrollment Data'),
                                   downloadButton('downloadenrollmentPlot', 'Download Enrollment Plot'),
                                   plotOutput("plot_enrollment")),
                          
                          tabPanel("Majors", 
                                   img(src='Majors.png', align = "center"),
                                   h2("Majors"),
                                   "To examine racial and ethnic diversity in major areas of study, I began by downloading data on major 
programs of study from IPEDS. This data is broken out into five mean areas which include Education, 
Biological Sciences Life Sciences, Mathematics, Physical Sciences, and Business Management and 
Administrative Services.",
                                   br(),
                                   br(),
                                   "Next, to provide for a benchmark comparison, I used the fall enrollment data that was described previously. 
Pairing these two datasets will allow me to determine whether the racial and ethnic diversity in major 
programs of study at Western Oregon University aligns with the racial and ethnic diversity of the students 
enrolled at Western Oregon University or if there are groups that are under- or over-represented as compared 
to those students enrolled.",
                                   
                                   br(),
                                   br(),
                                   
                                   "Under-represented: When the enrollment estimate is higher than the major estimate, this suggests that a
racial or ethnic group may be under-represented in a major area of study at WOU.",
                                   
                                   br(),
                                   br(),
                                   
                                   "Equally-represented: When the enrollment estimate is equal (i.e. largely overlapping) to the major 
estimate, this suggests that a racial or ethnic group is fairly equally represented in a major area of 
study at WOU.",
                                   br(),
                                   br(),
                                   "Over-represented: When the enrollment estimate is lower than the major estimate, this suggests that a 
racial or ethnic group may be over-represented at WOU.",
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
                                   downloadButton('downloadmajorsData', 'Download Majors Data'),
                                   downloadButton('downloadmajorsPlot', 'Download Majors Plot'),
                                   plotOutput("plot_majors")),
                          
                          tabPanel("Graduates",
                                   img(src='Graduates.png', align = "center"),
                                   "This tab depicts...",  
                                   hr(),
                                   selectInput("Year3",
                                               "Year:",
                                               choices = seq(2011,2019,1)),
                                   hr(),
                                   downloadButton('downloadgraduatesData', 'Download Graduates Data'),
                                   downloadButton('downloadgraduatesPlot', 'Download Graduates Plot'),
                                   plotOutput("plot_graduates")),
                          
                          tabPanel("Completers", 
                                   img(src='Completers.png', align = "center"),
                                   h2("Completers"),
                                   "To examine racial and ethnic diversity in completers, I began by downloading data on completers from IPEDS. This metric is defined as the number of students receiving awards/degrees. This metric tends to be lower than completions because completions include all awards/degrees conferred, whereas for completers, each student is only counted once.",
                                   br(),
                                   br(),
                                   "Again, to provide for a benchmark comparison, I used the fall enrollment data that was described previously. Pairing these two datasets will allow me to determine whether the racial and ethnic diversity for completers at Western Oregon University aligns with the racial and ethnic diversity of the students enrolled at Western Oregon University or if there are groups that are under- or over-represented as compared to those students enrolled.",
                                   
                                   br(),
                                   br(),
                                   
                                   "Under-represented: When the enrollment estimate is higher than the completers estimate, this suggests that a racial or ethnic group may be under-represented for completers at WOU.",
                                   
                                   br(),
                                   br(),
                                   
                                   "Equally-represented: When the enrollment estimate is equal (i.e. largely overlapping) to the completers estimate, this suggests that a racial or ethnic group is fairly equally represented for completers at WOU.",
                                   br(),
                                   br(),
                                   "Over-represented: When the enrollment estimate is lower than the completers estimate, this suggests that a racial or ethnic group may be over-represented for completers at WOU.",
                                   hr(),
                                   
                                   selectInput("Year4",
                                               "Year:",
                                               choices = seq(2012, 2019, 1)),
                                   hr(),
                                   downloadButton('downloadcompletersData', 'Download Completers Data'),
                                   downloadButton('downloadcompletersPlot', 'Download Completers Plot'),
                                   plotOutput("plot_completers")),
                          
                          tabPanel("Faculty",
                                   img(src='Faculty.png', align = "center"),
                                   h2("Faculty"),
                                   "",
                                   br(),
                                   br(),
                                   "",
                                   
                                   br(),
                                   br(),
                                   
                                   "",
                                   
                                   br(),
                                   br(),
                                   
                                   "",
                                   br(),
                                   br(),
                                   "",
                                   hr(),
                                   selectInput("Rank",
                                               "Rank:",
                                               choices = c("Professor",
                                                           "Associate Professor",
                                                           "Assistant Professor",
                                                           "Instructor",
                                                           "Lecturer",
                                                           "No Academic Rank")),
                                   selectInput("Year5",
                                               "Year:",
                                               choices = seq(2012, 2019, 1)),
                                   hr(),
                                   downloadButton('downloadfacultyData', 'Download Faculty Data'),
                                   downloadButton('downloadfacultyPlot', 'Download Faculty Plot'),
                                   plotOutput("plot_faculty")) 
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #### Enrollment ####
    
    plot_reactive_enrollment = reactive({
        data_enrollmentgraph %>%
            filter(year == input$Year) %>%
            filter(valuetype == "percent") %>%
            mutate(percent = as.numeric(value)*100) %T>%
            assign("enrollmentdatadl", ., .GlobalEnv) %>%            
            ggplot(., aes(x=percent, y= race, group = source)) + 
            geom_line(aes(group = race)) +
            geom_point(aes(color = source)) +
            scale_color_manual(values = c("blue", "black")) +
            scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
            ggtitle("Enrollment") +
            theme_minimal() + 
            minimaltheme})
    
    output$plot_enrollment = renderPlot(plot_reactive_enrollment(), height =400, width = 700)
    
    output$downloadenrollmentData <- downloadHandler(
        filename = function() {
            paste('data-enrollment-', input$Year, "_", Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(enrollmentdatadl, con)
        }
    )
    
    output$downloadenrollmentPlot <- downloadHandler(
        filename = function() { paste('plot-enrollment-',  input$Year, "_", Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(plot = plot_reactive_enrollment(), file,  device = "png", height = 4, width = 7, units = "in")
        }
    )
    
    
    #### Majors ####
    
    plot_reactive_majors = reactive({
        data_majorsgraph %>%
            filter(year == input$Year2) %>%
            filter(major == input$Major) %>%
            filter(valuetype == "percent") %>%
            mutate(percent = as.numeric(value)*100) %T>%
            assign("majorsdatadl", ., .GlobalEnv) %>%
            ggplot(., aes(x=percent, y= race, group = source)) + 
            geom_line(aes(group = race)) +
            geom_point(aes(color = source)) +
            scale_color_manual(values = c("blue", "black")) +
            scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
            ggtitle("Majors") +
            theme_minimal() + 
            minimaltheme})
    
    output$plot_majors = renderPlot(plot_reactive_majors(), height =400, width = 700)
    
    output$downloadmajorsData <- downloadHandler(
        filename = function() {
            paste('data-majors-', input$Year2, "-", input$Major, "_", Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(majorsdatadl, con)
        }
    )
    
    output$downloadmajorsPlot <- downloadHandler(
        filename = function() { paste('plot-majors-', input$Year2, "-", input$Major, "_", Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(plot = plot_reactive_majors(), file,  device = "png", height = 4, width = 7, units = "in")
        }
    )
    
    
    #### Completers ####
    
    plot_reactive_completers = reactive({
        data_completersgraph %>%
            filter(year == input$Year4) %>%
            filter(valuetype == "percent") %>%
            mutate(percent = as.numeric(value)*100) %T>%
            assign("completersdatadl", ., .GlobalEnv) %>%
            ggplot(., aes(x=percent, y= race, group = source)) + 
            geom_line(aes(group = race)) +
            geom_point(aes(color = source)) +
            scale_color_manual(values = c("blue", "black")) +
            scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
            ggtitle("Completers") +
            theme_minimal() + 
            minimaltheme})
    
    output$plot_completers = renderPlot(plot_reactive_completers(), height =400, width = 700)
    
    output$downloadcompletersData <- downloadHandler(
        filename = function() {
            paste('data-completers-', input$Year4, "_",  Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(completersdatadl, con)
        }
    )
    
    output$downloadcompletersPlot <- downloadHandler(
        filename = function() { paste('plot-completers-', input$Year4, "_",  Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(plot = plot_reactive_completers(), file,  device = "png", height = 4, width = 7, units = "in")
        }
    )
    
    #### Graduates ####
    
    plot_reactive_graduates = reactive({
        data_graduatesgraph %>%
            filter(year == input$Year3) %>%
            filter(valuetype == "percent") %>%
            mutate(percent = as.numeric(value)*100) %T>%
            assign("graduatesdatadl", ., .GlobalEnv) %>%
            ggplot(., aes(x=percent, y= race, group = source)) + 
            geom_line(aes(group = race)) +
            geom_point(aes(color = source)) +
            scale_color_manual(values = c("blue", "black")) +
            scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
            ggtitle("Graduates") +
            theme_minimal() + 
            minimaltheme})
    
    output$plot_graduates = renderPlot(plot_reactive_graduates(), height =400, width = 700)
    
    output$downloadgraduatesData <- downloadHandler(
        filename = function() {
            paste('data-graduates-', input$Year3, "_", Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(graduatesdatadl, con)
        }
    )
    
    output$downloadgraduatesPlot <- downloadHandler(
        filename = function() { paste('plot-graduates-', input$Year3, "_", Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(plot = plot_reactive_graduates(), file,  device = "png", height = 4, width = 7, units = "in")
        }
    )
    
    #### Faculty ####
    
    plot_reactive_faculty = reactive({
        data_facultygraph %>%
            filter(year == input$Year5) %>%
            filter(rank == input$Rank) %>%
            filter(valuetype == "percent") %>%
            mutate(percent = as.numeric(value)*100) %T>%
            assign("facultydatadl", ., .GlobalEnv) %>%
            ggplot(., aes(x=percent, y= race, group = source)) + 
            geom_line(aes(group = race)) +
            geom_point(aes(color = source)) +
            scale_color_manual(values = c("blue", "black")) +
            scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
            ggtitle("Faculty") +
            theme_minimal() + 
            minimaltheme})
    
    output$plot_faculty = renderPlot(plot_reactive_faculty(), height =400, width = 700)
    
    output$downloadfacultyData <- downloadHandler(
        filename = function() {
            paste('data-faculty-', input$Year5, "-", input$Rank, "_", Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(facultydatadl, con)
        }
    )
    
    output$downloadfacultyPlot <- downloadHandler(
        filename = function() { paste('plot-faculty-', input$Year5, "-", input$Rank, "_", Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(plot = plot_reactive_faculty(), file,  device = "png", height = 4, width = 7, units = "in")
        }
    )
}

shinyApp(ui = ui, server = server)
