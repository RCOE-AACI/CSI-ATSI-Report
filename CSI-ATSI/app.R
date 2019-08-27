#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

support_level <- readRDS('support_level.RDS')
group_file <- readRDS('group_file.RDS')
school_choices <- sort(support_level$school_w_district)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Border style
    tags$style(HTML("
      #red-border {
          border: 4px solid red;
      }
    ")),
    
    # Application title
    titlePanel("CSI-ATSI School Report - 2018-2019 Identification Cycle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("schoolName",
                        "Please type in a school name:",
                        choices = c("Choose"="", school_choices),
                        width = "400px",
                        selected = ""),
            fluidRow(
                div("This application enables you to lookup any school in the State of California and better understand its status within the California System of Support. ")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           fluidRow(
                      div(h3(strong("School:"), textOutput("title", inline = TRUE)), p(em("CDS: "),textOutput("cds", inline = TRUE)))
           ),
           hr(),
           fluidRow(h3("School Demographics")),
           fluidRow(
               column(3,
                      div(em("Total Enrollment"), 
                          textOutput("totalenrollment"))
                      ),
               column(2,
                      div(em("EL%"), 
                          textOutput("el"))
                      ),
               column(2,
                      div(em("SED%"), 
                          textOutput("sed"))
                      ),
               column(2,
                      div(em("SWD%"), 
                          textOutput("swd"))
                      ),
               column(2,
                      div(em("FOS%"), 
                          textOutput("fos"))
                      )
           ),
           hr(),
           fluidRow(h3("Identification Status")),
           fluidRow(h4(textOutput("status"))),
           hr(),
           fluidRow(h3("CSI - Grad Rate Test")),
           fluidRow(p("The first test for Comprehensive Support is for the school's grad rate. A grad rate of below 67% automatically qualifies as school for CSI.")),
           fluidRow(column(2,
                           div(em("Average Grad Rate (%):"), textOutput("gradrate"))
                           ),
                    column(2,
                           div(em("Grad rate below 67%?"), 
                            strong(textOutput("csi_grad"))
                           )
                    )
           ),
           hr(),
           fluidRow(h3("CSI - Low Performing")),
           fluidRow(p("The next test for Comprehensive Support is for the school's outcomes on the CA School Dashboard. There are several combinations of dashboard colors which can results in identification as CSI, Low Performing.")),
           fluidRow(h4("Dashboard Data")),
           dataTableOutput("dashboard"),
           fluidRow(h4("CSI Tests")),
           fluidRow(
               column(3,
                      div(em("All Red"), p("Are all dashboard indicators red?"), 
                          strong(textOutput("all_red")))
               ),
               column(3,
                      div(em("All red but 1"), p("Are all dashboard indicators red but one?"), 
                          strong(textOutput("all_red_but_1")))
               ),
               column(3,
                      div(em("Majority red"), p("Are the majority of dashboard indicators red when there are at least 5 indicators?"), 
                          strong(textOutput("majority_red")))
               ),
               column(3,
                      div(em("Red and orange"), p("Are all of the indicators red and orange?"),
                          strong(textOutput("red_and_orange")))
               )
           ),
           hr(),
           fluidRow(h3("ATSI Data")),
           fluidRow(h4("Student Group Results on Past Two Dashboards")),
           fluidRow(h5("Socioeconomically Disadvantaged")),
           dataTableOutput("groups_sed")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    schoolData <- reactive({
        schoolData <- support_level %>%
            filter(school_w_district == input$schoolName) %>%
            mutate(grad_average = round(grad_average, digits = 1))

        return(schoolData)    
        })
    
    dashboard <- reactive({
        req(input$schoolName)
        dashboard <- support_level %>%
            filter(school_w_district == input$schoolName) %>%
            select(`CCI` = cci,
                   `Chronic Absenteeism` = chronic, 
                   `Suspension Rate` = susp,
                   `Grad Rate` = grad, 
                   `ELA` = ela, 
                   `Mathematics` = math 
    )
        return(dashboard)
    })
    
    cds <- reactive({
        schoolData() %>%
            pull(cds)
    })
    
    groups <- reactive({
        req(cds())
        school_groups <- group_file %>%
            filter(cds == cds()) %>%
            select(`Student Group` = studentgroup,
                   `Year`= year,
                   `CCI` = cci,
                   `Chronic Absenteeism` = chronic, 
                   `Suspension Rate` = susp,
                   `Grad Rate` = grad, 
                   `ELA` = ela, 
                   `Mathematics` = math 
            )
    
        studentgrouplist <- c(
            'SED',
            'EL',
            'FOS',
            'SWD',
            'HOM',
            'AA',
            'AI',
            'AS',
            'FI',
            'HI',
            'MR',
            'WH'
        )

        get_group_data <- function(group,dataframe) {
            results <- dataframe %>%
                filter(`Student Group` == group)
            return(results)
        }
        
        results <- lapply(studentgrouplist, get_group_data, dataframe = school_groups)
    })
    
    output$title <-
        renderText({
            input$schoolName
            })

    output$cds <-
        renderText({
            schoolData()$cds
        })
    
    output$el <-
        renderText({
            schoolData()$EL
        })
    output$fos <-
        renderText({
            schoolData()$FOS
        })
    output$sed <-
        renderText({
            schoolData()$SED
        })
    output$swd <-
        renderText({
            schoolData()$SWD
        })
    output$totalenrollment <-
        renderText({
            schoolData()$totalenrollment
        })
    output$status <-
        renderText({
            schoolData()$identification
        })
    output$dashboard <-
        renderDT({
            datatable(
                dashboard(), 
                options = list(
                    dom = 't',
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '16.6%', targets = c(1, 2, 3, 4, 5, 6 )))
                    )
                ) %>% 
                formatStyle(names(dashboard()), 
                              backgroundColor = styleEqual(
                                  c("Red","Orange","Yellow","Green","Blue"),
                                  c('rgba(255,0,0,0.5)','rgba(255,165,0,0.5)','rgba(255,255,0,0.5)','rgba(0,255,0,0.5)','rgba(0,0,255,0.5)')
                              )
                     )
        }) 

    output$groups_sed <-
        renderDT({
            datatable(
                groups()[[1]],
                options = list(
                    dom = 't',
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '8%', targets = c(1, 2)),
                                      list(width = '14%', targets = c(3, 4, 5, 6, 7, 8 ))
                                      )
                )
            ) %>% 
                formatStyle(names(groups()[[1]]), 
                            backgroundColor = styleEqual(
                                c("Red","Orange","Yellow","Green","Blue"),
                                c('rgba(255,0,0,0.5)','rgba(255,165,0,0.5)','rgba(255,255,0,0.5)','rgba(0,255,0,0.5)','rgba(0,0,255,0.5)')
                            )
                )
        })

    output$gradrate <-
        renderText({
            schoolData()$grad_average
        })
    output$csi_grad <-
        renderText({
            schoolData()$csi_grad
        })
    output$all_red <-
        renderText({
            schoolData()$all_red
        })
    output$all_red_but_1 <-
        renderText({
            schoolData()$all_red_but_1
        })
    output$majority_red <-
        renderText({
            schoolData()$majority_red
        })
    output$red_and_orange <-
        renderText({
            schoolData()$red_and_orange
        })

    
    
    }


# Run the application 
shinyApp(ui = ui, server = server)
