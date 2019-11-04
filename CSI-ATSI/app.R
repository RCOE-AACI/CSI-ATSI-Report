#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(DT)

support_level <- readRDS('support_level.RDS')
group_file <- readRDS('group_file.RDS')
school_choices <- sort(support_level$school_w_district)

studentgrouplist <- c(
  'EL',
  'SED',
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

studentgrouplong <- c(
  "English Learner",
  "Socioeconomically Disadvantaged",
  "Foster Youth",
  "Students with Disabilities",
  "Homeless",
  "Black/African American",
  "American Indian or Alaska Native",
  "Asian",
  "Filipino",
  "Hispanic",
  "Multiple Races/Two or More",
  "White"
)

studentgroupdf <-
  tibble(num = seq(1,12), studentgrouplist, studentgrouplong)

transform_logical <- function(x) {
  case_when(
    x ~ '<div style="background-color: rgba(255,0,0,0.5)"><strong>Yes</strong></div>',
    !x ~ '<strong>No</strong>',
    TRUE ~ "NA"
  )
}

transform_logical_dt <- function(x) {
  case_when(
    x ~ 'Yes',
    !x ~ 'No',
    TRUE ~ "NA"
  )
}


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
        div("This application enables you to lookup any school in the State of California and better understand its status within the California System of Support. "),
        br(),
        div("For more information on Comprehensive Support and Improvement, please go to the ", a(href="https://www.cde.ca.gov/sp/sw/t1/csi.asp", "California Department of Education's CSI website"), ".")
      ),
      width = 3),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(5,
               div(h3(strong("School:"), textOutput("title")), p(em("CDS: "),textOutput("cds", inline = TRUE)))
        ),
        column(4,
               fluidRow(h2("Identification Status")),
               uiOutput("status")
               ),
        column(3,
               fluidRow(h2("Title I Status")),
               uiOutput("titlei")
        )
      ),
      hr(),
      fluidRow(
        h2("School Demographics"),
        fluidRow(
          column(4,
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
        )
      ),
      hr(),
      fluidRow(h2("CSI - Grad Rate Test")),
      fluidRow(p("The first test for Comprehensive Support is for the school's grad rate for the 16-17 and 17-18 school year. A grad rate of below 67% automatically qualifies as school for CSI.")),
      fluidRow(
        column(3,
               div(em("Average Grad Rate (%):"), textOutput("gradrate"))
        ),
        column(3,
               div(em("Grad rate below 67%?"), 
                   uiOutput("csi_grad")
               )
        )
      ),
      hr(),
      fluidRow(h2("CSI - Low Performing")),
      fluidRow(p("The next test for Comprehensive Support is for the school's outcomes on the CA School Dashboard for the 17-18 school year. There are several combinations of dashboard colors which can results in identification as CSI, Low Performing. In addition, the school must have received Title I funding in the 17-18 school year.")),
      fluidRow(h3("Dashboard Data")),
      fluidRow(dataTableOutput("dashboard")),
      fluidRow(h3("CSI Tests")),
      fluidRow(
        column(3,
               h4("Overall Status"), p("Did the school meet the CSI low performing criteria for this year?"), 
               uiOutput("met_lowest5"))
        ,
        column(9,
               h4("Detailed Tests"), 
               fluidRow(
                 column(3,
                        div(em("All Red"), p("Are all dashboard indicators red?"), 
                            uiOutput("all_red"))
                 ),
                 column(3,
                        div(em("All red but 1"), p("Are all dashboard indicators red but one?"), 
                            uiOutput("all_red_but_1"))
                 ),
                 column(3,
                        div(em("Majority red"), p("Are the majority of dashboard indicators red when there are at least 5 indicators?"), 
                            uiOutput("majority_red"))
                 ),
                 column(3,
                        div(em("Red and orange"), p("Are all of the indicators red and orange?"),
                            uiOutput("red_and_orange"))
                 ))
        )
      ),
      # EL Info Block
      hr(),
      fluidRow(h2("ATSI Data")),
      fluidRow(h3("Student Group Results on Past Two Dashboards")),
      fluidRow(div(HTML("<small>Note: Must have a minimum of two indicators to meet any of the tests. If a student group meets the tests in BOTH years, then the school is identified for ATSI for that group.</small>"))),
      fluidRow(h4("English Learners")),
      fluidRow(dataTableOutput("groups_el")),
      br(),
      fluidRow(h5("ATSI Tests by Year")),
      fluidRow(dataTableOutput("tests_el")),
      
      # Other student group info blocks
      uiOutput("other_groups")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  schoolData <- reactive({
    
    tempData <- support_level %>%
      filter(`school_w_district` == input$schoolName) %>%
      mutate(grad_average = round(grad_average, digits = 1)) # %>%
    # mutate_if(is.logical, transform_logical)
    
    return(tempData)    
  })
  
  dashboard <- reactive({
    req(input$schoolName)
    dashboard <- support_level %>%
      filter(school_w_district == input$schoolName) %>%
      mutate(year = 2018) %>%
      select(`Year` = year,
             `CCI` = cci,
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
             `Mathematics` = math,
             `ELPI` = elpi
      )
    
    get_group_data <- function(group,dataframe) {
      results <- dataframe %>%
        filter(`Student Group` == group)
      return(results)
    }
    
    results <- lapply(studentgrouplist, get_group_data, dataframe = school_groups)
  })
  
  groups_reasons <- reactive({
    req(cds())
    school_groups <- group_file %>%
      filter(cds == cds()) %>%
      select(`Student Group` = studentgroup,
             `Year`= year,
             `Met Criteria for This Year?` = lowest5_dash,
             `All Red?` = all_red,
             `All Red but 1?` = all_red_but_1,
             `Majority red` = majority_red,
             `All red and orange` = red_and_orange
      ) %>%
      mutate_if(is.logical, transform_logical_dt)
    
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
  
  output$titlei <-
    renderUI({
      h3(schoolData()$TitleI1718)
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
    renderUI({
      status <- schoolData()$identification[1] 
      result <- case_when(
        status == "CSI Grad Rate" ~ "<h3>CSI Grad Rate</h3>",
        status == "CSI Low Perform" ~ "<h3>CSI Low Perform</h3>",
        status == "ATSI" ~ "<h3>ATSI</h3>",
        status == "General Assistance" ~ "<h3>General Assistance</h3>"
      )
      HTML(result)
    })
  output$met_lowest5 <-
    renderUI({
      HTML(schoolData()$lowest5_dash %>%
             transform_logical())
    })
  output$dashboard <-
    renderDT({
      datatable(
        dashboard(), 
        options = list(
          dom = 't',
          searching = FALSE,
          autoWidth = TRUE,
          columnDefs = list(list(width = '10%', targets = c(1)),
                            list(width = '15%', targets = c(2, 3, 4, 5, 6, 7)))
        )
      ) %>% 
        formatStyle(names(dashboard()), 
                    backgroundColor = styleEqual(
                      c("Red","Orange","Yellow","Green","Blue"),
                      c('rgba(255,0,0,0.5)','rgba(255,165,0,0.5)','rgba(255,255,0,0.5)','rgba(0,255,0,0.5)','rgba(0,0,255,0.5)')
                    )
        )
    }) 
  
  output$groups_el <-
    renderDT({
      datatable(
        groups()[[1]] %>%
          select(-`Student Group`),
        options = list(
          dom = 't',
          searching = FALSE,
          autoWidth = TRUE,
          columnDefs = list(list(width = '9%', targets = c(1)),
                            list(width = '13%', targets = c(2, 3, 4, 5, 6, 7, 8))
                            
          )
        )
      ) %>% 
        formatStyle(names(groups()[[1]] %>%
                            select(-`Student Group`) ), 
                    backgroundColor = styleEqual(
                      c("Red","Orange","Yellow","Green","Blue"),
                      c('rgba(255,0,0,0.5)','rgba(255,165,0,0.5)','rgba(255,255,0,0.5)','rgba(0,255,0,0.5)','rgba(0,0,255,0.5)')
                    )
        )
    })
  
  output$tests_el <- renderDT({
    datatable(
      groups_reasons()[[1]] %>%
        select(-`Student Group`),
      options = list(
        dom = 't',
        searching = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '10%', targets = c(1)),
                          list(width = '16%', targets = c(2, 3, 4, 5, 6)))
      )
    ) %>% 
      formatStyle(names(groups_reasons()[[1]] %>%
                          select(-`Student Group`)), 
                  backgroundColor = styleEqual(
                    c("Yes"),
                    c('rgba(255,0,0,0.5)')
                  )
      )
  })
  
  output$other_groups <-
    renderUI({
      lapply(2:12, function(i) {
        
        tagList(
          hr(),
          fluidRow(h4(studentgroupdf[i,3])),
          fluidRow(h5("Student Data")),
          renderDT(
            datatable(
              groups()[[i]] %>%
                select(-ELPI, -`Student Group`),
              options = list(
                dom = 't',
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = '10%', targets = c(1)),
                                  list(width = '15%', targets = c(2, 3, 4, 5, 6, 7)))
              )
            ) %>% 
              formatStyle(names(groups()[[i]] %>%
                                  select(-ELPI, -`Student Group`)), 
                          backgroundColor = styleEqual(
                            c("Red","Orange","Yellow","Green","Blue"),
                            c('rgba(255,0,0,0.5)','rgba(255,165,0,0.5)','rgba(255,255,0,0.5)','rgba(0,255,0,0.5)','rgba(0,0,255,0.5)')
                          )
              ),
            
          ),
          br(),
          fluidRow(h5("ATSI Tests by Year")),
          renderDT(
            datatable(
              groups_reasons()[[i]] %>%
                select(-`Student Group`),
              options = list(
                dom = 't',
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = '10%', targets = c(1)),
                                  list(width = '16%', targets = c(2, 3, 4, 5, 6)))
              )
            ) %>% 
              formatStyle(names(groups_reasons()[[i]] %>%
                                  select(-`Student Group`)), 
                          backgroundColor = styleEqual(
                            c("Yes"),
                            c('rgba(255,0,0,0.5)')
                          )
              )
          )
        )
      })
    })
  
  output$gradrate <-
    renderText({
      schoolData()$grad_average
    })
  output$csi_grad <-
    renderUI({
      HTML(schoolData()$csi_grad %>%
             transform_logical())
    })
  output$all_red <-
    renderUI({
      HTML(schoolData()$all_red %>%
             transform_logical())
    })
  output$all_red_but_1 <-
    renderUI({
      HTML(schoolData()$all_red_but_1 %>%
             transform_logical())
    })
  output$majority_red <-
    renderUI({
      HTML(schoolData()$majority_red %>%
             transform_logical())
    })
  output$red_and_orange <-
    renderUI({
      HTML(schoolData()$red_and_orange %>%
             transform_logical())
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
