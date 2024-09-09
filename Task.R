#R packages####
library(shinythemes)
library(ggplot2)
library(shiny)
library(DT)

#Design UI layout####
ui <- fluidPage(
  titlePanel(title="Hello Shiny!"),   #1. Create a title for the app called "Hello Shiny!". Hint: see titlePanel().
  theme = shinytheme("sandstone"),    #2. Apply a theme to your shiny app. Hint: see shinythemes package for a list of themes.
  sidebarLayout(                      #3. Create a Shiny layout that contains a sidebar for inputs. Hint: use sidebarPanel() to create a sidebar.
    sidebarPanel(
      markdown("Welcome to Shiny Workshop! Let's get started!"),    #3.1 In the sidebar, add a plain text, "Welcome to Shiny Workshop! Let's get started!"
      radioButtons("rb_button", "Choose a dataset:",                #3.2 Add 2 line breaks after the plain text. Hint use br().
                   c("mtcars" = "mtcars",                           #3.3 Add radio buttons that allow users to select from a list of global dataset available in R. Hint: use radioButtons() to create radio buttons and the choices are mtcars, pressure, rock, and cars.
                     "pressure" = "pressure",
                     "rock" = "rock", 
                     "cars" = "cars")),
      textInput("download_name", "Enter a title for the dataset:")),   #3.4 Add a text input which allows users to give a title to the selected dataset. Hint: use textInput() to create a text box input.
    mainPanel(                                                         #4. Create a Shiny layout that contains a main area for outputs. Hint: use mainPanel() to create a main area. 
      tabsetPanel(                                                     #4.1 In the main area, add two tabs: a "Data" tab and a "Plot" tab. Hint: use tabsetPanel() to create a tabset panel and tabPanel() to add tabs.
        tabPanel("Data",                                               #5. Create a text output to display the title of the selected dataset provided in (3.4). Hint: use textOutput() to create a text output. To add a heading to the title, use h1() - h6().
                 h2(textOutput("mydataname")),
                 br(),
                 dataTableOutput("table"),
                 downloadButton("downloadData", label ="Download")),
        tabPanel("PLot",
                 uiOutput("columns_to_select"),
                 plotOutput("histogram")),
        )
      )
    )
  )
  
  
  

  

  #5.1 Add a line break
  #5.2 Add a table output to display the selected dataset in (3.3). Hint: use DT::dataTableOutput() to create table output.
  #5.3 Add a download button to allow users to export the selected dataset in (3.3). Hint: use downloadButton() to create a download button.
  
  #In the Plot tab,
  #6. Create a selection of variables from the selected dataset in (3.3). Hint: use uiOutput() to create a placeholder for the selection output.
  #6.1 Add a plot output to produce a histogram of the variable selected in (6). Hint: use plotOutput() to create a plot output.
  

#Define a server logic####
server <- function(input, output, session){
  
  load_data <- reactive({
    req(input$rb_button)
    df <- get(input$rb_button)
    return(df)
  })
  
  load_histo <- function(df, col_to_use) {
    if(!is.null(col_to_use)){
      histo <- ggplot(df, aes(x = !!sym(col_to_use))) + 
        geom_histogram(bins=30)
      return(print(histo))
    }
  }
  
  #Display the title of the dataset. Hint use output$ and renderText()
  output$mydataname <- renderText({
    input$download_name
  })
  
  #Extract a list of variables from the selected dataset and create a set of select inputs from the variables. Hint: use output$ and renderUI()
  output$columns_to_select <- renderUI({
    df <- load_data()
    selectInput("a_column", "Select a variable to create a histogram", choices = append("", names(df)), selected = NULL, multiple = F)
  })
  
  #Retrieve the selected dataset from the user interface and display its data. Hint: use output$ and DT::renderDataTable()
  output$table <- DT::renderDataTable({
    load_data()
  })
  
  #Plot a histogram from the selected variable. Hint: use output$ and renderPlot()
  output$histogram <- renderPlot({
    req((input$a_column) %in% names(load_data())) #the previous input is still cached when user changes datasets, give it time to load ...
    load_histo(load_data(), input$a_column)
    
  }) 
  
  #After the download button is clicked, export the selected dataset. Hint: use output$ and downloadHandler()
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("", input$download_name, ".csv", sep="")
    },
    content = function(file) {
      write.csv(load_data(), file)
    }
  )
  
  
}

#Run the application####
shinyApp(server = server, ui = ui)

