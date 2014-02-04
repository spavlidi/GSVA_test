library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("GSVA"),
  
  sidebarPanel(

    h5("Type GSE ID",align = "left",style = "color:gray"),
    textInput("symb", "", "gse35014"),
    actionButton("get", "Select"),
    tags$style(type='text/css', "button#get { margin-bottom: 9px; }"),
    p("To browse GEO series, visit the ",
      a("GEO homepage.", 
        href = "http://www.ncbi.nlm.nih.gov/geo/",target="_blank")),
    
  
    br(),

    h5("Upload GeneSetCollection",align = "left",style = "color:gray"),
    fileInput("gset", "", multiple = FALSE, accept = NULL),



    actionButton("gsv", "Apply GSVA"),

    h6("___________________",align = "center",style = "color:gray"),
    
    uiOutput("saveui"),
    
 
    uiOutput("choose_columns_plot"),
    uiOutput("choose_columns_boxplot"),
    uiOutput("choose_cohort_plot"),
    
    br(),

    img(src="janssen_logo.jpg")
  ),


  mainPanel(
    
    ### show timer
    conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
                     img(src="loading_icon.gif"),
                     id='progressIndicator',
                     div(id='progress',includeHTML("timer.js"))
    ),
    
    tags$head(tags$style(type="text/css",
                         '#progressIndicator {',
                         '  position:fixed; top: 8px; right: 710px; top: 310px; width: 130px; height: 30px;',
                         '  padding: 8px; border: 1px transparent #CCC; border-radius: 8px;',
                         '}'
    )),
    

  tabsetPanel(
    tabPanel("Table",tableOutput("table")),
    tabPanel("Plot",plotOutput("plot")),
    tabPanel("BoxPlot",plotOutput("boxplot")),
    id = "selectedTab"
  )
    
    
    
    
    
    
    )

))