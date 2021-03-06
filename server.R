source("helper.R")
library("hgu133plus2.db")
library("hgu133a.db")
library(shiny)
library(GEOquery)
#library(affy)
library(GSVA)
library(ggplot2)
#library(GEOmetadb)
#library(CellMix)

shinyServer(function(input, output) {

  
#Download microarray data
  dataInput <- reactive({  
    if(input$get == 0) return(NULL)
    
    isolate({

# #       ## Perform GEO query  
#       dataset<-getGEO(input$symb)[[1]]
#       dataset<-add_annotation(dataset)
#       print("dowloading: ")
#       print(input$symb)
#   
#       return(dataset)
      
      
      #annotation(dataset) <- "hgu133plus2.db"
      #save(dataset,file="data/gse.r")
      
#       ##load local GSE15258 (R Object - ExpressionSet)     
      print("loading localy stored dataset gse15258")
      dataset<-get(load("data/gse15258_2.r"))

#       print("loading localy stored dataset gse5553.r")
#       dataset<-get(load("data/gse5553.r"))


# #          
#       print("loading localy stored dataset gse35014")
#       dataset<-get(load("data/gse35014.r"))
#       dataset<-dataset[[1]]
#       dataset<-add_annotation(dataset)

      
       # print(pData(dataset));return(dataset)
      
    })
  })


#Upload gene set collection
  gsetInput <- reactive({ 
  
    if(is.null(input$gset)) return(NULL)
  
    isolate({
    print("Loading gene signatures")
    genesetFile <- input$gset    
    gsc<-read_signatures(genesetFile[[4]])
    
    print(names(gsc));return(gsc)
    
                        })
            })


#Apply GSVA
  gsvaInput <- reactive({ 
    
   if(input$gsv == 0 || is.null(input$gset) || input$get==0)  return(NULL)
   
   isolate({
   print("Inside Apply GSVA") 
   esdf<-applyGSVA(dataInput(),gsetInput())
   print("Formating ES table")
    fesdf<-formatesdf(esdf) 
    return(fesdf) 
  })
})
 



#display select boxes for plotting
output$choose_columns_plot <- renderUI({
 
  print(input$selectedTab)
  
  if(input$gsv == 0 || input$selectedTab != "Plot" ) return()
  
    # Choose signatures to plot
  isolate({
    print("Choose columns to plot")
    col_names <- colnames(gsvaInput())
    

    
    sig_cols <- col_names[grep(1,as.numeric(lapply(gsvaInput(),is.numeric)))]
 
#     # Create the checkboxes
#     checkboxGroupInput("columns", "Select signatures", 
#                        choices  = sig_cols,
#                        selected = NULL)
c1<-selectInput("columns1", "Select columns to plot", 
            choices  = sig_cols,
            selected = sig_cols[1])

c2<-selectInput("columns2", "Select columns to plot", 
            choices  = sig_cols,
            selected = sig_cols[2])

c<-c(c1,c2)


  })
})





output$choose_cohort_plot <- renderUI({

  if(input$gsv == 0 || input$selectedTab != "BoxPlot") return()
  
  # Choose column representing cohorts of interest
  print("Choose cohort column (factor)")
  
  col_names <- colnames(gsvaInput())
  
  
  
  factor_cols <- col_names[grep(1,as.numeric(lapply(gsvaInput(),is.factor)))]
  factor_cols=factor_cols[2:length(factor_cols)]
  # Create the select cohort box
  selectInput("cohort", "Select cohort column", 
              choices  = factor_cols,
              selected = factor_cols[1])

})

#display signature for boxplotting
output$choose_columns_boxplot <- renderUI({
  
  print(input$selectedTab)
  
  if(input$gsv == 0 || input$selectedTab != "BoxPlot" ) return()
  
  # Choose signatures to plot
  isolate({
    print("Choose columns to plot")

    col_names <- colnames(gsvaInput())
    
    
    
    sig_cols <- col_names[grep(1,as.numeric(lapply(gsvaInput(),is.numeric)))]
    
    # Create the checkboxes
    selectInput("sig_box", "Select signature", 
                choices  = sig_cols,
                selected = sig_cols[1])

  })
})

#Plot based on selected boxes
output$plot <- renderPlot({

  if(is.null(gsvaInput()))
    return()
  
  
  # Get the data set
  dat <- gsvaInput()
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  if (is.null(input$columns1) || is.null(input$columns2))
    {print(input$columns1);print(input$columns2);return()}
  isolate({
  # Keep the selected columns
  dat <- dat[, c(input$columns1,input$columns2), drop = FALSE]

  print("Inside Plot")
  # Plot

  showplot(dat,input$columns1,input$columns2)
  
  
  })
})






#BoxPlot based on selected boxes
output$boxplot <- renderPlot({
  
  if(is.null(gsvaInput()) || is.null(input$sig_box) || is.null(input$cohort))
    return()
  
  # Get the data set
  dat <- gsvaInput()
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
#   if (is.null(input$sig_box))
#     return()
  isolate({
      
  # Keep the selected columns along with slected cohort as last column
  dat <- dat[, c(input$sig_box,input$cohort), drop = FALSE]
  
  # BoxPlot
  
 
  iny<-grep(input$sig_box,colnames(dat))
  showboxplot(dat,ncol(dat),iny)

  
  })
})


#Display ES table
  output$table <- renderTable({
    if(input$gsv == 0) return(NULL)
      
      isolate({
      print("Inside Table")
        gsvaInput()
        
      })      
  })

#display select boxes for plotting
output$testui <- renderUI({
  
  print(input$selectedTab)
  
  if(is.null(gsvaInput()) || input$selectedTab != "Table" ) return()
  
  # Choose signatures to plot
  isolate({

#     col_names <- colnames(gsvaInput())
#     
#     
#     
#     sig_cols <- col_names[grep(1,as.numeric(lapply(gsvaInput(),is.numeric)))]
    
    # Create the checkboxes
     actionButton("save", "Save table")
    
    
    
  })
})

observe({
  
  if(input$save == 0 || is.null(input$save)) return()
  
  isolate({
  print(input$save)
  write.csv(gsvaInput(),file="C:/Users/user/Documents/test.csv")
  print("File saved in Documents")

          })
  
})


})