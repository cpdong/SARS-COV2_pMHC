library(shiny)
library(DT)
library(ggplot2)
library(ggpmisc)
library(shinydashboard)

# Define UI for data download app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Scatterplot with regression line equation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      # Input: Choose dataset ----
      selectInput(inputId = "inputX",
                  label = "Choose the X-axis column:",
                   ""),
      selectInput(inputId = "inputY",
                  label = "Choose the Y-axis column:",
                   ""),
        br(),
      fluidRow(
          column(6,style=list("padding-right: 5px;"),
                selectInput(inputId = "cexSize",
                  label = "Set Point Size:",
                  choices = c(0.5, 1, 1.5, 2,2.5, 3,3.5,4,5))
          ),
          column(6,style=list("padding-left: 5px;"),
                    selectInput(inputId = "pointShape",
                  label = "Shape of Points:",
                  choices = c(1,8,16,19,21,23)))),

      #sliderInput(inputId = "xlim",label = "Set the xlim", value= 10, min = 0, max=30),
      #sliderInput(inputId = "ylim",label = "Set the ylim", value= 30, min = 0, max=60),

      br(),
      h5("Download plot as PDF file"),
      downloadButton("downloadPlot", "Download"),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

       #textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),

      plotOutput("thePlot", height=500),
      hr(),
      br(),
      br()

    )
  )
)



server <- function(input, output, session) { # added session for updateSelectInput

    inFile <- reactive({
        if (is.null(input$file1)) {
            return(NULL)
        }else{
            input$file1
        }
    })

    myData <- reactive({
        if (is.null(inFile())) {
            return(NULL)
        }else{
            read.csv(inFile()$datapath, header = TRUE)
        }
    })

    observe({
         updateSelectInput(session, "inputX", 
                           choices=names(myData())[-1], selected = names(myData())[2])
         updateSelectInput(session, "inputY", 
                           choices = names(myData())[-1], selected = names(myData())[2])
         updateSelectInput(session, "cexSize", 
                           choices = c(0.5, 1.0, 1.5,2.0,2.5,3.0,3.5,4.0,5.0), selected = 1.0)
         updateSelectInput(session, "pointShape", 
                           choices = c(1,8,16,19,21,23), selected = 16)
#          min = 5, max = ceiling(myData()[, colnames(myData()) %in% input$inputX]/5)*5)
    })

    # Define server logic to display and download selected file ----
    #output$table <- DT::renderDataTable(datatable(myData(), 
    #     options = list(pageLength = 5,lengthChange=FALSE)) %>% formatRound(c(2:7), 3))

    inputX <- reactive({ input$inputX })
    inputY <- reactive({ input$inputY })
    cexSize <- reactive({ input$cexSize })
    pointShape <- reactive({ input$pointShape })

    output$text2 <- renderText({paste("X axis using: ", inputX(),sep="")})
    output$text3 <- renderText({paste("Y axis using: ", inputY(),sep="")})

    myPlot<- function(inputX, inputY, cexSize, pointShape){
		data<- myData()[,c(inputX,inputY)]
		names(data)<- c("x", "y")
		#
		tt<- cor.test(data[,1], data[,2], method='spearman')
		rho<- round(as.numeric(tt$estimate),3)
		pval<- round(tt$p.value,4)
		p<- ggplot(data, aes(x=x, y=y)) + xlab(inputX) + ylab(inputX) + 
		geom_smooth(method="lm", se=TRUE, formula = y ~ x) + 
		stat_poly_eq(formula = y ~ x, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +  
		geom_point(size=as.numeric(cexSize), shape=as.numeric(pointShape)) + 
		theme_bw() + 
		theme(axis.text=element_text(size=13), axis.title=element_text(size=14));
		
		p1<- p + annotate("text",  x=Inf, y = Inf, vjust=1.5, hjust=1.2,
				label=paste("rho = ",rho,"\n", "p = ", pval, sep=""),size=5)
		p1
    }

    output$thePlot = renderPlot({
        myPlot(input$inputX, input$inputY, input$cexSize, input$pointShape)
    })

    # Downloadable the generated plot ----
    output$downloadPlot <- downloadHandler(
        filename = function() { paste("result", "ggplot.pdf",sep="_") },

        content = function(file) {
            pdf(file) # open the pdf device
            print(myPlot(input$inputX, input$inputY, input$cexSize, input$pointShape))
            dev.off()  # turn the device off
    })


}

# Create Shiny app ----
shinyApp(ui, server)
