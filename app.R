### Application name : CA1 B9DA101 - Statistics for Data Analytics 
### Course : MSc (Data Analytics) - Sep 2019 - Group <<A>>  
### Developed by : Sidharth koparde(10521114) / Manoj Sahu(10519151) 
###/ Shilpa Rawat (10504995) / Kunal Merala (10520575)  
### College : Dublin Business School  
### URL : <<Published URL >> 

library(shiny)
library(ggplot2)
library(shinythemes)

###>>> Begin of processing >>> Sidharth Koparde (10521114) >>> 
#Processing the data
test <- read.csv("test.csv")
train <- read.csv("train.csv")

#There is no loss variable in test file. 
test$loss <- NA

test$type <- "test"
train$type <- "train"

alldata <- rbind(train, test)
alldata
###>>> End of processing >>> Sidharth Koparde (10521114) >>>

ui<-navbarPage("Statistics Data Analytics App ",
               
               
               ###>>> Begin of UI >>> Sidharth Koparde (10521114) >>>
               tabPanel("Allstate data Descriptive statistics",
                        sidebarLayout(
                        sidebarPanel(
                                
                        # select dataset 
                        selectInput("Dataset", "Train/Test", 
                                    choices = unique(alldata$type)),
                                
                        # select variable
                        selectInput("Variable", "Column", 
                                            choices = colnames(alldata)[-1]),
                                
                        # select plot type
                        selectInput("Plot", "Plot Type", 
                                    choices = c("Histogram", "QQplot", "Bar Plot"))
                        ),
                            
                        mainPanel(
                                tabsetPanel(
                                # The plot is called Descriptive and will be created in ShinyServer part
                                tabPanel("Plot",plotOutput("Descriptive")),
                                    
                                tabPanel("Summary", verbatimTextOutput("summary"))  
                                )
                            )
                        )
               ),
               ###>>> End of UI>>> Sidharth Koparde (10521114) >>> 
               ###>>> Begin UI Code  >>> Manoj Sahu (Student ID : 10519151 ) >>> 
               tabPanel("Discreate Random Models",
                        sidebarLayout(
                            sidebarPanel( 
                                
                                #RadioButtons Options for the Discrete Model
                                
                                radioButtons("discretemodel", "Select Model:", 
                                             c("Binomial" = "binomial",
                                               "Poisson" = "poisson",
                                               "Geometric" = "geometric"
                                             )),
                                # Values should be between M and L
                                conditionalPanel(                            
                                    condition = "input.discretemodel == 'binomial'", 
                                    numericInput("l", "Binomial Parameter - L" , value = 20), 
                                    numericInput("m", "Binomial Parameter - M" , value = 0.5) 
                                ), 
                                #Lambda Value for Poisson
                                conditionalPanel(                            
                                    condition = "input.discretemodel == 'poisson'", 
                                    numericInput("lamb", "Parameter lambda in Poisson" , value = 1) 
                                    
                                ), 
                                
                                #Default Input for Geometric
                                conditionalPanel(                             
                                    condition = "input.discretemodel == 'geometric'", 
                                    numericInput("n", "Parameter N in Geometric" , value = 0.5)
                                ), 
                                
                                
                                #Upper Limit for X
                                numericInput("max", "Upper Limit of X" , value = 5),  
                                
                                #Type or Select the inputs
                                selectInput("s", "Select the number of Stimulated Data" ,choices = c(1:1000)),  
                                
                                #Code for Conditional Panel based on selection of the Model.
                                
                                conditionalPanel( 
                                    condition = "input.discretemodel == 'binomial'", 
                                    numericInput("m1", "B for Binomial" , value = 1) 
                                ), 
                                conditionalPanel( 
                                    condition = "input.discretemodel == 'poisson'", 
                                    numericInput("m2", "P for Poisson" , value = 1)
                                ), 
                                conditionalPanel( 
                                    condition = "input.discretemodel == 'geometric'", 
                                    numericInput("m3", "G for geometric" , value = 1)
                                ) 
                            ),  
                            #Main Panel showing Histogram
                            mainPanel(  
                                plotOutput("histogram"),  
                                tableOutput('tab')  
                                
                            )
                            
                            
                        )),
###<<< End UI Code <<< Manoj Sahu (Student ID : 10519151 ) <<<  
###>>> Begin UI Code Shilpa >>> Shilpa Rawat (10504995) >>> 
tabPanel("Select Data for Hypothesis",
         sidebarLayout(
             
             
             
             sidebarPanel( #Sidebar panel for input
                 
                 
                 fileInput("file1", "Select a CSV file", # Select a file ----
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv",
                                      ".xlsx")),
                 
                 
                 tags$hr(),
                 
                 
                 checkboxInput("header", "Header", TRUE), #header checkbox
                 
                 
                 radioButtons("sep", "Separator", #selecting separator
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 
                 radioButtons("quote", "Quote", # selecting quote
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 
                 tags$hr(),
                 
                 
                 radioButtons("disp", "Display", # Rows to display
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head"),
                 
                 actionButton("SourceData", "Use Data!", class = "btn-primary")
                 
             ),
             
             
             mainPanel( #main panel for outputs
                 
                 
                 tableOutput("contents"),
                 textOutput("ColNames")
                 
             )
             
             
         )
),


tabPanel("Hypothesis Testing",  
         
         
         
         sidebarPanel( 
             
             sliderInput("bins", 
                         
                         "Size of Graph", 
                         
                         min = 1, 
                         
                         max = 50, 
                         
                         value = 2 
                         
             ), 
             
             radioButtons("sample", 
                          
                          "Select the T-test type", 
                          
                          choices = c("One Sample Test" = "oneSamp","Two Sample Test" = "twoSamp")), 
             
             selectInput("var1",  
                         
                         label = "Select a Numerical Value", 
                         
                         "Loading..." 
                         
             ), 
             
             conditionalPanel(condition = "input.sample == 'twoSamp'", 
                              
                              selectInput("var2",  
                                          
                                          label = "Select a Numerical Value", 
                                          
                                          "Loading..." 
                                          
                              ), 
                              
                              radioButtons("varequal", 
                                           
                                           "Two samples have equal variance : ", 
                                           
                                           choices = c("Yes" = "y", 
                                                       
                                                       "No" = "n")) 
                              
             ), 
             
             selectInput("tail", 
                         
                         label = "Select a relation to be tested", 
                         
                         choices = c("Equal" = "two.sided",  
                                     
                                     "Less" = "less", 
                                     
                                     "Greater" = "greater")), 
             
             conditionalPanel(condition = "input.sample == 'oneSamp'", 
                              
                              numericInput("test", 
                                           
                                           "Choose the Mean Value to Test", 
                                           
                                           value = 0 
                                           
                                           
                                           
                              ) 
                              
             ), 
             
             numericInput("conf", 
                          
                          label = "Select the confidence level:", 
                          
                          value = 0.95, 
                          
                          min = 0.8, 
                          
                          max = 0.99), 
             
             helpText("Important: Kindly assign a number between 0 and 1 in the numeric Input") 
             
             
             
             
             
         ), 
         
         mainPanel( 
             
             fluidRow(column(10, offset = 1, 
                             
                             plotOutput("graph1"))), 
             
             hr(), 
             
             fluidRow(column(8, offset = 1,
                             
                             hr(), 
                             
                             h2("Summary"), 
                             
                             paste("Observed sample statistics:"), 
                             
                             tableOutput("parametric"), 
                             
             ) 
             
             ) 
             
             
             
         ) 
         
),
###>>> End of UI COde >>> Shilpa >>> 
###>>> Begin of UI COde >>> Kunal Merala(10520575) >>> 
#UI Code to import Dataset using csv files
tabPanel(" GLM /Data Import",
         sidebarLayout(sidebarPanel( 
             fileInput("file","Upload your CSV",multiple = FALSE),
             tags$hr(),
             h5(helpText("Select the read.table parameters below")),
             checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
             checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
             radioButtons(inputId = 'sep', label = 'Separator', 
                          choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
         ),
         mainPanel(uiOutput("tb1"))
         ) ),
#UI Code to Display GLM Models

tabPanel("GLM / Linear and Logistic Models",
         sidebarLayout(sidebarPanel(
             h5(helpText("Select the Parameter's for Linear model")),
             uiOutput("model1"),
             uiOutput("var1_select"),
             uiOutput("rest1_var_select"),
             tags$hr(),
             h5(helpText("Select the Parameter's for Logistic model")),
             uiOutput("model2"),
             uiOutput("var2_select"),
             uiOutput("rest2_var_select")
         ),
         mainPanel( helpText("Your Selected variables for Linear Model"),
                    verbatimTextOutput("other1_val_show"),
                    helpText("Your Selected variables for Logistic Model"),
                    verbatimTextOutput("other2_val_show")))),
#UI Code to genrate Scatter Plot's

tabPanel("GLM / Scatter Plot",
         sidebarLayout(sidebarPanel(
             uiOutput("Select_X_AxisLi"),
             uiOutput("Select_Y_AxisLi"),
             uiOutput("Select_X_AxisLo"),
             uiOutput("Select_Y_AxisLo")),
             mainPanel(helpText("Scatter plot for Linear Model"),
                       plotOutput("scatterplot1"),
                       helpText("Scatter plot for Logistic Model"),
                       plotOutput("scatterplot2")
             )
         )),

# UI Code to generate Histogram.

tabPanel("GLM / Distribution",
         sidebarLayout(sidebarPanel(
             uiOutput("Select_hist1"),
             uiOutput("Select_hist2")),
             mainPanel(helpText("Histogram"),
                       fluidRow(
                           column(6, plotOutput("distribution1")),
                           column(6, plotOutput("distribution2")))        
             )))


)

###<<< END of UI Code <<< Kunal Merala(10520575) <<<



server<- function(input, output, session){
    ###>>> Begin of Server >>> Sidharth Koparde (10521114) >>>
    
    output$Descriptive <- renderPlot({
        
        # subset of data
        plotdata <- alldata[ alldata$type == input$Dataset, input$Variable]
        
        # choose the type of plot
        if (input$Plot == "Histogram"){
            
            # whether the variable is continuous or not
            if (substr(input$Variable, 1,4) == "cont"){ 
                
                # histogram for continuous variable
                ggplot(data.frame(plotdata),aes(x=plotdata))+ geom_histogram()
                
            } else {
                
                # barplot for categorical variable
                ggplot(data.frame(plotdata),aes(x=plotdata))+ geom_bar()
                
            }
            
            # if select the QQplot
        } else if (input$Plot == "QQplot") {
            
            # whether the variable is continuous or not
            if (substr(input$Variable, 1,4) == "cont"){
                
                # QQplot for continuous variables
                ggplot(data.frame(plotdata),aes(sample=plotdata)) + stat_qq()
                
            }}
    })
    output$summary<- renderPrint({
        sumdata<- alldata[ alldata$type == input$Dataset]
        summary(sumdata)
    })
###>>> End of Server >>> Sidharth Koparde (10521114) >>>
###<<< Begin  Server  Code <<< Manoj Sahu (Student ID : 10519151 ) <<<  
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Probabilistic model <<<<<<<<<<<<<<<<<<<<<<#
    output$histogram <- renderPlot({ 
        
        
        
        # code for binomial plot
        
        if (input$discretemodel == 'binomial') { 
            
            par(mfrow=c(1,2))  
            
            d <- density(rbinom(1000,input$l,input$m))  
            
            plot(d, main="KERNEL DENSITY FOR THE GENERATED DATA")  
            
            polygon(d, col="green", border="red") 
            
            x=0:input$l  
            
            plot(x,dbinom(x,input$l,input$m))  
            
            
            
        } 
        
        
        
        
        
        # code for poisson plot
        
        
        
        if (input$discretemodel == 'poisson') { 
            
            par(mfrow=c(1,2))   
            
            D=rpois(input$s, input$lamb)  
            
            tab=table(D)  
            
            barplot(tab,col='green')  
            
            x1=0:input$max  
            
            y1=dpois(x1,input$lamb)  
            
            plot(x1,y1,type='b')  
            
        } 
        
        # code for geometric  plot
        
        if (input$discretemodel == 'geometric') { 
            
            par(mfrow=c(1,2)) 
            
            D=rgeom(input$s, input$n)  
            
            tab=table(D)  
            
            barplot(tab,col='green')  
            
            x2=0:input$max  
            
            y2=dgeom(x2,input$n)  
            
            plot(x2,y2,type='b')  
            
        } 
        
        
        
    })    
    
    
    #output tab showing vector of inputs for binomial,poisson and geometric model
    
    output$tab <- renderTable({  
        
        
        
        p1=dbinom(input$m1,input$l, input$m)  
        
        p2=dpois(input$m2,input$lamb)  
        
        p3=dgeom(input$m3,input$n)  
        
        c(p1,p2,p3) 
        
        
        
        
        
    })
    
    ###<<< End Server  Code <<< Manoj Sahu (Student ID : 10519151 ) <<<      
    ###>>> Begin Server  Code >>> Shilpa Rawat (Student ID : 10504995 ) >>>
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    
    
    general_df <- reactive({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                
                stop(safeError(e))
            }
        )
        
        
    })
    
    output$contents <- renderTable({
        
        
        
        
        if(input$disp == "head") {
            return(head(general_df()))
        }
        else {
            return(general_df())
        }
        
    })
    
    
    
    output$ColNames <- renderText({  # display column names
        
        colnames(general_df())
        
    })
    
    observe({
        
        cacheColName <- colnames(general_df())
        updateSelectInput(session, "DataForAnalysis", choices = cacheColName)
        updateSelectInput(session, "DataForSpread", choices = cacheColName)
        updateSelectInput(session, "XAxis", choices = cacheColName)
        updateSelectInput(session, "YAxis", choices = cacheColName)
        updateSelectInput(session, "var1", choices = cacheColName)
        updateSelectInput(session, "var2", choices = cacheColName)
        
        
    })
    
    output$DisplayMean <- renderText({
        
        Cachedf <- general_df()
        DtMean <- input$DataForAnalysis
        mean(Cachedf[[DtMean]])
    })
    
    output$DisplayMedian <- renderText({
        
        Cachedf <- general_df()
        DtMedian <- input$DataForAnalysis
        median(Cachedf[[DtMedian]])
    })
    
    Mode = function(x){
        ta = table(x)
        tam = max(ta)
        if (all(ta == tam))
            mod = NA
        else
            if(is.numeric(x))
                mod = as.numeric(names(ta)[ta == tam])
        else
            mod = names(ta)[ta == tam]
        return(mod)
    }
    
    output$DisplayMode <- renderText({
        
        Cachedf <- general_df()
        DtMode <- input$DataForAnalysis
        Mode(Cachedf[[DtMode]])
    })
    
    
    output$ScatterOutput <- renderPlot({
        
        
        Cachedf <- general_df()
        library(car)
        scatterplot(Cachedf[[input$XAxis]],Cachedf[[input$YAxis]], main="Scatterplot", col = 'green',
                    xlab=Cachedf[[input$XAxis]], ylab=Cachedf[[input$YAxis]], pch=19)
        
        
    })
    
    output$HistogramOutput <- renderPlot({
        
        
        Cachedf <- general_df()
        hist(Cachedf[[input$XAxis]], main="Histogram", 
             xlab="Selected Column", 
             border="blue", 
             col="green",
             breaks=7)
    })
    
    var.p = function(x){var(x)*(length(x)-1)/length(x)}
    
    output$DisplayVariance <- renderText({
        
        Cachedf <- general_df()
        DtVariance <- input$DataForSpread
        var.p(Cachedf[[DtVariance]])
    })
    
    sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
    
    output$DisplayStd <- renderText({
        
        Cachedf <- general_df()
        DtStd <- input$DataForSpread
        sd.p(Cachedf[[DtStd]])
        
    })
    
    
    
    
    output$graph1 <- renderPlot({
        var1 <- general_df()[,input$var1]
        var2 <- general_df()[,input$var2]
        if (is.null(var1)){return(NULL)}
        if (is.null(var2)){return(NULL)}
        graph2 <- ifelse(input$sample == 'oneSamp', FALSE, TRUE)
        p1 <- hist(var1, breaks = input$bins)
        p2 <- hist(var2, breaks = input$bins)
        plot(p1, col=rgb(0,0,1,1/4))
        if(input$sample == 'twoSamp')
            plot(p2, col=rgb(1,0,0,1/4),add = graph2)
    })
    
    output$parametric <- renderTable({
        var1 <- general_df()[,input$var1]
        if (is.null(var1)){return(NULL)}
        var2 <- general_df()[,input$var2]
        if (is.null(var2)){return(NULL)}
        mean1 <- mean(var1)
        mean2 <- mean(var2)
        standard_deviation1 <- sd(var1)
        standard_deviation2 <- sd(var2)
        standard_error1 <- sd(var1)/sqrt(length(var1))
        standard_error2 <- sd(var2)/sqrt(length(var2))
        parametric1 <- data.frame(mean = mean1, 
                                  standard_deviation=standard_deviation1, 
                                  standard_error=standard_error1)
        row.names(parametric1) <- input$var1
        parametric2 <- data.frame(mean = mean2, 
                                  standard_deviation=standard_deviation2, 
                                  standard_error=standard_error2)
        row.names(parametric2) <- input$var2
        if(input$sample == "oneSamp") {return(parametric1)}
        if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
    })  
    
    # Create a one sample and two sample t-test reactive function
    ttestout <- reactive({
        var1 <- general_df()[,input$var1]
        conf <- input$conf
        if (is.null(var1)){return(NULL)}
        t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
        var2 <- general_df()[,input$var2]
        if (is.null(var2)){return(NULL)}
        ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
        t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve, conf.level = conf)
        if(input$sample == "oneSamp") {return(t1)}
        if(input$sample == "twoSamp") {return(t2)}
        
    })
    
    # Output of one sample t value of t-test
    output$tvalue <- renderPrint({
        vals <- ttestout()
        if (is.null(vals)){return(NULL)}
        vals$statistic
    })
    
    # Output of p value
    output$pvalue <- renderPrint({
        vals <- ttestout()
        if (is.null(vals)){return(NULL)}
        vals$p.value 
    })
    ###<<< End Server  Code <<< Shilpa Rawat (Student ID : 10504995 ) <<<
    ###<<< Begin Server  Code <<< Kunal Merala (Student ID : 10520575 ) <<<
    data1 <- reactive({        #Storing File input in a reactive function
        file11 <- input$file
        if(is.null(file11)){return()} 
        read.table(file=file11$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
        
    })  
    output$table <- renderTable({ #Gatherring Data in a Table
        if(is.null(data1())){return ()}
        data1()
    })
    output$tb1 <- renderUI({ #DIsplaying Data in the Output Table
        tableOutput("table")
    })
    
    #Generating Inputs Based on the CSV Uplaoded for GLM Models
    
    output$model1<-renderUI({
        selectInput("modelselect","Linear Model",choices = c("Linear Model"="linreg"))
        
    })
    output$model2<-renderUI({
        selectInput("modelselect","Logistic Model",choices = c("Logistic Model"="logreg"))
        
    })
    
    output$var1_select<-renderUI({
        selectInput("ind1_var_select","Select Independent Var", choices =as.list(names(data1())),multiple = FALSE)
    })
    
    output$var2_select<-renderUI({
        selectInput("ind2_var_select","Select Independent Var", choices =as.list(names(data1())),multiple = FALSE)
    })
    
    
    output$rest1_var_select<-renderUI({
        checkboxGroupInput("other1_var_select","Select other Var",choices =as.list(names(data1())))
    })
    
    output$rest2_var_select<-renderUI({
        checkboxGroupInput("other2_var_select","Select other Var",choices =as.list(names(data1())))
    })
    
    
    #Generating and Displaying Data for Linear Model
    
    output$other1_val_show<-renderPrint({
        input$other1_var_select
        input$ind1_var_select
        f<-data1()
        
        library(caret)
        form <- sprintf("%s~%s",input$ind1_var_select,paste0(input$other1_var_select,collapse="+"))
        print(form)
        
        linreg <-lm(as.formula(form),data=f)
        print(summary(linreg))
        
    })
    
    #Generating and Displaying Data for Logistic Model
    
    output$other2_val_show<-renderPrint({
        input$other2_var_select
        input$ind2_var_select
        f1<-data1()
        
        library(caret)
        form1 <- sprintf("%s~%s",input$ind2_var_select,paste0(input$other2_var_select,collapse="+"))
        print(form1)
        
        logreg <-glm(as.formula(form1),family=binomial(),data=f1)
        print(summary(logreg))
        
    })
    
    #Generating UI Based on CSV files for Scatter plot Linear Model
    
    output$Select_X_AxisLi<-renderUI({
        selectInput("Select_XLi","Select X Axis For Linear Model",  choices =as.list(names(data1())),multiple = FALSE)
    })
    
    output$Select_Y_AxisLi<-renderUI({
        selectInput("Select_YLi","Select Y Axis For Linear Model",  choices =as.list(names(data1())),multiple = FALSE)
    })
    
    #Generating UI Based on CSV files for Scatter plot Logistic Model
    
    output$Select_X_AxisLo<-renderUI({
        selectInput("Select_XLo","Select X Axis for Logistic Model",  choices =as.list(names(data1())),multiple = FALSE)
    })
    
    output$Select_Y_AxisLo<-renderUI({
        selectInput("Select_YLo","Select X Axis for Logistic Model",  choices =as.list(names(data1())),multiple = FALSE)
    })
    
    #Generating and displaying scatter plot for Linear model
    
    output$scatterplot1 <- renderPlot({
        file2<- data1()
        library(caret)
        form2 <- sprintf("%s~%s",input$Select_YLi,paste0(input$Select_XLi,collapse="+"))
        
        linereg1 <-lm(as.formula(form2),data=file2)
        
        plot(file2[,input$Select_XLi], file2[,input$Select_YLi], main="Scatterplot",
             xlab=input$Select_XLi, ylab=input$Select_YLi, pch=19)
        abline(linereg1,col="red")
        lines(lowess(file2[,input$Select_XLi], file2[,input$Select_YLi]),col="blue")
        
        
    }, height=400)
    
    #Generating and displaying scatter plot for Logistic model
    
    output$scatterplot2 <- renderPlot({
        file3<- data1()
        library(caret)
        form3 <- sprintf("%s~%s",input$Select_YLo,paste0(input$Select_XLo,collapse="+"))
        
        
        logreg1 <-glm(as.formula(form3),family=binomial(),data=file3)
        
        plot(file3[,input$Select_XLo], file3[,input$Select_YLo], main="Scatterplot",
             xlab=input$Select_XLo, ylab=input$Select_YLo, pch=19)
        abline(logreg1,col="red")
        lines(lowess(file3[,input$Select_XLo], file3[,input$Select_YLo]),col="blue")
    }, height=400)
    
    #Generating UI Based on CSV files for Histogram.
    
    output$Select_hist1<-renderUI({
        selectInput("Select_dist1","Select Distribution for Histogram 1",  choices =as.list(names(data1())),multiple = FALSE)
    })
    
    output$Select_hist2<-renderUI({
        selectInput("Select_dist2","Select Distribution for Histogram 2",  choices =as.list(names(data1())),multiple = FALSE)
    })
    
    #Generating and displaying histogram.
    
    output$distribution1 <- renderPlot({
        file4<- data1()
        
        hist(file4[,input$Select_dist1], main="" , xlab=input$Select_dist1)
    }, height=300, width=300)
    
    output$distribution2 <- renderPlot({
        file5<- data1()
        hist(file5[,input$Select_dist2], main="" , xlab=input$Select_dist2)
    }, height=300, width=300)
    ###<<< End Server  Code <<< Kunal Merala (Student ID : 10520575 ) <<<
}
shinyApp(ui = ui, server = server)