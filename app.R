## ==================================================================================== ##
# ShinyAIM for visualization of interactive Manhattan plots of longitudnal GWAS data.

#Copyright (c) 2018, Waseem Hussain,  code licensed under Artistic License 2.0.
#This license establishes the terms under which a given free software Package may be copied,
#modified, distributed, and/or redistributed. The intent is that the Copyright Holder maintains
#some artistic control over the development of that Package while still keeping the Package
#available as open source and free software.
#You are always permitted to make arrangements wholly outside of this license directly with the
#Copyright Holder of a given Package. If the terms of this license do not permit the full use that
#you propose to make of the Package, you should contact the Copyright Holder and seek a
#different licensing arrangement.
# For more details see Artistic License 2.0. at https://opensource.org/licenses/Artistic-2.0
# 
# You may contact the author of this code, Waseem Hussain, at <waseem.hussain@ unl.edu>
# You can run the application by clicking the 'Run App' button above in R studio.
#================================================================================================ ##

# Load the required packages
  
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(plotly)
  library(manhattanly)
  library(forcats)

# Define UI for the application, for more information on it please see https://shiny.rstudio.com/gallery/
  
      ui<- fluidPage(

# Create the Application Title uing headerPanel and formate it
  
        #headerPanel(h1("ShinyAIM: Shiny Application for Interactive Manhattan Plots", style = "font-family: 'Trattatello', fantasy; font-weight: 500; line-height: 1.1; color: #D2691E;", align = "center")),
        tags$head(
        tags$style(HTML("
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
         h1 {
        font-family: 'Trattatello', fantasy;
        font-weight: 500;
        line-height: 1.1;
       color: #D2691E;
      align = 'center'
      }

    "))
        ),
    headerPanel("ShinyAIM: Shiny Application for Interactive Manhattan Plots"),
        
# Blocks printing any errors in the Shiny UI.
  
     tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
# Create tabset and tabPanels for the application

    tabsetPanel(

# First tabpanel is the "Information" and all the information for application can be found in Information.R file
    
    tabPanel(
      h4("Information", style = "color: #800080;"),
      #img(src = 'image.png', align = "right", width = "50%", height = "50%"),
      source("Information.R", local = TRUE)[1]
    ),
    
# Creat tabpanel Interactive Manhattan plots

    tabPanel(
      h4("Interactive Manhattan Plots", style = "color: #800080;"),
      
# Within this tabpanel sidebar layout and  sidebar panel is framed
      
      sidebarLayout(
      sidebarPanel(width = 3,
                   
# Data upload button is created

      fileInput('file1', 'Upload Data File for Interactive Manhattan Plots:',
                               accept=c('text/csv','text/comma-separated-values,text/plain')),

# Check wheather file has header or not

      checkboxInput('header', 'Data File has Variable Names as Column Headers.', TRUE),

#Data file seperator 

      radioButtons('sep', 'Data File Separator Value:',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t')
                     ),

# uioutput creates the button where user can control the input of file

      uiOutput("manOutput"),
      uiOutput("sumOutput"),

# Sliderinput button to allow users to choose significance level

      sliderInput("logpvalue", "Choose -log 10 p-value:",
                                 min = -log10(0.01), max = -log10(0.00000001),
                                 value = -log10(0.00001), step=0.5),
# Sliderinput button to allow users to display top significant SNPs

conditionalPanel(
# Displays the SNPs with highest -logpValue
  condition = "input.sum",
      sliderInput("p", "How many Significant SNPs to be Displayed in Table:",
                                 min = 1, max =80,
                                  value = 2, step=1))

                                   ),

# main panel reserves space for the plot 

      mainPanel(h4("Interactive Manhattan Plot", align = "center"),
          plotlyOutput("mymanhattan"),
          br(),
          hr(),
          conditionalPanel(
            
# Displays the SNPs in table arranged with highest -logpValue 
            
            condition = "input.sum", 
            tags$h4("Markers Arranged in Significance Order", align = "center"),
            verbatimTextOutput("summary"))
      )
          
      )),


 #============================MANHATTAN GRID PLOTS=================================================#
    
      tabPanel(
      h4("Manhattan Grid Plot", style = "color: #800080;"),
      sidebarLayout(

 # Data upload button is created
        
      sidebarPanel(width = 3,
                     fileInput('file2', 'Upload Data File for Manhattan Grid Plot:',
                               accept=c('text/csv','text/comma-separated-values,text/plain')),
                   
# Check wheather file has header or not

      checkboxInput('header', 'Data File has Variable Names as Column Headers.', TRUE),
                     radioButtons('sep', 'Data File Separator Value:',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t')
                     ),

# uioutput creates the button where user can control the input of file

      uiOutput("gridOutput"),
      
# Sliderinput button to allow users to choose significance level

      sliderInput("pvalue", "Choose -log 10 p-value:",
                                 min = -log10(0.01), max = -log10(0.00000001),
                                 value = -log10(0.00001), step=0.5),

# Sliderinput button to allow users to choose the number of columns in grid plot

     sliderInput("ncol", "Select the Number of Columns for Grid Plot:",
                                 min = 2, max =10,
                                 value =4, step=1)
        ),


# main panel reserves a for the plot

    mainPanel(align="center",
                  tags$h4("Manhattan Grid Plot", align = "center"),
                  plotOutput("mygrid", height=800))
      )),


#============================Compare only Associated Markers Across Time Points========================================#
tabPanel(
  h4("Comparison of Associated Markers", style = "color: #800080;"),
  sidebarLayout(
    
# Frame side bar layout
    
    sidebarPanel(width = 3,
                 #fileInput('file3', 'Upload Data File for Combined Manhattan Plot:',
                           #accept=c('text/csv','text/comma-separated-values,text/plain')),
                 
                 # Check wheather file has header or not
                 
                 #checkboxInput('header', 'Data File has Variable Names as Column Headers.', TRUE),
                 #radioButtons('sep', 'Data File Separator Value:',
                            #  c(Comma=',',
                               # Semicolon=';',
                                #Tab='\t')
                 uiOutput("signiOutput"),
                 hr(),
                 tags$h5("To see the interactive plot and compare the associated markers across timepoints or different phenotypes, upload the data file in Manhattan Grid Plots data browse box. It uses the same data file. To modify or change the plot based on p value, directly enter the value by typing in the box", align = "center")
                 ),
                
    
    
 # main panel reserves a space for the plot
    
    mainPanel(align="center",
              tags$h4("Compare Associated Markers Across Timepoints", align = "center"),
              hr(),
              tags$h6("Shapes and colors represent timepoints or phenotypes", align = "center", style = "color: darkred;"),
              plotlyOutput("mysig", height=600))
  )),

#============================PHENOTYPIC DATA VISUALIZATION========================================#
    
    tabPanel(
    h4("Phenotypic Data Visulaization", style = "color: #800080;"),
    sidebarLayout(
        
    sidebarPanel(width = 3,
                     fileInput('file4', 'Upload Data File:',
                               accept=c('text/csv','text/comma-separated-values,text/plain')),
                     
    checkboxInput('header', 'Data File has Variable Names as Column Headers.', TRUE),
    radioButtons('sep', 'Data File separator value:',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t')),
                     
    uiOutput("timeOutput"),
                     
    selectInput("plot.type","Plot Type:",
                                 c(Histogram = "histogram", Density="density", DensityAll="densityall", Boxplot = "boxplot"))
        ),
        
    mainPanel(align="center",
                  tags$h2("", align = "center"),
                  plotlyOutput("plot")))
      
      
    )        
  )
)
  

      
# Choose the size of shiny app
      
    options(shiny.maxRequestSize = 100*1024^2)

# Define the Server part for application

    server <- function(input, output) {

#==============================INTERACTIVE MANHATTAN PLOTS========================================#
# read the file if uploaded otherwise return null
  
  read1 <- reactive({
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    data <- read.csv(inFile1$datapath, 
                     header=input$header,
                     na.strings = input$na.strings,
                     sep=input$sep)
    return(data)
    
# timepoint as factor
    
    data$timepoint<-as.factor(data$timepoint)
    })
  
# Select the timepoint by using selectinput value
  
    output$manOutput <- renderUI({
    if (is.null(read1()))
      return(NULL)
    selectInput("man", "Choose Time Point or Phenotypes", unique(read1()$timepoint), selected = "")
    })
    
# Make the data reactive and filter it based on timepoints in the uploaded file
    
    data1<-reactive({
    if (is.null(read1()))
      return(NULL)
    read1()%>%
      filter(timepoint==unique(input$man))
    })
    
# Plot the interactive manhattan plots
  
    output$mymanhattan<-renderPlotly({
    if (is.null(read1()))
      return(NULL)
    if (is.null(input$man))
      return()
      
# please see the details on Manhattanly package for this code http://sahirbhatnagar.com/manhattanly/
    
    manhattanly(data1(), chr="chrom", snp="marker", bp="pos", p="P", col=c("#D2691E","#800080","#6495ED","#9ACD32"), 
                point_size=7,showlegend = FALSE,xlab = "Chromosome", ylab = "-log10(p)",
                suggestiveline = input$logpvalue, suggestiveline_color = "blue", 
                suggestiveline_width = 2, genomewideline =FALSE, title = "")
    
  })
    
# Display the SNPs or marker with highest significant -logpValue
  
    output$sumOutput <- renderUI({
      if (is.null(read1()))
        return(NULL)
      checkboxInput("sum", "Display in Table Significant SNPs", FALSE)
    })
    
# Make data reactive
    data5<-reactive({
      if (is.null(read1()))
        return(NULL)
      read1()%>%
        filter(timepoint==unique(input$man))
    })
    output$summary <- renderPrint({
      data6<-arrange(data5(), P)
      data6[1:input$p,]
    })
#================================COMBINED/GRID MANHATTAN PLOTS==========================================#
# read the file if uploaded otherwise return null
    
    read2 <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    data2 <- read.csv(inFile2$datapath, 
                      header=input$header,
                      na.strings = input$na.strings,
                      sep=input$sep)
    return(data2)
    
#timepoint as factor
    
    data2$timepoint<-as.factor(data2$timepoint)
    })
    
# Most of this code is adapted from https://www.r-graph-gallery.com/wp-content/uploads/2018/02/Manhattan_plot_in_R.html
# Here we modify the code as per requriement.
# First loop is created for each time point in the file to run the code
    data2<-reactive ({
    days<- as.factor(unique(read2()$timepoint)) # treat timepoint as factor
    for(i in 1:length(days)){
    don<- read2()%>% 
    group_by(chrom) %>% 
    summarise(chr_len=max(pos))%>% 
        
 # Calculate cumulative position of each chromosome
      
        mutate(tot=cumsum(chr_len)-chr_len) %>%
        select(-chr_len)%>%
        
# Add this info to the initial dataset
      
        left_join(read2(), ., by=c("chrom"="chrom"))%>%
        
 # Add a cumulative position of each marker or SNP

        arrange(chrom, pos) %>%
        mutate( BPcum=pos+tot)
    }
    # create text to be displayed in interactive visualization
    
    don$text <- paste("marker: ", don$marker, "\nChromosome: ", don$chrom, sep="") 
    
    return (don)
    #data2()$chrom<-as.factor(data2()$chrom) 
    })
# Create the x axis
    
   axisdf<-reactive ({
    data2()%>% group_by(chrom) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
    
  })
# create the combined plot using facet_wrap in ggplot
    
   isolate({
    output$mygrid<-renderPlot({
      if (is.null(read2())) {
        return()
      }
      ggplot(data2(), aes(x=BPcum, y=-log10(P))) +
        facet_wrap(~timepoint, ncol=input$ncol)+theme_bw()+
        
# add horizontal threshold line
        
        geom_hline(yintercept =input$pvalue, color = "blue", size =1,show.legend = TRUE,linetype = "dashed")+
        
# Show all points and color
        
        geom_point( aes(color=as.factor(chrom)), alpha=1, size=1.5) +
        scale_color_manual(values = rep(c("#D2691E","#800080","#6495ED","#9ACD32"), 22 ))+
        
 # custom X axis and Y axis and legend:
        
        scale_x_continuous(label = (axisdf()$chrom), breaks= (axisdf()$center)) +
        scale_y_continuous(expand = c(0, 0) )+
        theme(axis.text.x = element_text(colour = 'black', face="bold", size = 7, vjust=0.5)) +
        theme(axis.text.y = element_text(colour = 'black', face="bold", size = 7)) +
        theme(axis.title.x = element_text(colour = 'black', face="bold", size = 12, vjust=-0.25)) +
        theme(axis.title.y = element_text(colour = 'black', face="bold", size = 12, angle=90,
                                          vjust=1.5))+xlab("Chromosome") +ylab("-log10(P)") +
        theme(strip.text.x = element_text(size = 8, face="bold", colour = "black"))+
        
        theme(strip.background = element_rect(fill = "white", color = "black", size=1))+
        theme(legend.position="none")
      
      })
      })
   
   #==============================Compare Significant Markers across timepoints========================================#
   
   # read the file if uploaded otherwise return null
   output$signiOutput <- renderUI({
    numericInput("pval", "Select Top Markers Based on p-value:", min = 0.000001, max =0.001, step = 0.01, value =0.000001)
     
   })
   
# filter the data based on the P -value significance
  data33<-reactive ({
    if (is.null(read2())) {
      return(NULL)
    }
# input$pval adds flexibility to chose differenr set of p values user is interested
    filter(data2(),P<input$pval)
     
     
  }) 
   
# create the combined plot
   output$mysig<-renderPlotly({
     if (is.null(read2()))
       return(NULL)
     if (is.null(input$pval))
       return()
     ggplotly( 
       ggplot(data33(), aes(x=BPcum, y=-log10(P), text=text))+theme_bw()+
        geom_point( aes(color=factor(timepoint), shape=factor(timepoint)), alpha=1, size=2.5)+
         scale_shape_identity()+
        #scale_shape_manual(values=rep(c(16,17,18,19,7,8,9,10,11,12,13,14,15,22, 23, 24)))+
         scale_x_continuous(label = (axisdf()$chrom), breaks= (axisdf()$center))+
         scale_y_continuous(expand = c(0, 0) )+
         
#Add highlighted points

         theme(axis.text.x = element_text(colour = 'black', face="bold", size = 7, vjust=0.5)) +
         theme(axis.text.y = element_text(colour = 'black', face="bold", size = 7)) +
         theme(axis.title.x = element_text(colour = 'black', face="bold", size = 12, vjust=-0.25)) +
         theme(axis.title.y = element_text(colour = 'black', face="bold", size = 12, angle=90,
                                        vjust=1.5))+xlab("Chromosome") +ylab("-log10(P)")+
         theme(legend.title = element_text(colour="darkred", size=14, face="bold"),
             legend.text = element_text(colour="grey0", size=12, face="bold"))+
              theme(legend.position="none")
)

       
    #) %>%
      # layout(
        # legend = list(
           #orientation = "h", x = 0.2, y=1.1, text='New Legend Title', showarrow=T
        # )
       #)
     #%>%layout(showlegend = FALSE)
     
  })
   
#==============================PHENOTYPIC DATA VISUALIZATION========================================#

# read the file if uploaded otherwise return null  
    read4 <- reactive({
    inFile4 <- input$file4
    if (is.null(inFile4))
      return(NULL)
    data4 <- read.csv(inFile4$datapath, 
                      header=input$header,
                      na.strings = input$na.strings,
                      sep=input$sep)
    
#timepoint as factor
    
    data4$timepoint<-as.factor(data4$timepoint)
    return(data4)
    })
    
    output$timeOutput <- renderUI({
    selectInput("timepoint", "Choose Time Point or Phenotypes", unique(read4()$timepoint), selected = NULL)
  })
# Make data reactive
    
    data4<-reactive({
    if (is.null(read4())) {
      return()
    }
    read4()%>%
    filter(timepoint==unique(input$timepoint))
  })
  
# Plot histogram
    
  output$plot <- renderPlotly({
    if (is.null(read4())) {
      return()
    }
# Plot the required graphs
    
    if (input$plot.type == "histogram") {
      ggplot(data4(), aes(Value)) +
        geom_histogram(color="darkblue", fill="lightblue")+
        geom_vline(aes(xintercept=mean(Value)),
                   color="darkred", linetype="dashed", size=1)+
        labs(title="",x="Value", y = "Count")+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=14, face="bold", hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        ggtitle("Histogram")
      
    }
    
# Density Plot
    else if (input$plot.type == "density") {
      ggplot(data4(), aes(Value)) +
        geom_density(alpha = 0.1,fill="darkblue", color="red" )+
        geom_vline(aes(xintercept=mean(Value)),
                   color="black", linetype="dashed", size=1)+
        #geom_density(position = "stack")+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=14, face="bold", hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        theme(legend.position="none")+
        ggtitle("Density Plot")
    }
    
# Density Combined
    else if (input$plot.type == "densityall") {
      ggplot(read4(), aes(Value, fill = timepoint, colour = timepoint)) +
        geom_density(alpha = 0.1)+
        #geom_density(position = "stack")+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=14, face="bold",hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        theme(legend.position="none")+
        ggtitle("Density Plot Across All the Timepoints")
      
        }
    
# Box Plot
    
    else if (input$plot.type == "boxplot") {
      ggplot(read4(), aes(x=timepoint, y=Value)) +
        geom_boxplot(aes(fill=timepoint))+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=14, face="bold",hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        theme(legend.position="none")+
        aes(x = fct_inorder(timepoint))+
        labs(title="",x="Time Point", y = "Trait Value")+
        ggtitle("Data Trend Along the Timepoints")
      
    }
  })
  
}
#run the app
    
  shinyApp(ui = ui, server = server)