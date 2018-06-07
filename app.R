## ==================================================================================== ##
# ShinyAIM for visualization of interactive Manhatten plots of longitudnal GWAS data.

#Copyright (c) 2000-2006, The Perl Foundation. 
#This program is free software: you can redistribute it and/or modify
# it under the terms of the Artistic License 2.0.
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

  rm(list=ls())
# Load required packages
  
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(plotly)
  library(manhattanly)
  library(forcats)

# Define UI for the application, for more information on it please see https://shiny.rstudio.com/gallery/
  
      ui<- fluidPage(
  
# Create the Application Title uing headerPanel
  
      headerPanel(h1("ShinyAIM: Shiny Application for Interactive Manhatten Plots", style = "font-family: 'Trattatello', fantasy; font-weight: 500; line-height: 1.1; color: #D2691E;", align = "center")),

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
    
# Creat tabpanel Interactive Manhatten plots

    tabPanel(
      h4("Interactive Manhatten Plots", style = "color: #800080;"),
      
# Within this tabpanel sidebar layout and  sidebar panel is framed
      
      sidebarLayout(
      sidebarPanel(width = 3,
                   
# Data upload button is created
      fileInput('file1', 'Upload data file for Interactive Manhatten Plot:',
                               accept=c('text/csv','text/comma-separated-values,text/plain')),

# Check wheather file has header or not

      checkboxInput('header', 'Data file has variable names as column headers.', TRUE),

#Data file seperator               
      radioButtons('sep', 'Data File separator value:',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t')
                     ),

# uioutput creates the button where user can control the input of file

      uiOutput("manOutput"),

# Sliderinput button to allow users to choose significance level
      sliderInput("logpvalue", "Choose -log pValue:",
                                 min = -log10(0.01), max = -log10(0.00000001),
                                 value = -log10(0.00001), step=0.5)
                     
        ),

# main panel reserves space for the plot 

      mainPanel(align="center",
                  plotlyOutput("mymanhatten")
        )
      )),


 #============================MANHATTEN GRID PLOTS=================================================#
    
      tabPanel(
      h4("Manhatten Grid Plots", style = "color: #800080;"),
      sidebarLayout(

 # Data upload button is created
        
      sidebarPanel(width = 3,
                     fileInput('file2', 'Upload data file for Combined Manhatten Plot:',
                               accept=c('text/csv','text/comma-separated-values,text/plain')),
                   
# Check wheather file has header or not

      checkboxInput('header', 'Data file has variable names as column headers.', TRUE),
                     radioButtons('sep', 'Data File separator value:',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t')
                     ),

# uioutput creates the button where user can control the input of file

      uiOutput("gridOutput"),

# Sliderinput button to allow users to choose significance level

      sliderInput("pvalue", "Choose -log pValue:",
                                 min = -log10(0.001), max = -log10(0.00000001),
                                 value = -log10(0.00001), step=0.5),

# Sliderinput button to allow users to choose the number of columns in grid plot

     sliderInput("ncol", "Number of columns for grid plot:",
                                 min = 2, max =10,
                                 value =4, step=1)
        ),

# main panel reserves a for the plot

    mainPanel(align="center",
                  tags$h2("", align = "center"),
                  plotOutput("mygrid", height=700))
      )),

    
#============================PHENOTYPIC DATA VISUALIZATION========================================#
    
    tabPanel(
    h4("Phenotypic Data visulaization", style = "color: #800080;"),
    sidebarLayout(
        
    sidebarPanel(width = 3,
                     fileInput('file3', 'Upload data file:',
                               accept=c('text/csv','text/comma-separated-values,text/plain')),
                     
    checkboxInput('header', 'Data file has variable names as column headers.', TRUE),
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

#==============================INTERACTIVE MANHATTEN PLOTS========================================#
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
    selectInput("man", "Choose Time Point", unique(read1()$timepoint), selected = "")
    })
    
# Make the data reactive and filter it based on timepoints in the uploaded file
    
    data1<-reactive({
    if (is.null(read1()))
      return(NULL)
    read1()%>%
      filter(timepoint==unique(input$man))
    })
    
# Plot the interactive manhatten plots
  
    output$mymanhatten<-renderPlotly({
    if (is.null(read1()))
      return(NULL)
    if (is.null(input$man))
      return()
# please see the details on Manhattanly package for this code http://sahirbhatnagar.com/manhattanly/
    
    manhattanly(data1(), chr="chrom", snp="marker", bp="pos", p="P", col=c("#D2691E","#800080","#6495ED","#9ACD32"), 
                point_size=7,showlegend = FALSE,xlab = "Chromosome", ylab = "-log10(p)",
                suggestiveline = input$logpvalue, suggestiveline_color = "blue", 
                suggestiveline_width = 2, genomewideline =FALSE, title = "Interacative Manhatten Plot")
    
  })
    
#================================COMBINED/GRID MANHATTEN PLOTS==========================================#
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
# Here loop is created for each time point in the file to run the code
    data2<-reactive ({
    days<- as.factor(unique(read2()$timepoint))
    for(i in 1:length(days)){
      #day1 <- data[which(data$timepoint==days[i]),]
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
    return (don)
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
   
   
#==============================PHENOTYPIC DATA VISUALIZATION========================================#

   # read the file if uploaded otherwise return null  
    read3 <- reactive({
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    data3 <- read.csv(inFile3$datapath, 
                      header=input$header,
                      na.strings = input$na.strings,
                      sep=input$sep)
    
#timepoint as factor
    
    data3$timepoint<-as.factor(data3$timepoint)
    return(data3)
    })
    
    output$timeOutput <- renderUI({
    selectInput("timepoint", "Choose Time Point", unique(read3()$timepoint), selected = NULL)
  })
# Make data reactive
    
    data3<-reactive({
    if (is.null(read3())) {
      return()
    }
    read3()%>%
    filter(timepoint==unique(input$timepoint))
  })
  
# Plot histogram
    
  output$plot <- renderPlotly({
    if (is.null(read3())) {
      return()
    }
# Plot the required graphs
    
    if (input$plot.type == "histogram") {
      ggplot(data3(), aes(Value)) +
        geom_histogram(color="darkblue", fill="lightblue")+
        geom_vline(aes(xintercept=mean(Value)),
                   color="darkred", linetype="dashed", size=1)+
        labs(title="",x="Time Point", y = "Count")+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=10, hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))
      
    }
    
# Density Plot
    else if (input$plot.type == "density") {
      ggplot(data3(), aes(Value)) +
        geom_density(alpha = 0.1,fill="darkblue", color="red" )+
        geom_vline(aes(xintercept=mean(Value)),
                   color="black", linetype="dashed", size=1)+
        #geom_density(position = "stack")+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=10, hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        theme(legend.position="none")
    }
    
# Density Combined
    else if (input$plot.type == "densityall") {
      ggplot(read3(), aes(Value, fill = timepoint, colour = timepoint)) +
        geom_density(alpha = 0.1)+
        #geom_density(position = "stack")+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=10, hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        theme(legend.position="none")
      
        }
    
# Box Plot
    
    else if (input$plot.type == "boxplot") {
      ggplot(read3(), aes(x=timepoint, y=Value)) +
        geom_boxplot(aes(fill=timepoint))+
        theme_classic()+
        theme (plot.title = element_text(color="black", size=10, hjust=0),
               axis.title.x = element_text(color="black", size=10, face="bold"),
               axis.title.y = element_text(color="black", size=10, face="bold")) +
        theme(axis.text = element_text(colour = "black"))+
        theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
        theme(legend.position="none")+
        aes(x = fct_inorder(timepoint))+
        labs(title="",x="Time Point", y = "Trait Value")
      
    }
  })
  
}
#run the app
    
  shinyApp(ui = ui, server = server)