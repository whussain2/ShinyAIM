# Choose the size of shiny app

options(shiny.maxRequestSize = 100*1024^2)

# Define the Server part for application

server <- shinyServer(function(input, output) {
  
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
  
})