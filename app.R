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
      
# Load the required packages if not installed
    packages = c("shiny", "ggplot2", "dplyr", "grid", "plotly", "manhattanly", "forcats",
                 "DT", "shinycssloaders", "bslib", "colourpicker", "shinyWidgets", 
                 "ggrepel", "scales", "viridis")
    package.check <- lapply(packages, FUN = function(x) {
     if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
   })
  # Load the required packages   
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(plotly)
  library(manhattanly)
  library(forcats)
  library(DT)
  library(shinycssloaders)
  library(bslib)
  library(colourpicker)
  library(shinyWidgets)
  library(ggrepel)
  library(scales)
  library(viridis)
  
# Helper functions for statistical calculations
calculate_lambda <- function(pvalues) {
  # Calculate genomic inflation factor (lambda)
  chisq <- qchisq(1 - pvalues, 1)
  lambda <- median(chisq) / qchisq(0.5, 1)
  return(lambda)
}

calculate_bonferroni <- function(alpha, n_tests) {
  return(alpha / n_tests)
}

calculate_fdr <- function(pvalues, alpha = 0.05) {
  # Benjamini-Hochberg FDR
  n <- length(pvalues)
  sorted_p <- sort(pvalues)
  ranks <- 1:n
  threshold_idx <- max(which(sorted_p <= (ranks / n) * alpha), na.rm = TRUE)
  if (length(threshold_idx) == 0 || threshold_idx == -Inf) {
    return(0)
  }
  return(sorted_p[threshold_idx])
}

# Define UI theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#800080",
  base_font = font_google("Open Sans")
)

# Define UI for the application, for more information on it please see https://shiny.rstudio.com/gallery/
  
      ui<- fluidPage(
        theme = app_theme,

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
      # Creat button to downlaod sample files
      #downloadButton("downloadData", label = "Download sample files"),
# Within this tabpanel sidebar layout and  sidebar panel is framed
      
      sidebarLayout(
      sidebarPanel(width = 3,
  
# Creat button to downlaod sample files
                   downloadButton("downloadData", label = "Download Sample File"),
                    hr(),

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
      
      # Color scheme selector
      selectInput("color_scheme", "Color Scheme:",
                  choices = c("Default" = "default",
                             "Colorblind-Friendly" = "colorblind",
                             "Grayscale" = "grayscale",
                             "Custom" = "custom")),
      
      conditionalPanel(
        condition = "input.color_scheme == 'custom'",
        colourpicker::colourInput("custom_color1", "Color 1:", "#D2691E"),
        colourpicker::colourInput("custom_color2", "Color 2:", "#800080"),
        colourpicker::colourInput("custom_color3", "Color 3:", "#6495ED"),
        colourpicker::colourInput("custom_color4", "Color 4:", "#9ACD32")
      ),
      
      # Multiple testing correction
      selectInput("correction_method", "Multiple Testing Correction:",
                  choices = c("None" = "none",
                             "Bonferroni" = "bonferroni",
                             "FDR (Benjamini-Hochberg)" = "fdr")),
# Sliderinput button to allow users to display top significant SNPs

conditionalPanel(
# Displays the SNPs with highest -logpValue
  condition = "input.sum",
      sliderInput("p", "How many Significant SNPs to be Displayed in Table:",
                                 min = 1, max =80,
                                  value = 2, step=1))

                                   ),

# main panel reserves space for the plot 

      mainPanel(
        h4("Interactive Manhattan Plot", align = "center"),
        # Summary statistics box
        fluidRow(
          column(3, uiOutput("n_markers_box")),
          column(3, uiOutput("n_significant_box")),
          column(3, uiOutput("lambda_box")),
          column(3, uiOutput("threshold_box"))
        ),
        br(),
        # Export buttons
        fluidRow(
          column(12, 
                 downloadButton("download_manhattan_png", "PNG", class = "btn-sm"),
                 downloadButton("download_manhattan_pdf", "PDF", class = "btn-sm"),
                 downloadButton("download_manhattan_svg", "SVG", class = "btn-sm"),
                 style = "text-align: right;"
          )
        ),
        br(),
        withSpinner(plotlyOutput("mymanhattan"), type = 6, color = "#800080"),
        br(),
        hr(),
        conditionalPanel(
            
# Displays the SNPs in table arranged with highest -logpValue 
            
            condition = "input.sum", 
            tags$h4("Markers Arranged in Significance Order", align = "center"),
            DT::dataTableOutput("summary"))
      )
          
      )),

# Add new QQ Plot tab
    tabPanel(
      h4("QQ Plot", style = "color: #800080;"),
      sidebarLayout(
        sidebarPanel(width = 3,
          tags$p("QQ plots assess whether observed p-values follow the expected distribution under the null hypothesis."),
          hr(),
          uiOutput("qq_timepoint_selector"),
          hr(),
          tags$h5("Genomic Inflation Factor (λ):"),
          verbatimTextOutput("lambda_value"),
          hr(),
          # Export buttons for QQ plot
          downloadButton("download_qq_png", "Export PNG", class = "btn-sm"),
          downloadButton("download_qq_pdf", "Export PDF", class = "btn-sm")
        ),
        mainPanel(
          h4("Quantile-Quantile Plot", align = "center"),
          withSpinner(plotlyOutput("qqplot", height = "600px"), type = 6, color = "#800080"),
          br(),
          tags$div(
            tags$p(tags$strong("Interpretation:"), 
                   "Points falling along the diagonal line indicate expected p-values. 
                   Deviation suggests potential population stratification or true associations."),
            style = "font-size: 14px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;"
          )
        )
      )
    ),


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
                                 value =4, step=1),
     
     # Data preprocessing options
     hr(),
     tags$h5("Data Preprocessing:"),
     numericInput("maf_threshold", "MAF Threshold (if available):", 
                  value = 0, min = 0, max = 0.5, step = 0.01),
     numericInput("missing_threshold", "Missing Rate Threshold:", 
                  value = 1, min = 0, max = 1, step = 0.05),
     
     # Export button
     downloadButton("download_grid_png", "Export Grid Plot", class = "btn-sm")
        ),


# main panel reserves a for the plot

    mainPanel(align="center",
                  tags$h4("Manhattan Grid Plot", align = "center"),
                  withSpinner(plotOutput("mygrid", height=800), type = 6, color = "#800080"))
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

#============================DATA MANAGEMENT TAB========================================#
    
    tabPanel(
      h4("Data Management", style = "color: #800080;"),
      sidebarLayout(
        sidebarPanel(width = 3,
          tags$h4("Session Management"),
          hr(),
          tags$p("Save your current analysis state to reload later."),
          textInput("session_name", "Session Name:", value = "my_analysis"),
          downloadButton("save_session", "Save Session (.RDS)", class = "btn-primary"),
          hr(),
          fileInput("load_session", "Load Saved Session:",
                   accept = c(".rds", ".RDS")),
          hr(),
          tags$h4("Multiple File Comparison"),
          fileInput("compare_file1", "Upload File 1 (GWAS):",
                   accept = c('text/csv','text/comma-separated-values,text/plain')),
          fileInput("compare_file2", "Upload File 2 (GWAS):",
                   accept = c('text/csv','text/comma-separated-values,text/plain')),
          checkboxInput("show_comparison", "Show Comparison Plot", FALSE)
        ),
        mainPanel(
          h4("Session Information", align = "center"),
          conditionalPanel(
            condition = "!input.show_comparison",
            verbatimTextOutput("session_info")
          ),
          conditionalPanel(
            condition = "input.show_comparison",
            tags$h4("Side-by-Side Comparison", align = "center"),
            fluidRow(
              column(6, 
                     tags$h5("File 1", align = "center"),
                     withSpinner(plotOutput("compare_plot1", height = "400px"), type = 6)),
              column(6, 
                     tags$h5("File 2", align = "center"),
                     withSpinner(plotOutput("compare_plot2", height = "400px"), type = 6))
            )
          )
        )
      )
    ),

#============================PHENOTYPIC DATA VISUALIZATION========================================#
    
    tabPanel(
    h4("Phenotypic Data Visualization", style = "color: #800080;"),
    sidebarLayout(
        
    sidebarPanel(width = 3,
                 downloadButton("downloadData1", label = "Download Sample File"),
                 hr(),
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
                  withSpinner(plotlyOutput("plot"), type = 6, color = "#800080")))
      
      
    )        
  )
)
  

#================================serever part=================================     
# Choose the size of shiny app
      
    options(shiny.maxRequestSize = 100*1024^2)

# Define the Server part for application

    server <- function(input, output) {

#==============================INTERACTIVE MANHATTAN PLOTS========================================#

# Download data button
      output$downloadData <- downloadHandler(
        filename <- function() {
          paste("Sample_file_Manhattan", ".csv", sep = "")
        },
        
        content <- function(file) {
          file.copy("Samplefiles/samplefile1_manhattan.csv", file)
        },
        contentType = "application/csv")
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
    
    # Summary statistics
    output$n_markers_box <- renderUI({
      if (is.null(data1())) return(NULL)
      tags$div(
        style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; text-align: center;",
        tags$h5("Total Markers", style = "margin: 5px; color: #1976d2;"),
        tags$h3(format(nrow(data1()), big.mark = ","), style = "margin: 5px;")
      )
    })
    
    output$n_significant_box <- renderUI({
      if (is.null(data1())) return(NULL)
      threshold <- 10^(-input$logpvalue)
      n_sig <- sum(data1()$P < threshold, na.rm = TRUE)
      tags$div(
        style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; text-align: center;",
        tags$h5("Significant Markers", style = "margin: 5px; color: #388e3c;"),
        tags$h3(n_sig, style = "margin: 5px;")
      )
    })
    
    output$lambda_box <- renderUI({
      if (is.null(data1())) return(NULL)
      lambda_val <- calculate_lambda(data1()$P)
      tags$div(
        style = "background-color: #fff3e0; padding: 10px; border-radius: 5px; text-align: center;",
        tags$h5("Lambda (λ)", style = "margin: 5px; color: #f57c00;"),
        tags$h3(sprintf("%.3f", lambda_val), style = "margin: 5px;")
      )
    })
    
    output$threshold_box <- renderUI({
      if (is.null(data1())) return(NULL)
      threshold_val <- 10^(-input$logpvalue)
      
      # Apply correction if selected
      if (input$correction_method == "bonferroni") {
        threshold_val <- calculate_bonferroni(0.05, nrow(data1()))
      } else if (input$correction_method == "fdr") {
        threshold_val <- calculate_fdr(data1()$P, 0.05)
      }
      
      tags$div(
        style = "background-color: #fce4ec; padding: 10px; border-radius: 5px; text-align: center;",
        tags$h5(paste("Threshold", if(input$correction_method != "none") paste0("(", input$correction_method, ")")), 
                style = "margin: 5px; color: #c2185b;"),
        tags$h3(sprintf("%.2e", threshold_val), style = "margin: 5px;")
      )
    })
    
    # Get color palette based on selection
    get_colors <- reactive({
      if (input$color_scheme == "default") {
        return(c("#D2691E","#800080","#6495ED","#9ACD32"))
      } else if (input$color_scheme == "colorblind") {
        return(viridis(4, option = "D"))
      } else if (input$color_scheme == "grayscale") {
        return(c("#000000", "#4d4d4d", "#999999", "#cccccc"))
      } else if (input$color_scheme == "custom") {
        return(c(input$custom_color1, input$custom_color2, 
                input$custom_color3, input$custom_color4))
      }
      return(c("#D2691E","#800080","#6495ED","#9ACD32"))
    })
    
# Plot the interactive manhattan plots
  
    output$mymanhattan<-renderPlotly({
    if (is.null(read1()))
      return(NULL)
    if (is.null(input$man))
      return()
      
# please see the details on Manhattanly package for this code http://sahirbhatnagar.com/manhattanly/
    
    colors <- get_colors()
    manhattanly(data1(), chr="chrom", snp="marker", bp="pos", p="P", col=colors, 
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
    
    output$summary <- DT::renderDataTable({
      if (is.null(data5())) return(NULL)
      data6 <- arrange(data5(), P)
      data6 <- data6[1:min(input$p, nrow(data6)),]
      DT::datatable(data6, 
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    class = 'cell-border stripe')
    })
    
    # Download handlers for Manhattan plot
    output$download_manhattan_png <- downloadHandler(
      filename = function() {
        paste0("manhattan_", input$man, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        if (is.null(data1())) return(NULL)
        png(file, width = 1200, height = 600, res = 150)
        # Create base manhattan plot with ggplot
        p <- ggplot(data1(), aes(x = pos, y = -log10(P))) +
          geom_point(aes(color = as.factor(chrom)), size = 2) +
          scale_color_manual(values = rep(get_colors(), 22)) +
          geom_hline(yintercept = input$logpvalue, color = "blue", linetype = "dashed") +
          theme_bw() +
          labs(x = "Chromosome", y = "-log10(P)", title = paste("Manhattan Plot -", input$man)) +
          theme(legend.position = "none")
        print(p)
        dev.off()
      }
    )
    
    output$download_manhattan_pdf <- downloadHandler(
      filename = function() {
        paste0("manhattan_", input$man, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (is.null(data1())) return(NULL)
        pdf(file, width = 12, height = 6)
        p <- ggplot(data1(), aes(x = pos, y = -log10(P))) +
          geom_point(aes(color = as.factor(chrom)), size = 2) +
          scale_color_manual(values = rep(get_colors(), 22)) +
          geom_hline(yintercept = input$logpvalue, color = "blue", linetype = "dashed") +
          theme_bw() +
          labs(x = "Chromosome", y = "-log10(P)", title = paste("Manhattan Plot -", input$man)) +
          theme(legend.position = "none")
        print(p)
        dev.off()
      }
    )
    
    output$download_manhattan_svg <- downloadHandler(
      filename = function() {
        paste0("manhattan_", input$man, "_", Sys.Date(), ".svg")
      },
      content = function(file) {
        if (is.null(data1())) return(NULL)
        svg(file, width = 12, height = 6)
        p <- ggplot(data1(), aes(x = pos, y = -log10(P))) +
          geom_point(aes(color = as.factor(chrom)), size = 2) +
          scale_color_manual(values = rep(get_colors(), 22)) +
          geom_hline(yintercept = input$logpvalue, color = "blue", linetype = "dashed") +
          theme_bw() +
          labs(x = "Chromosome", y = "-log10(P)", title = paste("Manhattan Plot -", input$man)) +
          theme(legend.position = "none")
        print(p)
        dev.off()
      }
    )
    
#==============================QQ PLOT========================================#
    # Timepoint selector for QQ plot
    output$qq_timepoint_selector <- renderUI({
      if (is.null(read1())) return(NULL)
      selectInput("qq_timepoint", "Choose Time Point or Phenotypes:", 
                  unique(read1()$timepoint), selected = unique(read1()$timepoint)[1])
    })
    
    # QQ plot data
    qq_data <- reactive({
      if (is.null(read1()) || is.null(input$qq_timepoint)) return(NULL)
      read1() %>%
        filter(timepoint == input$qq_timepoint)
    })
    
    # Lambda value display
    output$lambda_value <- renderPrint({
      if (is.null(qq_data())) return(NULL)
      lambda_val <- calculate_lambda(qq_data()$P)
      cat(sprintf("λ = %.4f\n\n", lambda_val))
      if (lambda_val > 1.1) {
        cat("Note: λ > 1.1 suggests potential population stratification or inflation.")
      } else if (lambda_val < 0.9) {
        cat("Note: λ < 0.9 suggests potential deflation or over-correction.")
      } else {
        cat("Note: λ ≈ 1 indicates well-controlled test statistics.")
      }
    })
    
    # QQ plot
    output$qqplot <- renderPlotly({
      if (is.null(qq_data())) return(NULL)
      
      # Prepare QQ plot data
      observed <- sort(-log10(qq_data()$P))
      expected <- -log10(ppoints(length(observed)))
      qq_df <- data.frame(expected = expected, observed = observed)
      
      # Create plot
      p <- ggplot(qq_df, aes(x = expected, y = observed)) +
        geom_point(alpha = 0.5, size = 2, color = "#800080") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        labs(x = "Expected -log10(p)", 
             y = "Observed -log10(p)",
             title = paste("QQ Plot -", input$qq_timepoint)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10))
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # QQ plot downloads
    output$download_qq_png <- downloadHandler(
      filename = function() {
        paste0("qqplot_", input$qq_timepoint, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        if (is.null(qq_data())) return(NULL)
        observed <- sort(-log10(qq_data()$P))
        expected <- -log10(ppoints(length(observed)))
        qq_df <- data.frame(expected = expected, observed = observed)
        
        png(file, width = 800, height = 800, res = 150)
        p <- ggplot(qq_df, aes(x = expected, y = observed)) +
          geom_point(alpha = 0.5, size = 2, color = "#800080") +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
          labs(x = "Expected -log10(p)", y = "Observed -log10(p)",
               title = paste("QQ Plot -", input$qq_timepoint)) +
          theme_bw()
        print(p)
        dev.off()
      }
    )
    
    output$download_qq_pdf <- downloadHandler(
      filename = function() {
        paste0("qqplot_", input$qq_timepoint, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (is.null(qq_data())) return(NULL)
        observed <- sort(-log10(qq_data()$P))
        expected <- -log10(ppoints(length(observed)))
        qq_df <- data.frame(expected = expected, observed = observed)
        
        pdf(file, width = 8, height = 8)
        p <- ggplot(qq_df, aes(x = expected, y = observed)) +
          geom_point(alpha = 0.5, size = 2, color = "#800080") +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
          labs(x = "Expected -log10(p)", y = "Observed -log10(p)",
               title = paste("QQ Plot -", input$qq_timepoint)) +
          theme_bw()
        print(p)
        dev.off()
      }
    )
    
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
   
   # Download handler for grid plot
   output$download_grid_png <- downloadHandler(
     filename = function() {
       paste0("manhattan_grid_", Sys.Date(), ".png")
     },
     content = function(file) {
       if (is.null(read2())) return(NULL)
       png(file, width = 1600, height = 1200, res = 150)
       p <- ggplot(data2(), aes(x=BPcum, y=-log10(P))) +
         facet_wrap(~timepoint, ncol=input$ncol)+theme_bw()+
         geom_hline(yintercept =input$pvalue, color = "blue", size =1, linetype = "dashed")+
         geom_point( aes(color=as.factor(chrom)), alpha=1, size=1.5) +
         scale_color_manual(values = rep(c("#D2691E","#800080","#6495ED","#9ACD32"), 22 ))+
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
       print(p)
       dev.off()
     }
   )
   
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
   # Download data button
   output$downloadData1 <- downloadHandler(
     filename <- function() {
       paste("Sample_file_Phenotypic", ".csv", sep = "")
     },
     
     content <- function(file) {
       file.copy("Samplefiles/samplefile2_phenotypic.csv", file)
     },
     contentType = "application/csv") 
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
  
#==============================DATA MANAGEMENT========================================#
  
  # Session save handler
  output$save_session <- downloadHandler(
    filename = function() {
      paste0(input$session_name, "_", Sys.Date(), ".rds")
    },
    content = function(file) {
      session_data <- list(
        file1_data = read1(),
        file2_data = read2(),
        file4_data = read4(),
        input_values = list(
          man = input$man,
          logpvalue = input$logpvalue,
          color_scheme = input$color_scheme,
          correction_method = input$correction_method,
          pvalue = input$pvalue,
          ncol = input$ncol
        )
      )
      saveRDS(session_data, file)
    }
  )
  
  # Session info display
  output$session_info <- renderPrint({
    cat("Session Information\n")
    cat("===================\n\n")
    
    if (!is.null(read1())) {
      cat("Interactive Manhattan Data:\n")
      cat("  - Loaded: Yes\n")
      cat("  - Markers:", nrow(read1()), "\n")
      cat("  - Timepoints:", length(unique(read1()$timepoint)), "\n\n")
    } else {
      cat("Interactive Manhattan Data: Not loaded\n\n")
    }
    
    if (!is.null(read2())) {
      cat("Grid Manhattan Data:\n")
      cat("  - Loaded: Yes\n")
      cat("  - Markers:", nrow(read2()), "\n")
      cat("  - Timepoints:", length(unique(read2()$timepoint)), "\n\n")
    } else {
      cat("Grid Manhattan Data: Not loaded\n\n")
    }
    
    if (!is.null(read4())) {
      cat("Phenotypic Data:\n")
      cat("  - Loaded: Yes\n")
      cat("  - Samples:", nrow(read4()), "\n")
      cat("  - Timepoints:", length(unique(read4()$timepoint)), "\n\n")
    } else {
      cat("Phenotypic Data: Not loaded\n\n")
    }
    
    cat("R Session Info:\n")
    cat("  - R Version:", R.version.string, "\n")
    cat("  - Platform:", R.version$platform, "\n")
  })
  
  # Comparison file readers
  compare_data1 <- reactive({
    inFile <- input$compare_file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    return(data)
  })
  
  compare_data2 <- reactive({
    inFile <- input$compare_file2
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    return(data)
  })
  
  # Comparison plots
  output$compare_plot1 <- renderPlot({
    if (is.null(compare_data1()) || !input$show_comparison) return(NULL)
    
    data <- compare_data1()
    if ("timepoint" %in% names(data)) {
      # Use first timepoint for comparison
      data <- data %>% filter(timepoint == unique(timepoint)[1])
    }
    
    ggplot(data, aes(x = pos, y = -log10(P))) +
      geom_point(aes(color = as.factor(chrom)), size = 1.5, alpha = 0.7) +
      scale_color_manual(values = rep(c("#D2691E","#800080","#6495ED","#9ACD32"), 22)) +
      theme_bw() +
      labs(x = "Position", y = "-log10(P)", title = "File 1") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$compare_plot2 <- renderPlot({
    if (is.null(compare_data2()) || !input$show_comparison) return(NULL)
    
    data <- compare_data2()
    if ("timepoint" %in% names(data)) {
      data <- data %>% filter(timepoint == unique(timepoint)[1])
    }
    
    ggplot(data, aes(x = pos, y = -log10(P))) +
      geom_point(aes(color = as.factor(chrom)), size = 1.5, alpha = 0.7) +
      scale_color_manual(values = rep(c("#D2691E","#800080","#6495ED","#9ACD32"), 22)) +
      theme_bw() +
      labs(x = "Position", y = "-log10(P)", title = "File 2") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
}
#run the app
    
  shinyApp(ui = ui, server = server)