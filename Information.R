column(8,
       tags$body(
         h2(strong('General information')),
         #br(),
         tags$div(
           tags$p('ShinyAIM allows users to dynamically view and interpret longitudinal GWAS results.
                  '), 
           style = "font-size: 18px;"),
         
         
        h3(strong('Features of the Application')),
         
        tags$div(
        img(src = 'image.png', align = "center", width = "100%", height = "100%")
        ), 
        br(),
        
      h3(strong('Data Visualization in interactive way')),
       #br(),
       tags$div(
         tags$div(tags$ul(
           tags$li("Interactive Manhattan Plots."),
           tags$li("Manhattan Plots Combined Across All Time Points."),
           tags$li("Phenotypic Data visualization: Interactive Histograms, Density plots, and Box plots,")),
           style = "font-size: 18px")
         ),
     
      
      h3(strong('Guidelines')),
      tags$div(
        
        tags$ol(
          tags$li("The application has three tabs including (a) Manhattan plots, (b) Combined Manhattan Plots, and (3) Phenotypic Data Visualization."), 
          tags$li("The application accepts CSV file format. Users can see the sample files available in GitHub folder. Also, the snapshot of file format for Manhatten plot, Combined Manhatten Plot and Phenotypic Data
                  Visulaization is shown below:."), 
          img(src = 'dataformat.png', align = "center", width = "100%", height = "100%"),
          tags$ul(
            tags$li("Files must have header rows"),
            tags$li("Column names of the files should match with sample file names, order of the columns is not important."),
            tags$li("Day represents timepoint and can be timepoint or different trait in users file."),
            tags$li("chrom: represents number of chromosomes."),
            tags$li("pos: represents position of markers."),
            tags$li("P: represents p value for markers."),
            tags$li("NSFT_ID: is id for genotypes and can be anything in users file."),
            tags$li("Value: represents the phenotypic data for each time point.")
          ),
          tags$li("For each tab separate CSV files needs to be uploaded, and users can use sample files from the github directory."),
          tags$li("For Manhattan plot tab, once the CSV file in right format is uploaded, users need to choose the separator value in the file. Once users choose the separator value, the time points will be automatically updated in Choose Time point box.  Users can select any time point and Manhattan plot will be generated, and users can interact with it to get more information. Users can choose the significant threshold level by moving slider input bar."),
          tags$li("For phenotypic data visualization, once the right CVS file is uploaded and file separator value chosen, the time point will be automatically updated. The users can select the time point, and choose the time point to see the histogram and density plots. Users can also select box plot type to see the trend and variation across all the time points. All the plot types are interactive, and users can interact to get more information   ."),
          tags$li("For combined Manhattan plot, once CSV file is uploaded and separator value choose, the combined Manhattan plot will be automatically generated. Users can choose the significant threshold level and also number of columns they want in the combined/grid plot. Note: CSV file for Manhattan plot and combined Manhattan plot is same, however, they need to be uploaded separately.")
          #tags$li("Please select colors of your conditions. They will be used for plotting."),
          #tags$li("Enjoy, and give us feedback.")
          
        ), style = "font-size: 16px; line-height: 1.7;"),
      
      
      h2(strong('Source Code and sample files')),
      
      tags$div(
        tags$p('Source code and sample files are available through GitHub at:.'),
        style = "font-size: 18px;"),
      tags$br(),
      tags$div(
        tags$p(strong('How to cite ShinyAIM:'), "Waseem Hussain, Malachy Campbell, Harkamal Walia, and Gota Morota, University of Nebraska Lincoln (manuscript under preperation)"), 
        style = "font-size: 16px"),
      
      h3(strong('Contact Information and Help:')),
      tags$div(
        tags$p('Waseem Hussain - waseem.hussain@unl.edu; waseemhussain907@gmail.com, Gota Morota  -morota@unl.edu.'), 
        style = "font-size: 16px"),
      br(),
      tags$div(
        tags$footer('Copyright (C) 2018, code licensed under GPLv3'),
        style = "font-size: 16px")
))




         
       
      
       

       
      