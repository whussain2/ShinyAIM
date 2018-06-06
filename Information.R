column(8,
       tags$body(
         h2(strong('General information')),
         #br(),
         tags$div(
           tags$p('ShinyAIM allows users to dynamically view and interpret longitudinal GWAS results.
                  '), 
           style = "font-size: 18px;"),
         
        h3(strong('Features of the Application: Interactive data visualization')),
         
        tags$div(
        img(src = 'image.png', align = "center", width = "100%", height = "100%")
        ), 
       # br(),
      #h3(strong('Data Visualization in interactive way')),
       #br(),
       tags$div(
         tags$div(tags$ul(
           tags$li("Interactive Manhattan Plots."),
           tags$li("Manhattan Plots Combined Across All Time Points."),
           tags$li("Phenotypic Data visualization: Interactive Histograms, Density plots, and Box plots.")),
           style = "font-size: 18px")
         ),
    
      h3(strong('Guidelines')),
      tags$div(
        
        tags$ol(
          tags$li("The application has three main tabs including (a) Interactiv Manhattan plots, (b) Combined Manhattan Plots, and (3) Phenotypic Data Visualization."), 
          tags$li("The application accepts CSV file format. Users can see the sample files available in GitHub folder named as samplefile1_manhatten (for Manhatten and combined Manhatten plots) and 
          samplefile2_phenotypic (for phenotyic data visualization. Also, the snapshot of file format for Manhatten plots, Combined manhatten plot and Phenotypic data
                  visulaization is shown below:."), 
          img(src = 'dataformat.png', align = "center", width = "100%", height = "100%"),
          tags$ul(
            tags$li("Files must have header rows"),
            tags$li("Column names of the files should match with sample file names, order of the columns is not important."),
            tags$li("timepoint represents the time or day at which data was collected, this column can represent different time points or  and it can be different traits in users file,but make sure column name is as timepoint."),
            tags$li("chrom: represents number of chromosomes."),
            tags$li("pos: represents position of markers or SNPs."),
            tags$li("P: represents p value for markers or SNPs."),
            tags$li("NSFT_ID: is id for genotypes and can be anything in users file."),
            tags$li("Value: represents the phenotypic data for each time point and must be numeric.")
          ),
          tags$li("For each main tab separate CSV files needs to be uploaded, and users can use sample files from the github directory in begiining to get used to application."),
          tags$li("For Manhattan plot tab, once the CSV file in right format is uploaded, users need to choose the separator value in the file. Once users choose the separator value, the time points will be automatically updated in Choose Time point box.  Users can select any time point and Interactive Manhattan plot will be automatically generated in left main panel, and users can interact with it to get more information. Users can choose the significant threshold level by moving slider input bar."),
          tags$li("For phenotypic data visualization, once the right CVS file is uploaded and file separator value chosen, the time point will be automatically updated. The users can select the time point, and choose the time point to see the histogram and density plots. Users can also select box plot type to see the trend and variation across all the time points. All the plot types are interactive, and users can interact to get more information   ."),
          tags$li("For combined Manhattan plot, once CSV file is uploaded and separator value choose, the combined Manhattan plot will be automatically generated in left main panel. Users can choose the significant threshold level and also number of columns in the combined/grid plot. Note: CSV file for Manhattan plot and combined Manhattan plot is same, however, they need to be uploaded separately.")
    
          
        ), style = "font-size: 16px; line-height: 1.7;"),
      
      h2(strong('Source Code and Sample Files')),
      
      tags$div(
        tags$p('Source code and sample files are available through GitHub at:.'),
        tags$a(href="https://github.com/whussain2/ShinyAIM", "ShinyAIM GitHub Repository"),
        style = "font-size: 18px;"),
      tags$br(),
      tags$div(
        tags$p(strong('How to cite ShinyAIM:'), "Waseem Hussain, Malachy Campbell, Harkamal Walia, and Gota Morota, University of Nebraska Lincoln (manuscript under preperation)"), 
        style = "font-size: 18px"),
      
      h3(strong('Contact Information and Help:')),
      tags$div(
        tags$p('Waseem Hussain - waseem.hussain@unl.edu; waseemhussain907@gmail.com, Gota Morota  -morota@unl.edu.'), 
        style = "font-size: 18px"),
      tags$p("Enjoy, and please give us feedback.", style = "font-size: 18px"),
      br(),
      tags$div(
        tags$footer('Copyright (C) 2018, code licensed under Artistic License 2.0'),
        style = "font-size: 16px")
))



         
       
      
       

       
      