column(8,
       tags$body(
         h2(strong('General Information')),
         hr(),
         tags$div(
           tags$p('ShinyAIM allows users to dynamically view and interpret longitudinal GWAS results.
                  '), 
           style = "font-size: 18px;"),
         hr(),
        h3(strong('Features of the Application: Interactive Data Visualization')),
        hr(),
        tags$div(
        img(src = 'image.png', align = "center", width = "100%", height = "100%")
        ),
        hr(),
        #br(),
      #h3(strong('Data Visualization in interactive way')),
       #br(),
       tags$div(
         tags$div(tags$ul(
           tags$li("Interactive Manhattan Plots."),
           tags$li("Manhattan Plots Combined Across All Time Points."),
           tags$li("Compare Significant Markers Across All Time Points."),
           tags$li("Phenotypic Data Visualization: Interactive Histograms, Density Plots, and Box Plots.")),
           style = "font-size: 18px")
         ),
    hr(),
      h3(strong('Guidelines')),
      tags$div(
        
        tags$ol(
          tags$li("The application has four main tab Panels including (a) Interactive Manhattan Plots, (b) Combined Manhattan Plots, (c) Compare Significant Markers, and (d) Phenotypic Data Visualization."), 
          tags$li("The application accepts text/csv,text/comma-separated-values,text/plainfile format. Users can see the sample files available in GitHub folder:"), 
          tags$a(href="https://github.com/whussain2/ShinyAIM/blob/master/Samplefiles/samplefile1_manhatten.csv", "Samplefile1:Manhatten Plots"),
          br(),
          tags$a(href="https://github.com/whussain2/ShinyAIM/blob/master/Samplefiles/samplefile2_phenotypic.csv", "Samplefile2: Phenotypic Data"),
          tags$li("The screenshot of sample files for Interactive Manhatten Plots, Combined Manhatten Plots and Phenotypic Data Visualization  is also shown below:"),
          hr(),
          img(src = 'dataformat.png', align = "center", width = "100%", height = "100%"),
          hr(),
          tags$ul(
            tags$li("Files must have header rows"),
            tags$li("Column names of the files should match with sample file names, order of the columns is not important."),
            tags$li("timepoint: timepoint represents the time or day at which data was collected, this column can represent different time points or phenotypes  and it can be different traits in users file, but make sure column is named as timepoint."),
            tags$li("chrom: represents number of chromosomes."),
            tags$li("pos: represents position of markers or SNPs."),
            tags$li("P: represents p value for markers or SNPs."),
            tags$li("NSFTV_ID: is ID for genotypes and can be anything in users file."),
            tags$li("Value: represents the phenotypic data for each time point and must be numeric.")
          ),
          tags$li("For each main tab panel separate input files needs to be uploaded, and users can use sample files from the GitHub directory to get used to the application."),
          tags$li("For Manhattan Plot panel, once the input file in right format is uploaded, users need to choose the separator value in the file. Once users choose the separator value, the time points will be automatically updated in Choose Time point box.  Users can select any time point and Interactive Manhattan plot will be automatically generated in left main panel, and users can interact with it to get more information. Users can choose the significant threshold level by moving slider input bar."),
          tags$li("In Manhattan Plot panel, users can also see the list of significant SNPs in table format by checking Display in Table Significant SNPs box. Once checked, List of SNPs will be displayed with higest p values on top. Users can use slider input to control the number of SNPs displayed in table."),
          tags$li("For Combined Manhattan Plot panel, once input file is uploaded and separator value choose, the Combined Manhattan Plot will be automatically generated in left main panel. Users can choose the significant threshold level and also number of columns in the combined/grid plot. Note: CSV file for Manhattan plot and combined Manhattan plot is same, however, they need to be uploaded separately."),
          tags$li("For Compare Significant Markers panel, It uses the same data file as uploded for Combined Manhatten Plot so no need to upload the data for this tab Panel. Once Significant Marker tab Panel is clicked,  plot will be automatically displayed in right panel. To modify or change the plot based on significant p value, directly enter the value by typing in the box."),
          tags$li("For Phenotypic Data Visualization, once the right CVS file is uploaded and file separator value is chosen, the time point will be automatically updated. The users can select the time point, and choose the time point to see the histogram and density plots. Users can also select box plot type to see the trend and variation across all the time points. All the plot types are interactive, and users can interact to get more information   .")
          
    
          
        ), style = "font-size: 16px; line-height: 1.7;"),
    hr(),
    
    
    h2(strong('Shiny Application is Hosted at:'),
       style = "font-size: 16px"),
     tags$div(
      tags$a(href="https://github.com/whussain2/ShinyAIM", "ShinyAIM:Website"),
      style = "font-size: 18px;"),
    hr(),
      
      h2(strong('Source Code and Sample Files')),
      
      tags$div(
        tags$p('Source code and sample files are available through GitHub at:.'),
        tags$a(href="https://github.com/whussain2/ShinyAIM", "ShinyAIM GitHub Repository"),
        style = "font-size: 18px;"),
      tags$br(),
      tags$div(
        tags$p(strong('How to cite ShinyAIM:'), "Waseem Hussain, Malachy Campbell, Harkamal Walia, and Gota Morota, University of Nebraska Lincoln (manuscript under preperation)"), 
        style = "font-size: 18px"),
      hr(),
      h3(strong('Contact Information and Help:')),
      tags$div(
        tags$p('Waseem Hussain - waseem.hussain@unl.edu; waseemhussain907@gmail.com, Gota Morota  -morota@unl.edu.'), 
        style = "font-size: 18px"),
      tags$p("Enjoy, and please give us valuable feedback.", style = "font-size: 18px"),
      hr(),
      tags$div(
        tags$footer('Copyright (c) 2000-2006, The Perl Foundation, code licensed under Artistic License 2.0'),
        style = "font-size: 16px")
))



         
       
      
       

       
      