## ==================================================================================== ##
# ShinyAIM for visualization of interactive Manhatten plots of longitudnal GWAS data.
# Copyright (C) 2018  Waseem Hussain
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

ui<-fluidPage(
  
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
                     fileInput('file1', 'Upload CSV data file for Manhatten Plot:',
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
                                 min = -log10(0.001), max = -log10(0.00000001),
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
                     fileInput('file2', 'Upload CSV data file for Manhatten Plot:',
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
                                 value =5, step=1)
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
                     fileInput('file3', 'Upload CSV data file:',
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
