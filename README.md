<b>ShinyAIM: Application for Interactive Manhattan Plot Visualization<b>


The application is hosted on Shinyapps.io here: https://chikudaisei.shinyapps.io/shinyaim/

The application can be run  locally with just one command in R software or RStudio:
library(shiny)
shiny::runGitHub("ShinyAIM", "whussain2")
Make sure the required libraries including "shiny", "ggplot2", "dplyr", "grid", "plotly", "manhattanly", "forcats" are installed on your machine before running the above command.
These packages can be installed through following command line: 

install.packages(c("shiny","ggplot2","dplyr","grid","plotly","manhattanly","forcats"))


Licensing

This shiny code is licensed under the For more details see Artistic License 2.0. at https://opensource.org/licenses/Artistic-2.0. 
ShinyAIM for visualization of interactive Manhattan plots of longitudnal GWAS data.
Copyright (c) 2018, Waseem Hussain,  code licensed under Artistic License 2.0.

Contact

You may contact the author of this code, Waseem hussain, at <waseem.hussain@unl.edu>; <waseemhussain907@gmail.com>
Code adapted for use in app:

DOI

https://doi.org/10.5281/zenodo.1419296
