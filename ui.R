library(shiny)
library(shinyWidgets)
library(ggbiplot)
library(ggplot2)
library(DT)

ui <- fluidPage(
  headerPanel(
    title=HTML(paste("", tags$span(style="color:black; font-size:18px; font-family:lobster; font-weight:bold", "PCA plotter; v1.02"), sep = "")), windowTitle = "PCA plotter"), 

  sidebarLayout(
    sidebarPanel(
      tags$div(style="margin-bottom:-20px; margin-top:-20px", title="Load data frame in .csv or .txt format",
               fileInput("file1", h5(style="margin-bottom:-10px","Choose DATA File"),
                multiple = TRUE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
      
      tags$div(style="margin-bottom:-10px",checkboxInput("header", "Header", TRUE)),
      radioButtons("sep", h5(style="margin-bottom:-20px", "Separator"), inline = T, choices = c(Comma = ",", Tab = "\t"), selected = ","),
      tags$div(style="margin-bottom:-10px",prettySwitch("log", "DATA Log-transformation", value = FALSE)),
      tags$div(style="margin-bottom:-10px",prettySwitch("scale", "DATA scaling", value = FALSE)),
      hr(style="border-color:grey"),
      
      radioButtons("factors", h5(style="margin-bottom:-20px; margin-top:-20px","Choose NUMBER OF FACTORS"),
                   choices = c("1: Color","2: Color/Shape","3: Color/Shape/Size"),
                   selected = "1: Color"),
      
      tags$div(style="margin-bottom:-20px; margin-top:-10px",title="Load vector of 1st factor names for color representation",
        fileInput("file2", h5(style="margin-bottom:-10px","Choose COLOR File"), multiple = TRUE,
          accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))),
      
      tags$div(style="margin-bottom:-10px",title="Load vector of 2nd factor names for shape representation",
        fileInput("file3", h5(style="margin-bottom:-10px","Choose SHAPE File"), multiple = TRUE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
    
      tags$div(style="margin-bottom:-20px; margin-top:-20px",title="Load vector of 3rd factor names for size representation",
               fileInput("file4", h5(style="margin-bottom:-10px","Choose SIZE File"), multiple = TRUE,
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
      hr(style="border-color:grey"),
      h5(style="margin-bottom:20px; margin-top:10px", "PCA color theme"),
      tags$div(style="margin-top:-10px", sliderTextInput("color_theme", label=NULL, force_edges = T, hide_min_max = T, choices = c("Multicolor","Wong's color blindness set", "Grayscale"), grid = T)),
      hr(style="border-color:grey"),
      h5(style="margin-bottom:20px; margin-top:20px", "PCA plot options"),
      tags$div(style="margin-bottom:-10px; margin-top:-10px", prettySwitch("pca_label", "Add labels", value = FALSE)),
      tags$div(style="margin-bottom:-10px", prettySwitch("pca_ellipse", "Add ellipse", value = FALSE)),
      hr(style="border-color:grey"),
      h6(style="color:black; font-family:helvetica, cursive; text-align:justify", "The software is based on", a("R-project", href="https://www.r-project.org"), "packages:", a("shiny", href="https://shiny.posit.co"), "and", a("ggplot2.", href="https://ggplot2.tidyverse.org"),"PCA utilizes 'prcomp'-function of", a("stat-package.", href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp")),
      hr(style="border-color:grey"),
      h6(style="color:black; font-family:lobster, cursive; text-align:center", "with any questions please contact me by:", h6(style="color:black; font-family:monospace; font-weight:bold; text-align:center","albertbat@bgu.ac.il"),
      h6(style="text-align:center",tags$img(src="omrf.png", height="10%", width="10%", style="opacity:0.5")),
      h6(style="text-align:center",tags$img(src="bgu.png", height="8%", width="8%", style="opacity:0.5"))) 
    ),
    
    mainPanel(
     tabsetPanel(
        tabPanel("Tutorial",
                 h6(style="color:red; font-family:helvetica, text-align:justify", "NEW in version 1.02:"),
                 h6(style="color:green; font-family:helvetica, text-align:justify", "You can now download example files."),
                 h6(style="color:green; font-family:helvetica, text-align:justify", "Tutorial was updated."),
                 h6(style="color:green; font-family:helvetica, text-align:justify", "Legend's font size was increased."),
                 h6(style="color:green; font-family:helvetica, text-align:justify", "Plots in .png format have a transparent background now."),
                 br(),  
                 downloadButton("downloadExample", label = "Example Data", title="Download example dataframe format"), downloadButton("downloadColor", label="Example Factor 1", title="Download example format for color-assigned factor"), downloadButton("downloadShape", label="Example Factor 2", title="Download example format for shape-assigned factor"), downloadButton("downloadSize", label="Example Factor 3", title="Download example format for size-assigned factor"),
        
        tags$div(style="margin-top:20px", "1. Click 1st", tags$b("Browse"), "button to locate and upload DATA file in the view below:"),
        br(),
        tableOutput("example1"),
        tags$div(style="text-align:justify; margin-top:-20px", "Where 'Feature' - is a gene, protein, metabolite, etc., G - is any condition (for example, genotype), T - is an another condition (for example treatment), and R - is a third conditions (for example replicate)."),
        p(),
        tags$div(style="text-align:justify", "In this example there are four groups under two conditions (G and T), two levels of the each condition (1 and 2) and each group is in duplicate (biological/technical replicates - 1 and 2). The current format will allow to visualize all three types of the samples. The actual names of objects are allowed if they follow the style above. It means that object names must not be duplicated."),
        p(),
        tags$div(style="text-align:justify", "Missing values are not allowed for PCA. The data will be automatically inspected on missing values and the results will be displayed for the user at the top of data frame on", tags$b("Data tab.")),
        p(),
        tags$div(style="text-align:justify", "2. The PCA is based on variance. This makes it sensitive to the variables that level is strongly different from the majority of the other variables. To avoid an effect caused by this, it is usually recommended to transform or scale the dataset. The application provides options to perform PCA on the raw data, an Log-transformed (use", tags$b("DATA Log-transformation switch)"), "and on scaled (the data is standardized to mean 0 and standard deviation 1) data (use", tags$b("DATA scaling switch).")),
        p(),
        tags$div(style="text-align:justify", "3. Most data do not require visualization of three, and even two factors. In respect to this, use a proper", tags$b("NUMBER OF FACTORS"), "selection. At least one factor", tags$b("(1: Color)"), "is required for graphical visualization of PCA plot."),
        p(),
        tags$div(style="text-align:justify", "4. After determining the needed number of factors, consequently click 2nd, 3rd, 4th", tags$b("Browse"), "buttons (from the top), locate and upload", tags$b("COLOR, SHAPE, SIZE"), "file(s) in the view below:"),
        p(),
        tableOutput("example2"),
        tags$div(style="margin-top:-20px", "where", tags$b("G"), "is a first condition, and there are up to nine different levels of this condition;"),
        br(),
        tableOutput("example3"),
        tags$div(style="margin-top:-20px", "where", tags$b("T"), "is a second condition, and there are up to six different levels of this condition;"),
        br(),
        tableOutput("example4"),
        tags$div(style="margin-top:-20px", "where", tags$b("R"), "is a third condition, and there are up to four different levels of this condition."),
        p(),
        tags$div(style="text-align:justify", "The groups according to these files will be colored/shaped/sized differently on PCA plot. The actual names of conditions are allowed if they follow the style above. It means that condition names must be identical for the entire group and different from the another group."),
        br(),
        tags$div(style="text-align:justify", "5. Click on", tags$b("Data"), "tab to see the uploaded data frame, factor strings, and presence of missing values (optionally)."),
        br(),
        tags$div(style="text-align:justify", "6. After uploading the files click on", tags$b("PCA"), "tabs to obtain graphical results."),
        br(),
        tags$div(style="text-align:justify", "7.", tags$b("PCA color theme"), "slider provides options for visualization based on color preferences, including an option suggested for color-blinded users by", a("Wong,", href="https://www.nature.com/articles/nmeth.1618"), "and greyscale option for users interested in black-white figures only. The data that consist of more than 4 groups can be difficult to distinguish in this mode."),
        br(),
        tags$div(style="text-align:justify", "8. Use", tags$b("PCA plot style"), "switches to add sample labels, and/or 95% CI group ellipse (based on COLOR factor)."),
        br(),
        tags$div(style="text-align:justify", "9.", tags$b("PCA summary"), "tab provides information about the input of every object to the samples separation on PCA plot."),
        p()),
        tabPanel("Data", tableOutput("table4"), tableOutput("table3"), tableOutput("table2"), htmlOutput("numeric"), tableOutput("table1")), 
        tabPanel("PCA", h5("Please make sure that PCA parameters (color, shape and size) are loaded correspondingly to Factor selection"), downloadButton(outputId = "download_plot1", label = "Download PCA in .pdf"), downloadButton(outputId = "download_plot1a", label = "Download PCA in .png"), plotOutput("plot1"), downloadButton(outputId = "download_plot2", label = "Download PCA in .pdf"), downloadButton(outputId = "download_plot2a", label = "Download PCA in .png"), plotOutput("plot2")),
        tabPanel("PCA Summary", h5("Please note that values in the table are absolute values of the loadings"), downloadButton(outputId = "download_table5", label = "Download results"), dataTableOutput("table5"))
        
      )
    )
  )
)
