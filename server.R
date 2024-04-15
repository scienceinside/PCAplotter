server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) 
  
  getData1 <- reactive({
    inFile <- input$file1
        if (is.null(input$file1))
        return(NULL)
    read.csv(inFile$datapath, header=input$header, row.names = 1, sep=input$sep, 
             quote=input$quote)
  })
  
  data1 <-reactive({
    if(input$log==FALSE){
    getData1()}
    else{
      log10(getData1())
    }
  })

  getData2 <- reactive({
    inFile <-input$file2
    if (is.null(input$file2))
      return(NULL)
    scan(inFile$datapath, what = "character", sep = ",")
  })
  
  getData3 <- reactive({
    inFile <-input$file3
    if (is.null(input$file3))
      return(NULL)
    scan(inFile$datapath, what = "character", sep = ",")
  })
  
  getData4 <- reactive({
    inFile <-input$file4
    if (is.null(input$file4))
      return(NULL)
    scan(inFile$datapath, what = "character", sep = ",")
  })
  
  output$downloadExample <- downloadHandler(
    filename <- function() {
      paste("data", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("data_test.csv", file)
    })
  
  output$downloadColor <- downloadHandler(
    filename <- function() {
      paste("color", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("color_test.csv", file)
    })
  
  output$downloadShape <- downloadHandler(
    filename <- function() {
      paste("shape", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("shape_test.csv", file)
    })
  
  output$downloadSize <- downloadHandler(
    filename <- function() {
      paste("size", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("size_test.csv", file)
    })
  
  output$example1 <- renderTable(read.csv("./tutorial_data1.csv"), colnames = T, striped = T, bordered = T)
  output$example2 <- renderTable(read.table("./tutorial_data2.csv", sep = ","), colnames = F, striped = T, bordered = T)
  output$example3 <- renderTable(read.table("./tutorial_data3.csv", sep = ","), colnames = F, striped = T, bordered = T)
  output$example4 <- renderTable(read.table("./tutorial_data4.csv", sep = ","), colnames = F, striped = T, bordered = T)
  
  output$table1 <- renderTable({
    req(input$file1)
    df_full=cbind(Name=row.names(data1()),data1())
  })
  
  output$numeric <- renderText({
    req(input$file1)
    if (all(sapply(data1(),is.finite)))
    paste(tags$span(style="color:green", h6("No missing values")))
    else paste(tags$span(style="color:red", h6("Missing values")))
  })
  
  output$table2 <- renderTable({
    req(input$file2)
    df <- getData2()
    cbind("Color",t(df))
  }, colnames=F, bordered=T)
  
  output$table3 <- renderTable({
    req(input$file3)
    df <- getData3()
    cbind("Shape",t(df))
   }, colnames=F, bordered=T)
  
  output$table4 <- renderTable({
    req(input$file4)
    df <- getData4()
    cbind("Size",t(df))
  }, colnames=F, bordered=T)
  
   plotPCA_1_2 <-reactive({
    gr_col <- as.vector(getData2())
    gr_shape <- as.vector(getData3())
    gr_size <- as.vector(getData4())
    if(input$scale==FALSE){
    pca=prcomp(t(data1()), scale=F)}
    else {pca=prcomp(t(data1()), scale=T)}
    
    my_pca_plot_1_2 <- function(pca_ellipse){
      ggbiplot(pca, choices=c(1,2), scale = 0, groups = gr_col, ellipse = pca_ellipse, ellipse.prob = 0.95, alpha = 0,  var.axes = F) 
    }

    my_theme <- function() { 
      theme_bw() + 
        theme(plot.background = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), panel.border = element_rect(color = "grey"), axis.ticks = element_blank(), axis.title = element_text(colour = "black"), axis.text = element_blank(), legend.title=element_blank(), legend.key = element_blank(), legend.background = element_blank(), legend.text = element_text(colour = "black", size = 12), aspect.ratio = 1) 
    }
    
    my_hlines <- function() {
      geom_hline(aes(yintercept = 0), linetype="dashed", color="grey") 
    }
    
    my_vline <- function() {
      geom_vline(aes(xintercept = 0), linetype="dashed", color="grey")
    }
    
    my_color <- function() {
      if (input$color_theme=="Multicolor") {scale_colour_brewer(palette = "Set1")}
      else if (input$color_theme=="Wong's color blindness set") {scale_color_manual(values =c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#0072B2", "#D55E00", "#CC79A7"))}
      else if (input$color_theme=="Grayscale") {scale_color_manual(values = c("#000000", "#A0A0A0", "#D0D0D0", "#F0F0F0", "#B0B0B0", "#C0C0C0", "#E0E0E0", "#F5F5F5", "#F8F8F8"))}
    }
    
    my_text <- function() {
      geom_text(label=colnames(data1()), aes(colour=factor(gr_col)), hjust=1, vjust=2, angle=45, size=3, show.legend = FALSE)
    }
    
    my_style <- function() {
      if (input$pca_label==FALSE){
        list(my_theme(),my_hlines(),my_vline(),my_color())
      }
      else {list(my_theme(),my_hlines(),my_vline(),my_color(),my_text())
      }
    }
    
    
  if(input$factors=="1: Color"){
        my_pca_plot_1_2(input$pca_ellipse) +
        my_style() +
        geom_point(size=4, aes_string(colour=factor(gr_col))) +
        guides(color=guide_legend(order=1))
    }
  else if (input$factors=="2: Color/Shape"){
        my_pca_plot_1_2(input$pca_ellipse) +
        my_style() + 
        geom_point(size=4, aes_string(colour=factor(gr_col), shape=factor(gr_shape))) + 
        guides(color=guide_legend(order=1), shape=guide_legend(order=2)) 
    }
  else if (input$factors=="3: Color/Shape/Size"){
        my_pca_plot_1_2(input$pca_ellipse) +
        my_style() + 
        geom_point(aes_string(colour=factor(gr_col), shape=factor(gr_shape), size=factor(gr_size))) + 
        guides(color=guide_legend(order=1), shape=guide_legend(order=2), size=guide_legend(order=3))  
    }
})
  
    output$plot1 <- renderPlot({
    req(input$file1)
    if(input$factors=="1: Color"){
      req(input$file2)   
    }
    else{
      if(input$factors=="2: Color/Shape"){
        req(input$file2) 
        req(input$file3)
      }
      else{
        req(input$file2)
        req(input$file3)
        req(input$file4)
      }
    }
  plotPCA_1_2()
  })
  
  output$download_plot1 = downloadHandler(
    filename = function(){
      paste("pca_1_2.pdf")
    },
    content = function(file){
      pdf(file, width = 6, height = 6, family = "Helvetica")
      plot(plotPCA_1_2())
      dev.off()
    })
  
  output$download_plot1a = downloadHandler(
    filename = function(){
      paste("pca_1_2.png")
    },
    content = function(file){
      png(file, width = 3000, height = 3000, res=600, bg="transparent")
      plot(plotPCA_1_2())
      dev.off()
    })
  
  plotPCA_1_3 <-reactive({
    gr_col <- as.vector(getData2())
    gr_shape <- as.vector(getData3())
    gr_size <- as.vector(getData4())
    if(input$scale==FALSE){
      pca=prcomp(t(data1()), scale=F)}
    else {pca=prcomp(t(data1()), scale=T)}
    
    my_pca_plot_1_3 <- function(pca_ellipse){
      ggbiplot(pca, choices=c(1,3), scale = 0, groups = gr_col, ellipse = pca_ellipse, alpha = 0, ellipse.prob = 0.95,  var.axes = F) 
    }
    
    my_theme <- function() { 
      theme_bw() + 
        theme(plot.background = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), panel.border = element_rect(color = "grey"), axis.ticks = element_blank(), axis.title = element_text(colour = "black"), axis.text = element_blank(), legend.title=element_blank(), legend.key = element_blank(), legend.background = element_blank(), legend.text = element_text(colour = "black", size = 12), aspect.ratio = 1) 
    }
    
    my_hlines <- function() {
      geom_hline(aes(yintercept = 0), linetype="dashed", color="grey") 
    }
    
    my_vline <- function() {
      geom_vline(aes(xintercept = 0), linetype="dashed", color="grey")
    }
    
    my_color <- function() {
      if (input$color_theme=="Multicolor") {scale_colour_brewer(palette = "Set1")}
      else if (input$color_theme=="Wong's color blindness set") {scale_color_manual(values =c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#0072B2", "#D55E00", "#CC79A7"))}
      else if (input$color_theme=="Grayscale") {scale_color_manual(values = c("#000000", "#A0A0A0", "#D0D0D0", "#F0F0F0", "#B0B0B0", "#C0C0C0", "#E0E0E0", "#F5F5F5", "#F8F8F8"))}
    }
    
    my_text <- function() {
      geom_text(label=colnames(data1()), aes(colour=factor(gr_col)), hjust=1, vjust=2, angle=45, size=3, show.legend = FALSE)
    }
    
    my_style <- function() {
      if (input$pca_label==FALSE){
        list(my_theme(),my_hlines(),my_vline(),my_color())
      }
      else {list(my_theme(),my_hlines(),my_vline(),my_color(), my_text())
      }
    }

    if(input$factors=="1: Color"){
      my_pca_plot_1_3(input$pca_ellipse) +
        my_style() +
        geom_point(size=4, aes(colour=factor(gr_col))) +
        guides(color=guide_legend(order=1))
    }
    else if (input$factors=="2: Color/Shape"){
      my_pca_plot_1_3(input$pca_ellipse) +
        my_style() + 
        geom_point(size=4, aes(colour=factor(gr_col), shape=factor(gr_shape))) + 
        guides(color=guide_legend(order=1), shape=guide_legend(order=2)) 
    }
    else if (input$factors=="3: Color/Shape/Size"){
      my_pca_plot_1_3(input$pca_ellipse) +
        my_style() + 
        geom_point(aes(colour=factor(gr_col), shape=factor(gr_shape), size=factor(gr_size))) + 
        guides(color=guide_legend(order=1), shape=guide_legend(order=2), size=guide_legend(order=3))
    }
  })
  
  output$plot2 <- renderPlot({
    req(input$file1)
    if(input$factors=="1: Color"){
      req(input$file2)   
    }
    else{
      if(input$factors=="2: Color/Shape"){
        req(input$file2) 
        req(input$file3)
      }
      else{
        req(input$file2)
        req(input$file3)
        req(input$file4)
      }
    }
    plotPCA_1_3()
  })
  
  output$download_plot2 = downloadHandler(
    filename = function(){
      paste("pca_1_3.pdf")
    },
    content = function(file){
      pdf(file, width = 6, height = 6, family = "Helvetica")
      plot(plotPCA_1_3())
      dev.off()
    })
  
  output$download_plot2a = downloadHandler(
    filename = function(){
      paste("pca_1_3.png")
    },
    content = function(file){
      png(file, width = 3000, height = 3000, res=600, bg="transparent")
      plot(plotPCA_1_3())
      dev.off()
    })
  
  pcaSummary <- reactive({
    if(input$scale==FALSE){
      pca=prcomp(t(data1()), scale=F)}
    else {pca=prcomp(t(data1()), scale=T)}
    rotation=as.data.frame(round(abs(pca$rotation[,1:3]),2))
  })
  
  output$table5 <- renderDataTable({
    req(input$file1)
    pcaSummaryOutput=datatable(pcaSummary(),class = "cell-border stripe", colnames=c("Feature"=1))
  })
  
  output$download_table5 = downloadHandler(
    filename = function(){
      paste("pca_results.csv")
    },
    content = function(file){
      write.csv(pcaSummary(), file)
  })
  
}
