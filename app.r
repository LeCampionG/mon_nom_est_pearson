library(shinythemes)
library(shiny)
library(shinydashboard)
require(visNetwork)
library(sparkline)
library(funModeling)
library(knitr)

options(shiny.maxRequestSize = 30*1024^2)

ui<-fluidPage(theme = shinytheme("yeti"),
              navbarPage(tags$a("Mon nom est Pearson",href = 'https://github.com/LeCampionG/mon_nom_est_pearson',
                                   icon("github"), target="_blank"),
                         tabPanel("A propos",
                                  #code Html
                                  h2(strong("Bienvenue sur l'application Mon Nom est Pearson !!!")),
                                  p(strong("Mon Nom est Pearson"),"vous permet de réaliser et visualiser des matrices de corrélations de vos jeux de données ainsi que des corrélogrammes."),
                                  p("Si vous ne savez pas qui sont Pearson, Spearman et Kendall, ou que vous souhaitez un bref rappel sur le calcul des corrélations et ce qu'il implique, avant d'utiliser cette application vous pouvez lire le tutoriel sur la réalisation de corrélations qui est hébergé sur la plateforme du pôle ARD :", strong(a("OUVRIR", href="http://ouvrir.passages.cnrs.fr/wp-content/uploads/2019/04/rapp_correlation.html")),"."),
                                  br(),
                                  h3(strong("Mode d'emploi")),
                                  p("Cette application propose 3 autres onglets en plus de celui"),
                                  p("Le premier vous permettra de charger et visualiser vos données et la distribution de vos variables. Vous ne pouvez importer qu'uniquement des fichiers au format csv et non vide. Par ailleurs, pour réaliser un calcul de corrélation il sera également nécessaire que chacune de vos variables soit codées numériquement."),
                                  p("Le deuxième onglet vous permettra de  calculer le coefficient de corrélation le plus adapté à vos données. En cas de doute je vous renvoie une nouvelle fois vers le tutoriel sur le calcul des corrélations(",a("ICI", href="http://ouvrir.passages.cnrs.fr/wp-content/uploads/2019/04/rapp_correlation.html"), "). Vous pourrez visualiser la matrice des corrélations avec une indication sur les coefficients significatifs, le corrélogramme associé et enfin vous aurez une aide à la lecture de la matrice ou du corrélogramme qui reprendra deux à deux les différents liens entre chacune des variables. Un téléchargement de la matrice sous forme de base de données est également possible."),
                                  p("Le dernier onglet vous permettra de créer votre propre corrélogramme à partir des coefficients de corrélations calculés. Il vous sera possible d'exporter votre corrélogramme au format png, pdf ou svg."),
                                  br(),
                                  p("Toutes remarques est la bienvenue, pour ce faire vous pouvez me contacter à cette adresse : gregoire.lecampion@cnrs.fr"),
                                  br(),
                                  h3(strong("Citation")), 
                                  p("Dans l'éventualité où vous utiliseriez cette application dans le cadre d'une publication, vous pouvez éventuellement citer cet outil comme ceci :"),
                                  p(strong("Le Campion G. ", a("Mon nom est Pearson: un outil de visualisation et d'exploration des corrélations.", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Mon_nom_est_Pearson/")," Pôle ARD UMR 5319 UMR Passages. 2019."))
                                  
                         ),
                         
                         tabPanel("Import des données",
                                  sidebarLayout(
                                    ############################################################
                                    # 1. Le menu de gauche, présentant les options de l'import #
                                    ############################################################
                                    sidebarPanel(fileInput("file1", "Charger un fichier CSV",
                                                           multiple = FALSE,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain", "text/semicolon-separated-values,text/plain" ,
                                                                      ".csv")),
                                                 h5(helpText("Le poid des fichier est limité à 30Mb")),
                                                 tags$hr(),
                                                 h5(helpText("Ajuster les options suivantes en fonction de votre fichier importé")),
                                                 # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                                 checkboxInput("header", "1ere ligne comme en-tête", TRUE),
                                                 #déterminer le séparateur de champ
                                                 radioButtons("sep", "Séparateur de champ",
                                                              choices = c(Comma = ",",
                                                                          Semicolon = ";",
                                                                          Tab = "\t"),
                                                              selected = ","),
                                                 #déterminer séparateur de texte
                                                 radioButtons("quote", "Séparateur de texte",
                                                              choices = c("Aucun" = "",
                                                                          "Guillemet double" = '"',
                                                                          "Guillemet simple" = "'"),
                                                              selected = '"'),
                                                 #Choix du mode de visualistaion
                                                 radioButtons("disp", "Visualiser",
                                                              choices = c("Uniquement les 1eres lignes" = "head",
                                                                          "Ensemble des données"= "all"),
                                                              selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("1- Mes données",
                                               uiOutput("tb1")
                                      ),
                                      tabPanel("2- Distribution",
                                               plotOutput("distribution", height="750px"),
                                               h5("Dans l'éventualité où vous estimez qu'une de vos variables suivrait une loi normale, confirmez le avec un test de normalité (K-S, Shapiro...)"),
                                               h5("Vous pouvez le faire grâce à l'application",a("Est-ce bien normal ?", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Est_ce_normal/"),"."))
                                    )
                                    ))),
                         tabPanel("Calcul des corrélations",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h3("Paramètres de l'analyse"),
                                      radioButtons("methode", label="Quelle corrélation ?", choices=list("auto","pearson", "spearman", "kendall"), selected="auto"),
                                      radioButtons("factor", label="Vos données se composent-elles de variables catégorielles ?", choices=list("Oui"="TRUE","Non"="FALSE"), selected="FALSE"),
                                      selectizeInput("correct", "Quelle correction apportée au p-value :", choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), multiple=FALSE, selected="holm")
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("1- Matrice de corrélation",
                                                 h3("Affichage de la matrice de corrélation"),
                                                 verbatimTextOutput("cor_result"),
                                                 h5("Lorsque votre coefficient de corrélation est assortie d'une étoile cela veut dire qu'il est significatif."),
                                                 h5("Le nombre d'étoile indique le niveau de significativité :*** cela veut dire que le p-value associé est <0.001, ** p-value <0.01, * p-value<0.05"),
                                                 downloadButton("downloadData", "Matrice sous forme de base de données")
                                        ),
                                        tabPanel("2- Réseau de Correlations",
                                                 plotOutput("correlogramme", height="750px"),
                                                      radioButtons(
                                                        inputId = "filetype_corr",
                                                        label = "Quel format d'image :",
                                                        inline = TRUE,
                                                        choices = list("PDF", "PNG","SVG")),
                                                      downloadButton(outputId = "downloadcorr01", label = "Télécharger Corrélogramme")
                                        )
                                      )
                                    )
                                  )
                                  
                         ),
                         tabPanel("Réaliser votre Corrélogramme",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h3("Paramètres de l'analyse"),
                                      selectizeInput("corrplot_method", label="Quelle méthode d'affichage des corrélations", choices=list("circle", "square", "ellipse", "number", "shade", "pie", "color"), multiple=FALSE, selected="color"),
                                      helpText("Conformément aux principes de sémiologie graphique, un caractère de taux ou de rapport  doit être représenté à l'aide de la variable rétinienne Valeur prenant la forme d'un aplat de couleurs. Les représentations des corrélations sont formes de cercles ou de carrés proportionnels sont à déconseiller."),
                                      selectizeInput("layout", "Quelle forme pour votre corrélogramme :", choices = c("full", "upper", "lower"), multiple=FALSE, selected="full"),
                                      selectizeInput("color", "Quelle palette de couleur", choices = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="RdGy"),
                                      helpText("Les palettes proposées ici viennent de RcolorBrewer, une adaptation sur R de la palette cartographique de référence colorbrewer. Attention toutefois aux palettes dite arc-en-ciel (comme spectral) qui sont à déconseillées pour représenter un phénomène quantitatif. A voir les travaux de", strong(a("Laurent Jégou",href="https://couleurs.hypotheses.org/453")),"pour davantage de précisions."),
                                      selectizeInput("order", "Ordonnancement des variables", choices = c("alphabet", "hclust", "FPC", "AOE"), multiple=FALSE, selected="alphabet"),
                                      sliderInput("siglevel", label="Niveau de significativité", value=0.05, min=0, max=0.1, step=0.01),
                                      helpText("Pour rappel, si vous augmentez le niveau de significativité au dessus de 0.05 vous risquez d'intégrer des corrélation non significative. Dans le cas des p-value situés entre 0.05 et 0.1 on parle d'un effet tendanciel et non de significativité."),
                                      selectizeInput("pch", "Comment marquer la non significativité", choices = c("pch", "p-value", "blank"), multiple=FALSE, selected="blank"),
                                      sliderInput("tlsrt", label="Degrée d'inclinaison des intitulés de variables", value=45, min=0, max=90, step=5),
                                      sliderInput("addrect", label="Dessiner rectangle autour des classes (ne marche qu'avec l'ordonnencement des variables hclust et une forme pleine du corrélogramme)", value=1, min=0, max=100, step=1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats#
                                    ############################################################################
                                    mainPanel(plotOutput("customgraph", height="750px"),
                                              radioButtons(
                                                inputId = "filetype_corr2",
                                                label = "Quel format d'image :",
                                                inline = TRUE,
                                                choices = list("PDF", "PNG","SVG")),
                                              downloadButton(outputId = "downloadcorr02", label = "Télécharger Corrélogramme")
                                    )
                                  )
                         ),
                         tabPanel("Histogramme par variable",
                                  sidebarLayout(sidebarPanel(
                                    h4("Choisir la variable à explorer :"),
                                    uiOutput("select_var1"),
                                    selectizeInput("colorhist", "Quelle palette de couleur ? (inspiré par la palette de couleur des films de Wes Anderson)", choices = c("BottleRocket1","BottleRocket2","Rushmore1", "Rushmore","Royal1", "Royal2","Zissou1", "Darjeeling1", "Darjeeling2","Chevalier1","FantasticFox1","Moonrise1", "Moonrise2","Moonrise3", "Cavalcanti1", "GrandBudapest1", "GrandBudapest2", "IsleofDogs1", "IsleofDogs2"), multiple=FALSE, selected="RdGy"),
                                    selectizeInput("themehisto", "Quel thème pour votre graphique ?", choices = c("theme_gray","theme_bw","theme_linedraw", "theme_light","theme_minimal", "theme_void","theme_dark", "theme_tufte", "theme_economist","theme_calc","theme_wsj","theme_hc"), multiple=FALSE)
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("1-Visualiser les données",
                                               uiOutput("table_var"),
                                               downloadButton("downloadDatahist", "Télécharger")),
                                      tabPanel("2- Histogramme",
                                               plotOutput("histo", height="750px"),
                                               radioButtons(
                                                 inputId = "filetype_histo",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PDF", "PNG","SVG")),
                                               downloadButton(outputId = "downloadhisto", label = "Télécharger Histogramme")
                                      )
                                      
                                    )
                                  )
                                  )     
                         ),
                         tabPanel("Scatterplot par variable",
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("1-Visualiser les données",
                                               p("Choisir la variable à explorer :"),
                                               uiOutput("select_var2"),
                                               uiOutput("table_var2"),
                                               downloadButton("downloadDatascatter", "Télécharger")),
                                      tabPanel("2- Scatter plot vue d'ensemble",
                                               selectizeInput("themescatterplot", "Quel thème pour votre graphique ?", choices = c("theme_gray","theme_bw","theme_linedraw", "theme_light","theme_minimal", "theme_void","theme_dark", "theme_tufte", "theme_economist","theme_calc","theme_wsj","theme_hc"), multiple=FALSE),
                                               plotOutput("scatter", height="750px"),
                                               radioButtons(
                                                 inputId = "filetype_scatter",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PDF", "PNG","SVG")),
                                               downloadButton(outputId = "downloadscatter", label = "Télécharger Scatter plot")
                                      ),
                                      tabPanel("3- Scatter plot 2 à 2",
                                               uiOutput("select_var3"),
                                               uiOutput("select_var4"),
                                               selectizeInput("themescatterplot2", "Quel thème pour votre graphique ?", choices = c("theme_gray","theme_bw","theme_linedraw", "theme_light","theme_minimal", "theme_void","theme_dark", "theme_tufte", "theme_economist","theme_calc","theme_wsj","theme_hc"), multiple=FALSE),
                                               plotOutput("scatter2", height="750px"),
                                               radioButtons(
                                                 inputId = "filetype_scatter2",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PDF", "PNG","SVG")),
                                               downloadButton(outputId = "downloadscatter2", label = "Télécharger Scatter plot 2")
                                               
                                      )
                                    )
                                  )     
                                  #)
                         )
              )
)




server <- function(input, output, session) {
  
  
  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  library(sparkline)
  require(report)
  library(correlation)
  library(see)
  library(ggraph)
  library(corrplot)
  library(funModeling)
  library(viridis)
  library(RColorBrewer)
  require(visNetwork)
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(corrr)
  library(ggplot2)
  library(wesanderson)
  library(ggthemes)
  
  #####################################################
  # 5. Charger les données importées et les visualiser#
  #####################################################
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
  })  
  output$table <- renderTable({
    req(input$file1)
    
    # Si le séparateur de champs est un point-virgule,
    # Avoir une virgule comme séparateur causera une erreur dans la fonction read.csv
    df <- reactive({ read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
    })
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  output$distribution <- renderPlot({
    plot_num(dat())
  })
  
  ################################################################
  # 6. Charger les données et maj des selectizeInput #
  ################################################################ 
  dat <- reactive({
    file1 <- input$file1
    req(file1)
    dataSet <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- colnames(dataSet)
    #row <- nrow(dataSet)
    
    updateSelectizeInput(session, "selectVd", "Choix des variables à corréler", choices = vars)
    dataSet
  })
  
  observe({
    varVI <- colnames(dat())
    updateSelectizeInput(session, "selectVd", "Choix des variables à corréler", choices = varVI)
  })
  
  
  corr <- reactive({
    correlation(dat(), method=input$methode, p_adjust=input$correct, include_factors =input$factor)
  })
  
  output$cor_result<-renderPrint({as.table(corr())})
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("matrice_bdd", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(corr(), file, row.names = TRUE)
    }
  )
  
  output$correlogramme <- renderPlot({ 
    plot(corr())
  })
  
reseau <- reactive({
  plot(corr())
})
  
    output$downloadcorr01 <- downloadHandler(
      filename = function(){
        paste("Correlogramme", tolower(input$filetype_corr), sep =".")
      }, 
      content = function(file){
        width  <- 900
        height <- 900
         pixelratio <- 2
        if(input$filetype_corr == "PNG")
          png(file, width=width*pixelratio, height=height*pixelratio,
              res=72*pixelratio, units = "px")
        else if(input$filetype_corr == "SVG")
          svg(file, width=12, height=12)
        else
          pdf(file, width = 12, height = 12)
         print(reseau())
        dev.off()
      })
  
  step1 <- reactive({
   as.table(corr())
  })
  
  step2 <- reactive({
    as_tibble(rownames_to_column(step1()))
  })
  
  step3 <- reactive({
    step2() %>% select(-rowname)
  })
  
  step4 <- reactive({
    step3() %>% rename(rowname=Parameter)
  })
  
  step5 <- reactive({
    column_to_rownames(step4())
  })
  
  M <- reactive({
    as.matrix(step5())
  })
  
  nb <- reactive({
   nb <-  nrow(dat())
  })
  
  p.value <- reactive({
    cor_to_p(M(), n=nb(), method = "auto")
  })
  
  p.mat <-reactive({
    p.value()$p
  })
  
  
#  M <- reactive({corr()$values$r})
#  pmat <- reactive({corr()$values$p})
  
  output$customgraph <- renderPlot({ 
    print(corrplot(M(), p.mat = p.mat(), method= input$corrplot_method, type= input$layout, order=input$order , tl.srt=input$tlsrt  , sig.level = input$siglevel, insig = input$pch, addrect = input$addrect, col= brewer.pal(n = 20, name = input$color)))
  })
  
  output$downloadcorr02 <- downloadHandler(
    filename = function(){
      paste("Mon_Correlogramme", tolower(input$filetype_corr2), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_corr2 == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_corr2 == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      corrplot(M(), p.mat = p.mat(), method= input$corrplot_method, type= input$layout, order=input$order , tl.srt=input$tlsrt  , sig.level = input$siglevel, insig = input$pch, addrect = input$addrect, col= brewer.pal(n = 20, name = input$color))
      dev.off()
    })
  
  
  
  x <- reactive({ 
    
    M() %>% 
      as_cordf() %>% focus(input$selectvar1)
    
  })
  
  bdhisto <- reactive({
    step1() %>% select(-Parameter)
  })
  
  # 1st Input rendered by the server <---
  output$select_var1 <- renderUI({
    
    selectizeInput('selectvar1', '', choices = c("select" = "",  colnames(bdhisto())))
    
  })
  
  output$table_var <- renderTable({ 
    validate(
      need(input$selectvar1 != "", "Sélectionner une variable à explorer")
    )
    x()
    
  })
  
  x_df <- reactive({as.data.frame(x())})
  
  pos <- reactive({x_df()[,2]>0})
  
  
  output$downloadDatahist <- downloadHandler(
    filename = function() {
      paste("data_histo", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(x_df(), file, row.names = TRUE)
    }
  )
  
  hist <- reactive({x_df() %>% 
      #mutate(rowname = factor(x_df()[,1], levels = rowname[order(x_df()[,2])])) %>%  # Order by correlation strength
      ggplot(aes(x =reorder(x_df()[,1], x_df()[,2]), y = x_df()[,2], fill=pos())) +
      geom_bar(stat = "identity", colour="black") +
      scale_fill_manual(values = wes_palette(n=3, name=input$colorhist), guide=FALSE) +
      ylab("Correlation") +
      xlab("Variables") +
      get(input$themehisto)()
  })
  
  output$histo <- renderPlot({
    validate(
      need(input$selectvar1 != "", "Sélectionner une variable à explorer")
    )
    hist()
  })
  
  
  y <- reactive({ 
    vary <- input$selectvar2
    dat() %>% gather(-vary, key = "var", value = "value") 
    
  })
  
  # 1st Input rendered by the server <---
  output$select_var2 <- renderUI({
    
    selectizeInput('selectvar2', '', choices = c("select" = "",  colnames(dat())))
    
  })
  
  output$table_var2 <- renderTable({ 
    validate(
      need(input$selectvar2 != "", "Sélectionner une variable à explorer")
    )
    y()
    
  })
  
  output$downloadDatascatter <- downloadHandler(
    filename = function() {
      paste("data_scatterplot", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(y(), file, row.names = TRUE)
    }
  )
  
  scatplot <- reactive({y() %>% 
      ggplot(aes(x = y()[,3], y = y()[,1])) +
      facet_wrap(~ y()[,2], scales = "free") +
      geom_point() +
      stat_smooth() +
      get(input$themescatterplot)()
  })
  
  
  output$scatter <- renderPlot({ 
    validate(
      need(input$selectvar2 != "", "Sélectionner une variable à explorer")
    )
    scatplot()
  })
  
  
  output$select_var3 <- renderUI({
    
    selectizeInput('selectvar3', 'Axe des abscisses (x)', choices = c("select" = "",  colnames(dat())))
    
  })
  
  output$select_var4 <- renderUI({
    
    selectizeInput('selectvar4', 'Axe des ordonnés (y)', choices = c("select" = "",  colnames(dat())))
    
  })
  
  
  scatplot2 <- reactive({dat() %>% 
      ggplot(aes_string(x = input$selectvar3, y = input$selectvar4)) +
      geom_point() +
      stat_smooth() +
      get(input$themescatterplot2)()
  })
  
  output$scatter2 <- renderPlot({ 
    validate(
      need(input$selectvar3 != "", "Sélectionner une variable pour l'axe des x")
    )
    validate(
      need(input$selectvar4 != "", "Sélectionner une variable pour l'axe des y")
    )
    scatplot2()
  })
  
  
  output$downloadhisto <- downloadHandler(
    filename = function(){
      paste("Mon_Histogramme", tolower(input$filetype_histo), sep =".")
    }, 
    content = function(file2){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_histo == "PNG")
        png(file2, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_histo == "SVG")
        svg(file2, width=12, height=12)
      else
        pdf(file2, width = 12, height = 12)
      print(hist())
      dev.off()
    })
  
  
  output$downloadscatter <- downloadHandler(
    filename = function(){
      paste("Mon_Scatterplot", tolower(input$filetype_scatter), sep =".")
    }, 
    content = function(file3){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_scatter == "PNG")
        png(file3, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_scatter == "SVG")
        svg(file3, width=12, height=12)
      else
        pdf(file3, width = 12, height = 12)
      print(y() %>% 
              ggplot(aes(x = y()[,3], y = y()[,1])) +
              facet_wrap(~ y()[,2], scales = "free") +
              geom_point() +
              stat_smooth() +
              get(input$themescatterplot)())
      dev.off()
    })
  
  output$downloadscatter2 <- downloadHandler(
    filename = function(){
      paste("Mon_scatterplot2", tolower(input$filetype_scatter2), sep =".")
    }, 
    content = function(file4){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_scatter2 == "PNG")
        png(file4, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_scatter2 == "SVG")
        svg(file4, width=12, height=12)
      else
        pdf(file4, width = 12, height = 12)
      print(dat() %>% 
              ggplot(aes_string(x = input$selectvar3, y = input$selectvar4)) +
              geom_point() +
              stat_smooth() +
              get(input$themescatterplot2)())
      dev.off()
    })
  
  
  
}



shinyApp(ui=ui,server=server)


