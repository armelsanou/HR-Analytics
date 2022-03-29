#library(devtools)
library(ggplot2)
#library(ggthemes)
library(plotly)
#library(shiny)
library(shinydashboard)
library(DT)
library(DBI)


###mes bib badis
library(shiny)
library(ggplot2)
library(reshape2)
library(visdat)
library(funModeling)
library(dplyr)
library(corrplot)
#library(summarytools) #Commenté Armel car, m'empêche de lancer l'application

# librairies NAWEL
library(e1071)
library(lattice)
library(tidyverse) 
library(Boruta) #pour la selection des features
library(tree) 
library(ROCR)
library(caret)


#Load libraries ARMEL&REDA
library('fastDummies')
library('shinyalert')
library(clusterSim)
library(rsconnect)


#library(RMySQL)

#uc<-dbConnect(MySQL(),user="root",
              #password="",dbname="bucky")
#dat<-dbGetQuery(uc,"SELECT * FROM `table 2`")
#View(dat)
#setwd("Users/anushka/Desktop/tydy intern/codes")
#dat<-read.csv("bdd.csv",header = TRUE,stringsAsFactors = TRUE)

ui<-dashboardPage(skin="red",
                  dashboardHeader(title="HR Analytics"),
                  dashboardSidebar(title = "Attributes",
                                   sidebarMenu(
                                     menuItem("Raw Data", tabName = "raw", icon = icon("table")),
                                     menuItem("Dashboard", tabName = "dash", icon = icon("bar-chart")),
                                    

                                     menuItem("Apprentissage", tabName = "app", icon = icon("chart-pie"))#nawel onglet
                                     
                                     
                                     #menuItem("Education", tabName = "education", icon = icon("mortar-board")),
                                     #menuItem("Gender", tabName = "gender", icon = icon("intersex")),
                                     #menuItem("Job-Satisfaction", tabName = "satisfaction", icon = icon("smile-o")),
                                     #menuItem("year_company", tabName = "year", icon = icon("handshake-o"))
                                     
                                   )
                  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "raw",
              #fluidRow(
              #column(10, 
                       # Bouton de recherche du fichier à charger
              #fileInput(inputId = "file1", label = "Choisir un fichier CSV",
              #accept = c("text/plain", ".csv")
              #)),
              #),
              
              #fluidRow(
                #column(2, 
                       # Bouton de chargement 'en retard'
                       #actionButton(inputId = "go", label = "Load"))
              #),
      
      
              #-----------------------------------------------#
              #--------------DEBUT AJOUT BOUTTONS A&R---------#
              #-----------------------------------------------#
              wellPanel(
                fluidRow(
                  box(width = 4, column(12, 
                                        # Bouton de recherche du fichier à charger
                                        fileInput(inputId = "file1", label = "Choisir un fichier CSV",
                                                  accept = c("text/plain", ".csv")
                                        ))
                  ),
                  box(width = 4 , title="Gérer les valeurs manquantes", column(12,
                      # Bouton pour gérer les valeurs manquantes
                      actionButton("dropAll" ,"Drop All NA", icon("trash"),
                                   class = "btn btn-sm btn-success"),
                      
                      actionButton("replaceAll" ,"Replace All NA", icon("plus"),
                                   class = "btn btn-sm btn-primary"),
                      
                      actionButton("resetAll" ,"Reset", icon("sync"),
                                   class = "btn btn-sm btn-danger")
                    )
                  ),
                  box(width = 4,title="Normalisation", column(12,
                      # Bouton pour gérer les valeurs manquantes
                      actionButton("normalize" ,"Normaliser", icon("filter"),
                                   class = "btn btn-sm btn-warning"),
                      
                      actionButton("cancel" ,"Cancel", icon("sync"),
                                   class = "btn btn-sm btn-default")
                    )
                  )
                  ),
                fluidRow(
                  box(width = 4,title="Dummification",
                      #Liste des colonnes du dataset
                      selectizeInput(inputId = "columns_select",
                                     label = "Choose column",
                                     choices = NULL, 
                                     multiple = TRUE,
                                     selected = NULL),
                      
                      # Bouton pour la dummification
                      actionButton("dummify" ,"Dummify", icon("sync"),
                                   class = "btn btn-sm btn-success"),
                      # Bouton pour gérer les valeurs manquantes
                      actionButton("remove_all" ,"Remove All", icon("trash"),
                                   style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"),
                      
                      # Bouton pour gérer les valeurs manquantes
                      #actionButton("add_all" ,"Add All", icon("minus"),
                      #style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"),
                      
                  ),
                  box(width = 8,title="Déséquilibre des classes", column(12,
                                                                         
                       sliderInput("balance_level", "Régler le problème de déséquilibre à combien de %:",
                                   min = 0, max = 100,
                                   value = 0, step = 5,
                                   animate = animationOptions(interval = 300, loop = TRUE)),
                       
                       # Bouton pour gérer les valeurs manquantes
                       actionButton("balance_add" ,"Ajouter Classe Minoritaire", icon("plus"),
                                    class = "btn btn-sm btn-success"),
                       
                       # Bouton pour gérer les valeurs manquantes
                       actionButton("balance_delete" ,"Diminuer Classe Majoritaire", icon("minus"),
                                    class = "btn btn-sm btn-danger"),
                       
                       
                       #actionButton("pop" ,"Reset", icon("sync"),
                       #class = "btn btn-sm btn-danger")
                    )
                  )
                )
              ),
              #-----------------------------------------------#
              #--------------FIN AJOUT BOUTTONS A&R-----------#
              #-----------------------------------------------#
              
              fluidRow(
                box(width = 12,dataTableOutput("table"),title="Data")
              )
        
      ),
      tabItem(tabName = "dash",
              fluidRow(
                h5("Cette section sera consacrée analyse exploratoire de données  basée sur le dataset HR Analytics: Job Change of Data Scientists",
                   tags$br(),tags$br(),"Rappelons le contexte : une entreprise cherche à recruter des data scientists parmi ceux qui ont passé leurs examens/entretiens. 
On cherche à savoir parmi ceux qui ont canditaté, quels sont les candidats qui souhaitent réellement intégrer l'entreprise ou ceux qui cherchent à rejoindre une autre entreprise. 
Cette prédiction permettrait de réduire les coûts de formation 
                   et de planifier la catégorisation des candidats.",
                ),
                box(width=6,plotOutput("plot1",height = 300),
                    footer = "Sur ce graphe on peut voir que plus de 90% des candidats de notre dataset sont de sexe masculin.",collapsible = T),
                box(width=6,plotOutput("plot2",height = 300),
                    footer="plus de 87% des candidats ont une experience pertinente.",collapsible = T)
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot3",height = 300),
                    footer="le but de ce graphe est d'esmiter le nombre de candidat ayant une experience pertinante et cela par leurs niveau d'education, on remarque qu'en globalite 
                    pour chaque niveau d'etudes avoir une experience pertiante est plus frequent.",collapsible = T)
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot4",height = 500),
                    footer="73% des candidats ne sont pas actuellement inscrits a l'universite. on peut supposer que la plupart des candidats ont deja obtenu leur diplome universitaire. Les 27 % restants sont partages par des cours 
                    a temps plein et des cours a temps partiel.",
                    collapsible = T)
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot5",height = 500),
                footer="Le graphe nous indique que plus de 60% des candidats ont un diplome de premier cycle,
                les candidats ayant un master represente plus de 22% et ceux du doctorat represente seulement 2%. ce qui indique que 
                85% ont poursuivi des etudes superieures.",collapsible = T)
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot6",height = 500),
                    footer="les etudes lies au domaine d'etudes STEM ( sciences, technologie, ingenierie et mathematiques) est le domaine le plus populaire chez les candidats masculins et feminins, en effet 75% des cadidats ont choisit ce parcours pour leurs etudes.  ",collapsible = T)
                
              ),
              fluidRow(
                
                box(width=6,plotOutput("plot17",height = 500),
                    footer="Contrairement a toute autre variable, la taille de l'entreprise etait equitablement repartie. Cependant, il y a encore une petite tendance ou plus de candidats avaient un emploi dans une petite entreprise. La taille de l'entreprise de 50 ~ 99 detiennent la majorite avec 16 %. La taille de l'entreprise de 100 ~ 500 vient ensuite avec 13,4 %. 10000+ etait le troisieme avec 10,5%.
                    ",collapsible = T),
                box(width=6,plotOutput("plot18",height = 500),
                    footer="Pres de 75% des candidats travaillent ou ont travaille dans une SARL (Pvt Ltd). Le graphique nous indique qu'il n'y a que peu de personnes qui ont travaille ou travaillent dans une start-up.",collapsible = T),
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot19",height = 500),
                    footer="Beaucoup de candidats ont deja plus de 20 ans d'experience. Cependant, un bon nombre de candidats  ont une experience qui varie entre 2 a 7 ans.",
                    ,
                    collapsible = T)
                
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot20",height = 500),
                    footer="Plus de 44% des candidats ont un an d'ecart entre leur emploi precedent et leur emploi actuel. presque 22% des candidats ont une difference de plus de
                    4 ans entre leur emploi precedent et leur emploi actuel."
                    ,collapsible = T)
                
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot21",height = 500),
                    footer="La plupart des candidats sont issus d'une ville qui un indice de developpement d'aux alentours de 0,9."
                    ,collapsible = T)
                
                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot22",height = 500),
                    footer = "ce graphe illustre la densite de candidats par le nombre d'heures de formation, Il semble que la plupart des candidats aient environ 0 a 60 heures de formation."
                    ,
                    collapsible = T)

                
              ),
              fluidRow(
                
                box(width=12,plotOutput("plot23",height = 500),
                    footer = "Dans cette fenetre on presente notre matrice de correlation, qui servira a faire notre analyse bidimensionnelle des variables. Notez qu'en ammont une dummification des variables qualitatives a bien ete effectuer,
                    neanmoins dans un soucis de nettete quelques variables ne sont pas presente sur cette matrice.",
                  
                    
                    collapsible = T)
                
                
              ),
              
              
              
              
      ),
      
      #-----------------------------------------------#
      #--------------DEBUT ONGLET NAWEL---------------#
      #-----------------------------------------------#
      tabItem(tabName = "app",
               fluidRow(
           
                 tabsetPanel(
                   
                   #-----------------début section Informations
                   tabPanel(
                     "Informations",
                     #debut h5
                     h5("Bienvenue dans la section apprentissage basée sur le dataset HR Analytics: Job Change of Data Scientists",
                        tags$br(),tags$br(),
                        "Cette base de données, nettoyée et analysée précédemment dans les sections cleaning et analytics respectivement est composée de 8955 lignes et 14 colonnes avec un split train/test de 80-20%.",
                        tags$br(),"Parmi les différentes colonnes, nous avons pu sélectionner les features les importantes qui sont affichées ci-dessous.",
                     ),
                     #plot feature selection, 
                     plotOutput("plot_feature"),
                     
                     h5("On observe ainsi que la feature ayant le plus d'impact est le 'City development index' qui mesure le développement des villes, indicateur qui permet de classer les villes du monde.",
                        tags$br(),"Pour nos différents modèles de classifications supervisés (Support Vector Machine/Régression Logistique/Tree), nous avons sélectionné les features ayant un impact de plus de 20% sur notre base de données.",
                        tags$br(),tags$br(),"Pour chacun de nos modèles, nous avons analysé leurs performances en calculant leur AUC, Accuracy, ROC en fonction d'un seuil, précision, recall, fscore et leur matrice de confusion.",
                        tags$br(),"",
                        
                     ),
                     
                   ),#fin tab pannel
                   #-----------------fin section Informations
                   #-----------------début section ROC
                   tabPanel(
                     "Évaluation ROC",
                     h5("Un ROC est le résultat du tracé du taux de vrais positifs par rapport au taux de faux positifs. Plus la courbe ROC est proche du coin supérieur gauche, plus la précision du test est grande.",
                        tags$br(),"On voit donc sur le graphique que le SVM présente une précision médiocre contrairement aux deux autres modèles (pas de pic en haut à gauche, le modèle est tiré en bas à droite). Cela se reflète d’ailleurs dans le score de son AUC (représentant l’aire sous la courbe du ROC) qui est très faible. Or plus l’aire sous la courbe est grande, moins le modèle fait d’erreur, ce qui n’est pas le cas ici.",
                        tags$br(),tags$br(),"Son Accuracy, représentant la proximité des résultats de mesure avec la valeur réelle, est dérisoire aussi.",
                        tags$br(),"Rappelons que l’Accuracy va de 0 (pas de prédiction correcte) à 1 (toutes les prédictions sont correctes). En comparant ces 2 métriques d’évaluation on voit que pour l’instant le SVM n’est pas performant.",
                        tags$br(),"Toujours sur l’observation de l’AUC et Accuracy, on observe que le modèle Tree a une plus grande aire que le SVM et semblable à celle de la Régression Logistique, en revanche face à ce dernier son Accuracy est médiocre.",
                        tags$br(),"Le modèle de Régression Logistique semble donc à première vue plus performant.",
                        tags$br(),tags$br(),"Sur le graphique, on observe que de légères variations montrent que si le taux de faux positifs est supérieur à 0,1 alors la Régression Logistique est légèrement meilleure par rapport au modèle Tree.",
                        tags$br(),tags$br(),"Un curseur (sliderInput) peut être manipulé pour ajuster le seuil représenté par la ligne rouge. Ce dernier nous permet de modifier les résultats des algorithmes en définissant le taux de faux positifs et voir ainsi leur impact sur les matrices de confusions.",
                        
                     ),
                     column(
                       width = 7,
                       class = "well",
                       h4("Courbe ROC"),
                       plotOutput("plot_roc"),
                       style = "background-color:white;",
                       sliderInput(
                         "seuil_roc",
                         label = "",
                         min = 0,
                         max = 1,
                         value = c(0.5)
                       )
                     ),
                     column(
                       width = 5,
                       class = "well",
                       tabsetPanel(
                         tabPanel(
                           "Tree",
                           h4("Matrice de Confusion (Tree)"),
                           plotOutput("matriceConfusion_roc_tree"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "SVM",
                           h4("Matrice de Confusion (SVM)"),
                           plotOutput("matriceConfusion_roc_svm"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "Regression Logistique",
                           h4("Matrice de Confusion (Regression Logistique)"),
                           plotOutput("matriceConfusion_roc_lr"),
                           style = "background-color:white;"
                         )
                       ),
                       style = "background-color:white;"
                     )
                   ),
                   #-----------------fin section ROC
                   
                   #-----------------début section Precision
                   tabPanel(
                     "Évaluation Precision",
                     h5("Une façon de visualiser la performance de nos modèles de Machine Learning est de calculer la précision (VP/(VP+FP)) qui permet de connaître le nombre de prédictions positives bien effectuées.",
                        tags$br(),"Plus elle est élevée plus le modèle minimise le nombre de faux positifs, c’est à dire que le modèle se trompe moins sur les positifs.",
                        tags$br(),tags$br(),"En contrôlant le seuil avec notre curseur, nous pouvons à nouveau comparer les valeurs de précision entre les différents modèles.",
                        tags$br(),"Lorsqu'on augmente le seuil de classification, la courbe de précision augmente à l'exception de celle du SVM.",
                        tags$br(),tags$br(),"Par conséquent, le SVM semble ne pas être adapté en raison de sa très faible courbe sur le graphique. Cela s’explique par sa matrice de confusion où le nombre de vrais positifs est dérisoire.",
                        
                        tags$br(),"Aussi en comparant les résultats entre les deux autres modèles, on observe que la Régression Logistique a une plus grande valeur que le modèle Tree, signifiant qu’il fait moins d’erreurs sur les positifs.",
                        
                     ),
                     column(
                       width = 7,
                       class = "well",
                       h4("Courbe Precision avec seuil"),
                       plotOutput("plot_precision"),
                       style = "background-color:white;",
                       sliderInput(
                         "seuil_precision",
                         label = "",
                         min = 0,
                         max = 1,
                         value = c(0.5)
                       )
                     ),
                     column(
                       width = 5,
                       class = "well",
                       tabsetPanel(
                         tabPanel(
                           "Tree",
                           h4("Matrice de Confusion (Tree)"),
                           plotOutput("matriceConfusion_precision_tree"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "SVM",
                           h4("Matrice de Confusion (SVM)"),
                           plotOutput("matriceConfusion_precision_svm"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "Regression Logistique",
                           h4("Matrice de Confusion (Regression Logistique)"),
                           plotOutput("matriceConfusion_precision_lr"),
                           style = "background-color:white;"
                         )
                       ),
                       style = "background-color:white;"
                     ),
                     #fin col
                   ),
                   #fin tabpanel
                   #-----------------fin section precision
                   
                   #-----------------début section Recall
                   tabPanel(
                     "Évaluation Recall",
                     h5(
                       tags$br(),"Afin d’évaluer correctement nos modèles, nous pouvons calculer le recall (==sensibilité, VP/(VP+FN)) qui permet de connaître le pourcentage de positifs bien prédit par notre modèle.",
                       tags$br(),"Plus il est élevé, plus notre algorithme maximise le nombre de Vrai Positif.",
                       tags$br(),"Cela signifie que si le recall de notre modèle est haut alors il ne ratera aucun positif (à noter que cela ne nous donne pas d’informations supplémentaires sur les négatifs).",
                       tags$br(),tags$br(),"En contrôlant le seuil avec notre curseur, nous pouvons à nouveau comparer les valeurs de recall entre les différents modèles.",
                       tags$br(),"Contrairement à la courbe de precision, lorsqu’on augmente le seuil de classification, le recall baisse.",
                       tags$br(),"En effet, plus on accroît le seuil, plus les positifs prédits sont certains (le nombre de FP diminue plus vite que le nombre de VP), mais il y a moins de prédiction positives et ainsi le recall décroît.",
                       tags$br(),tags$br(),"A un même seuil, cette fois-ci le rang de performance des modèles est le suivant : SVM > Tree > Régression Logistique.",
                       tags$br(),tags$br(),"Bien que les métriques Recall et precision soient utiles, analysées séparément, elles ne nous permettent pas d’évaluer entièrement les modèles de machine learning.",
                       tags$br(),"En effet, on a vu que le SVM était à la fois inefficace selon la courbe de Precision et le meilleur modèle selon la courbe de Recall (sans prendre en compte l’AUC/Accuracy et la matrice de confusion).",
                       tags$br(),"Si notre algorithme prédit tout le temps positif alors le recall sera élevé, au contraire si le modèle ne prédit pas jamais 'positif' alors la précision sera élevé.",
                       tags$br(),tags$br(),"Nous allons donc utiliser une métrique permettant de combiner les deux : le Fscore.",
                       
                     ),
                     column(
                       width = 7,
                       class = "well",
                       h4("Courbe Recall avec seuil"),
                       plotOutput("plot_recall"),
                       style = "background-color:white;",
                       sliderInput(
                         "seuil_recall",
                         label = "",
                         min = 0,
                         max = 1,
                         value = c(0.5)
                       )
                     ),
                     column(
                       width = 5,
                       class = "well",
                       tabsetPanel(
                         tabPanel(
                           "Tree",
                           h4("Matrice de Confusion (Tree)"),
                           plotOutput("matriceConfusion_fscore_tree"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "SVM",
                           h4("Matrice de Confusion (SVM)"),
                           plotOutput("matriceConfusion_fscore_svm"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "Regression Logistique",
                           h4("Matrice de Confusion (Regression Logistique)"),
                           plotOutput("matriceConfusion_fscore_lr"),
                           style = "background-color:white;"
                         )
                       ),
                       style = "background-color:white;"
                     ),
                     #fin col
                   ),
                   #fin tab
                   #-----------------fin section recall
                   #-----------------début section Fscore
                   tabPanel(
                     "Évaluation Fscore",
                     h5(
                       tags$br(),"Le Fscore est une métrique d’évaluation permettant de combiner la precision et le recall. Appelé aussi moyenne harmonique, il permet d’effectuer une bonne évaluation de la performance de notre modèle et calcule ainsi la moyenne de taux (2*(recall*precision)/(recall+precision)).",
                       tags$br(),tags$br(),"Plus notre Fscore est élevé plus notre modèle est performant.",
                       tags$br(),"Ici le résultat est sans équivoque : le SVM n’est pas adapté à notre dataset et ne dois pas être utilisé.",
                       tags$br(),tags$br(),"Pour notre Régression Logistique, on observe qu’avec un seuil de 0,4, nous atteignons un score de 60% ce qui équivaut environ à VP=1/2 *(FN+FP).",
                       tags$br(),"Cela signifie que pour une prédiction réussie par notre modèle, celui-ci fait 2 erreurs (FN ou FP).",
                       tags$br(),"Le modèle Tree lui ne fait que croître son score avec l’augmentation du seuil.",
                       tags$br(),tags$br(),"Que choisir entre Tree et RL ?",
                       tags$br(),"Face aux différentes métriques utilisées, la régression logistique nous a prouvé qu’elle était légèrement plus performante ou similaire (precision, recall et fscore) mais en revanche domine très largement en ce qui concerne le ROC/AUC et Accuracy associés.",
                       tags$br(),tags$br(),"Par conséquent, des 3 modèles ici utilisés, celui de Régression Logistique est le plus performant.",
                       
                     ),
                     column(
                       width = 7,
                       class = "well",
                       h4("Courbe Fscore avec seuil"),
                       plotOutput("plot_fscore"),
                       style = "background-color:white;",
                       sliderInput(
                         "seuil_fscore",
                         label = "",
                         min = 0,
                         max = 1,
                         value = c(0.5)
                       )
                     ),
                     column(
                       width = 5,
                       class = "well",
                       tabsetPanel(
                         tabPanel(
                           "Tree",
                           h4("Matrice de Confusion (Tree)"),
                           plotOutput("matriceConfusion_recall_tree"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "SVM",
                           h4("Matrice de Confusion (SVM)"),
                           plotOutput("matriceConfusion_recall_svm"),
                           style = "background-color:white;"
                         ),
                         tabPanel(
                           "Regression Logistique",
                           h4("Matrice de Confusion (Regression Logistique)"),
                           plotOutput("matriceConfusion_recall_lr"),
                           style = "background-color:white;"
                         )
                       ),
                       style = "background-color:white;"
                     ),
                     #fin col
                   ),
                   #fin tab
                   #-----------------fin section fscore
                 )
                 
                 
                 #-----------------------------------------------#
                 #----------------FIN ONGLET NAWEL---------------#
                 #-----------------------------------------------#
                 
                 )
              
              
              
              
              
      )
      #fin tabItem attrition
      
      
    )
    
  )
)


#------DEBUT DU SERVER-------


server<-function(input,output,session){  #Ajout session Armel
  
  
  #data<- eventReactive(input$go, {
    #inFile <- input$file1
    #if (is.null(inFile)) return(NULL)
    #read.csv(inFile$datapath, header = TRUE,stringsAsFactors = TRUE)
  #})
  
  #-----------------------------------------------#
  #--------------DEBUT CHARGEMENT DATA A&R--------#
  #-----------------------------------------------#
  
  #déclaration des variables
  dataset_replaced <-NULL             #dataset avec les valeurs NA remplacées
  initial_data <-NULL                 #dataset initialement chargé
  previous_dataset <-NULL
  current_dataset <-NULL
  categorial_columns_list <- c()      #Colonnes qualitatives         
  
  
  choosed_dataset_number <- 0 #if value == 0, then it's initial_data, if value == 1, it's dataset (whithout NA values), if value == 2 it's cleaned_dataset(replaced values)
  
  data<- eventReactive(input$file1, {
    
    #Read uploaded dataset
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    initial_data <<- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = TRUE, na.strings = c("","NA"))
    
    #Initialize select
    if (!is.null(initial_data)) {
      
      # dplyr get all categorial columns
      df<-initial_data %>% summarise_all(funs(n_distinct))
      data_size<-length(df)
      
      for (i in 1:data_size ) {
        if (df[1,i] < 10 && df[1,i] > 2 && df[1,i]/dim(initial_data)[1] < 0.01 ) { #10 parce qu'on suppose que, on ne veut pas dummifier une variable qui contient plus de 10 valeurs distinctes, on ne la considère pas comme une variable quantitative
          categorial_columns_list <- c(categorial_columns_list, names(df)[i])
        }
      }
      
      #initialize select with all categorial variables
      updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
                           selected=NULL, options = list(placeholder="Please Select at Least One Column")
      )
      
    }
    if (!is.null(initial_data)) {
      tabNa <<- which(is.na(initial_data),arr.ind=TRUE)
      previous_dataset <<- initial_data
      current_dataset <<- initial_data
    }
  })
  #-----------------------------------------------#
  #--------------FIN CHARGEMENT DATA A&R----------#
  #-----------------------------------------------#
  
  
  #debut badis ?
  bdd_2 <- read.csv("bdd.csv",na.strings=c("","NA"))
  d_2<-drop_na(bdd_2)

  
  
  
  
  
  
  
  output$plot1<-renderPlot({
    #dessiner le graphe des frequence pour chaque variable numerique
    #plot_num(data()[,-1])+
    #labs(title="xxxxxxxxxxxxxxxxxx")
    ggplot(d_2,aes(x=gender))+
      geom_bar(fill = "blue")+
      geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
      ggtitle("Distribution Sur le Genre")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Gender")

  }
  )

  output$plot2<-renderPlot({
    
    #dessiner le graphe des frequence pour chaque variable numerique
    ggplot(d_2,aes(x=relevent_experience))+
      geom_bar(fill = "blue")+
      geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Relevent Experience")
    
    
    
    
    #vis_miss(data()[,-1]) +
    #labs(title="valeurs manquantes")
  }
  )
  
  output$plot3<-renderPlot({
    ggplot(d_2,aes(x=relevent_experience))+
      geom_bar(fill = "blue")+
      facet_wrap(~education_level)+
      ggtitle("Relevent experience by education level")+
      theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
      theme(plot.title = element_text(hjust = 0.5))
    
    
  }
  )
  output$plot4<-renderPlot({
    ggplot(d_2,aes(x=enrolled_university,fill = relevent_experience))+
      geom_bar()+
      facet_wrap(~relevent_experience)+
      theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_text(stat='count', aes(label=..count..), vjust=-1)+
      ggtitle("Enrolled University")+
      xlab("Enrolled University")+
      ylab("Count")
    
  }
  )
  output$plot5<-renderPlot({
    ggplot(d_2,aes(x=education_level,fill=relevent_experience))+
      geom_bar()+
      theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
      geom_text(stat='count', aes(label=..count..), vjust=-1)+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Education Background")+
      xlab("Education Level")+
      ylab("Count")
  }
  )
  
  output$plot6<-renderPlot({
    ggplot(d_2,aes(x=major_discipline,fill=relevent_experience))+
      geom_bar()+
      facet_wrap(~gender)+
      theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
      ggtitle("College major")+
      xlab("Major")+
      ylab("Count")
  }
  
  )
  output$plot17<-renderPlot({
    ggplot(d_2,aes(x=company_size))+
      geom_bar(fill = "blue")+
      geom_text(stat='count', aes(label=..count..), vjust=-1)+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Company size")+
      xlab("Size")+
      ylab("Count")
    
  }
  
  )
  output$plot18<-renderPlot({
    ggplot(d_2,aes(x= company_type))+
      geom_bar(fill = "blue")+
      theme(axis.text.x = element_text(angle = 45, hjust =1, vjust = 1))+
      geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Company Type")+
      xlab("Type")+
      ylab("Count")
    
  }
  
  )
  output$plot19<-renderPlot({
    ggplot(d_2,aes(x=experience))+
      geom_bar(fill = "blue")+
      geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Experience")+
      xlab("Experience")+
      ylab("Count")
    
    
  }
  
  )
  output$plot20<-renderPlot({
    ggplot(d_2,aes(x=last_new_job))+
      geom_bar(fill = "blue")+
      geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Time gap")+
      xlab("Last job")+
      ylab("Count")
    
  }
  )
  
  output$plot21<-renderPlot({
    ggplot(d_2,aes(x=city_development_index,fill = relevent_experience))+
      geom_bar()+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("City development index")+
      xlab("City development")+
      ylab("Count")
  }
  )
  output$plot22<-renderPlot({
    ggplot(d_2,aes(x= training_hours,fill = relevent_experience))+
      ggtitle("Training hours")+
      geom_density()
  }
  )
  d_3 <- dummy_cols(d_2, select_columns = "gender")
  d_3 <- dummy_cols(d_3, select_columns = "relevent_experience")
  d_3 <- dummy_cols(d_3, select_columns = "enrolled_university")
  d_3 <- dummy_cols(d_3, select_columns = "education_level")
  #d_3 <- dummy_cols(d_3, select_columns = "major_discipline")
  d_3 <- dummy_cols(d_3, select_columns = "company_type")

  
  
  mcor <- cor(d_3[sapply(d_3,is.numeric)])

  output$plot23<-renderPlot({
    corrplot(mcor,method="color",type="upper",tl.col="black")
    
  }
  )
  
  
  output$plot24<-renderPlot({
    corrplot(mcor,method="number")
    
  }
  )
  
  
  

  
  #fin badis?
  
  
  
  output$table<-DT::renderDataTable({
    tmp.dat <- data()
    DT::datatable(tmp.dat, 
                  options = list(scrollX = TRUE),filter='top')
  }) 
  
  
  
  
  
  #-----------------------#
  #------NAWEL DEBUT------#
  #-----------------------#

  
  #MODIFIER LE PATH SI NECESSAIRE
  # on importe la base de données 
  #transformation des espaces vides en na et supression de ces derniers
  #bdd <- read.csv("~/rstudio-workplace/prog-web1/data1/aug_train.csv",na.strings=c("","NA"))  #Chargement dataset Nawel commenté par Armel pour le charger en évitant le chemin absolu, bloque le démarrage de l'application
  bdd <- read.csv("bdd.csv",na.strings=c("","NA"))
  d<-drop_na(bdd)
  #transformations des variables catégorielles en nombre
  d$enrolled_university  <- as.integer(as.factor(d$enrolled_university ))
  d$company_type <- as.integer(as.factor(d$company_type))
  d$city <- as.integer(as.factor(d$city))
  d$education_level <- as.integer(as.factor(d$education_level))
  d$last_new_job <- as.integer(as.factor(d$last_new_job))
  d$major_discipline <- as.integer(as.factor(d$major_discipline))
  d$training_hours <- as.integer(as.factor(d$training_hours))
  d$gender <- as.integer(as.factor(d$gender))
  d$experience <- as.integer(as.factor(d$experience))
  d$relevent_experience <- as.integer(as.factor(d$relevent_experience))
  d$company_size <- as.integer(as.factor(d$company_size))
  
  
  # On split notre base de données une partie training et test 80%-20% (donc 4/5)
  set.seed(0)
  tr.number<-sample(nrow(d),nrow(d)*4/5)  
  train<-d[tr.number,]
  test<-d[-tr.number,]
  #pour la validation
  train_Y = as.numeric(train$target)
  train$target<-NULL
  test_Y = test$target
  test$target<-NULL
  #On numérise nos 2 sets
  train[] <- lapply(train, as.numeric)
  test[] <- lapply(test, as.numeric)
  
  train$target<-train_Y
  
  # SVM MODELE
  svm_model<-svm(target~city_development_index+company_size+major_discipline+last_new_job+company_type+relevent_experience +city+enrolled_university+education_level,    
                 type="C-classification",   #on choisit notre type de classification
                 data=train,
                 cross=3,                  
                 probability = TRUE        #pour permettre la prédiction en probabilité
  )
  
  # début des predictions SVM
  svm_model.predict<-predict(svm_model, test, probability=TRUE) 
  # On stocke la probabilité 
  svm_model.prob <-attr(svm_model.predict,"probabilities")
  
  
  #REGRESSION LOGISTIQUE MODELE
  LR_model <- glm(target~city_development_index+company_size+major_discipline+last_new_job+company_type+relevent_experience +city+enrolled_university+education_level,family=binomial(link='logit'),data=train)
  
  # début des predictions Regression logistique
  LR_model.predict <- predict(LR_model, test, type = "response")
  
  #TREE MODELE
  tree_model <- tree(target~city_development_index+company_size+major_discipline+last_new_job+company_type+relevent_experience +city+enrolled_university+education_level,data = train)
  tree_model.predict <-predict(tree_model, test)
  
  #on stocke les ROC métriques grâce à nos prédictions 
  ROCRpred_svm <- prediction(svm_model.prob[,2], test_Y)
  ROCRpred_lr <- prediction(LR_model.predict, test_Y)
  ROCRpred_tree <- prediction(tree_model.predict, test_Y)
  
  #on plot nos ROC
  
  #on définit nos couleurs
  cols <- c("SVM" = "blue", "Regression Logistique" = "black", "Tree" = "green")
  
  #les variables qui sont dans ui.R, on injecte nos intercepts dedans 
  xintercept_roc <- reactive({
    input$seuil_roc
  })
  xintercept_precision <- reactive({
    input$seuil_precision
  })
  xintercept_recall <- reactive({
    input$seuil_recall
  })
  xintercept_fscore <- reactive({
    input$seuil_fscore
  })
  
  #Grace à la fonction de prédiction on va obtenir les auc, fpr (False Positive Rate) et tpr (True Positive Rate) 
  #on définit  la fonction get_seuil pour obtenir la probabilité de seuil pour un fpr fixe.
  #Toute probabilité prédite au dessus du seuil == target et réciproquement.
  get_seuil <- function(perf, threshold)
  {
    cutoffs <- data.frame(fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
    cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
    cutoffs <- subset(cutoffs, fpr <= threshold)
    if(nrow(cutoffs) == 0){ return(1.0)}
    else return(cutoffs[1, 1])
  }
  
  #-------------------------------------------------------
  #On définit une confusion matrix en fonction de la table de confusion calculée et de l'auc
  #Cette confusion matrix sera appelée pour chaque modele, à chaque section d'évaluation (-> à optimiser)  
  dessin_confusion_matrice <- function(cm, auc, color) {
    layout(matrix(c(1,1,2)))
    par(mar=c(0,0.1,1,0.1))
    plot(c(125, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    
    # création de la matrice
    rect(150, 430, 240, 370, col=color)
    text(195, 435, '0', cex=1.2)
    rect(250, 430, 340, 370, col='white')
    text(295, 435, '1', cex=1.2)
    text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
    text(245, 450, 'Actual', cex=1.3, font=2)
    rect(150, 305, 240, 365, col='white')
    rect(250, 305, 340, 365, col=color)
    text(140, 400, '0', cex=1.2, srt=90)
    text(140, 335, '1', cex=1.2, srt=90)
    
    #On ajoute les valeurs numériques dans la table
    res <- as.numeric(cm$table)
    text(195, 400, res[1], cex=1.6, font=2, col='white')
    text(195, 335, res[2], cex=1.6, font=2, col='black')
    text(295, 400, res[3], cex=1.6, font=2, col='black')
    text(295, 335, res[4], cex=1.6, font=2, col='white')
    
    # on ajoute les détails
    plot(c(0, 100), c(0, 50), type = "n", xlab="", ylab="", main = "", xaxt='n', yaxt='n')
    
    # on ajoute lees valeurs d'AUC et Accuracy en dessous
    text(25, 30, "AUC", cex=1.8, font=2)
    text(25, 20, round(as.numeric(auc), 3), cex=1.8)
    text(75, 30, names(cm$overall[1]), cex=1.8, font=2)
    text(75, 20, round(as.numeric(cm$overall[1]), 3), cex=1.8)
  }
  #-------------------------------------------------------
  
  #CREATION DU PLOT ROC
  #Le ROC du SVM
  roc_perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')                  
  roc_svm.data <- data.frame(fpr=unlist(roc_perf_svm@x.values),
                             tpr=unlist(roc_perf_svm@y.values), model="SVM")
  
  #Le ROC de la Regression Logistique
  roc_perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')
  roc_lr.data <- data.frame(fpr=unlist(roc_perf_lr@x.values),
                            tpr=unlist(roc_perf_lr@y.values), model="LR")
  #Le ROC du Tree
  roc_pref_tree <- performance(ROCRpred_tree, 'tpr','fpr')
  roc_tree.data <- data.frame(fpr=unlist(roc_pref_tree@x.values),
                              tpr=unlist(roc_pref_tree@y.values), model="Tree")
  
  #et on injecte nos différents résultats sur le plot de l'UI.R
  output$plot_roc<-renderPlot({ggplot() + 
      
      geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "SVM")) + #SVM courbe
      geom_line(data = roc_lr.data, aes(x = fpr, y=tpr, colour = "Regression Logistique")) + #Regression Logistique courbe
      geom_line(data = roc_tree.data, aes(x=fpr, y=tpr, colour = "Tree")) + #Tree courbe
      
      geom_vline(xintercept = xintercept_roc(), color = "red", linetype=2) + theme_bw() + 
      scale_colour_manual(name = "modèles", values = cols) + 
      xlab("False Positive Rate") +
      ylab("True Positive Rate") +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  
  
  
  # SVM confusion matrice
  output$matriceConfusion_roc_svm<-renderPlot({
    roc_auc_svm <- performance(ROCRpred_svm, measure = "auc")#on recupere l'AUC grâce à la fonction performance
    roc_perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')    #on récupèrele TPR et FPR               
    roc_cut <- get_seuil(roc_perf_svm, xintercept_roc())#on récupère le seuil de probabilité
    roc_pred_values_svm <- ifelse(svm_model.prob[,2] > roc_cut,1,0)#on classe selon le seuil
    roc_cm_svm <- confusionMatrix(data = factor(roc_pred_values_svm), reference = factor(test_Y))#on recupère la matrice de confusion
    dessin_confusion_matrice(roc_cm_svm, roc_auc_svm@y.values, "blue")#et on la dessine
  })
  
  # Rgression Logistique confusion matrice
  output$matriceConfusion_roc_lr<-renderPlot({
    roc_auc_lr <- performance(ROCRpred_lr, measure = "auc")#on recupere l'AUC grâce à la fonction performance
    roc_perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')#on récupèrele TPR et FPR 
    roc_cut <- get_seuil(roc_perf_lr, xintercept_roc())#on récupère le seuil de probabilité
    roc_pred_values_lr <- ifelse(LR_model.predict > roc_cut,1,0)#on classe selon le seuil
    roc_cm_lr <- confusionMatrix(data = factor(roc_pred_values_lr), reference = factor(test_Y))#on recupère la matrice de confusion
    dessin_confusion_matrice(roc_cm_lr, roc_auc_lr@y.values, "black")#et on la dessine
  })
  
  # Tree Matrice
  #on push la matrice dedans
  output$matriceConfusion_roc_tree<-renderPlot({
    roc_auc_tree <- performance(ROCRpred_tree, measure = "auc")  #on recupere l'AUC grâce à la fonction performance
    roc_pref_tree <- performance(ROCRpred_tree, 'tpr','fpr')  #on récupèrele TPR et FPR
    roc_cut <- get_seuil(roc_pref_tree, xintercept_roc()) #on récupère le seuil de probabilité
    roc_pred_values_tree <- ifelse(tree_model.predict > roc_cut,1,0) #on classe selon le seuil
    roc_cm_tree <- confusionMatrix(data = factor(roc_pred_values_tree), reference = factor(test_Y)) #on recupère la matrice de confusion
    dessin_confusion_matrice(roc_cm_tree, roc_auc_tree@y.values, "green")  #et on la dessine
  })
  
  
  
  
  
  #-------------------------------------------------------
  #CREATION DU PLOT Precision
  
  #Precision du SVM
  precision_perf_svm <- performance(ROCRpred_svm, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer                  
  precision_svm.data <- data.frame(x=unlist(precision_perf_svm@x.values), y=unlist(precision_perf_svm@y.values),
                                   model="SVM")
  
  #Precision de la Regression Logistique
  precision_perf_lr <- performance(ROCRpred_lr, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer                    
  precision_lr.data <- data.frame(x=unlist(precision_perf_lr@x.values), y=unlist(precision_perf_lr@y.values),
                                  model="LR")
  
  #Precision du Tree
  precision_perf_tree <- performance(ROCRpred_tree, "prec", "cutoff") #on utilise la métrique de précision et de seuil pour mesurer
  precision_tree.data <- data.frame(x=unlist(precision_perf_tree@x.values), y=unlist(precision_perf_tree@y.values),
                                    model="Tree")
  
  cols <- c("SVM" = "blue", "Regression Logistique" = "black", "Tree" = "green")
  
  
  #On plot le graphique de precision
  output$plot_precision<-renderPlot({ggplot() +
      geom_line(data = precision_svm.data, aes(x =x, y=y, colour = "SVM")) + 
      geom_line(data = precision_lr.data, aes(x =x, y=y, colour = "Regression Logistique")) + 
      geom_line(data = precision_tree.data, aes(x=x, y=y, colour = "Tree")) + 
      scale_colour_manual(name = "modèles", values = cols) + 
      xlab("Seuil") +
      ylab("Precision") +
      geom_vline(xintercept = xintercept_precision(), color = "red", linetype=2) + theme_bw() +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  
  #matrice des 3 modèles 
  
  # SVM confusion matrice
  output$matriceConfusion_precision_svm<-renderPlot({
    precision_auc_svm <- performance(ROCRpred_svm, measure = "auc")
    precision_perf_svm <- performance(ROCRpred_svm, "prec", "cutoff") #on utilise la métrique de précision et de seuil pour mesurer                 
    precision_cut <- get_seuil(precision_perf_svm, xintercept_precision())
    precision_pred_values_svm <- ifelse(svm_model.prob[,2] >  precision_cut,1,0)
    precision_cm_svm <- confusionMatrix(data = factor(precision_pred_values_svm), reference = factor(test_Y))
    dessin_confusion_matrice(precision_cm_svm, precision_auc_svm@y.values, "blue")
  })
  
  # Regression Logistique confusion matrice
  output$matriceConfusion_precision_lr<-renderPlot({
    precision_auc_lr <- performance(ROCRpred_lr, measure = "auc")
    precision_perf_lr <- performance(ROCRpred_lr, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer                    
    precision_cut <- get_seuil(precision_perf_lr, xintercept_precision())
    precision_pred_values_lr <- ifelse(LR_model.predict >  precision_cut,1,0)
    precision_cm_lr <- confusionMatrix(data = factor(precision_pred_values_lr), reference = factor(test_Y))
    dessin_confusion_matrice(precision_cm_lr, precision_auc_lr@y.values, "black")
  })
  
  # Tree Matrice
  output$matriceConfusion_precision_tree<-renderPlot({
    precision_auc_tree <- performance(ROCRpred_tree, measure = "auc")  #on recupere l'AUC grâce à la fonction performance
    precision_perf_tree <- performance(ROCRpred_tree, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer
    precision_cut <- get_seuil(precision_perf_tree, xintercept_precision()) #on récupère le seuil de probabilité
    precision_pred_values_tree <- ifelse(tree_model.predict >  precision_cut,1,0) #on classe selon le seuil
    precision_cm_tree <- confusionMatrix(data = factor(precision_pred_values_tree), reference = factor(test_Y)) #on recupère la matrice de confusion
    dessin_confusion_matrice(precision_cm_tree, precision_auc_tree@y.values, "green")  #et on la dessine
  })
  
  
  
  #-------------------------------------------------------
  #CREATION DU PLOT recall
  
  
  #SVM recall
  recall_perf_svm <- performance(ROCRpred_svm, "rec", "cutoff") #on utilise la métrique de recall et de seuil pour mesurer                 
  recall_svm.data <- data.frame(x=unlist(recall_perf_svm@x.values), y=unlist(recall_perf_svm@y.values),
                                model="SVM")
  
  #Regression Logistique recall
  recall_perf_lr <- performance(ROCRpred_lr, "rec", "cutoff")  #on utilise la métrique de recall et de seuil pour mesurer                  
  recall_lr.data <- data.frame(x=unlist(recall_perf_lr@x.values), y=unlist(recall_perf_lr@y.values),
                               model="LR")
  
  #Tree recall
  recall_perf_tree <- performance(ROCRpred_tree, "rec", "cutoff") #on utilise la métrique de recall et de seuil pour mesurer
  recall_tree.data <- data.frame(x=unlist(recall_perf_tree@x.values), y=unlist(recall_perf_tree@y.values),
                                 model="Tree")
  
  cols <- c("SVM" = "blue", "Regression Logistique" = "black", "Tree" = "green")
  
  #plot du Recall
  output$plot_recall<-renderPlot({ggplot() +
      geom_line(data = recall_svm.data, aes(x =x, y=y, colour = "SVM")) + 
      geom_line(data = recall_lr.data, aes(x =x, y=y, colour = "Regression Logistique")) + 
      geom_line(data = recall_tree.data, aes(x=x, y=y, colour = "Tree")) + 
      
      scale_colour_manual(name = "modèles", values = cols) + 
      xlab("Seuil") +
      ylab("recall") +
      geom_vline(xintercept = xintercept_recall(), color = "red", linetype=2) + theme_bw() +
      theme(legend.position = c(0.8, 0.8), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  
  
  # SVM confusion matrice
  output$matriceConfusion_recall_svm<-renderPlot({
    recall_auc_svm <- performance(ROCRpred_svm, measure = "auc")
    recall_perf_svm <- performance(ROCRpred_svm, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer                                      
    recall_cut <- get_seuil(recall_perf_svm, xintercept_recall())
    recall_pred_values_svm <- ifelse(svm_model.prob[,2] > recall_cut,1,0)
    recall_cm_svm <- confusionMatrix(data = factor(recall_pred_values_svm), reference = factor(test_Y))
    dessin_confusion_matrice(recall_cm_svm, recall_auc_svm@y.values, "blue")
  })
  
  # Regression Logistique confusion matrice
  output$matriceConfusion_recall_lr<-renderPlot({
    recall_auc_lr <- performance(ROCRpred_lr, measure = "auc")
    recall_perf_lr <- performance(ROCRpred_lr, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer                                        
    recall_cut <- get_seuil(recall_perf_lr, xintercept_recall())
    recall_pred_values_lr <- ifelse(LR_model.predict > recall_cut,1,0)
    recall_cm_lr <- confusionMatrix(data = factor(recall_pred_values_lr), reference = factor(test_Y))
    dessin_confusion_matrice(recall_cm_lr, recall_auc_lr@y.values, "black")
  })
  
  # Tree Matrice
  output$matriceConfusion_recall_tree<-renderPlot({
    recall_auc_tree <- performance(ROCRpred_tree, measure = "auc")  #on recupere l'AUC grâce à la fonction performance
    recall_perf_tree <- performance(ROCRpred_tree, "prec", "cutoff")#on utilise la métrique de précision et de seuil pour mesurer                    
    recall_cut <- get_seuil(recall_perf_tree, xintercept_recall()) #on récupère le seuil de probabilité du recall
    recall_pred_values_tree <- ifelse(tree_model.predict > recall_cut,1,0) #on classe selon le seuil
    recall_cm_tree <- confusionMatrix(data = factor(recall_pred_values_tree), reference = factor(test_Y)) #on recupère la matrice de confusion
    dessin_confusion_matrice(recall_cm_tree, recall_auc_tree@y.values, "green")  #et on la dessine
  })
  
  #-------------------------------------------------------
  #CREATION DU PLOT Fscore
  
  #SVM Fscore
  fscore_perf_svm <- performance(ROCRpred_svm, "f", "cutoff")  #on utilise la métrique de fscore et de seuil pour mesurer                                                        
  fscore_svm.data <- data.frame(x=unlist(fscore_perf_svm@x.values), y=unlist(fscore_perf_svm@y.values),
                                model="SVM")
  
  #Regression Logistique Fscore
  fscore_perf_lr <- performance(ROCRpred_lr, "f", "cutoff")#on utilise la métrique de fscore et de seuil pour mesurer                                                            
  fscore_lr.data <- data.frame(x=unlist(fscore_perf_lr@x.values), y=unlist(fscore_perf_lr@y.values),
                               model="LR")
  #Tree Fscore
  fscore_perf_tree <- performance(ROCRpred_tree, "f", "cutoff") #on utilise la métrique de fscore et de seuil pour mesurer                                        
  fscore_tree.data <- data.frame(x=unlist(fscore_perf_tree@x.values), y=unlist(fscore_perf_tree@y.values),
                                 model="Tree")
  
  cols <- c("SVM" = "blue", "Regression Logistique" = "black", "Tree" = "green")
  
  output$plot_fscore<-renderPlot({ggplot() +
      geom_line(data = fscore_svm.data, aes(x =x, y=y, colour = "SVM")) + 
      geom_line(data = fscore_lr.data, aes(x =x, y=y, colour = "Regression Logistique")) + 
      geom_line(data = fscore_tree.data, aes(x=x, y=y, colour = "Tree")) + 
      scale_colour_manual(name = "modèles", values = cols) + 
      xlab("Seuil") +
      ylab("fscore") +
      geom_vline(xintercept = xintercept_fscore(), color = "red", linetype=2) + theme_bw() +
      theme(legend.position = c(0.8, 0.8), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  # Tree Matrice
  output$matriceConfusion_fscore_tree<-renderPlot({
    fscore_auc_tree <- performance(ROCRpred_tree, measure = "auc")  #on recupere l'AUC grâce à la fonction performance
    fscore_perf_tree <- performance(ROCRpred_tree, "f", "cutoff") #on utilise la métrique de fscore et de seuil pour mesurer                                                            
    fscore_cut <- get_seuil(fscore_perf_tree, xintercept_fscore()) #on récupère le seuil de probabilité du fscore
    fscore_pred_values_tree <- ifelse(tree_model.predict > fscore_cut,1,0)  #on classe selon le seuil
    fscore_cm_tree <- confusionMatrix(data = factor(fscore_pred_values_tree), reference = factor(test_Y)) #on recupère la matrice de confusion
    
    dessin_confusion_matrice(fscore_cm_tree, fscore_auc_tree@y.values, "green")  #et on la dessine
  })
  
  # SVM confusion matrice
  output$matriceConfusion_fscore_svm<-renderPlot({
    fscore_auc_svm <- performance(ROCRpred_svm, measure = "auc") #on recupere l'AUC grâce à la fonction performance
    fscore_perf_svm <- performance(ROCRpred_svm, "f", "cutoff")   #on utilise la métrique de fscore et de seuil pour mesurer                                                                           
    fscore_cut <- get_seuil(fscore_perf_svm, xintercept_fscore())#on récupère le seuil de probabilité du fscore
    fscore_pred_values_svm <- ifelse(svm_model.prob[,2] > fscore_cut,1,0) #on classe selon le seuil
    fscore_cm_svm <- confusionMatrix(data = factor(fscore_pred_values_svm), reference = factor(test_Y))#on recupère la matrice de confusion
    
    dessin_confusion_matrice(fscore_cm_svm, fscore_auc_svm@y.values, "blue")#et on la dessine
  })
  
  # Regression Logistique confusion matrice
  output$matriceConfusion_fscore_lr<-renderPlot({
    fscore_auc_lr <- performance(ROCRpred_lr, measure = "auc")#on recupere l'AUC grâce à la fonction performance
    fscore_perf_lr <- performance(ROCRpred_lr, "f", "cutoff")   #on utilise la métrique de fscore et de seuil pour mesurer                                                                             
    fscore_cut <- get_seuil(fscore_perf_lr, xintercept_fscore())#on récupère le seuil de probabilité du fscore
    fscore_pred_values_lr <- ifelse(LR_model.predict > fscore_cut,1,0) #on classe selon le seuil
    fscore_cm_lr <- confusionMatrix(data = factor(fscore_pred_values_lr), reference = factor(test_Y))#on recupère la matrice de confusion
    
    dessin_confusion_matrice(fscore_cm_lr, fscore_auc_lr@y.values, "black")#et on la dessine
  })
  
  
  #_______________________________________________________AFFICHAGE FEATURES
  ####fonction qui recupere l'ensemble du dataset et determine les features les + importantes
  #A decommenter pour la version finale, ici c'est commenté pour afficher plus rapidement lors des tests
  
  res_boruta<-Boruta(target~.,data=d,doTrace=0)
  
  
  output$plot_feature<-renderPlot({
  plot(res_boruta,cex.axis=.7,las=2,xlab="",main="Les Features les plus importantes")}
   )
  
  #----------------------------Fin affichage Features
  
  
  #-----------------------#
  #------NAWEL FIN--------#
  #-----------------------#
  
  
  
  
  
  
  
  #-----------------------------------------------#
  #--------------DEBUT TRAITEMENTS DATA A&R-------#
  #-----------------------------------------------#
  #Drop all NA values
  observeEvent(input$dropAll, {
    if (!is.null(initial_data)) {
      #get all not empty values
      previous_dataset <<- current_dataset
      current_dataset <<- na.omit(initial_data)
      choosed_dataset_number <<- 2
      #print(dim(current_dataset))
      
      #sync data
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
    }else{
      shinyalert("Oops!", "Veuillez charger un dataset", type = "error")
    }
  })
  
  #Replace all NA values
  observeEvent(input$replaceAll, {
    if (!is.null(initial_data)) {
      
      #get all cells where empty values are
      tabNa<-which(is.na(initial_data),arr.ind=TRUE)
      
      previous_dataset <<- current_dataset
      
      #replace all other lines
      for (i in 1:dim(current_dataset)[2]){
        if ( is.na(current_dataset[1,i]))
        {
          j<-2
          while(is.na(current_dataset[j,i])){
            j<-j+1
          }
          current_dataset[1,i]<<-current_dataset[j,i]
        }
      }
      
      #replace all other lines
      for (i in 1:(length(tabNa)/2)){
        ligne<-tabNa[i,1]
        colonne<-tabNa[i,2]
        if(ligne > 1){
          current_dataset[ligne,colonne]<<-current_dataset[ligne-1,colonne]
        }
      }
      
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      print(dim(current_dataset))
      choosed_dataset_number <<- 1
      
    }else{
      shinyalert("Oops!", "Veuillez d'abord charger un dataset", type = "error")
    }
    
  })
  
  #reset dataset
  observeEvent(input$resetAll, {
    if (!is.null(initial_data)) {
      #reset to initial dataset
      data<- eventReactive(input$file1, {
        initial_data
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      choosed_dataset_number <<- 0
    }
  })
  
  observeEvent(input$remove_all, {
    updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
                         selected=NULL, options = list(placeholder="Please Select at Least One Column")
    )
  })
  
  #observeEvent(input$add_all, {
  #updateSelectizeInput(session,"columns_select",choices=sort(unique(current_dataset)), selected=sort(unique(current_dataset)) )
  #})
  
  #Dummify
  observeEvent(input$dummify, {
    vector = c()
    if (choosed_dataset_number != 0) {
      if (!is.null(current_dataset)) {
        previous_dataset <<- current_dataset
        selected_values <- input$columns_select
        if (!is.null(selected_values)) {
          groups <- c(selected_values)
          selected_values_list <- as.list(paste(strsplit(selected_values, split = "[ ]+"))) #remove all spaces from string returned by the select
          
          for (v in selected_values_list)
            vector <- c(vector, v)
          
          #Delete selected column into choosed dataset before apply dummification
          #croped_data_set <- current_dataset[ , !(names(current_dataset) %in% selected_values_list)]
          
          #Dummification
          data_dummy <<- dummy_cols(current_dataset, remove_selected_columns = TRUE, select_columns = vector)
          
          #allow us to know on which dataset operating
          choosed_dataset_number <<- -1
          
          #syncing data
          data<- eventReactive(input$file1, {
            data_dummy
          })
          
          #displaying on the screen
          output$table<-DT::renderDataTable({
            tmp.dat <- data()
            DT::datatable(tmp.dat, 
                          options = list(scrollX = TRUE),filter='top')
          })
        }else{
          shinyalert("Oops!", "Veuillez choisir au moins une colonne", type = "warning")
        }
      }
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données.", type = "error")
    }
  })
  
  #Déséquilibre
  balance_level = NULL
  ajusted_data <- NULL
  occ0 = 0
  occ1 = 0
  difference = 0
  balance_value = 0
  
  makeReactiveBinding("balance_level")
  
  observeEvent(input$balance_level, {
    balance_level <<- input$balance_level
    balance_level <<- as.integer(balance_level)
    #print(balance_level)
  })
  
  
  #Balance data
  observeEvent(input$balance_add, {
    
    if (!is.null(current_dataset)) {
      
      previous_dataset <<- current_dataset
      
      #Count occurence
      occ0<<-sum(current_dataset$target == 0)
      occ1<<-sum(current_dataset$target == 1)
      
      
      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(current_dataset, target == 0)
      
      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(current_dataset, target == 1)
      
      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      }else{
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)
      
      print("------ occurence de 0 ------")
      print(occ0)
      
      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)
      
      #apply value on slider input
      balance_value = as.integer((balance_level * difference) / 100)  #this is the number of rows we are going to add into datased to make it balanced
      
      print("----- nombre de lignes à ajouter -----")
      print(balance_value)
      
      #if we have more 0 than 1
      if (occ1 < occ0) {
        #ajusted_data <- dataset_one_values_on_target[rep(seq_len(balance_value), each = 1), ]
        ajusted_data <<- dataset_one_values_on_target[sample(nrow(dataset_one_values_on_target), balance_value, replace = TRUE, prob = NULL), ]
      }else{ #if not
        #ajusted_data <<- sample_n(dataset_zero_values_on_target, balance_value)
        ajusted_data <<- dataset_zero_values_on_target[sample(nrow(dataset_zero_values_on_target), balance_value), ]
      }
      balanced_dataset <- rbind(current_dataset, ajusted_data)
      print(dim(balanced_dataset))
      
      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset$target == 0))
      print(sum(balanced_dataset$target == 1))
      
      current_dataset <<- balanced_dataset
      
      #Refreshing view
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      
      
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données.", type = "error")
    }
  })
  
  
  #Déséquilibre
  observeEvent(input$balance_delete, {
    if (!is.null(current_dataset)) {
      
      previous_dataset <<- current_dataset
      
      #Count occurence
      occ0<<-sum(current_dataset$target == 0)
      occ1<<-sum(current_dataset$target == 1)
      
      
      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(current_dataset, target == 0)
      
      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(current_dataset, target == 1)
      
      print("dim")
      print(dim(dataset_one_values_on_target)[1])
      
      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      }else{
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)
      
      print("------ occurence de 0 ------")
      print(occ0)
      
      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)
      
      #apply value on slider input
      balance_value = as.integer((balance_level * difference) / 100)  #this is the number of rows we are going to add into datased to make it balanced
      
      print("----- nombre de lignes à supprimer -----")
      print(balance_value)
      
      #if we have more 0 than 1
      if (occ1 < occ0) {
        ajusted_data <<- tail(dataset_zero_values_on_target, n=dim(dataset_zero_values_on_target)[1]-balance_value)
        balanced_dataset <- rbind(dataset_one_values_on_target, ajusted_data)
      }else{ #if not
        ajusted_data <<- tail(dataset_one_values_on_target, n=dim(dataset_one_values_on_target)[1]-balance_value)
        balanced_dataset <- rbind(dataset_zero_values_on_target, ajusted_data)
      }
      print(dim(balanced_dataset))
      current_dataset <<- balanced_dataset
      
      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset$target == 0))
      print(sum(balanced_dataset$target == 1))
      
      #Refreshing view
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
      
      
    }else{
      shinyalert("Oops!", "Veuillez appliquer un traitement sur les données.", type = "error")
    }
  })
  
  #Normalisation
  observeEvent(input$normalize, {
    if (choosed_dataset_number != 0) {
      if (!is.null(current_dataset)) {
        previous_dataset <<- current_dataset
        df<-sapply(current_dataset, class)
        df2<-current_dataset%>%summarise_all(funs(n_distinct))
        for (i in 1:dim(current_dataset)[2] ) {
          if ((df[i] == "integer" || df[i] == "double" || df[i] == "numeric") &&  df2[1,i] > 2 && df2[1,i]!=dim(current_dataset)[1]) {
            for (j in 1:dim(current_dataset)[1]){
              current_dataset[j,i]<<-current_dataset[j,i]/quantile(current_dataset[,i], 0.975)
            }
          }
        }
        print(df2[1,1]!=dim(current_dataset)[1])
        #syncing data
        data<- eventReactive(input$file1, {
          current_dataset
        })
        
        #displaying on the screen
        output$table<-DT::renderDataTable({
          tmp.dat <- data()
          DT::datatable(tmp.dat, 
                        options = list(scrollX = TRUE),filter='top')
        })
      }else{
        shinyalert("Oops!", "Veuillez appliquer un traitement sur les données manquantes.", type = "error")
      }
    }else{
      shinyalert("Oops!", "Veuillez charger un jeu de données ou gérer les valeurs manquantes", type = "error")
    }
  })
  
  observeEvent(input$cancel, {
    if (!is.null(current_dataset)) {
      current_dataset <<- previous_dataset
      #syncing data
      data<- eventReactive(input$file1, {
        current_dataset
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
                      options = list(scrollX = TRUE),filter='top')
      })
    }else{
      shinyalert("Oops!", "Veuillez gérer les valeurs manquantes", type = "error")
    }
  })
  #-----------------------------------------------#
  #--------------FIN TRAITEMENTS DATA A&R---------#
  #-----------------------------------------------#
  
  
  
}
#------FIN DU SERVER-------

shinyApp(ui,server)
