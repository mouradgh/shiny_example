#Charger la bibliotheque Shiny
library(shiny)

#Debut de l'interface graphique
shinyUI(
  #Barre de navigation
  #Nom de l'application
  navbarPage("PMV",
              #Page d'identification (Login/Mot de pass)
              tabPanel("Login",
                       
                       tags$h3('Entrez vos identifiants'),
                       #Ligne horizontale
                       tags$hr(), 
                       #Champ du Login
                       htmlOutput("uiLogin"),
                       #Champ du mot de pass
                       htmlOutput("pass")
              ),
              #Page de chargement du fichier données
              tabPanel("Charger le fichier et choisir la variable à ",
                       titlePanel("Charger le fichier"),
                       sidebarLayout(
                         sidebarPanel(
                           #Button pour charger un fichier à partir de l'ordinateur
                           fileInput('file1', 'Choisir le fichier de données',
                                     #Types de fichiers acceptés
                                     accept=c('text/csv', 
                                              'text/comma-separated-values,text/plain', 
                                              '.csv')),
                           tags$hr(),
                           #Case à cocher si le fichier contient un en-tête
                           checkboxInput('header', 'En-tête', TRUE),
                           #Button pour choisir le séparateur (Virgule, Point virgule ou tabulation)
                           radioButtons('sep', 'Séparateur',
                                        #Liste des choix
                                        c(Virgule=',',
                                          PointVirgule=';',
                                          Tabulation='\t'),
                                        #Selectionner point-virgule par défaut 
                                        ';' ),
                           
                  
#                            radioButtons('quote', 'Quote',
#                                         c(None='',
#                                           'Double Quote'='"',
#                                           'Single Quote'="'"),
#                                         '"'),

                           #Button pour choisir le séparateur décimal (point ou virgule)
                           radioButtons('dec', 'Séparateur décimal',
                                        c('Virgule'=",",
                                          'Point'="."),
                                        #Selectionner virgule par défaut
                                        'Virgule'),
                           tags$hr(),
                           #Choix de la variable à éxpliquer
                           htmlOutput("selectDependent"),
                           #Slider pour le choix du degre de regression polynomiale
                           sliderInput('deg', 'Degré maximum pour la regression polynomiale', 2, 4, 2, 1)
                         ),
                         mainPanel(
                           tags$h4('Si vous avez des erreur, assurez vous du choix des options de chargement du fichiers: en-tête, séparateur, décimal ...'),
                           
                           #Affichage d'un résumé des données
                           #verbatimTextOutput("summary"),
                           
                           #Affichage des premieres ligne du fichier de donnees
                           tableOutput("view")
                         )
                       )
              ),
              #Page d'affichage des différentes régressions
              tabPanel("Choix du meilleur modèle",
                       #Adapter la page à la taille de l'écran
                       fluidRow(
                         column(3,
                              #Button pour stocker le modèle choisi dans la base
                              actionButton("action", "Envoyer à la base de données")
                         ),
                         #Choix du meilleur modèle de regression
                         column(3, offset = 1,
                                radioButtons('mtype', h3('Model type'),
                                             c(Linear = 'linear',
                                               Exponential ='exponential',
                                               Logarithmic ='logarithmic',
                                               Polynomial ='polinomial'),
                                             ',')
                         ),
                         #Choix des variables explicatives
                         column(4,
                                htmlOutput("checkvariables"),
                                verbatimTextOutput("value")
                         )),
                         tags$hr(),
                         tags$h3('Regressions'),
                         
                         #Affichage du tableau contenant tous les modèles
                         dataTableOutput("bigtable")
                       
              ),
             
             #Page d'affichage du graphe (de regression)nuage de points)
             tabPanel("Nuage de points",
                      plotOutput('plot'),
                      hr(),
                      fluidRow(
                        column(4, offset = 1,
                               #Choisir la variable 1
                               htmlOutput("selectUIx")
                              ),
                        column(5, offset = 1,
                               #Choisir la variable 2
                               htmlOutput("selectUIy")
                              ) 
                        )
                      )
             )
)
  

