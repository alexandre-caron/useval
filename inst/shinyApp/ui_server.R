# telechargement Library

#etapes a réaliser
#1-transformer jeu de données en matrice
#2- compter nbr lignes colonnes de notre jeu de données et l'afficher
#3- Calculer la fréquence pour chaque pb
#4-une fois nombre m_max demander on lance a l'aide d'un bouton notre fonction posterior analysis
#5- appliquer a dm notre fct model_parameters
#6- realiser le graphique
#7- sachant qu'on garde j=nbr de pbs detectés, n= nbr lignes


install.packages("anyLib")
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly",
                 "ggplot2", "googleVis", "colourpicker"))


# COTE UI

ui <- dashboardPage(
  dashboardHeader(title = "stage adam"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Lecture des donnees", tabName = "readData", icon = icon("readme")),
                menuItem("Visualisation des donnees", tabName = "visualization", icon = icon("poll"))
    )
  ),
  dashboardBody(

    tabItems(
      # lecture des donnees
      tabItem(tabName = "readData",
              h1("Data reading"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),

              fluidRow(
                column(2,
                       h3("Parameters for Analysis"),

                       #nbr de pbrlm max
                       sliderInput(inputId = "m_max", label = "problèmes max",
                                   value = 1, min = 1, max = 70)
                ),
                       column(3,
                              h3("Parameters for Data reading"),

                              # choix de garder headers ou non, important pr graph vu que les headers servent pr les graph
                              radioButtons(inputId = "header",
                                           label = "Header",
                                           choices = c("Yes" = TRUE,
                                                       "No" = FALSE),
                                           selected = TRUE, inline=T),

                              # choix des types de separateur dans le fichier
                              radioButtons(inputId = "sep",
                                           label = "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = "\t", inline=T),

                              # choix guillemets si il y en a
                              radioButtons(inputId = "quote",
                                           label= "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = "", inline=T)
                       ),
                column(7,
                       h3("File preview"),
                       dataTableOutput(outputId = "preview")
                )
              ),
              tags$br(),

              div(actionButton(inputId = "actBtnVisualisation", label = "lancer l'analyse",icon = icon("play") ))



      ),

      #
      # visualization

      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              h2("Exploration du tableau"),
              dataTableOutput('dataTable'),
              h2("Graphiques"),
              fluidRow(
                column(4,plotOutput("plotprobleme")),
                column(4, colourpicker::colourInput("colR", "Couleur graphique R", "black",allowTransparent = T),
                       sliderInput("cex", "Taille",
                                   min = 0.5, max = 3,
                                   value = 1,step = 0.2
                       )),
                column(4, selectInput(inputId = "pch", choices = 1:20, label = "Type de points",selected = 1),
                       textInput("title", "Titre", "probabilité d'avoir tant de problèmes") )
              ),
              tags$br(),
              )
    )
  )
)



#partie server

server <- function(input, output, session) {

  data <- reactiveValues()


# Preview et verif

  output$preview <-  renderDataTable({

    req(input$dataFile)

    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=10
    )
  },  options = list(scrollX = TRUE , dom = 't'))

##df2 <- as.matrix(df)
##nrow(df2) et ncol(df2)

  # Lecture_donnees

  observeEvent(input$actBtnVisualisation, {

    if(!is.null(input$dataFile$datapath)){
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            probleme = input$m_max,
                            quote = input$quote)
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Le fichier a bien été lu !",
        type = "success"
      )

      updateTabItems(session, "tabs", selected = "visualization")
    }

  })


  # Exploration du tableau pour graph

  output$dataTable = DT::renderDataTable(data$table)


  # Graphiques fixe

    output$plotprobleme <- renderPlot({
    ggplot(data=data$table, aes(x = m_max, y = m_max)) +
      xlab("Number of non-critical problems") +  ylab("Number of critical problems") +
      ggtitle("Proba problem (ggplot2")
  })
}

  shinyApp(ui, server)
