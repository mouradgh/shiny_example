#Charger les librairies nécessaures

#Shiny pour l'application web
library(shiny)

#plyr nécessaires pour quelques calculs
library(plyr)

#ggplot2 pour dessiner le nuage de points
library(ggplot2)

#Variable booléenne
#Vrai si l'utilisateur est identifié, Faux sinon
#Tant que la valeur de la variable est False, aucune action ne peut être effectuée
Logged <- FALSE

#Login et Mot de pass
#Changer 'writer' and '123456' par le mot de pass/login depuis la base
PASSWORD <- data.frame(Brukernavn = "writer", Password = "123456")   
options(DT.options = list(autoWidth = TRUE))

#Call.linear est une fonction qui effectue le calcul 
#de tous les modèles linéaires
#Elle prend en paramètres les données, la variable à expliquer (dependant)
#ainsi que les variables explicatives (regressors)
all.linear <- function(data, dependent, regressors){
  
  combinations <- function(n){
    expand.grid(rep(list(0:1),n))
  }
  regMat <- combinations(length(regressors))
  regMat <- sapply(regMat, as.logical)
  #Changer les noms de colonnes de regMat
  colnames(regMat) <- regressors
  
  all.linear <- apply(regMat, 1, function(x) as.formula(paste(c(paste0(dependent," ~ 1"), regressors[x]), collapse=" + ")))
  all.linear.result <- lapply(all.linear, function(x) lm(x, data=data))
  
  #Les coefficients de la regression
  dfCoefNum   <- ldply(all.linear.result, function(x) as.data.frame(t(coef(x))))
  #L'erreur standard
  dfStdErrors <- ldply(all.linear.result, function(x) as.data.frame(t(coef(summary(x))[, "Std. Error"])))
  #Statistique T
  dftValues   <- ldply(all.linear.result, function(x) as.data.frame(t(coef(summary(x))[, "t value"])))
  #p value
  dfpValues   <- ldply(all.linear.result, function(x) as.data.frame(t(coef(summary(x))[, "Pr(>|t|)"]))) 
  
  names(dfStdErrors) <- paste("se", names(dfStdErrors), sep=".")
  names(dftValues) <- paste("t", names(dftValues), sep=".")
  names(dfpValues) <- paste("p", names(dfpValues), sep=".")
  
  #R carré
  R2       <- unlist(lapply(all.linear.result, function(x)  summary(x)$r.squared))
  #R carré ajusté
  adjR2    <- unlist(lapply(all.linear.result, function(x)  summary(x)$adj.r.squared))
  #Type de regression : linéaire
  type <- rep(x = 'lin', length(R2))
  #Stocker les résultats dans un tableau
  results <- data.frame( Model_Formula = as.character(all.linear),
                         R2 = R2,
                         adjR2 = adjR2, 
                         Coef = dfCoefNum,
                         SdErr = dfStdErrors,
                         pValue = dfpValues
  )
  results[,-1] <- round(results[,-1], 3)
  results <- cbind(type, results)
  results
}

#Fonction qui calcule toutes les regressions logarithmiques
all.logarithmic <- function(data, dependent, regressors){
  epsilon <- 10^(-6)
  data1 <- as.data.frame(lapply(data, function(x) log(abs(x)+epsilon)))
  temp <- all.linear(data1, dependent, regressors)
  temp[,1] <- gsub('lin','log', temp[,1], fixed = TRUE) 
  temp
}

#Fonction qui calcule toutes les regressions exponentielles
all.exponential <- function(data, dependent, regressors){
  epsilon <- 10^(-6)
  data1 <- data
  data1[,which(names(data) == dependent, TRUE)] <- log(epsilon+abs(data[,which(names(data) == dependent, TRUE)]))
  temp <- all.linear(data1, dependent, regressors)
  temp[,1] <- gsub('lin','exp', temp[,1], fixed = TRUE) 
  temp
}


#Fonction qui calcule toutes les regressions polynomiales
all.poly <- function(data, dependent, regressors, maxdegree){
  data1 <- data[,which(names(data) != dependent, TRUE)]
  for(i in 2:length(names(data))){
    for(j in 2:maxdegree){
      data1 <- cbind(data1,(data[,i])^j)
      colnames(data1)[length(names(data1))] <- paste0(colnames(data)[i],'^',j)
    }
  }
  data1 <- cbind(data[,which(names(data) == dependent, TRUE)],data1)
  colnames(data1)[1] <- dependent
  reg <- names(data[!grepl(dependent, names(data), fixed = TRUE)])
  reg <- names(data1)[-1]
  temp <- all.linear(data1, dependent, reg)
  temp[,1] <- gsub('lin','poly', temp[,1], fixed = TRUE) 
  temp
}


shinyServer(function(input, output) {
  
  USER <- reactiveValues(Logged = FALSE)
  output$uiLogin <- renderUI({
    #Si l'utilisateur n'est pas identifié, afficher les cases pour renseigner le login/mot de pass
    #Dès qu'il les rentre correctement ça disparait
    if (USER$Logged == FALSE) {
      wellPanel(
        textInput("Username", "Identifiant:"),
        textInput("Password", "Mot de pass:"),
        br(),
        actionButton("Login", "Connexion")
      )
    }
  })
  
  output$pass <- renderText({  
    if (USER$Logged == FALSE) {
      if (input$Login > 0) {
        Username <- isolate(input$Username)
        Password <- isolate(input$Password)
        Id.username <- which(PASSWORD$Brukernavn == Username)
        Id.password <- which(PASSWORD$Password    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER$Logged <- TRUE
            print("You are log in! Wellcome!")
          } 
        } else  {
          "User name or password failed!"
        }
      } 
    }
  })
  
  data <- reactive({
    if (USER$Logged == TRUE){
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      mydata <- read.csv(inFile$datapath, 
                         header = input$header,sep = input$sep, 
                         quote = input$quote, dec = input$dec,  stringsAsFactors =FALSE)
      mydata
    } else {
       mydata <- data.frame(WARNING = 'You are not log in! Nothing will work. Please log in')
      } 
  })
  
  
  observe({
    if(input$action==0) return()
    isolate({
      mtype <- input$mtype
      dep <- dependent()
      regr <- paste(input$checkGroup,collapse=" ")
      output$value <- renderPrint({
        paste(mtype,dep,regr)
      })
  })
})
  
 
  output$summary <- renderPrint({ str(data())})
  output$view <- renderTable({ head(data(), n = 5)})
  output$selectUIx <- renderUI({ 
    selectInput("x", "Select X:", names(data()))
  })
  output$checkvariables <- renderUI({
    checkboxGroupInput("checkGroup", label = h3("Variables for model"), 
                       choices = as.list(names(data())[!grepl(dependent(), names(data()), fixed = TRUE)]))
  })
  
  output$selectUIy <- renderUI({ 
    selectInput("y", "Select Y:", names(data()))
  })
  output$selectDependent <- renderUI({
    selectInput("dependent", "Select Dependent Variable:", names(data()))
  })
  dependent <- reactive({
    mydependent <- input$dependent
    mydependent
  })
  output$plot <- renderPlot({p <- ggplot(data(), aes_string(x=input$x, y=input$y))
                             p <- p + geom_point()
                             p <- p + geom_smooth(method="lm",formula=y~x)
                             print(p)
                             })
  sliderValues <- reactive({
    value = input$deg
    value
    })
  output$bigtable <- renderDataTable({
    reg <- names(data())[!grepl(dependent(), names(data()), fixed = TRUE)]
    result1 <- all.linear(data = data(), dependent = dependent(), regressors = reg)
    result2 <- all.logarithmic(data = data(), dependent = dependent(), regressors = reg)
    result3 <- all.exponential(data = data(), dependent = dependent(), regressors = reg)
    result4 <- all.poly(data = data(), dependent = dependent(), regressors = reg, maxdegree = sliderValues())
    rbind(result1, result2, result3, result4)
    })
})
