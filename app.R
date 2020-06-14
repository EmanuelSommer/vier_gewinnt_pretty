library(tidyverse)

# Implementierung eines vier gewinnt spieles
# "R" bzw 1 wird für rot stehen und "G" bzw -1 für gelb. 

start_matrix <- matrix(rep("E",7*6),nrow = 6)

# implementiert einen Zug im Spiel
ziehen <- function(mat,farbe,spalte){
    if(mat[1,spalte] != "E"){
        return(list(mat,"Unerlaubter Zug"))
    } else {
        n_empty <- sum(mat[,spalte] == "E")
        mat[n_empty,spalte] <- farbe
        return(list(mat,"Erlaubter Zug"))
    }
}

# helper function um eine Matrix in Minoren aufzuteilen und diese in einer Liste zu speichern
unterteile_in_minoren <- function(mat, dimension = 4){
    # nur fuer 4 3 und 2 dimensionen
    dim_mat <- dim(mat)
    list4 <- list()
    for(i in 1:(dim_mat[1]+1-dimension)){
        for (j in 1:(dim_mat[2]+1-dimension)) {
            list4 <- append(list4,list(mat[c(i:(i+(dimension-1))),c(j:(j+(dimension-1)))]))
        }
    }
    return(list4) 
}

# evaluiert einen 4 kreuz 4 Minor, wird dann in spielende() benutzt
analyse_minor <- function(minor,spieler){
    mat_rot <- minor == "R"
    mat_gelb <- minor == "G"
    mat_leer <- minor == "E"
    if(any(rowSums(mat_rot) == 4) | any(rowSums(t(mat_rot)) == 4) | sum(diag(mat_rot)) == 4 | sum(diag(mat_rot[,4:1])) == 4){
        return(Inf)
    } else if(any(rowSums(mat_gelb) == 4) | any(rowSums(t(mat_gelb)) == 4) | sum(diag(mat_gelb)) == 4 | sum(diag(mat_gelb[,4:1])) == 4){
        return(-Inf)
    } else {
        rot_gefaehrlich <- sum(rowSums(mat_rot) == 3 & rowSums(mat_leer) == 1) +
            sum(rowSums(t(mat_rot)) == 3 & rowSums(t(mat_leer)) == 1) +
            as.integer(sum(diag(mat_rot)) == 3 & sum(diag(mat_leer)) == 1) +
            as.integer(sum(diag(mat_rot[,4:1])) == 3 & sum(diag(mat_leer[,4:1])) == 1)
        gelb_gefaehrlich <- sum(rowSums(mat_gelb) == 3 & rowSums(mat_leer) == 1) +
            sum(rowSums(t(mat_gelb)) == 3 & rowSums(t(mat_leer)) == 1) +
            as.integer(sum(diag(mat_gelb)) == 3 & sum(diag(mat_leer)) == 1) +
            as.integer(sum(diag(mat_gelb[,4:1])) == 3 & sum(diag(mat_leer[,4:1])) == 1)
        # initiale Strategie hat folgende Konstrukte benutzt (nicht sehr erfolgreich)
        # minor3_ergeb <- sapply(unterteile_in_minoren(minor,3), function(minor3){
        #     mat_rot3 <- minor3 == "R"
        #     mat_gelb3 <- minor3 == "G"
        #     rot3 <- sum(rowSums(mat_rot3) == 3) + sum(rowSums(t(mat_rot3)) == 3) +
        #         as.integer(sum(diag(mat_rot3)) == 3) + as.integer(sum(diag(t(mat_rot3))) == 3)
        #     gelb3 <- sum(rowSums(mat_gelb3) == 3) + sum(rowSums(t(mat_gelb3)) == 3) +
        #         as.integer(sum(diag(mat_gelb3)) == 3) + as.integer(sum(diag(t(mat_gelb3))) == 3)
        #     return(rot3 - gelb3)
        # },simplify = TRUE)
        # minor2_ergeb <- sapply(unterteile_in_minoren(minor,2), function(minor2){
        #     mat_rot2 <- minor2 == "R"
        #     mat_gelb2 <- minor2 == "G"
        #     rot2 <- sum(rowSums(mat_rot2) == 2) + sum(rowSums(t(mat_rot2)) == 2) +
        #         as.integer(sum(diag(mat_rot2)) == 2) + as.integer(sum(diag(t(mat_rot2))) == 2)
        #     gelb2 <- sum(rowSums(mat_gelb2) == 2) + sum(rowSums(t(mat_gelb2)) == 2) +
        #         as.integer(sum(diag(mat_gelb2)) == 2) + as.integer(sum(diag(t(mat_gelb2))) == 2)
        #     return(rot2 - gelb2)
        # },simplify = TRUE)
        # freiraum_aussen <- ifelse(sum(minor3_ergeb) != 0,sum(c(minor[1,],minor[2,1],minor[3,1],minor[2,4],minor[3,4]) == "E"),0)
        #
        # Strategie:
        # Gefährdungen ausschalten wichtiger als selbst welche aufzubauen:
        if(spieler){
            return(-200 * gelb_gefaehrlich^2 + 10 * rot_gefaehrlich)
        } else {
            return(-200 * rot_gefaehrlich^2 + 10 * gelb_gefaehrlich)
        }
    }
}

# evaluiert die ganze Matrix
spielende <- function(mat,spieler){
    # analyse 4kreuz4 Minoren
    minoren_ergebnisse <- sapply(unterteile_in_minoren(mat,4) , function(minor){
        analyse_minor(minor,spieler)
    },simplify = TRUE)
    # analyse 5kreuz5 minoren für das verhindern von "Fallen"
    minoren55_ergebnisse <- sapply(unterteile_in_minoren(mat,5) , function(minor){
        mat_rot <- minor == "R"
        mat_gelb <- minor == "G"
        mat_leer <- minor == "E"
        rot_gefaehrlich <- sum(rowSums(mat_rot) == 3 & rowSums(mat_leer) == 2) +
            sum(rowSums(t(mat_rot)) == 3 & rowSums(t(mat_leer)) == 2) +
            as.integer(sum(diag(mat_rot)) == 3 & sum(diag(mat_leer)) == 2) +
            as.integer(sum(diag(mat_rot[,5:1])) == 3 & sum(diag(mat_leer[,5:1])) == 2)
        gelb_gefaehrlich <- sum(rowSums(mat_gelb) == 3 & rowSums(mat_leer) == 2) +
            sum(rowSums(t(mat_gelb)) == 3 & rowSums(t(mat_leer)) == 2) +
            as.integer(sum(diag(mat_gelb)) == 3 & sum(diag(mat_leer)) == 2) +
            as.integer(sum(diag(mat_gelb[,5:1])) == 3 & sum(diag(mat_leer[,5:1])) == 2)
        if(spieler){
            return(-300 * gelb_gefaehrlich + 5 * rot_gefaehrlich)
        } else {
            return(-300 * rot_gefaehrlich + 5 * gelb_gefaehrlich)
        }
    },simplify = TRUE)
    # nachträgliche marginale gewichtung der minoren ergebnisse (gefährdungen auf Minoren weiter unten sind relevanter)
    minoren_ergebnis <- sum(minoren_ergebnisse*c(rep(c(0.8,1,1.2),each = 4))) + sum(minoren55_ergebnisse*c(rep(c(0.8,1,1.2),each = 2)))
    if(minoren_ergebnis == Inf){
        return("R")
    } else if(minoren_ergebnis == -Inf){
        return("G")
    } else if(all(mat != "E")){
        return("Unentschieden") 
    } else {
        return(minoren_ergebnis)
    }
}

#kleine helper function um mögliche Züge zu bestimmen
moegliche_zuege <- function(mat){
    return(c(1:7)[mat[1,] == "E"])
}

# Implementierung des Algorithmus
minimax <- function(mat, spieler, tiefe, alpha, beta){
    spielende_temp <- spielende(mat,spieler)
    if(spielende_temp %in% c("R","G","Unentschieden") | tiefe == 0){
        return(list(best_val = spielende_temp,
                    best_move = "",
                    alpha,
                    beta))
    }
    best_move <- ""
    if(spieler){
        best_val <- -Inf
        symbol <- "R"
    } else {
        best_val <- Inf
        symbol <- "G"
    }
    for(zug in sample(moegliche_zuege(mat))){
        new_mat <- ziehen(mat,symbol,zug)[[1]]
        hypo_val <- minimax(new_mat,!spieler,tiefe-1,alpha,beta)$best_val
        if(is.character(hypo_val) & hypo_val == "Unentschieden"){
            hypo_val <- 0
        }
        if(is.character(hypo_val) & hypo_val == "R"){
            hypo_val <- Inf
        }
        if(is.character(hypo_val) & hypo_val == "G"){
            hypo_val <- -Inf
        }
        
        if(spieler & hypo_val > best_val){
            best_val <- hypo_val
            best_move <- zug
            alpha <- max(alpha, best_val)
        } 
        if(!spieler & hypo_val < best_val){
            best_val <- hypo_val
            best_move <- zug
            beta <- min(beta, best_val)
        }
        if(alpha > beta){
            break
        }
    }
    return(list(best_val = best_val,
                best_move = best_move,
                alpha,
                beta))
}

# Plotten der Matrix
plot_grid <- function(mat){
    colnames(mat) <- as.character(1:7)
    mat %>%
        as_tibble() %>%
        mutate(n_row = 6:1) %>%
        pivot_longer(as.character(1:7)) %>%
        mutate(n_col = as.integer(name)) %>%
        ggplot(aes(x = n_col,y = n_row,fill = value)) +
        geom_point(shape = 21,size = 22) +
        scale_fill_manual(values = c(E = "white",R = "red",G = "yellow"),
                          guide = NULL)+
        labs(x = "",y = "")+
        theme_bw()+
        xlim(0.5,7.5) + ylim(0.5,6.5) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid.minor = element_line(colour = "#7575a3",size = 2),
              panel.grid.major = element_line(colour = "#7575a3",size = 2),
              panel.background = element_rect(fill = "#a3a3c2",colour = "#a3a3c2"),
              plot.background = element_rect(fill = "#7575a3", colour = "#7575a3"),
              plot.margin = margin(20,20,0,0))
}



#####################################################################################################################
#####################################################################################################################
#
#
#                           Shiny Teil
#
#
#####################################################################################################################
#####################################################################################################################

library(shiny)
library(shinydashboard)

ui <- fluidPage(
    tags$head(
        tags$link(href="style.css", rel="stylesheet", type="text/css"),
        tags$script(src = "http://platform.twitter.com/widgets.js")
    ),
    tags$div(
        class = "title-app",
        tags$h1("Vier Gewinnt"),
        tags$h4("Bezwinge den Computer!")
    ),
    tags$br(),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FFFFFF; color: #041f70}")),
    tags$div(
        style = "width: 33%; margin: auto; color: #FFFFFF; text-align: center;",
        tags$br(),
        sliderInput("tiefe",label = "Einstellung der Schwierigkeit:",min = 1, max = 5,value = 3,width = "100%"),
        tags$br()
    ),
    
    tags$div(
        style = "width: 650px; margin: auto;",
        tags$br(),
        plotOutput("spielfeld",click = "plot_click"),
        tags$br()
    ),
    tags$div(
        style = "width: 650px; margin: auto; text-align: center;",
        tags$br(),
        actionButton("reset",label = "Neues Spiel",icon = icon("redo")),
        tags$br(),
        tags$br()
    ),
)
# ui <- dashboardPage(
#     dashboardHeader(title = "Vier gewinnt",
#                     dropdownMenu(
#                         type = "notifications",
#                         icon = icon("link"),
#                         headerText = "Links",
#                         notificationItem("Mein Github", icon = icon("github"),
#                                          href = "https://github.com/EmanuelSommer/")
#                     )
#     ),
#     dashboardSidebar(
#         sliderInput("tiefe",label = "Einstellung der Schwierigkeit:",min = 1, max = 5,value = 3),
#         tags$br()
#     ),
#     dashboardBody(
#         fluidRow(
#             column(width = 6,
#                    infoBoxOutput("status",width = NULL)
#             ),
#             column(width = 6,
#                    tags$br(),
#                    actionButton("reset",label = "Neues Spiel",icon = icon("redo")),
#                    tags$br()
#             ),
#             column(width = 12,
#                    box(width = NULL,title = "Spielfeld",
#                        plotOutput("spielfeld",click = "plot_click"))),
#             column(width = 12,
#                    infoBox(title = "Herausforderung",width = 12,
#                            subtitle = "Für wen der Gegner zu leicht zu besiegen ist, der kann sich einer deutlich schwereren Herausforderung stellen: Erreiche ein Unentschieden bei möglichst hohem Schwierigkeitsgrad.",
#                            icon = icon("medal"),color = "blue"),
#                    infoBox(title = "Tipp",width = 12,
#                            subtitle = "Auf mobilen Geräten ist Querformat meist angenehmer zum Spielen.",
#                            icon = icon("info"),color = "blue")
#             )
#         )
#     ),
#     skin = "black"
#     
# )

#####################################################################################################################

server <- function(input, output,session) {
    values <- reactiveValues(mat = start_matrix,text = "Weiter",over = FALSE)
    observeEvent(input$reset,{
        values$mat <- start_matrix
        values$text <- "Weiter"
        values$over <- FALSE
    })
    # clicken aufs spielfeld:
    observeEvent(input$plot_click,{
        x_click <- round(input$plot_click$x)
        if(!values$over & x_click %in% moegliche_zuege(values$mat)){
            values$mat <- ziehen(values$mat,"R",x_click)[[1]]
            spielende_temp <- spielende(values$mat,TRUE)
            if(is.character(spielende_temp)){
                values$over <- TRUE
                values$text <- spielende_temp
            } else {
                minimax_res <- minimax(values$mat,FALSE,input$tiefe,-20000,20000)
                if(!is.numeric(minimax_res$best_move)){
                    moeglich_temp <- moegliche_zuege(values$mat)
                    if(length(moeglich_temp) == 0){
                        values$over <- TRUE
                        values$text <- spielende(values$mat,TRUE)
                    } else {
                        next_move <- sample(moeglich_temp,1)
                    }
                } else {
                    next_move <- minimax_res$best_move
                }
                values$mat <- ziehen(values$mat,"G",next_move)[[1]]
                spielende_temp <- spielende(values$mat,TRUE)
                if(is.character(spielende_temp)){
                    values$over <- TRUE
                    values$text <- spielende_temp
                }
            }
        }
    })
    
    observe({
        if (values$text == "R") {
            showModal(modalDialog(
                tags$div(
                    style = "text-align: center;color: #7575a3;font-weight: bold;",
                    tags$h2(
                        tags$span(icon("trophy"), style = "color: #F7E32F;"),
                        "Du gewinnst!",
                        tags$span(icon("trophy"), style = "color: #F7E32F;")
                    ),
                    tags$br(), tags$br(),
                    actionButton(
                        inputId = "reload",
                        label = "Nochmal!",
                        style = "width: 100%; color: #7575a3"
                    )
                ),
                footer = NULL,
                easyClose = FALSE
            ))
        } else if(values$text == "G") {
            showModal(modalDialog(
                tags$div(
                    style = "text-align: center;color: #7575a3;font-weight: bold;",
                    tags$h2(
                        tags$span(icon("skull"), style = "color: #000000;"),
                        "Du verlierst!",
                        tags$span(icon("skull"), style = "color: #000000;")
                    ),
                    tags$br(), tags$br(),
                    actionButton(
                        inputId = "reload",
                        label = "Nochmal!",
                        style = "width: 100%;"
                    )
                ),
                footer = NULL,
                easyClose = FALSE
            ))
        } else if(values$text == "Unentschieden") {
            showModal(modalDialog(
                tags$div(
                    style = "text-align: center;color: #7575a3;font-weight: bold;",
                    tags$h2(
                        tags$span(icon("meh"), style = "color: #fd8d03;"),
                        "Unentschieden!",
                        tags$span(icon("meh"), style = "color: #fd8d03;")
                    ),
                    tags$br(), tags$br(),
                    actionButton(
                        inputId = "reload",
                        label = "Nochmal!",
                        style = "width: 100%;"
                    )
                ),
                footer = NULL,
                easyClose = FALSE
            ))
        }
    })
    
    observeEvent(input$reload, {
        session$reload()
    }, ignoreInit = TRUE)
    
    output$spielfeld <- renderPlot({
        plot_grid(values$mat)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
