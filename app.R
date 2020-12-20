library(shiny)
library(tidyverse)
library(plotly)

combat_feat_choices <- c("Flurry/Rapid Shot 1", 
                         "Flurry/Rapid Shot 2", 
                         "Flurry/Rapid Shot 3",
                         "Critical Strike/Sniper Shot 1", 
                         "Critical Strike/Sniper Shot 2",
                         "Critical Strike/Sniper Shot 3",
                         "Power Attack/Power Blast 1",
                         "Power Attack/Power Blast 2", 
                         "Power Attack/Power Blast 3",
                         "None")

combat_style_choices <- c("Dueling 1", 
                          "Dueling 2", 
                          "Dueling 3",
                          "Two-Weapon Fighting 1", 
                          "Two-Weapon Fighting 2",
                          "Two-Weapon Fighting 3",
                          "None")

force_choices <- c("None", "Burst of Speed", "Knight Speed", "Master Speed")

keenness_choices <- c(0, 1) #0=not keen, 1=keen

threat_range_choices <- c(0.05, 0.1) #0.05=20-20, 0.1=19-20

balance_choices <- c(0, -2) #balanced, unbalanced

DEF_ATK <- seq(2, 20, 1)

map_mods <- function(myFactors){ #this calculates the bonuses for each feat/style/force
    if(all(myFactors$styles=="Dueling 1")){
        myFactors$modifiers1 <- myFactors$modifiers1+1
    }
    if(all(myFactors$styles=="Dueling 2")){
        myFactors$modifiers1 <- myFactors$modifiers1+2
    }
    if(all(myFactors$styles=="Dueling 3")){
        myFactors$modifiers1 <- myFactors$modifiers1+3
    }
    if(all(myFactors$styles=="Two-Weapon Fighting 1")){
        myFactors$n2 <- 1
        myFactors$modifiers1 <- myFactors$modifiers1-6
        myFactors$modifiers2 <- myFactors$modifiers2-6
    }
    if(all(myFactors$styles=="Two-Weapon Fighting 2")){
        myFactors$n2 <- 1
        myFactors$modifiers1 <- myFactors$modifiers1-4
        myFactors$modifiers2 <- myFactors$modifiers2-4
    }
    if(all(myFactors$styles=="Two-Weapon Fighting 3")){
        myFactors$n2 <- 1
        myFactors$modifiers1 <- myFactors$modifiers1-2
        myFactors$modifiers2 <- myFactors$modifiers2-2
    }
    if(all(myFactors$feats=="Flurry/Rapid Shot 1")){
        myFactors$n1 <- myFactors$n1+1
        myFactors$modifiers1 <- myFactors$modifiers1-4
        myFactors$modifiers2 <- myFactors$modifiers2-4
    }
    if(all(myFactors$feats=="Flurry/Rapid Shot 2")){
        myFactors$n1 <- myFactors$n1+1
        myFactors$modifiers1 <- myFactors$modifiers1-2
        myFactors$modifiers2 <- myFactors$modifiers2-2
    }
    if(all(myFactors$feats=="Flurry/Rapid Shot 3")){
        myFactors$n1 <- myFactors$n1+1
        myFactors$modifiers1 <- myFactors$modifiers1-1
        myFactors$modifiers2 <- myFactors$modifiers2-1
    }
    if(all(myFactors$feats=="Critical Strike/Sniper Shot 1")){
        myFactors$crit_bonus <- 2
    }
    if(all(myFactors$feats=="Critical Strike/Sniper Shot 2")){
        myFactors$crit_bonus <- 3
    }
    if(all(myFactors$feats=="Critical Strike/Sniper Shot 3")){
        myFactors$crit_bonus <- 4
    }
    if(all(myFactors$feats=="Power Attack/Power Blast 1")){
        myFactors$power_attack <- 5
        myFactors$modifiers1 <- myFactors$modifiers1-3
        myFactors$modifiers2 <- myFactors$modifiers2-3
    }
    if(all(myFactors$feats=="Power Attack/Power Blast 2")){
        myFactors$power_attack <- 8
        myFactors$modifiers1 <- myFactors$modifiers1-3
        myFactors$modifiers2 <- myFactors$modifiers2-3
    }
    if(all(myFactors$feats=="Power Attack/Power Blast 3")){
        myFactors$power_attack <- 10
        myFactors$modifiers1 <- myFactors$modifiers1-3
        myFactors$modifiers2 <- myFactors$modifiers2-3
    }
    if(all(myFactors$force=="Knight Speed")){
        myFactors$n1 <- myFactors$n1+1
    }
    if(all(myFactors$force=="Master Speed")){
        myFactors$n1 <- myFactors$n1+2
    }
    return(myFactors) 
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("KOTOR Combat Feat Comparisons"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width=5,
            fluidRow(
                column(width=4, 
                       textInput("main_max", "Main Hand Weapon Maximum Damage", value = 6),
                       textInput("main_min", "Main Hand Weapon Minimum Damage", value = 1),
                            #keen radio buttons
                       radioButtons("main_keen", "Main Weapon Keenness", 
                                    choiceNames = c("Not Keen", "Keen"),
                                    choiceValues = keenness_choices
                                    ),
                       radioButtons("threat_range_main", "Main Hand Weapon Threat Range",
                                    choiceNames = c("20-20", "19-20"),
                                    choiceValues = threat_range_choices
                                         ),
                       radioButtons("feats", "Combat Feat Level: 
                                          Critical Strike/Sniper Shot, 
                                          Flurry/Rapid Shot,
                                          Power Attack/Power Blast", 
                                          choiceNames = c("None", "Level 1", "Level 2", "Level 3"),
                                          choiceValues = c("None", "1", "2", "3")
                                          ),
                       radioButtons("force", "Force Powers", 
                                    choiceNames = force_choices,
                                    choiceValues = force_choices
                       )
                            ),
                     column(width=4, 
                            textInput("offhand_max", "Off-Hand Weapon Maximum Damage", value = 6),
                            textInput("offhand_min", "Off-Hand Weapon Minimum Damage", value = 1),
                            radioButtons("offhand_keen", "Off-Hand Weapon Keenness", 
                                         choiceNames = c("Not Keen", "Keen"),
                                         choiceValues = keenness_choices
                                         ),
                            radioButtons("threat_range_offhand", "Off-Hand Hand Weapon Threat Range",
                                         choiceNames = c("20-20", "19-20"),
                                         choiceValues = threat_range_choices
                                         ),
                            radioButtons("offhand_balance", "Off-Hand Weapon Balance", 
                                         choiceNames = c("Balanced", "Unbalanced"),
                                         choiceValues = balance_choices
                            ),
                            radioButtons("styles", "Combat Style Level:
                                          Dueling, 
                                          Two-Weapon Fighting", 
                                        choiceNames = c("None", "Level 1", "Level 2", "Level 3"),
                                        choiceValues = c("None", "1", "2", "3")
                                               )
                            )
                )
            ),
        
        mainPanel(width=7,
                  plotlyOutput("plot", width="100%", height="800px")
                  #tableOutput("table") #for debugging purposes

        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    #the idea here is to go ahead and calculate everything, and filter what data to display
    model_factors <- expand.grid(
        feats=combat_feat_choices,
        styles=combat_style_choices,
        force=force_choices,
        DEF_ATK=DEF_ATK,
        n1=1,
        n2=0,
        modifiers1=0,
        modifiers2=0,
        crit_bonus=0,
        power_attack=0)
    
    model_factors <- 
        model_factors %>% 
        mutate(styles=as.character(styles),
               feats=as.character(feats),
               force=as.character(force)) %>% 
        group_by(styles, feats, force) %>% 
        group_split() %>% 
        map(map_mods) %>% 
        bind_rows()
    
    observe({ 
        DMG_main_base <- (as.numeric(input$main_max)+as.numeric(input$main_min))/2
        DMG_offhand_base <- (as.numeric(input$offhand_max)+as.numeric(input$offhand_min))/2
        main_keen <- as.numeric(input$main_keen)
        balance_choice <- as.numeric(input$offhand_balance)
        threat_range_main <- as.numeric(input$threat_range_main)
        feats_choice <- input$feats
        offhand_keen <- as.numeric(input$offhand_keen)
        threat_range_offhand <- as.numeric(input$threat_range_offhand)
        styles_choice <- input$styles
        force_choice <- input$force
        
        model_factors <- 
            model_factors %>% 
            mutate(DMG_main=DMG_main_base,
                   DMG_offhand=DMG_offhand_base,
                   threat_main=threat_range_main,
                   threat_offhand=threat_range_offhand,
                   keen_main=main_keen*threat_main,
                   keen_offhand=offhand_keen*threat_offhand,
                   balance=balance_choice) %>% 
            mutate(balance=replace(balance, !grepl("Weapon", styles), 0), #this makes sure unbalanced only affects two-weapon fighting
                   total_damage=n1*DMG_main*(1.05-0.05*(DEF_ATK-modifiers1-balance))*
                       (1+threat_main*crit_bonus+keen_main*threat_main)+
                       n1*power_attack*(1.05-0.05*(DEF_ATK-modifiers1-balance))+
                       n2*DMG_offhand*(1.05-0.05*(DEF_ATK-modifiers2))*
                       (1+threat_offhand*crit_bonus+keen_offhand*threat_offhand)+
                       n1*power_attack*(1.05-0.05*(DEF_ATK-modifiers2))) %>% 
            mutate(total_damage=replace(total_damage, total_damage<0, 0))
        
        model_factors$feats2 <- gsub("/", "/\\\n", model_factors$feats)
        model_factors$styles2 <- gsub("Two-Weapon Fighting", "Two-Weapon\\\nFighting", 
                                      model_factors$styles)
        
        output$plot <- renderPlotly({
            text_size <- list(size = 18)
                
            xaxis_specs <- list(
                title = "(Defense) - (Total To-Hit Bonus)",
                titlefont = text_size,
                tickfont = text_size)
            
            yaxis_specs <- list(
                title = "Average Damage per Round",
                titlefont = text_size,
                tickfont = text_size)
            
            legend_specs <- list(
                font = list(size = 18),
                tracegroupgap=40
                )
            
            model_factors %>% 
                filter(grepl(feats_choice, feats)) %>% 
                filter(grepl(styles_choice, styles)) %>% 
                filter(force==force_choice) %>% 
                plot_ly(x = ~(DEF_ATK), y = ~total_damage, type = 'scatter', mode = 'lines',
                        linetype = ~styles2, color = ~feats2) %>% 
                layout(xaxis = xaxis_specs, yaxis = yaxis_specs, legend = legend_specs)
            
                
        })
        
        output$table <- renderTable({
            model_factors %>%   
                filter(grepl(feats_choice, feats)) %>% 
                filter(grepl(styles_choice, styles)) %>% 
                filter(force==force_choice)
        })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

