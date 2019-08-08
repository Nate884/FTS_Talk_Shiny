library(shiny)
library(bs4Dash)
library(dygraphs)
library(purrr)
library(rlang)
library(xts)
library(dplyr)
library(timetk)

tbi_aws_data <- aws.s3::s3readRDS("shiny/glencore/data.rda", bucket="tbi-ml")


contracts <- list ("M1", "M2", "M3")
Engine_Mod_UI <- function(id){
  ns <- NS(id)
blah_pmx <- function(x){
  Gap <- eval(parse_expr(paste0("tbi_aws_data$pmx_Gap_",x)))
  Eng <- eval(parse_expr(paste0("tbi_aws_data$pmx_Eng_CE_",x)))
  Imp <- eval(parse_expr(paste0("tbi_aws_data$pmx_Imp_CE_",x)))
  Datex <- eval(parse_expr(paste0("tbi_aws_data$pmx_Date_",x)))
  
  text <- ifelse(abs(Gap) >= 0.05, 
                 paste("PMX", 
                       x,
                       "/",
                       Eng, 
                       Imp),
                 paste("PMX",
                       x, 
                       "Do Nothing / ", 
                       Imp)
  )
  date_pmx <- paste("4TC Signal Date", Datex)
  signal_color <- 
    ifelse(abs(Gap) < 0.05,
           "warning",
           ifelse(
             Eng == "Buy",
             "success",
             "danger"
           )
    )
  
  Final_List <- list(text, date_pmx, signal_color)
}
Recs <- lapply(contracts, blah_pmx)

bs4Card(
  width = 12,
  gradientColor = "primary",
  title = "Overall Recommendations",
  fluidRow(
    lapply (1:length(Recs), function(i){
      bs4ValueBox(
        value = Recs[[i]][[1]],
        subtitle = Recs[[i]][[2]],
        status = Recs[[i]][[3]],
        width = 4)
    }
    )
  )    
)    
}

Engine_Mod_UI_Cape <- function(id){
  ns <- NS(id)
 blah_cape <- function(x){
  Gap <- eval(parse_expr(paste0("tbi_aws_data$cape_Gap_",x)))
  Eng <- eval(parse_expr(paste0("tbi_aws_data$cape_Eng_CE_",x)))
  Imp <- eval(parse_expr(paste0("tbi_aws_data$cape_Imp_CE_",x)))
  Datex <- eval(parse_expr(paste0("tbi_aws_data$cape_Date_",x)))
  CV <- eval(parse_expr(paste0("tbi_aws_data$cape_CV_",x)))
  Range <- eval(parse_expr(paste0("tbi_aws_data$cape_Range_",x)))
  
  limits <- per_contract_limits[[x]]
  text <- limits[1]
  
  stuff <- ifelse(abs(Gap) >= limits[1] & CV <= limits[2] & Range <= limits[3],
                  paste0("Cape ",
                         x,
                         ": ",
                         Eng,
                         " / ",
                         Imp),
                  paste0("Cape ",
                         x,
                         ": Do Nothing",
                         " / ",
                         Imp)
  )
  
  date_pmx <- paste("5TC Signal Date", Datex)
  
  signal_color <- 
    ifelse(abs(Gap) < 0.05,
           "warning",
           ifelse(
             Eng == "Buy",
             "success",
             "danger"
           )
    )
  Final_List <- list(stuff, date_pmx, signal_color)
}

Recs2 <- lapply(contracts, blah_cape)

bs4Card(
  width = 12,
  gradientColor = "primary",
  title = "Overall",
  fluidRow(
    lapply (1:length(Recs2), function(i){
      bs4ValueBox(
        value = Recs2[[i]][[1]],
        subtitle = Recs2[[i]][[2]],
        status = Recs2[[i]][[3]])
    }
)
)
)
}


Engine_Recs2_UI <- function(id, Eng_M){
  ns <- NS(id)
  
  Commod_ImpCE <- eval(parse_expr(paste0("tbi_aws_data$pmx_Imp_CE_",Eng_M)))
  Commod_NameCE <- eval(parse_expr(paste0("tbi_aws_data$pmx_Name_CE_",Eng_M)))
  
  name_eng <- case_when(
    Commod_NameCE == "A" ~ "E1",
    Commod_NameCE == "B" ~ "E2",
    Commod_NameCE == "C" ~ "E3",
    Commod_NameCE == "D" ~ "E4",
    Commod_NameCE == "E" ~ "E5",
    TRUE        ~ "E6")
  
  Final_ImpCE_Title <- paste0(
    Commod_ImpCE,
    " (",
    name_eng,
    ")"
  )
  
  Final_ImpCE_Subtitle <- "CE Implied Forecast" 
  
Engine_Recs2 <- function(x){
  Commod_Choice <- eval(parse_expr(paste0("tbi_aws_data$pmx_Eng_",x,"_",Eng_M)))
  Commod_Imp <- eval(parse_expr(paste0("tbi_aws_data$pmx_Imp_",x,"_",Eng_M)))
  Commod_Rank <- eval(parse_expr(paste0("tbi_aws_data$pmx_Rnk_",x,"_",Eng_M)))

  Final_Title <- paste0(
    Commod_Choice,
    ": ",
    Commod_Imp
  )
  
  Final_Subtitle <- paste0(
    " pmx-",
    "E",
    x,
    " Rank: ",
    Commod_Rank
  )
  
  Final_Color <- ifelse(Commod_Choice == "Buy", "success", "danger")
  
  list(Final_Title, Final_Subtitle, Final_Color)
}

engine_numbers <- list (1, 2, 3, 4, 5, 6)
Engine_Final <- lapply(engine_numbers, Engine_Recs2)

bs4Card(
  width = 12,
  title = "Engine Recommendations",
  fluidRow(
    
    bs4ValueBox(
      value = Final_ImpCE_Title,
      subtitle = Final_ImpCE_Subtitle,
      status = "warning"
    ),
    lapply (1:length(Engine_Final), function(i){
      bs4ValueBox(
        value = Engine_Final[[i]][[1]],
        subtitle = Engine_Final[[i]][[2]],
        status = Engine_Final[[i]][[3]]
        )
    }
    )
  )
)
}

Engine_Recs2_Cape_UI <- function(id, Eng_M){
  ns <- NS(id)
  
  Commod_ImpCE <- eval(parse_expr(paste0("tbi_aws_data$cape_Imp_CE_",Eng_M)))
  Commod_NameCE <- eval(parse_expr(paste0("tbi_aws_data$cape_Name_CE_",Eng_M)))
  
  name_eng <- case_when(
    Commod_NameCE == "A" ~ "E1",
    Commod_NameCE == "B" ~ "E2",
    Commod_NameCE == "C" ~ "E3",
    Commod_NameCE == "D" ~ "E4",
    Commod_NameCE == "E" ~ "E5",
    TRUE        ~ "E6")
  
  Final_ImpCE_Title <- paste0(
    Commod_ImpCE,
    " (",
    name_eng,
    ")"
  )
  
  Final_ImpCE_Subtitle <- "CE Implied Forecast" 
  
  Engine_Recs2 <- function(x){
    Commod_Choice <- eval(parse_expr(paste0("tbi_aws_data$cape_Eng_",x,"_",Eng_M)))
    Commod_Imp <- eval(parse_expr(paste0("tbi_aws_data$cape_Imp_",x,"_",Eng_M)))
    Commod_Rank <- eval(parse_expr(paste0("tbi_aws_data$cape_Rnk_",x,"_",Eng_M)))
    
    Final_Title <- paste0(
      Commod_Choice,
      ": ",
      Commod_Imp
    )
    
    Final_Subtitle <- paste0(
      " cape-",
      "E",
      x,
      " Rank: ",
      Commod_Rank
    )
    
    Final_Color <- ifelse(Commod_Choice == "Buy", "success", "danger")
    
    list(Final_Title, Final_Subtitle, Final_Color)
  }
  
  engine_numbers <- list (1, 2, 3, 4, 5)
  Engine_Final <- lapply(engine_numbers, Engine_Recs2)
  
  bs4Card(
    width = 12,
    title = "Engine Recommendations",
    fluidRow(
      
      bs4ValueBox(
        value = Final_ImpCE_Title,
        subtitle = Final_ImpCE_Subtitle,
        status = "warning"
      ),
      lapply (1:length(Engine_Final), function(i){
        bs4ValueBox(
          value = Engine_Final[[i]][[1]],
          subtitle = Engine_Final[[i]][[2]],
          status = Engine_Final[[i]][[3]]
        )
      }
      )
    )
  )
}

DyGraphs_Mod <- function(id, engine){
  ns <- NS(id)
  bs4Card(
    width = 12,
    title = "Engines",
    tagList(
      selectInput(
        "Test",
        "Engine:",
        c("Chosen Engine" = "Implied_engine",
          "Engine 1" = "Implied_E1",
          "Engine 2" = "Implied_E2",
          "Engine 3" = "Implied_E3",
          "Engine 4" = "Implied_E4",
          "Engine 5" = "Implied_E5",
          "Engine 6" = "Implied_E6"
        )
      ),
      selectInput(
        "Test2",
        "Fill / Line:",
        c("Fill", "Line")
      ),
      dygraphOutput(paste0("dygraph_server_",engine))
    )
  )
}

DyGraphs_Mod_Cape <- function(id, engine){
  ns <- NS(id)
  bs4Card(
    width = 12,
    title = "Engines",
    tagList(
      selectInput(
        "Testcape",
        "Engine:",
        c("Chosen Engine" = "Implied_engine",
          "Engine 1" = "Implied_E1",
          "Engine 2" = "Implied_E2",
          "Engine 3" = "Implied_E3",
          "Engine 4" = "Implied_E4",
          "Engine 5" = "Implied_E5"
        )
      ),
      selectInput(
        "Test2cape",
        "Fill / Line:",
        c("Fill", "Line")
      ),
      dygraphOutput(paste0("dygraph_server_cape_", engine))
    )
  )
}




Data_Exp_1 <- function(id, engine){
  ns <- NS(id)
  bs4Card(
    width = 12,
    title = "Data Exploration",
  tagList(
  dateInput(
  "filter1",
  value = "2017-06-01",
  label = "Start Date PNL",
  min = "2017-06-01",
  startview = "year"
),
sliderInput(
  "filter2",
  "TBI Gap Filter",
  0, 0.3, 0.05, step = 0.05
),
sliderInput(
 "filter3",
  "Trades / Month (30 D/lots)", 2, 24, 13, step = 2
),
dygraphOutput(paste0("dygraph_issue_", engine)),
dygraphOutput(paste0("pnlgraph_", engine))
)
)
}

Data_Exp2 <- function(id, contract){
  slider_cv_contract_ls = list("M1" = 0.5, "M2" = 0.75, "M3" = 1)
  slider_range_contract_ls = list("M1" = 0.25, "M2" = 0.25, "M3" = 0.2)
  bs4Card(
    width = 12,
    title = "Data Exploration",
  tagList(
  dateInput(
  "DE1",
  value = "2017-06-01",
  label = "Start Date PNL",
  min = "2017-06-01",
  startview = "year"
),
sliderInput(
  "DE2",
  "TBI Gap Filter",
  0, 0.3, 0.05, step = 0.05
),
sliderInput(
  "DE3",
  "TBI Volatility Filter",
  0.1, 1.5,
  0.1, step = 0.05
),
sliderInput(
  "DE4",
  "TBI Range Filter",
  0.1, 1,
  0.25, step = 0.05
),
sliderInput(
  "DE5",
  "Number of Trades / Month PNL",
  1, 6, 4, step = 1
),
fluidRow(
  width = 12,
  box(
    width=4,
    title = "Gap %",
    dygraphOutput(paste0("dygraph_issue_cape_", contract))
  ),
  box(
    width=4,
    title = "CV Value",
    dygraphOutput(paste0("dygraph_CV_", contract))
  ),
  box(
    width=4,
    title = "Range Value",
    dygraphOutput(paste0("dygraph_Range_", contract))
  )
),
box(
  width = 12,
  title = "Cumulative PnL",
dygraphOutput(paste0("pnlgraph_cape_", contract))
)
)
)
}


sidebar <- bs4DashSidebar(
  skin = "dark",
  status = "primary",
  title = "True Bearing Insights",
  brandColor = "primary",
  url = "",
  src = "https://www-staging.truebearinginsights.com/img/logo_wh@2x.png",
  elevation = 3,
  opacity = 0.8,
  bs4SidebarUserPanel(
    img = "https://www-staging.truebearinginsights.com/img/logo_wh@2x.png", 
    text = "Analytics Options"
  ),
  bs4SidebarMenu(
    bs4SidebarHeader("Classifier"),
    bs4SidebarMenuItem(
      "Commodity - PMX",
      icon = "sliders",
      startExpanded = TRUE,
      bs4SidebarMenuSubItem(
        text = "Contract M1",
        tabName = "PMX_M1",
        icon = "clipboard"
      ),
      bs4SidebarMenuSubItem(
        text = "Contract M2",
        tabName = "PMX_M2",
        icon = "clipboard"
      ),
      bs4SidebarMenuSubItem(
        text = "Contract M3",
        tabName = "PMX_M3",
        icon = "clipboard"
      )
    ),
    bs4SidebarMenuItem(
      "Commodity - Cape",
      startExpanded = TRUE,
      bs4SidebarMenuSubItem(
        text = "Contract M1",
        tabName = "Cape_M1",
        icon = "clipboard"
      ),
      bs4SidebarMenuSubItem(
        text = "Contract M2",
        tabName = "Cape_M2",
        icon = "clipboard"
      ),
      bs4SidebarMenuSubItem(
        text = "Contract M3",
        tabName = "Cape_M3",
        icon = "clipboard"
      )
    )
  )
)

body <- bs4DashBody(
  bs4TabItems(
    bs4TabItem(
      tabName = "PMX_M1",
        Engine_Mod_UI(id = "item1"),
        Engine_Recs2_UI(id="item1_M1", Eng_M = "M1"),
        DyGraphs_Mod(id="dy_M1", engine = "M1"),
        Data_Exp_1(id="D_E_1", engine = "M1")
    ),
    bs4TabItem(
      tabName = "PMX_M2",
        Engine_Mod_UI(id = "item1"),
        Engine_Recs2_UI(id="item1_M2", Eng_M = "M2"),
        DyGraphs_Mod(id="dy_M2", engine = "M2"),
        Data_Exp_1(id="D_E_2", engine = "M2")
      ),
    bs4TabItem(
      tabName = "PMX_M3",
      Engine_Mod_UI(id = "item1"),
      Engine_Recs2_UI(id="item1_M3", Eng_M = "M3"),
      DyGraphs_Mod(id="dy_M3", engine = "M3"),
      Data_Exp_1(id="D_E_3", engine = "M3")
    ),
    bs4TabItem(
      tabName = "Cape_M1",
      Engine_Mod_UI_Cape(id = "item2"),
      Engine_Recs2_Cape_UI(id="item2_M1", Eng_M = "M1"),
      DyGraphs_Mod_Cape(id ="Cape_M1", engine = "M1"),
      Data_Exp2(id="D_E_41", contract = "M1")
    ),
    bs4TabItem(
      tabName = "Cape_M2",
      Engine_Mod_UI_Cape(id = "item2"),
      Engine_Recs2_Cape_UI(id="item2_M2", Eng_M = "M2"),
      DyGraphs_Mod_Cape(id ="Cape_M2", engine = "M2"),
      Data_Exp2(id="D_E_42", contract = "M2")
),
  bs4TabItem(
  tabName = "Cape_M3",
  Engine_Mod_UI_Cape(id = "item3"),
  Engine_Recs2_Cape_UI(id="item2_M3", Eng_M = "M3"),
  DyGraphs_Mod_Cape(id ="Cape_M3", engine = "M3"),
  Data_Exp2(id="D_E_43", contract = "M3")
)
)
)


server <- function(input,output,session){

    dygraphfunc <- function(x){
      Dy_Data <- eval(parse_expr(paste0("tbi_aws_data$dt_graphs.pmx_Engines_",x)))  
      renderDygraph({
        if (input$Test2 == "Fill"){
          dygraph(Dy_Data[ ,c("Contract", input$Test)]) %>%
            dySeries(input$Test, fillGraph = TRUE, color="green") %>%
            dySeries("Contract", fillGraph = TRUE, color = "red")
        }
        else {
          dygraph(Dy_Data[ ,c(input$Test, "Contract", "Settlement")])
        }
      })
    }
    
    output$dygraph_server_M1 <- dygraphfunc("M1")
    output$dygraph_server_M2 <- dygraphfunc("M2")
    output$dygraph_server_M3 <- dygraphfunc("M3")
    
    dygraphfunc_cape <- function(x){
      Dy_Data <- eval(parse_expr(paste0("tbi_aws_data$dt_graphs.cape_Engines_",x)))  
      renderDygraph({
        if (input$Test2cape == "Fill"){
          dygraph(Dy_Data[ ,c("Contract", input$Testcape)]) %>%
            dySeries(input$Testcape, fillGraph = TRUE, color="green") %>%
            dySeries("Contract", fillGraph = TRUE, color = "red")
        }
        else {
          dygraph(Dy_Data[ ,c(input$Testcape, "Contract", "Settlement")])
        }
      })
    }
    
    output$dygraph_server_cape_M1 <- dygraphfunc_cape("M1")
    output$dygraph_server_cape_M2 <- dygraphfunc_cape("M2")
    output$dygraph_server_cape_M3 <- dygraphfunc_cape("M3")
    
    
    dygraph_PMX_2 <- function(x){
      Dy_Data_2 <- eval(parse_expr(paste0("tbi_aws_data$dt_graphs.pmx_Gap_",x)))  
      
      renderDygraph({
        dygraph(Dy_Data_2) %>%
        dyShading(from = -0.05, to = 0.05, axis = "y", color="#F89999") %>%
        dyLimit(
          input$filter2,
          color = "black",
          strokePattern = "dashed") %>%
        dyLimit(
          -input$filter2,
          color = "Filter2",
          strokePattern = "dashed")
        })
    }
    output$dygraph_issue_M1 <- dygraph_PMX_2("M1")
    output$dygraph_issue_M2 <- dygraph_PMX_2("M2")
    output$dygraph_issue_M3 <- dygraph_PMX_2("M3")
    
    dygraph_PMX_cape_2 <- function(x){
      Dy_Data_2 <- eval(parse_expr(paste0("tbi_aws_data$dt_graphs.cape_Gap_",x)))  
      
      renderDygraph({
        dygraph(Dy_Data_2) %>%
          dyShading(from = -0.05, to = 0.05, axis = "y", color="#F89999") %>%
          dyLimit(
            input$DE2,
            color = "black",
            strokePattern = "dashed") %>%
          dyLimit(
            -input$DE2,
            color = "Filter2",
            strokePattern = "dashed")
      })
    }
    output$dygraph_issue_cape_M1 <- dygraph_PMX_cape_2("M1")
    output$dygraph_issue_cape_M2 <- dygraph_PMX_cape_2("M2")
    output$dygraph_issue_cape_M3 <- dygraph_PMX_cape_2("M3")
    
    dygraphfunc3 <- function(x){
    Dy_Data_3 <- eval(parse_expr(paste0("tbi_aws_data$dt_graphs.cape_CV_",x)))  
    renderDygraph({
      dygraph(Dy_Data_3) %>%
        dyShading(from = 0, to = 0.5, axis = "y", color="#CCEBD6") %>%
        dyLimit(
          input$DE3,
          color = "black",
          strokePattern = "dashed")
    })
    }
    output$dygraph_CV_M1 <- dygraphfunc3("M1")
    output$dygraph_CV_M2 <- dygraphfunc3("M2")
    output$dygraph_CV_M3 <- dygraphfunc3("M3")
    
    dygraphfunc4 <- function(x){
      Dy_Data_4 <- eval(parse_expr(paste0("tbi_aws_data$dt_graphs.cape_Range_",x)))  
      renderDygraph({
        dygraph(Dy_Data_4) %>%
          dyShading(from = 0, to = 0.5, axis = "y", color="#CCEBD6") %>%
          dyLimit(
            input$DE4,
            color = "black",
            strokePattern = "dashed")
      })
    }
    
    output$dygraph_Range_M1 <- dygraphfunc4("M1")
    output$dygraph_Range_M2 <- dygraphfunc4("M2")
    output$dygraph_Range_M3 <- dygraphfunc4("M3")
    
    pnlgraph1 <- function(x) {
      c_pnl = eval(parse_expr(paste0("tbi_aws_data$pmx_cumpnl_",x)))
      renderDygraph({
        c_pnl2 = reactive({c_pnl[which(abs(c_pnl$gap) >= abs(input$filter2)), ]})
        c_pnl3 <- c_pnl2()
        c_pnl3 = split.xts(c_pnl3, "months")

        c_pnl4 = reactive({ 
          do.call(rbind, lapply(c_pnl3, function(x) {
            if (dim(x)[1] >= input$filter3) {
              dt = x[1:input$filter3]
            } else{
              dt = x
            }
          }))
        })
        
        c_pnl5 <- c_pnl4()
        
        date_end_test = index(xts::last(c_pnl5))
        c_pnl6 = reactive({c_pnl5[paste(input$filter1, date_end_test, sep = "::"), ]})
        c_pnl7 <- c_pnl6()
        dygraph(round(cumsum(c_pnl7[, "outcome_FFAs"]))) %>%
          dySeries("outcome_FFAs", fillGraph = TRUE, label = "Cumulative Returns")
      })
    }
    output$pnlgraph_M1 <- pnlgraph1("M1")
    output$pnlgraph_M2 <- pnlgraph1("M2")
    output$pnlgraph_M3 <- pnlgraph1("M3")
    
    pnlgraph2 <- function(x) {
      c_pnl = eval(parse_expr(paste0("tbi_aws_data$cape_cumpnl_",x)))
      renderDygraph({
        
        c_pnl2 <- reactive({c_pnl %>%
          tk_tbl() %>%
          filter(CV_filter <= input$DE2) %>%
          filter(range <= input$DE4) %>%
          tk_xts() })
        
        #c_pnl2a = reactive({c_pnl[which(c_pnl$CV_filter <= input$DE2), ]})
        #c_pnl2ax <- c_pnl2a()
        #c_pnl2b = reactive({c_pnl2ax[which(abs(c_pnl2ax$gap) >= abs(input$DE3)), ]})
        #c_pnl2bx <- c_pnl2b()
        #c_pnl2c = reactive({c_pnl2bx[which(c_pnl2bx$range <= input$DE4), ]})
        
        c_pnl3 <- c_pnl2()
        c_pnl3 = split.xts(c_pnl3, "months")
        
        c_pnl4 = reactive({ 
          do.call(rbind, lapply(c_pnl3, function(x) {
            if (dim(x)[1] >= input$DE5) {
              dt = x[1:input$DE5]
            } else{
              dt = x
            }
          }))
        })
        
        c_pnl5 <- c_pnl4()
        
        date_end_test = index(xts::last(c_pnl5))
        c_pnl6 = reactive({c_pnl5[paste(input$DE1, date_end_test, sep = "::"), ]})
        c_pnl7 <- c_pnl6()
        dygraph(round(cumsum(c_pnl7[, "outcome_options_1000"]))) %>%
          dySeries("outcome_options_1000", fillGraph = TRUE, label = "Cumulative Returns")
      })
    }
    
    output$pnlgraph_cape_M1 <- pnlgraph2("M1")
    output$pnlgraph_cape_M2 <- pnlgraph2("M2")
    output$pnlgraph_cape_M3 <- pnlgraph2("M3")
}




shiny::shinyApp(
  ui = bs4DashPage(
    old_school = FALSE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = TRUE,
    title = "Basic Dashboard",
    navbar = bs4DashNavbar(),
    sidebar = sidebar,
    footer = bs4DashFooter(),
    body = body
  ),
  server = server
)
