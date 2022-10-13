library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(gt)



ui <- navbarPage("ViewPoint report table generator",
                 
                 theme = shinytheme("united"),
                 
                 tabPanel("Table", 
                          
                          textInput("rows",
                                    "Enter desired number of rows",
                                    value = "3"
                          ),
                          textInput("columns",
                                    "Enter desired number of columns",
                                    value = "3"
                          ),
                          
                          br(),
                          h4("Note:"),
                          h5("• All columns require a label; excess rows will be ingored"),
                          h5("• To wrap text in a cell or column label, simply insert '</br>' at desired linebreaks, which render in the formatted table below"), 
                          
                          br(),
                          br(),
                          
                          
                          
                          DT::dataTableOutput("mytable"),
                          
                          
                          
                          actionButton("button", "Click to Format Table"),
                          
                          tags$br(), 
                          
                          gt::gt_output("new_table"),
                          
                          tags$br(),
                          
                          h5("Drag cursor to highlight formatted table text and copy/paste to ViewPoint!"),
                          h5("When you render to PDF or fax a report, it should look like a formatted ascii table."),
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          tags$head(tags$style("#new_table{
                              font-family:monospace;
                                 }"
                          )
                          )
                          
                 ), 
                 
                 tabPanel("About", 
                          tags$p(
                            HTML(
                              paste0(
                                "<b>Eric Bergh, M.D.</b> is an OB/GYN, Maternal-Fetal Medicine and Fetal Intervention Specialist",
                                " in Houston, TX with interests in both fetal diseases and computer/data science.",
                                tags$br(),
                                "This web app was designed using the ", 
                                tags$a(href = "https://www.r-project.org/", "R", target = "_blank"),  " programming language, ",
                                tags$a(href = "https://gt.rstudio.com/", "{gt}", target = "_blank"),", ", 
                                tags$a(href = "https://rstudio.github.io/DT/", "{DT}", target = "_blank"), " packages and the ", 
                                tags$a(href = "https://shiny.rstudio.com/", "{shiny}", target = "_blank"), " web framework.", 
                                tags$br(), 
                                "Please do not hesitate to contact ",
                                tags$a(href = "https://twitter.com/ericberghMD", "@ericberghMD ", target = "_blank"),
                                "with any questions/suggestions.", 
                                tags$br(), 
                                "The app is neither endorsed nor distributed by GE. It was created as a simple adjunt to improve upon my own reports while using GE ViewPoint software.", 
                                tags$br(), 
                                "The code for this app can be found ",
                                tags$a(href = "https://github.com/berghe01/jouRnal_club/blob/main/app.R", "here. ", target = "_blank"), 
                                tags$br() 
                              )
                            )
                          )
                 )
                 
                 
                 
                 
)




server <- function(input, output, session) {
  
  
  ###################### TABLE #############################
  
  
  data <- reactive({
    req(input$rows, input$columns)
    mydf <- as.data.frame(matrix(
      "",
      as.numeric(input$rows)+1,
      as.numeric(input$columns)
    ))
    
    colnames(mydf) <-gsub("V","col ",colnames(mydf))
    mydf
  })
  
  dfon <- reactiveValues(data = NULL)
  
  observe({
    dfon$data <- data() 
  })
  
  output$mytable <- DT::renderDataTable({
    datatable(
      data(),
      editable = "cell", 
      #rownames = FALSE, 
      options = list(
        #dom='t',
        searching = FALSE,
        ordering = FALSE,
        autoWidth = TRUE, 
        pageLength = 50,
        columnDefs = list(list(className = 'dt-center', 
                               width = '225px',
                               targets = "_all"))
      ), 
      rownames = c("Column label row*", 1:(dim(data())[1]-1))#, 
      
    ) %>%
      formatStyle(names(data()),# select all columns in table
                  height = 55)
  })
  
  observeEvent(input$mytable_cell_edit, {
    info <- input$mytable_cell_edit
    
    i <- info$row
    j <- info$col
    v <- info$value
    
    dfon$data[i, j] <- v
  })
  
  
  
  
  
  make_gt_table <- function(g){
    
    
    g <- g %>%
      janitor::row_to_names(1) 
    g <- g %>%
      mutate(across(everything(), as.character)) %>%
      mutate(across(everything(), ~paste0("╎ ", .)))
    
    
    
    
    add_col <- map_df(g, function(x) max(nchar(x)))
    add_col_sum <- rowSums(add_col[,]) + (dim(add_col)[2]*2)
    add_col <- add_col %>%
      mutate(across(everything(), ~paste0(rep("=", .), collapse = ""))) %>%
      mutate(across(everything(), ~paste0(" ", .)))
    

    
    mycols <- colnames(g)
    mycols_df <- mycols %>% as.data.frame() %>% t() %>% as.data.frame()
    
    mycols_df <- setNames(mycols_df, mycols)
    
    mycols_df <- lapply(mycols_df, function(x){
      x <- data.frame(x)
      x %>% separate_rows(1, sep = "</br>")
      
    })
    
    cnt <- sapply(mycols_df, nrow)
    cnt <- max(cnt)
    
    mycols_df <- map_df(mycols_df, function(x){
      x <- pull(x) 
      length(x) <- cnt
      x
      
    }

    )
    
    mycols_df <- mycols_df %>%
      mutate(across(everything(), .fns = ~replace_na(.,""))) 

    
    mycols_df <- mycols_df %>% 
      mutate(across(everything(), ~paste0("╎ ", . )))
    
    g <- rbind(mycols_df, g)
    
    mycols <- rep("", dim(g)[2])
    
    mycols <- setNames(mycols, names(g))
    
    
    g %>%
      gt::gt() %>%
      gt::tab_row_group(
        label = paste(rep("=", add_col_sum), collapse = ""), 
        rows = (dim(mycols_df)[1]+1):(dim(g)[1])
      ) %>%
      gt::cols_label(
        !!!mycols
      ) %>%
      gt::tab_options(row_group.default_label = '') %>%
      gt::row_group_order(c(NA, paste(rep("=", add_col_sum), collapse = "") ))
 
    
    
  }
  
  

  
  formatted_table <- eventReactive(input$button, {
    
    make_gt_table(dfon$data )
    
  })
  
  
  observeEvent(input$button, {
    
    
    
    output$new_table <- gt::render_gt({    
      
      validate(
        
        need(!berryFunctions::is.error(formatted_table()), "")
      )
      
      formatted_table()
      
      
      
    })
    
    
    
    
  })
  
  
  
  
  
  
  
}

shinyApp(ui, server)

