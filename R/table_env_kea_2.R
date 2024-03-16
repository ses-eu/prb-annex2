
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

#import data
sheet <- country
  range <- "A11:M14"
  df <- read_range(env_kea_file, sheet, range)  

## prepare data
data_for_table <- df %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  mutate(across(c(2:13), ~paste0(format(round(.*100,2)), "%"))) %>%  
  mutate_all(~ str_replace(., "NA%", "")) %>% 
  rename(a = 1)

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
                          "font-size" = "0.8rem",
                          "white-space"= "wrap"
                          ),
                         align = "center",
                         minWidth = 7,
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.8rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name='', 
                                    minWidth = 16, 
                                    align = "left", 
                                    style = list(whiteSpace = "pre",
                                                 "font-size" = "0.8rem"
                                      # , "font-weight" = "bold"
                                      )
                         ) # to preserve whitespace,

  )
)

t
