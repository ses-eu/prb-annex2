
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

#import data
sheet <- country

if (exists("range") == FALSE) {range <- "B42:N51"} # so it can be executed independently
# the range is passed from the generate files
df <- read_range(env_mil_file, sheet, range)  

mycolnames <- colnames(df)

## prepare data
data_for_table <- df %>% 
  select(1, 4, 6, 8, 10, 12)  %>% 
  mutate_at(c(-1), ~ case_when (
    suppressWarnings(is.na(as.numeric(.))) == TRUE  ~ ., 
    .default = suppressWarnings(paste0(format(round(as.numeric(.)*100, 0)), "%")))
    ) %>% 
  rename(a = 1) %>% 
  filter(is.na(a) == FALSE)

# 
# data_for_table <- data_for_table %>% 
#   mutate_all(~ str_replace(., "NA%", "")) 


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
                         minWidth = 15,
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.8rem",
                           style=list("white-space"= "wrap")
                           )
                         
                  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
                minWidth = 25, 
                align = "left", 
                style = list(whiteSpace = "pre",
                             "font-size" = "0.8rem"
                  # , "font-weight" = "bold"
                            )
                )
    
            )
)

t
