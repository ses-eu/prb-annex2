
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

## parameters
source("R/parameters.R")

## import data
sheet <- "1_ERT"
range <- if_else(nat_curr == 'EUR', "C21:M27", "C21:M28")
ert_1_3_1  <- read_range(file, sheet, range)
myrownames <- ert_1_3_1[1]
mycolnames <- colnames(ert_1_3_1)
if (nat_curr == 'EUR') {
  mycols <- c(6)} else {mycols <- c(6, 7)}

ert_1_3_1_s <- ert_1_3_1[,-1] 
ert_1_3_1_t <- transpose(ert_1_3_1_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1,4,5), ~format(round(.,0), big.mark = ",", scientific = F))) %>% 
  mutate(across(c(2), ~paste0(format(round(.*100,1)), "%"))) %>%
  # mutate(across(c(2), ~format(round(.*100,1)))) %>%
  mutate(across(c(3), ~format(round(.,1), big.mark = ",", scientific = F))) %>% 
  mutate(across(all_of(mycols), ~format(round(.,2), big.mark = ",", scientific = F))) %>% 
  mutate_all(~ str_replace(., "NA%", "NA"))

colnames(myrownames) <- "a"

ert_1_3_1_tt <- transpose(ert_1_3_1_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) 

## prepare data
data_for_table <- ert_1_3_1_tt %>% 
  select(!c(2:4)) %>% 
  mutate_all(~ str_replace(., "NA", NA_character_))

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef( style = function(value) {
    color <- if (is.na(value)) {'#F2F2F2'} 
    list(background = color,
         "font-size" = "0.72rem",
         "white-space"= "wrap")
  } ,
  align = "right",
  headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.72rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
                                    minWidth = 39, 
                                    align = "left",
                            style = list(background = 'white',
                            "font-size" = "0.72rem",
                            "white-space"= "wrap")
                         ), 
    V4 = colDef(name = "", minWidth = 0),
    V5 = colDef(name = "2020D", minWidth = 10),
    V6 = colDef(name = "2021D", minWidth = 10),
    V7 = colDef(name = "2020-2021D", minWidth = 11),
    V8 = colDef(name = "2022D", minWidth = 10),
    V9 = colDef(name = "2023D", minWidth = 10),
    V10 = colDef(name = "2024D", minWidth = 10)
  ),
  borderless = TRUE,
  rowStyle = 
    function(index) {
      if (index == nrow(data_for_table)) list(fontWeight = "bold",
                                              borderTop = "1px solid rgba(0, 0, 0, 0.1)")
    }
)

t

