
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

## import data
sheet <- "9_G2G"
range <- "C20:M24"
g2g_1_2  <- read_range(ceff_file, sheet, range)

myrownames <- g2g_1_2[1]
mycolnames <- colnames(g2g_1_2)

g2g_1_2_s <- g2g_1_2 %>% 
  select(6:11)
g2g_1_2_t <- transpose(g2g_1_2_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1:3), ~format(round(.,0), big.mark = ",", scientific = F))) %>% 
  mutate(across(c(4), ~paste0(format(round(.*100,1)), "%"))) %>%   
  mutate_all(~ str_replace(., "NA%", "NA")) %>% 
  mutate_all(~ str_replace(., "NA", "")) 
  
colnames(myrownames) <- "a"

g2g_1_2_tt <- transpose(g2g_1_2_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) 

## prepare data
data_for_table <- g2g_1_2_tt 

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
                          "font-size" = "0.72rem",
                          "white-space"= "wrap"
                          ),
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
                                    minWidth = 33, 
                                    align = "left"
                         ), 
    V1 = colDef(name = "202AD", minWidth = 11),
    V2 = colDef(name = "2021A", minWidth = 11),
    V3 = colDef(name = "2020-2021A", minWidth = 12),
    V4 = colDef(name = "2022A", minWidth = 11),
    V5 = colDef(name = "2023A", minWidth = 11),
    V6 = colDef(name = "2024A", minWidth = 11)
  ),
  borderless = TRUE
)

t

