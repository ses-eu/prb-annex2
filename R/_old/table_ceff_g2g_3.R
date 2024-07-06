
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

## import data
sheet <- "9_G2G"
range <- "C25:M28"
g2g_1_3  <- read_range(ceff_file, sheet, range)
myrownames <- g2g_1_3[1]
mycolnames <- colnames(g2g_1_3)

g2g_1_3_s <- g2g_1_3[,c(-1,-5)]  
g2g_1_3_t <- transpose(g2g_1_3_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(2), ~paste0(format(round(.*100,1)), "%"))) %>% 
  mutate(across(c(3), ~paste0(format(round(.,1)), " p.p."))) 

colnames(myrownames) <- "a"
mytypenames <- g2g_1_3[5] 
colnames(mytypenames) <- "b"

g2g_1_3_tt <- transpose(g2g_1_3_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) %>% 
  mutate( mytypenames, .before = V4 )

## prepare data
data_for_table <- g2g_1_3_tt %>% 
  select(!c(2:4)) %>% 
  mutate_all(~ str_replace(., "NA p.p.", "")) %>% 
  mutate_all(~ str_replace(., "NA%", "")) %>%  
  mutate_all(~ str_replace(., "NA", "")) 


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
               minWidth = 25, 
               align = "left"
    ), 
    b = colDef(name = "", minWidth = 8),
    V4 = colDef(name = "2020", minWidth = 11),
    V5 = colDef(name = "2021", minWidth = 11),
    V6 = colDef(name = "2020-2021", minWidth = 12),
    V7 = colDef(name = "2022", minWidth = 11),
    V8 = colDef(name = "2023", minWidth = 11),
    V9 = colDef(name = "2024", minWidth = 11)
  ),
  borderless = TRUE
)

t

