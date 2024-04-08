
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

## import data
sheet <- "2_ERT"
range <- range <- "J16:M33"
ert_1_6  <- read_range(ceff_file, sheet, range)

## prepare data
ert_1_6_c <- ert_1_6 %>% 
  clean_names() %>% 
  rename(nat_su = 3) %>% 
  mutate_at(c(2:4), ~ as.numeric(.))
  

if (nat_curr == 'EUR'){  
  data_for_table <- ert_1_6_c %>% 
  mutate(
    nat_su_f = case_when(
      components_of_the_aucu != 'AUCU vs. DUC' ~ format(round(nat_su,2)),
      TRUE  ~ paste0(if_else(nat_su >=0, '+', ''), format(round(nat_su * 100,1)), '%') 
    )
    ) %>% 
  select(!c(2, 3, 4)) %>% 
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
                          "font-size" = "0.75rem",
                          "white-space"= "wrap"
                          ),
                         align = "center",
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.75rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    components_of_the_aucu = colDef(name="Components of the AUCU", 
                                    minWidth = 60, 
                                    align = "left"
                                  ), 
    nat_su_f = colDef(name = if_else(nat_curr == 'EUR', '€/SU', paste0(nat_curr, '/SU' )), 
                align = "right", 
                minWidth = 40)
  ),
  borderless = TRUE,
  rowStyle = 
    function(index) {
      if (index == nrow(data_for_table)) list(fontWeight = "bold")
      else if (index == nrow(data_for_table)-1) list(fontWeight = "bold")
      else if (index == 3) list(fontWeight = "bold")
      else if (index == 8) list(background = "#F2F2F2")
      else if (index == 11) list(background = "#F2F2F2")
    }
)
} else{
  data_for_table <- ert_1_6_c %>% 
  mutate(
    nat_su_f = case_when(
      components_of_the_aucu != 'AUCU vs. DUC' ~ format(round(nat_su,2)),
      TRUE  ~ paste0(if_else(nat_su >=0, '+', ''), format(round(nat_su * 100,1)), '%') 
    ),
  su_f = case_when(
      components_of_the_aucu != 'AUCU vs. DUC' ~ format(round(su,2)),
      TRUE  ~ paste0(if_else(su >=0, '+', ''), format(round(su * 100,1)), '%') 
    )    
  ) %>% 
    select(!c(2, 3, 4)) %>% 
    mutate_all(~ str_replace(., "NA", NA_character_))
  
  ## plot table
  t <- reactable(
    data_for_table,
    bordered = TRUE,
    pagination = FALSE,
    striped = FALSE,
    compact = TRUE,
    highlight = TRUE,
    defaultColDef = colDef(style = list(
      "font-size" = "0.75rem",
      "white-space"= "wrap"
    ),
    align = "center",
    headerStyle = list(
      background = "#D9D9D9", 
      # color = "white", 
      fontSize = "0.75rem",
      style=list("white-space"= "wrap")
    )
    
    ),
    columns = list(
      components_of_the_aucu = colDef(name="Components of the AUCU", 
                                      minWidth = 58, 
                                      align = "left" 

      ),
      nat_su_f = colDef(name = if_else(nat_curr == 'EUR', '€/SU', paste0(nat_curr, '/SU' )), 
                    align = "right", 
                    minWidth = 21),
      su_f = colDef(name = '€/SU', 
                    align = "right", 
                    minWidth = 21)
    ),
    borderless = TRUE,
    rowStyle = 
      function(index) {
        if (index == nrow(data_for_table)) list(fontWeight = "bold")
        else if (index == nrow(data_for_table)-1) list(fontWeight = "bold")
        else if (index == 3) list(fontWeight = "bold")
        else if (index == 8) list(background = "#F2F2F2")
        else if (index == 11) list(background = "#F2F2F2")
      }
  )
  
  
}
t

