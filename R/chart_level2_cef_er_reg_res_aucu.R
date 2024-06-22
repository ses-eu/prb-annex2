
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "enroute")}
# ez=1

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

# get data ----
# regulatory result
data_prep_reg_all <- regulatory_result(cztype, mycz)
data_prep_reg <- data_prep_reg_all %>% 
  group_by(year_text, x5_4_total_su) %>% 
  summarise(reg_res = sum(regulatory_result)) %>% 
  mutate(reg_res_per_su = reg_res * 1000 / x5_4_total_su) %>% 
  select(year_text, reg_res, reg_res_per_su)

# aucu
data_prep_aucu_all <- aucu(cztype, mycz) 
data_prep_aucu <- data_prep_aucu_all %>% 
  select(year_text, aucu_excluding_or)

# join tables
data_prep <- data_prep_reg %>% 
  left_join(data_prep_aucu, by = 'year_text') %>% 
  select(-reg_res) %>% 
  mutate(rr_as_perc_aucu = reg_res_per_su / aucu_excluding_or * 100) %>% 
  pivot_longer(-c(year_text, rr_as_perc_aucu), names_to = 'type', values_to = 'mymetric') %>% 
  mutate(type = case_when(
    type == 'reg_res_per_su'  ~ 'Regulatory result per SU',
    type == 'aucu_excluding_or'  ~ 'AUCU (before other revenues)'
    ),
    xlabel = year_text,
    myothermetric = round(rr_as_perc_aucu, 2)
  ) 
  
# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c( '#9DC3E6', '#FFC000')
###set up order of traces
myfactor <- c("AUCU (before other revenues)",
              "Regulatory result per SU")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'transparent'

### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
mytitle_text <- paste0("AUCU and Regulatory result")

#### xaxis

#### yaxis
myyaxis_title <- "AUCU and Regulatory result (€/SU)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend

#### margin
mylocalmargin = list(t = 60, b = 0, l = 60, r = 60)

#____additional trace parameters
myat_name <- "Share of RR in AUCU (%)"
myat_mode <- "markers"
myat_yaxis <- "y2"
myat_symbol <- NA
myat_marker_color <- '#E46C0A'
myat_line_color <- 'transparent'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont

# plot chart  ----
mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., filter(data_prep, type == 'Regulatory result per SU'))  %>% 
  add_empty_trace(., data_prep) %>% 
  layout(
    yaxis = list(rangemode = "nonnegative"),   # to force the zeros to coincide
    yaxis2 = list(
    title = 'Regulatory result as a % of AUCU',
    zerolinecolor = '#E8E8E8',
    rangemode = "nonnegative",
    overlaying = "y",
    ticksuffix = '%',
    side = 'right',
    showgrid = FALSE
  ))
  
