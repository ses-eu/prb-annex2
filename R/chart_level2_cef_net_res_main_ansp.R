
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


# import data  ----
data_raw  <-  regulatory_result(cztype, mycz)

# prepare data ----
data_prep <- data_raw %>% 
  filter(type == "Main ANSP",
         year_text == if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))
         )  %>% 
  select(year_text, atsp_gain_loss_cost_sharing, trs, financial_incentive, ex_post_roe) %>% 
  pivot_longer(-year_text, names_to = "status", values_to = "mymetric") %>% 
  mutate (
    mymetric = mymetric/1000,
    ylabel = case_when (
    status == 'atsp_gain_loss_cost_sharing' ~ "Cost sharing", 
    status == 'trs' ~ "Traffic risk sharing",
    status == 'financial_incentive' ~ "Incentives",
    status == 'ex_post_roe' ~ "Actual RoE in value"),
    mylabel = format(round(mymetric,1), big.mark = ",", nsmall =1)
  )
  
# chart parameters ----
mychart_title <- paste0(main_ansp," net result from ", 
                        if_else(cztype == 'terminal', 'terminal', 'en route'),
                        " activity (M€) - ", year_report)
myaxis_title <- ""
mybarcolor_pos <- '#9ECF8D'
mybarcolor_neg <- '#F87474'
mytextcolor <- 'black'
myhovertemplate <- paste0('%{x:,.1f}<extra></extra>')
myxaxis_tickformat <- "0,"

###set up order of traces
myfactor <- c("Actual RoE in value",
              "Incentives",
              "Traffic risk sharing",
              "Cost sharing")

mylocalmargin <- list (t = 40, b = 60)
mydecimals <- 1

# plot chart  ----

### calculate x range, and annotation and image position
myroundup <- max(floor((log10(abs(max(data_prep$mymetric, na.rm = TRUE))))), floor((log10(abs(min(data_prep$mymetric, na.rm = TRUE))))))
range_min <- floor(min(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup - 10^myroundup/2
range_min <- if_else(range_min >0, 0, range_min)
range_max <- ceiling(max(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup + 10^myroundup/2

### plot chart and add annotations
myplot <- myhbarc(mywidth, myheight+30, myfont, mylocalmargin) %>% 
  layout(
    xaxis = list(
      range = c(range_min, range_max)
      ),
    images = list(
      list(
        # Add images
        source =  base64enc::dataURI(file = here("images","arrow_right.png")),  
        xref = "paper",  
        yref = "paper",  
        x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
        y = -0.12,  
        sizex = 0.18,  
        sizey = 0.18,  
        layer = "above",
        xanchor="left",  
        yanchor="bottom" 
      ),
      list(
        # Add images
        source =  base64enc::dataURI(file = here("images","arrow_left.png")),  
        xref = "paper",  
        yref = "paper",  
        x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
        y = -0.12,  
        sizex = 0.18,  
        sizey = 0.18,  
        layer = "above",
        xanchor="right",  
        yanchor="bottom" 
      )
      )
    ,
    annotations = list(
      list(
      xanchor = "left",
      x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)) + 0.05,
      y = -0.2,
      text = '<b>ANSP gain</b>',
      font = list(size = myfont*0.85),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    ),
    list(
      xanchor = "right",
      x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)) -0.05,
      y = -0.2,
      text = '<b>ANSP loss</b>',
      font = list(size = myfont*0.85),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    )
    )
  )

myplot
