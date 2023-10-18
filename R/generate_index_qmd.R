
#---------------- sections definition
# section 1
idx_1 <- paste0('<br>
[Annual Montiroring Report ', year_report, ']{class="fakeh1"}

[Local level view]{class="fakeh1" style="font-weight: normal"}

[', country,']{class="fakeh1" style="font-weight: normal"}
'
)                

# assemble all and create .qmd
cat(paste0(
  idx_1
  
),
file = "index.qmd")

