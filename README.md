# PC-incidence

Display incidence and mortality of Prostate Cancer in Sweden over time.

The repository contains the following:  

+ *data/*:  folder with data  
  - *Statistikdatabasen_2019-11-28 12_56_54.csv*: number of deaths per 100 000 from Prostate Cancer (C61) for different age categories. Data available from socialstyrelsen (https://sdb.socialstyrelsen.se/if_dor/val.aspx)  
  - *Statistikdatabasen_2019-11-28 12_56_57.csv*: number of new cases of Prostate Cancer per 100 000 for different age categories. Data available from socialstyrelsen (https://sdb.socialstyrelsen.se/if_can/val.aspx)  
+ *incidence-mortality-PC.R*: static script for the analysis of incidence and mortality rates in Sweden over time  
+ *PC_inc_mort/*: folder with shiny app (code + data)  
  - *www/*: folder with data  
  - *app.R*: R script for shiny app