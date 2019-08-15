
# ----- 0: Packages and libraries -----

setwd("C:/Users/Hamed/Desktop/BSA Project/Updated-Version/Rcoding")
getwd()

# install.packages(c("dplyr", "swmmr", "devtools", "knitr","purrr", "gridExtra","ggplot2", "doParallel", "DT", "plotrix", "tictoc"))

library(dplyr); library(swmmr); library(devtools); library(purrr); library(knitr); library(gridExtra); library(grid); 
library(ggplot2); library(readr); library(ggmap); library(htmlwidgets); library(widgetframe); library(DT); 
library(dygraphs); library(xts); library(zoo); library(htmltools); library(plotrix); library(tictoc)


  # ----- 0.1: Names -----

bs_item <- 64
bs_number <- 24 
bs_item_all <- bs_item +1
file_name_scenario_pre <- character(); file_inp_scenario_pre <- character(); 
file_out_scenario_pre <- character(); file_rpt_scenario_pre <- character(); 
for (i in 1:bs_item) { 
  file_name_scenario_pre [i] <- paste("S", i, sep = "") 
  file_inp_scenario_pre [i] <- paste("S", i, ".inp", sep = "") 
  file_out_scenario_pre [i] <- paste("S", i, ".out", sep = "") 
  file_rpt_scenario_pre [i] <- paste("S", i, ".rpt", sep = "") 
}

file_name_scenario <- c( "S0", file_name_scenario_pre ) 
file_inp_scenario  <- c( "S0.inp", file_inp_scenario_pre  ) 
file_out_scenario  <- c( "S0.out", file_out_scenario_pre  ) 
file_rpt_scenario  <- c( "S0.rpt", file_rpt_scenario_pre  ) 


scenario_detail <- data.frame(Detail = c(
  "S0: Main Model",
  "S1: RB 75-gal for all", 
  "S2: RB 75-gal for HDC", 
  "S3: RB 75-gal for HLawn",  
  "S4: RB 75-gal for HSplash", 
  "S5: RB 75-gal for HStreet",
  "S6: RB 75-gal for HDC + HStreet",
  
  "S7: RB 150-gal for all", 
  "S8: RB 150-gal for HDC", 
  "S9: RB 150-gal for HLawn",  
  "S10: RB 150-gal for HSplash", 
  "S11: RB 150-gal for HStreet",
  "S12: RB 150-gal for HDC + HStreet",
  
  "S13: 2 RB 150-gal for all", 
  "S14: 2 RB 150-gal for HDC", 
  "S15: 2 RB 150-gal for HLawn",  
  "S16: 2 RB 150-gal for HSplash", 
  "S17: 2 RB 150-gal for HStreet",
  "S18: 2 RB 150-gal for HDC + HStreet",
  
  "S19: RB 1000-gal for all", 
  "S20: RB 1000-gal for HDC", 
  "S21: RB 1000-gal for HLawn",  
  "S22: RB 1000-gal for HSplash", 
  "S23: RB 1000-gal for HStreet",
  "S24: RB 1000-gal for HDC + HStreet",
  
  "S25: Cistern 5000-gall for Rcom", 
  "S26: Downspout Disconnection (DSD)", 
  
  "S27: DSD + RB 75 all", 
  "S28: DSD + RB 75 HSplash", 
  "S29: DSD + RB 75 HStreet", 
  "S30: DSD + RB 150 all", 
  "S31: DSD + RB 150 HSplash", 
  "S32: DSD + RB 150 HStreet", 
  "S33: DSD + 2 RB 150 all", 
  "S34: DSD + 2 RB 150 HDC", 
  "S35: DSD + 2 RB 150 HSplash", 
  "S36: DSD + 2 RB 150 HStreet", 
  "S37: DSD + RB 1000 all", 
  "S38: DSD + RB 1000 HDC", 
  "S39: DSD + RB 1000 HSplash", 
  "S40: DSD + RB 1000 HStreet",
  
  "S41: Cistern + RB 75 HDC",
  "S42: Cistern + RB 75 HDC + HStreet", 
  "S43: Cistern + RB 150 HDC", 
  "S44: Cistern + RB 150 HDC + HStreet", 
  "S45: Cistern + 2 RB 150 all", 
  "S46: Cistern + 2 RB 150 HDC", 
  "S47: Cistern + 2 RB 150 HDC+HStreet", 
  "S48: Cistern + RB 1000 all", 
  "S49: Cistern + RB 1000 HDC", 
  "S50: Cistern + RB 1000 HSplash", 
  "S51: Cistern + RB 1000 HDC + HStreet", 
  
  "S52: DSD + Cistern + RB 75 all", 
  "S53: DSD + Cistern + RB 150 all", 
  "S54: DSD + Cistern + 2 RB 150 all", 
  "S55: DSD + Cistern + 2 RB 150 HDC", 
  "S56: DSD + Cistern + 2 RB 150 HSplash", 
  "S57: DSD + Cistern + 2 RB 150 HStreet", 
  "S58: DSD + Cistern + 2 RB 150 HDC + HStreet", 
  "S59: DSD + Cistern + RB 1000 all", 
  "S60: DSD + Cistern + RB 1000 HDC", 
  "S61: DSD + Cistern + RB 1000 HSplash", 
  "S62: DSD + Cistern + RB 1000 HStreet", 
  "S63: DSD + Cistern + RB 1000 HDC + HStreet", 
  "S64: DSD + Cistern" 

) )

# ----- 1: Management Scenarios -----

  # ----- 1.0: Preparation -----

# Read the input file .......... 
Name_SWMM_input     <- c( "BSA_Main_Model.inp", "BSA_Main_Model_RB.inp" )
bs_file_for_write   <- read_inp(Name_SWMM_input[2])
inp_subcatchment    <- bs_file_for_write $ subcatchments
bs_subcatchment_inp <- bs_file_for_write $ subcatchments
bs_outfall          <- bs_file_for_write $ outfalls
bs_lid_usage        <- bs_file_for_write $ lid_usage
bs_options          <- bs_file_for_write $ options

# Some convertors ..........
bs_coef_ac_to_ft2 <- 43560
bs_coef_inch_to_meter <- 0.0254
bs_coef_m2_to_ft2 <- 10.7639
bs_coef_gal_to_lit <- 3.78541
bs_area_min_ac_RB <- 0.002471   # minimum area of subcatchments for adding rain barrel to the roofs (10 m2)
bs_area_min_ac_CR <- 0.049421   # minimum area of subcatchments for adding cistern to the commercial roofs (200 m2)

# Identify the roof subcatchments which are suitalbe for implementing RB and Cistern .........
bs_sub_RB_pre <- bs_subcatchment_inp %>% 
  filter (grepl("HDC", Name) | grepl("HLawn", Name) | grepl("HSplash", Name) |grepl("HStreet", Name) | grepl("Hstreet", Name))
bs_sub_RB <- bs_sub_RB_pre %>% filter (Area >= bs_area_min_ac_RB)

bs_sub_cistern_pre <- bs_subcatchment_inp %>% filter (grepl("RCom", Name) )
bs_sub_cistern <- bs_sub_cistern_pre %>% filter (Area >= bs_area_min_ac_CR)

# Prepare LID information ..........
bs_RB_75 <- c("Rain_Barrel_75gal")
bs_RB_150 <- c("Rain_Barrel_150gal")
bs_RB_1000 <- c("Rain_Barrel_1000gal")
bs_cistern_5000 <- c("Cistern_5000gal")

bs_RB_75_vol_gal <- 75
bs_RB_150_vol_gal <- 150
bs_RB_1000_vol_gal <- 1000
bs_cistern_5000_vol_gal <- 5000

bs_RB_75_vol_l <- bs_RB_75_vol_gal * bs_coef_gal_to_lit
bs_RB_150_vol_l <- bs_RB_150_vol_gal * bs_coef_gal_to_lit
bs_RB_1000_vol_l <- bs_RB_1000_vol_gal * bs_coef_gal_to_lit
bs_cistern_5000_vol_l <- bs_cistern_5000_vol_gal * bs_coef_gal_to_lit

bs_RB_75_height_inch <- 36
bs_RB_150_height_inch <- 58
bs_RB_1000_height_inch <- 65
bs_cistern_5000_height_inch <- 102

bs_RB_75_height_m <- bs_RB_75_height_inch * bs_coef_inch_to_meter
bs_RB_150_height_m <- bs_RB_150_height_inch * bs_coef_inch_to_meter
bs_RB_1000_height_m <- bs_RB_1000_height_inch * bs_coef_inch_to_meter
bs_cistern_5000_height_m <- bs_cistern_5000_height_inch * bs_coef_inch_to_meter

bs_RB_75_area_m2 <- bs_RB_75_vol_l / (1000 * bs_RB_75_height_m)
bs_RB_150_area_m2 <- bs_RB_150_vol_l / (1000 * bs_RB_150_height_m)
bs_RB_1000_area_m2 <- bs_RB_1000_vol_l / (1000 * bs_RB_1000_height_m)
bs_cistern_5000_area_m2 <- bs_cistern_5000_vol_l / (1000 * bs_cistern_5000_height_m)

bs_RB_75_area_ft2 <- round(bs_RB_75_area_m2 * bs_coef_m2_to_ft2, digits = 3)
bs_RB_150_area_ft2 <- round(bs_RB_150_area_m2 * bs_coef_m2_to_ft2, digits = 3)
bs_RB_1000_area_ft2 <- round(bs_RB_1000_area_m2 * bs_coef_m2_to_ft2, digits = 3)
bs_cistern_5000_area_ft2 <- round(bs_cistern_5000_area_m2 * bs_coef_m2_to_ft2, digits = 3)

    # ----- 1.0.1: Option part -----

# To change the time of running the model, and modify the model (change routing step and maximum trial)
# bs_options <- bs_file_for_write $ options
bs_options_new <- bs_options
bs_options_new [4,2] <- c("04/01/1993")
bs_options_new [6,2] <- c("04/01/1993")
bs_options_new [8,2] <- c("11/01/1993")
# bs_options_new [16,2] <- 15
# bs_options_new [27,2] <- 20

    # ----- 1.0.2: Outfall part -----

# Fix the problem in outfall part ..........

# bs_outfall <- bs_file_for_write $ outfalls
bs_outfall_correct <- bs_outfall

for (i in 1: nrow(bs_outfall_correct)) {
  if (bs_outfall_correct$ Gated [[i]] == "YE") {
    bs_outfall_correct$ Gated [[i]] <- "YES"
  }
}
bs_outfall_correct[[43,3]] <- "FREE"
bs_outfall_correct[[44,2]] <- "0"
bs_outfall_correct[[44,3]] <- "FREE"
bs_outfall_correct[[62,3]] <- "FREE"

    # ----- 1.0.3: Main Model (S0) -----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

write_inp (bs_file_for_write_new, "S0.inp")

  # ----- 1.1: Rain Barrel (Scenarios 1- 24) ----- 

    # ----- 1.1.1: Preparation -----
# The roof subcatchments are categorized based on the area. 
# each 0.0459616 acres (186 m2) of roofts, is consider to be one house. 
bs_RB_area_cat <- 0.0459616   # roof area of each house. 

bs_sub_RB_pre <- bs_subcatchment_inp %>% 
  filter (grepl("HDC", Name) | grepl("HLawn", Name) | grepl("HSplash", Name) |grepl("HStreet", Name) | grepl("Hstreet", Name))
bs_sub_RB <- bs_sub_RB_pre %>% filter (Area >= bs_area_min_ac_RB)

bs_sub_RB_cat <- list(); 
bs_lid_1RB_75_pre <- list(); bs_lid_1RB_150_pre <- list() ; bs_lid_2RB_150_pre <- list(); bs_lid_1RB_1000_pre <- list()
bs_lid_1RB_75 <- data.frame(); bs_lid_1RB_150 <- data.frame(); bs_lid_2RB_150 <- data.frame(); bs_lid_1RB_1000 <- data.frame()

for (i in 1: ceiling (max(bs_sub_RB$Area)/bs_RB_area_cat)) {
  
  bs_sub_RB_cat [[i]] <- bs_sub_RB %>% filter ( Area >= bs_RB_area_cat * (i-1) , Area < bs_RB_area_cat * i )
  
  bs_lid_1RB_75_pre [[i]] <- bs_sub_RB_cat[[i]] %>% select(Name) %>% rename(Subcatchment = Name) %>%
    mutate(`LID Process` = bs_RB_75, Number = 1 * i, Area = bs_RB_75_area_ft2, Width = 0, InitSat = 0, FromImp = 50, ToPerv = 1)
  bs_lid_1RB_75 <- rbind (bs_lid_1RB_75, bs_lid_1RB_75_pre [[i]])
  
  bs_lid_1RB_150_pre [[i]] <- bs_sub_RB_cat[[i]] %>% select(Name) %>% rename(Subcatchment = Name) %>%
    mutate(`LID Process` = bs_RB_150, Number = 1 * i, Area = bs_RB_150_area_ft2, Width = 0, InitSat = 0, FromImp = 50, ToPerv = 1)
  bs_lid_1RB_150 <- rbind (bs_lid_1RB_150, bs_lid_1RB_150_pre [[i]])
  
  bs_lid_2RB_150_pre [[i]] <- bs_sub_RB_cat[[i]] %>% select(Name) %>% rename(Subcatchment = Name) %>%
    mutate(`LID Process` = bs_RB_150, Number = 2 * i, Area = bs_RB_150_area_ft2, Width = 0, InitSat = 0, FromImp = 50, ToPerv = 1)
  bs_lid_2RB_150 <- rbind (bs_lid_2RB_150, bs_lid_2RB_150_pre [[i]])
  
  bs_lid_1RB_1000_pre [[i]] <- bs_sub_RB_cat[[i]] %>% select(Name) %>% rename(Subcatchment = Name) %>%
    mutate(`LID Process` = bs_RB_1000, Number = 1 * i, Area = bs_RB_1000_area_ft2, Width = 0, InitSat = 0, FromImp = 50, ToPerv = 1)
  bs_lid_1RB_1000 <- rbind (bs_lid_1RB_1000, bs_lid_1RB_1000_pre [[i]])
  
}

bs_lid_RB <- list()
# Scenario 1 - 6  (RB 75 gallon) ..........
bs_lid_RB [[1]] <- bs_lid_1RB_75; 
bs_lid_RB [[2]] <- filter(bs_lid_1RB_75, grepl("HDC", Subcatchment)); 
bs_lid_RB [[3]] <- filter(bs_lid_1RB_75, grepl("HLawn", Subcatchment)); 
bs_lid_RB [[4]] <- filter(bs_lid_1RB_75, grepl("HSplash", Subcatchment)); 
bs_lid_RB [[5]] <- filter(bs_lid_1RB_75, grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment)); 
bs_lid_RB [[6]] <- filter(bs_lid_1RB_75, grepl("HDC", Subcatchment) | grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment));

# Scenario 7 - 12  (RB 150 gallon) ..........
bs_lid_RB [[7]]  <- bs_lid_1RB_150; 
bs_lid_RB [[8]]  <- filter(bs_lid_1RB_150, grepl("HDC", Subcatchment)); 
bs_lid_RB [[9]]  <- filter(bs_lid_1RB_150, grepl("HLawn", Subcatchment)); 
bs_lid_RB [[10]] <- filter(bs_lid_1RB_150, grepl("HSplash", Subcatchment)); 
bs_lid_RB [[11]] <- filter(bs_lid_1RB_150, grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment)); 
bs_lid_RB [[12]] <- filter(bs_lid_1RB_150, grepl("HDC", Subcatchment) | grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment));

# Scenario 13 - 18  (2 RB 150 gallon) ..........
bs_lid_RB [[13]] <- bs_lid_2RB_150; 
bs_lid_RB [[14]] <- filter(bs_lid_2RB_150, grepl("HDC", Subcatchment)); 
bs_lid_RB [[15]] <- filter(bs_lid_2RB_150, grepl("HLawn", Subcatchment)); 
bs_lid_RB [[16]] <- filter(bs_lid_2RB_150, grepl("HSplash", Subcatchment)); 
bs_lid_RB [[17]] <- filter(bs_lid_2RB_150, grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment)); 
bs_lid_RB [[18]] <- filter(bs_lid_2RB_150, grepl("HDC", Subcatchment) | grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment));

# Scenario 19 - 24  (RB 1000 gallon) ..........
bs_lid_RB [[19]] <- bs_lid_1RB_1000; 
bs_lid_RB [[20]] <- filter(bs_lid_1RB_1000, grepl("HDC", Subcatchment)); 
bs_lid_RB [[21]] <- filter(bs_lid_1RB_1000, grepl("HLawn", Subcatchment)); 
bs_lid_RB [[22]] <- filter(bs_lid_1RB_1000, grepl("HSplash", Subcatchment)); 
bs_lid_RB [[23]] <- filter(bs_lid_1RB_1000, grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment)); 
bs_lid_RB [[24]] <- filter(bs_lid_1RB_1000, grepl("HDC", Subcatchment) | grepl("HStreet", Subcatchment) | grepl("Hstreet", Subcatchment));

bs_lid_RB_new <- list()
for ( i in 1: bs_number) {
  bs_lid_RB_new [[i]] <- bs_lid_usage %>% full_join(bs_lid_RB[[i]] , by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
}

    # ----- 1.1.2: Building RB Scenarios -----

# Building Scenarios 1 - 24 related to the Rain Barrels 
bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

for ( i in 1: bs_number) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_new[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[i])
}

  # ----- 1.2: Downspout Disconnection (DSD) (Scenario 25) -----
  
    # ----- 1.2.1: Preparation -----

bs_sub_HDC_outlet <- bs_subcatchment_inp %>% 
  filter (grepl("HDC", Name)) %>% 
  rename(Name_HDC = Name, Outlet_HDC = Outlet) %>% 
  select (Name_HDC, Outlet_HDC) %>% mutate (Name_common = substr(Name_HDC, 5, 100))

bs_sub_HDC_outlet_2 <- bs_sub_HDC_outlet  %>% mutate(Name = paste0("SAperv_", Name_common))
bs_sub_HDC_outlet_3 <- bs_subcatchment_inp %>% inner_join(bs_sub_HDC_outlet_2, by = c("Name")) %>% select (Name_HDC, Outlet_HDC, Name) %>%  rename (Outlet = Name)
bs_HDC_new_outlet_pre <- bs_sub_HDC_outlet_3 %>% select(Name_HDC, Outlet) %>% rename(Name = Name_HDC)

bs_HDC_new_outlet <- bs_HDC_new_outlet_pre %>%
  inner_join(bs_subcatchment_inp, by = c("Name"))%>%
  select(-Outlet.y) %>%
  rename(Outlet = Outlet.x)%>%
  select (Name, `Rain Gage`, Outlet, Area, Perc_Imperv, Width, Perc_Slope, CurbLen, Snowpack)

bs_subcatchment_DSD_pre <- bs_subcatchment_inp
bs_subcatchment_DSD_pre[match(bs_HDC_new_outlet$Name, bs_subcatchment_DSD_pre$Name ), ] <- bs_HDC_new_outlet
bs_subcatchment_DSD <- bs_subcatchment_DSD_pre

    # ----- 1.2.2: Building DSD Scenario -----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

bs_file_for_write_new $ subcatchments <- bs_subcatchment_DSD
write_inp (bs_file_for_write_new, file_inp_scenario_pre[25])

  # ----- 1.3: Cistern (Scenario 26) -----

    # ----- 1.3.1: Preparation -----

# The Commercial roof subcatchments are categorized based on the area. 
# each 2* 1000 m2 (2 * 0.247105 acres) of roofts, is consider to be one house. 
bs_cistern_area_cat <- 0.247105 * 2   # roof area of each house. 

bs_sub_cistern_cat <- list(); bs_lid_1cistern_5000_pre <- list(); bs_lid_1cistern_5000 <- data.frame()
for (i in 1: ceiling (max(bs_sub_cistern$Area)/bs_cistern_area_cat)) {
    bs_sub_cistern_cat [[i]] <- bs_sub_cistern %>% filter ( Area >= bs_cistern_area_cat * (i-1) , Area < bs_cistern_area_cat * i )
    bs_lid_1cistern_5000_pre [[i]] <- bs_sub_cistern_cat[[i]] %>% select(Name) %>% rename(Subcatchment = Name) %>%
      mutate(`LID Process` = bs_cistern_5000, Number = 1 * i, Area = bs_cistern_5000_area_ft2, Width = 0, InitSat = 0, FromImp = 50, ToPerv = 1)
    bs_lid_1cistern_5000 <- rbind (bs_lid_1cistern_5000, bs_lid_1cistern_5000_pre [[i]])
}

bs_lid_cistern_new <- bs_lid_usage %>% 
  full_join(bs_lid_1cistern_5000, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))

    # ----- 1.3.2: Building Cistern Scenarios -----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

bs_file_for_write_new $ lid_usage <- bs_lid_cistern_new
write_inp (bs_file_for_write_new, file_inp_scenario_pre[26])

  # ----- 1.4: DSD + RB (Scenario 27 - 40)----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct
bs_file_for_write_new $ subcatchments <- bs_subcatchment_DSD

bs_lid_RB_new_2 <- list()
bs_lid_RB_new_2[[27]] <- bs_lid_RB_new[[1]]
bs_lid_RB_new_2[[28]] <- bs_lid_RB_new[[4]]
bs_lid_RB_new_2[[29]] <- bs_lid_RB_new[[5]]
bs_lid_RB_new_2[[30]] <- bs_lid_RB_new[[7]]
bs_lid_RB_new_2[[31]] <- bs_lid_RB_new[[10]]
bs_lid_RB_new_2[[32]] <- bs_lid_RB_new[[11]]
bs_lid_RB_new_2[[33]] <- bs_lid_RB_new[[13]]
bs_lid_RB_new_2[[34]] <- bs_lid_RB_new[[14]]
bs_lid_RB_new_2[[35]] <- bs_lid_RB_new[[16]]
bs_lid_RB_new_2[[36]] <- bs_lid_RB_new[[17]]
bs_lid_RB_new_2[[37]] <- bs_lid_RB_new[[19]]
bs_lid_RB_new_2[[38]] <- bs_lid_RB_new[[20]]
bs_lid_RB_new_2[[39]] <- bs_lid_RB_new[[22]]
bs_lid_RB_new_2[[40]] <- bs_lid_RB_new[[23]]

for ( i in 27:40) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_new_2[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[i])
}

  # ----- 1.5: Cistern + RB (Scenario 41 - 51)----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

bs_lid_RB_cistern_new <- list()
bs_lid_RB_cistern_new[[41]] <- bs_lid_RB_new [[2]]  %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[42]] <- bs_lid_RB_new [[6]]  %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[43]] <- bs_lid_RB_new [[7]]  %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[44]] <- bs_lid_RB_new [[12]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[45]] <- bs_lid_RB_new [[13]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[46]] <- bs_lid_RB_new [[14]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[47]] <- bs_lid_RB_new [[18]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[48]] <- bs_lid_RB_new [[19]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[49]] <- bs_lid_RB_new [[20]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[50]] <- bs_lid_RB_new [[22]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[51]] <- bs_lid_RB_new [[24]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))

for (i in 41:51) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_cistern_new[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[i])
}

  # ----- 1.6: DSD + Cistern + RB (Scenario 52 - 64)----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct
bs_file_for_write_new $ subcatchments <- bs_subcatchment_DSD

bs_lid_RB_cistern_new <- list()
bs_lid_RB_cistern_new[[52]] <- bs_lid_RB_new [[1]]  %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[53]] <- bs_lid_RB_new [[7]]  %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[54]] <- bs_lid_RB_new [[13]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[55]] <- bs_lid_RB_new [[14]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[56]] <- bs_lid_RB_new [[16]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[57]] <- bs_lid_RB_new [[17]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[58]] <- bs_lid_RB_new [[18]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[59]] <- bs_lid_RB_new [[19]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[60]] <- bs_lid_RB_new [[20]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[61]] <- bs_lid_RB_new [[22]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[62]] <- bs_lid_RB_new [[23]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[63]] <- bs_lid_RB_new [[24]] %>% full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
bs_lid_RB_cistern_new[[64]] <- bs_lid_cistern_new


for (i in 52:64) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_cistern_new[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[i])
}

# ----- 2: Functions -----

  # ----- 2.0: Initial information -----

    # ----- 2.0.1: Load input file -----

Input_file <- read_inp( file_inp_scenario[2] )
inp_options <- Input_file$options

    # ----- 2.0.2: Time steps -----

# Find the report time step (minutes)
option_report_step <-  (inp_options %>% filter(Option == "REPORT_STEP") %>% select (Value))[[1]]
time_step_min <- as.numeric(substr (option_report_step, 1,2)) * 60 + as.numeric(substr (option_report_step [[1]], 4,5))     # minutes
# time_step_min <- 15

# Find the model running duration time based on day
option_start_date <- (inp_options %>% filter(Option == "REPORT_START_DATE")  %>% select (Value))[[1]]
option_end_date <- (inp_options %>% filter(Option == "END_DATE")  %>% select (Value) )[[1]]
option_report_time_day <- data.frame(Start = option_start_date, End = option_end_date)
option_report_time_day$Duration = as.Date(as.character(option_report_time_day$End), format = "%m/%d/%Y") - 
  as.Date(as.character(option_report_time_day$Start), format = "%m/%d/%Y")  
time_report_duration <- as.numeric ( option_report_time_day$Duration )
time_run_day <- time_report_duration
# time_run_day <- 365   # Running time of the model (days)

    # ----- 2.0.3: Threshold values -----

cr_time_hr <- 12      # Minimum 12 hours between two activation
cr_flow_mgd <- 0.1    # Flow threshold (mgd)
cr_vol_mg <- 0.01     # Volume threshold (mg)

    # ----- 2.0.4: Coefficient -----

convert_flowmgd_volmg <- 1/(24*60)   # to convert flow (mgd) to volume (mg)
coef_flow_vol <- convert_flowmgd_volmg * time_step_min
coef_flow_vol_viceversa <- 1 / coef_flow_vol
coef_time <- time_step_min * 60   # time steps (seconds)
coef_cr_time <- cr_time_hr * 60 / time_step_min
coef_it_ts <- time_run_day*24*60 / time_step_min
coef_digitnumber <- 3      # digit number for values


  # ----- 2.1: Water Ballance Function -----

fun_water_ballance <- function(design_scenario) {
  
  s_report_file <- data.frame(); s_surface_runoff_continuity <- data.frame()
  s_report_file <- read_rpt (design_scenario)
  
  # ................. Surface Runoff Continuity ....................
  s_rpt_runoff_quan_cont_raw <- s_report_file $ runoff_quantity_continuity
  s_rpt_runoff_quan_cont_raw [nrow(s_rpt_runoff_quan_cont_raw),"Volume"] <- s_rpt_runoff_quan_cont_raw [nrow(s_rpt_runoff_quan_cont_raw),"Depth"]     
  s_rpt_runoff_quan_cont_raw [nrow(s_rpt_runoff_quan_cont_raw),"Depth"] <- 0
  s_rpt_runoff_quan_cont <- s_rpt_runoff_quan_cont_raw %>% mutate("Volume (Million gallons)" = round (Volume * 325851 / 1000000 , digits = 3) ) %>%
    rename("Volume (acre-feet)" = Volume, "Depth (inch)" = Depth)
  
  s_surface_runoff_continuity <- data.frame(Initial_LID_storage_mg = s_rpt_runoff_quan_cont [[1,"Volume (Million gallons)"]], 
                                            Total_precipitation_mg = s_rpt_runoff_quan_cont [[2,"Volume (Million gallons)"]], 
                                            Evaporation_loss_mg    = s_rpt_runoff_quan_cont [[3,"Volume (Million gallons)"]],
                                            Infiltration_loss_mg   = s_rpt_runoff_quan_cont [[4,"Volume (Million gallons)"]],
                                            Surface_runoff_mg      = s_rpt_runoff_quan_cont [[5,"Volume (Million gallons)"]], 
                                            LID_drainage_mg        = s_rpt_runoff_quan_cont [[6,"Volume (Million gallons)"]],
                                            Final_storage_mg       = s_rpt_runoff_quan_cont [[7,"Volume (Million gallons)"]] 
  )

  
  # ................. Groundwater Continuity .......................
  s_rpt_groundwater_cont_raw <- s_report_file $ groundwater_continuity
  s_rpt_groundwater_cont_raw [nrow(s_rpt_groundwater_cont_raw),"Volume"] <- s_rpt_groundwater_cont_raw [nrow(s_rpt_groundwater_cont_raw),"Depth"]  
  s_rpt_groundwater_cont_raw [nrow(s_rpt_groundwater_cont_raw),"Depth"] <- 0
  s_rpt_groundwater_cont <- s_rpt_groundwater_cont_raw %>% mutate("Volume (Million gallons)" = round (Volume * 325851 / 1000000 , digits = 3) ) %>%
    rename("Volume (acre-feet)" = Volume, "Depth (inch)" = Depth)
  
  s_groundwater_continuity <- data.frame(Initial_storage_mg  = s_rpt_groundwater_cont [[1,"Volume (Million gallons)"]],
                                         Infiltration_mg     = s_rpt_groundwater_cont [[2,"Volume (Million gallons)"]],
                                         Upper_zone_mg       = s_rpt_groundwater_cont [[3,"Volume (Million gallons)"]],
                                         Lower_zone_mg       = s_rpt_groundwater_cont [[4,"Volume (Million gallons)"]],
                                         Deep_percolation_mg = s_rpt_groundwater_cont [[5,"Volume (Million gallons)"]], 
                                         Groundwater_flow_mg = s_rpt_groundwater_cont [[6,"Volume (Million gallons)"]], 
                                         Final_storage_mg    = s_rpt_groundwater_cont [[7,"Volume (Million gallons)"]]
  )
  
  # ................. Flow Routing Continuity .......................
  s_rpt_flow_rout_cont_raw <- s_report_file $ flow_routing_continuity
  s_rpt_flow_rout_cont_raw [nrow(s_rpt_flow_rout_cont_raw),"Volume_a"] <- s_rpt_flow_rout_cont_raw [nrow(s_rpt_flow_rout_cont_raw),"Volume_b"]  
  s_rpt_flow_rout_cont_raw [nrow(s_rpt_flow_rout_cont_raw),"Volume_b"] <- 0
  s_rpt_flow_rout_cont<- s_rpt_flow_rout_cont_raw %>% rename("Volume (acre-feet)" = Volume_a, "Volume (Million gallons)" = Volume_b)
  
  s_flow_routing_continuity <- data.frame(Dry_weather_inflow_mg    = s_rpt_flow_rout_cont [[1,"Volume (Million gallons)"]], 
                                          Wet_weather_inflow_mg    = s_rpt_flow_rout_cont [[2,"Volume (Million gallons)"]], 
                                          Groundwater_inflow_mg    = s_rpt_flow_rout_cont [[3,"Volume (Million gallons)"]],
                                          RDII_inflow              = s_rpt_flow_rout_cont [[4,"Volume (Million gallons)"]],
                                          External_inflow_mg       = s_rpt_flow_rout_cont [[5,"Volume (Million gallons)"]], 
                                          External_outflow_mg      = s_rpt_flow_rout_cont [[6,"Volume (Million gallons)"]],
                                          Flooding_loss_mg         = s_rpt_flow_rout_cont [[7,"Volume (Million gallons)"]],
                                          Evaporation_loss_mg      = s_rpt_flow_rout_cont [[8,"Volume (Million gallons)"]],
                                          Exfiltration_loss_mg     = s_rpt_flow_rout_cont [[9,"Volume (Million gallons)"]],
                                          Initial_stored_volume_mg = s_rpt_flow_rout_cont [[10,"Volume (Million gallons)"]],
                                          Final_stored_volume_mg   = s_rpt_flow_rout_cont [[11,"Volume (Million gallons)"]]
  )
  
  # ............................ Ouput ..............................
  fun_water_ballance_output <- list( 
    surface_runoff_continuity = s_surface_runoff_continuity ,
    groundwater_continuity = s_groundwater_continuity, 
    flow_routing_continuity = s_flow_routing_continuity
  )
  
}


  # ----- 2.2: LID Number and Cost Function  -----

bs_RB_75_cost <- 120 * 1.1
bs_RB_150_cost <- 250 * 1.1
bs_RB_1000_cost <- 700 * 1.1
bs_cistern_5000_cost <- 2500 * 1.1
bs_DSD_cost <- 30

bs_RB_75 <- c("Rain_Barrel_75gal")
bs_RB_150 <- c("Rain_Barrel_150gal")
bs_RB_1000 <- c("Rain_Barrel_1000gal")
bs_cistern_5000 <- c("Cistern_5000gal")

fun_LID_number_cost <- function(design_scenario) {
  
  s_inp_file <- data.frame(); 
  s_inp_file <- read_inp (design_scenario)
  
  # To find cost of LIDs ............
  s_inp_lid <- s_inp_file $ lid_usage
  
  s_inp_lid_RB_75 <- filter( s_inp_lid, grepl(bs_RB_75, `LID Process` ) )
  s_inp_lid_RB_150 <- filter( s_inp_lid, grepl(bs_RB_150, `LID Process` ) )
  s_inp_lid_RB_1000 <- filter( s_inp_lid, grepl(bs_RB_1000, `LID Process` ) )
  s_inp_lid_cistern_5000 <- filter( s_inp_lid, grepl(bs_cistern_5000, `LID Process` ) )
  
  s_LID_number <- sum(s_inp_lid_RB_75$Number) + sum(s_inp_lid_RB_150$Number) + sum(s_inp_lid_RB_1000$Number) + sum(s_inp_lid_cistern_5000$Number)  
  s_LID_cost   <- ( sum(s_inp_lid_RB_75$Number)   * bs_RB_75_cost   + sum(s_inp_lid_RB_150$Number)       * bs_RB_150_cost + 
                    sum(s_inp_lid_RB_1000$Number) * bs_RB_1000_cost + sum(s_inp_lid_cistern_5000$Number) * bs_cistern_5000_cost ) / 1000000
  
  # To find cost of DSD ............
  s_inp_sub <- s_inp_file $ subcatchments
  s_inp_sub_DSD   <- filter (s_inp_sub,  grepl ("HDC", Name) & grepl("SAperv", Outlet) ) 

  if ( nrow(s_inp_sub_DSD) >=1 ) {
    
    bs_RB_area_cat    <- 0.0459616   # roof area of each house. 
    bs_area_min_ac_RB <- 0.002471 # Minimum area of each house of implementing LID
    s_sub_DSD <- s_inp_sub %>% filter ( grepl("HDC", Name) ) %>% filter (Area >= bs_area_min_ac_RB)
    
    s_sub_DSD_cat <- list(); s_lid_DSD_pre <- list(); s_lid_DSD <- data.frame()
    for (i in 1: ceiling (max(s_sub_DSD$Area)/bs_RB_area_cat)) {
      s_sub_DSD_cat [[i]] <- s_sub_DSD %>% filter ( Area >= bs_RB_area_cat * (i-1) , Area < bs_RB_area_cat * i )
      s_lid_DSD_pre [[i]] <- s_sub_DSD_cat[[i]] %>% select(Name) %>% rename(Subcatchment = Name) %>% mutate(Number = 1 * i)
      s_lid_DSD <- rbind (s_lid_DSD, s_lid_DSD_pre [[i]])
    }
    
    s_DSD_number <- sum ( s_lid_DSD $ Number )
    s_DSD_cost   <- s_DSD_number * bs_DSD_cost / 1000000
  } else {
    s_DSD_number <- 0 
    s_DSD_cost   <- 0
  }                  
  
  # The function's output ..........
  fun_LID_number_cost_output <- list(
    LID_number    = s_LID_number,
    LID_cost      = s_LID_cost, 
    DSD_number    = s_DSD_number, 
    DSD_cost      = s_DSD_cost, 
    Scenario_cost = s_LID_cost + s_DSD_cost
    )
}


  # ----- 2.3: Link Activation Function -----

# Ask user to upload this file: First column: link in the model. Second column: link name 
source_link <- read.csv ("SourceLink.csv")   
link_name <- source_link; link_name$Link <- as.character(link_name $Link); link_name$Name <- as.character(link_name $Name)

link_ts_prep <- link_name %>% mutate (iType = 2, object_name = Link, vIndex = 0) %>%  select(iType, object_name, vIndex)
link_it_num <- nrow(link_name)

# Link Activation Function is started: ........................................................
# .............................................................................................

fun_activation_link <- function (design_scenario) {
  
  # To read the .out file and extract the links time series
  link_ts_list = list()
  for (i in 1:  link_it_num) {
    link_ts_list[link_ts_prep$object_name[i]]  <- read_out(
      file = design_scenario,
      iType = link_ts_prep$iType[i], 
      object_name = link_ts_prep$object_name[i], 
      vIndex = link_ts_prep$vIndex[i]
    )
  }
  
  # To prepare for finding link activation .........................
  link_flow_max <- matrix()
  link_flow_min <- matrix()
  link_flow_average <- matrix()
  link_volume_sum <- matrix()  
  link_act_volume_sum <- matrix()
  link_act_num <- matrix()
  link_no_act <- data.frame()
  link_activation_all <- list()
  link_activation_all_2 <- list()
  # link_activation_all_plot <- list()
  # link_activation_all_2_plot <- list()
  link_flow_all <- data.frame()
  link_volume_all <- data.frame()
  
  for (k in 1: link_it_num ) {
    
    link_flow <- data.frame()
    link_volume <- data.frame()
    link_flow_volume <- data.frame()
    
    link_flow <- link_ts_list [[k]] [["flow_rate"]]
    link_flow_all  <- cbind (link_flow_all, link_flow[,1])
    
    link_flow_max [k] <- round( max (link_flow), digits = coef_digitnumber)
    link_flow_min [k] <- round( min (link_flow), digits = coef_digitnumber)
    link_flow_average [k] <- round( mean (link_flow), digits = coef_digitnumber)
    
    link_volume <- na.fill (((lag(link_flow)+link_flow)/2)*coef_flow_vol, fill=0)
    link_volume_2 <- link_volume
    link_volume [link_volume < 0] <- 0
    link_volume_all <- cbind (link_volume_all, link_volume[,1])
    link_volume_sum [k] <- round (sum(coredata(link_volume)), digits = coef_digitnumber)
    link_flow_volume  <-  cbind (Flow_mgd = link_flow, Volume_mg = link_volume)
    
    if (is.na (sum(link_flow) == TRUE) ) {
      print ( paste ( k, ": WARNING. The time seires has a problem. No number (NA) is shown in some time steps in the time series") )
      link_act_num [k] <- NA
      link_act_volume_sum [k] <- NA
      next
    }
    
    # to find the link activation ................ 
    link_flow_cri <- link_flow[coredata(link_flow) >= cr_flow_mgd]
    
    if ( nrow(link_flow_cri) > 0 ) {
      
      # ..........To find the flag 
      link_flag <- data.frame()
      for (i in 1: coef_it_ts) {
        if (coredata(link_flow)[i] > cr_flow_mgd) {
          link_flag <- rbind(link_flag, i)
        }
      }
      names(link_flag)[1] <- "time_step"
      
      # ..........To find the start point of the period
      period_start <- data.frame()
      if (nrow(link_flag)>1) {
        for (j in (nrow(link_flag)-1):1) {
          if (link_flag$time_step [j] != link_flag$time_step [j+1] - 1) {
            period_start <- rbind(period_start, link_flag$time_step[j+1]-1)
          }
        }
        period_start <- rbind(period_start, link_flag$time_step[1]-1)
        names(period_start)[1] <- "start_point"
        period_start <- data.frame(start_point = rev(period_start$start_point))
      } else {
        period_start <- data.frame(start_point = (link_flag$time_step[1]-1) )
      }
      
      if (first(period_start$start_point) == 0) {
        period_start$start_point[1] <- 1
      }
      
      # ..........To find the end point of the period
      period_end <- data.frame()
      if (nrow(link_flag)>1) {
        for (j in 2: (nrow(link_flag))) {
          if (link_flag$time_step [j] != link_flag$time_step [j-1] + 1) {
            period_end <- rbind(period_end, link_flag$time_step [j-1]+1)
          } 
        }
        period_end <- rbind(period_end, link_flag$time_step[j]+1)
        names(period_end)[1] <- "end_point"
      } else {
        period_end <- data.frame(end_point = (link_flag$time_step[1]+1) )
      }
      
      if (last(period_end$end_point) == coef_it_ts+1) {
        period_end$end_point[nrow(period_end)] <- coef_it_ts
      }
      
      # ..........To find the period for flow activation 
      period_total_1 <- data.frame()
      period_total_1 <- cbind(period_start, period_end)
      
      period_total_2 <- data.frame()
      period_total_2 <- period_total_1[1,1:2]
      
      if ( nrow(period_total_1) > 1 ) {
        
        for (n in 1: (nrow(period_total_1)-1)) {
          if (period_total_1 $ end_point[n] + coef_cr_time < period_total_1$start_point[n+1]  ) {
            period_total_2 [n,"end_point"] <- period_total_1[n, "end_point"]
            period_total_2 [n+1,"start_point"] <- period_total_1[n+1, "start_point"]
          }
        }
        period_total_2 [nrow(period_total_1),2] <- period_total_1 [nrow(period_total_1),2]
        
        period_total_3 <- data.frame()
        period_total_3 <- period_total_2 %>% 
          filter(is.na(start_point)==FALSE | is.na(end_point)==FALSE )
        
        period_total_4 <- data.frame()
        period_total_4 <- period_total_3
        for (m in 1: (nrow(period_total_3)-1)) {
          if ( is.na(period_total_3 $ start_point[m+1])  ) {
            period_total_4 [m,"end_point"] <- period_total_3 [m+1, "end_point"]
          } 
        }
        
        period_total <- data.frame()
        period_total <- period_total_4 %>% 
          filter(is.na(start_point)==FALSE)
        
      } else {
        period_total <- data.frame()
        period_total <- period_total_1
      }
      
      period_start_time <- index(link_flow [period_total $ start_point])
      period_end_time <- index(link_flow [period_total $ end_point])
      link_act_period_flow <- data.frame (start_point = period_start_time, end_point = period_end_time)
      
      # ..........TO find the volume and peak flow in the period for flow activation
      link_act_volume <- data.frame()
      link_act_peakflow <- data.frame()
      link_act_peakflow_time_pre <- data.frame()
      link_act_peakflow_time <- data.frame()
      for (i in 1: nrow(period_total)) {
        link_act_volume <- rbind(link_act_volume, round ( (sum( coredata( link_volume[period_total[i,1] : period_total[i,2]]) ) ), digits = coef_digitnumber))
        link_act_peakflow <- rbind(link_act_peakflow, round ( ( max( coredata(  link_flow[period_total[i,1] : period_total[i,2]]) ) ), digits = coef_digitnumber))  
        link_act_peakflow_time_pre <- rbind(link_act_peakflow_time_pre, which(grepl(link_act_peakflow[i,1], round(coredata(link_flow_cri), digits = coef_digitnumber)) ))
      }
      names(link_act_volume)[1] <- "Volume_mg"
      names(link_act_peakflow)[1] <- "PeakFlow_mgd"
      names(link_act_peakflow_time_pre)[1] <- "time_step"
      link_act_peakflow_time <- data.frame(PeakFlow_time = index(link_flow_cri [link_act_peakflow_time_pre $time_step]))
      
      # ..........To build the link activation 
      link_act_pre <- data.frame(Name = link_name[k,2], Link = link_name[k,1], link_act_period_flow, link_act_peakflow, link_act_peakflow_time, link_act_volume)
      link_act_pre$Name <- as.character(link_act_pre$Name)
      link_act_pre$Link <- as.character(link_act_pre$Link)
      
      link_activation <- link_act_pre %>% filter(Volume_mg >= cr_vol_mg)
      
      # # plot ....................
      # link_activation_plot <- 
      #   dygraph(link_flow, main = link_name[k,2], height = 500, width = 750, xlab = "Time", ylab = "Flow (mgd)") %>%
      #   dyRangeSelector(dateWindow = c("1993-01-01", "1994-01-01")) %>%
      #   dyOptions(stackedGraph = TRUE, drawPoints = TRUE, pointSize = 0.25) %>%
      #   dyRangeSelector(height = 30)%>%
      #   dyLimit(cr_flow_mgd, color = "red")
      
      link_act_volume_sum [k] <- sum(link_activation$Volume_mg)
      link_act_num [k] <- nrow(link_activation)
      
      if (is.na(link_activation[1,1])) {
        link_activation <- 0
        # link_activation_plot <- 0
        link_no_act <- rbind(link_no_act, k)
        print( paste(k, ": ", link_name[k,2], "is NOT ACTIVATED") )
      }else {
        print( paste(k, ": ", link_name[k,2], "is ACTIVATED", link_act_num [k], "times") )
      }
      
      
    } else  {
      
      link_no_act <- rbind(link_no_act, k)
      link_act_volume_sum [k] <- 0
      link_act_num [k] <- 0
      
      link_activation <- 0
      # link_activation_plot <- 0
      
      print( paste(k, ": ", link_name[k,2], "is NOT ACTIVATED") )
    }
    
    link_activation_all [[link_name[k,2]]] <- link_activation
    link_activation_all_2 [[k]] <- link_activation
    
    # link_activation_all_plot[[link_name[k,2]]] <- link_activation_plot
    # link_activation_all_2_plot [[k]] <- link_activation_plot
    
  }
  
  link_flow_info <- data.frame(Minimum_flow_mgd = link_flow_min, Average_flow_mgd = link_flow_average, Maximum_flow_mgd = link_flow_max)
  
  # link Activation Analysis .................................
  link_activation_main <- cbind (link_name, Activation = link_act_num, Volume_mg =  round(link_act_volume_sum, digits = coef_digitnumber), 
     Volume_mg_all = round(link_volume_sum, digits = coef_digitnumber), Flow_min_mgd = link_flow_min, Flow_max_mgd = link_flow_max, Flow_avg_mgd = link_flow_average)
  
  if (is.na (sum(link_activation_main$Activation) == TRUE) ) {
    print ( paste ( "In this scenario, some of the links have problem in their flow time series. Therefore, the result is UNRELIABLE") )
  }
 
  link_act_num_2 <- filter(data.frame(C1=1:link_it_num, C2=link_act_num), C2 > 0 )
  link_all <- data.frame()
  link_name_all <- data.frame()
  link_startpoint_all <- data.frame()
  link_endpoint_all <- data.frame()
  link_peakflow_mgd_all <- data.frame()
  link_peakflow_time_all <- data.frame()
  link_volume_mg_all <- data.frame()
  
  for (j in 1 :nrow(link_act_num_2)) {
    link_all <- rbind(link_all, data.frame (Link = link_activation_all_2[[link_act_num_2[j,1]]][["Link"]]) )
    link_name_all <- rbind(link_name_all, data.frame (Name = link_activation_all_2[[link_act_num_2[j,1]]][["Name"]]) )
    link_startpoint_all <- rbind(link_startpoint_all, data.frame (Start_point = link_activation_all_2[[link_act_num_2[j,1]]][["start_point"]]) )
    link_endpoint_all <- rbind(link_endpoint_all, data.frame (end_point = link_activation_all_2[[link_act_num_2[j,1]]][["end_point"]]) )
    link_peakflow_mgd_all <- rbind(link_peakflow_mgd_all, data.frame (Peak_flow_mgd = link_activation_all_2[[link_act_num_2[j,1]]][["PeakFlow_mgd"]]) )
    link_peakflow_time_all <- rbind(link_peakflow_time_all, data.frame (Peak_flow_time = link_activation_all_2[[link_act_num_2[j,1]]][["PeakFlow_time"]]) )
    link_volume_mg_all <- rbind(link_volume_mg_all, data.frame (Volume_mg = link_activation_all_2[[link_act_num_2[j,1]]][["Volume_mg"]]) )
    
  }
  
  link_act_all <- data.frame()
  link_act_all <- cbind(link_all, link_name_all, link_startpoint_all, link_endpoint_all, link_peakflow_mgd_all, link_peakflow_time_all, link_volume_mg_all )
  link_act_all$Link <- as.character(link_act_all$Link)
  link_act_all$Name <- as.character(link_act_all$Name)
  
  
  # Outputs of this function
  fun_activation_link_output <- list(
    Link_Flow_mgd = link_flow_all,
    Link_Volume_mg = link_volume_all, 
    Link_Flow_information = link_flow_info, 
    Link_activation = link_activation_all_2, 
    # Link_activation_plot = link_activation_all_2_plot, 
    Link_activation_main = link_activation_main, 
    Link_activation_all = link_act_all, 
    Link_activation_number = sum(link_act_num),
    Link_activation_volume = sum(link_act_volume_sum),
    Link_total_volume = sum(link_volume_sum) 
  ) 

  
}


  # ----- 2.4: Node Activation Function -----

# Ask user to upload this file: First column: Node in the model. Second column: Node name 
source_node <- read.csv ("SourceNode.csv")   
node_name <- source_node; node_name$Node <- as.character(node_name $Node); node_name$Name <- as.character(node_name $Name)

node_ts_prep <- node_name %>% mutate (iType = 1, object_name = Node, vIndex = 4) %>% select(iType, object_name, vIndex)
node_it_num <- nrow(node_name)

# Node Activation Function is started: ........................................................
# .............................................................................................

fun_activation_node <- function (design_scenario) {
  
  # To read the .out file and extract the nodes time series
  node_ts_list = list()
  for (i in 1:  node_it_num) {
    node_ts_list[node_ts_prep$object_name[i]]  <- read_out(
      file = design_scenario,
      iType = node_ts_prep$iType[i], 
      object_name = node_ts_prep$object_name[i], 
      vIndex = node_ts_prep$vIndex[i]
    )
  }
  
  # To prepare for finding node activation .........................
  node_flow_max <- matrix()
  node_flow_min <- matrix()
  node_flow_average <- matrix()
  node_no_act <- data.frame()
  node_volume_sum <- matrix()
  node_act_volume_sum <- matrix()
  node_act_num <- matrix()
  node_activation_all <- list()
  node_activation_all_2 <- list()
  # node_activation_all_plot <- list()
  # node_activation_all_2_plot <- list()
  node_flow_all <- data.frame()
  node_volume_all <- data.frame()
  
  for (kk in 1: node_it_num ) {
    
    node_flow <- data.frame()
    node_volume <- data.frame()
    node_flowvolume <- data.frame()
    
    node_flow <- node_ts_list [[kk]] [["total_inflow"]] 
    node_flow_all <- cbind (node_flow_all, node_flow[,1])
    
    node_flow_max [kk] <- round( max (node_flow), digits = coef_digitnumber)
    node_flow_min [kk] <- round( min (node_flow), digits = coef_digitnumber)
    node_flow_average [kk] <- round( mean (node_flow), digits = coef_digitnumber)
    
    node_volume <- na.fill (((lag(node_flow)+node_flow)/2)*coef_flow_vol, fill=0)
    node_volume_2 <- node_volume
    node_volume [node_volume < 0] <- 0
    node_volume_all <- cbind (node_volume_all, node_volume[,1])
    node_volume_sum [kk] <- round (sum(coredata(node_volume)), digits = coef_digitnumber)
    node_flowvolume  <-  cbind (Flow_mgd = node_flow, Volume_mg = node_volume)
    
    if (is.na (sum(node_flow) == TRUE) ) {
      print ( paste ( kk, ": WARNING. The time seires has a problem. No number (NA) is shown in some time steps in the time series") )
      node_act_num [kk] <- NA
      node_act_volume_sum [kk] <- NA
      next
    }
    
    # to find the node activation ................ 
    node_flow_cri <- node_flow[coredata(node_flow) >= cr_flow_mgd]
    
    if ( nrow(node_flow_cri) > 0 ) {
      
      # ..........To find the flag 
      node_flag <- data.frame()
      for (i in 1: coef_it_ts) {
        if (coredata(node_flow)[i] > cr_flow_mgd) {
          node_flag <- rbind(node_flag, i)
        }
      }
      names(node_flag)[1] <- "time_step"
      
      # ..........To find the start point of the period
      node_period_start <- data.frame()
      if (nrow(node_flag)>1) {
        for (j in (nrow(node_flag)-1):1) {
          if (node_flag$time_step [j] != node_flag$time_step [j+1] - 1) {
            node_period_start <- rbind(node_period_start, node_flag$time_step[j+1]-1)
          }
        }
        node_period_start <- rbind(node_period_start, node_flag$time_step[1]-1)
        names(node_period_start)[1] <- "start_point"
        node_period_start <- data.frame(start_point = rev(node_period_start$start_point))
      } else {
        node_period_start <- data.frame(start_point = (node_flag$time_step[1]-1) )
      }
      
      if (first(node_period_start$start_point) == 0) {
        node_period_start$start_point[1] <- 1
      }
      
      # ..........To find the end point of the period
      node_period_end <- data.frame()
      if (nrow(node_flag)>1) {
        for (j in 2: (nrow(node_flag))) {
          if (node_flag$time_step [j] != node_flag$time_step [j-1] + 1) {
            node_period_end <- rbind(node_period_end, node_flag$time_step [j-1]+1)
          } 
        }
        node_period_end <- rbind(node_period_end, node_flag$time_step[j]+1)
        names(node_period_end)[1] <- "end_point"
      } else {
        node_period_end <- data.frame(end_point = (node_flag$time_step[1]+1) )
      }
      
      if (last(node_period_end$end_point) == coef_it_ts+1) {
        node_period_end$end_point[nrow(node_period_end)] <- coef_it_ts
      }
      
      # ..........To find the period for flow activation 
      period_total_1 <- data.frame()
      period_total_1 <- cbind(node_period_start, node_period_end)
      
      period_total_2 <- data.frame()
      period_total_2 <- period_total_1[1,1:2]
      
      if ( nrow(period_total_1) > 1 ) {
        
        for (n in 1: (nrow(period_total_1)-1)) {
          if (period_total_1 $ end_point[n] + coef_cr_time < period_total_1$start_point[n+1]  ) {
            period_total_2 [n,"end_point"] <- period_total_1[n, "end_point"]
            period_total_2 [n+1,"start_point"] <- period_total_1[n+1, "start_point"]
          }
        }
        period_total_2 [nrow(period_total_1),2] <- period_total_1 [nrow(period_total_1),2]
        
        period_total_3 <- data.frame()
        period_total_3 <- period_total_2 %>% 
          filter(is.na(start_point)==FALSE | is.na(end_point)==FALSE )
        
        period_total_4 <- data.frame()
        period_total_4 <- period_total_3
        for (m in 1: (nrow(period_total_3)-1)) {
          if ( is.na(period_total_3 $ start_point[m+1])  ) {
            period_total_4 [m,"end_point"] <- period_total_3 [m+1, "end_point"]
          } 
        }
        
        node_period_total <- data.frame()
        node_period_total <- period_total_4 %>% 
          filter(is.na(start_point)==FALSE)
        
      } else {
        node_period_total <- data.frame()
        node_period_total <- period_total_1
      }
      
      period_start_time <- index(node_flow [node_period_total $ start_point])
      period_end_time <- index(node_flow [node_period_total $ end_point])
      node_act_period_flow <- data.frame (start_point = period_start_time, end_point = period_end_time)
      
      # ..........TO find the volume and peak flow in the period for flow activation
      nodeact_volume <- data.frame()
      nodeact_peakflow <- data.frame()
      nodeact_peakflow_time_pre <- data.frame()
      nodeact_peakflow_time <- data.frame()
      for (i in 1: nrow(node_period_total)) {
        nodeact_volume <- rbind(nodeact_volume, round ( (sum( coredata(  node_volume[node_period_total[i,1] : node_period_total[i,2]]) ) ), digits = coef_digitnumber))
        nodeact_peakflow <- rbind(nodeact_peakflow, round ( ( max( coredata(  node_flow[node_period_total[i,1] : node_period_total[i,2]]) ) ), digits = coef_digitnumber))  
        nodeact_peakflow_time_pre <- rbind(nodeact_peakflow_time_pre, which(grepl(nodeact_peakflow[i,1], round(coredata(node_flow_cri), digits = coef_digitnumber)) ))
      }
      names(nodeact_volume)[1] <- "Volume_mg"
      names(nodeact_peakflow)[1] <- "PeakFlow_mgd"
      names(nodeact_peakflow_time_pre)[1] <- "time_step"
      nodeact_peakflow_time <- data.frame(PeakFlow_time = index(node_flow_cri [nodeact_peakflow_time_pre $time_step]))
      
      # ..........To build the node activation 
      node_act_pre <- data.frame(Name=node_name[kk,2], Node=node_name[kk,1], node_act_period_flow, nodeact_peakflow, nodeact_peakflow_time, nodeact_volume)
      node_act_pre$Name <- as.character(node_act_pre$Name)
      node_act_pre$Node <- as.character(node_act_pre$Node)
      
      node_activation <- node_act_pre %>% filter(Volume_mg >= cr_vol_mg)
      
      # # plot ....................
      # node_activation_plot <- 
      #   dygraph(node_flow, main = node_name[kk,2], height = 500, width = 750, xlab = "Time", ylab = "Flow (mgd)") %>%
      #   dyRangeSelector(dateWindow = c("1993-01-01", "1994-01-01")) %>%
      #   dyOptions(stackedGraph = TRUE, drawPoints = TRUE, pointSize = 0.25) %>%
      #   dyRangeSelector(height = 30)%>%
      #   dyLimit(cr_flow_mgd, color = "red")
      
      node_act_volume_sum [kk] <- sum(node_activation$Volume_mg)
      node_act_num [kk] <- nrow(node_activation)
      
      if (is.na(node_activation[1,1])) {
        node_activation <- 0
        # node_activation_plot <- 0
        node_no_act <- rbind(node_no_act, kk)
        print( paste(kk, ": ", node_name[kk,2], "is NOT ACTIVATED") )
      }else {
        print( paste(kk, ": ", node_name[kk,2], "is ACTIVATED", node_act_num [kk], "times") )
      }
      
      
    } else  {
      
      node_no_act <- rbind(node_no_act, kk)
      node_act_volume_sum [kk] <- 0
      node_act_num [kk] <- 0
      
      node_activation <- 0
      # node_activation_plot <- 0
      
      print( paste(kk, ": ", node_name[kk,2], "is NOT ACTIVATED") )
    }
    
    node_activation_all [[node_name[kk,2]]] <- node_activation
    node_activation_all_2 [[kk]] <- node_activation
    
    # node_activation_all_plot[[node_name[kk,2]]] <- node_activation_plot
    # node_activation_all_2_plot [[kk]] <- node_activation_plot
    
  }
  
  node_flow_info <- data.frame (Minimum_flow_mgd = node_flow_min, Average_flow_mgd = node_flow_average, Maximum_flow_mgd = node_flow_max)
  
  # node activation analysis ..............................
  node_activation_main <- data.frame()
  node_activation_main <- cbind(node_name, Activation = node_act_num, Volume_mg =  round(node_act_volume_sum, digits = coef_digitnumber), 
    Volume_mg_all = round(node_volume_sum, digits = coef_digitnumber), Flow_min_mgd = node_flow_min, Flow_max_mgd = node_flow_max, Flow_avg_mgd = node_flow_average)

  if (is.na (sum(node_activation_main$Activation) == TRUE) ) {
    print ( paste ( "In this scenario, some of the nodes have problem in their flow time series. Therefore, the result is UNRELIABLE") )
  }
  
  node_act_num_2 <- filter(data.frame(C1=1:node_it_num, C2=node_act_num), C2 > 0 )
  node_all <- data.frame()
  node_name_all <- data.frame()
  node_startpoint_all <- data.frame()
  node_endpoint_all <- data.frame()
  node_peakflow_mgd_all <- data.frame()
  node_peakflow_time_all <- data.frame()
  node_volume_mg_all <- data.frame()
  
  for (j in 1 :nrow(node_act_num_2)) {
    node_all <- rbind(node_all, data.frame (Node = node_activation_all_2[[node_act_num_2[j,1]]][["Node"]]) )
    node_name_all <-  rbind(node_name_all, data.frame (Name = node_activation_all_2[[node_act_num_2[j,1]]][["Name"]]) ) 
    node_startpoint_all <- rbind(node_startpoint_all, data.frame (Start_point = node_activation_all_2[[node_act_num_2[j,1]]][["start_point"]]) )
    node_endpoint_all <- rbind(node_endpoint_all, data.frame (End_point = node_activation_all_2[[node_act_num_2[j,1]]][["end_point"]]) )
    node_peakflow_mgd_all <- rbind(node_peakflow_mgd_all, data.frame (Peak_flow_mgd = node_activation_all_2[[node_act_num_2[j,1]]][["PeakFlow_mgd"]]) )
    node_peakflow_time_all <- rbind(node_peakflow_time_all, data.frame (Peak_flow_time = node_activation_all_2[[node_act_num_2[j,1]]][["PeakFlow_time"]]) )
    node_volume_mg_all <- rbind(node_volume_mg_all, data.frame (Volume_mg = node_activation_all_2[[node_act_num_2[j,1]]][["Volume_mg"]]) )
    
  }
  
  node_act_all <- data.frame()
  node_act_all <- cbind(node_name_all, node_all, node_startpoint_all, node_endpoint_all, node_peakflow_mgd_all, node_peakflow_time_all, node_volume_mg_all )
  node_act_all$Name <- as.character(node_act_all$Name)
  node_act_all$Node <- as.character(node_act_all$Node)
  
  # Outputs of this function
  fun_activation_node_output <- list( 
    Node_Flow_mgd = node_flow_all,
    Node_Volume_mg = node_volume_all,
    Node_Flow_information = node_flow_info, 
    Node_activation = node_activation_all_2, 
    # Node_activation_plot = node_activation_all_2_plot, 
    Node_activation_main = node_activation_main, 
    Node_activation_all = node_act_all,
    Node_activation_number = sum(node_act_num),
    Node_activation_volume = sum(node_act_volume_sum),
    Node_total_volume = sum(node_volume_sum)
  ) 
  
  # The end of the function ...................
}


# ----- 3: Run the Functions -----

  # ----- 3.1: Run Water Ballance Function -----

s_surface_runoff_continuity <- data.frame();   s_groundwater_continuity <- data.frame(); s_flow_routing_continuity <- data.frame()
for (i in 1:bs_item_all)  {
  print(i)
  fun_water_ballance_result <- fun_water_ballance(file_rpt_scenario[i])
  s_surface_runoff_continuity   <- rbind (s_surface_runoff_continuity, fun_water_ballance_result[["surface_runoff_continuity"]])
  s_groundwater_continuity      <- rbind (s_groundwater_continuity, fun_water_ballance_result[["groundwater_continuity"]])
  s_flow_routing_continuity     <- rbind (s_flow_routing_continuity, fun_water_ballance_result[["flow_routing_continuity"]])
}

s_surface_runoff_continuity_2 <- s_surface_runoff_continuity %>% mutate(Summation = Infiltration_loss_mg+Surface_runoff_mg+Evaporation_loss_mg+LID_drainage_mg+Initial_LID_storage_mg)
s_groundwater_continuity_2 <- s_groundwater_continuity %>% mutate(Summation = Infiltration_mg+Deep_percolation_mg+Groundwater_flow_mg)
s_flow_routing_continuity_2 <- s_flow_routing_continuity %>% mutate(Summation = Dry_weather_inflow_mg+Wet_weather_inflow_mg+External_inflow_mg+Groundwater_inflow_mg-Flooding_loss_mg - External_outflow_mg)

  # ----- 3.2: Run LID Number and Cost Function  -----

s_LID_number <- data.frame(); s_LID_cost <- data.frame(); s_DSD_number <- data.frame(); s_DSD_cost <- data.frame(); s_LID_cost_all <- data.frame() 
for (i in 1:bs_item_all)  {
  print(i)
  fun_LID_number_cost_result <- fun_LID_number_cost (file_inp_scenario[i])
  s_LID_number   <- rbind (s_LID_number, fun_LID_number_cost_result[["LID_number"]])
  s_LID_cost     <- rbind (s_LID_cost, fun_LID_number_cost_result[["LID_cost"]])
  s_DSD_number   <- rbind (s_DSD_number, fun_LID_number_cost_result[["DSD_number"]])
  s_DSD_cost     <- rbind (s_DSD_cost, fun_LID_number_cost_result[["DSD_cost"]])
  s_LID_cost_all <- rbind (s_LID_cost_all, fun_LID_number_cost_result[["Scenario_cost"]])

}
colnames(s_LID_number) <- c("Number"); colnames(s_LID_cost) <- c("Cost_MD");
colnames(s_DSD_number) <- c("Number"); colnames(s_DSD_cost) <- c("Cost_MD");
colnames(s_LID_cost_all) <- c("Cost_MD");


  # ----- 3.3: Run Link Function  -----

link_result <- list()
tic()
for (i in 1 : bs_item_all) {
  print( paste("Link analysis for Scenario",i-1, "is running") )
  link_result [[i]] <- fun_activation_link (file_out_scenario[i])
}
toc()

  # ----- 3.4: Run Node Function  -----

tic()
node_result <- list()
for (i in 1 : bs_item_all) {
  print( paste("Node analysis for Scenario",i-1, "is running") )
  node_result [[i]] <- fun_activation_node (file_out_scenario[i])
}
toc()

# ----- 4: Results -----

  # ----- 4.1: Water Ballance Result -----

s_surface_runoff_continuity_table <- data.frame (Scenario = file_name_scenario , Detail = scenario_detail$Detail , s_surface_runoff_continuity )
s_groundwater_continuity_table    <- data.frame (Scenario = file_name_scenario , Detail = scenario_detail$Detail , s_groundwater_continuity )
s_flow_routing_continuity_table   <- data.frame (Scenario = file_name_scenario , Detail = scenario_detail$Detail , s_flow_routing_continuity )


s_water_ballance_result_compare <- data.frame (
  Scenario              = file_name_scenario , 
  Detail                = scenario_detail $ Detail ,
  Surface_runoff_mg     = s_surface_runoff_continuity $ Surface_runoff_mg,
  Infiltration_loss_mg  = s_surface_runoff_continuity $ Infiltration_loss_mg, 
  Infiltration_mg       = s_groundwater_continuity $ Infiltration_mg, 
  Deep_percolation_mg   = s_groundwater_continuity $ Deep_percolation_mg, 
  Groundwater_flow_mg   = s_groundwater_continuity $ Groundwater_flow_mg, 
  Wet_weather_inflow_mg = s_flow_routing_continuity $ Wet_weather_inflow_mg, 
  Groundwater_inflow_mg = s_flow_routing_continuity $ Groundwater_inflow_mg,
  External_outflow_mg   = s_flow_routing_continuity $ External_outflow_mg
) %>% 
  mutate( D_surface_runoff = round (Surface_runoff_mg - Surface_runoff_mg[1], digits = 3), 
          D_Infiltration_loss = round (Infiltration_loss_mg - Infiltration_loss_mg[1], digits = 3),
          D_Infiltration = round (Infiltration_mg - Infiltration_mg[1], digits = 3), 
          D_Deep_percolation = round (Deep_percolation_mg - Deep_percolation_mg[1], digits = 3), 
          D_Groundwater_flow = round (Groundwater_flow_mg - Groundwater_flow_mg[1], digits = 3),  
          D_Wet_weather_inflow = round (Wet_weather_inflow_mg - Wet_weather_inflow_mg[1], digits = 3),  
          D_Groundwater_inflow = round (Groundwater_inflow_mg - Groundwater_inflow_mg[1], digits = 3), 
          D_External_outflow = round (External_outflow_mg - External_outflow_mg[1], digits = 3)
          
  ) %>%
  select (Scenario, Detail, Surface_runoff_mg, D_surface_runoff, Infiltration_loss_mg, D_Infiltration_loss, Infiltration_mg, D_Infiltration, 
          Deep_percolation_mg, D_Deep_percolation, Groundwater_flow_mg, D_Groundwater_flow, Wet_weather_inflow_mg, D_Wet_weather_inflow, Groundwater_inflow_mg, D_Groundwater_inflow, 
          External_outflow_mg, D_External_outflow)

  # ----- 4.2: Link Result -----

s_link_flow_min_pre <- list(); s_link_flow_max_pre <- list(); s_link_flow_avg_pre <- list(); 
s_link_activation_pre <- list(); s_link_volume_pre <- list(); s_link_volume_all_pre <- list()
s_link_volume_sum <- data.frame(); s_link_activation_sum <- data.frame(); s_link_volume_all_sum <- data.frame()
for (i in 1:bs_item_all) {
  
  s_link_flow_min_pre [[i]] <- link_result[[i]][["Link_Flow_information"]]$Minimum_flow_mgd   
  s_link_flow_max_pre [[i]] <- link_result[[i]][["Link_Flow_information"]]$Maximum_flow_mgd  
  s_link_flow_avg_pre [[i]] <- link_result[[i]][["Link_Flow_information"]]$Average_flow_mgd  
  
  s_link_volume_pre     [[i]] <- link_result[[i]][["Link_activation_main"]]$Volume_mg            
  s_link_volume_all_pre [[i]] <- link_result[[i]][["Link_activation_main"]]$Volume_mg_all    
  s_link_activation_pre [[i]] <- link_result[[i]][["Link_activation_main"]]$Activation        
  
  s_link_volume_sum     <- rbind (s_link_volume_sum,     link_result[[i]][["Link_activation_volume"]] )              
  s_link_volume_all_sum <- rbind (s_link_volume_all_sum, link_result[[i]][["Link_total_volume"]] )            
  s_link_activation_sum <- rbind (s_link_activation_sum, link_result[[i]][["Link_activation_number"]] )       
  
}
colnames(s_link_volume_sum) <- c("Volume_mg"); colnames(s_link_volume_all_sum) <- c("Volume_mg"); colnames(s_link_activation_sum) <- c("Activation"); 

s_link_flow_min <- as.data.frame(s_link_flow_min_pre) ; names(s_link_flow_min) <- file_name_scenario            
s_link_flow_max <- as.data.frame(s_link_flow_max_pre) ; names(s_link_flow_max) <- file_name_scenario              
s_link_flow_avg <- as.data.frame(s_link_flow_avg_pre) ; names(s_link_flow_avg) <- file_name_scenario              

s_link_volume     <- as.data.frame(s_link_volume_pre)     ; names(s_link_volume) <- file_name_scenario                
s_link_volume_all <- as.data.frame(s_link_volume_all_pre) ; names(s_link_volume_all) <- file_name_scenario         
s_link_activation <- as.data.frame(s_link_activation_pre) ; names(s_link_activation) <- file_name_scenario        

# Comparable table ......................................................
s_link_result <- data.frame ( Scenario      = file_name_scenario , 
                              Detail        = scenario_detail $ Detail , 
                              Activation    = s_link_activation_sum $ Activation,
                              Volume_mg     = s_link_volume_sum $ Volume_mg ,
                              Volume_all_mg = s_link_volume_all_sum $ Volume_mg ,
                              Cost_MD       = s_LID_cost_all $ Cost_MD,
                              Surface_runoff_mg     = s_surface_runoff_continuity $ Surface_runoff_mg,
                              Infiltration_mg       = s_groundwater_continuity $ Infiltration_mg, 
                              Wet_weather_inflow_mg = s_flow_routing_continuity $ Wet_weather_inflow_mg, 
                              Groundwater_inflow_mg = s_flow_routing_continuity $ Groundwater_inflow_mg, 
                              External_outflow_mg   = s_flow_routing_continuity $ External_outflow_mg
)

s_link_result_compare <- s_link_result %>% mutate(
  D_Act = Activation - Activation[1], P_D_Act = round ((100* D_Act / Activation[1]), digits = 2), 
  D_Volume = round (Volume_mg - Volume_mg[1], digits = 3), P_D_Volume =  round ((100* D_Volume / Volume_mg[1]), digits = 2),
  D_Volume_all = round (Volume_all_mg - Volume_all_mg[1], digits = 3), P_D_Volume_all =  round ((100* D_Volume_all / Volume_all_mg[1]), digits = 2),
  `Cost_eff ($/1000gal)` = round (- Cost_MD *1000 / D_Volume, digits = 1),
  D_surface_runoff = round (Surface_runoff_mg - Surface_runoff_mg[1], digits = 3), P_D_surface_runoff =  round ((100* D_surface_runoff / Surface_runoff_mg[1]), digits = 2),
  D_Infiltration = round (Infiltration_mg - Infiltration_mg[1], digits = 3), P_D_Infiltration =  round ((100* D_Infiltration / Infiltration_mg[1]), digits = 2),
  D_Groundwater_inflow = round (Groundwater_inflow_mg - Groundwater_inflow_mg[1], digits = 3), 
  P_D_Groundwater_inflow =  round ((100* D_Groundwater_inflow / Groundwater_inflow_mg[1]), digits = 2) ) %>%
  select(Scenario, Detail, D_Act, P_D_Act, D_Volume, P_D_Volume, D_Volume_all, P_D_Volume_all, Cost_MD, `Cost_eff ($/1000gal)`, 
         D_surface_runoff, P_D_surface_runoff, D_Infiltration, P_D_Infiltration, D_Groundwater_inflow, P_D_Groundwater_inflow)


  # ----- 4.3: Node Result -----

s_node_flow_min_pre <- list(); s_node_flow_max_pre <- list(); s_node_flow_avg_pre <- list(); 
s_node_activation_pre <- list(); s_node_volume_pre <- list(); s_node_volume_all_pre <- list()
s_node_volume_sum <- data.frame(); s_node_activation_sum <- data.frame(); s_node_volume_all_sum <- data.frame()
for (i in 1:bs_item) {
  
  s_node_flow_min_pre [[i]] <- node_result[[i]][["Node_Flow_information"]]$Minimum_flow_mgd   
  s_node_flow_max_pre [[i]] <- node_result[[i]][["Node_Flow_information"]]$Maximum_flow_mgd  
  s_node_flow_avg_pre [[i]] <- node_result[[i]][["Node_Flow_information"]]$Average_flow_mgd  
  
  s_node_volume_pre [[i]] <- node_result[[i]][["Node_activation_main"]]$Volume_mg            
  s_node_volume_all_pre [[i]] <- node_result[[i]][["Node_activation_main"]]$Volume_mg_all    
  s_node_activation_pre [[i]] <- node_result[[i]][["Node_activation_main"]]$Activation        
  
  s_node_volume_sum     <- rbind (s_node_volume_sum,     node_result[[i]][["Node_activation_volume"]] )              
  s_node_volume_all_sum <- rbind (s_node_volume_all_sum, node_result[[i]][["Node_total_volume"]] )            
  s_node_activation_sum <- rbind (s_node_activation_sum, node_result[[i]][["Node_activation_number"]] )       
  
}
colnames(s_node_volume_sum) <- c("Volume_mg"); colnames(s_node_volume_all_sum) <- c("Volume_mg"); colnames(s_node_activation_sum) <- c("Activation"); 

s_node_flow_min <- as.data.frame(s_node_flow_min_pre) ; names(s_node_flow_min) <- file_name_scenario [1:bs_item]            
s_node_flow_max <- as.data.frame(s_node_flow_max_pre) ; names(s_node_flow_max) <- file_name_scenario [1:bs_item]             
s_node_flow_avg <- as.data.frame(s_node_flow_avg_pre) ; names(s_node_flow_avg) <- file_name_scenario [1:bs_item]            

s_node_volume <- as.data.frame(s_node_volume_pre) ; names(s_node_volume) <- file_name_scenario [1:bs_item]                     
s_node_volume_all <- as.data.frame(s_node_volume_all_pre) ; names(s_node_volume_all) <- file_name_scenario [1:bs_item]        
s_node_activation <- as.data.frame(s_node_activation_pre) ; names(s_node_activation) <- file_name_scenario [1:bs_item]          

# Comparable table ......................................................
s_node_result <- data.frame (Scenario      = file_name_scenario [1:bs_item] , 
                             Detail        = scenario_detail$Detail [1:bs_item] , 
                             Activation    = s_node_activation_sum$Activation,
                             Volume_mg     = s_node_volume_sum$Volume_mg ,
                             Volume_all_mg = s_node_volume_all_sum$Volume_mg ,
                             Cost_MD       = s_LID_cost$Cost_MD,
                             Surface_runoff_mg     = s_surface_runoff_continuity $ Surface_runoff_mg,
                             Infiltration_mg       = s_groundwater_continuity $ Infiltration_mg, 
                             Wet_weather_inflow_mg = s_flow_routing_continuity $ Wet_weather_inflow_mg, 
                             Groundwater_inflow_mg = s_flow_routing_continuity $ Groundwater_inflow_mg, 
                             External_outflow_mg   = s_flow_routing_continuity $ External_outflow_mg
)

s_node_result_compare <- s_node_result %>%
  mutate(D_Act = Activation - Activation[1], P_D_Act = round ((100* D_Act / Activation[1]), digits = 2) ) %>%
  mutate(D_Volume = round (Volume_mg - Volume_mg[1], digits = 3), P_D_Volume =  round ((100* D_Volume / Volume_mg[1]), digits = 2) ) %>%
  mutate(D_Volume_all = round (Volume_all_mg - Volume_all_mg[1], digits = 3), P_D_Volume_all =  round ((100* D_Volume_all / Volume_all_mg[1]), digits = 2) ) %>%
  mutate( `Cost_eff ($/1000gal)` = round (- Cost_MD *1000 / D_Volume, digits = 1) ) %>%
  mutate(D_surface_runoff = round (Surface_runoff_mg - Surface_runoff_mg[1], digits = 3), P_D_surface_runoff =  round ((100* D_surface_runoff / Surface_runoff_mg[1]), digits = 2) ) %>%
  mutate(D_Infiltration = round (Infiltration_mg - Infiltration_mg[1], digits = 3), P_D_Infiltration =  round ((100* D_Infiltration / Infiltration_mg[1]), digits = 2) ) %>%
  mutate(D_Groundwater_inflow = round (Groundwater_inflow_mg - Groundwater_inflow_mg[1], digits = 3), P_D_Groundwater_inflow =  round ((100* D_Groundwater_inflow / Groundwater_inflow_mg[1]), digits = 2) ) %>%
  select(Scenario, Detail, D_Act, P_D_Act, D_Volume, P_D_Volume, D_Volume_all, P_D_Volume_all, Cost_MD, `Cost_eff ($/1000gal)`, 
         D_surface_runoff, P_D_surface_runoff, D_Infiltration, P_D_Infiltration, D_Groundwater_inflow, P_D_Groundwater_inflow)



# ----- Try Link -----
file_out_scenario_try <- c("Try3_1.out", "try3_1_Modified.out")

tic()
link_result_try <- list()
for (i in 1: length(file_out_scenario_try)) {
  link_result_try [[i]] <- fun_activation_link (file_out_scenario_try[i])
}
toc()

s_link_flow_min_pre <- list(); s_link_flow_max_pre <- list(); s_link_flow_avg_pre <- list(); 
s_link_activation_pre <- list(); s_link_volume_pre <- list(); s_link_volume_all_pre <- list()
s_link_volume_sum <- data.frame(); s_link_activation_sum <- data.frame(); s_link_volume_all_sum <- data.frame()
for (i in 1: length(file_out_scenario_try)) {
  
  s_link_flow_min_pre [[i]] <- link_result_try[[i]][["Link_Flow_information"]]$Minimum_flow_mgd   
  s_link_flow_max_pre [[i]] <- link_result_try[[i]][["Link_Flow_information"]]$Maximum_flow_mgd  
  s_link_flow_avg_pre [[i]] <- link_result_try[[i]][["Link_Flow_information"]]$Average_flow_mgd  
  
  s_link_volume_pre [[i]] <- link_result_try[[i]][["Link_activation_main"]]$Volume_mg            
  s_link_volume_all_pre [[i]] <- link_result_try[[i]][["Link_activation_main"]]$Volume_mg_all    
  s_link_activation_pre [[i]] <- link_result_try[[i]][["Link_activation_main"]]$Activation        
  
  s_link_volume_sum     <- rbind (s_link_volume_sum,     link_result_try[[i]][["Link_activation_volume"]] )              
  s_link_volume_all_sum <- rbind (s_link_volume_all_sum, link_result_try[[i]][["Link_total_volume"]] )            
  s_link_activation_sum <- rbind (s_link_activation_sum, link_result_try[[i]][["Link_activation_number"]] )       
  
}
colnames(s_link_volume_sum) <- c("Volume_mg"); colnames(s_link_volume_all_sum) <- c("Volume_mg"); colnames(s_link_activation_sum) <- c("Activation"); 

s_link_flow_min <- as.data.frame(s_link_flow_min_pre) ; names(s_link_flow_min) <- c("S3", "Modified") ; s_link_flow_min <- s_link_flow_min %>% mutate(D = S3-Modified )           
s_link_flow_max <- as.data.frame(s_link_flow_max_pre) ; names(s_link_flow_max) <- c("S3", "Modified") ; s_link_flow_max <- s_link_flow_max %>% mutate(D = S3-Modified )              
s_link_flow_avg <- as.data.frame(s_link_flow_avg_pre) ; names(s_link_flow_avg) <- c("S3", "Modified") ; s_link_flow_avg <- s_link_flow_avg %>% mutate(D = S3-Modified )             

s_link_volume <- as.data.frame(s_link_volume_pre) ; names(s_link_volume) <- c("S3", "Modified") ; s_link_volume <- s_link_volume %>% mutate(D = S3-Modified )                  
s_link_volume_all <- as.data.frame(s_link_volume_all_pre) ; names(s_link_volume_all) <- c("S3", "Modified") ; s_link_volume_all <- s_link_volume_all %>% mutate(D = S3-Modified )         
s_link_activation <- as.data.frame(s_link_activation_pre) ; names(s_link_activation) <- c("S3", "Modified") ; s_link_activation <- s_link_activation %>% mutate(D = S3-Modified )           

# Comparable table ......................................................
s_link_result <- data.frame (
  Activation    = s_link_activation_sum$Activation,
  Volume_mg     = s_link_volume_sum$Volume_mg ,
  Volume_all_mg = s_link_volume_all_sum$Volume_mg 
  
)


scenario_link_flow_combine <- list()
scenario_link_flow_combine_plot <- list()
for (i in 1: 256) {
  
  scenario_link_flow_combine [[i]] <- cbind(link_result_try [[1]] [[1]][,i], link_result_try [[2]] [[1]][,i] )
  colnames(scenario_link_flow_combine[[i]]) <- c("S3", "Modified")
  scenario_link_flow_combine_plot [[i]] <- dygraph(scenario_link_flow_combine[[i]], main = link_name[i,2], 
                                                   height = 500, width = 800, xlab = "Time", ylab = "Flow (mgd)") %>%
    dyRangeSelector(dateWindow = c("1993-01-01", "1994-01-01")) %>%
    dyOptions(drawPoints = TRUE, pointSize = 0.25) %>%
    dyRangeSelector(height = 30)%>%
    dyLimit(cr_flow_mgd, color = "red")
  
}


# ----- Try Node -----

tic()
node_result_try <- list()
for (i in 1: length(file_out_scenario_try)) {
  node_result_try [[i]] <- fun_activation_node (file_out_scenario_try[i])
}
toc()


s_node_flow_min_pre <- list(); s_node_flow_max_pre <- list(); s_node_flow_avg_pre <- list(); 
s_node_activation_pre <- list(); s_node_volume_pre <- list(); s_node_volume_all_pre <- list()
s_node_volume_sum <- data.frame(); s_node_activation_sum <- data.frame(); s_node_volume_all_sum <- data.frame()
for (i in 1: length(file_out_scenario_try)) {
  
  s_node_flow_min_pre [[i]] <- node_result_try[[i]][["Node_Flow_information"]]$Minimum_flow_mgd   
  s_node_flow_max_pre [[i]] <- node_result_try[[i]][["Node_Flow_information"]]$Maximum_flow_mgd  
  s_node_flow_avg_pre [[i]] <- node_result_try[[i]][["Node_Flow_information"]]$Average_flow_mgd  
  
  s_node_volume_pre [[i]] <- node_result_try[[i]][["Node_activation_main"]]$Volume_mg            
  s_node_volume_all_pre [[i]] <- node_result_try[[i]][["Node_activation_main"]]$Volume_mg_all    
  s_node_activation_pre [[i]] <- node_result_try[[i]][["Node_activation_main"]]$Activation        
  
  s_node_volume_sum     <- rbind (s_node_volume_sum,     node_result_try[[i]][["Node_activation_volume"]] )              
  s_node_volume_all_sum <- rbind (s_node_volume_all_sum, node_result_try[[i]][["Node_total_volume"]] )            
  s_node_activation_sum <- rbind (s_node_activation_sum, node_result_try[[i]][["Node_activation_number"]] )       
  
}
colnames(s_node_volume_sum) <- c("Volume_mg"); colnames(s_node_volume_all_sum) <- c("Volume_mg"); colnames(s_node_activation_sum) <- c("Activation"); 

s_node_flow_min <- as.data.frame(s_node_flow_min_pre) ; names(s_node_flow_min) <- c("S3", "Modified") ; s_node_flow_min <- s_node_flow_min %>% mutate(D = S3-Modified )            
s_node_flow_max <- as.data.frame(s_node_flow_max_pre) ; names(s_node_flow_max) <- c("S3", "Modified") ; s_node_flow_max <- s_node_flow_max %>% mutate(D = S3-Modified )             
s_node_flow_avg <- as.data.frame(s_node_flow_avg_pre) ; names(s_node_flow_avg) <- c("S3", "Modified") ; s_node_flow_avg <- s_node_flow_avg %>% mutate(D = S3-Modified )            

s_node_volume <- as.data.frame(s_node_volume_pre) ; names(s_node_volume) <- c("S3", "Modified") ; s_node_volume <- s_node_volume %>% mutate(D = S3-Modified )                     
s_node_volume_all <- as.data.frame(s_node_volume_all_pre) ; names(s_node_volume_all) <- c("S3", "Modified") ; s_node_volume_all <- s_node_volume_all %>% mutate(D = S3-Modified )        
s_node_activation <- as.data.frame(s_node_activation_pre) ; names(s_node_activation) <- c("S3", "Modified") ; s_node_activation <- s_node_activation %>% mutate(D = S3-Modified )          

# Comparable table ......................................................
s_node_result <- data.frame (
  Activation    = s_node_activation_sum$Activation,
  Volume_mg     = s_node_volume_sum$Volume_mg ,
  Volume_all_mg = s_node_volume_all_sum$Volume_mg 
)

# ................. Plot .....................

scenario_node_flow_combine <- list()
scenario_node_flow_combine_plot <- list()
for (j in 1: 58) {
  scenario_node_flow_combine [[j]] <- cbind(node_result [[1]] [[1]][,j], node_result [[2]] [[1]][,j])
  colnames(scenario_node_flow_combine[[j]]) <- c("S3", "Modified")
  scenario_node_flow_combine_plot [[j]] <- dygraph(scenario_node_flow_combine[[j]], main = node_name[j,1], 
                                                   height = 500, width = 800, xlab = "Time", ylab = "Flow (mgd)") %>%
    dyRangeSelector(dateWindow = c("1993-01-01", "1994-01-01")) %>%
    dyOptions(drawPoints = TRUE, pointSize = 0.25) %>%
    dyRangeSelector(height = 30)%>%
    dyLimit(cr_flow_mgd, color = "red")
  
}


# ----- Extra -----


file_out_scenario <- c(
  "Main_model.out",
  "S1.out",
  "S2.out", 
  "S3.out", 
  "S4.out",
  "S5.out",
  "S6.out",
  "S7.out", 
  "S8.out", 
  "S9.out"
)

link_result <- list()
node_result <- list()
for (i in 1: length(file_out_scenario)) {
  
  print( paste("Scenario",i, "is running") )
  
  link_result [[i]] <- fun_activation_link (file_out_scenario[i])
  node_result [[i]] <- fun_activation_node (file_out_scenario[i])
  # result [[i]] [[1]]: (Link or Node) Flow (mgd)
  # result [[i]] [[2]]: (Link or Node) Volume (mg)
  # result [[i]] [[3]]: (Link or Node) Flow information
  # result [[i]] [[4]]: (Link or Node) Activation details
  # result [[i]] [[5]]: (Link or Node) Flow plot
  # result [[i]] [[6]]: (Link or Node) Activation main information
  # result [[i]] [[7]]: (Link or Node) All activation in one table
  # result [[i]] [[8]]: (Link or Node) Number of activation
  # result [[i]] [[9]]: (Link or Node) Total activation volume
  # result [[i]] [[10]]:(Link or Node) Total volume
}
# object.size(link_result)
# object.size(node_result)

link_table_1 <- data.frame()
link_table_2 <- data.frame()
link_table_3 <- data.frame()
link_table_4 <- data.frame()
node_table_1 <- data.frame()
node_table_2 <- data.frame()
node_table_3 <- data.frame()
node_table_4 <- data.frame()
for (i in 1: length(file_out_scenario)) {
  link_table_1 [i,1] <- i-1
  link_table_2 [i,1] <- link_result [[i]] [[8]]
  link_table_3 [i,1] <- link_result [[i]] [[9]]
  link_table_4 [i,1] <- link_result [[i]] [[10]]
  node_table_1 [i,1] <- i-1
  node_table_2 [i,1] <- node_result [[i]] [[8]]
  node_table_3 [i,1] <- node_result [[i]] [[9]]
  node_table_4 [i,1] <- node_result [[i]] [[10]]
}


link_table  <- data.frame (Scenario = link_table_1$V1, Activation = link_table_2$V1,  Volume_mg = link_table_3$V1, Volume_all_mg = link_table_4$V1, ) %>%
  mutate(Act_Dif = Activation - Activation[1], Act_Dif_Perc = round ((100* Act_Dif / Activation[1]), digits = 2) ) %>%
  mutate(Volume_Dif = Volume_mg - Volume_mg[1], Volume_Dif_Perc =  round ((100* Volume_Dif / Volume_mg[1]), digits = 2) ) %>%
  mutate(Volume_all_Dif = Volume_all_mg - Volume_all_mg[1], Volume_all_Dif_Perc =  round ((100* Volume_all_Dif / Volume_all_mg[1]), digits = 2) ) %>%
  select(Scenario, Activation, Act_Dif, Act_Dif_Perc, Volume_mg, Volume_Dif, Volume_Dif_Perc, Volume_all_mg, Volume_all_Dif, Volume_all_Dif_Perc)

node_table  <- data.frame (Scenario = node_table_1$V1, Activation = node_table_2$V1,  Volume_mg = node_table_3$V1, Volume_all_mg = node_table_4$v1) %>%
  mutate(Act_Dif = Activation - Activation[1], Act_Dif_Perc = round ((100* Act_Dif / Activation[1]), digits = 2) ) %>%
  mutate(Volume_Dif = Volume_mg - Volume_mg[1], Volume_Dif_Perc =  round ((100* Volume_Dif / Volume_mg[1]), digits = 2) ) %>%
  mutate(Volume_all_Dif = Volume_all_mg - Volume_all_mg[1], Volume_all_Dif_Perc =  round ((100* Volume_all_Dif / Volume_all_mg[1]), digits = 2) ) %>%
  select(Scenario, Activation, Act_Dif, Act_Dif_Perc, Volume_mg, Volume_Dif, Volume_Dif_Perc, Volume_all_mg, Volume_all_Dif, Volume_all_Dif_Perc)





# ----- RB + DSD (Scenarios 101-124) -----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

bs_file_for_write_new $ subcatchments <- bs_subcatchment_DSD

for ( i in 1: bs_number) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_new[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[100+i])
}

# ----- RB + Cistern (Scenarios 201-224) -----

bs_lid_RB_cistern_new <- list()
for ( i in 1: bs_number) {
  bs_lid_RB_cistern_new [[i]] <- bs_lid_RB_new [[i]] %>% 
    full_join(bs_lid_cistern_new, by = c("Subcatchment", "LID Process", "Area", "Number",  "Area", "Width", "InitSat", "FromImp", "ToPerv"))
}

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

for ( i in 1: bs_number) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_cistern_new[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[200+i])
}

# ----- RB + DSD + Cistern (Scenarios 301-324) -----

bs_file_for_write_new <- bs_file_for_write
bs_file_for_write_new $ options  <- bs_options_new
bs_file_for_write_new $ outfalls <- bs_outfall_correct

bs_file_for_write_new $ subcatchments <- bs_subcatchment_DSD

for ( i in 1: bs_number) {
  bs_file_for_write_new $ lid_usage <- bs_lid_RB_cistern_new[[i]]
  write_inp (bs_file_for_write_new, file_inp_scenario_pre[200+i])
}





y <-data.frame (Y = c(1:10000)/100 )
y2 <-data.frame (Y2 = c(22000:23000)/100000 )
View(y)
q <- y %>% mutate(Q = ((1.49 /0.014 ) * 4 * Y * ((4*Y/(4+2*Y))^(2/3)) * (0.36397 ^0.5) ) -20.11) %>% arrange 
q2 <- y2 %>% mutate(Q = ((1.49 /0.014 ) * 4 * Y2 * ((4*Y2/(4+2*Y2))^(2/3)) * (0.36397 ^0.5) ) -20.11) %>% arrange 

View(q)



# ----- Simulation model characteristics ----- 

Name_SWMM_input     <- c( "BSA_Main_Model.inp", "BSA_Main_Model_RB.inp" )
bs_file_for_write   <- read_inp(Name_SWMM_input[2])
inp_subcatchment    <- bs_file_for_write $ subcatchments

sim_subcatchment_all <- nrow (inp_subcatchment)
sim_subcatchment_SAperv <- nrow ( filter (inp_subcatchment, grepl("SAperv", Name)) )
sim_sub_out_area <- sum ( (arrange(inp_subcatchment, desc(Area))$Area)[1:5] ) +280
sim_sub_area_all <- sum(inp_subcatchment$Area)
sim_junction_all <- nrow(bs_file_for_write $junctions)
sim_storage_all <- nrow(bs_file_for_write $storage)



