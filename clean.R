library(tidyverse)
library(data.table)

# Store data file paths 
tbl_fin <-
  list.files(path = "./data/financial/",
             pattern = "\\.tab$",
             full.names = T)

tbl_ifs <-
  list.files(path = "./data/ifs/",
             pattern = "\\.tab$",
             full.names = T)

# Initialize financial and individual and family survey (ifs) data df
fin <- data.frame()
ifs <- data.frame()

# Combine financial and ifs data into respective dfs
for (i in 1:10){
  
  # read wave data
  fin0 <- read.delim(tbl_fin[i])
  
  # find and add wave number based on file name
  no <- str_extract(tbl_fin[i], "\\d+")
  fin0$Wave <- no
  
  # find column with wave-specific household id
  wave_house <- colnames(fin0)[grep("^idahhw", colnames(fin0))]
  
  # add columns of interest to overall financial data
  fin0 <- fin0 %>% select(c("Wave","idauniq", wave_house, 
                                      "eqtotinc_bu_s","nettotw_bu_s"))
  colnames(fin0) <- c("Wave", "ID_Ind", "ID_Household", "Income", "Wealth")
  
  fin <- rbind(fin, fin0)
  
}

# Ifs
for (i in 1:10){
  
  # read wave data
  ifs0 <- read.delim(tbl_ifs[i])
  
  # ifs and add wave number based on file name
  no <- str_extract(tbl_ifs[i], "\\d+")
  ifs0$Wave <- no
  
  # ifs column with wave-specific household id
  wave_house <- colnames(ifs0)[grep("^idahhw", colnames(ifs0))]
  
  # add columns of interest to overall ifs data
  ifs0 <- ifs0 %>% select(c("Wave","idauniq", wave_house, 
                            "edqual", "srh_hrs", "srh_hse"
                            ))

  colnames(ifs0) <- c("Wave", "ID_Ind", "ID_Household", "Education", 
                      "Health_HRS", "Health_HSE")
  
  ifs <- rbind(ifs, ifs0)
  
}

# Join the 2 datasets by wave number, ind id, and wave-specific household id
data <- inner_join(fin, ifs, by = c("Wave", "ID_Ind","ID_Household"))
data0 <- data #backup

# Check proportions of response rate
table(data$Health_HSE)
table(data$Health_HRS)

# only keep HRS since many respondents weren't asked HSE version in many waves (code -3)
data <- data %>% select(c(-Health_HSE))

# remove records where any field is missing
data <- data %>% filter(if_all(everything(), ~ . >= 0))


num_vars <- c("Wave", "ID_Ind", "ID_Household", "Education",
              "Health_HRS",  "Wealth", "Income")

data[num_vars] <- lapply(data[num_vars], as.numeric)

data <- data %>% filter(Wave > 6)

# create a copy to store temporary values
temp <- data$Health_HRS

# apply simultaneous recoding
data$Health_HRS[temp == 1] <- 5
data$Health_HRS[temp == 2] <- 4
data$Health_HRS[temp == 4] <- 2
data$Health_HRS[temp == 5] <- 1

# pivot to wide format
data_w <- data %>% pivot_wider(id_cols = ID_Ind, names_from = Wave, 
    values_from = c(Wealth, Income, Education, Health_HRS),  
    names_sep = "_W"                     
  )

# standardize data
data_w_std <- data_w %>%
  mutate(across(starts_with("Education"), scale),
         across(starts_with("Wealth"), scale),
         across(starts_with("Health_HRS"), scale),
         across(starts_with("Income"), scale))


