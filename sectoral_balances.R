# clean environment 
rm(list=ls())

# Obtaining and cleaning the data 
# Manually download .txt zip from https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en
# 
files <- dir("C:/Users/rdesg/Documents/sectoral_balances/ameco0", ".TXT", full.names = TRUE)

# Load packages for data extraction
library(dplyr)
library(tidyr)
library(tibble)
# Read files, bind together, clean, save
all_files <- lapply(files, function(file) {
  read.table(file, TRUE, ";", fill = TRUE,
             stringsAsFactors = FALSE, strip.white = TRUE)
})

#@unlink(temp_dir, recursive = TRUE)

ameco <- do.call(rbind, all_files)
ameco <- tibble::as_tibble(ameco)
ameco <- ameco[, -ncol(ameco)] #Drop stray/empty last column
names(ameco) <- tolower(names(ameco))
# Extract short country names 
ameco$cntry <- regmatches(ameco$code, regexpr("^[[:alnum:]+", ameco$code))

# Convert to long format 
ameco <- gather(ameco, key = year, value = value, starts_with("x"))
ameco$year <- gsub("x", "", ameco$year)
ameco$year <- as.numeric(ameco$year)
ameco$value <- suppressWarnings(as.numeric(ameco$value))

# Save preparer data                                
save(ameco, file = "C:/Users/rdesg/Documents/sectoral_balances/ameco.RData", 
     compress = "xz")

# Load packages for analysis 
library(ggplot2)
library(reshape2)

# Load data
load("C:/Users/rdesg/Documents/sectoral_balances/ameco.RData")

# Extract data for Switzerland (CHE)
ch <- subset(ameco, country == "Switzerland")

# Codes of the different variables for Net lending - Net borrowing (Mrd CHF)
# Find codes for Net lending/Net borrowing 
# UBLG = General Government (Mrd CHF )
# UBLA = Rest of the World : UBLA (Mrd CHF)
# UBLC = Corporations (Financial and Non-Financial) (Mrd CHF)
# UBLH = Households and non-profit Institutions serving households (Mrd CHF)
# UVGD = Gross Domestic Product (Current prices) 
# Public Sector 
UBLG <- subset(ch, code=="CHE.1.0.0.0.UBLG")
# Rest of the world 
UBLA <- subset(ch, code=="CHE.1.0.0.0.UBLA")
# Private sector 
  # Corporations
UBLC <- subset(ch, code=="CHE.1.0.0.0.UBLC")
  # Households and non-profit Institutions serving households 
UBLH <- subset(ch, code=="CHE.1.0.0.0.UBLH")

# GDP 
UVGD <- subset(ch, code=="CHE.1.0.0.0.UVGD")

# Merge them under one df 
df <- rbind(UBLG, UBLA, UBLC, UBLH, UVGD)

# Select vars 
myvars <- c("year", "code", "value")
df <- df[myvars]

# Long to wide 
dfw <- spread (df,
               key = code,
               value = value)
# rename codes
dfw <- rename(dfw, UBLG = "CHE.1.0.0.0.UBLG")
dfw <- rename(dfw, UBLA = "CHE.1.0.0.0.UBLA")
dfw <- rename(dfw, UBLC = "CHE.1.0.0.0.UBLC")
dfw <- rename(dfw, UBLH = "CHE.1.0.0.0.UBLH")
dfw <- rename(dfw, UVGD = "CHE.1.0.0.0.UVGD")

# Calculate the share of each sector's balance in the GDP
dfw$share_public <- (dfw$UBLG / dfw$UVGD)*100
dfw$share_row <- ((dfw$UBLA / dfw$UVGD)*100)*-1
dfw$share_corp <- (dfw$UBLC / dfw$UVGD)*100
dfw$share_household <- (dfw$UBLH / dfw$UVGD)*100
dfw$share_priv <- ((dfw$UBLC + dfw$UBLH) / dfw$UVGD)*100

melted_df <- dfw%>%
  select(year, share_public, share_corp, share_household, share_row) %>%
  gather(key = "variable", value = "value", -year)

# Graph  
my_plot <- ggplot() + 
  geom_bar(data = subset(melted_df, year >= 1995), aes(x = year, y = value, fill = variable), stat = "identity") + 
  scale_fill_manual(values = c("share_public" = "red", "share_row" = "blue", "share_corp" = "green", "share_household" = "yellow"),
                    labels = c("Public sector", "Rest of the World", "Corporations", "Household")) +
  labs(x = "Year", y = "Net lending - Net borrowing", fill = "Balance", color = "") + 
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = seq(1995, 2023, by = 5)) + 
  labs(title = "Sectoral balances",
       subtitle = "Switzerland, 19995-2023", 
       caption = "Source: AMECO")
print(my_plot)  
