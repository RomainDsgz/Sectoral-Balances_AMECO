# clean environment 
rm(list=ls())

# Obtaining and cleaning the data 
# Manually download .txt zip from https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en
# 
files <- dir("path/sectoral_balances/ameco0", ".TXT", full.names = TRUE)

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
#Drop stray/empty last column
ameco <- ameco[, -ncol(ameco)] 
names(ameco) <- tolower(names(ameco))
# Extract short country names 
ameco$cntry <- regmatches(ameco$code, regexpr("^[[:alnum:]+]", ameco$code))

# Convert to long format 
ameco <- gather(ameco, key = year, value = value, starts_with("x"))
ameco$year <- gsub("x", "", ameco$year)
ameco$year <- as.numeric(ameco$year)
ameco$value <- suppressWarnings(as.numeric(ameco$value))

# Save prepared data                                
save(ameco, file = "path/sectoral_balances/ameco.RData", 
     compress = "xz")

# Load packages for analysis 
library(ggplot2)
library(reshape2)

# Load data
load("path/sectoral_balances/ameco.RData")

# Extract data for Switzerland (CHE)
ch <- subset(ameco, country == "Switzerland")

# Codes of the different variables for Net lending - Net borrowing (Mrd CHF)
# Find codes for Net lending/Net borrowing 
# Private Sector 
# UBLC = Net lending - Net borrowing of Corporations (Financial and Non-Financial) (Mrd National Currency)
# UBLH = Net lending - Net borrowing of Households and non-profit Institutions serving households (Mrd National Currency)
# Public Sector 
# URTG = General Government revenue
# UUTG = General government expenses
# Note that URTG - UUTG = UBLG
# UBLG = General Government (Mrd National Currency)
# Rest of the world 
# UXGS = Exports of goods and services
# UMGS = Imports of goods and services 
# Note that UXGS - UMGS =/= CAB, as it requires foreign net income. 
#For this reason, we will use UBLA
# UBLA = Rest of the World : UBLA (Mrd National Currency)
# UVGN = Gross National Income (Current prices)

UBLC <- subset(ch, code == "CHE.1.0.0.0.UBLC")
UBLH <- subset(ch, code == "CHE.1.0.0.0.UBLH")
URTG <- subset(ch, code == "CHE.1.0.0.0.URTG")
UUTG <- subset(ch, code == "CHE.1.0.0.0.UUTG")
UBLG <- subset(ch, code == "CHE.1.0.0.0.UBLG")
UXGS <- subset(ch, code == "CHE.1.0.0.0.UXGS")
UMGS <- subset(ch, code == "CHE.1.0.0.0.UMGS")
UBLA <- subset(ch, code == "CHE.1.0.0.0.UBLA")
UVGN <- subset(ch, code == "CHE.1.0.0.0.UVGN")

# Merge them into one dataframe
df <- rbind(UBLC, UBLH,URTG, UUTG, UBLG, UXGS, UMGS, UBLA, UVGN)

# Select vars
myvars <- c("year", "code", "value")
df <- df[myvars]

# Long to wide 
dfw <- spread (df,
               key = code,
               value = value)

# rename codes
dfw <- rename(dfw, UIGT = "CHE.1.0.0.0.UIGT")
dfw <- rename(dfw, UBLC = "CHE.1.0.0.0.UBLC")
dfw <- rename(dfw, UBLH = "CHE.1.0.0.0.UBLH")
dfw <- rename(dfw, USGP = "CHE.1.0.0.0.USGP")
dfw <- rename(dfw, OIST = "CHE.1.0.0.0.OIST")
dfw <- rename(dfw, UKTGT = "CHE.1.0.0.0.UKTGT")


dfw <- rename(dfw, URTG = "CHE.1.0.0.0.URTG")
dfw <- rename(dfw, UUTG = "CHE.1.0.0.0.UUTG")
dfw <- rename(dfw, UBLG = "CHE.1.0.0.0.UBLG")

dfw <- rename(dfw, UXGS = "CHE.1.0.0.0.UXGS")
dfw <- rename(dfw, UMGS = "CHE.1.0.0.0.UMGS")
dfw <- rename(dfw, UBLA = "CHE.1.0.0.0.UBLA")

dfw <- rename(dfw, UVGN = "CHE.1.0.0.0.UVGN")

#Now that the data is ready, we can now determine the macroeconomic sectoral balances. 
# (S - I)+ (T - G) + (- CAB) ??? 0 
## Public Sector can be found via two methods:
dfw$pub <- dfw$URTG - dfw$UUTG # Revenues - Expenses 
# Or use Net lending - Net borrowing of public sector (UBLG)
dfw$pub <- dfw$UBLG

## Rest of the World
# According to Mitchell, Wray and Watts (2019), CAB = Exports - Imports + Foreign Net Income
# Note that row_bal is negative
dfw$row <- dfw$UBLA  * -1
dfw$row
## Private Sector
#Since AMECO does not include some financial data, the sum of Net lending - Net borrowing of Corporations (UBLC) + Net lending - Net borrowing of Households (UBLH) =/= Private sector balance
# Hence, we can use (S - l) ??? (G - T) + (X - M + FNl) ??? (G - T) + CAB

# Note that the sign of pub_bal is inversed, since the deficit of the public sector is the surplus of the private sector
dfw$priv <- (dfw$pub *-1) - dfw$row
# Now that we have all balances, we can check the equality (S - I)+ (T - G) + (- CAB) ??? 0
dfw$bal <- dfw$priv + dfw$pub + dfw$row
dfw$bal 

# Now we can calculate the share (in % of GNI)
dfw$priv_bal <- (dfw$priv/dfw$UVGN)*100
dfw$pub_bal <- (dfw$pub/dfw$UVGN)*100
dfw$row_bal <- (dfw$row/dfw$UVGN) *100

# Check if equality (S-I) + (G-T) + (-CAB) = 0
dfw$balances <- dfw$priv_bal + dfw$pub_bal + dfw$row_bal
dfw$balances

# Prepare the data for plotting for 3 sectors 
melted_df <- dfw %>%
  select(year, priv_bal, pub_bal, row_bal) %>%
  gather(key = "variable", value = "value", -year)

colors3 <- c("indianred3", "chartreuse4", "turquoise3")
plot_3sectors <- ggplot() + 
  geom_bar(data = subset(melted_df, year >= 1995 & year<=2022), aes(x = year, y = value, fill = variable), stat = "identity") + 
  scale_fill_manual(values = colors3,
                    labels = c("Private sector", "Public Sector", "Rest of the World")) +
  labs(x = "Year", y = "Net lending - Net borrowing", fill = "Balances") + 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(1995, 2022, by = 5)) +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(nrow=1)) +
  labs(title = "Sectoral balances",
       subtitle = "Switzerland, 1995-2022", 
       caption = "Source: AMECO")
print(plot_3sectors)  
