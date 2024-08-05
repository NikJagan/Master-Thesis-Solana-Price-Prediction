# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)



# Read the CSV files
btc_data <- read.csv("BTC-USD.csv")
eth_data <- read.csv("ETH-USD.csv")
sol_data <- read.csv("SOL-USD.csv")
sp500_data <- read_csv("sp500.csv")
cpi_data <- read_csv("CPILFESL.csv")


#############################################################################

# Preprocess the data

# Select only the relevant columns

btc_data <- btc_data %>%
  select(-Adj.Close) %>%
  rename(btc_open = Open, btc_high = High, btc_low = Low, btc_close = Close, btc_volume = Volume) %>%
  mutate(Date = ymd(Date))

eth_data <- eth_data %>%
  select(-Adj.Close) %>%
  rename(eth_open = Open, eth_high = High, eth_low = Low, eth_close = Close, eth_volume = Volume) %>%
  mutate(Date = ymd(Date))

sol_data <- sol_data %>%
  select(-Adj.Close) %>%
  rename(sol_open = Open, sol_high = High, sol_low = Low, sol_close = Close, sol_volume = Volume) %>%
  mutate(Date = ymd(Date))


# Rename the 'DATE' column to 'Date' and convert to date format in the CPI dataset
cpi_data <- cpi_data %>% 
  rename(Date = DATE) %>% 
  mutate(Date = ymd(Date))


# Remove the 'tic' and 'gvkeyx' columns, rename columns, and convert 'Date' to date format in the S&P 500 dataset
sp500_data <- sp500_data %>%
  select(-tic, -gvkeyx) %>%
  rename(Date = datadate, sp_close = `prccd`, sp_high = `prchd`, sp_low = `prcld`) %>%
  mutate(Date = ymd(Date))


# Convert monthly data to daily data and interpolate
cpi_data <- cpi_data %>%
  # Create a complete sequence of daily dates
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  # Perform linear interpolation for missing values
  mutate(CPILFESL = na.approx(CPILFESL, x = Date, rule = 2))

# Left join all datasets with sol_data based on Date
df1 <- sol_data %>%
  left_join(btc_data, by = "Date") %>%
  left_join(eth_data, by = "Date") %>%
  left_join(cpi_data, by = "Date") %>%
  left_join(sp500_data, by = "Date")

# Remove all rows from df1 with dates from 2024-06-02 onwards
df1 <- df1 %>% filter(Date < ymd("2024-06-02"))


# Forward fill the sp_close, sp_high, and sp_low columns
df1 <- df1 %>%
  mutate(
    sp_close = na.locf(sp_close, na.rm = FALSE),
    sp_high = na.locf(sp_high, na.rm = FALSE),
    sp_low = na.locf(sp_low, na.rm = FALSE)
  )


# Dates to be removed
dates_to_remove <- as.Date(c("2020-04-10", "2020-04-11", "2020-04-12"))

# Remove the specified dates from df1
df1 <- df1 %>%
  filter(!Date %in% dates_to_remove)


# Export df1 as a CSV file
write.csv(df1, "df1.csv", row.names = FALSE)

