Payrolls_latest_data <- bls_api("CES0000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest) %>%
  arrange(date) %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(diff(value),2))

Payrolls_formatted_change <- ifelse(Payrolls_latest_data$change_from_prior_month >= 0,
                                paste0("+", Payrolls_latest_data$change_from_prior_month),
                                Payrolls_latest_data$change_from_prior_month)

UNRATE_latest_data <- bls_api("LNS14000000", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest) %>%
  arrange(date) %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(diff(value),2))

UNRATE_formatted_change <- ifelse(UNRATE_latest_data$change_from_prior_month >= 0,
                                    paste0("+", UNRATE_latest_data$change_from_prior_month),
                                    UNRATE_latest_data$change_from_prior_month)

output_string <- paste0("NEW JOBS DATA:\n\nNon-farm Payrolls: ", Payrolls_formatted_change, "k")
output_string <- paste0(output_string, "\nUnemployment Rate: ", UNRATE_latest_data$latest_value,"%", " (",UNRATE_formatted_change,"%)")

EPOP_1990 <- bls_api("LNS12300060", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_2010 <- bls_api("LNS12300060", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPop <- rbind(EPOP_1990,EPOP_2010) %>%
  arrange(date)

EPop_latest_data <- EPop %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(diff(value),2))

# Find the date of the closest prior value that's higher
EPop_date_of_higher_value <- if (EPop_latest_data$change_from_prior_month > 0) {
  EPop %>%
    filter(date < EPop_latest_data$latest_date, value >= EPop_latest_data$latest_value) %>%
    summarise(date_of_higher_value = max(date)) %>%
    pull() %>%
    as.Date(origin = "1970-01-01") %>%
    format("%B %Y")
} else {
  NA
}

EPop_formatted_change <- ifelse(EPop_latest_data$change_from_prior_month >= 0,
                           paste0("+", EPop_latest_data$change_from_prior_month),
                           EPop_latest_data$change_from_prior_month)


# Concatenate the string
output_string <- paste0(output_string,"\nPrime Age (25-54) Employment-Population Ratio: ", EPop_latest_data$latest_value,
                        "% (", EPop_formatted_change,"%")

# Add the "Highest since" section if change is positive
if (!is.na(EPop_date_of_higher_value)) {
  output_string <- paste0(output_string, ", Highest since ", date_of_higher_value,"!!)")
}

AHE_latest_data <- bls_api("CES0500000003", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest) %>%
  arrange(date) %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(100 * diff(value) / head(value, 1), 1))

AHE_formatted_change <- ifelse(AHE_latest_data$change_from_prior_month >= 0,
                                  paste0("+", AHE_latest_data$change_from_prior_month),
                                  AHE_latest_data$change_from_prior_month)


output_string <- paste0(output_string, "\nAverage Hourly Earnings: ",AHE_formatted_change,"%")


EPOP_WOM_1990 <- bls_api("LNS12300062", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_WOM_2010 <- bls_api("LNS12300062", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPOP_WOM <- rbind(EPOP_WOM_1990,EPOP_WOM_2010) %>%
  arrange(desc(value)) %>%
  rev()

cat(paste0("DATE VERIFICATION:\n",Payrolls_latest_data$latest_date,"\n",UNRATE_latest_data$latest_date,"\n",EPop_latest_data$latest_date,"\n",AHE_latest_data$latest_date))
cat(paste0("Highest Women's EPOP: ",EPOP_WOM$date[1]))
# Print the result
cat(output_string)

