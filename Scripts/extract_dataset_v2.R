########################
### Extract all data ###
########################

# Libraries
library(DBI)
library(odbc)
library(jsonlite)
library(lubridate)
library(data.table)


## Connect to the SQL database ##

ensure_az_login <- function(tenant_id = NULL) {
  # Check whether Azure CLI already has a usable account
  check <- system2(
    "az",
    args = c("account", "show", "--output", "json"),
    stdout = TRUE,
    stderr = TRUE
  )
  
  status <- attr(check, "status")
  
  if (is.null(status) || status == 0) {
    account <- jsonlite::fromJSON(paste(check, collapse = "\n"))
    message("Azure CLI is already logged in as: ", account$user$name)
    message("Tenant: ", account$tenantId)
    return(invisible(account))
  }
  
  message("Azure CLI is not logged in. Starting az login...")
  
  login_args <- c("login", "--output", "json", "--allow-no-subscriptions")
  
  if (!is.null(tenant_id) && nzchar(tenant_id)) {
    login_args <- c(login_args, "--tenant", tenant_id)
  }
  
  login <- system2(
    "az",
    args = login_args,
    stdout = TRUE,
    stderr = TRUE
  )
  
  login_status <- attr(login, "status")
  
  if (!is.null(login_status) && login_status != 0) {
    stop(
      "az login failed:\n",
      paste(login, collapse = "\n")
    )
  }
  
  # Re-check the active account after login
  account_raw <- system2(
    "az",
    args = c("account", "show", "--output", "json"),
    stdout = TRUE,
    stderr = TRUE
  )
  
  account_status <- attr(account_raw, "status")
  
  if (!is.null(account_status) && account_status != 0) {
    stop(
      "az login appeared to complete, but az account show failed:\n",
      paste(account_raw, collapse = "\n")
    )
  }
  
  account <- jsonlite::fromJSON(paste(account_raw, collapse = "\n"))
  message("Azure CLI logged in as: ", account$user$name)
  message("Tenant: ", account$tenantId)
  
  invisible(account)
}

get_azure_sql_token <- function(tenant_id = NULL) {
  ensure_az_login(tenant_id = tenant_id)
  
  token_raw <- system2(
    "az",
    args = c(
      "account", "get-access-token",
      "--resource", "https://database.windows.net/",
      "--output", "json"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  
  token_status <- attr(token_raw, "status")
  
  if (!is.null(token_status) && token_status != 0) {
    stop(
      "Azure CLI token request failed:\n",
      paste(token_raw, collapse = "\n")
    )
  }
  
  token_response <- jsonlite::fromJSON(paste(token_raw, collapse = "\n"))
  
  message("Token acquired. Expires: ", token_response$expiresOn)
  
  token_response$accessToken
}

# Get tokens to access databases
token <- get_azure_sql_token(
  tenant_id = NULL
) # This will ask you to log in first (email: mark.green@northwestsde.nhs.uk) - opens behind RStudio so minimise - only need to do once

# Log onto relevant server
con <- dbConnect(
  odbc(),
  Driver = "ODBC Driver 18 for SQL Server",
  Server = "tcp:sql-nwsdeprod-ws-62ab-svc-fcf7.database.windows.net,1433",
  Database = "sqldb-nwsdeprod-ws-62ab-svc-fcf7",
  Encrypt = "yes",
  TrustServerCertificate = "no",
  HostNameInCertificate = "*.database.windows.net",
  attributes = list(
    azure_token = token
  )
)



## Create population spine ## 

# Aim: To create a single dataset which contains everyone in CIPHA and some basic demographic information about them.

# DBI::dbListTables(con) # Get list of all tables available
# test <- DBI::dbGetQuery(con, "SELECT Top 10 * FROM [Client_SystemP].[patient]") # Load table quickly to inspect all columns
# CG_nwids9_alleth_BWB

# Get UPRN linked green spaces
greenspace_uprn <- DBI::dbGetQuery(con, "SELECT * FROM [dbo].[uprn_greenspace_distances]") 
# uprn_acorn, uprn_acorn_v2, uprn_evi_cleaned_v5_processed, greenspace_uprn_3_30_300

# Get data to link UPRNs to individuals
uprn_linkage <- DBI::dbGetQuery(con, "SELECT * FROM [Client_SystemP].[uprn_res_linkage]") 

# Get Groundswell table GP events for depression and anxiety
gp_events <- DBI::dbGetQuery(con, "SELECT * FROM [Client_SystemP_RW].[XH_GW_GP_events_depression_anxiety_dev]")

# Query database for what you want - get main details about people (slow - 5mins)
spine <- dbGetQuery(con, "
SELECT DISTINCT PK_Patient_ID, Patient_ID, FK_Patient_Link_ID, ModifDate, HDMModifDate, Sex, Dob, EthnicOrigin, NWSDE_Pseudo_Number, FrailtyDeficits, FrailtyScore, FrailtyDeficitList, EDC_Codes, QOFRegisters, IMD_Score, LSOA_Code
FROM Client_SystemP.patient
WHERE
OptedOutType2Flag = 'N'
AND FK_Reference_Tenancy_ID = 2
                 ")
# Note: FK_Reference_Tenancy_ID = 2    --- GP in C&M region

# Merge datasets together
joined <- merge(spine, uprn_linkage, by = "NWSDE_Pseudo_Number", all.x = TRUE) # Join on pseudo-UPRN to spine
hold <- greenspace_uprn[!is.na(greenspace_uprn$pseudo_uprn),] # Drop rows with missing IDs
joined <- merge(joined, hold, by = "pseudo_uprn", all.x = TRUE) # Join on distance to nearest green space

# Create age
joined$age <- as.numeric(today() - as_date(paste0(joined$Dob, "-15"))) # Calculates age in days
joined$age <- floor(joined$age / 365.25) # Convert age to years

# Create 10-year age group
breaks <- c(0, 9, 19, 29, 39, 49, 59, 69, 79, 89, 121) # Define age group cut points
labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+") # Define labels for age group
joined$age_group <- cut(joined$age, breaks = breaks, labels = labels, right = FALSE) # Calculate which age group each person belongs to based on age

# Save
write.csv(joined, "./Data/population_spine.csv")
rm(joined, spine, uprn_linkage, greenspace_uprn, breaks, labels) # Tidy up R
gc()


# Get Ethnicity records

# Call table
ethnicity <- dbGetQuery(con, "
SELECT DISTINCT PK_Patient_Link_ID, Patient_Link_ID, OrgLinks_FK_Patient_ID, EthnicMainGroup, EthnicGroupDescription, EthnicCategoryDescription, NHS_EthnicCategory, EthnicGroupAlgorithm, Deceased
FROM Client_SystemP.patient_link
WHERE
OptedOut = 'N'
                 ")

# Save
write.csv(ethnicity, "./Data/ethnicity.csv")
rm(ethnicity)

# Chris' update
ethnicity2 <- DBI::dbGetQuery(con, "SELECT * FROM [Client_SystemP_RW].[CG_nwids9_alleth_BWB]")
write.csv(ethnicity2, "./Data/ethnicity2.csv")


## Get common mental health diagnoses ##

# GP events / Diagnoses # 

# Query database to extract saved table - diagnoses two years before exposure and after
cmd_tab <- DBI::dbGetQuery(con, 
  "SELECT DISTINCT * 
  FROM [Client_SystemP_RW].[XH_GW_GP_events_depression_anxiety_all_dev]
  WHERE EventDate BETWEEN '2022-06-04' AND '2026-03-03'
  ")

# Create outcome measures
cmd_tab <- data.table(cmd_tab) # Convert object type to aggregate later
cmd_tab$cmd_gp_event_pre <- 0 # Outcome pre exposure
cmd_tab$cmd_gp_event_pre[cmd_tab$EventDate <= "2024-06-04"] <- 1
cmd_tab$cmd_gp_event_post <- 0 # Outcome post exposure
cmd_tab$cmd_gp_event_post[cmd_tab$EventDate > "2024-06-04"] <- 1
cmd_tab$cmd_gp_event_new_diag <- 0 # Outcome post exposure and first diagnosis
cmd_tab$cmd_gp_event_new_diag[cmd_tab$EventDate > "2024-06-04" & cmd_tab$First_Diag == 1] <- 1
cmd_tab_wide <- cmd_tab[, list(cmd_gp_event_pre = max(cmd_gp_event_pre, na.rm = TRUE), cmd_gp_event_post = max(cmd_gp_event_post, na.rm = TRUE), cmd_gp_event_new_diag = max(cmd_gp_event_new_diag, na.rm = TRUE)), by = "FK_Patient_ID"] # Convert to wide format by reshaping and creating a binary measure for the outcome
write.csv(cmd_tab_wide, "./Data/common_mental_health_diagnoses.csv") # Save
rm(cmd_tab, cmd_tab_wide)
gc()

# Subset outcomes after exposure date 4th June - new diagnoses only. Upto 18 months after

test <- DBI::dbGetQuery(con, "SELECT Top 10 * FROM [Client_SystemP_RW].[XH_GW_GP_medications_depression_anxiety_all_dev]")


# Medications # 

# Query database to extract saved table - diagnoses two years before exposure and after
cmd_tab <- DBI::dbGetQuery(con, 
  "SELECT DISTINCT * 
  FROM [Client_SystemP_RW].[XH_GW_GP_medications_depression_anxiety_all_dev]
  WHERE MedicationDate BETWEEN '2022-06-04' AND '2026-03-03'
  ")

# Create outcome measures
cmd_tab <- data.table(cmd_tab) # Convert object type to aggregate later
cmd_tab$cmd_med_pre <- 0 # Outcome pre exposure
cmd_tab$cmd_med_pre[cmd_tab$MedicationDate <= "2024-06-04"] <- 1
cmd_tab$cmd_med_post <- 0 # Outcome post exposure
cmd_tab$cmd_med_post[cmd_tab$MedicationDate > "2024-06-04"] <- 1
cmd_tab$cmd_gp_event_first_med <- 0 # First med
cmd_tab$cmd_gp_event_first_med[cmd_tab$MedicationDate > "2024-06-04" & (cmd_tab$Date_First_Diag > "2024-06-04" | is.na(cmd_tab$Date_First_Diag))] <- 1
cmd_tab_wide <- cmd_tab[, list(cmd_med_pre = max(cmd_med_pre, na.rm = TRUE), cmd_med_post = max(cmd_med_post, na.rm = TRUE), cmd_gp_event_first_med = max(cmd_gp_event_first_med, na.rm = TRUE)), by = "FK_Patient_ID"] # Convert to wide format by reshaping and creating a binary measure for the outcome
write.csv(cmd_tab_wide, "./Data/common_mental_health_meds.csv") # Save
rm(cmd_tab, cmd_tab_wide)
gc()



# Disconnect from the database
DBI::dbDisconnect(con)
