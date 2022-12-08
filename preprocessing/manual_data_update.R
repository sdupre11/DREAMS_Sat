library(aws.s3)
library(readxl)
library(paws)
library(jsonlite)

# connect ---------------------------------------

#Set the profile and region here
Sys.setenv(SECRET_NAME = Sys.getenv("SECRET_NAME"),
           AWS_DEFAULT_REGION = "us-east-1",
           AWS_REGION = "us-east-1")

svc <- secretsmanager()

#Put the name of the secret which contains the aws key info
see <- svc$get_secret_value(
  SecretId = Sys.getenv("SECRET_NAME")
)

see <- fromJSON(see$SecretString)

#Fill in the strings
Sys.setenv(AWS_ACCESS_KEY_ID = see$aws_access_key,
           AWS_SECRET_ACCESS_KEY = see$aws_secret_access_key,
           #AWS_PROFILE = Sys.getenv("AWS_PROFILE"),
           AWS_DEFAULT_REGION = "us-east-1",
           AWS_REGION = "us-east-1")

#Delete the secret info as its now an env variable
rm(see)
rm(svc)

# read from the main read bucket -----------------------------------------------
my_bucket <- Sys.getenv("TEST_BUCKET")

# Lists all of bucket contents, fill in your bucket
choices <- aws.s3::get_bucket(bucket = my_bucket)

# get just path names
choices <- lapply(choices, "[[", 1)

# get just file names
cleaned_choices <- lapply(choices, function(x) gsub(".*\\/", "", x))

# make dataframe of file names and path names
choices <- do.call(rbind, Map(data.frame, file_names = cleaned_choices,
                              path_names = choices, stringsAsFactors = FALSE))
# filter just files that end in txt or xlsx or csv
choices <- choices[grepl("txt$|xlsx$|csv$|xlsx$", choices$file_names), ]

print(choices)
# reset row names
rownames(choices) <- NULL

choices_recent_countries_all_historic <- subset(choices, grepl("MER_Structured_Datasets/Current_Frozen/PSNU_Historic/txt/", path_names))

choices_recent_countries_all2_historic <- subset(choices_recent_countries_all_historic, grepl("Botswana|Kenya|Lesotho|Zimbabwe", path_names))


my_data_historic <- 
  lapply(choices_recent_countries_all2_historic$path_names, function(the_file) {
    
    print(the_file)
    
    # read the data
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ), 
                                 bucket = my_bucket,
                                 object = the_file)

    res <- data %>% 
      filter(indicator == "AGYW_PREV" & sex == "Female" & (standardizeddisaggregate == "Age/Sex/Time/Complete" |standardizeddisaggregate == "Age/Sex/Time/Complete+")) %>%
      filter(country %in% c("Botswana", "Kenya", "Lesotho", "Zimbabwe")) %>%
      filter(!is.na(qtr4)) %>%
      group_by(country, snu1, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
        
  }) %>% 
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    AREA_NAME = case_when(
      (snu1 == "Central District" & country == "Botswana") ~ "CENTRAL",
      (snu1 == "Kgatleng District" & country == "Botswana") ~ "KGATLENG",
      (snu1 == "Kweneng District" & country == "Botswana") ~ "KWENENG",
      (snu1 == "North East District" & country == "Botswana") ~ "NORTH EAST",
      (snu1 == "South East District" & country == "Botswana") ~ "SOUTH EAST",
      (snu1 == "Southern" & country == "Botswana") ~ "SOUTHERN",
      (snu1 == "Homa Bay County" & country == "Kenya") ~ "HOMA BAY",
      (snu1 == "Kiambu County" & country == "Kenya") ~ "KIAMBU",
      (snu1 == "Kisumu County" & country == "Kenya") ~ "KISUMU",
      (snu1 == "Migori County" & country == "Kenya") ~ "MIGORI",
      (snu1 == "Mombasa County" & country == "Kenya") ~ "MOMBASA",
      (snu1 == "Nairobi County" & country == "Kenya") ~ "NAIROBI CITY",
      (snu1 == "Siaya County" & country == "Kenya") ~ "SIAYA",
      (snu1 == "Berea" & country == "Lesotho") ~ "BEREA",
      (snu1 == "Mafeteng" & country == "Lesotho") ~ "MAFETENG",
      (snu1 == "Maseru" & country == "Lesotho") ~ "MASERU",
      (snu1 == "Mohale's Hoek" & country == "Lesotho") ~ "MOHALE'S HOEK",
      (snu1 == "Bulawayo" & country == "Zimbabwe") ~ "BULAWAYO",
      (snu1 == "Manicaland" & country == "Zimbabwe") ~ "MANICALAND",
      (snu1 == "Mashonaland Central" & country == "Zimbabwe") ~ "MASHONALAND CENTRAL",
      (snu1 == "Matabeleland North" & country == "Zimbabwe") ~ "MATABELELAND NORTH",
      (snu1 == "Matabeleland South" & country == "Zimbabwe") ~ "MATABELELAND SOUTH",
      (snu1 == "Midlands" & country == "Zimbabwe") ~ "MIDLANDS"
    )
  )

choices_recent_countries_all_recent <- subset(choices, grepl("MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/", path_names))

choices_recent_countries_all2_recent <- subset(choices_recent_countries_all_recent, grepl("Botswana|Kenya|Lesotho|Zimbabwe", path_names))


my_data_recent <- 
  lapply(choices_recent_countries_all2_recent$path_names, function(the_file) {
    
    print(the_file)
    
    # read the data
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ), 
                                 bucket = my_bucket,
                                 object = the_file)
    
    res <- data %>% 
      filter(indicator == "AGYW_PREV" & sex == "Female" & (standardizeddisaggregate == "Age/Sex/Time/Complete" |standardizeddisaggregate == "Age/Sex/Time/Complete+")) %>%
      filter(country %in% c("Botswana", "Kenya", "Lesotho", "Zimbabwe")) %>%
      filter(!is.na(qtr4)) %>%
      group_by(country, snu1, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>% 
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    AREA_NAME = case_when(
      (snu1 == "Central District" & country == "Botswana") ~ "CENTRAL",
      (snu1 == "Kgatleng District" & country == "Botswana") ~ "KGATLENG",
      (snu1 == "Kweneng District" & country == "Botswana") ~ "KWENENG",
      (snu1 == "North East District" & country == "Botswana") ~ "NORTH EAST",
      (snu1 == "South East District" & country == "Botswana") ~ "SOUTH EAST",
      (snu1 == "Southern" & country == "Botswana") ~ "SOUTHERN",
      (snu1 == "Homa Bay County" & country == "Kenya") ~ "HOMA BAY",
      (snu1 == "Kiambu County" & country == "Kenya") ~ "KIAMBU",
      (snu1 == "Kisumu County" & country == "Kenya") ~ "KISUMU",
      (snu1 == "Migori County" & country == "Kenya") ~ "MIGORI",
      (snu1 == "Mombasa County" & country == "Kenya") ~ "MOMBASA",
      (snu1 == "Nairobi County" & country == "Kenya") ~ "NAIROBI CITY",
      (snu1 == "Siaya County" & country == "Kenya") ~ "SIAYA",
      (snu1 == "Berea" & country == "Lesotho") ~ "BEREA",
      (snu1 == "Mafeteng" & country == "Lesotho") ~ "MAFETENG",
      (snu1 == "Maseru" & country == "Lesotho") ~ "MASERU",
      (snu1 == "Mohale's Hoek" & country == "Lesotho") ~ "MOHALE'S HOEK",
      (snu1 == "Bulawayo" & country == "Zimbabwe") ~ "BULAWAYO",
      (snu1 == "Manicaland" & country == "Zimbabwe") ~ "MANICALAND",
      (snu1 == "Mashonaland Central" & country == "Zimbabwe") ~ "MASHONALAND CENTRAL",
      (snu1 == "Matabeleland North" & country == "Zimbabwe") ~ "MATABELELAND NORTH",
      (snu1 == "Matabeleland South" & country == "Zimbabwe") ~ "MATABELELAND SOUTH",
      (snu1 == "Midlands" & country == "Zimbabwe") ~ "MIDLANDS"
    )
  )

my_data <- rbind(my_data_historic, 
                 my_data_recent)

### A few basic data checks, if entries are found with no Q4 data, no cumulative data, but Q2 data...
### or if found with no data at all, pass on tp data managers for QC

# noQ4 <- my_data %>% 
#   filter(is.na(qtr4))
# 
# noQ4orCum <- my_data %>% 
#   filter((is.na(qtr4)&is.na(cumulative)))
# 
# Q2only <- my_data %>% 
#   filter(is.na(qtr4)) %>%
#   filter(is.na(cumulative)) %>%
#   filter(!is.na(qtr2))
# 
# NoData <- my_data %>% 
#   filter(is.na(qtr4)) %>%
#   filter(is.na(cumulative)) %>%
#   filter(is.na(qtr2))

# Q2only %>% 
#   write.csv(file = 'q2only.csv')


rm(my_data_historic)
rm(my_data_recent)
rm(choices_recent_countries)
rm(choices_recent_countries_all)
rm(choices_recent_countries_all_historic)
rm(choices_recent_countries_all_recent)
rm(choices_recent_countries_all2)
rm(choices_recent_countries_all2_historic)
rm(choices_recent_countries_all2_recent)
rm(my_data_qs_check)

###

# writing --------------
# write to dreams workspace

print("writing to system dreams...")
s3write_using(my_data, FUN = write.csv,
              bucket = Sys.getenv("TEST_BUCKET_WRITE"),
              object = "system_dreams_saturation/AGYW_PREVbyCountry.csv"
)


# test reading from write bucket --------
# read from dreams workspace

# print("reading dreams data...")
# read_testing <- s3read_using(FUN = read.csv,
#              bucket = Sys.getenv("TEST_BUCKET_WRITE"),
#              object = "system_dreams_saturation/AGYW_PREVbyCountry.csv") %>%
#   as.data.frame()

