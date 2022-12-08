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

choices_recent_countries_all <- subset(choices, grepl("MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/", path_names))

choices_recent_countries_all2 <- subset(choices_recent_countries_all, grepl("Botswana|Kenya|Lesotho|Zimbabwe", path_names))


my_data <- 
  lapply(choices_recent_countries_all2$path_names, function(the_file) {
    
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
      summarize(qtr2 = sum(as.numeric(qtr2)),
                qtr4 = sum(as.numeric(qtr4)),
                cumulative = sum(as.numeric(cumulative)))
        
  }) %>% 
  bind_rows()

### A few basic data checks

noQ4 <- my_data %>% 
  filter(is.na(qtr4))

noQ4orCum <- my_data %>% 
  filter((is.na(qtr4)&is.na(cumulative)))

Q2only <- my_data %>% 
  filter(is.na(qtr4)) %>%
  filter(is.na(cumulative)) %>%
  filter(!is.na(qtr2))

NoData <- my_data %>% 
  filter(is.na(qtr4)) %>%
  filter(is.na(cumulative)) %>%
  filter(is.na(qtr2))

###

# read mer structured data
# read options
#read in data, fill in your bucket name and file name (object should hold the name of the file you want to read)
# my_data <- "MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/MER_Structured_Datasets_PSNU_IM_Recent_Ukraine.txt"
# data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
#                              trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
#                              ), 
#                              bucket = my_bucket,
#                              object = my_data)
# 
# head(data, 5)


# writing --------------
# write to dreams workspace

print("writing to system dreams...")
s3write_using(my_data, FUN = write.csv,
              bucket = Sys.getenv("TEST_BUCKET_WRITE"),
              object = "system_dreams_saturation/write_testing.csv"
)


# test reading from write bucket --------
# read from dreams workspace

# print("reading dreams data...")
# read_testing <- s3read_using(FUN = read.csv,
#              bucket = Sys.getenv("TEST_BUCKET_WRITE"),
#              object = "system_dreams_saturation/write_testing.csv") %>% 
#   as.data.frame()

