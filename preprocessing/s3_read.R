library(aws.s3)
library(readxl)
library(paws)


s3_read <- function(bucket) {
  
  # params and structuring ----
  my_bucket <- bucket
  
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
  
  if(Sys.getenv("SECRET_ID") == "system_narratives") {
    print("reading narratives restricted data...")
    
    my_data <- "Narratives/Current_Frozen/xlsx/Narratives.xlsx"
    data <- aws.s3::s3read_using(FUN = readxl::read_xlsx,
                                 trim_ws = TRUE, 
                                 bucket = my_bucket,
                                 object = my_data)
    
    head(data, 5)
    
  } else {
    print("reading yoda restricted data...")
    # read options
    #read in data, fill in your bucket name and file name (object should hold the name of the file you want to read)
    my_data <- "MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/MER_Structured_Datasets_PSNU_IM_Recent_Ukraine.txt"
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ), 
                                 bucket = my_bucket,
                                 object = my_data)
    
    head(data, 5)
    
  }
  
  
  
}



