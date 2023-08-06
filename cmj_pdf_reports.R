################################################################################
# Pacey PDF Tutorial
# By: Anthony Osnacz
################################################################################
################################################################################
# Libraries
################################################################################
# List of packages
options(repos = c(CRAN = "https://cloud.r-project.org/"))
packages <- c("tidyverse", "dplyr", "tryCatchLog","httr", 
              "tidyr", "janitor", "rlang", "RColorBrewer", "shinyWidgets",
              "reactablefmtr", "htmlwidgets", "pagedown", "htmltools", "base64enc")

# Function to load or install packages
load_or_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# Use the function
load_or_install_packages(packages)

################################################################################
# Hard Coded Variables and Functions
################################################################################
# This is a relative path so you can run the script on a virtual machine
local_path <- "~/pacey_project/"

# Function to round dataframes
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[, nums] <- round(df[, nums], digits = digits)
  
  (df)
}
################################################################################
# Load in our Man City Jump Tests
################################################################################
# Read in our csv, call clean names from janitor to help clean up our column names
# Try exploring faster way to read in data like .fst/.feather/postgresSQL
man_city_cmj_data <- clean_names(read.csv(paste0(local_path, "/man_city_data/CMJ.csv")))

# our date column is currently a string, lets extract the actual date from it
man_city_cmj_data <- man_city_cmj_data %>%
  separate(fecha_y_hora, c("date", "col_to_delete"), "_")

man_city_cmj_data$date <- as.Date(man_city_cmj_data$date)

################################################################################
# Clean up the data
################################################################################
# Here we are filtering out jumps that are obviously too high or too low
man_city_cmj_data <- dplyr::filter(man_city_cmj_data, altura >= 10 & altura <= 80)

# Here we are filtering out jumps that are outliers
tryCatch(
  {
    # Wrapping functions in tryCatches is good practice as it prevents script failure and provides the error message for debugging
    man_city_cmj_data <- dplyr::group_by(man_city_cmj_data, nombre_de_atleta, date) %>%
      dplyr::mutate(
        mean_outlier_detect = mean(altura, na.rm = T),
        sd_outlier_detect = sd(altura, na.rm = T),
        cv_by_trial = (sd(altura, na.rm = T) / mean(altura, na.rm = T)) * 100,
        Tmin = mean_outlier_detect - (3 * sd_outlier_detect),
        Tmax = mean_outlier_detect + (3 * sd_outlier_detect)
      ) %>%
      ungroup()
  },
  error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  }
)

tryCatch(
  {
    # if a row is identified as an outlier it's labeled
    man_city_cmj_data <- man_city_cmj_data %>% dplyr::mutate(is_outlier = ifelse(altura < Tmin | altura > Tmax,
                                                                                 paste0("yes"),
                                                                                 paste0("no")))
  },
  error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  }
)

# filter out the data by label
man_city_cmj_data <- dplyr::filter(man_city_cmj_data, is_outlier != "yes" | is.na(is_outlier))

################################################################################
# Mutating the Data to Fit the Demands of our Report
################################################################################
# Here we are getting the athlete means by day after removing outliers 
tryCatch(
  {man_city_cmj_data <- group_by(
    man_city_cmj_data,
    nombre_de_atleta,
    id_de_atleta,
    date
  ) %>%
    dplyr::mutate(
      altura = mean(altura, na.rm = T),
      potencia = mean(potencia, na.rm = T),
      tv = mean(tv, na.rm = T),
      velocidad_inicial = mean(velocidad_inicial, na.rm = T)
    )},
  error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  }
)

# Let's also remove columns we don't plan on using and that could disrupt us getting unique rows
man_city_cmj_data$id_de_salto <- NULL
man_city_cmj_data$col_to_delete <- NULL

# Run distinct to remove duplicate data
man_city_cmj_data <- distinct(man_city_cmj_data)

# Let's filter so each athlete only has 1 instance in the norms
tryCatch(
  {man_city_cmj_data_norms <- group_by(
    man_city_cmj_data,
    nombre_de_atleta,
    id_de_atleta
  ) %>%
    dplyr::filter(date == max(date))},
  error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  }
)

# Let's create a team column 
man_city_cmj_data_norms$team <- "Man City"

# Let's get normative data for our team
tryCatch(
  {man_city_cmj_data_norms <- group_by(
    man_city_cmj_data_norms,
    team
  ) %>%
    dplyr::summarise(
      mean_altura = mean(altura, na.rm = T),
      mean_potencia = mean(potencia, na.rm = T),
      mean_tv = mean(tv, na.rm = T),
      mean_velocidad_inicial = mean(velocidad_inicial, na.rm = T),
      sd_altura = sd(altura, na.rm = T),
      sd_potencia = sd(potencia, na.rm = T),
      sd_tv = sd(tv, na.rm = T),
      sd_velocidad_inicial = sd(velocidad_inicial, na.rm = T),
      eight_four_percentile_altura = quantile(altura, 0.841),
      sixteen_percentile_altura = quantile(altura, 0.159),
      eight_four_percentile_potencia = quantile(potencia, 0.841),
      sixteen_percentile_potencia = quantile(potencia, 0.159),
      eight_four_percentile_tv = quantile(tv, 0.841),
      sixteen_percentile_tv = quantile(tv, 0.159),
      eight_four_percentile_velocidad_inicial = quantile(velocidad_inicial, 0.841),
      sixteen_percentile_velocidad_inicial = quantile(velocidad_inicial, 0.159)
    )},
  error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  }
)

man_city_cmj_data$team <- "Man City"

# Let's merge our data together so now we have norms for each row to use
man_city_cmj_data_final <- merge(man_city_cmj_data, man_city_cmj_data_norms)

# Let's select only the columns we're interested in for this specific report
# Since we want a report on the team's jump heights we only grab those metrics
man_city_cmj_data_height <- select(
  man_city_cmj_data_final,
  team,
  nombre_de_atleta,
  date,
  altura,
  mean_altura,
  sd_altura,
  eight_four_percentile_altura,
  sixteen_percentile_altura
)

# Let's add a description of the test type
man_city_cmj_data_height$test_type <- "CMJ Height (cm)"

# Let's translate the columns to English
man_city_cmj_data_height <- rename(man_city_cmj_data_height,
                                   athlete = nombre_de_atleta,
                                   value = altura,
                                   mean_value = mean_altura,
                                   sd_value = sd_altura,
                                   eight_four_percentile = eight_four_percentile_altura,
                                   sixteen_percentile = sixteen_percentile_altura
)

# Now round the dataframe to 1 decimal place
man_city_cmj_data_height <- round_df(man_city_cmj_data_height, 1)

# Order the dataframe by date, grouped by 'athlete' and 'test_type'
man_city_cmj_data_height <- man_city_cmj_data_height %>%
  arrange(athlete, test_type, date)

# Extract the last three rows since we want to display an average of the athlete's last 3 tests
last_three_values <- group_by(man_city_cmj_data_height,
                              athlete, 
                              test_type) %>% slice(1:3) %>% ungroup()

# Calculate the average of the values
tryCatch({
  last_three_values <- group_by(last_three_values,
                                athlete, 
                                test_type) %>% mutate(last_three_avg = mean(value)) %>% ungroup()
},
error = function(e) {
  cat("ERROR :", conditionMessage(e), "\n")
}
)

# create a df to store the last 3 average
last_three_values <- select(last_three_values, athlete, date, test_type, last_three_avg)

# Merge the data together on athlete, date, and test type columns
man_city_cmj_data_height <- merge(man_city_cmj_data_height, 
                                  last_three_values, 
                                  by = c('athlete', 'date', 'test_type'), all = T) 

# Sort our data so it goes from oldest to newest
man_city_cmj_data_height <- man_city_cmj_data_height %>% arrange(as.Date(date))

# Here we get the data over time to use for a sparkline in our report later!
man_city_cmj_data_height <- mutate(group_by(man_city_cmj_data_height, athlete, test_type),
                                   historical = list(value)) %>% ungroup()

# filter for the max date to avoid having old data in our report
man_city_cmj_data_height_table <- filter(group_by(man_city_cmj_data_height, athlete, test_type),
                                         date == max(as.Date(date))) %>% ungroup()

# using our norms from before we can calculate a z-score
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>%
  dplyr::mutate(
    zscore_value = (value - mean_value) / sd_value
  ) %>%
  ungroup()

# and with that z-score we can get our percentile ranking for athletes
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>%
  dplyr::mutate(
    percentile_value = pnorm(zscore_value) * 100
  ) %>%
  ungroup()

# Let's add a column that interprets the percentile for coaches
# we could go off of the normal distribution cutoffs where anything more than 2 sd's is excellent or very poor
# but this tends to be a little bit more descriptive to coaches so use your own intuition here
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>% dplyr::mutate(
  status = ifelse(percentile_value >= 85, paste0("Excellent"),
                  ifelse(percentile_value < 85 & percentile_value >= 60, paste0("Good"),
                         ifelse(percentile_value < 60 & percentile_value >= 40, paste0("OK"),
                                ifelse(percentile_value < 40 & percentile_value >= 15, paste0("Poor"),
                                       ifelse(percentile_value < 15 & percentile_value >= 0, paste0("Very Poor"),
                                              paste0("Missing Data")
                                       )
                                )
                         )
                  )
  )
)

# this is also an arbitrary threshold use your own discretion
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>% dplyr::mutate(
  focus = ifelse(percentile_value >= 60, paste0("Maintain"),
                 paste0("Improve")
  )
)

# Assign colors to each status 
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>%
  mutate(
    cols_status = case_when(
      status == "Excellent" ~ "#69B34C",
      status == "Good" ~ "#ACB334",
      status == "OK" ~ "#FAB733",
      status == "Poor" ~ "#FF7676",
      status == "Very Poor" ~ "#FF0D0D",
      TRUE ~ "grey"
    )
  )

# Assign colors to each focus 
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>%
  mutate(
    cols_focus = case_when(
      focus == "Maintain" ~ "#69B34C",
      focus == "Improve" ~ "#FF0D0D",
      TRUE ~ "grey"
    )
  )

# Assign colors to sparkline, we're just recycling here so it's all black
man_city_cmj_data_height_table <- man_city_cmj_data_height_table %>%
  mutate(
    cols_graph = case_when(
      focus == "Maintain" ~ "black",
      focus == "Improve" ~ "black",
      TRUE ~ "black"
    )
  )

# select relevant columns from our dataframe
man_city_cmj_data_height_table <- man_city_cmj_data_height_table[, c(
  "athlete",
  "date",                 
  "test_type",
  "team",               
  "value", 
  "mean_value", 
  "last_three_avg", 
  "status",                
  "focus", 
  "historical",    
  "sd_value", 
  "eight_four_percentile", 
  "sixteen_percentile", 
  "zscore_value",          
  "percentile_value",  
  "cols_status",           
  "cols_focus",   
  "cols_graph"  
)]

# Remove any duplicates that may have snuck in
man_city_cmj_data_height_table <- distinct(man_city_cmj_data_height_table)

# Filter data for just our CMJ Heights
man_city_cmj_data_height_table <- dplyr::filter(
  distinct(man_city_cmj_data_height_table),
  test_type == "CMJ Height (cm)"
)

# Arrange the data by values
man_city_cmj_data_height_table <- distinct(man_city_cmj_data_height_table) %>%
  arrange(desc(value))

# Define the path to your image
img_path <- paste0(local_path, "styles/man_city_logo.png")

# Read the image file and convert to Base64
img_data <- base64encode(img_path)

# Create the data URI
img_uri <- paste0("data:image/png;base64,", img_data)

# Round dataframe to 1 decimal place
man_city_cmj_data_height_table <- round_df(man_city_cmj_data_height_table, 1)

################################################################################
# Make table
################################################################################
# Build our table using reactable!
man_city_cmj_data_height_table <- reactable(
  man_city_cmj_data_height_table,
  theme = fivethirtyeight(),
  resizable = FALSE, # remove html options since it's a pdf report
  pagination = FALSE,
  searchable = FALSE,
  defaultPageSize = 50, # can play with this as you please
  defaultColDef = colDef(align = 'center'), # center data
  showPageSizeOptions = FALSE,
  highlight = FALSE,
  columns = list(
    cols_status = colDef(show = FALSE), # hide columns
    cols_focus = colDef(show = FALSE),
    cols_graph = colDef(show = FALSE),
    sd_value = colDef(show = FALSE),
    team = colDef(show = FALSE),
    eight_four_percentile = colDef(show = FALSE),
    sixteen_percentile = colDef(show = FALSE),
    zscore_value = colDef(show = FALSE),
    percentile_value = colDef(show = FALSE),
    test_type = colDef(
      name = "Test Type",
      maxWidth = 250
    ),
    date = colDef(
      name = "Date",
      maxWidth = 250
    ),
    athlete = colDef(
      name = "Name",
      maxWidth = 1000
    ),
    value = colDef(
      name = "Value",
      maxWidth = 250
    ),
    mean_value = colDef(
      name = "Team Average",
      maxWidth = 250
    ),
    last_three_avg = colDef(
      name = "Athlete Average",
      maxWidth = 250
    ),
    status = colDef(
      name = "Status",
      maxWidth = 250,
      cell = pill_buttons(
        data = man_city_cmj_data_height_table,
        color_ref = "cols_status",
        box_shadow = FALSE
      )
    ),
    focus = colDef(
      name = "Focus",
      maxWidth = 250,
      cell = pill_buttons(
        data = man_city_cmj_data_height_table,
        color_ref = "cols_focus",
        box_shadow = FALSE
      )
    ),
    historical = colDef(
      name = "Historical",
      cell = react_sparkline(
        man_city_cmj_data_height_table,
        height = 40,
        line_color_ref = "cols_graph",
        highlight_points = highlight_points(min = "black", max = "black"),
        labels = c("min", "max"),
        statline = "mean",
        bandline = "innerquartiles",
        tooltip_type = 2
      )
    )
  )
) %>% # this is our table's title
  reactablefmtr::add_title(
    title = html(paste0("<img style='vertical-align:middle;' src='", img_uri, "' width=150 height=150><div style='vertical-align:middle; display:inline;'> Manchester City CMJ Data</div>")),
    align = "left",
    margin = reactablefmtr::margin(t = 10, r = 0, b = 2, l = 0)
  )

# since reactable uses html we must save it first as such 
html <- paste0(local_path, "pdf_reports/", unique(max(man_city_cmj_data_height_table$date)), "man_city_cmj_data_height_table.html")

# adding some formatting to the table so that everything in our report fits right, you can adapt these to your needs
browsable(
  tagList(
    tags$head(
      tags$style(
        HTML("
        body {
          width: auto !important;
          height: auto !important;
        }
        .reactable {
          table-layout: auto !important;
          width: 100% !important;
        }
        .reactable th {
          text-align: center;  # Center the column names
        }
        ")
      )
    ),
    man_city_cmj_data_height_table  # Put the HTML widget directly
  )
) -> man_city_cmj_data_height_table_fixed_size

# save the final html product
htmltools::save_html(man_city_cmj_data_height_table_fixed_size, html)

# now we save the table as a pdf by using a function that opens up the html and snaps a pic of it
pdf_file <- paste0(local_path, "pdf_reports/", unique(max(man_city_cmj_data_height_table$date)), "man_city_cmj_data_height_table.pdf")
pagedown::chrome_print(html, output = pdf_file, options = list(landscape = TRUE))

# Congrats! You just created your first pdf report using R. Please try to build off of this! There is a great package called SlackR you can use to post reports to slack
# You can put this script on a virtual machine and have reports post to a channel every X time, etc. Please be creative and expand upon this. The best way to learn is to challenge
# yourself. Also use chatGPT, it can help accelerate the learning process. Thanks! - Anthony 
