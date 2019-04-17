# 1.Amalgamating all crime data into single file.

Read_all_csv_file <- list.files(path = "NI_Crime_Datasets", pattern = "*.csv", recursive = TRUE)
Read_all_csv_file

merge_files <- function(list_of_all_files) {
  NI_crime_file <- NULL
  for (i in list_of_all_files) {
    file <- read.csv(header = TRUE, paste("NI_Crime_Datasets/", i, sep = ""), stringsAsFactors = FALSE)
    NI_crime_file <- rbind(NI_crime_file, file)
  }
  return(NI_crime_file)
  
}
NIcrimeData <- merge_files(Read_all_csv_file)
head(NIcrimeData)
str(NIcrimeData)


#writing data into AllNICrimeData csv file
write.csv(NIcrimeData, file = "AllNICrimeData.csv", quote = FALSE, na = "",
          row.names = FALSE)

AllNICrimeData <- read.csv("AllNICrimeData.csv", stringsAsFactors = FALSE)

#No. of rows in AllNICrime file
NROW(AllNICrimeData)
head(AllNICrimeData)
str(AllNICrimeData)


# 2.Removing the unwanted columns
AllNICrimeData <- AllNICrimeData[, c(2, 5, 6, 7, 10)]
head(AllNICrimeData)

# Removing the unwanted columns
str(AllNICrimeData)


#3. Factorizing crime_type attribute
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
head(AllNICrimeData, 10)
str(AllNICrimeData)



# 4.Modifying location attribute
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)
head(AllNICrimeData, 10)
str(AllNICrimeData)



# 5.moving all null values from crime dataset.
new_AllNICrimeData <- na.omit(AllNICrimeData) 
# taking random 100 records
random_crime_sample <- new_AllNICrimeData[sample(1:nrow(new_AllNICrimeData), 1000, replace = TRUE),]
is.na(random_crime_sample)
head(random_crime_sample)
nrow(random_crime_sample)
str(random_crime_sample)


library(dplyr)

#reading CleanNIPostcodeData.csv file and storing into data frame Read_postcode_data
Read_postcode_data= read.csv("CleanNIPostcodeData.csv", stringsAsFactors = FALSE)
head(Read_postcode_data)
nrow(Read_postcode_data)


#Loading only the Primary.Thorfare and Postcode to the tbl_df data frame
postcode_dataset <- tbl_df(Read_postcode_data[, c(3, 8)])
str(postcode_dataset)
head(postcode_dataset, 10)

#function
find_a_postcode <- function(Location) {
  #Filtering the row matching the location using the filter function of dplyr library
  data_filter <- filter(postcode_dataset, Primary_Thorfare == Location)
  #Finding the most repeated value and extracting its postcode using which.names function
  result <- names(which.max(table(data_filter$Postcode)))
  return(result)
  
}


#Finding the postcode for each location using lapply function 
postcodes_function <- lapply(random_crime_sample$Location, find_a_postcode)
str(postcodes_function)
head(postcodes_function)
is.na(postcodes_function)

# stored the postcodes_function results into random_crime_sample dataframe
postcodes_function <- sapply(postcodes_function, paste0, collaspse= "")
random_crime_sample$postcode <- postcodes_function
head(random_crime_sample)
str(random_crime_sample)
nrow(random_crime_sample)


# 6 Save the modified dataset in a csv file called random_crime_sample.csv
write.csv(random_crime_sample, file = "random_crime_sample.csv", quote = FALSE, row.names = FALSE)


# 7 extract the data from random_crime_sample.csv into new data frame updated_random_sample
updated_random_sample <- read.csv("random_crime_sample.csv", header = TRUE, stringsAsFactors = FALSE)
updated_random_sample$postcode <- factor(chart_data$postcode)
head(updated_random_sample)
str(updated_random_sample)
View(updated_random_sample)

# Eleminating the blank values from the dataset
updated_random_sample[updated_random_sample$postcode==""] <- "NA"
updated_random_sample <- na.omit(updated_random_sample)
sum(is.na(updated_random_sample))

# moving the data from updated_random_sample into chart_data data frame
chart_data  <- updated_random_sample
head(chart_data)
str(chart_data)

#sort the data based on the columns postcode and crimetype
chart_data <- chart_data[with(chart_data,order(postcode,Crime.type)),]
head(chart_data)
chart_data <- na.omit(chart_data)
str(chart_data)
sum(is.na(chart_data))


#summary stats for crime type
summary(chart_data)

#occurence of each crime type in eachpostcode.
tapply(chart_data$Crime.type, chart_data$postcode,  summary)

#total occurence of each crime
table(chart_data$Crime.type)
str(chart_data)
head(chart_data)

#Plotting the final results in Bar chart
counts <- table(chart_data$Crime.type)
barplot(counts, main="CRIMR REPORT",
        xlab="TYPE OF CRIME", ylab = "COUNT OF THE CRIME")
