#1.Importing NIPostcode dataset
Read_NIpostcode_dataset <- read.csv("NIPostcodes.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = c("","NA"))
Read_NIpostcode_dataset

#Showing total number of rows 
nrow(Read_NIpostcode_dataset)

#Showing Structure of the dataset
str(Read_NIpostcode_dataset)

#First 10 rows of the dataset
head(Read_NIpostcode_dataset, n = 10)


# 2. Adding suitable title to each attribute 

column_names <- c("Organisation_Name","Sub_building_Name","Building_Name","Number","Primary_Thorfare",
                  "Alt_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","Postcode",
                  "x_coordinates","y_coordinates","Primary_Key")
colnames(Read_NIpostcode_dataset) <- column_names
head(Read_NIpostcode_dataset)
str(Read_NIpostcode_dataset)


# 4. Number of missing values in each column
sum_missing_values <- sapply(Read_NIpostcode_dataset, function(x) sum(is.na(x)))
mean_missing_values <- sapply(Read_NIpostcode_dataset, function(x) mean(is.na(x)))
sum_missing_values
mean_missing_values


# 3. Removing the data with missing values
Read_NIpostcode_dataset <- Read_NIpostcode_dataset[, - c(1, 2, 3, 6, 7)]
Read_NIpostcode_dataset
str(Read_NIpostcode_dataset)



# 5. Modify the county attribute to the categorising factor
Read_NIpostcode_dataset$County <- factor(Read_NIpostcode_dataset$County)
head(Read_NIpostcode_dataset)
str(Read_NIpostcode_dataset)


# 6. Moving primary key to start of dataframe
Read_NIpostcode_dataset = Read_NIpostcode_dataset[, c(10, 1:9)]
head(Read_NIpostcode_dataset)
str(Read_NIpostcode_dataset)


# 7Create a new dataset called Limavady_data
Limavady_data <- subset(Read_NIpostcode_dataset, grepl("LIMAVADY", Locality) & 
                          grepl("LIMAVADY", Townland) & grepl("LIMAVADY", Town))
head(Limavady_data)
str(Limavady_data)

#Store it as a csv.
write.csv(Limavady_data, file = "Limavady.csv", row.names = FALSE)




# 8. Save the modified dataset in a csv file called CleanNIPostcodeData
write.csv(Read_NIpostcode_dataset, file = "CleanNIPostCodeData.csv ", quote = FALSE, row.names = FALSE)
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv ")
head(CleanNIPostcodeData)
str(CleanNIPostcodeData)
