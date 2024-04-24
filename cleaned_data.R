employee_data <- read.csv('Employee Attrition.csv')
View(employee_data)

#Cleaning data - checking for missing data
# Checking for complete cases (rows with no missing values)
complete_rows <- complete.cases(employee_data)

# Counting the number of rows with missing values
num_rows_with_missing <- sum(!complete_rows)

cat("Number of rows with missing values:", num_rows_with_missing, "\n")


rows_with_missing <- !complete.cases(employee_data)

# Subset the dataset to show rows with missing values
rows_missing_data <- employee_data[rows_with_missing, ]

# Print the rows with missing data
print(rows_missing_data)


# Assuming your dataset is named 'employee_data'
cleaned_employee_data <- na.omit(employee_data)

# Print the cleaned dataset
View(cleaned_employee_data)

# renaming dataset fields
cleaned_employee_data <- cleaned_employee_data %>%
  rename(employee_id = Emp.ID,
         number_of_projects = number_project,
         average_monthly_hours = average_montly_hours,
         employee_tenure = time_spend_company,
         work_accidents = Work_accident,
         employee_department = dept,
         salary_level = salary)


#standardizing value names in employee_department column
cleaned_employee_data <- cleaned_employee_data %>%
  mutate(employee_department = case_when(
    tolower(employee_department) == "randd" ~ "Research & Development",  # Ensure "Randd" is correctly renamed
    employee_department == "product_mng" ~ "Product Management",
    tolower(employee_department) == "it" ~ "IT",  # Ensure "IT" is capitalized correctly
    tolower(employee_department) == "hr" ~ "HR",  # Ensure "HR" is capitalized correctly
    TRUE ~ str_to_title(employee_department)  # Capitalize other department names
  ))


