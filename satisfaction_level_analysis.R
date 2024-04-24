# 1. Calculating average satisfaction level by department
avg_satisfaction_department <- cleaned_employee_data %>%
  group_by(employee_department) %>%
  summarise(avg_satisfaction = mean(satisfaction_level, na.rm = TRUE))
print(avg_satisfaction_department)

# Defining colors for each department
department_colors <- c("Accounting" = "blue",
                       "HR" = "#F28500",
                       "IT" = "#8F00FF",
                       "Management" = "#50C878",
                       "Marketing" = "#6D071A",
                       "Product Management" = "#0D98BA",
                       "Research & Development" = "#FFDB58",
                       "Sales" = "#FF0000",
                       "Support" = "#954535",
                       "Technical" = "#35374B")

# Visualization of average satisfaction level by department without a legend
department_plot <- ggplot(avg_satisfaction_department, aes(x = reorder(employee_department, -avg_satisfaction), y = avg_satisfaction, fill = employee_department)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Satisfaction Level by Department", x = "Department", y = "Average Satisfaction Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  
  scale_fill_manual(values = department_colors, guide = 'none')

# Saving the plot as an image file
ggsave("department_plot.png", plot = department_plot, width = 10, height = 6)



# 2. Calculating the average satisfaction level by salary level
avg_satisfaction_salary <- cleaned_employee_data %>%
  group_by(salary_level) %>%
  summarise(avg_satisfaction = mean(satisfaction_level, na.rm = TRUE))
print(avg_satisfaction_salary)

# Defining colors for each salary level
salary_colors <- c("low" = "red",
                   "medium" = "#F28500",
                   "high" = "purple")


# Visualization of average satisfaction level by salary level
salary_plot <- ggplot(avg_satisfaction_salary, aes(x = salary_level, y = avg_satisfaction, fill = salary_level)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Satisfaction Level by Salary Level", x = "Salary Level", y = "Average Satisfaction Level") +
  scale_fill_manual(values = salary_colors, guide = 'none')

# Saving the plot as an image file
ggsave("salary_plot.png", plot = salary_plot, width = 10, height = 6)

# 3. Satisfaction Level with monthly hours worked

# Setting the seed for reproducibility
set.seed(123)

# Selecting a random sample of indices
sample_indices <- sample(1:nrow(cleaned_employee_data), 375)

# Subset the dataset using the sampled indices
sampled_employee_data <- cleaned_employee_data[sample_indices, ]

# Scatter plot of job satisfaction levels vs. average monthly hours worked for the sampled data
monthlyhours_scatter_plot <- ggplot(sampled_employee_data, aes(x = average_monthly_hours, y = satisfaction_level)) +
  geom_point(alpha = 0.5) +  # Add transparency to points
  labs(title = "Relationship Between Job Satisfaction and Average Monthly Hours Worked",
       x = "Average Monthly Hours Worked",
       y = "Job Satisfaction") +
  theme_minimal()

# Saving the plot as an image file
ggsave("monthlyhours_scatter_plot.png", plot = monthlyhours_scatter_plot, width = 10, height = 6)

# Calculating correlation coefficient for the sampled data
correlation <- cor(sampled_employee_data$average_monthly_hours, sampled_employee_data$satisfaction_level)
cat("Correlation coefficient for sampled data:", correlation, "\n")


# 4. Promotion and satisfaction level

# Subset of the data for employees who received promotions and those who did not
promoted <- cleaned_employee_data %>%
  filter(promotion_last_5years == 1) %>%
  select(satisfaction_level)

not_promoted <- cleaned_employee_data %>%
  filter(promotion_last_5years == 0) %>%
  select(satisfaction_level)

# Performing a t-test to compare job satisfaction levels between the two groups
t_test_result <- t.test(promoted$satisfaction_level, not_promoted$satisfaction_level)

print(t_test_result)


# Creating a density plot of job satisfaction levels for both groups
plot <- ggplot(cleaned_employee_data, aes(x = satisfaction_level, fill = factor(promotion_last_5years))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Job Satisfaction Levels by Promotion Status",
       x = "Job Satisfaction Level",
       y = "Density",
       fill = "Promotion Last 5 Years") +
  scale_fill_discrete(name = "Promotion Last 5 Years", labels = c("No", "Yes")) +
  theme_minimal()

# Setting the desired width and height of the plot
ggsave("density_plot.png", plot, width = 10, height = 6)

# 5.Employee tenure and job satisfaction
# Setting seed for reproducibility
set.seed(123)

# Selecting random sample of 375 observations
sampled_data <- cleaned_employee_data[sample(nrow(cleaned_employee_data), 375), ]

# Scatter plot of job satisfaction levels vs. tenure
tenure_scatter_plot <- ggplot(sampled_data, aes(x = employee_tenure, y = satisfaction_level)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Relationship Between Job Satisfaction and Tenure (Sampled Data)",
       x = "Tenure (Years)",
       y = "Job Satisfaction") +
  theme_minimal()

# Saving the plot as an image file
ggsave("tenure_scatter_plot.png", plot = tenure_scatter_plot, width = 10, height = 6)

# Calculating correlation coefficient
correlation <- cor(sampled_data$employee_tenure, sampled_data$satisfaction_level)
cat("Correlation coefficient:", correlation, "\n")


# 6.Employee work accidents and job satisfaction
# Performing independent samples t-test
t_test_result <- t.test(satisfaction_level ~ work_accidents, data = cleaned_employee_data)

print(t_test_result)


# Creating a density plot of job satisfaction levels for both groups
workaccidents_plot <- ggplot(cleaned_employee_data, aes(x = satisfaction_level, fill = factor(work_accidents))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Job Satisfaction Levels by Work Accidents",
       x = "Job Satisfaction Level",
       y = "Density",
       fill = "Work Accidents") +
  scale_fill_discrete(name = "Work Accidents", labels = c("No", "Yes")) +
  theme_minimal()

# Setting the desired width and height of the plot
ggsave("workaccidents_plot.png", plot = workaccidents_plot , width = 10, height = 6)


# 7. Performance Evaluation and Satisfaction Levels 
# Setting seed for reproducibility
set.seed(123)

# Selecting random sample of 375 observations
sampled_data <- cleaned_employee_data[sample(nrow(cleaned_employee_data), 375), ]

# Creating scatter plot
performance_scatter_plot <- ggplot(sampled_data, aes(x = last_evaluation, y = satisfaction_level)) +
  geom_point(alpha = 0.5) +  # Add transparency to points
  labs(title = "Relationship Between Performance Evaluation and Job Satisfaction",
       x = "Performance Evaluation Score",
       y = "Job Satisfaction Level") +
  theme_minimal()

# Setting the desired width and height of the plot
ggsave("performance_scatter_plot.png", plot = performance_scatter_plot, width = 10, height = 6)


# Calculating correlation coefficient
correlation <- cor(sampled_data$last_evaluation, sampled_data$satisfaction_level)
cat("Correlation coefficient:", correlation, "\n")