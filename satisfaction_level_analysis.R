# Calculating average satisfaction level by department
avg_satisfaction_department <- cleaned_employee_data %>%
  group_by(employee_department) %>%
  summarise(avg_satisfaction = mean(satisfaction_level, na.rm = TRUE))
print(avg_satisfaction_department)

# Define colors for each department
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
ggplot(avg_satisfaction_department, aes(x = reorder(employee_department, -avg_satisfaction), y = avg_satisfaction, fill = employee_department)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Satisfaction Level by Department", x = "Department", y = "Average Satisfaction Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  
  scale_fill_manual(values = department_colors, guide = 'none')


# Calculating the average satisfaction level by salary level
avg_satisfaction_salary <- cleaned_employee_data %>%
  group_by(salary_level) %>%
  summarise(avg_satisfaction = mean(satisfaction_level, na.rm = TRUE))
print(avg_satisfaction_salary)

# Defining colors for each salary level
salary_colors <- c("low" = "red",
                   "medium" = "#F28500",
                   "high" = "purple")

# Visualization of average satisfaction level by salary level
ggplot(avg_satisfaction_salary, aes(x = salary_level, y = avg_satisfaction, fill = salary_level)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Satisfaction Level by Salary Level", x = "Salary Level", y = "Average Satisfaction Level") +
  scale_fill_manual(values = salary_colors, guide = 'none') 