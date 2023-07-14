# Load the library
library(tidyverse)
# Load the necessary packages
library(ggplot2)
# Install and load the gtsummary package
install.packages("gtsummary")
library(gtsummary)


# Reshape the data
data_long <- CABtalShort %>%
  gather(key = "TAL", value = "Response", starts_with("TAL")) %>%
  group_by(rgn, TAL) %>%
  summarise(Response = sum(Response)) %>%
  filter(Response > 0)

# Plot the data
ggplot(data_long, aes(x = rgn, y = Response, fill = TAL)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "Count of Responses", fill = "TAL item", title = "Distribution of TAL data across regions")


# Join the datasets
data_long <- left_join(data_long, CABtalShortAlias, by = "TAL")

# Replace the TAL column with the Alias column
data_long <- data_long %>% 
  mutate(TAL = ifelse(!is.na(Alias), Alias, TAL)) %>%
  select(-Alias)

# Create the plot
ggplot(data_long, aes(x = rgn, y = Response, fill = TAL)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "Count of Responses", fill = "TAL item alias", title = "Distribution of TAL data across regions")

# First, reshape the data to count the number of occurrences for each occupation in each region
data_ocp <- CABtalShort %>%
  group_by(rgn, ocp) %>%
  summarise(count = n(), .groups = "keep")

# Create the plot
ggplot(data_ocp, aes(x = rgn, y = count, fill = ocp)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "Count", fill = "Occupation", title = "Distribution of occupations by region")

head(CABtalShort)
# Filter rows where ocp equals "Administration" and count the number of such rows
admin_count <- CABtalShort %>%
  filter(ocp == "Administration;") %>%
  nrow()

print(paste("The number of 'Administration' in 'ocp':", admin_count))

# Aggregate the data
data_agg <- data_long %>%
  group_by(rgn, TAL) %>%
  summarise(sum = sum(Response), .groups = "keep")

# Create the plot
ggplot(data_agg, aes(x = rgn, y = TAL, fill = sum)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "TAL item alias", fill = "Sum of Responses", 
       title = "Heatmap of TAL responses across regions")


# Aggregate the data
data_agg <- data_long %>%
  group_by(rgn, TAL) %>%
  summarise(sum = sum(Response), .groups = "keep")

# Create the plot
ggplot(data_agg, aes(x = rgn, y = TAL, fill = sum)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +  # Changed gradient colors to yellow and red
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "TAL item alias", fill = "Sum of Responses", 
       title = "Heatmap of TAL responses across regions")


# Aggregate the data
data_agg <- data_long %>%
  group_by(rgn, TAL) %>%
  summarise(sum = sum(Response), .groups = "keep")

# Calculate the total sum of responses for each TAL item
TAL_order <- data_agg %>%
  group_by(TAL) %>%
  summarise(total_sum = sum(sum), .groups = "keep") %>%
  arrange(desc(total_sum)) %>%
  pull(TAL)

# Create the plot, ordering the TAL items by total sum of responses
ggplot(data_agg, aes(x = rgn, y = factor(TAL, levels = TAL_order), fill = sum)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "TAL item alias", fill = "Sum of Responses", 
       title = "Heatmap of TAL responses across regions")


# Aggregate the data
data_agg <- data_long %>%
  group_by(rgn, TAL) %>%
  summarise(sum = sum(Response), .groups = "keep")

# Calculate the total sum of responses for each TAL item
TAL_order <- data_agg %>%
  group_by(TAL) %>%
  summarise(total_sum = sum(sum), .groups = "keep") %>%
  arrange(desc(total_sum)) %>%
  pull(TAL)

# Reverse the order of TAL items
TAL_order <- rev(TAL_order)

# Create the plot, ordering the TAL items by total sum of responses (reversed)
ggplot(data_agg, aes(x = rgn, y = factor(TAL, levels = TAL_order), fill = sum)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "TAL item alias", fill = "Sum of Responses", 
       title = "Heatmap of TAL responses across regions")


# Aggregate the data
data_agg <- data_long %>%
  group_by(rgn, TAL) %>%
  summarise(sum = sum(Response), .groups = "keep")

# Create the plot, ordering the TAL items alphabetically
ggplot(data_agg, aes(x = rgn, y = TAL, fill = sum)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "TAL item alias", fill = "Sum of Responses", 
       title = "Heatmap of TAL responses across regions")


# Aggregate the data
data_agg <- data_long %>%
  group_by(rgn, TAL) %>%
  summarise(sum = sum(Response), .groups = "keep")

# Create the plot, ordering the TAL items in reverse alphabetical order
ggplot(data_agg, aes(x = rgn, y = factor(TAL, levels = rev(sort(unique(TAL)))), fill = sum)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "TAL item alias", fill = "Sum of Responses", 
       title = "Heatmap of TAL responses across regions")


# Create the plot
ggplot(CABtalShort, aes(x = id, y = svc)) +
  geom_point(shape = 21, fill = "red", size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "ID", y = "Service",
       title = "Heatmap with Circles for CABtalShort data")

# Calculate the number of services each person responded to
services_per_person <- CABtalShort %>%
  group_by(id) %>%
  summarise(num_services = n_distinct(svc))

# Divide the number of services by 10
services_per_person$num_services <- services_per_person$num_services / 10

# Merge the services_per_person data with the original dataset
merged_data <- left_join(CABtalShort, services_per_person, by = "id")

# Plot the number of occupations by the number of services each person responded to
ggplot(merged_data, aes(x = num_services, y = ocp)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Number of Services per Person (divided by 10)", y = "Occupation",
       title = "Number of Occupations that submitted Responses to TAL Survey (total 27)")


#-------------------- above in different color per bar----------
# Calculate the number of services each person responded to
services_per_person <- CABtalShort %>%
  group_by(id) %>%
  summarise(num_services = n_distinct(svc))

# Divide the number of services by 10
services_per_person$num_services <- services_per_person$num_services / 10

# Merge the services_per_person data with the original dataset
merged_data <- left_join(CABtalShort, services_per_person, by = "id")

# Plot the number of occupations by the number of services each person responded to
ggplot(merged_data, aes(x = num_services, y = ocp, fill = as.factor(id))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow(n_distinct(merged_data$id))) +
  labs(x = "Number of Services per Factor", y = "Occupation",
       title = "Cross-product of Services x Factor x Occupations to TAL Survey (Factor == person)")
#-------------------- above in different color per bar----------



# Convert the id column to a factor
CABtalShort$id <- as.factor(CABtalShort$id)

# Calculate the number of occurrences for each ID and ocp combination
occurrences <- CABtalShort %>%
  group_by(id, ocp) %>%
  summarise(count = n())

# Create the plot
ggplot(occurrences, aes(x = ocp, y = id, size = count)) +
  geom_point() +
  scale_x_discrete(limits = unique(occurrences$ocp)) +
  scale_y_discrete(limits = unique(occurrences$id)) +
  labs(x = "Ocp", y = "ID", size = "Occurrences",
       title = "Number of Occurrences by ID and Ocp")


# Convert the id column to a factor
CABtalShort$id <- as.factor(CABtalShort$id)

# Calculate the number of occurrences for each ID and ocp combination
occurrences <- CABtalShort %>%
  group_by(id, ocp) %>%
  summarise(count = n())

# Create the plot
ggplot(occurrences, aes(x = ocp, y = id, size = count)) +
  geom_point() +
  scale_x_discrete(limits = unique(occurrences$ocp)) +
  scale_y_discrete(limits = unique(occurrences$id)) +
  labs(x = "Ocp", y = "ID", size = "Occurrences",
       title = "Number of Occurrences by ID and Ocp") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Convert the id column to a factor
CABtalShort$id <- as.factor(CABtalShort$id)

# Calculate the number of occurrences for each ID and ocp combination
occurrences <- CABtalShort %>%
  group_by(id, ocp) %>%
  summarise(count = n())

# Create the plot
ggplot(occurrences, aes(x = ocp, y = id, size = count)) +
  geom_point() +
  scale_x_discrete(limits = unique(occurrences$ocp)) +
  scale_y_discrete(limits = unique(occurrences$id)) +
  labs(x = "Ocp", y = "ID", size = "Occurrences",
       title = "Number of Occurrences by ID and Ocp") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate the number of occurrences for each ID, Occupation, and Region combination
occurrences <- CABtalShort %>%
  group_by(id, ocp, rgn) %>%
  summarise(count = n())

# Create the plot
ggplot(occurrences, aes(x = rgn, y = ocp, size = count)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  labs(x = "Region", y = "Occupation", size = "Occurrences",
       title = "Number of Occurrences by Region, Occupation, and ID")


# Calculate the number of occurrences for each ID, Occupation, and Region combination
services <- CABtalShort %>%
  group_by(id, ocp, rgn) %>%
  summarise(count = n())

# Create the plot
ggplot(services, aes(x = rgn, y = ocp, size = count, fill = count)) +
  geom_point(shape = 21) +
  scale_size_continuous(range = c(1, 10)) +
  scale_fill_gradientn(colours = c("blue", "yellow", "red"),
                       values = c(0, 0.5, 1),
                       limits = c(1, 6)) +
  labs(x = "Region", y = "Occupation", size = "Services",
       title = "Number of Services by Occupation and Region")


# Calculate the number of occurrences for each ID, Occupation, and Region combination
occurrences <- CABtalShort %>%
  group_by(id, ocp, rgn) %>%
  summarise(count = n())

# Create the plot
ggplot(occurrences, aes(x = rgn, y = ocp, size = count, fill = count)) +
  geom_point(shape = 21) +
  scale_size_continuous(range = c(1, 10)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Region", y = "Occupation", size = "Occurrences",
       title = "Number of Occurrences by Region, Occupation, and Individuals Responding")
