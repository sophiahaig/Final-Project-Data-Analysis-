#Final Project

#First I loaded in my dataset. 
main_data<- read.csv("/Users/sophiahaig/Desktop/PLAN372/final project/Manatee_Carcass_Recovery_Locations_in_Florida.csv")

#I also uploaded any packages I needed. 
#library(dplyr)

#library(ggplot2)


#I checked for na values and found none so I didn't have to do anything further there.  
colSums(is.na(main_data))

#Then I made a new dataset called filtered data that included only the relevant variables such as repdate, repyear, repmonth, county, mortality, tlength, lat, and long:
filtered_data <- main_data %>%
  select(-created_user, -created_date, -last_edited_user, -last_edited_date, -X, -Y)

filtered_data <- subset(main_data, select = c(REPDATE, REPYEAR, REPMONTH, REPDAY, COUNTY, MORTALITY, TLENGTH, LAT, LONG_))


#I also made sure that the dataset I would be using included only the manatee deaths in years 2000 to 2020. 
filtered_data <- main_data %>%
  filter(REPYEAR >= 2000 & REPYEAR <= 2020)

#I double checked that I filtered it correctly. 
View(filtered_data)

###Calculating Monthly Frequency of Deaths:

#Then I grouped the data by REPMONTH using groupby and counted the number of mortality instances for each month using summarize. 
monthly_deaths <- filtered_data %>%
  group_by(REPMONTH) %>%
  summarize(total_deaths = n())

#Then I used the ggplot function to create a bargraph that would show the month and the total number of deaths. 
## I had to add the levels and labels to make sure the month names and not integers showed up from the REPMONTH column. 
ggplot(monthly_deaths, aes(x = factor(REPMONTH, levels = 1:12,
                                      labels = c("January", "February", "March", "April", "May", "June",
                                                 "July", "August", "September", "October", "November", "December")),
                           y = total_deaths)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Monthly Manatee Deaths", x = "Month", y = "Total Deaths") +
  theme_minimal()

### Calculating Deaths by Cause Per Month: 

#Here I did the same thing as above but used groupby to group the data by month and mortality and I summarized the grouped data to calculate the total count of mortality instances for each combination of month and cause.
deaths_by_cause <- filtered_data %>%
  group_by(REPMONTH, MORTALITY) %>%
  summarize(count = n()) %>%
#I calculated the proportion of each cause of death within its respective month by dividing the count for each cause by the total count of all causes in that month.  
  mutate(proportion = count / sum(count))

#I used ggplot to create a bar graph that showed the count of manatee deaths by month and cause of death. I did the same thing I did above and had to add labels to the levels to ensure the months showed up in an easy to interpret way. 
ggplot(deaths_by_cause, aes(x = factor(REPMONTH, levels = 1:12,
                                       labels = c("January", "February", "March", "April", "May", "June",
                                                  "July", "August", "September", "October", "November", "December")),
                            y = count, fill = MORTALITY)) +
  #I used geombar with stat being identity to make a stacked bar chart where the heights of the bars represent the total count of deaths for each month and are broken down by cause of death. 
  geom_bar(stat = "identity") +
  labs(title = "Manatee Deaths by Cause and Month", x = "Month", y = "Count") +
  theme_minimal() +
  
  #I used a color Brewer palette to make the graph easier to view and interpret. 
  scale_fill_brewer(palette = "Set3")

#Then I saved this so I could use it in my QGIS portion. 
write.csv(filtered_data, "/Users/sophiahaig/Desktop/PLAN372/final project/filtered_dataset.csv", row.names = FALSE)

##QGIS
# I created a new dataset called county_deaths to get the number of manatee deaths per county as it was not included in the original dataset.
county_deaths <- filtered_data %>%
  # First, I grouped the data by the COUNTY column to ensure that all rows that belong to the same county are grouped together. 
  group_by(COUNTY) %>%
  # Then I summarized the grouped data by calculating the total number of rows (deaths) in each county. I used the n() command to count the number of rows for each group, which corresponds to the number of deaths per county.
  summarize(total_deaths = n())
#Then I saved this so I could use it in my QGIS portion to create the county death toll map. 
write.csv(county_deaths, "/Users/sophiahaig/Desktop/PLAN372/final project/countydeaths/county_deaths.csv", row.names = FALSE)



