#A1 - Catarina Jesus, Student id: 26810706
#Visualizing & Analyzing Data with R: Methods & Tools - DAT-5323 - BMBAN1
#March 29th 2023

# Libs
library(readr)
library(ggplot2)
library(maps)
library(ggthemes)
library(leaflet)
library(mapproj)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(corrplot)

# Load the data sources
inHouse <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv')
consumer <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv')
donation <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv')
political <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv')

#See nulls 
colSums(is.na(inHouse))
colSums(is.na(consumer))
colSums(is.na(donation))
colSums(is.na(political))

#See blanks
colSums(inHouse == "")
colSums(consumer == "")
colSums(donation == "")
colSums(political == "")

#Examine data
head(inHouse)
head(consumer)
head(donation)
head(political)

#Check name of columns 
names(inHouse)
names(consumer)
names(donation)
names(political)

#DataSet inHouse

# Convert blank values to NA
inHouse$Age[inHouse$Age == ""] <- NA
inHouse$Gender[inHouse$Gender == ""] <- NA
inHouse$y_householdSpend[inHouse$y_householdSpend == ""] <- NA
inHouse$HomePurchasePrice[inHouse$HomePurchasePrice == ""] <- NA
inHouse$state[inHouse$state == ""] <- NA

# Remove non-numeric characters from HomePurchasePrice column
inHouse$HomePurchasePrice <- gsub("[^[:digit:].]", "", inHouse$HomePurchasePrice)

# Convert HomePurchasePrice column to numeric
inHouse$HomePurchasePrice <- as.numeric(inHouse$HomePurchasePrice)

# Replace null values with the mean value of the columns
inHouse$Age <- ifelse(is.na(inHouse$Age), mean(inHouse$Age, na.rm = TRUE), inHouse$Age)
inHouse$y_householdSpend <- ifelse(is.na(inHouse$y_householdSpend), mean(inHouse$y_householdSpend, na.rm = TRUE), inHouse$y_householdSpend)
inHouse$HomePurchasePrice <- ifelse(is.na(inHouse$HomePurchasePrice), mean(inHouse$HomePurchasePrice, na.rm = TRUE), inHouse$HomePurchasePrice)

# Set the probability of replacing null values with 'F' to 0.5
prob_F <- 0.5

# Replace null values with 'F' or 'M' based on probability
set.seed(123) # for reproducibility
inHouse$Gender[is.na(inHouse$Gender)] <- sample(c("F", "M"), sum(is.na(inHouse$Gender)), replace = TRUE, prob = c(prob_F, 1 - prob_F))

# Vector of possible states
possible_states <- c("California", "New York", "Texas", "Florida", "Illinois")

# Replace NA value with random state
inHouse$state <- ifelse(is.na(inHouse$state), sample(possible_states, 1), inHouse$state)

# Replace "NULL" with a random state
inHouse$state[inHouse$state == "NULL"] <- sample(possible_states, sum(inHouse$state == "NULL"), replace = TRUE)

#First EDA

#Eliminate columns
inHouse1 <- inHouse[, -c(2,3,6:20)]

#Create bins of age column
inHouse1$AgeBin <- cut(inHouse1$Age, breaks = c(20, 40, 60, 80, Inf), labels = c("21-40", "41-60", "61-80", ">80"))

# Count occurrences of each combination of AgeBin and Gender
df_df <- table(inHouse1$AgeBin, inHouse1$Gender)
df <- as.data.frame(df_df)

#Change name of the columns
colnames(df)[1] <- "Age"
colnames(df)[2] <- "Gender"
colnames(df)[3] <- "Population"

#Create barplot
ggplot(df, aes(x = Age, fill = Gender,
               y = ifelse(test = Gender == "M",
                          yes = -Population, no = Population))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(df$Population) * c(-1,1)) +
  labs(title = "Distribution of customers per age", x = "Age", y = "Number of customers") +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "top") +
  scale_fill_manual(values = c("lightblue", "lightpink"))


# Second EDA

#Eliminate columns
inHouse6 <- inHouse[, -c(2,3,4,6:10, 12:20)]

# Count occurrences of each combination of AgeBin and State
dfA <- table(inHouse1$AgeBin, inHouse6$state)
dfA1 <- as.data.frame(dfA)

#Change name of the columns
colnames(dfA1)[1] <- "Age"
colnames(dfA1)[2] <- "State"
colnames(dfA1)[3] <- "Population"

# Filter the data frame to include only the rows where age is ">80"
filtered_dfA <- filter(dfA1, Age == ">80")

#Create a barplot
ggplot(filtered_dfA, aes(x = reorder(State, Population), y = Population)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "State", y = "Population") +
  ggtitle("Number of customers per state >80 years old") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank())


#Third EDA

#Eliminate columns
inHouse7 <- inHouse[, -c(2,3,6:10, 12:20)]

# Count occurrences of each combination of AgeBin, State and Gender
dfG <- table(inHouse1$AgeBin, inHouse7$state, inHouse7$Gender)
dfG1 <- as.data.frame(dfG)

#Change name of the columns
colnames(dfG1)[1] <- "Age"
colnames(dfG1)[2] <- "State"
colnames(dfG1)[3] <- "Gender"
colnames(dfG1)[4] <- "Population"

# Filter the data frame to include only the rows where age is ">80"
filtered_dfG <- filter(dfG1, Age == ">80")

# Filter the data frame to include only the rows where gender is "F"
filtered_dfG1 <- filter(filtered_dfG, Gender == "F")

#Create barplot
ggplot(filtered_dfG1, aes(x = reorder(State, Population), y = Population)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "State", y = "Population") +
  ggtitle("Number of customers per state >80 years old and female") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank())


#Fourth EDA

#Eliminate columns
inHouse2 <- inHouse[, -c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]

#Histogram with the median of household expenses with BBY
options(scipen = 999)

ggplot(inHouse2, aes(x = y_householdSpend)) + 
  geom_histogram(fill = "lightblue") +
  ylab("Number of people") +
  xlab("Average household spend with BBY ($)") +
  ggtitle("Distribution of Average Household Spend with BBY") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#Firth EDA

#Eliminate columns
inHouse3 <- inHouse[, -c(2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]

#Create barplot
ggplot(inHouse3, aes(x = Gender, y = y_householdSpend, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Average household spend with BBY ($)", title = "Average household spend ($) per Gender") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) 
        
#Database Consumer

# Convert blank values to NA
consumer$NetWorth[consumer$NetWorth == ""] <- NA

# Vector of possible networth
possible_NetWorth <- c("$1-4999", "$5000-9999", "$10000-24999", "$25000-49999",
                       "$5000-99999","$100000-249999", "$250000-499999", "$499999+")

# Replace NA value with random networth
consumer$NetWorth <- ifelse(is.na(consumer$NetWorth), sample(possible_NetWorth, 1), consumer$NetWorth)

#Sixth EDA

#Eliminate columns
consumer1 <- consumer[, -c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]

# Compute frequency distribution of net worth
net_worth_freq <- table(consumer1$NetWorth)

# Create barplot
barplot(net_worth_freq, xlab = "", ylab = "Number of People", main = "Distribution of Net Worth", las = 2, cex.names = 0.4, col = "lightblue")
mtext("Net Worth Category", side = 1, line = 4)

#Seventh EDA

#Eliminate columns
inHouse5 <- inHouse[, -c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19)]

# Group by storeVisitFrequency and calculate mean of y_householdSpend
inHouse5 <- inHouse5 %>%
  group_by(storeVisitFrequency) %>%
  mutate(mean_bpm = mean(y_householdSpend))

# Calculate mean of y_householdSpend by storeVisitFrequency using aggregate function
mean_spend <- aggregate(y_householdSpend ~ storeVisitFrequency, inHouse5, mean)

# Create a barplot
ggplot(mean_spend, aes(x = storeVisitFrequency, y = y_householdSpend)) +
  geom_bar(stat = "identity", fill = "#ADD8E6") +
  geom_text(aes(label = round(y_householdSpend, 2)), 
            vjust = -0.5, 
            size = 3.5) +
  xlab("Store Visit Frequency") +
  ylab("Mean Household Spend") +
  ggtitle("Mean Household Spend by Store Visit Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Insight 1
#Which states have more visitors of the store and spend on the store

#Left join of inHouse and consumer databases 
df1 <- merge(inHouse,consumer, by = "tmpID", all.x = TRUE)

#Eliminate columns
df2 <- df1[, -c(1,2,3,4,5,6,9,10,12,13,14,15,16,18,19,21:45)]

# group by State and mutate store visit frequency and household spend
df1_summary <- df2 %>%
  group_by(state) %>%
  mutate(
    total_store_visits = sum(storeVisitFrequency),
    average_household_spend = mean(y_householdSpend)
  )

#Calculate mean of y_householdSpend by state using aggregate function
mean_spend1 <- aggregate(y_householdSpend ~ state, df1_summary, mean)

#Calculate sum of storeVisitFrequency by state using aggregate function
sum_spend1 <- aggregate(storeVisitFrequency ~ state, df1_summary, sum)

#Left join of mean_spend1 and sum_spend1
df3 <- merge(mean_spend1,sum_spend1, by = "state", all.x = TRUE)

#Filter the states
filtered_df3 <- df3 %>%
  filter(state %in% c("Texas", "California", "Pennsylvania", "New York", "Illinois", "New Mexico", "Ohio", "Michigan", "Missouri", "Colorado"))

#Create a barplot
ggplot(data = filtered_df3, aes(x = storeVisitFrequency, y = y_householdSpend, color = state)) +
  geom_point(alpha = 0.7, size = 5) +
  scale_color_manual(values = c("Texas" = "#E69F00", "California" = "#56B4E9", "Pennsylvania" = "#009E73", "New York" = "#F0E442", "Illinois" = "#0072B2", "New Mexico" = "#D55E00", "Ohio" = "#CC79A7", "Michigan" = "#999999", "Missouri" = "#000000", "Colorado" = "#6DCCDA")) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Store Visit Frequency vs. Household Spending",
       x = "Store Visit Frequency",
       y = "Household Spending",
       color = "State") +
  theme_minimal()

#Insight 2
#In that states which people are more likely to have a children

# Convert blank values to NA
consumer$PresenceOfChildrenCode[consumer$PresenceOfChildrenCode == ""] <- NA

# Vector of possible child
possible_child <- c("Likely to have a child", "Not Likely to have a child", "Modeled Likely to have a child", "Modeled Not as Likely to have a child")

# Replace NA value with random child
consumer$PresenceOfChildrenCode <- ifelse(is.na(consumer$PresenceOfChildrenCode), sample(possible_child, 1), consumer$PresenceOfChildrenCode)

# Replace "NULL" with a random state
consumer$PresenceOfChildrenCode[consumer$PresenceOfChildrenCode == "NULL"] <- sample(possible_child, sum(consumer$PresenceOfChildrenCode == "NULL"), replace = TRUE)

#Eliminate columns
df4 <- df1[, -c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20:23,25:45)]

# Define the bins for each option
bins <- c("Likely to have a child" = 1, "Not Likely to have a child" = 2, 
          "Likely to have a child" = 3, "Modeled Not as Likely to have a child" = 4)

# Create a new column with the bin values
df5 <- df4 %>%
  mutate(PresenceOfChildrenCodeBins = bins[PresenceOfChildrenCode])

# Filter the data frame to include only the rows where children code is "1"
filtered_df <- filter(df5, PresenceOfChildrenCodeBins == 1)

#Filter the states
filtered_df4 <- filtered_df %>%
  filter(state %in% c("Texas", "California", "Pennsylvania", "New York", "Illinois", "New Mexico", "Ohio", "Michigan", "Missouri", "Colorado"))

# Group by state and calculate sum of PresenceOfChildrenCodeBins
filtered_df5 <- filtered_df4 %>%
  group_by(state) %>%
  mutate(sum_child = sum(PresenceOfChildrenCodeBins))
sum_child<- aggregate(PresenceOfChildrenCodeBins ~ state, filtered_df5, sum)

# Create a bar plot
ggplot(sum_child, aes(x = state, y = PresenceOfChildrenCodeBins, fill = "lightblue")) +
  geom_bar(stat = "identity") +
  ggtitle("States with likelihood to have a childreen") +
  labs(x = "State", y = "Count", fill = "State") +
  scale_fill_identity() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Insight 3
#Customers who donate to charity also tend to have higher income levels and are more likely to make larger purchases at BBY.

#Convert blank values to NA
donation$DonatesToCharityInHome[donation$DonatesToCharityInHome == ""] <- NA

# Vector of possible child
possible_donations<- c("Yes", "Unknown")

# Replace NA value with random child
donation$DonatesToCharityInHome <- ifelse(is.na(donation$DonatesToCharityInHome), sample(possible_donations, 1), donation$DonatesToCharityInHome)

# Replace "NULL" with a random state
donation$DonatesToCharityInHome[donation$DonatesToCharityInHome == "NULL"] <- sample(possible_businessowner, sum(donation$DonatesToCharityInHome == "NULL"), replace = TRUE)

#Left join of donation and consumer databases
dfe <- merge(donation,consumer, by = "tmpID", all.x = TRUE)

#Eliminate columns
dfe1 <- dfe[, -c(1,2,3,5:23,25:45)]

# Define the bins for each option
bin <- c("Yes" = 1, "Unknown" = 2)

# Create a new column with the bin values
dfe2 <- dfe1 %>%
  mutate(DonatesEnvironmentCauseInHomeBins = bin[DonatesEnvironmentCauseInHome])

# Filter the data frame to include only the rows where donates is "1"
filtered_dfe <- filter(dfe2, DonatesEnvironmentCauseInHomeBins == 1)

# Group by state and calculate sum of DonatesEnvironmentCauseInHomeBins
filtered_dfe <- filtered_dfe %>%
  group_by(NetWorth) %>%
  mutate(sum_donate = sum(DonatesEnvironmentCauseInHomeBins))

#Calculate sum of DonatesEnvironmentCauseInHomeBins by NetWorth using aggregate function
sum_donate <- aggregate(DonatesEnvironmentCauseInHomeBins ~ NetWorth, filtered_dfe, sum)

# Create a bar plot 
ggplot(sum_donate, aes(x = NetWorth, y = DonatesEnvironmentCauseInHomeBins, fill = "lightblue")) +
  geom_bar(stat = "identity") +
  ggtitle("Donation Frequency by Net Worth") +
  labs(x = "NetWorth", y = "Count", fill = "NetWorth") +
  scale_fill_identity() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Insight 4
#Female Household gender by State

#Left join inHouse and consumer
dfp <- merge(inHouse, consumer, by = "tmpID", all.x = TRUE)

#Eliminate columns 
dfp1 <- dfp[, -c(1:10, 12:20,22:48)]

# Define the bins for each option
bin1 <- c("Mixed Gender Household" = 1, "Female Only Household" = 2, "Male Only Household" =3, "Cannot Determine" = 4)

# Create a new column with the bin values
dfp2 <- dfp1 %>%
  mutate(ResidenceHHGenderDescriptionBins = bin1[ResidenceHHGenderDescription])

# Filter the data frame to include only the rows where residenceHHgender is "2"
filtered_dfp <- filter(dfp2, ResidenceHHGenderDescriptionBins == 2)

#Filter by state
filtered_dfp1 <- filtered_dfp %>%
  filter(state %in% c("Texas", "California", "Pennsylvania", "New York", "Illinois", "New Mexico", "Ohio", "Michigan", "Missouri", "Colorado"))

# Group by state and calculate sum of ResidenceHHGenderDescriptionBins
filtered_dfp1 <- filtered_dfp1 %>%
  group_by(state) %>%
  mutate(sum_parties = sum(ResidenceHHGenderDescriptionBins))

#Calculate sum of ResidenceHHGenderDescriptionBins by state using aggregate function
sum_parties <- aggregate(ResidenceHHGenderDescriptionBins ~ state, filtered_dfp1, sum)

# Create a barplot
ggplot(sum_parties, aes(x = state, y = ResidenceHHGenderDescriptionBins, fill = "lightblue")) +
  geom_bar(stat = "identity") +
  ggtitle("Female Household Gender by State") +
  labs(x = "State", y = "Count", fill = "State") +
  scale_fill_identity() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

