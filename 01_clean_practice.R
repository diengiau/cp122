#Step 1: Clean data
# Run the packages
library(tidyverse) # for MacOS: library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(skimr)
library(readxl)

#1. Import the data file to R
Starbucks = read_excel("data/raw/starbucks-nutrition.xlsx")

#2. The column names are not clean
Starbucks = clean_names(Starbucks)
head(Starbucks[,1:3])

#3. Notice that some rows are totally missing. Try to delete all these rows in R (not Excel).
#4. Some rows are duplicated
Starbucks = Starbucks %>% distinct() #If they are totally the same, just keep a unique row
Starbucks = Starbucks[-17, ] #Delete the row that is totally missing
Starbucks = Starbucks[-74, ] #Keep the rows with more information

#5. Some size has typo: instead of "venti", we make typo to "vendi"
Starbucks = Starbucks %>% mutate(
  size = str_replace(size, "vendi", "venti")
)

#6. Any column has the wrong data type? If you can find them, let's convert them into the correct data format.
#No!

#7. The whip column should have only two values: 1 if we added whip cream and 0 otherwise.
#But we have some other values like 2 or 3. Please change all these typos to 1.
Starbucks = Starbucks %>% mutate(
  whip = str_replace(whip, "2", "1"),
  whip = str_replace(whip, "3", "1"),
  whip = as.numeric(whip)
)

# alternative way:
Starbucks %>% count(whip)
Starbucks %>% mutate(
  whip2 = ifelse(whip > 1, 1, whip),
) %>% 
  count(whip2) # same freq as whip above

#8. Some size is not quite popular, let's skip them. Only keep these size: short, tall, grande, venti
Starbucks = Starbucks %>% filter(size == "short"|size == "tall"|size == "grande"|size == "venti")

#9. Create a new column milk_type as follow:
Starbucks = Starbucks %>% mutate(
  milk_type = case_when(
    milk == "0" ~ "none",
    milk == "1" ~ "nonfat",
    milk == "2" ~ "2%",
    milk == "3" ~ "soy",
    milk == "4" ~ "coconut",
    milk == "5" ~ "whole",
  )
)

#10. Save your final data into the "data/process" folder as the name: starbucks_clean.rds
saveRDS(Starbucks, "data/process/starbucks_clean.rds")

#Step 2: Summarize the data a bit
#1. How many kinds of drink can we order in Starbucks?
Starbucks %>% count(product_name)
n_distinct(Starbucks$product_name)

#2. On average, a Starbucks drink has how many calories, how much sugar and caffeine
mean(Starbucks$calories)
mean(Starbucks$sugar_g)
mean(Starbucks$caffeine_mg)

#3. Which drink has the largest and lowest calories, sugar, total fat, and caffeine?
#calories
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(calories == max(calories))
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(calories == min(calories)) %>% 
  distinct(product_name)

#sugar
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(sugar_g == max(sugar_g))
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(sugar_g == min(sugar_g)) %>% 
  distinct(product_name)

#total fat
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(total_fat_g == max(total_fat_g))
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(total_fat_g == min(total_fat_g)) %>% 
  distinct(product_name)

#caffeine
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(caffeine_mg == max(caffeine_mg))
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(caffeine_mg == min(caffeine_mg)) %>% 
  distinct(product_name)

#4. Among all coffee drinks, which one has the largest sugar and caffeine? And the lowest?
#sugar
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(caffeine_mg > 0) %>% 
  filter(sugar_g == max(sugar_g) | sugar_g == min(sugar_g)) %>% 
  distinct(product_name, sugar_g)

#caffeine_mg
Starbucks %>% select(product_name, calories, sugar_g, total_fat_g, caffeine_mg) %>% 
  filter(caffeine_mg > 0) %>% 
  filter(caffeine_mg == max(caffeine_mg) | caffeine_mg == min(caffeine_mg)) %>% 
  distinct(product_name, caffeine_mg)


#5. After your analysis, which drink you want to recommend to your friends? And explain your choice.
#No code
# me: I'll choose a brewed coffee (low fat, low calories, and with coffee)
