install.packages("tidyverse")
library(tidyverse)
caffeine_data <- read.csv('C:/Users/nikki/Desktop/praca IT/Portfolio/R/caffeine.csv')

#Kontrola danych
head(caffeine_data)
View(caffeine_data)
str(caffeine_data)
colnames(caffeine_data)
summary(caffeine_data)

#Sprawdzaenie brakujących wartości
colSums(is.na(caffeine_data))

#Sprawdzenie kategorii
count(caffeine_data, type)

# Liczba produktów w każdej kategorii
drink_types <- caffeine_data %>%
  count(type) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(type, "\n", round(percentage, 1), "%")
  )

# Wykres kołowy
ggplot(drink_types, aes(x = "", y = n, fill = type)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Distribution of Drink Categories",
    fill = "Drink Type"
  ) +
  theme_void()

#Podział na produkty zero kcal i produkty kaloryczne
calorie_distribution <- caffeine_data %>%
  mutate(
    calorie_group = if_else(
      Calories == 0,
      "Zero kcal",
      "Above 0 kcal"
    )
  ) %>%
  count(calorie_group) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

# Tabela
print(calorie_distribution)

# Wykres
ggplot(calorie_distribution,
       aes(x = "", y = n, fill = calorie_group)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Zero Kcal vs Caloric Drinks",
    fill = "Calorie Group"
  ) +
  theme_void()


# Średnia kofeina według kategorii
caffeine_by_type <- caffeine_data %>%
  group_by(type) %>%
  summarise(
    avg_caffeine = mean(`Caffeine..mg.`, na.rm = TRUE),
    max_caffeine = max(`Caffeine..mg.`, na.rm = TRUE),
    total_caffeine = sum(`Caffeine..mg.`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_caffeine))

print(caffeine_by_type)

# Wykres średniej zawartości kofeiny
ggplot(caffeine_by_type,
       aes(x = reorder(type, avg_caffeine),
           y = avg_caffeine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Caffeine Content by Drink Type",
    x = "Drink Type",
    y = "Average Caffeine (mg)"
  ) +
  theme_minimal()

# TOP 10 produktów z największą ilością kofeiny
top_caffeine_all <- caffeine_data %>%
  arrange(desc(`Caffeine..mg.`)) %>%
  slice_head(n = 10)

print(top_caffeine_all)


# Wyodrębnienie Energy Drinks
energy_drinks <- caffeine_data %>%
  filter(type == "Energy Drinks")

# Wyodrębnienie wybranych marek
energy_brands <- energy_drinks %>%
  mutate(
    brand = case_when(
      str_detect(drink, regex("Monster", ignore_case = TRUE)) ~ "Monster",
      str_detect(drink, regex("Red Bull", ignore_case = TRUE)) ~ "Red Bull",
      str_detect(drink, regex("Tiger", ignore_case = TRUE)) ~ "Tiger",
      str_detect(drink, regex("Rockstar", ignore_case = TRUE)) ~ "Rockstar",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(brand))

# Sprawdzenie
View(energy_brands)

count(energy_brands, brand)

#Zestawienie marek
brand_statistics <- energy_brands %>%
  group_by(brand) %>%
  summarise(
    products = n(),
    
    avg_volume = mean(`Volume..ml.`),
    min_volume = min(`Volume..ml.`),
    max_volume = max(`Volume..ml.`),
    
    avg_calories = mean(Calories),
    min_calories = min(Calories),
    max_calories = max(Calories),
    
    avg_caffeine = mean(`Caffeine..mg.`),
    min_caffeine = min(`Caffeine..mg.`),
    max_caffeine = max(`Caffeine..mg.`),
    
    .groups = "drop"
  )

print(brand_statistics)


# TOP 5 najmniej kalorycznych
top_5_low_calories <- energy_brands %>%
  arrange(Calories) %>%
  select(brand, drink, `Volume..ml.`, Calories,
         `Caffeine..mg.`) %>%
  slice_head(n = 5)

print(top_5_low_calories)


# TOP 5 najbardziej kalorycznych
top_5_high_calories <- energy_brands %>%
  arrange(desc(Calories)) %>%
  select(brand, drink, `Volume..ml.`, Calories,
         `Caffeine..mg.`) %>%
  slice_head(n = 5)

print(top_5_high_calories)


# TOP 5 największa ilość kofeiny
top_5_high_caffeine <- energy_brands %>%
  arrange(desc(`Caffeine..mg.`)) %>%
  select(brand, drink, `Volume..ml.`, Calories,
         `Caffeine..mg.`) %>%
  slice_head(n = 5)

print(top_5_high_caffeine)


# TOP 5 najmniejsza ilość kofeiny
top_5_low_caffeine <- energy_brands %>%
  arrange(`Caffeine..mg.`) %>%
  select(brand, drink, `Volume..ml.`, Calories,
         `Caffeine..mg.`) %>%
  slice_head(n = 5)

print(top_5_low_caffeine)


brand_count <- energy_brands %>%
  count(brand, name = "number_of_products") %>%
  arrange(desc(number_of_products))

print(brand_count)

# Wykres
ggplot(brand_count,
       aes(x = reorder(brand, number_of_products),
           y = number_of_products)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Number of Products per Brand",
    x = "Brand",
    y = "Number of Products"
  ) +
  theme_minimal()

# Sprawdzenie zależności pomiędzy kaloriami, a kofeiną

ggplot(energy_brands,
       aes(x = `Caffeine..mg.`,
           y = Calories,
           color = brand)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Calories vs Caffeine in Energy Drinks",
    x = "Caffeine (mg)",
    y = "Calories (kcal)",
    color = "Brand"
  ) +
  theme_minimal()

cor(
  energy_brands$Calories,
  energy_brands$`Caffeine..mg.`,
  method = "pearson",
  use = "complete.obs"
)

#Ile energetyków ma 0 kcal?

energy_zero_kcal <- energy_brands %>%
  mutate(
    calorie_group = if_else(
      Calories == 0,
      "Zero kcal",
      "Above 0 kcal"
    )
  ) %>%
  count(brand, calorie_group)

print(energy_zero_kcal)

# Wykres
ggplot(energy_zero_kcal,
       aes(x = brand, y = n, fill = calorie_group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Zero kcal Energy Drinks by Brand",
    x = "Brand",
    y = "Number of Products",
    fill = "Calories"
  ) +
  theme_minimal()

# Ile jest kofeiny w 100 ml w energy drinkach?

energy_brands <- energy_brands %>%
  mutate(
    caffeine_per_100ml =
      `Caffeine..mg.` / `Volume..ml.` * 100,
    
    calories_per_100ml =
      Calories / `Volume..ml.` * 100
  )

# Porównanie marek
energy_brands %>%
  group_by(brand) %>%
  summarise(
    avg_caffeine_100ml = mean(caffeine_per_100ml),
    avg_calories_100ml = mean(calories_per_100ml),
    .groups = "drop"
  )

