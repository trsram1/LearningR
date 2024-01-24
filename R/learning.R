# Loading packages --------------------------------------------------------

library(tidyverse)
library(NHANES)

# Briefly glimpse contents of dataset

glimpse(NHANES)
str(NHANES)

# Select specific columns -------------------------------------------------

select(NHANES, Age)
select(NHANES, Age, Weight, BMI)
select(NHANES, -Gender)

select(NHANES, starts_with("BP"))
select(NHANES, ends_with("Day"))
select(NHANES, contains("Age"))


# Renaming columns in snake_case ------------------------------------------

rename_with(NHANES, snakecase::to_snake_case)
NHANES_small <- rename_with(NHANES, snakecase::to_snake_case)



# Renaming specific column ------------------------------------------------


NHANES_small <- rename(NHANES_small, sex = gender)
NHANES_small
view(NHANES_small)


# chaining the function with pipe -----------------------------------------

colnames(NHANES_small)
NHANES_small %>%
  colnames()

NHANES_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)


# Tasks -------------------------------------------------------------------

NHANES_small %>%
  select(bp_sys_ave, education)

NHANES_small <-
  NHANES_small %>%
  rename(
    bp_sys = bp_sys_ave,
    bp_dia = bp_dia_ave
  )



NHANES_small %>%
  select(starts_with("bp_")) %>%
    rename(by_systolic=by_sys_ave)



# Filtering data by row ---------------------------------------------------

NHANES_small %>%
    filter(NHANES_small,phys_active == "No")

NHANES_small %>%
    filter(phys_active != "No")

NHANES_small %>%
    filter(bmi >= 25)

TRUE & TRUE
TRUE & FALSE
FALSE & FALSE
TRUE | TRUE
FALSE | FALSE

NHANES_small %>%
    filter(bmi == 25 & phys_active == "No") %>%
    select(bmi,phys_active)

NHANES_small %>%
    filter(bmi == 25 | phys_active == "No") %>%
    select(bmi,phys_active)

# arranging rows ----------------------------------------------------------

NHANES_small %>%
    arrange(age)

NHANES_small %>%
    arrange(education)

NHANES_small %>%
    arrange(bmi) %>%
    arrange(age)

NHANES_small %>%
    arrange(desc(age)) %>%
    select(age)


# Transform or add new column -----------------------------------------

NHANES_small %>%
    mutate(age = age * 12)

NHANES_small %>%
    mutate(age=age*12,
           logged_bmi=log(bmi)) %>%
        select(age, logged_bmi)

NHANES_small %>%
    mutate(
        old=if_else(age>=30, "Yes", "No")
    ) %>%
    select(old)


# Excercise before lunch --------------------------------------------------

# 1. BMI between 20 and 40 with diabetes
NHANES_small %>%
    # Format should follow: variable >= number or character
    filter(bmi >= 20 & bmi <= 40 & diabetes == "Yes")

# Pipe the data into mutate function and:
NHANES_modified <- NHANES_small %>% # Specifying dataset
    mutate(
        # 2. Calculate mean arterial pressure
        mean_arterial_pressure = ((2*bp_dia)+bp_sys-ave)/3,
        # 3. Create young_child variable using a condition
        young_child = if_else(age<6, "Yes", "No")
    )


# calculating summary statistics ------------------------------------------

NHANES_small %>%
    summarise(max_bmi=max(bmi,na.rm=TRUE),
              min_bmi=min(bmi,na.rm=TRUE))


# summary statistics by group ---------------------------------------------

library(NHANES)
NHANES_small<-
    NHANES_small %>%
    group_by(diabetes) %>%
    summarise(mean_age = mean(age,na.rm=TRUE),
                     mean_bmi = mean(bmi,na.rm=TRUE))

NHANES_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(mean_age = mean(age,na.rm=TRUE),
              mean_bmi = mean(bmi,na.rm=TRUE))
    ungroup()


# saving data files -------------------------------------------------------

    readr::write_csv(NHANES_small,here::here("data/NHANES_small.csv"))



# sriram loading ----------------------------------------------------------

    NHANES_big <- readr::read_csv(here::here("data/nhanes_small.csv"))
