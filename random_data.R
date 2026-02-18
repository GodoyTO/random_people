if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

# Setting the set for reproducibility
set.seed(0)

# Loading original dataset
ds_original <- read_csv("FakeNameGenerator.com_f469bbf6.csv")

# Cleaning names and setting snake_cases
ds <- ds_original %>% 
    rename_with(~str_to_lower(str_replace_all(., "([a-z])([A-Z])", "\\1_\\2"))) %>% 
    select(cd_pac = number, tp_sex = gender, nm_ethn = name_set,
           nm_first = given_name, nm_middle = mothers_maiden, nm_last = surname,
           cd_cpf = national_id, dt_birth = birthday, nu_age = age,
           nu_weight = kilograms, nu_height = centimeters,
           nm_city = city, cd_state = state, lat = latitude, long = longitude,
           nm_sign = tropical_zodiac, nm_color = color, tp_blood = blood_type,
           vehicle, occupation, nm_cc = cctype, nu_cc = ccnumber,
           cd_cc = cvv2, dt_cc_exp = ccexpires,
           email_address, username, password, guid
           )

# Setting type sex, fixing names and dates
ds <- ds %>% 
    mutate(
        tp_sex   = if_else(tp_sex  == 'female', 'F', 'M'),
        nm_ethn  = if_else(nm_ethn == 'Japanese (Anglicized)', 'Japanese', nm_ethn),
        dt_birth = mdy(dt_birth) %m-% years(5),
        nu_cc    = as.character(nu_cc))

# Creating stage variable
ds <- ds %>%
    mutate(stage = str_count(occupation, "\\w+"), .after = long) %>% 
    mutate(stage = case_when(
        stage == 1 ~ "Stage 3",
        stage == 2 ~ "Stage 1",
        stage == 3 ~ "Stage 2",
        stage %in% c(4,5)  ~ "Stage 4",
        stage > 5 ~ "Stage 5"
    ))

# Creating relationship between nm_sign and the actual months
sign_month <- c(
    "Aries" = 4, "Taurus" = 5, "Gemini" = 6, "Cancer" = 7,
    "Leo" = 8, "Virgo" = 9, "Libra" = 10, "Scorpio" = 11,
    "Sagittarius" = 12, "Capricorn" = 1, "Aquarius" = 2, "Pisces" = 3)

# Creating date of diagnosis variable using nm_sign and vehicle
ds <- ds %>%
    mutate(
        year    = as.integer(str_extract(vehicle, "^\\w+")) + 7,
        month   = sign_month[nm_sign],
        dt_diag = make_date(year = year, month = month, day = 1),
        .after  = stage ) %>% select(-year, -month)

# Standardizing age variable and fixing dates of diagnosis before births
ds <- ds %>%
    mutate(
        nu_age = (as.numeric(difftime(
            ymd("2026-02-13"), dt_birth, units = "days")) / 365.25) %>% floor(),
        nu_age_diag = as.numeric(difftime(
            dt_diag, dt_birth, units = "days")) / 365.25,
        .after = nu_age) 

ds <- ds %>%
    mutate(fix_year = if_else(
        nu_age_diag < 0 & nu_age_diag >= -30, 
        ceiling(abs(nu_age_diag) / 5) * 5, 
        0))

ds <- ds %>%
    mutate(dt_diag = dt_diag %m+% years(fix_year),
           nu_age_diag = (as.numeric(difftime(
               dt_diag, dt_birth, units = "days")) / 365.25) %>% floor())

# Creating full name variable
ds <- ds %>% 
    mutate(nm_full = paste(nm_first, nm_middle, nm_last), .after = tp_sex) %>% 
    select(-c(nm_first, nm_middle, nm_last))

# Creating age groups
ds <- ds %>% 
    mutate(age_group = factor(case_when(
        between(nu_age,  0,  9)  ~ '00 to 09 year',
        between(nu_age, 10, 19)  ~ '10 to 19 year',
        between(nu_age, 20, 29)  ~ '20 to 29 year',
        between(nu_age, 30, 39)  ~ '30 to 39 year',
        between(nu_age, 40, 49)  ~ '40 to 49 year',
        between(nu_age, 50, 59)  ~ '50 to 59 year',
        between(nu_age, 60, 69)  ~ '60 to 69 year',
        between(nu_age, 70, 79)  ~ '70 to 79 year',
        between(nu_age, 80, 89)  ~ '80 to 89 year',
        between(nu_age, 90, 999) ~ '90 or more'
    ), levels = c(
        '00 to 09 year', '10 to 19 year', '20 to 29 year', '30 to 39 year',
        '40 to 49 year', '50 to 59 year', '60 to 69 year', '70 to 79 year',
        '80 to 89 year', '90 or more')
    ), .after = nu_age)

# slice <- c(
#     '00 to 09 year' = 0.05, '10 to 19 year' = 0.20, '20 to 29 year' = 0.50,
#     '30 to 39 year' = 0.70, '40 to 49 year' = 0.80, '50 to 59 year' = 0.90,
#     '60 to 69 year' = 1.00, '70 to 79 year' = 0.95, '80 to 89 year' = 0.75,
#     '90 or more' = 0.55)

# Slice array to subset the dataset for a more natural population pyramid
slice <- c(
    '00 to 09 yearM' = 0.05, '10 to 19 yearM' = 0.20, '20 to 29 yearM' = 0.50,
    '30 to 39 yearM' = 0.70, '40 to 49 yearM' = 0.80, '50 to 59 yearM' = 0.90,
    '60 to 69 yearM' = 1.00, '70 to 79 yearM' = 0.95, '80 to 89 yearM' = 0.75,
    '90 or moreM' = 0.55,
    '00 to 09 yearF' = 0.05, '10 to 19 yearF' = 0.20, '20 to 29 yearF' = 0.45,
    '30 to 39 yearF' = 0.65, '40 to 49 yearF' = 0.75, '50 to 59 yearF' = 0.85,
    '60 to 69 yearF' = 0.95, '70 to 79 yearF' = 0.85, '80 to 89 yearF' = 0.65,
    '90 or moreF' = 0.45)

# Slicing the dataset for age_group
ds <- ds %>%
    mutate(group = paste0(age_group, tp_sex)) %>% 
    group_by(group) %>%
    group_modify(~slice_sample(.x, prop = slice[.y$group])) %>%
    ungroup() %>% select(-group)

# Creating race variable
ds <- ds %>%
    mutate(nm_race = case_when(
        nm_ethn == "Arabic"     ~ "Middle Eastern or North African",
        nm_ethn == "Chinese"    ~ "Asian",
        nm_ethn == "Czech"      ~ "White",
        nm_ethn == "Finnish"    ~ "White",
        nm_ethn == "Igbo"       ~ "Black",
        nm_ethn == "Japanese"   ~ "Asian",
        nm_ethn == "Vietnamese" ~ "Asian",
        TRUE                    ~ NA_character_
    ), .after = nm_ethn)

# Sample function to choose the race
ds <- ds %>%
    group_by(nm_ethn) %>%
    mutate(nm_race = case_when(
        !is.na(nm_race) ~ nm_race,
        
        nm_ethn == "Brazil"   ~ sample(
            x = c("Black", "White", "Native American", "Asian"), size = n(),
            replace = T, prob = c(0.50, 0.35, 0.10, 0.05)),
        
        nm_ethn == "Eritrean" ~ sample(
            x = c("Black", "Middle Eastern or North African"), size =  n(),
            replace = T, prob = c(0.5, 0.5)),
        
        nm_ethn == "French"   ~ sample(
            x = c("Black", "White"), size = n(), 
            replace = T, prob = c(0.15, 0.85)),
        
        nm_ethn == "German"   ~ sample(
            x = c("Black", "White", "Middle Eastern or North African", "Asian"),
            size = n(), 
            replace = T, prob = c(0.05, 0.75, 0.15, 0.05)),
        
        nm_ethn == "Hispanic" ~ sample(
            x = c("Black", "White", "Native American", "Asian"), size = n(), 
            replace = T, prob = c(0.35, 0.40, 0.20, 0.05)),
        
        nm_ethn == "Italian"  ~ sample(
            x = c("White", "Middle Eastern or North African"), size = n(), 
            replace = T, prob = c(0.85, 0.15)),
        
        nm_ethn == "Scottish" ~ sample(
            x = c("Black", "White", "Middle Eastern or North African", "Asian"),
            size = n(),
            replace = T, prob = c(0.05, 0.75, 0.10, 0.10)),
        
        TRUE                    ~ "Unknown"
    )) %>%
    ungroup()

# Setting patient code based in date of diagnosis
ds <- ds %>%
    arrange(dt_diag) %>% 
    mutate(cd_pac = sprintf("%06d", row_number()))


# Checking CPF duplication
ds %>% select(cd_pac, cd_cpf) %>% 
    filter(duplicated(cd_cpf) | duplicated(cd_cpf, fromLast = T))
 
# Fixing CPF numbers to end variable duplication
ds <- ds %>% 
    mutate(
        cd_cpf = case_when(
            # cd_pac == '078516' ~ '566.229.886-68',
            # cd_pac == '081699' ~ '610.280.257-85',
            cd_pac == '091076' ~ '917.288.867-91',
            T ~ cd_cpf
        )
    )

write_csv2(ds, 'dash_ds.csv')
saveRDS(ds, 'dash_ds.rds')
