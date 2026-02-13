if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

set.seed(0)

ds_original <- read_csv("FakeNameGenerator.com_f469bbf6.csv")

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

ds <- ds %>% 
    mutate(
        cd_pac   = sprintf("%06d", cd_pac),
        tp_sex   = if_else(tp_sex  == 'female', 'F', 'M'),
        nm_ethn  = if_else(nm_ethn == 'Japanese (Anglicized)', 'Japanese', nm_ethn),
        dt_birth = mdy(dt_birth),
        nu_cc    = as.character(nu_cc)
    )

ds <- ds %>% 
    mutate(nm_full = paste(nm_first, nm_middle, nm_last), .after = tp_sex) %>% 
    select(-c(nm_first, nm_middle, nm_last))

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

slice <- c(
    '00 to 09 yearM' = 0.05, '10 to 19 yearM' = 0.20, '20 to 29 yearM' = 0.50,
    '30 to 39 yearM' = 0.70, '40 to 49 yearM' = 0.80, '50 to 59 yearM' = 0.90,
    '60 to 69 yearM' = 1.00, '70 to 79 yearM' = 0.95, '80 to 89 yearM' = 0.75,
    '90 or moreM' = 0.55,
    '00 to 09 yearF' = 0.05, '10 to 19 yearF' = 0.20, '20 to 29 yearF' = 0.45,
    '30 to 39 yearF' = 0.65, '40 to 49 yearF' = 0.75, '50 to 59 yearF' = 0.85,
    '60 to 69 yearF' = 0.95, '70 to 79 yearF' = 0.85, '80 to 89 yearF' = 0.65,
    '90 or moreF' = 0.45)

ds <- ds %>%
    mutate(group = paste0(age_group, tp_sex)) %>% 
    group_by(group) %>%
    group_modify(~slice_sample(.x, prop = slice[.y$group])) %>%
    ungroup() %>% select(-group)


ds <- ds %>%
    rowwise() %>%
    mutate(nm_race = case_when(
        nm_ethn == "Arabic"     ~ "Middle Eastern or North African",
        nm_ethn == "Brazil"     ~ sample(c("Black", "White", "Native American",
                                           "Asian"), size = 1,
                                         prob = c(0.50, 0.35, 0.10, 0.05)),
        nm_ethn == "Chinese"    ~ "Asian",
        nm_ethn == "Czech"      ~ "White",
        nm_ethn == "Eritrean"   ~ sample(c("Black", "Middle Eastern or North African"),
                                         size = 1, prob = c(0.5, 0.5)),
        nm_ethn == "Finnish"    ~ "White",
        nm_ethn == "French"     ~ sample(c("Black", "White"), size = 1,
                                         prob = c(0.15, 0.85)),
        nm_ethn == "German"     ~ sample(c("Black", "White",
                                           "Middle Eastern or North African",
                                           "Asian"), size = 1,
                                         prob = c(0.05, 0.75, 0.15, 0.05)),
        nm_ethn == "Hispanic"   ~ sample(c("Black", "White", "Native American",
                                           "Asian"), size = 1,
                                         prob = c(0.35, 0.40, 0.20, 0.05)),
        nm_ethn == "Igbo"       ~ "Black",
        nm_ethn == "Italian"    ~ sample(c("White", "Middle Eastern or North African"),
                                         size = 1, prob = c(0.85, 0.15)),
        nm_ethn == "Japanese"   ~ "Asian",
        nm_ethn == "Scottish"   ~ sample(c("Black", "White",
                                           "Middle Eastern or North African", "Asian"),
                                         size = 1, prob = c(0.05, 0.75, 0.10, 0.10)),
        nm_ethn == "Vietnamese" ~ "Asian",
        TRUE                    ~ "Unknown"
    ), .after = nm_ethn) %>%
    ungroup()

ds <- ds %>%
    mutate(stage = str_count(occupation, "\\w+"), .after = long) %>% 
    mutate(stage = case_when(
        stage == 1 ~ "Stage 3",
        stage == 2 ~ "Stage 1",
        stage == 3 ~ "Stage 2",
        stage %in% c(4,5)  ~ "Stage 4",
        stage > 5 ~ "Stage 5"
    ))

ds <- ds %>%
    mutate(dt_diag = str_extract(vehicle, "^\\w+") %>% as.integer()+7,
           .after = stage) %>% 
    mutate(dt_diag = case_when(
        nm_sign == "Aries"       ~ paste0(dt_diag, "-04"),
        nm_sign == "Taurus"      ~ paste0(dt_diag, "-05"),
        nm_sign == "Gemini"      ~ paste0(dt_diag, "-06"),
        nm_sign == "Cancer"      ~ paste0(dt_diag, "-07"),
        nm_sign == "Leo"         ~ paste0(dt_diag, "-08"),
        nm_sign == "Virgo"       ~ paste0(dt_diag, "-09"),
        nm_sign == "Libra"       ~ paste0(dt_diag, "-10"),
        nm_sign == "Scorpio"     ~ paste0(dt_diag, "-11"),
        nm_sign == "Sagittarius" ~ paste0(dt_diag, "-12"),
        nm_sign == "Capricorn"   ~ paste0(dt_diag, "-01"),
        nm_sign == "Aquarius"    ~ paste0(dt_diag, "-02"),
        nm_sign == "Pisces"      ~ paste0(dt_diag, "-03")
    ))


ds <- ds %>% 
    mutate(
        cd_cpf = case_when(
            cd_pac == '078516' ~ '566.229.886-68',
            cd_pac == '081699' ~ '610.280.257-85',
            cd_pac == '091076' ~ '917.288.867-91',
            T ~ cd_cpf
        )
    )

write_csv2(ds, 'dash_ds.csv')
saveRDS(ds, 'dash_ds.rds')
