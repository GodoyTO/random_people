if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

ds_original <- read_csv("FakeNameGenerator.com_f469bbf6.csv")

ds <- ds_original %>% 
    rename_with(~str_to_lower(str_replace_all(., "(?<=.)([A-Z])", "_\\1"))) %>% 
    select(cd_pac = number, tp_sex = gender, nm_ethn = name_set, nm_first = given_name,
           nm_middle = mothers_maiden, nm_last = surname,
           cd_cpf = national_i_d, dt_birth = birthday, nu_age = age, nu_weight = kilograms,
           nu_height = centimeters,
           nm_city = city, cd_state = state, lat = latitude, long = longitude,
           nm_sign = tropical_zodiac, nm_color = color, tp_blood = blood_type,
           vehicle, occupation,
           nm_cc = c_c_type, nu_cc = c_c_number, cd_cc = c_v_v2, dt_cc_exp = c_c_expires,
           email_address, username, password, guid = g_u_i_d
           )

ds <- ds %>% 
    mutate(cd_pac   = sprintf("%06d", cd_pac),
           tp_sex   = if_else(tp_sex  == 'female', 'F', 'M'),
           nm_ethn  = if_else(nm_ethn == 'Japanese (Anglicized)', 'Japanese', nm_ethn),
           dt_birth = mdy(dt_birth))

ds <- ds %>% 
    mutate(
        cd_cpf = case_when(
            cd_pac == '078516' ~ '566.229.886-68',
            cd_pac == '081699' ~ '610.280.257-85',
            cd_pac == '091076' ~ '917.288.867-91',
            T ~ cd_cpf
        )
    )