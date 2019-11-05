library(tidyverse)
library(readxl)

# Create entity list
directory <- read_excel('data/pubschls.xlsx', skip = 5) %>%
  select(cds = CDSCode, County, District, School, StatusType) %>%
  filter(StatusType == "Active")
lea_list <- directory %>%
  mutate(cds = substr(cds,1,7)) %>%
  select(cds, County, District) %>%
  unique()
county_list <- directory %>%
  mutate(cds = substr(cds,1,2)) %>%
  select(cds, County) %>%
  unique()
saveRDS(directory, file = 'CSI-ATSI/directory.RDS')
saveRDS(lea_list, file = 'CSI-ATSI/lea_list.RDS')
saveRDS(county_list, file = 'CSI-ATSI/county_list.RDS')

# Import 2017 data

eladownload2017 <- read_tsv('data/2017/ela38download2017f.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "ela",
    year = "2017"
  )

elpidownload2017 <- read_tsv('data/2017/elpidownload2017f.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         color) %>%
  mutate(
    studentgroup = "EL",
    indicator = "elpi",
    year = "2017"
  )


graddownload2017 <- read_tsv('data/2017/graddownload2017f.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "grad",
    year = "2017"
  )

mathdownload2017 <- read_tsv('data/2017/math38download2017f.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "math",
    year = "2017"
  )

suspdownload2017 <- read_tsv('data/2017/suspdownload2017f.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "susp",
    year = "2017"
  )

all2017 <- bind_rows(
  eladownload2017, 
  elpidownload2017,
  graddownload2017,
  mathdownload2017, 
  suspdownload2017
)

rm(list = c(
          "eladownload2017", 
          "elpidownload2017",
          "graddownload2017",
          "mathdownload2017", 
          "suspdownload2017"
          )
  )


# Import 2018 Dashboard Data

cci2018 <- read_tsv('data/2018/ccidownload2018.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "cci",
    year = "2018"
  )

chronicdownload2018 <- read_tsv('data/2018/chronicdownload2018.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "chronic",
    year = "2018"
  )

eladownload2018 <- read_tsv('data/2018/eladownload2018.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "ela",
    year = "2018"
  )

graddownload2018 <- read_tsv('data/2018/graddownload2018.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "grad",
    year = "2018"
  )

gradrate <- read_tsv('data/2018/graddownload2018.txt') %>%
  filter(
    rtype == "S",
    studentgroup == "ALL"
  ) %>%
  mutate(
    total_num = (currnumer + priornumer),
    total_den = (currdenom + priordenom),
    grad_average = total_num/total_den * 100,
    indicator = "grad_avg",
    year = "2018",
    usable_rate = currdenom >= 30 & priordenom >= 30
  ) %>% 
  mutate(grad_average = if_else(
    usable_rate == TRUE, 
    grad_average, 
    NULL)
         ) %>%
  select(
    cds,
    studentgroup,
    indicator,
    year,
    grad_average
  ) 

mathdownload2018 <- read_tsv('data/2018/mathdownload2018.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "math",
    year = "2018"
  )

suspdownload2018 <- read_tsv('data/2018/suspdownload2018.txt') %>%
  filter(
    rtype == "S"
  ) %>%
  select(cds,
         studentgroup,
         color) %>%
  mutate(
    indicator = "susp",
    year = "2018"
  )

all2018 <- bind_rows(
  cci2018, 
  chronicdownload2018, 
  eladownload2018, 
  graddownload2018,
  mathdownload2018, 
  suspdownload2018
)

rm(list = c(
  "cci2018", 
  "chronicdownload2018", 
  "eladownload2018", 
  "graddownload2018",
  "mathdownload2018", 
  "suspdownload2018"
)
)

# Combine years
all_years <- bind_rows(
  all2018,
  all2017
)

# Determine CSI eligibility - One row per school per student group
all_wide_test <- all_years %>%
  group_by(year, cds, studentgroup) %>%
  summarize(
    red_count = sum(color == 1, na.rm = TRUE),
    orange_count = sum(color ==2, na.rm = TRUE),
    ygb_count = sum(color %in% c(3,4,5), na.rm = TRUE),
    all_count = red_count + orange_count + ygb_count
    ) %>%
  ungroup() %>%
  mutate(
    all_red = if_else(studentgroup == "ALL",
              if_else(
                (red_count == all_count) & (all_count > 0), 
                TRUE, 
                FALSE
                ),
              if_else(
                (red_count == all_count) & (all_count > 1), 
                TRUE, 
                FALSE
                )
              ),
    all_red_but_1 = if_else((red_count == (all_count - 1)) & (all_count > 0) & (red_count > 0), TRUE, FALSE),
    majority_red = if_else((as.numeric(red_count) > (all_count / 2)) & all_count >= 5, TRUE, FALSE),
    red_and_orange = if_else(studentgroup == "ALL",
                             if_else(((red_count + orange_count) == all_count) & (red_count > 0) & (all_count > 0), TRUE, FALSE), 
                             if_else(((red_count + orange_count) == all_count) & (red_count > 0) & (all_count > 1), TRUE, FALSE)
    ),
    lowest5_dash = if_else(all_red | all_red_but_1 | majority_red | red_and_orange, TRUE, FALSE)
    ) %>%
  arrange(cds, studentgroup, year)

# Combine with original file
all_wide <- all_years %>%
  mutate(
    color = case_when(
      color == 0 ~ "None",
      color == 1 ~ "Red",
      color == 2 ~ "Orange",
      color == 3 ~ "Yellow",
      color == 4 ~ "Green",
      color == 5 ~ "Blue",
      TRUE ~ "None"
    )
  ) %>%
  spread(key = indicator, value = color, fill = "None") %>%
  left_join(all_wide_test) 

# Create school level data frame

school_file <- all_wide %>%
  filter(year == "2018",
         studentgroup == "ALL") %>%
  left_join(gradrate %>%
              select(-indicator)) %>%
  mutate(
    csi_grad = (grad_average < 67),
  )
  
# Calculate TSI/ATSI qualification
group_file <- all_wide %>%
  filter(studentgroup != "ALL") 

group_file_summary <- group_file %>%
  select(cds, studentgroup, year, lowest5_dash) %>%
  spread(key=year, value=lowest5_dash) %>%
  rename(met_csi_2017 = `2017`,
         met_csi_2018 = `2018`) %>%
  mutate(group_met_atsi = met_csi_2017 & met_csi_2018) %>%
  group_by(cds) %>%
  summarize(atsi_group_count = sum(group_met_atsi ,na.rm = TRUE)) %>%
  mutate(met_atsi = atsi_group_count > 0)
  
# Import state data
state_elig <- read_csv('data/sch_eligibility_state_sg.csv') %>%
  select(cds = CDS,
         TitleI1718,
         CDEStatus = AssistanceStatus)

# School category
support_level <- school_file %>% 
  full_join(state_elig) %>%
  full_join(group_file_summary) %>%
  mutate(identification = case_when(
    csi_grad == TRUE ~ "CSI Grad",
    lowest5_dash == TRUE & TitleI1718 == "Yes" ~ "CSI Low Perform",
    met_atsi == TRUE ~ "ATSI", 
    TRUE ~ "General Assistance"
  ))

# Merge in demographic data
demos <- read_tsv('data/2018/censusenrollratesdownload2018.txt') %>%
  filter(rtype == "S")

demos_groups <- demos %>%
  filter(
    studentgroup %in% c("SED", "SWD", "EL", "FOS")
  ) %>%
  select(cds, studentgroup, rate) %>%
  spread(key = studentgroup, value = rate)

demos_all <- demos %>%
  select(
    cds, schoolname, districtname, countyname, totalenrollment
  ) %>%
  unique() %>%
  left_join(demos_groups)

support_level_w_demos <-
  support_level %>%
  left_join(demos_all) %>%
  mutate(school_w_district = paste0(schoolname, ' (', districtname, ')'),
         mismatch = if_else(identification != CDEStatus, TRUE, FALSE)) %>%
  filter(mismatch == FALSE | !is.na(mismatch))

# Audit
support_audit <- support_level %>%
  filter(identification != CDEStatus)

# Check for RCOE Data
rcoe_csi <- support_level %>%
  filter(substr(cds,1,2) == "33")

# Create ATSI Info


# Save data for Shiny app

saveRDS(support_level_w_demos, file = 'CSI-ATSI/support_level.RDS')
saveRDS(group_file, file = 'CSI-ATSI/group_file.RDS')
