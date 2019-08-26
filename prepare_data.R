library(tidyverse)

# Import 2018 data

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
    grad_average = (currstatus + priorstatus) / 2,
    indicator = "grad_avg",
    year = "2018"
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

# Import state data
state_elig <- read_csv('data/sch_eligibility_state_sg.csv') %>%
  select(cds = CDS,
         TitleI1718,
         CDEStatus = AssistanceStatus)


# Determine eligibility - One row per school per student group
all2018_wide_test <- all2018 %>%
  group_by(cds, studentgroup) %>%
  summarize(
    red_count = sum(color == 1, na.rm = TRUE),
    orange_count = sum(color ==2, na.rm = TRUE),
    ygb_count = sum(color %in% c(3,4,5), na.rm = TRUE),
    all_count = red_count + orange_count + ygb_count
    ) %>%
  ungroup() %>%
  mutate(
    all_red = if_else((red_count == all_count) & (all_count > 0), TRUE, FALSE),
    all_red_but_1 = if_else((red_count == (all_count - 1)) & (all_count > 0) & (red_count > 0), TRUE, FALSE),
    majority_red = if_else((as.numeric(red_count) > (all_count / 2)) & all_count >= 5, TRUE, FALSE),
    red_and_orange = if_else(((red_count + orange_count) == all_count) & (red_count > 0) & (all_count > 0), TRUE, FALSE),  
    lowest5_dash = if_else(all_red | all_red_but_1 | majority_red | red_and_orange, TRUE, FALSE)
  ) 

# Combine with original file
all_2018_wide <- all2018 %>%
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
  left_join(all2018_wide_test) %>%
  left_join(gradrate %>%
              select(-indicator)) %>%
  mutate(
    csi_grad = (grad_average < 67),
  )

# School category
support_level <- all_2018_wide %>% 
  full_join(state_elig) %>%
  filter(studentgroup == "ALL") %>%
  mutate(identification = case_when(
    csi_grad == TRUE ~ "CSI-Grad Rate",
    lowest5_dash == TRUE & TitleI1718 == "Yes" ~ "CSI-Low Perform",
    TRUE ~ "General Assistance"
  ))

rcoe_csi <- support_level %>%
  filter(substr(cds,1,2) == "33")
