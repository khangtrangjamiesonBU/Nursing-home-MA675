library(dplyr)
library(lme4)
library(broom.mixed)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(gtsummary)
library(gt)
library(kableExtra)

#Read datasets
df_micro <- read.csv("Microtransitions_Data_11.4.25.csv")
df_demo <- read.csv("Demographics_Data_11.4.25.csv")

#For dataset df_micro, filter only CSG facility
#CSG facility is "Cornerstone Gardens Healthcare & Rehabilitation" in the code book.
df_micro_CSG <- df_micro |> filter(grepl("^CSG", df_micro$linking_id))

#Because a patient can have multiple outings, their IDs repeatedly showed up.
#Count the frequency of outings per patient
df_micro_freq <- df_micro_CSG |> group_by(linking_id) |> summarise(Frequency = n())

#Add Frequency column back to the Mirco_CSG dataset
df_micro_CSG <- df_micro_CSG |> group_by(linking_id) |> mutate(Frequency = n()) |> ungroup() |> relocate(Frequency, .after = linking_id)

#Check if all IDs in Demo are the same as ones in Microtransition
all(df_demo$linking_id %in% df_micro_freq$linking_id)

#Check which IDs missing
missing_id <- setdiff(df_demo$linking_id, df_micro_freq$linking_id)
missing_id
length(missing_id)

#Remove missing ids from demo
df_demo_clean <- df_demo |> filter(!linking_id %in% missing_id)

#Check if all IDs in Demo_clean are the same as ones in Microtransition
all(df_demo_clean$linking_id %in% df_micro_freq$linking_id)

#Because we have duplicates in Demo_clean, we only keep one row per ID
df_demo_unique <- df_demo_clean |>
  distinct(linking_id, .keep_all = TRUE)   # keeps first row for each ID

#Merge dataset based on linking_ID
merged_df <- df_micro_CSG |>
  left_join(df_demo_unique, 
            by = "linking_id",
            relationship = "many-to-one")

#---Added Dec 4: Recode infection_diagnosed_yesno
merged_df <- merged_df %>%
  mutate(
    # make sure it's numeric first (if it's already numeric, this is harmless)
    infection_diagnosed_yesno = as.numeric(infection_diagnosed_yesno),
    infection_diagnosed_yesno = case_when(
      infection_diagnosed_yesno %in% c(1, 2, 3) ~ 1L,
      infection_diagnosed_yesno == 0           ~ 0L,
      TRUE                                     ~ NA_integer_
    )
  )

#Export dataset
write.csv(merged_df, "merged_dataset.csv")

#There's no 0 frequency, which means all patients have at least 1 microtransition
outing_cols <- merged_df[, c("medical_outing_type___1", "medical_outing_type___2", 
                             "medical_outing_type___3", "medical_outing_type___4", 
                             "medical_outing_type___5", "medical_outing_type___6", 
                             "medical_outing_type___7", "nonmedical_outing_type___1", 
                             "nonmedical_outing_type___2", "nonmedical_outing_type___3", 
                             "nonmedical_outing_type___4", "nonmedical_outing_type___5", 
                             "nonmedical_outing_type___6", "nonmedical_outing_type___7", 
                             "nonmedical_outing_type___8")]
counts <- colSums(outing_cols == 1, na.rm = TRUE)

#Turn the counts into a data frame
outing_counts <- data.frame(
  outing_type = names(counts),
  n_1 = as.numeric(counts)
)

library(dplyr)
library(ggplot2)

# Map variable names -> label text (from your screenshot)
outing_counts <- outing_counts %>%
  mutate(
    outing_label = dplyr::recode(
      outing_type,
      # medical outings
      "medical_outing_type___1" = "Clinic visit",
      "medical_outing_type___2" = "Labs, imaging, testing",
      "medical_outing_type___3" = "Chemo or radiation",
      "medical_outing_type___4" = "Dialysis",
      "medical_outing_type___5" = "Swallow study (done in parking lot)",
      "medical_outing_type___6" = "Medical procedure (not otherwise listed)",
      "medical_outing_type___7" = "Other (medical outing)",
      # non-medical outings
      "nonmedical_outing_type___1" = "Meal",
      "nonmedical_outing_type___2" = "Recreational activity (e.g., trip to park, CVS, etc.)",
      "nonmedical_outing_type___3" = "Ceremony/Special Occasion (wedding, funeral, graduation, etc.)",
      "nonmedical_outing_type___4" = "Day services (e.g., church, temple, etc.)",
      "nonmedical_outing_type___5" = "Financial management / legal services",
      "nonmedical_outing_type___6" = "Overnight stay",
      "nonmedical_outing_type___7" = "Other (non-medical outing)",
      "nonmedical_outing_type___8" = "Home visit (non-overnight)"
    )
  )


ggplot(outing_counts,
       aes(x = reorder(outing_label, n_1), y = n_1)) +
  geom_col() +
  labs(
    x = "Outing type",
    y = "Number of outings (value = 1)",
    title = "Distribution of Outing Types"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(face = "bold", size = 14)
  )

#One row per patient + list of dates
library(dplyr)
library(tidyverse)

dates_by_patient <- merged_df%>%
  group_by(linking_id) %>%
  summarise(
    frequency = n(),   # number of outings (rows) for this patient
    dates = paste(sort(unique(microtransition_date)),
                  collapse = ", "),
    .groups = "drop"
  )

time_window <- data.frame(dates_by_patient)

#Make sure dates are real Dates
merged_df <- merged_df %>%
  mutate(microtransition_date = as.Date(microtransition_date))

#Compute day gaps within each patient
df_with_gaps <- merged_df %>%
  group_by(linking_id) %>%
  arrange(microtransition_date, .by_group = TRUE) %>%
  mutate(
    diff_days = as.integer(microtransition_date - lag(microtransition_date))
  ) %>%
  ungroup()

#Summarise to the table + a new “gaps” column
summary_with_gaps <- df_with_gaps %>%
  group_by(linking_id) %>%
  summarise(
    frequency = n(),
    dates = paste(sort(unique(microtransition_date)), collapse = ", "),
    gaps  = paste(na.omit(diff_days), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(frequency > 1)   # keep only patients with >1 outing

gaps_long <- summary_with_gaps %>%
  # split "13, 6, 10, 2" into separate rows
  separate_rows(gaps, sep = ",\\s*") %>%
  # remove any empty strings just in case
  filter(gaps != "") %>%
  # convert to numeric
  mutate(gaps = as.numeric(gaps))

ggplot(gaps_long, aes(x = gaps)) +
  geom_histogram(
    binwidth = 1, 
    boundary = 0, 
    closed = "left",
    fill = "#4A90E2",   # soft blue
    color = "white"
  ) +
  labs(
    x = "Gap between outings (days)",
    y = "Number of gaps",
    title = "Distribution of gaps between outings"
  ) +
  theme_minimal(base_size = 14)

#Overlapping exposure
gaps_long %>%
  count(gaps) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = percent, y = reorder(factor(gaps), percent))) +
  geom_segment(aes(x = 0, xend = percent, yend = factor(gaps)), color = "grey60") +
  geom_point(size = 3, color = "#1F78B4") +
  labs(
    x = "Percent (%)",
    y = "Gap between outings (days)",
    title = "Percent of each gap length"
  ) +
  theme_minimal(base_size = 14)

#There are 47 patients have only 1 outing, so we have 68 patients have more than 1 outing
#Do this to check the exposure window

#-Total numbers of adverse events
#-yesno_cols <- grep("72_yesno$", names(merged_df), value = TRUE)
#-length(yesno_cols)

#-- Define all 28 AE variables
## ---- Define ALL AE variables (should total 28) ----

# 1) Adverse events that end with "72_yesno"
ae_72_yesno <- grep("72_yesno$", names(merged_df), value = TRUE)

# 2) Extra AE variables that do NOT have "72_yesno" in the name
#    (NOTE: do NOT include gi_issues_72hr here – it's just the umbrella)
extra_ae <- c(
  "death_72hr",
  "sepsis_yesno",
  "infection_diagnosed_yesno"
)

# 3) GI issue sub-items (1–8 only; exclude 9 = "No GI issues")
gi_ae <- grep("^gi_issues_72hr___[1-8]$", names(merged_df), value = TRUE)

# Combine into one AE list
ae_cols <- c(ae_72_yesno, extra_ae, gi_ae) |> unique()

length(ae_cols)   # should now be 28
ae_cols           # optional: inspect to confirm gi_issues_72hr is NOT here

#Compute number of “Yes (1)” adverse events per outing (per date)
#merged_df <- merged_df %>%
#  mutate(
#    total_yes_ae = rowSums(across(all_of(yesno_cols), ~ . == 1)),
#    total_ae_vars = length(yesno_cols),
#    ae_rate = total_yes_ae / total_ae_vars,
#    ae_summary = paste0(total_yes_ae, "/", total_ae_vars)
#  )

#-Added Dec 4

merged_df <- merged_df %>%
  mutate(
    total_yes_ae = rowSums(across(all_of(ae_cols), ~ . == 1), na.rm = TRUE),
    total_ae_vars = length(ae_cols),   # now 28
    ae_rate      = total_yes_ae / total_ae_vars,
    ae_summary   = paste0(total_yes_ae, "/", total_ae_vars)
  )

#Combine this with gaps table
#df_with_gaps <- merged_df %>%
#  mutate(microtransition_date = as.Date(microtransition_date)) %>%
#  group_by(linking_id) %>%
#  arrange(microtransition_date, .by_group = TRUE) %>%
#  mutate(diff_days = as.integer(microtransition_date - lag(microtransition_date))) %>%
#  ungroup() %>%
#  mutate(
#    total_yes_ae = rowSums(across(all_of(yesno_cols), ~ . == 1)),
#    total_ae_vars = length(yesno_cols),
#    ae_summary = paste0(total_yes_ae, "/", total_ae_vars)
#  )

#- Added Dec 4
df_with_gaps <- merged_df %>%
  mutate(microtransition_date = as.Date(microtransition_date)) %>%
  group_by(linking_id) %>%
  arrange(microtransition_date, .by_group = TRUE) %>%
  mutate(diff_days = as.integer(microtransition_date - lag(microtransition_date))) %>%
  ungroup() %>%
  mutate(
    total_yes_ae = rowSums(across(all_of(ae_cols), ~ . == 1), na.rm = TRUE),
    total_ae_vars = length(ae_cols),
    ae_summary = paste0(total_yes_ae, "/", total_ae_vars)
  )
#Add this to summary table
summary_with_gaps_ae <- df_with_gaps %>%
  group_by(linking_id) %>%
  summarise(
    frequency = n(),
    dates = paste(sort(unique(microtransition_date)), collapse = ", "),
    gaps = paste(na.omit(diff_days), collapse = ", "),
    ae = paste(ae_summary, collapse = ", "),
    .groups = "drop"
  )

#Graph distribution of adverse events

ae_long <- summary_with_gaps_ae %>%
       select(linking_id, ae) %>%
       mutate(ae = str_split(ae, ",\\s*")) %>%   # split "0/17, 1/17, 0/17"
       unnest(ae) %>%                           # make 1 row per outing
       filter(!is.na(ae) & ae != "")
ae_counts <- ae_long %>%
       count(ae, sort = TRUE) %>%
       mutate(percent = n / sum(n) * 100)
ggplot(ae_counts, aes(x = reorder(ae, percent), y = percent)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            hjust = -0.1, size = 4) +
  labs(
    title = "Distribution of AE per Outing (Percent Frequency)",
    x = "AE summary (total_yes / total_possible)",
    y = "Percent Frequency (%)"
  ) +
  theme_minimal() +
  ylim(0, max(ae_counts$percent) * 1.2)
