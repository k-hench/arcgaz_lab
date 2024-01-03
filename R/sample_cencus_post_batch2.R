library(tidyverse)
library(patchwork)
# sequenced individuals
data_seq <- str_c(
  "data/lab/seq_center/",
  c("2022-12-21_BMK220615-AX608-Project_Report_SampleID_BMKID.csv",
    "2023-04-17-BMK221130-BE661_SampleID_BMKID.csv")
  ) |> 
  map_dfr( read_tsv )

# sample meata data
data_raw <- readxl::read_xlsx(path = here::here("data/lab/Unique_filtered_dataset_2022.xlsx"), sheet = 1)|>
  select(PupTissueID, PupBirthyear, status, PupSex, Beach )

# get sample metadata fro sequenced individuals
data_combined <- data_seq |> 
  mutate(PupTissueID = str_c("AG", `Sample ID`)) |> 
  left_join(data_raw)

# comparison of sequenced and potential samples
p1 <- data_combined |> 
  mutate(type = "seqd") |> 
  bind_rows(data_raw |> 
              mutate(type = "all")) |> 
  ggplot(mapping = aes(x = PupBirthyear)) +
  geom_vline(color = "red",
             xintercept = 1996) +
  geom_bar(aes(fill = type)) + 
  facet_grid(status + type ~ ., scales = "free")

# missing factor combinations
p2 <- data_raw |> 
  left_join(data_combined |> select( PupBirthyear, status, `BMK ID`)) |> 
  filter(is.na(`BMK ID`),
         PupSex == "F") |> 
  group_by(PupBirthyear, status, Beach) |> 
  count() |> 
  ggplot(mapping = aes(x = PupBirthyear, y = n)) +
  geom_vline(color = "red",
             xintercept = 1996) +
  geom_bar(stat = 'identity', aes(fill = Beach)) + 
  facet_grid(status ~ ., scales = "free")

p1 / p2 &
  coord_cartesian(xlim = c(1974, 2020))

data_combined |> 
  filter(!(is.na(status)),
         Beach == "SSB") |> 
  group_by(PupBirthyear, status) |> 
  sample_n(1) |> 
  ungroup() |> 
  group_by(status) |> # + 3/4/1 (extractions todo)
  count()
  # ggplot(mapping = aes(x = PupBirthyear)) +
  # geom_vline(color = "red",
  #            xintercept = 1996) +
  # geom_bar() + 
  # facet_grid(status ~ ., scales = "free")

# geographical branch
data_geo <- readxl::read_xlsx("data/lab/Other_populations_metadata.xlsx", sheet = 1)

data_geo |> 
  mutate(`Sample ID` = str_remove(Sample, " .*")) |> 
  left_join(data_seq |> mutate(`Sample ID` = str_remove(`Sample ID`, "^B"))) |> View()
  ggplot(aes(x = Population)) +
  geom_bar(aes(fill = Population %in% 
                 c("South_Shetlands", "Bouvetoya",
                   "Marion", "Kerguelen", "Macquarie"))) +
  geom_hline(yintercept = 20, color = "red") +
  scale_fill_brewer(palette = "Set2", guide ="none", direction = -1) +
  theme(axis.text.x = element_text(angle = 90)) +
 facet_grid(is.na(`BMK ID`) ~ ., scale = "free")
  

