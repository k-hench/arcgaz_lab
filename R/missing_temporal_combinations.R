## 2023-03-15 missing temporal factor combinations
# based on 01_sample_selection.qmd

batch_1 <- c("F99009","F96013","F02007","F99031",
"P02046","F02004","F09012","F12002","P10325","F00014",
"F10012","F97012","F10017","F96003","F04009",
"F96002","P05320","F02022","P09063","F99046","F05008",
"F99058","P11171","F00036","F04014","P10152","P02080",
"P07481","P01018","P05191") %>%
  str_c("AG",.)

batch_2 <- c("P19226","P19239","P18182",
              "P17431","P17190","P17398",
              "P16121","P16235","P16087","P15345",
              "P15388","P14092","P14236","P13150",
              "P13075","P13215","P12470",
              "P12295","P12145","P11196","P11376",
              "P10102","P09153","P09185","P08335",
              "P08235","P07093","P07432",
              "P06161","P05188","P04247","P03040",
              "P03051","P03016","P02085","P01083",
              "P00129","P99032","P99038","F04026",
              "P98013","P97115","P97016",
              "P96030","P96140","F04030","F97022",
              "F97024","F99004","F97017","F99005",
              "P19032","P19039","P19059",
              "P19130","P18026","P18167","PC18001",
              "P19054","P19176","PC19004",
              "PC19005","PC18007","PC19008","PC19010",
              "PC19012","PC19014","PC19015",
              "PC19019","PC19022","PC19024","A1039",
              "A1095","B2270","B1827") %>%
  str_c("AG", .)

sent_ids <- tibble(tissue_id = c(batch_1, batch_2),
                   batch = rep(1:2, c(length(batch_1), length(batch_2))),
                   sent = TRUE)

combinations_sent_so_far <- read_tsv("data/lab/samples_temporal_initial_pick.tsv") |> 
  dplyr::select(year = birth_year, everything()) |> 
  bind_rows(temporal_preselection) |>
  filter(!duplicated(tissue_id)) |> 
  dplyr::select(year, unique_id, tissue_id, status:sex, plate_number, plate_location) |>
  left_join(sent_ids) |> 
  mutate(sent = replace_na(sent, FALSE))  

combinations_sent_so_far |> 
  filter(status %in% c("Recruited", "NonRecruited", "BeachDead")) |> 
  ggplot(aes(x = year, y = status, color = status, shape = sent, size = sent)) +
  geom_vline(xintercept = 1980:2020, color = rgb(0,0,0,.2), linewidth = .4) +
  geom_point() +
  scale_size_manual(values = c(`TRUE` = 1.5, `FALSE` = 3))+
  scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 1))

combinations_sent_so_far |> 
  group_by(year, status) |> 
  mutate(any_sent = any(sent)) |> 
  arrange(year) |> 
  ungroup() |> 
  filter(!any_sent,
         !is.na(year),
         status %in% c("Recruited", "NonRecruited", "BeachDead")) |> 
  arrange(year) |>
  write_tsv("data/lab/2023-03-12_combinations_still_missing.tsv")

# control
combinations_sent_so_far |> 
  group_by(year, status) |> 
  mutate(any_sent = any(sent)) |> 
  arrange(year) |> 
  ungroup() |> 
  filter(any_sent,
         !is.na(year),
         status %in% c("Recruited", "NonRecruited", "BeachDead")) |> 
  arrange(year)

length(sent_ids)
temporal_preselection |> 
  filter(!(tissue_id %in% sent_ids))

##  
dropouts <- read_tsv("data/lab/sample_dropouts.tsv")
data |> 
  filter(status == "NonRecruited",
         PupBirthyear %in% c(1998, 2000, 2004, 2006, 2018),
         PupSex == "F") |> 
  left_join(data_con) |> 
  arrange(PupBirthyear, PupTissueID) |> 
  filter(!(PupTissueID %in% dropouts$PupTissueID)) |> 
  group_by(PupBirthyear) |> 
  count()

data |> 
    filter(status == "Recruited",
           PupBirthyear ==  2014,
           PupSex == "F") |> 
    left_join(data_con) |> 
    arrange(PupBirthyear, PupTissueID) |> 
    filter(!(PupTissueID %in% dropouts$PupTissueID)) |> 
    group_by(PupBirthyear) |> count()
  