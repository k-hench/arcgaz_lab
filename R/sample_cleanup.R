library(tidyverse)
library(here)

read_gel <- \(fl){
  read_tsv(here("data", "lab", "gels", fl)) |> 
    rename_with(str_to_lower) |> 
     dplyr::select(tissue_id, unique_id, plate)
}

data_gels <- map_dfr(c("2022-11-17_samples_for_gels_org.tsv",
          "2022-11-24_samples_for_gels_p5_org.tsv",
          "2022-12-01_samples_for_gels_org.tsv",
          "2022-12-02_samples_for_gels_b_org.tsv",
          "2022-12-08_samples_for_gels_org.tsv"),
        read_gel ) |> 
  filter(! duplicated(tissue_id))
  
data_yes <- read_tsv("data/lab/samples_for_sequencing.tsv") |> 
  dplyr::select(tissue_id = PupTissueID, lab_qc) |> 
  mutate(accepted = "yes")

data_no <- read_tsv("data/lab/sample_dropouts.tsv") |> 
  dplyr::select(tissue_id = PupTissueID, lab_qc = dropout_point) |> 
  mutate(accepted = "no")

data_pico <- read_tsv("data/lab/pico_green/samples_needing_concentration_measurements.tsv") |> 
  dplyr::select(tissue_id, unique_id = uniqueID, plate = PlateNumber) |> 
  mutate(lab_qc = "pico",
         accepted = "pico") |> 
  filter(!(tissue_id %in% data_gels$tissue_id))

all_selected <- data_yes |> 
  bind_rows(data_no) |>
  left_join(data_gels) |>
  bind_rows(data_pico) |> 
  filter(lab_qc != "david_lab")

data_sent <- read_tsv("data/lab/2022-12-13_sample_box_pos_dna400_print.csv") |> 
  dplyr::select(tissue_id, unique_id) |> 
  mutate(lab_qc = "sent",
         accepted = "sent") |> 
  left_join(all_selected |> dplyr::select(tissue_id, plate))
  
all_sent <- all_selected |> 
  bind_rows(data_sent) |> 
  filter(!duplicated(tissue_id, fromLast = TRUE)) |> 
  arrange(plate,lab_qc)

all_sent |> write_tsv("data/lab/2022-12-16_sample_cleanup.tsv")
