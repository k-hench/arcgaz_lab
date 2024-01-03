library(tidyverse)
library(here)
library(xlsx)
source(here("R", "theme_kh.R"))
cellColor <- function(style) 
{
  fg  <- style$getFillForegroundXSSFColor()
  rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
  rgb <- paste(rgb, collapse = "")
  return(rgb)
}

row_colors <- \(xls_sheet,
                sheet_idx = 1,
                col_idx = 1){
  wb     <- loadWorkbook(xls_sheet)
  sheet1 <- getSheets(wb)[[sheet_idx]]
  
  # get all rows
  rows  <- getRows(sheet1)
  cells <- getCells(rows, colIndex = col_idx)
  styles <- sapply(cells, getCellStyle)
  purrr::map_chr(styles[2:length(styles)], cellColor)
}

clr_row <- \(row_color){
  if_else(row_color == "", "#ffffff", str_c("#", row_color))|>
    str_to_upper()
}

xls_david_con <- here("data", "lab", "All_unique_notSNPped.xlsx")
clr_classes <- c("#FF0000" = "DNA not usable, needs reextraction.",
                 "#FFC000" = "DNA of borderline quality - avoid if possible.",
                 "#00B050" = "DNA good, maybe re-run gel before further use. Already (planed) genotyped on SNP array.",
                 "#92D050" = "DNA good, maybe re-run gel before further use. Spare sample (high n).",
                 "#FFFFFF" = "Not checked")

data_david_con <- readxl::read_xlsx(xls_david_con) |> 
  mutate(row_color = row_colors(xls_david_con)) |>
  dplyr::select(PupTissueID,
                david_con = Concentration,
                originally_considdered_borderline  =`...13`,
                row_color) |> 
  mutate(originally_considdered_borderline = !is.na(originally_considdered_borderline),
         row_color = clr_row(row_color) ,
         dna_quality_class = factor(clr_classes[row_color], levels = clr_classes))

data_david_substitutes <- readxl::read_xlsx(here("data", "lab", "Kosmas_samples.xlsx")) |> 
  mutate(substitue_class = c(red = "Need substitute",
                             green = "Substitute",
                             blue = "Measure concentration!",
                             white = "Not checked",
                             yellow = "What do we do with this?")[row_color]) |> 
  dplyr::select(PupTissueID = tissue_id, substitue_class)

data_con <- readxl::read_xlsx(here("data", "lab", "Agaz_DNA_extraction_info.xlsx"),
                              col_types = c("text", "text", "text", "numeric","numeric", "text", "text")) |>  
  rename(PupTissueID = SampleID)

data <- readxl::read_xlsx(here("data", "lab", "Unique_filtered_dataset_2022.xlsx")) |> 
  dplyr::select(PupTissueID, uniqueID_pup, PupBirthyear, status, PupSex, pup_SNPed, Beach) |> 
  filter(Beach == "SSB",
         PupSex == "F",
         !is.na(PupBirthyear)#,
         # !(PupTissueID %in% troubblemakers)
         )|> 
  mutate(fitness_class = if_else(status %in% c("BeachDead", "NonRecruited"),
                                 "NonRecruited", status)) |> 
  filter(!duplicated(uniqueID_pup),
         PupSex == "F",
         fitness_class %in% c("Recruited", "NonRecruited") ) |>  # 4,025 × 8
  left_join(data_con) |>                                         # 4,320 × 14 (tissue samples have duplicated concentration measurements)
  left_join(data_david_con) |>                                   # 4,320 × 18
  left_join(data_david_substitutes) |>                           # 4,320 × 19
  mutate(con_check = abs(as.double(Picogreen_con)-as.double(david_con)),
         con_inconsistent = if_else(is.na(con_check), FALSE, con_check > .1 ),
         dna_con = if_else(is.na(Picogreen_con),
                            as.double(david_con),                  # incl. new concentration measurements
                            as.double(Picogreen_con)),             # incl. original bad concentrations that were not transferred to davids file
         vol_wgs_nl = 1.2 * 450 / dna_con,                        # 400ng with > 2.5ng/ul of concentration.
         vol_SNP_nl = 750 / dna_con,
         vol_total = replace_na(vol_wgs_nl,0) +
           replace_na(vol_SNP_nl,0))

data_yes <- read_tsv("data/lab/samples_for_sequencing.tsv") |> 
  left_join(data, by = "PupTissueID") |>
  dplyr::select(unique_id = uniqueID,
                tissue_id = PupTissueID,
                lab_qc, sent,
                year = PupBirthyear,
                status,
                dna_con,
                v_wgs = vol_wgs_nl, 
                v_snp = vol_SNP_nl,
                v_total = vol_total,
                set) |> 
  mutate(sent = replace_na(sent, "not_yet")) |> 
  group_by(year, status) |> 
  mutate(factor_sent = any(sent == "2022-11-16"),
         n_factor = length(unique_id),
         v_class = cut(v_total,c(0,10,25,50,60))) |>  # duplications of factor combinations that are already sent for seq
  ungroup()

bouvetoya_samples <- c("A1039", "A1095", 2270)

data_yes |> 
  # group_by(year, status,factor_sent, sent) |>
  # filter(!factor_sent) |> 
  # summarise(n = length(unique_id)) |> 
  # ungroup() |> 
  filter(!is.na(status),
         !is.na(year),
         !factor_sent
         ) |> 
  # arrange()
  ggplot(aes(x= year)) +
  geom_bar(aes(fill = v_class,#factor(n_factor),
               color = sent == "2022-11-16",
               group = tissue_id)) +
  facet_grid(status~.) +
  # scale_fill_distiller(palette = "Greys") +
  scale_fill_brewer(palette = "Greens",direction = -1, na.value = "gray55") +
  scale_color_manual("",values = c(`TRUE` = "red", `FALSE` = "black")) +
  theme_minimal()

data_std <- tibble(row_idx = 12, col_idx = 1:8,
                   label = str_c("std<br>",
                                 c(1000, 500,250, 125, 62.5, 31.3, 15.6,0)),
                   dna_con = c(1000, 500,250, 125, 62.5, 31.25, 15.625,0))

data_plate <- data_yes |> 
  # filter(!is.na(status) | unique_id %in% bouvetoya_samples,
  #        !is.na(year) | unique_id %in% bouvetoya_samples,
  #        !factor_sent | unique_id %in% bouvetoya_samples
  # )  |> 
  filter(is.na(dna_con)) |>
  bind_rows(read_tsv("data/lab/gels/2022-12-08_samples_for_gels.tsv")|> 
              filter(is.na(v_wgs)) |> 
              filter(row_number() <= 13) |> 
              dplyr::select(unique_id = uniqueID,tissue_id, birth_year, set = status) |> 
              mutate(set = str_to_lower(set))) |>  
  mutate(batch_nr = row_number() %/% 88,
         batch_idx = row_number() %% 88,
         row_idx =  (batch_idx - 1) %% 11 + 1,
         col_idx = (batch_idx - 1) %/% 11 + 1,
         col_lab = LETTERS[col_idx],
         pos = str_c(col_lab, row_idx),
         label = tissue_id |> str_remove("AG") |> 
           str_replace("([MPF][C]*[0-9]{2})([0-9]{3})", "\\1<br>\\2")) 

data_plate |>
  ggplot(aes(x = row_idx, y = -col_idx)) +
  geom_polygon(inherit.aes = FALSE,
               data = tibble(x = c(0,.5,12.75,12.75,.5,0),
                             y = c(.5,0,0,8.75,8.75,8.25)),
               aes(x,-y),
               fill = NA, color = "black") +
  ggforce::geom_circle(inherit.aes = FALSE,
                       data = cross_df(list( x = 1:12, y = 1:8)) |> 
                         mutate(idx = row_number(),
                                grp = if_else(x == 12,
                                              "control",
                                              if_else(idx <= 46,
                                                      "seal", "empty"))),
                       aes(x0 = x, y0 = -y,color = grp, r = .45),
                       fill = "transparent") +
  scale_color_manual(NULL, values = scales::colour_ramp(c(clr1,"gray90","black"))(seq(1, 0, length.out = 3))) +
  ggnewscale::new_scale_color()+
  ggtext::geom_richtext(aes(label = label, color = set),
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"),
                        family = fnt_sans, vjust = .55 )  +
  ggtext::geom_richtext(data = data_std,
                        aes(label = label, color = "std"),
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"),
                        family = fnt_sans, vjust = .55 )  +
  geom_text(data = tibble(x = c(1:12,rep(.3,8)),
                          y = c(rep(.3,12),1:8),
                          lab = c(1:12, LETTERS[1:8] )),
            inherit.aes = FALSE,
            aes(x,-y,label = lab), family = fnt_serif) +
  scale_x_continuous(NULL, limits = c(-.3, 12.9), breaks = 1:12, position = "top") +
  scale_y_continuous(NULL, limits = c(-9.3, .3), breaks = -8:-1, labels = LETTERS[8:1]) +
  scale_color_manual(NULL, values = scales::colour_ramp(c(clr1,"gray80","black"))(seq(1, 0, length.out = 4)) |> 
                       set_names(nm = c("std", "RNA", "bouvetoya", "temporal"))) +
  coord_equal(expand = 0) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("~/Desktop/pico_plate.png", width = 8,height = 6, bg = "white")

data_plate |> 
  dplyr::select(unique_id:dna_con,
                v_total:n_factor,
                row_idx, col_lab,
                pos ) |> 
  mutate(sent = str_remove(sent, "_yet")#,
         # lab_qc = str_remove(lab_qc, "2022-")
         ) |> 
  write_tsv("data/lab/pico_green/2022-12-08_sample_layout_kh.tsv")


# after pico-green measurement (joining based on well-position)
data_conc <- data_plate |> 
  dplyr::select(unique_id:dna_con,
                v_total:n_factor,
                row_idx, col_lab,
                pos ) |> 
  mutate(sent = str_remove(sent, "_yet")) |> 
  left_join(read_tsv("data/lab/pico_green/2022-12-09_arcgaz_pico_green_hench_conc.tsv")) |> 
  mutate(vol_wgs_nl = 1.2 * 450 / dna_conc_ng_mul,                        # 400ng with > 2.5ng/ul of concentration.
         vol_SNP_nl = 750 / dna_conc_ng_mul,
         vol_total = replace_na(vol_wgs_nl,0) +
           replace_na(vol_SNP_nl,0))

data_conc |> 
  write_tsv("data/lab/pico_green/2022-12-08_sample_layout_vol.tsv")

         