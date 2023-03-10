library(tidyverse)
library(ape)
library(ggtree)

data <- read_tsv("data/carnivora_ref_genomes.tsv") |>
  mutate(`Organism Name` = `Organism Name` |> 
           str_replace("Canis lupus familiaris", "Canis lupus") |> 
           str_replace("Taxidea taxus jeffersonii", "Taxidea taxus") |> 
           str_replace("Enhydra lutris kenyoni", "Enhydra lutris") |> 
           str_replace("Mustela putorius furo", "Mustela putorius") |> 
           str_replace("Odobenus rosmarus divergens", "Odobenus rosmarus") |> 
           str_replace("Ursus thibetanus thibetanus", "Ursus thibetanus"),
         repo = c(GCF = "refseq", GCA = "genbank")[str_sub(`Assembly Accession`,1,3)] |> 
           factor(levels = c("refseq", "genbank"))) |>
  arrange(`Organism Name`, 
          -as.numeric(`Assembly Submission Date`),
          as.numeric(repo))
#  filter(!duplicated(`Organism Name`, fromLast = TRUE))
tree <- read.tree("data/carnivora_list.nwk")

data_tree <- ggtree(tree, layout = "fan",
                    open.angle = 180)$data |> 
  groupClade(.node = 119) |> 
  mutate(spec_group = if_else(label == "Arctocephalus_gazella", "focal",
                              if_else(label %in% c("Callorhinus_ursinus", "Eumetopias_jubatus"),
                                      "outgroup", "other")),
         label = str_replace(label, "_", " "))

data_tree |> 
  ggtree(layout = "fan",aes(color = spec_group),
       open.angle = 15, size = .4) +
  geom_tiplab(aes(x = x+ 2.5,
                  label = str_replace(label, "_", " ")),
              size = 3, fontface = 'italic') +
  scale_x_continuous(limits = c(0, 120)) +
  scale_color_manual(values = c(focal = "#4f7ca6", outgroup = "black", other = "gray50"),
                     guide = 'none')

data_tree |> 
  arrange(label) |> 
  filter(grepl( "[a-z]", label)) |>
  dplyr::select(`Organism Name` = label, spec_group) %>%
  left_join(data |>
              dplyr::select(`Organism Name`,
                            `Assembly Accession`,
                            `Assembly Submission Date`,
                            repo), . ) |>
  filter(!is.na(spec_group)) |> 
  # filter(!duplicated(`Organism Name`)) # 67
  group_by(`Organism Name`) |> 
  mutate(genome_alternative = row_number()) |> 
  ungroup() |> 
  mutate(`Organism Name` = str_replace_all(`Organism Name`, " ", "_") |> 
           str_to_lower(),
         genome_version = str_c(`Organism Name`, "_", genome_alternative),
         spec = str_c(str_sub(`Organism Name`, 1, 3),
                        str_extract(`Organism Name`, "_[a-z]{3}") |> str_remove("_"))) |>
  filter(genome_alternative == 1, `Organism Name` != "Arctocephalus_gazella") |>
  set_names(nm = function(str){str_to_lower(str) |> str_replace_all(" ", "_")}) |>
  dplyr::select(spec, everything()) |> 
  write_tsv("data/carnivora_genome_and_timetree.tsv")
