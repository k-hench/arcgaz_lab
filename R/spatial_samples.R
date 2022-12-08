library(tidyverse)
library(here)
library(ggstance)

pops <- c("Bouvetoya", "Is", "Olav", "Hbr",
          "Husvik", "bay", "Wilson Harbour",
          "Heard", "Kergulen", "Macquarie",
          "South Georgia", "South Shetlands",
          "Marion island", "Tropicalis_Macquarie",
          "Crozet")

pop_samples <- readxl::read_xlsx(here("data","lab" ,"Agaz_DNA_extraction_info.xlsx")) |> 
  mutate(SampleID = SampleID |>
           str_replace(" ", "-") |>
           str_replace("\\(", "-") |>
           str_remove("\\)")) |> 
  separate(SampleID, into = c("sample_id_pop", "pop", "comment"),
           sep = "-", remove = FALSE) |> 
  mutate(pop = str_remove(pop, " *$") |> 
           str_remove("^ *") |> 
           str_replace("Bovetoya","Bouvetoya")) |> 
  filter(!is.na(pop)) |> 
  filter(pop %in% pops) |> 
  mutate(pop = as.factor(pop))

pop_count <- pop_samples |> 
  group_by(pop) |> 
  count() |> 
  ungroup() |> 
  mutate(n_class = cut(n, breaks = c(0,10,20,30,50,500)))

pop_count |> 
  ggplot(aes(y = pop, x = n)) +
  geom_barh(stat = "identity", aes(fill = n_class)) +
  geom_linerange(data = pop_count |> filter(pop %in% c(
    "Bouvetoya",
    "South Shetlands", "South Georgia",
    "Marion island", 
    "Macquarie", "Kergulen")),
    aes(ymin = as.numeric(pop)-.45,
        ymax = as.numeric(pop)+.45,
        x = 20),
    lty = 3) +
  rcartocolor::scale_fill_carto_d(direction = -1) +
  theme_minimal(base_family = "Josefin sans") +
  theme(axis.title = element_blank(), legend.position = c(1,1),legend.justification = c(1,1))

# ggsave("~/Desktop/spatial_samples.png", width = 5,height = 5)
