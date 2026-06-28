# Анализ данных по Онежскому Поморью
# Автор кода - Д.А. Аристов
# 3.12.2025

# LIBS
library(tidyverse)
library(readxl)

# FUNS

# темы
theme_set(theme_bw())

# Подгружаем дата-сет
path <- "F:\\LMBE\\paper2025\\Онежское_Поморье\\"
setwd(path)

# дата-сет ----
data_OP <- read_xlsx(paste0(path, "base_OP_new.xlsx"), sheet = "NB")
data_OP$density <- as.numeric(data_OP$density)
data_OP$biomass <- as.numeric(data_OP$biomass)
# добавляем участки
data_OP <- data_OP %>% mutate(region = case_when(station <= 19 ~ "V", station >19 ~ "G"))
# пересчитываем все на кв. м.
sample_square <- read_xlsx(paste0(path, "base_OP_new.xlsx"), sheet = "samples_description") %>% select(station, sample, area) %>% group_by(station, sample) %>% summarize(max_area  = max(area))

data_OP <- left_join(data_OP, sample_square, by = c("station", "sample"))

data_OP <- data_OP %>% mutate(N_sq = density/max_area, B_sq = biomass/max_area)

# Суммарная численность и биомасса ----

NB_summary <- data_OP %>% group_by(region, station, sample) %>% summarize(total_N = sum(N_sq, na.rm = T), total_B = sum(B_sq, na.rm  = T))

N_total <- ggplot(NB_summary, aes(x = as.factor(station), y = total_N, fill = region)) + geom_boxplot() + xlab("Номер станции") + ylab("Суммарная плотность, экз./кв.м") + scale_fill_discrete(name = "Участок", label = c("Глубокий", "Вейга"))

# ggsave("N_total.png", N_total, width = 6, height = 4)

B_total <- ggplot(NB_summary, aes(x = as.factor(station), y = total_B, fill = region)) + geom_boxplot() + xlab("Номер станции") + ylab("Суммарная биомасса, г/кв.м") + scale_fill_discrete(name = "Участок", label = c("Глубокий", "Вейга"))

# ggsave("B_total.png", B_total, width = 6, height = 4)

# Таксономический состав ----
samples_number <- data_OP %>% group_by(region) %>% summarize(length(unique(station_name)))

# подключаем таксономию
taxonomy <- read_xlsx(paste0(path, "base_OP_new.xlsx"), sheet = "taxonomy")

data_OP <- left_join(data_OP, taxonomy, by = "taxon_name")

freq_table <- data_OP %>% group_by(parent2, parent1, ord, taxon_name, region) %>% summarise(occ = length(taxon_name)) %>% mutate(p = case_when(region == "G" ~ round(occ/18, 2), region == "V" ~ round(occ/15,2))) %>% mutate(se_p = case_when(region == "G" ~ round(sqrt(p*(1-p)/17),2), region == "V" ~ round(sqrt(p*(1-p)/14), 2))) %>% mutate(p_fin = paste(p, se_p, sep = "±")) %>% arrange(ord, parent1) %>% select(!c(occ, p, se_p)) %>% pivot_wider(names_from = "region", values_from = "p_fin", values_fill = "0±0")

#write_csv2(freq_table, "occ_table.csv")

# Представленность таксонов на разных участках
NB_mean_st <- data_OP %>% group_by(region, station, taxon_name, parent1, parent2) %>% summarize(N_mean = mean(N_sq, na.rm = T), B_mean = mean(B_sq, na.rm = T))

NB_mean_region <- ungroup(NB_mean_st) %>% group_by(region, taxon_name, parent1, parent2) %>% summarize(N_mean = mean(N_mean, na.rm = T), B_mean = mean(B_mean, na.rm = T))

abund_taxon_region <- NB_mean_region %>% ungroup(.) %>% group_by(region) %>%
  arrange(-N_mean) %>% 
  mutate(N_max = row_number()) %>%
  arrange(-B_mean) %>%
  mutate(B_max = row_number())

comm_N_region <- abund_taxon_region %>% group_by(region) %>% 
  mutate(taxa_name = case_when(N_max %in% 1:4 ~ taxon_name, .default = "Others")) %>% group_by(region, taxa_name) %>% summarize(N = sum(N_mean, na.rm = T))
comm_N_region$taxa_name <- factor(comm_N_region$taxa_name, levels = c("Hydrobia ulvae", "Hydrobia ventrosa", "Hydrobia sp.", "Littorina obtusata", "Mytilus edulis", "Tubifex costatus", "Others"))


comm_B_region <- abund_taxon_region %>% group_by(region) %>% 
  mutate(taxa_name = case_when(B_max %in% 1:4 ~ taxon_name, .default = "Others")) %>% group_by(region, taxa_name) %>% summarize(B = sum(B_mean, na.rm = T))

comm_B_region$taxa_name <- factor(comm_B_region$taxa_name, levels = c("Fucus vesiculosus", "Hydrobia ulvae", "Hydrobia ventrosa", "Macoma balthica", "Mytilus edulis", "Mya arenaria", "Others"))


region_names <- c(G = "Глубокий", V = "Вейга")

colors_taxa_N <- c('#ffff33', '#e41a1c','#377eb8','#4daf4a','#ff7f00', '#984ea3' ,'#a65628')
names(colors_taxa_N) <- c("Hydrobia ulvae", "Hydrobia ventrosa", "Hydrobia sp.", "Littorina obtusata", "Mytilus edulis", "Tubifex costatus", "Others")

comm_N_plot <- ggplot(comm_N_region, aes(x = "", y = N, fill = taxa_name)) + geom_bar(stat = "identity", color = "black") + facet_wrap("region", scale = "free_y", labeller = as_labeller(region_names)) + scale_fill_manual(name =  "Таксоны", values = colors_taxa_N, labels = c("Peringia ulvae", "Ecrobia ventrosa", "Hydrobiidae spp.", "Littorina obtusata", "Mytilus edulis", "Tubifex costatus", "Others")) + coord_polar(theta = "y", direction = -1) + theme_void()

# ggsave("comm_N.png", comm_N_plot, width = 7, height = 3)


colors_taxa_B <- c('#a6cee3','#ffff33','#e41a1c', '#b2df8a','#ff7f00','#33a02c', '#a65628')

names(colors_taxa_B) <- levels(comm_B_region$taxa_name)


comm_B_plot <- ggplot(comm_B_region, aes(x = "", y = B, fill = taxa_name)) + geom_bar(stat = "identity", color = "black") + facet_wrap("region", scale = "free_y", labeller = as_labeller(region_names)) + scale_fill_manual(name =  "Таксоны", values = colors_taxa_B, labels = c("Fucus vesiculosus","Peringia ulvae", "Ecrobia ventrosa", "Macoma balthica", "Mytilus edulis", "Mya arenaria", "Others")) + coord_polar(theta = "y", direction = -1) + theme_void()
# ggsave("comm_B.png", comm_B_plot, width = 7, height = 3)
