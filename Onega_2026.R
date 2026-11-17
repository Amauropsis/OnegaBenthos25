## LIBS ####----
# install.packages("xlsx")
# install.packages("writexl")
# install.packages("vegan")
#install.packages("ggvegan")
# install.packages("ggrepel")
# install.packages("indicspecies")

library(tidyverse)
library(writexl)
library(readxl)
library(vegan)
library(ggvegan)
library(ggrepel)
library(indicspecies)

## data-set ####
# data_OP <- read.delim2("https://raw.githubusercontent.com/Amauropsis/OnegaBenthos25/refs/heads/main/OP_NB.csv", sep = ";") # исправил, потому что в read_delim нет уточнения про десятичный разделитель, а в read.delim2 он "," по умолчанию, но можно эксплицитно поставить "." или ","

data_OP <- read_xlsx(file.choose(), sheet = "Sheet1")
data_OP$density <- as.numeric(data_OP$density)
data_OP$biomass <- as.numeric(data_OP$biomass)

#пересчитывваем на метр
data_OP <- data_OP %>%  # упростил предыдущий код
  mutate(N_sqm = case_when(area == 30 ~ density * 30, area == 1 ~ density * 4)) %>%
  mutate(B_sqm = case_when(area == 30 ~ biomass * 30, area == 1 ~ biomass * 4))


#суммарная биомасса и численность в Вейге и Глубоком----
data_OP_sum <- data_OP %>% group_by(station_name) %>% summarise(biomass_sum = sum(B_sqm, na.rm = TRUE), density_sum = sum(N_sqm, na.rm = TRUE)) %>% separate_wider_delim(cols = station_name, delim = "_", names = c("station", "station1"))%>% mutate(station_num = c(rep("16", 3), rep("17", 3), rep("18", 2), rep("19", 3), rep("23", 3), rep("24", 3), rep("25", 3), rep("31", 3), rep("32", 3), rep("34", 3), rep("8", 3)))

data_OP_sum$station_num <- as.numeric(data_OP_sum$station_num)

data_OP_sum_or <- data_OP_sum[order(data_OP_sum$station_num), ]

data_OP_sum_or <- data_OP_sum_or %>% mutate(location = case_when(station_num < 20 ~ "veiga", station_num < 30 ~ "glub_near", station_num > 30 ~ "glub_far"))

data_OP_sum_or <- data_OP_sum_or %>%
  mutate(location = factor(location, levels = c("veiga","glub_near", "glub_far")))

# задаем цвета для точек на всех графиках
locs_colors <- c("veiga" = "#1E90FF",
                 "glub_near" = "#00A86B",
                 "glub_far" = "#E63946")

# графики по суммарной численности и биомассе в Вейге и Глубоком
# численность
boxplot_den <- ggplot(data_OP_sum_or, mapping = aes(x = as.factor(station_num), y = density_sum, fill = as.factor(location))) + geom_boxplot() + xlab("номер станции") + ylab("численность") + scale_fill_manual(name = "локация", labels = c("Вейга", "перед Глубоким", "за Глубоким"), values = locs_colors) + theme_minimal() + theme(legend.position = "top", legend.text = element_text(size = 18), legend.title = element_text(size = 20), axis.text = element_text(size = 20), axis.title = element_text(size = 20))
# биомасса
boxplot_mass <- ggplot(data_OP_sum_or, aes(x = as.factor(station_num), y = biomass_sum, fill = as.factor(location))) + geom_boxplot() + xlab("номер станции") + ylab("биомасса") + scale_fill_manual(name = "локация", labels = c("Вейга", "перед Глубоким", "за Глубоким"), values = locs_colors) + theme_minimal() + theme(legend.position = "top", legend.text = element_text(size = 19), legend.title = element_text(size = 20), axis.text = element_text(size = 20), axis.title = element_text(size = 20))

ggsave("boxplot_den.png", plot = boxplot_den, path = "C:\\Users\\user\\Documents\\лэмб\\Onega_benthos\\OnegaBenthos25")
ggsave("boxplot_mass.png", plot = boxplot_mass, path = "C:\\Users\\user\\Documents\\лэмб\\Onega_benthos\\OnegaBenthos25")

#Графики по относительному обилию гидробий----

data_OP_sum_H <-  data_OP %>% filter(taxon_name == "Hydrobia ulvae"|taxon_name == "Hydrobia ventrosa") %>% group_by(taxon_name, station_name) %>% summarise(biomass_sum = sum(B_sqm, na.rm = TRUE), density_sum = sum(N_sqm, na.rm = TRUE)) %>% ungroup() %>%  separate_wider_delim(cols = station_name, delim = "_", names = c("station", "station1")) %>% group_by(station, taxon_name) %>% summarise(biomass_mean = mean(biomass_sum, na.rm = TRUE), density_mean = mean(density_sum, na.rm = TRUE)) %>% mutate(station_num = gsub("[^0-9]", "", station)) %>% ungroup()
  
data_OP_sum_H$station_num <- as.numeric(data_OP_sum_H$station_num)

data_OP_sum_H_or <- data_OP_sum_H[order(data_OP_sum_H$station_num), ] %>% mutate(location = case_when(station_num < 20 ~ "veiga", station_num < 30 ~ "glub near", station_num > 30 ~ "glub far")) %>% mutate(location2 = case_when(station_num < 20 ~ "veiga", station_num > 20 ~ "glub")) %>% mutate(den_all = case_when(location2 == "veiga"  ~ density_mean/35925, location2 == "glub"  ~  density_mean/9898.66667))

data_OP_sum_H_or$location2 <- factor(data_OP_sum_H_or$location2, levels = c("veiga", "glub"))
data_OP_sum_H_or$taxon_name <- factor(data_OP_sum_H_or$taxon_name, levels = c("Hydrobia ventrosa", "Hydrobia ulvae"))

hydr_color <- c("Hydrobia ulvae" = "#008080", "Hydrobia ventrosa" = "#C71585")

ratio_2 <- ggplot(data_OP_sum_H_or, aes(x = as.factor(station_num), y = den_all, fill = taxon_name)) + xlab("Номер станции") + geom_col(position = "stack") + facet_wrap (~ location2, scales = "free_x", labeller = as_labeller(c("glub" = "мыс Глубокий", "veiga" = "эстуарий Вейги"))) + scale_fill_manual(name = "вид", labels = c("E. ventrosa", "Per. ulvae"), values = hydr_color)+ theme_minimal() + ylab("Соотношение P. ulvae и E. ventrosa") + theme(legend.position = "top", strip.text = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20), axis.text = element_text(size = 15), axis.title = element_text(size = 18))

ggsave("ratio_2.jpg", plot = ratio_2, path = "C:\\Users\\user\\Documents\\лэмб\\Onega_benthos\\OnegaBenthos25")

#средняя доля вентрозы на станциях с ошибкой
data_OP_hydr <- data_OP %>% group_by(station_name, taxon_name) %>% summarise(biomass_sum = sum(B_sqm, na.rm = TRUE), density_sum = sum(N_sqm, na.rm = TRUE)) %>% filter(taxon_name == "Hydrobia ulvae"|taxon_name == "Hydrobia ventrosa")

wide <- data_OP_hydr %>% select(taxon_name, density_sum, station_name) %>% pivot_wider(names_from = "taxon_name", values_from = "density_sum") %>% rename("ventrosa"  = "Hydrobia ventrosa", "ulvae" ="Hydrobia ulvae") %>% separate_wider_delim(cols = station_name, delim = "_", names = c("station", "station1")) %>% mutate(station_num = gsub("[^0-9]", "", station))

wide$station_num <-  as.numeric(wide$station_num)
wide <- wide[order(wide$station_num), ]

wide <- wide %>% mutate(location = case_when(station_num < 20 ~ "veiga", station_num > 20 ~ "glub")) %>% mutate(sum = ventrosa + ulvae) %>% mutate(dol = ventrosa/sum)

wide_sum <- wide %>% filter(location == "veiga") %>% summarise(meam = mean(dol), sd = sd(dol), se = sd/(length(dol)-1))

#расчет встречаемости по станциям-----
data_OP_occ <- data_OP %>% 
  group_by(station, taxon_name) %>% 
  summarise(all_biomass = sum(B_sqm, na.rm = TRUE), all_density = sum(N_sqm, na.rm = TRUE)) %>%
  group_by(taxon_name) %>% summarise(occ = length(taxon_name)) %>% mutate(freq = occ/11, occ_SE = sqrt(freq*(1-freq)/10)) %>% arrange(-freq) # отсортировал по убыванию встречаемости

#расчет встречаемости по пробам
data_OP_occ2 <- data_OP %>% group_by(station_name, taxon_name) %>% 
  summarise(all_biomass = sum(B_sqm, na.rm = TRUE), all_density = sum(N_sqm, na.rm = TRUE)) %>% group_by(taxon_name) %>% summarise(occ = length(taxon_name)) %>% mutate(freq = occ/33, occ_SE = sqrt(freq*(1-freq)/32)) %>% arrange(-freq)

data_OP_sum1 <- data_OP %>% group_by(location, taxon_name) %>% summarise(biomass_sum = sum(B_sqm, na.rm = TRUE), density_sum = sum(N_sqm, na.rm = TRUE))

# nMDS по численности----
 
data_mds <- left_join(data_OP, data_OP_occ, by = "taxon_name")

OP_wide <-  data_mds %>% filter(occ > 2) %>% mutate(tn = abbreviate(taxon_name)) %>% select(station, location, sample, tn, N_sqm) %>% group_by(station, sample, tn) %>%
  summarize(N_sqm = sum(N_sqm, na.rm = T))  %>% pivot_wider(names_from = tn, values_from = N_sqm, values_fill = 0) #из матрицы удалены все виды, встречающиеся меньше, чем 3 раза

OP_wide_mds <- OP_wide[,3:ncol(OP_wide)]

OP_wide_mds_m <- as.matrix(OP_wide_mds)

nmds <-  metaMDS(OP_wide_mds_m, distance  = "bray")
nmds$points

as.data.frame(nmds$species)

plot(nmds)

nmds$stress

OP_MDS <- OP_wide %>% add_column(MDS1 = nmds$points[,1]) %>% add_column(MDS2 = nmds$points[,2]) %>% add_column(location = c(rep("veiga", 14), rep("glub_near", 9), rep("glub_far", 9))) 

nmds_points <- ggplot(OP_MDS, aes(x = MDS1, y = MDS2)) + geom_text(aes(labels = station, vjust = +1.2)) + geom_point(aes(color = location))

#anosim
ano = anosim(OP_wide_mds_m, OP_MDS$location, distance = "bray", permutations = 9999)

OP_MDS <- OP_MDS %>% mutate(location = case_when(station < 20 ~ "veiga", station < 30 ~ "glub_near", station > 30 ~ "glub_far"))

legend <- c( "Вейга", "Перед Глубоким", "За Глубоким")

OP_MDS$location <- factor(OP_MDS$location,
                   levels = c("veiga", "glub_near", "glub_far"),
                   labels = c("Вейга", "Перед Глубоким", "За Глубоким"))

nmds_density <- ggplot(OP_MDS, aes(x = MDS1, y = MDS2)) + geom_text(aes(label = station, vjust = +1.2), size = 3, color = "grey70")  + geom_point(aes(color = location)) + coord_fixed() + xlim(-2.3, 2.3) + scale_color_discrete(name = "Локация", labels = legend) + theme_minimal() + stat_ellipse(aes(color = location), level = 0.75)

ggsave("nmds_density.png", plot = nmds_density)

# создаем датафрейм для многомерного анализа 
# без "Hydrobia sp.", из биомассы взят квадратный корень
OP_wide_bm_hydr_2 <-  data_mds %>% filter(taxon_name != "Hydrobia sp.") %>% mutate(B_sqrt = B_sqm^(1/2))%>% filter(occ > 2) %>% select(station, sample, taxon_name, B_sqrt) %>% group_by(station, sample, taxon_name) %>% summarize(B_sqrt_sum = sum(B_sqrt, na.rm = T)) %>% pivot_wider(names_from = taxon_name, values_from = B_sqrt_sum, values_fill = 0) %>% mutate(location = case_when(station < 20 ~ "veiga", station < 30 ~ "glub_near", station > 30 ~ "glub_far")) %>% relocate(location, .before = station) 

location <- OP_wide_bm_hydr_2$location

OP_MDS_bm_hydr_2 <- OP_wide_bm_hydr_2[,4:ncol(OP_wide_bm_hydr_2)]

OP_MDS_bm_m_hydr_2 <- as.matrix(OP_MDS_bm_hydr_2)

#насколько фактор локация определяет различия между станциями----
perm_result_2 <- adonis2(OP_MDS_bm_m_hydr_2 ~ location, permutations = 999)

beta_disp2 <- betadisper(vegdist(OP_MDS_bm_m_hydr_2, method = "bray"), location)

#simper. за счет каких видов различаются станции-----
sim_1 <- simper(OP_MDS_bm_m_hydr_2, location, permutations = 999)
sim_sum <- summary(sim_1)

# для каждого попарного сравнения выбираем виды, которые значимо влияют
vpg_dif <- as.data.frame(sim_sum$veiga_glub_near) %>% filter(cumsum < 0.995 & p < 0.05) %>% mutate(location = "veiga_glub_near")

vzg_dif <- as.data.frame(sim_sum$veiga_glub_far) %>% filter(cumsum < 0.995 & p < 0.05) %>% mutate(location = "veiga_glub_far")

pgzg_dif <- as.data.frame(sim_sum$glub_near_glub_far) %>% filter(cumsum < 0.995 & p < 0.05) %>% mutate(location = "glub_near_glub_far")

simper_df <- rbind(vpg_dif, vzg_dif, pgzg_dif) 

#анализ индикаторных видов------

indval <- multipatt(OP_MDS_bm_hydr_2, location, func = "IndVal.g", control = how(nperm = 999))
raw_p <- indval$sign$p.value

# поправка на множественные сравнения
adjusted_p <- p.adjust(raw_p, method = "fdr") 

# создаем датафрейм только с теми видами, которые влияют значимо
ind_sum <- as.data.frame(indval$sign)
ind_sum$adjusted_p <- adjusted_p
ind_sum_p <- ind_sum %>% filter(adjusted_p < 0.05)
ind_sum_p_names <- data.frame(Species = rownames(ind_sum_p),ind_sum_p, row.names = NULL)
summary(indval, alpha = 0.10, indvalcomp = TRUE)

# создаем датафрейм с названиями видов для нанесения на график
species <-(rownames(ind_sum_p))
species <- c(species, "Littorina saxatilis", "Enchitreidae")
species <- species[species != "Eteone longa"]
species <- as.data.frame(species)

#nmds по биомассе-----
nmds_bm_new_hydr_2 <- metaMDS(OP_MDS_bm_m_hydr_2, distance  = "bray", trymax = 100)
nmds_bm_new_hydr_2$points
nmds_bm_new_hydr_2$stress
as.data.frame(nmds_bm_new_hydr_2$species)
plot(nmds_bm_new_hydr_2)

#создаем отдельные датафрейм с координатами точек nmds
nmds_bm_points <- as.data.frame(nmds_bm_new_hydr_2$points)
nmds_bm_points$location <- location
nmds_bm_points$station <- OP_wide_bm_hydr_2$station
nmds_bm_points <- nmds_bm_points %>% mutate(location = factor(location, levels = c("veiga","glub_near", "glub_far"))) 

#создаем отдельные датафрейм с координатами конца стрелок nmds
sp.fit <- as.data.frame(nmds_bm_new_hydr_2$species)
sp.fit$species <- rownames(sp.fit)
sp.fit.fin <- left_join(species, sp.fit, by = "species") #выбираем только те виды, которые значимо влияют по результатам анализа индикаторых видов
sp.fit.fin$species[sp.fit.fin$species == "Hydrobia ventrosa"] <- "Ecrobia ventrosa"
sp.fit.fin$loc <- c(rep("veiga", 5), rep("glub", 9))

#создаем отдельные датафреймы с точками и стрелочками для вейги и глубокого
nmds_bm_v <- nmds_bm_points %>% filter(location == "veiga") 
sp.fit.fin_v <- sp.fit.fin %>% filter(loc == "veiga")
nmds_bm_g <- nmds_bm_points %>% filter(location == "glub_near"|location == "glub_far")
sp.fit.fin_g <- sp.fit.fin %>% filter(loc == "glub")

# итоговый график с результатами nmds
nmds_bm <- ggplot() + geom_point(data = nmds_bm_v, aes(x = MDS1, y = MDS2, color = location)) + coord_fixed() + xlim(-2.3, 2.3) + theme_minimal() + stat_ellipse(data = nmds_bm_v, aes(x = MDS1, y = MDS2, color = location), level = 0.83) + scale_color_manual(values = locs_colors) + geom_segment(data = sp.fit.fin_v, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), color = "grey70", arrow = arrow(length = unit(.2, "cm"), angle = 20)) + geom_text_repel(data = sp.fit.fin_v, aes(x = MDS1, y = MDS2, label = species), size = 3, point.padding = unit(0.2, "lines"), box.padding = unit(0.3, "lines"), force = 0.6, nudge_x = -0.25, direction = "both", max.overlaps = Inf) + xlim(-2, 2) + ylim(-2, 1.5) + geom_point(data = nmds_bm_g, aes(x = MDS1, y = MDS2, color = location)) + coord_fixed() + xlim(-2.3, 2.3) + theme_minimal() + stat_ellipse(data = nmds_bm_g, aes(x = MDS1, y = MDS2, color = location), level = 0.83) + scale_color_manual(name = "локация", labels = c("Вейга", "перед Глубоким", "за Глубоким"), values = locs_colors) + geom_segment(data = sp.fit.fin_g, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), color = "grey70", arrow = arrow(length = unit(.2, "cm"), angle = 20)) + geom_text_repel(data = sp.fit.fin_g, aes(x = MDS1, y = MDS2, label = species), size = 3, point.padding = unit(0.2, "lines"), box.padding = unit(0.3, "lines"), force = 0.6, nudge_x = 0.25, direction = "both", max.overlaps = Inf) + theme(legend.position = "top", legend.text = element_text(size = 13), legend.title = element_text(size = 12), axis.text = element_text(size = 13), axis.title = element_text(size = 13)) + xlim(-2, 2) + ylim(-2, 1.5)

ggsave("nmds_bm_2.jpg", plot = nmds_bm, path = "C:\\Users\\user\\Documents\\лэмб\\Onega_benthos\\OnegaBenthos25")




