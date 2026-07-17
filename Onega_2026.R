## LIBS ####
install.packages("xlsx")
install.packages("writexl")
install.packages("vegan")
library(tidyverse)
library(writexl)
library(readxl)
library(vegan)

## data-set ####
data_OP <- read_delim("https://raw.githubusercontent.com/Amauropsis/OnegaBenthos25/refs/heads/main/OP_NB.csv", delim = ";")

write_xlsx(data_OP, "data_OP.xlsx")

data_OP <- read_xlsx(file.choose(), sheet = "Sheet1")
data_OP$density <- as.numeric(data_OP$density)
data_OP$biomass <- as.numeric(data_OP$biomass)

#пересчитываем на метр
data_OP1 <- data_OP %>% filter(area == 30) %>% mutate(density_new = density*30) %>% mutate(biomass_new = biomass*30)
data_OP2 <- data_OP %>% filter(area == 1) %>% mutate(density_new = density*4) %>% mutate(biomass_new = biomass*4)
data_OP_new <- rbind(data_OP1, data_OP2)

#суммарная биомасса и численность в Вейге и Глубоком
data_OP_sum <- data_OP_new %>% group_by(station_name) %>% summarise(biomass_sum = sum(biomass_new, na.rm = TRUE), density_sum = sum(density_new, na.rm = TRUE)) %>% separate_wider_delim(cols = station_name, delim = "_", names = c("station", "station1")) %>% mutate(location = c(rep("veiga", 11), rep("glub", 18), rep("veiga", 3)))


ggplot(data_OP_sum, aes(y = density_sum, line = station, fill = as.factor(location))) + geom_boxplot()

ggplot(data_OP_sum, aes(y = biomass_sum, line = station, fill = as.factor(location))) + geom_boxplot()

#рассчет встречаемости по станциям
data_OP_vst <- data_OP_new %>% group_by(station, taxon_name) %>% summarise(all_biomass = sum(biomass_new, na.rm = TRUE), all_density = sum(density_new, na.rm = TRUE))

data_OP_vst1 <- data_OP_vst %>% group_by(taxon_name) %>% summarise(vst = length(taxon_name)) %>% mutate(vst1 = vst/11, SE = sqrt(vst1*(1-vst1)/11))

#рассчет встречаемости по пробам
data_OP_vst2 <- data_OP_new %>% group_by(station_name, taxon_name) %>% summarise(all_biomass = sum(biomass_new, na.rm = TRUE), all_density = sum(density_new, na.rm = TRUE))

data_OP_vst3 <- data_OP_vst2 %>% group_by(taxon_name) %>% summarise(vst = length(taxon_name)) %>% mutate(vst1 = vst/33, SE = sqrt(vst1*(1-vst1)/33))


data_OP_sum1 <- data_OP_new %>% group_by(location, taxon_name) %>% summarise(biomass_sum = sum(biomass_new, na.rm = TRUE), density_sum = sum(density_new, na.rm = TRUE))

#индекс шенона по приколу

OP_glub <- data_OP_sum1 %>% filter(location == "glub") 
glub_all <- sum(OP_glub$density_sum)
glub_all_biomass <- sum(OP_glub$biomass_sum)
OP_glub_shenon <- OP_glub %>% mutate(vkl = density_sum/glub_all, ln = -vkl*log10(vkl)) %>% summarise(shenon = sum(ln, na.rm = TRUE))
OP_glub_shenon_biomass <- OP_glub %>% mutate(vkl = biomass_sum/glub_all_biomass, ln = -vkl*log10(vkl)) %>% summarise(shenon = sum(ln, na.rm = TRUE))

OP_veiga <- data_OP_sum1 %>% filter(location == "veiga")
veiga_all <- sum(OP_veiga$density_sum)
veiga_all_biomass <- sum(OP_veiga$biomass_sum)
OP_veiga_shenon <- OP_veiga %>% mutate(vkl = density_sum/veiga_all, ln = -vkl*log10(vkl)) %>% summarise(shenon = sum(ln, na.rm = TRUE))
OP_veiga_shenon_biomass <- OP_veiga %>% mutate(vkl = biomass_sum/veiga_all_biomass, ln = -vkl*log10(vkl)) %>% summarise(shenon = sum(ln, na.rm = TRUE))

#MDS
OP_MDS <- data_OP_new %>% select(station, taxon_name, density_new) %>% group_by(station, taxon_name) %>% summarise(all_density = sum(density_new, na.rm = TRUE)) %>% ungroup() %>% pivot_wider(names_from = taxon_name, values_from = all_density) %>% as.data.frame()

OP_MDS <- OP_MDS %>% replace(is.na(OP_MDS), 0)

rownames(OP_MDS) <- OP_MDS$taxon_name

OP_MDS1 <- OP_MDS[, -1] 

OP_MDS1 <- as.matrix(OP_MDS1)

set.seed(1876598979)
dist <- vegdist(OP_MDS1, method = "bray") 

nmds <- metaMDS(dist)

nmds$points

scores(nmds) %>% as_tibble(rownames = "station") %>% mutate(station = c(8, 16, 17, 18, 19, 23, 24, 25, 31, 32, 34), location = c(rep("veiga", 5), rep("glub", 6))) %>% ggplot(aes(x = NMDS1, y = NMDS2, shape = as.factor(location), color = as.factor(station))) + geom_point()


write_xlsx(data_OP_sum1, "data_OP_sum1.xlsx")
                                                                                                              