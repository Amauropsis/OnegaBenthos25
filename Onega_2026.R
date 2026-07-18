## LIBS ####
# install.packages("xlsx")
# install.packages("writexl")
# install.packages("vegan")
library(tidyverse)
library(writexl)
library(readxl)
library(vegan)

## data-set ####
data_OP <- read.delim2("https://raw.githubusercontent.com/Amauropsis/OnegaBenthos25/refs/heads/main/OP_NB.csv", sep = ";") # исправил, потому что в read_delim нет уточнения про десятичный разделитель, а в read.delim2 он "," по умолчанию, но можно эксплицитно поставить "." или ","

# write_xlsx(data_OP, "data_OP.xlsx") # это я не понял, зачем, прокомментируй, плз, зачем
# data_OP <- read_xlsx(file.choose(), sheet = "Sheet1") 
data_OP$density <- as.numeric(data_OP$density)
data_OP$biomass <- as.numeric(data_OP$biomass)

#пересчитываем на метр
# data_OP1 <- data_OP %>% filter(area == 30) %>% mutate(density_new = density*30) %>% mutate(biomass_new = biomass*30)
# data_OP2 <- data_OP %>% filter(area == 1) %>% mutate(density_new = density*4) %>% mutate(biomass_new = biomass*4)
# data_OP_new <- rbind(data_OP1, data_OP2)

data_OP <- data_OP %>%  # упростил предыдущий код
  mutate(N_sqm = case_when(area == 30 ~ density * 30, area == 1 ~ density * 4)) %>%
  mutate(B_sqm = case_when(area == 30 ~ biomass * 30, area == 1 ~ biomass * 4))


#суммарная биомасса и численность в Вейге и Глубоком
data_OP_sum <- data_OP %>% group_by(station_name) %>% summarise(biomass_sum = sum(B_sqm, na.rm = TRUE), density_sum = sum(N_sqm, na.rm = TRUE)) %>% separate_wider_delim(cols = station_name, delim = "_", names = c("station", "station1")) %>% mutate(location = c(rep("veiga", 11), rep("glub", 18), rep("veiga", 3)))


ggplot(data_OP_sum, aes(y = density_sum, line = station, fill = as.factor(location))) + geom_boxplot()

ggplot(data_OP_sum, aes(y = biomass_sum, line = station, fill = as.factor(location))) + geom_boxplot()

#расчет встречаемости по станциям
data_OP_occ <- data_OP %>% 
  group_by(station, taxon_name) %>% 
  summarise(all_biomass = sum(B_sqm, na.rm = TRUE), all_density = sum(N_sqm, na.rm = TRUE)) %>%
  group_by(taxon_name) %>% summarise(occ = length(taxon_name)) %>% mutate(freq = occ/11, occ_SE = sqrt(freq*(1-freq)/10)) %>% # там ошибка была в формуле - в делителе SE n-1, а не n
  arrange(-freq) # отсортировал по убыванию встречаемости

#расчет встречаемости по пробам
data_OP_occ2 <- data_OP %>% group_by(station_name, taxon_name) %>% 
  summarise(all_biomass = sum(B_sqm, na.rm = TRUE), all_density = sum(N_sqm, na.rm = TRUE)) %>%
  group_by(taxon_name) %>% 
  summarise(occ = length(taxon_name)) %>% mutate(freq = occ/33, occ_SE = sqrt(freq*(1-freq)/32)) %>%
  arrange(-freq)


 data_OP_sum1 <- data_OP %>% group_by(location, taxon_name) %>% summarise(biomass_sum = sum(B_sqm, na.rm = TRUE), density_sum = sum(N_sqm, na.rm = TRUE))

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

# многомерное шкалирование nMDS ----

# Плотность
# подготовка дата-фрейма
OP_wide <-  data_OP %>% select(station, sample, taxon_name, N_sqm) %>%
  group_by(station, sample, taxon_name)%>%
  summarize(N_sqm = sum(N_sqm, na.rm = T)) %>% #в базе есть дублирующиеся строчки?
  pivot_wider(names_from = taxon_name, values_from = N_sqm) # плохая матрица 32 строчки на 55 видов. Почему, кстати, 32? Там же должно быть 33, в каждой станции брали по 3 пробы...


#OP_MDS <- data_OP %>% select(station, taxon_name, N_sqm) %>% group_by(station, taxon_name) %>% summarise(all_density = sum(density_new, na.rm = TRUE)) %>% ungroup() %>% pivot_wider(names_from = taxon_name, values_from = all_density) %>% as.data.frame()

# Вот для примера код про MDS с визуализацией

# data_ord <- data.noage %>% mutate(par.short = abbrv(Par.sp)) %>% pivot_wider(id_cols = c("Period", "Location", "Station"), names_from = "par.short", values_from = "ext")
# 
# ord <- metaMDS(data_ord[4:10], distance = "euclidean")
# points <- as.data.frame(ord$points)
# data_ord$mds1 <- points$MDS1
# data_ord$mds2 <- points$MDS2
# sp.fit <- as.data.frame(ord$species)
# sp.fit$sp <- rownames(sp.fit)
# 
# ord.plot <- ggplot(data_ord) + geom_point(aes(x = mds1, y = mds2, color = as.factor(Period), shape = as.factor(Location)), size = 2) + theme_bw() + scale_color_brewer(type = "qual", palette = "Accent", name = "Период") + scale_shape_discrete(name = "Локация", labels = c("Б. Цинковый", "М. Цинковый")) + scale_x_continuous(name = "nMDS1", breaks = NULL) + scale_y_continuous(name = "nMDS2", breaks = NULL) + geom_segment(data = sp.fit, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), color = "darkred", arrow = arrow(length = unit(.2, "cm"), angle = 20))  + geom_text(data = sp.fit, aes(x = MDS1, y = MDS2, label = sp), vjust = -.5, color = "darkred") + annotate("text", x = .35, y = .25, label = expression(italic("Stress = 0.01")))


# дальше начинаются обрывки кода, которые я делал во время беседы
OP_MDS <- OP_wide %>% replace(is.na(OP_wide), 0)

mds_op <- metaMDS(OP_MDS[3:55])

mds_op$stress

plot(mds_op)

OP_MDS <- OP_MDS %>% add_column(MDS1 = mds_op$points[,1]) %>% add_column(MDS2 = mds_op$points[,2])

ggplot(OP_MDS, aes(x = MDS1, y = MDS2)) + geom_text(aes(labels = station))


rownames(OP_MDS) <- OP_MDS$taxon_name

OP_MDS1 <- OP_MDS[, -1] 

OP_MDS1 <- as.matrix(OP_MDS1)

set.seed(1876598979)
dist <- vegdist(OP_MDS1, method = "bray") 

nmds <- metaMDS(dist)

nmds$points

scores(nmds) %>% as_tibble(rownames = "station") %>% mutate(station = c(8, 16, 17, 18, 19, 23, 24, 25, 31, 32, 34), location = c(rep("veiga", 5), rep("glub", 6))) %>% ggplot(aes(x = NMDS1, y = NMDS2, shape = as.factor(location), color = as.factor(station))) + geom_point()


write_xlsx(data_OP_sum1, "data_OP_sum1.xlsx")
                                                                                                              