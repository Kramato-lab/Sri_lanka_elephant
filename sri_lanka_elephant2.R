library(vegan)
library(ggplot2)
library(car)


setwd("/Users/katherineamato/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Sri_lanka_elephant")

#read in alpha data
alpha = read.table("alpha_div.txt", header = T)

#Alpha diversity
##overall
time<-lm(alpha$shannon_entropy~min_to_collect, data=alpha)
summary(aov(time))

time2<-lm(alpha$observed_features~min_to_collect, data=alpha)
summary(aov(time2))

time3<-lm(alpha$faith_pd~min_to_collect, data=alpha)
summary(aov(time3))

loc<-lm(alpha$shannon_entropy~location, data=alpha)
summary(aov(loc))

loc2<-lm(alpha$observed_features~location, data=alpha)
summary(aov(loc2))

loc3<-lm(alpha$faith_pd~location, data=alpha)
summary(aov(loc3))

mo<-lm(alpha$shannon_entropy~Month, data=alpha)
summary(aov(mo))

mo2<-lm(alpha$observed_features~Month, data=alpha)
summary(aov(mo2))

mo3<-lm(alpha$faith_pd~Month, data=alpha)
summary(aov(mo3))

shannon_mo <- ggplot(alpha, aes(x=Month_order, y= shannon_entropy)) +
  geom_boxplot(aes(fill=Month_order), color="black") + 
  scale_fill_manual(values=c('darkorange','darkgoldenrod2','indianred2','gray30','gray90','cyan2','dodgerblue1','turquoise4'))+
  geom_point(size=0.75) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  xlab("Month") + ylab("Shannon Diversity")
shannon_mo

rf<-lm(alpha$shannon_entropy~Rain, data=alpha)
summary(aov(rf))

rf2<-lm(alpha$observed_features~Rain, data=alpha)
summary(aov(rf2))

rf3<-lm(alpha$faith_pd~Rain, data=alpha)
summary(aov(rf3))

s<-lm(alpha$shannon_entropy~sex, data=alpha)
summary(aov(s))

s2<-lm(alpha$observed_features~sex, data=alpha)
summary(aov(s2))

s3<-lm(alpha$faith_pd~sex, data=alpha)
summary(aov(s3))

a<-lm(alpha$shannon_entropy~age, data=alpha)
summary(aov(a))

a2<-lm(alpha$observed_features~age, data=alpha)
summary(aov(a2))

a3<-lm(alpha$faith_pd~age, data=alpha)
summary(aov(a3))

b<-lm(alpha$shannon_entropy~BCS, data=alpha)
summary(aov(b))

b2<-lm(alpha$observed_features~BCS, data=alpha)
summary(aov(b2))

b3<-lm(alpha$faith_pd~BCS, data=alpha)
summary(aov(b3))


##Minneriya only
alpha_minn = read.table("alpha_div_minn.txt", header = T)

v_minn<-lm(alpha_minn$shannon_entropy~Vehic, data=alpha_minn)
summary(aov(v_minn))

v_lag_minn<-lm(alpha_minn$shannon_entropy~Vehic_1, data=alpha_minn)
summary(aov(v_lag_minn))

v_lagg_minn<-lm(alpha_minn$shannon_entropy~Vehic_2, data=alpha_minn)
summary(aov(v_lagg_minn))

v_cat_minn<-lm(alpha_minn$shannon_entropy~Vehic_cat, data=alpha_minn)
summary(aov(v_cat_minn))
TukeyHSD(aov(v_cat_minn))

shannon_minn <- ggplot(alpha_minn, aes(x=Vehic_cat, y=shannon_entropy)) +
  geom_boxplot(aes(fill=Vehic_cat), color="black") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  xlab("Vehic_cat") + ylab("Shannon Diversity")
shannon_minn

shannon_minn <- ggplot(alpha_minn, aes(x=Vehic_1, y=shannon_entropy)) +
  geom_point() + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  xlab("Vehic_1") + ylab("Shannon Diversity")
shannon_minn

#Beta diversity
library(vegan)

##overall
uw_dm = as.dist(read.table("uw_distance-matrix.tsv", header = T))
w_dm = as.dist(read.table("w-distance-matrix.tsv", header = T))
metadata = read.table("metadata_KRA.txt", header = T)

adonis2(uw_dm ~ min_to_collect, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect, data=metadata, permutations = 5000) 

adonis2(uw_dm ~ min_to_collect+Month, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+Month, data=metadata, permutations = 5000)

adonis2(uw_dm ~ min_to_collect+Rain, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+Rain, data=metadata, permutations = 5000)

adonis2(uw_dm ~ min_to_collect+location, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+location, data=metadata, permutations = 5000) 

adonis2(uw_dm ~ min_to_collect+age, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+age, data=metadata, permutations = 5000)

adonis2(uw_dm ~ min_to_collect+BCS, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+BCS, data=metadata, permutations = 5000) 

adonis2(uw_dm ~ min_to_collect+Month+location, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+Month+location, data=metadata, permutations = 5000)

adonis2(uw_dm ~ min_to_collect+Month, strata=metadata$location, data=metadata, permutations = 5000) 
adonis2(w_dm ~ min_to_collect+Month, strata=metadata$location, data=metadata, permutations = 5000)

uw_mds<-metaMDS(uw_dm, trymax=100)
uw_mds.points<-merge(x = uw_mds$points, y = metadata, by.x = "row.names", by.y = "sample.id")
uwnmds <- ggplot(uw_mds.points, aes(x = MDS1, y = MDS2)) +  
  geom_point(aes(color=Month), size=5) +
  scale_color_manual(values=c('gray30', 'turquoise4', 'indianred2', 'darkgoldenrod2', 'darkorange', 'dodgerblue1', 'cyan2', 'gray90'))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
uwnmds

w_mds<-metaMDS(w_dm, trymax=100)
w_mds.points<-merge(x = w_mds$points, y = metadata, by.x = "row.names", by.y = "sample.id")
wnmds <- ggplot(w_mds.points, aes(x = MDS1, y = MDS2)) +  
  geom_point(aes(color=Month), size=5) +
  scale_color_manual(values=c('gray30', 'turquoise4', 'indianred2', 'darkgoldenrod2', 'darkorange', 'dodgerblue1', 'cyan2', 'gray90'))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
wnmds

uw_mds<-metaMDS(uw_dm, trymax=100)
uw_mds.points<-merge(x = uw_mds$points, y = metadata, by.x = "row.names", by.y = "sample.id")
site_uwnmds <- ggplot(uw_mds.points, aes(x = MDS1, y = MDS2)) +  
  geom_point(aes(color=location), size=5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
site_uwnmds

w_mds<-metaMDS(w_dm, trymax=100)
w_mds.points<-merge(x = w_mds$points, y = metadata, by.x = "row.names", by.y = "sample.id")
site_wnmds <- ggplot(w_mds.points, aes(x = MDS1, y = MDS2)) +  
  geom_point(aes(color=location), size=5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
site_wnmds

###Beta dispersion overall
anova(betadisper(uw_dm, metadata$location)) 
anova(betadisper(w_dm, metadata$location)) 

anova(betadisper(uw_dm, metadata$Month)) 
anova(betadisper(w_dm, metadata$Month)) 

##Minneriya only
uw_dm_minn = as.dist(read.table("uw_distance-matrix_minn.txt", header = T))
w_dm_minn = as.dist(read.table("w-distance-matrix_minn.txt", header = T))
metadata_minn = read.table("metadata_KRA_minn.txt", header = T)

adonis2(uw_dm_minn ~ min_to_collect+Month, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Month, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Vehic, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Vehic, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Vehic_1, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Vehic_1, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Vehic_2, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Vehic_2, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Monthprior, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Monthprior, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Weekprior, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Weekprior, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Twodayprior, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Twodayprior, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Rain, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Rain, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Vehic_1+Rain, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Vehic_1+Rain, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Vehic_1+Month, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Vehic_1+Month, data=metadata_minn, permutations = 5000)

adonis2(uw_dm_minn ~ min_to_collect+Vehic_1+Rain+Month, data=metadata_minn, permutations = 5000) 
adonis2(w_dm_minn ~ min_to_collect+Vehic_1+Rain+Month, data=metadata_minn, permutations = 5000)

#ANCOM 
##for site
library(tidyverse)
library(phyloseq)
library(ANCOMBC)

asv = read.table("asv_table_abc.txt", header=T, check.names=FALSE)
metadata_full = read.table("metadata_KRA.txt", header=T)
taxonomy = read.table("taxonomy_abc.txt", header=T)

asv_matrix = asv %>% column_to_rownames("sampleid") %>% as.matrix()
tax_matrix = taxonomy %>% column_to_rownames("feature") %>% as.matrix()
meta = metadata_full %>% column_to_rownames("sampleid")

ASV<-otu_table(asv_matrix, taxa_are_rows = TRUE)
TAX<-tax_table(tax_matrix)
samples<-sample_data(meta)

asv_phylo = phyloseq(ASV, TAX, samples)

site_asv = ancombc2(data=asv_phylo, fix_formula="location",
                     p_adj_method = "fdr", lib_cut = 10000, group="location",
                     global = T)

res_t_asv<-site_asv$res
res_global_t_asv<-site_asv$res_global

write_csv(res_t_asv, "Diff_abund_site_asv.csv")
write_csv(res_global_t_asv, "Diff_abund_site_asv_global.csv")

#MAASLIN
library(Maaslin2)

##time to collection

asv_m = read.table("asv_table_maaslin.txt", header=T, check.names=FALSE)
metadata_full = read.table("metadata_KRA.txt", header=T)

fit_data2 = Maaslin2(
  input_data = asv_m, 
  input_metadata = metadata_full, 
  output = "time_asv", 
  fixed_effects = "min_to_collect")

genus_m = read.table("genus_for_r.txt", header=T, check.names=FALSE)
metadata_full = read.table("metadata_KRA.txt", header=T)

fit_data2 = Maaslin2(
  input_data = genus_m, 
  input_metadata = metadata_full, 
  output = "time_genus", 
  fixed_effects = "min_to_collect")

##month

fit_data2 = Maaslin2(
  input_data = asv_m, 
  input_metadata = metadata_full, 
  output = "month_asv", 
  fixed_effects = "Month.1",
  random_effects = "location")

fit_data2 = Maaslin2(
  input_data = genus_m, 
  input_metadata = metadata_full, 
  output = "month_genus", 
  fixed_effects = "Month.1",
  random_effects = "location")

##rainfall

asv_minn_m = read.table("asv_table_maaslin_minn.txt", header=T, check.names=FALSE)
metadata_minn = read.table("metadata_KRA_minn.txt", header=T)

fit_data2 = Maaslin2(
  input_data = asv_minn_m, 
  input_metadata = metadata_minn, 
  output = "rain_asv_minn", 
  fixed_effects = "Rain")

genus_minn_m = read.table("genus_for_r_minn.txt", header=T, check.names=FALSE)
metadata_minn = read.table("metadata_KRA_minn.txt", header=T)

fit_data2 = Maaslin2(
  input_data = genus_minn_m, 
  input_metadata = metadata_minn, 
  output = "rain_genus_minn", 
  fixed_effects = "Rain")

##vehicle

fit_data2 = Maaslin2(
  input_data = asv_minn_m, 
  input_metadata = metadata_minn, 
  output = "vehic_asv_minn", 
  fixed_effects = "Vehic_1")

fit_data2 = Maaslin2(
  input_data = genus_minn_m, 
  input_metadata = metadata_minn, 
  output = "vehic_genus_minn", 
  fixed_effects = "Vehic_1")

fit_data2 = Maaslin2(
  input_data = asv_minn_m, 
  input_metadata = metadata_minn, 
  output = "vehic_asv_minn2", 
  fixed_effects = "Vehic_2")

fit_data2 = Maaslin2(
  input_data = genus_minn_m, 
  input_metadata = metadata_minn, 
  output = "vehic_genus_minn2", 
  fixed_effects = "Vehic_2")

