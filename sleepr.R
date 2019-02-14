library(sleepr)
library(zoo)
dt <- load_dam(metadata, FUN = sleepr::sleep_dam_annotation)
dt

library(sleepr)
library(ggetho)

URL <- "https://github.com/rethomics/rethomics.github.io/raw/source/material/sleep_dam.RData"
load(url(URL))
summary(dt)

ggetho(dt, aes(z=asleep)) +
  stat_ld_annotations(height = 1)+
  stat_tile_etho() 

dt_curated <- curate_dead_animals(dt)
summary(dt_curated)
setdiff(dt[, id, meta=T],
        dt_curated[, id, meta=T])
ggetho(dt_curated, aes(z=asleep)) +
  stat_ld_annotations(ypos = "top")+
  stat_tile_etho() 


# we make a summary table of all lifespan for each animals
lifespan_dt <- dt_curated[, .(lifespan = max(t)), by=id]
# we filter this table for lifespan>2 and we keep the id
valid_ids <- lifespan_dt[lifespan > days(2), id]
# we apply this filter
dt_curated <- dt_curated[id %in% valid_ids]
summary(dt_curated)


dt_curated <- dt_curated[t %between% c(days(0), days(2.5))]
summary(dt_curated)

p <- ggetho(dt_curated, aes(y=asleep, colour=sex)) +
  stat_pop_etho() +
  stat_ld_annotations() +
  facet_grid(genotype ~ .)

ggplot2_data <- dt_curated
ggplot2_data <- left_join(ggplot2_data, select(dt_curated[meta=T], id, sex, genotype), by = "id")


ggplot2_data$tm <- ggplot2_data$t / 60
ggplot2_data <- ggplot2_data %>% arrange(genotype, sex, id)

# average_mean_30_mins <- lapply(c("A", "B", "C"), function(g) lapply(c("M", "F"), function(s) rollmean(x = filter(, genotype == g, sex == s) %>% select(asleep), k = 30)))

rolled_mean <- lapply(0:3570, function(delta) {  
  if (delta %% 100 == 0) print(delta)
  res <- ggplot2_data %>% filter(tm > delta, tm < delta+30 ) %>%
    group_by(sex, genotype) %>% summarise(mean_asleep = mean(asleep), sd_asleep = sd(asleep)
                                          # q1 = quantile(asleep)[2], q3 = quantile(asleep)[4]
                                          )
  res$d <- delta
  res
  }
)


rolled_mean_bind <- do.call(what = rbind, args = rolled_mean)

g <- ggplot(data = rolled_mean_bind,
       mapping = aes(x = d / 60,
                     y = mean_asleep)) +
  
  geom_line(aes(col = sex)) +
  facet_grid(rows = vars(genotype)) +
  
  geom_ribbon(aes(x = d / 60,
                  ymin = mean_asleep - sd_asleep,
                  # ymin = q1,
                  ymax = mean_asleep + sd_asleep),
                  # ymax = q3),
              alpha=0.1)
g
g + labs(x="hours", y = "asleep")
