DATA_DIR <- "."
list.files(DATA_DIR, pattern= "*.txt|*.csv")

library(dplyr)
library(damr)
metadata <- fread("metadata.csv")
metadata
# View(metadata)

metadata <- link_dam_metadata(metadata, result_dir = DATA_DIR)
dt <- load_dam(metadata)
summary(dt)

library(ggetho)
ggetho(dt[xmv(replicate) == 1 ], aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()

dt[meta=T, replicate == 1] %>% View

# Let's get rid of not OK animals





metadata$status <- "OK"
metadata <- metadata %>% arrange(replicate, genotype, id)
metadata[grep(pattern = "Monitor64.txt\\|26", x = metadata$id)[1],"status"] <- "dead"
metadata$file <- lapply(metadata$file_info, function(x) x[[2]]) %>% unlist
metadata <- link_dam_metadata(metadata, result_dir = DATA_DIR)
dt <- load_dam(metadata[status == "OK"])
summary(dt)
library(ggetho)
ggetho(dt[xmv(replicate) == 1 ], aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()


# Toy experiment
# metadata <- data.table( id = paste("toy_experiment", 1:10, sep = "|"),
#                         sex = rep(c("male", "female"), each = 5),
#                         condition = c("A", "B") )
# metadata
# dt <- toy_dam_data(metadata, duration = days(1))
# dt
