library(TAF)

sam_assessment <- "wit.27.3a47d_2025_ICupdated"

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/conf/"
  )

files <- "model.cfg"

for (file in files) {
  download(paste0(sam_dir, file))
}

