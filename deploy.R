#!/usr/bin/Rscript

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio/bin/pandoc")

COMMIT_MSG <- paste0(
  "Static Website index.html updated on ", Sys.Date()
  # "Fix duplicate in df_time_played"
)

file.copy(
  from = "/opt/spunkybot/data.sqlite",
  to = "/home/daniel/ut-eterstats.github.io/src/data/",
  overwrite = TRUE
)

file.copy(
  from = "/opt/spunkybot/devel.log",
  to = "/home/daniel/ut-eterstats.github.io/src/data/",
  overwrite = TRUE
)

rmarkdown::render("/home/daniel/ut-eterstats.github.io/src/stats.Rmd")

file.copy(
  from = "/home/daniel/ut-eterstats.github.io/src/stats.html",
  to = "/home/daniel/ut-eterstats.github.io/index.html",
  overwrite = TRUE
)

system(
  paste0("git --git-dir=/home/daniel/ut-eterstats.github.io/.git --work-tree=/home/daniel/ut-eterstats.github.io/ commit -m '", COMMIT_MSG, "' index.html; git --git-dir=/home/daniel/ut-eterstats.github.io/.git push;")  
)
