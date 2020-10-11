exercise_database <- list(
  list(
    datasets = list(list(package = "palmerpenguins", name = "penguins")),
    title = "Group by species and count",
    text_instructions = "Count the number of rows by species ordered by species in ascending order",
    id = 1,
    sql_solution = glue::glue_sql("
SELECT species, COUNT(*) AS n
FROM penguins
GROUP BY species
ORDER BY species
"),
    dplyr = quote({
      library(dplyr)
      data("penguins", package = "palmerpenguins")
      penguins %>%
        count(species) %>%
        arrange(species)
    }),
    datatable = quote({
      library(data.table)
      data("penguins", package = "palmerpenguins")
      penguins <- as.data.table(penguins)
      penguins[order(species), .N, by = species]
    })
  ),
  list(
    datasets = list(list(package = "palmerpenguins", name = "penguins")),
    title = "Average body mass per species",
    text_instructions = "Compute the average body mass per species on islands with more than 60 observations \nordered by highest average body mass in descending order. Exclude those that have a missing body mass.",
    id = 2,
    sql_solution = glue::glue_sql("
SELECT species, AVG(body_mass_g) AS avg_body_mass
FROM penguins P
INNER JOIN (
  SELECT island
  FROM penguins
  GROUP BY island
  HAVING COUNT(*) > 60) T ON T.island = P.island
WHERE body_mass_g IS NOT NULL
GROUP BY species
ORDER BY AVG(body_mass_g) DESC
"),
    dplyr = quote({
      library(dplyr)
      data("penguins", package = "palmerpenguins")
      penguins %>%
        filter(!is.na(body_mass_g)) %>%
        group_by(island) %>%
        filter(n() > 60) %>%
        group_by(species) %>%
        summarise(avg_body_mass = mean(body_mass_g), .groups = "drop") %>%
        arrange(desc(avg_body_mass))
    }),
    datatable = quote({
      library(data.table)
      data("penguins", package = "palmerpenguins")
      "Needs to be written"
    })
  )
)
