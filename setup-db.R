# build_duckdbs.R
# Convert two Hive-style partitioned Parquet directories into two DuckDB files
#   prep/demand_partitioned  -> data/demand.duckdb (table: demand)
#   prep/supply_partitioned  -> data/supply.duckdb (table: supply)
# With: ordering on hot keys and composite indexes

library(DBI)
library(duckdb)

# ---------- Helpers ----------
list_parquet <- function(dir) {
  stopifnot(dir.exists(dir))
  fs <- list.files(dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  if (length(fs) == 0) stop("No parquet files found in ", dir)
  # Build DuckDB array literal: ['file1','file2',...]
  paste0(
    "[",
    paste(sprintf("'%s'", gsub("'", "''", normalizePath(fs, winslash = "/"))), collapse = ","),
    "]"
  )
}

recreate_db <- function(path) {
  if (file.exists(path)) file.remove(path)
  path
}

# ---------- Paths ----------
dir_demand <- "prep/demand_partitioned"
dir_supply <- "prep/supply_partitioned"
out_dir    <- "data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==========================
# DEMAND
# ==========================
message("\n=== Building demand.duckdb ===")

demand_db <- recreate_db(file.path(out_dir, "demand.duckdb"))
con_d <- dbConnect(duckdb(), dbdir = demand_db)

files_demand <- list_parquet(dir_demand)

message("Creating demand table from Parquet…")
dbExecute(con_d, sprintf(
  "CREATE TABLE demand AS\n   SELECT * FROM read_parquet(%s, filename=true);\n",
  files_demand
))

message("Ordering demand by cz_label, year (improves compression/pruning)…")
dbExecute(con_d, "\n  CREATE TABLE demand_sorted AS\n  SELECT * FROM demand ORDER BY cz_label, year;\n  DROP TABLE demand;\n  ALTER TABLE demand_sorted RENAME TO demand;\n")

message("Creating index on (cz_label, year)…")
dbExecute(con_d, "CREATE INDEX idx_demand_cz_year ON demand(cz_label, year);")

message("Running ANALYZE and optimizer…")
# DuckDB expects one statement per call
try(dbExecute(con_d, "ANALYZE;"), silent = TRUE)
try(dbExecute(con_d, "PRAGMA optimize;"), silent = TRUE)

message("Demand summary:")
print(dbGetQuery(con_d, "SELECT COUNT(*) AS n_rows, MIN(year) AS min_year, MAX(year) AS max_year FROM demand"))

message("Demand indexes:")
print(dbGetQuery(con_d, "SELECT * FROM duckdb_indexes() WHERE table_name = 'demand'"))

dbDisconnect(con_d, shutdown = TRUE)

# ==========================
# SUPPLY
# ==========================
message("\n=== Building supply.duckdb ===")

supply_db <- recreate_db(file.path(out_dir, "supply.duckdb"))
con_s <- dbConnect(duckdb(), dbdir = supply_db)

files_supply <- list_parquet(dir_supply)

message("Creating supply table from Parquet…")
dbExecute(con_s, sprintf(
  "CREATE TABLE supply AS\n   SELECT * FROM read_parquet(%s, filename=true);\n",
  files_supply
))

message("Ordering supply by instnm, year…")
dbExecute(con_s, "\n  CREATE TABLE supply_sorted AS\n  SELECT * FROM supply ORDER BY instnm, year;\n  DROP TABLE supply;\n  ALTER TABLE supply_sorted RENAME TO supply;\n")

message("Creating index on (instnm, year)…")
dbExecute(con_s, "CREATE INDEX idx_supply_inst_year ON supply(instnm, year);")

message("Running ANALYZE and optimizer…")
try(dbExecute(con_s, "ANALYZE;"), silent = TRUE)
try(dbExecute(con_s, "PRAGMA optimize;"), silent = TRUE)

message("Supply summary:")
print(dbGetQuery(con_s, "SELECT COUNT(*) AS n_rows, MIN(year) AS min_year, MAX(year) AS max_year FROM supply"))

message("Supply indexes:")
print(dbGetQuery(con_s, "SELECT * FROM duckdb_indexes() WHERE table_name = 'supply'"))

dbDisconnect(con_s, shutdown = TRUE)

message("\nAll done. DBs written to 'data/demand.duckdb' and 'data/supply.duckdb'.")

library(dplyr)

# Open both DBs read-only
con_demand <- dbConnect(duckdb(), dbdir = "data/demand.duckdb", read_only = TRUE)
con_supply <- dbConnect(duckdb(), dbdir = "data/supply.duckdb", read_only = TRUE)

# Lazy dplyr table handles
Demand <- tbl(con_demand, "demand")
Supply <- tbl(con_supply, "supply")

# ----------------------------
# Sample queries
# ----------------------------

# 1. Demand for a given CZ + year
demand_knox_2024 <- Demand %>%
  filter(cz_label == "Knoxville, TN", year == 2024) %>%
  collect()

print(demand_knox_2024)

# Disconnect
dbDisconnect(con_demand, shutdown = TRUE)
dbDisconnect(con_supply, shutdown = TRUE)
