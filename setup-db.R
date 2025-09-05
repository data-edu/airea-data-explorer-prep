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
  "CREATE TABLE demand AS
   SELECT * FROM read_parquet(%s, filename=true, union_by_name=true);",
  files_demand
))

message("Ordering demand by cz_label, year (improves compression/pruning)…")
dbExecute(con_d, "
  CREATE TABLE demand_sorted AS
  SELECT * FROM demand ORDER BY cz_label, year;
  DROP TABLE demand;
  ALTER TABLE demand_sorted RENAME TO demand;
")

message("Creating index on (cz_label, year)…")
dbExecute(con_d, "CREATE INDEX idx_demand_cz_year ON demand(cz_label, year);")

message("Running ANALYZE and optimizer…")
try(dbExecute(con_d, "ANALYZE;"), silent = TRUE)
try(dbExecute(con_d, "PRAGMA optimize;"), silent = TRUE)

message("Demand summary:")
print(dbGetQuery(con_d, "SELECT COUNT(*) AS n_rows, MIN(year) AS min_year, MAX(year) AS max_year FROM demand"))

message("Demand indexes:")
print(dbGetQuery(con_d, "SELECT * FROM duckdb_indexes() WHERE table_name = 'demand'"))

dbDisconnect(con_d, shutdown = TRUE)

# ==========================
# SUPPLY  (per-file ingest with casts; avoids global schema inference)
# ==========================
message("\n=== Building supply.duckdb ===")

supply_db <- recreate_db(file.path(out_dir, "supply.duckdb"))
con_s <- dbConnect(duckdb(), dbdir = supply_db)

# Create target table with fixed schema
dbExecute(con_s, "
  CREATE TABLE supply (
    instnm                      VARCHAR,
    unitid                      BIGINT,
    aire_ind                    SMALLINT,
    year                        INTEGER,
    state                       VARCHAR,
    tribal                      SMALLINT,
    rural                       SMALLINT,
    cip                         INTEGER,
    ciptitle                    VARCHAR,
    award_level                 SMALLINT,
    total_completions           INTEGER,
    airea_completions           INTEGER,
    total_students_enrolled     INTEGER,
    total_ft_students_enrolled  INTEGER,
    webaddr                     VARCHAR,
    latitude                    DOUBLE,
    longitud                    DOUBLE,
    cz_label                    VARCHAR,
    src_file                    VARCHAR
  );
")

# Gather files (each loads fine alone; we avoid cross-file unification)
files <- list.files(dir_supply, pattern="\\.parquet$", recursive=TRUE, full.names=TRUE)
stopifnot(length(files) > 0)

esc <- function(x) gsub("'", "''", normalizePath(x, winslash = "/"))

message(sprintf("Ingesting %d supply files (per-file casts)…", length(files)))
fail <- list()
n <- 0L

for (f in files) {
  n <- n + 1L
  if (n %% 200 == 0) message(sprintf("… %d/%d files", n, length(files)))
  sql <- sprintf("
    INSERT INTO supply
    SELECT
      CAST(instnm AS VARCHAR)                                         AS instnm,
      TRY_CAST(unitid AS BIGINT)                                      AS unitid,
      TRY_CAST(aire_ind AS SMALLINT)                                  AS aire_ind,
      TRY_CAST(year AS INTEGER)                                       AS year,
      CAST(state AS VARCHAR)                                          AS state,
      TRY_CAST(tribal AS SMALLINT)                                    AS tribal,
      TRY_CAST(rural AS SMALLINT)                                     AS rural,
      TRY_CAST(cip AS INTEGER)                                        AS cip,
      CAST(ciptitle AS VARCHAR)                                       AS ciptitle,
      TRY_CAST(award_level AS SMALLINT)                               AS award_level,
      TRY_CAST(total_completions AS INTEGER)                          AS total_completions,
      TRY_CAST(airea_completions AS INTEGER)                          AS airea_completions,
      TRY_CAST(total_students_enrolled AS INTEGER)                    AS total_students_enrolled,
      TRY_CAST(total_ft_students_enrolled AS INTEGER)                 AS total_ft_students_enrolled,
      CAST(webaddr AS VARCHAR)                                        AS webaddr,
      TRY_CAST(latitude AS DOUBLE)                                    AS latitude,
      TRY_CAST(longitud AS DOUBLE)                                    AS longitud,
      CAST(cz_label AS VARCHAR)                                       AS cz_label,
      filename                                                        AS src_file
    FROM read_parquet('%s',
          filename=true,
          hive_partitioning=1,
          union_by_name=true,
          binary_as_string=true
    );
  ", esc(f))
  tryCatch(
    dbExecute(con_s, sql),
    error = function(e) fail[[length(fail)+1]] <<- list(file=f, err=conditionMessage(e))
  )
}

if (length(fail)) {
  message(sprintf("WARNING: %d files failed to ingest. Showing first few:", length(fail)))
  print(head(do.call(rbind, lapply(fail, as.data.frame)), 10))
}

message("Ordering supply by instnm, year…")
dbExecute(con_s, "
  CREATE TABLE supply_sorted AS
  SELECT * FROM supply ORDER BY instnm, year;
  DROP TABLE supply;
  ALTER TABLE supply_sorted RENAME TO supply;
")

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

# ----------------------------
# Sample query usage (read-only)
# ----------------------------
library(dplyr)

con_demand <- dbConnect(duckdb(), dbdir = "data/demand.duckdb", read_only = TRUE)
con_supply <- dbConnect(duckdb(), dbdir = "data/supply.duckdb", read_only = TRUE)

Demand <- tbl(con_demand, "demand")
Supply <- tbl(con_supply, "supply")

# Example: Demand for a given CZ + year
demand_knox_2024 <- Demand %>%
  filter(cz_label == "Knoxville, TN", year == 2024) %>%
  collect()

print(demand_knox_2024)

dbDisconnect(con_demand, shutdown = TRUE)
dbDisconnect(con_supply, shutdown = TRUE)
