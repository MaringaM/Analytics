library(RMariaDB)

dbConfig <- config::get("database")
conn <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = dbConfig$dbname,
  host = dbConfig$host,
  port = dbConfig$port,
  username = dbConfig$username,
  password = dbConfig$password
)

dbExecute(conn,
          "CREATE TABLE IF NOT EXISTS ArimaHTS (
            county varchar(255),
            Date datetime,
            Point_Forecast double,
            Lo_80 double,
            Hi_80 double,
            Lo_95 double,
            Hi_95 double,
            num_pos integer,
            num_tests integer
    )"
)

dhf <-readRDS('C:/Users/Admin/OneDrive/Analytics/ARIMA Models/Arima HTS from DWH/arimahtsdwh.rds')

dbWriteTable(conn, "ArimaHTS", df, append = TRUE)
dbDisconnect(conn)

