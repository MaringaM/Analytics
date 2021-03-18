
#connect to a database connection

library(odbc)
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "localhost",
                      Database = "PortalDev", UID = "sa", PWD = "c0nstella",
                      Port = 1433)
