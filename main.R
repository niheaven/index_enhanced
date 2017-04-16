#
#   index_enhanced: Using ML to Enhance Stock Index Investment
#
#   Copyright (C) 2016-2017  Hsiao-nan Cheung zxn@hffunds.cn
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#   main: Main Source File

pacman::p_load(tidyquant, 
               mlr)

# Table Name for main.*
# TABLE.PERIOD <- "FACTORS_PERIOD"
TABLE.STOCK <- "FACTORS_STOCK"

source("src/.SupFun.R", chdir = TRUE, encoding = 'UTF-8')
source("src/read_factors.R", encoding = "UTF-8")
source("src/ml_train_n_predict.R", encoding = "UTF-8")
source("src/backtesting.R", encoding = "UTF-8")

# For DB Connections
if (!tryCatch(DBI::dbIsValid(db_data$con), error = function(e) FALSE))
# db_data <- RSQLServer::src_sqlserver("DBSERVER", 
#                                      file = "dbi/sql.yaml", 
#                                      database = "CAIHUI")
  db_data <- src_mysql(dbname = "CAIHUI", 
                       user = NULL, 
                       password = NULL, 
                       default.file = "dbi/my.cnf")
if (!tryCatch(DBI::dbIsValid(db_factors$con), error = function(e) FALSE))
	db_factors <- src_mysql(dbname = "factors", 
	                        user = NULL, 
	                        password = NULL, 
	                        default.file = "dbi/my.cnf")
tbl_factors <- tbl(db_factors, TABLE.STOCK)

source("test/test.R", encoding = "UTF-8")

#perf_data_300 <- make_perf_data(index_symbol = "000300", start_date, end_date)
#perf_data_500 <- make_perf_data(index_symbol = "399905", start_date, end_date)
