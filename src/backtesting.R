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
#   backtesting: Backtest ML Prediction

# Calculate equal weighted performance
calc_perf_ew <- function(tbl_factors, stock_symbols, end_date) {
	tbl_factors %>% 
		filter(Date == end_date & 
			   	Symbol %in% stock_symbols) %>% 
		summarise(mean(Momentum_1M)) %>%
		collect() %>%
		as.numeric()
}

# Get monthly seq
seq_monthly <- function(start_date, end_date) {
	as.yearmon(end_date) %>% 
		"-"(as.yearmon(start_date)) %>%
		prod(12) %>% 
		":"(0, .) %>% 
		months() %>%
		"%m+%"(start_date, .) %>%
		last.day() %>% 
		"names<-"(., .)
}


# Backtest for single period
backtesting0 <- function(index_symbol, end_date) {
	message(paste("Backtesting date:", end_date), appendLF = FALSE)
	end_date_1 <- last.day(end_date - 31)
	factors <- get_index_comps(db_data, index_symbol, end_date_1) %>%
		load_factors(tbl_factors, ., features, end_date_1)
	res <- train_n_predict(factors, n = 30) %>% 
		factors$predict$Symbol[.] %>% 
		list(Perf = calc_perf_ew(tbl_factors, ., end_date), 
			 Symbol = .)
	message(" Done!")
	res
}

# Backtest for several period
backtesting <- function(index_symbol, start_date, end_date) {
	if (missing(start_date))
		start_date <- end_date
	seq_monthly(start_date, end_date) %>% 
		map(backtesting0, index_symbol = index_symbol) %>%
		transpose() %>%
		map(bind_cols)
}

# Calculate index monthly return
get_index_mreturn <- function(index_symbol, start_date, end_date) {
	end_Ymd <- format(end_date, "%Y%m%d")
	start_Ymd <- format(start_date - 50, "%Y%m%d")
	left_join(tbl(db_data, "TQ_QT_INDEX"), 
			  tbl(db_data, "TQ_OA_STCODE"), 
			  by = c("SECODE", "EXCHANGE")) %>% 
		filter(SYMBOL == index_symbol, 
			   between(TRADEDATE, start_Ymd, end_Ymd)) %>% 
		select(TRADEDATE, TCLOSE) %>% 
		collect() %>% 
		mutate(TRADEDATE = ymd(TRADEDATE)) %>%
		tq_transmute_xy(x = TCLOSE, 
						mutate_fun = monthlyReturn, 
						type = "log") %>%
		slice(-1) %>%
		mutate(TRADEDATE = last.day(TRADEDATE))
}

# Collect backtesting performance and index performance
make_perf_data <- function(index_symbol, start_date, end_date) {
	bt_perf_n_symbol <- backtesting(index_symbol, start_date, end_date)
	bt_perf <- t(bt_perf_n_symbol$Perf) %>% 
		as_tibble(preserve_row_names = TRUE) %>%
		transmute(Date = ymd(row.names), IndexEnhanced = V1)
	idx_perf <- get_index_mreturn(index_symbol, start_date, end_date) %>% 
		rename(Date = TRADEDATE, Index = monthly.returns)
	list(Data = full_join(idx_perf, bt_perf, by = "Date"), 
		 Stock.Lists = bt_perf_n_symbol$Symbol)
}

# Plot index and porfolio P/L
bt_plot <- function(perf_data) {
	last.day(first(index(perf_data$Data)) - 31) %>% 
		xts(t(c(1, 1)), .) %>% 
		rbind(cumprod(1 + perf_data$Data)) %>% 
		"colnames<-"(c("Index", "Index Enhanced")) %>%
		plot()
}
