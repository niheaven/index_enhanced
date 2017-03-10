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
#   read_factors: Read Factors from DB

# Define features to be used
features <- c("EP_LYR", 
              "EP_TTM", 
              "SP_TTM", 
              "CashFlowYield_LYR", 
              "CashFlowYield_TTM", 
              "FreeCashFlowYield_TTM", 
              "BP_LR", 
              "Sales2EV", 
              "SaleEarnings_SQ_YoY", 
              "Earnings_SQ_YoY", 
              "Sales_SQ_YoY", 
              "Earnings_LTG", 
              "Sales_LTG", 
              "Earnings_STG", 
              "Sales_STG", 
              "Asset_STG", 
              "ROE_LR", 
              "ROA_LR", 
              "LTD2Equity_LR", 
              "AssetTurnover", 
              "CurrentRatio", 
              "Momentum_1M", 
              "Momentum_12M_1M", 
              "Momentum_60M", 
              "LnFloatCap", 
              "TSKEW", 
              "MACrossover", 
              "RealizedVolatility_1Y") %>%
			c("Date", "Symbol", .)

# Get index components from DB
get_index_comps <- function(db_data, index_symbol, end_date) {
	getIndexComp(symbol = index_symbol, 
				 channel = db_data$con, 
				 date = end_date) %>% 
		fixCode() %>%
		"names<-"(NULL)
}

# Cut performance to 2 levels and label the level
cut_perf_binary <- function(perf) {
	perf %>%
		ntile(n = 2) %>% 
		factor(labels = c("Poor", "Excellent"))
}

# Cut performance to 3 levels and label the level
cut_perf_triple <- function(perf) {
	perf %>%
		ntile(n = 3) %>% 
		factor(labels = c("Poor", "Neutral", "Excellent"))
}

# Load factors from DB and add a Performance column
load_factors <- function(tbl_factors, 
						 stock_symbols, 
                         selected_factors, 
                         end_date) {
	start_date <- last.day(end_date %m-% years(1))
	factors_data <- tbl_factors %>% 
		filter(Date >= start_date & 
			   Date <= end_date &
			   Symbol %in% stock_symbols & 
			   !is.na(Momentum_12M)) %>%
		select(one_of(selected_factors)) %>% 
		collect() %>% 
		group_by(Symbol) %>% 
		mutate(Perf = lead(Momentum_1M, order_by = Date))
	factors_data_train <- factors_data %>%
		filter(Date < end_date) %>%
		group_by(Date) %>%
		mutate(Perf_Level = cut_perf_binary(Perf)) %>%
		ungroup()
	factors_data_predict <- factors_data %>%
		filter(Date == end_date) %>% 
		ungroup()
	list(train = factors_data_train, 
	     predict = factors_data_predict)
}

