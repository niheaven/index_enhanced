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
#   ml_train_n_predict: Train ML Task and Predict

# Main function for train and predict of certain period
train_n_predict <- function(factors_data, n = 30) {
	mfss.task <- factors_data$train %>%
		select(-Date, -Symbol, -Perf) %>%
		data.frame %>%
		makeClassifTask(id = "MF Stock Selection", 
						data = ., 
						target = "Perf_Level", 
						positive = "Excellent")
	
	mfss.lrn.parset <- list(boos = FALSE, 
							mfinal = 20, 
							maxdepth = 3)
	
	mfss.lrn <- makeLearner("classif.boosting", 
							par.vals = mfss.lrn.parset, 
							predict.type = "prob")
	
	mfss.mod <- train(learner = mfss.lrn, 
					  task = mfss.task)
	
	mfss.pred <- factors_data$predict %>%
		select(-Date, -Symbol, -Perf) %>%
		data.frame() %>%
		predict(mfss.mod, newdata = .)
	
	order(mfss.pred$data$prob.Excellent, decreasing = TRUE) %>%
		head(n = n)
}

# mfss.train.pred <- predict(mfss.mod, task = mfss.task)
