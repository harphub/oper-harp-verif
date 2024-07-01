#' ranking of non-FSS scores
#'
#' @param  verif_data output from verify_spatial
#' @param  score as character for which to do the ranking
#' @return verif_data with additional ranking entry
score_ranking <- function(verif_data, score) {
	message("ranking score: ", score)
        
        tmp <- lapply(lapply(verif_data, '[[', score), '[[', score)

	if (length(unlist(tmp)) != length(names(tmp))){
	   null_counts <- length(names(tmp)) - length(unlist(tmp))
	   print(tmp)
	   warning("ERROR: ", null_counts, " of the models contain(s) NULL for score ", score)
	}

        if (score == "bias"){
                tmp = abs(as.numeric(tmp))   # TODO: error message if NULL in entry ; bias:  lowest absolut is best
                ranking <- rank_min(tmp)
        } else if (score == "Rpearson") {
                tmp = as.numeric(tmp)
                ranking <- rank_max(tmp)     # Rperson correlation max is best
        } else {
                tmp = as.numeric(tmp)        # mae, rmse lowest is best
                ranking <- rank_min(tmp)
        }
        # ranking number 0: score == NA
        ranking <- replace(ranking, which(is.na(tmp)), 0)

        for (i in 1:length(verif_data)){
                verif_data[[i]][[score]][["ranking"]] <- ranking[[i]]
        }
        return(verif_data)
}


#' ranking of FSS scores
#'
#' @param  verif_data output from verify_spatial
#' @param  score as character for which to do the ranking
#' @return verif_data with additional ranking entry
fss_ranking <- function(verif_data, score){
	message("ranking score: ", score)
        fss_fobs <- 0.5  # TODO: replace fss_fobs <- 0.5 by actual fobs
        tmp <- lapply(lapply(verif_data, '[[', score), '[[', 'fss')

        if (length(unlist(tmp)) != length(names(tmp))){
           null_counts <- length(names(tmp)) - length(unlist(tmp))
           print(tmp)
           warning("ERROR: ", null_counts, " of the models contain(s) NULL for score ", score)
        }

        command <- "mapply(rank_min"
        for (mod in names(verif_data)) {command <- paste0(command, ", tmp$", mod)}
        command <- paste0(command, ")")
        ranking <- eval(parse(text=command))
        ranking <- ranking + 2  # 'normal' score ranking only starts at 3

        for (i in 1:length(verif_data)){
                # ranking number 0: score == NA
                ranking[i,] <- replace(ranking[i,],
                                       which(is.na(as.numeric(unlist(tmp[i])))), 0)
                # ranking number 1: FSS < fss_fobs
                ranking[i,] <- replace(ranking[i,],
                                       which(as.numeric(unlist(tmp[i])) < fss_fobs), 1)
                # ranking number 2: perfect score
                ranking[i,] <- replace(ranking[i,],
                                       which(as.numeric(unlist(tmp[i])) == 1), 2)
                verif_data[[i]][[score]][["ranking"]] <- ranking[i,]
        }
        return(verif_data)
}


#' make average over the non-FSS ranks
#'
#' @param data_model scores of one model ranking accessible via data_model[[score]]$ranking
#' @param scores over which to average
#' @return averaged rank
average_rank_nofss <- function(data_model, scores=c("bias", "mae", "rmse", "Rpearson")){
        average_rank <- 0
        for (s in 1:length(scores)){
                average_rank <- average_rank + data_model[[scores[s]]]$ranking
        }
        average_rank <- average_rank / s
        return(average_rank)
}


#' get sum of inverted fss ranks 
#'
#' @param data_model scores of one model ranking accessible via data_model[[score]]$ranking
#' @param scores over which to average
#' @return sum inverted fss ranks
sum_inverted_fss_ranks <- function(data_model){
        tmp_rr <- data_model[["FSS"]]$ranking
        tmp_rr <- replace(tmp_rr, which(tmp_rr==2), 3)
        tmp_rr <- replace(tmp_rr, which(tmp_rr==1), 0)
        tmp_rr <- tmp_rr %>% ifelse(. == 0, ., 1/.)

        return(sum(tmp_rr))
}



#' main ranking function which calls all the subfuncitons
#' @param verif_data list of tibble containing the fc and obdata
#' @return verif_data with additional ranking information
main_ranking <- function(verif_data){
   # ## score ranking

   scores_nofss <- scores[ !scores %in% c("FSS", "FSSp")]
   for (score in scores_nofss){
           verif_data <- score_ranking(verif_data, score)
   }
   message("Finished score_ranking of nofss scores.")
   
   scores_fss <- c("FSS", "FSSp")
   for (score in scores_fss){
           verif_data <- fss_ranking(verif_data, score)
   }
   message("Finished score_ranking of fss scores.")
   
   # ## average ranking
   for (model in names(verif_data)){
   
      verif_data[[model]][['avg_rank']] <-
          average_rank_nofss(verif_data[[model]],
                                 scores_nofss)
      verif_data[[model]][['fss_sum_rank']] <-
          sum_inverted_fss_ranks(verif_data[[model]])
   }
   message("Finished averaging of ranks.")
   
   tmp <- as.numeric(lapply(verif_data, '[[', "avg_rank"))
   ranks_avg_rank <- rank_min(tmp)  # with 1 being the best rank
   
   tmp <- as.numeric(lapply(verif_data, '[[', "fss_sum_rank"))
   ranks_avg_fss_rank <- rank_max(tmp) # with 1 being the best rank
   
   for (i in 1:length(verif_data)){
           verif_data[[i]][["rank_avg_rank"]] <- ranks_avg_rank[i]
           verif_data[[i]][["rank_avg_fss_rank"]] <- ranks_avg_fss_rank[i]
   }
   message("All rankings done.")
   return(verif_data)
}

## ranks
# 0 - NaN

# 2 - fss below 0.5 + 0.5f
# 2 - perfect score
# 3 - gold
# 4 - silver
# 5 - bronze
# 6+- white
# equal values get the same rank
# if N values are equal, the next N-1 tanks are skipped
# ranks 3 and higher are valid ranks


