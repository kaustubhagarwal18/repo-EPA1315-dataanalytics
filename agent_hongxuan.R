library(R6)
Agent <- R6Class("Agent",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   greeting = NULL,
                   id = NULL,
                   opponent_id = NULL,
                   opponent_greeting = NULL,
                   round = NULL, 
                   response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$response <- response
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   set_greeting = function() {
                     greeting <- "Eureka!"
                   },
                   
                   receive_greeting = function(greeting = NA) {
                     self$opponent_greeting = greeting
                   },
                   
                   get_bid = function() {
                     bid_vector <- c("cooperate","defect")
                     last_move_self <- NA
                     last_move_opponent <- NA
                     history_performance_opponent <- NA
                     count_cooperation <-NA
                     last_round <- tail(subset(self$book, (id1 == self$id | id2 == self$id) & 
                                                 (id1 == self$opponent_id | id2 == self$opponent_id)), 1)  # get details of lasat round!
                     history_performance <- subset(self$book,(id1 == self$id | id2 == self$id) & 
                                                  (id1 == self$opponent_id | id2 == self$opponent_id) &  (tradeno < 200))
                     
                     if ((nrow(last_round)) == 0 )  {                                            # number of columns in last_round
                     } else if (last_round[1,1] == self$id) {                               # If the first column is your id get your last move
                       last_move_self <- last_round[1,4]
                       last_move_opponent <- last_round[1,5]                                # get the move of your opponent
                     } else if (last_round[1,1] == self$opponent_id) {                      # if the first column is of opponent  
                       last_move_self <- last_round[1,5]                                    # your move will be in 5th column
                       last_move_opponent <- last_round[1,4]                                # your move will be in 4th column
                     }
                     
                     history_performance_opponent <-c()
                     j = 1
                     while ( j  < dim(history_performance)[1]){
                       if (history_performance[j:1] == self$id) {                               
                         history_performance_opponent = c( history_performance_opponent,history_performance[j,5])
                       } else if (history_performance[j:1] == self$opponent_id) {                       
                        history_performance_opponent = c( history_performance_opponent,history_performance[j,4])
                       }
                       j = j + 1
                      }
                     
                     count = 0
                     for (w in history_performance_opponent){
                       if (w =='cooperation') {count = count + 1 }
                     }
                     count_cooperation <- count
                 
                   if(self$round < 200){
                      if ((nrow(last_round)) == 0) {                                             # if the first round play cooperate!
                        self$bid <- bid_vector[1]                                                    
                      } else if ((last_move_self == bid_vector[1]) & (last_move_opponent == bid_vector[1])) { # cooperate is opponent cooperated last time
                        self$bid <- bid_vector[1]
                      } else if ((last_move_self == bid_vector[1]) & (last_move_opponent == bid_vector[2])) { # Tit for tat
                        self$bid <- bid_vector[2]
                      } else if ((last_move_self == bid_vector[2]) & (last_move_opponent == bid_vector[1])) { # cooperate
                        self$bid <- bid_vector[1]
                      } else if ((last_move_self == bid_vector[2]) & (last_move_opponent == bid_vector[2])) { # tit for tat
                        self$bid <- bid_vector[2]
                      }
                    }
                    else{
                      if (count >= 2){
                        self$bid <- bid_vector[1] 
                      }
                      else{
                        self$bid <- bid_vector[2] 
                      }
                    }
                      
                   },
                   
                   formulate_bid = function() {
                     self$get_bid()
                   }
                 )
)
