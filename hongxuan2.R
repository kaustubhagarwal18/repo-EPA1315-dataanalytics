Sys.setlocale("LC_CTYPE",locale="English_United Kingdom.1252")
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
                     count_defect <-NA
                     history_performance <- subset(self$book, (id1 == self$id | id2 == self$id) & 
                                                     (id1 == self$opponent_id | id2 == self$opponent_id) )
                     last_round <- tail(history_performance,1)  
                     number_of_meet <- dim(history_performance)[1]
                     
                     if ((nrow(last_round)) == 0 )  {                                            # number of columns in last_round
                     } else if (last_round[1,1] == self$id) {                               # If the first column is your id get your last move
                       last_move_self <- last_round[1,4]
                       last_move_opponent <- last_round[1,5]                                # get the move of your opponent
                     } else if (last_round[1,1] == self$opponent_id) {                      # if the first column is of opponent  
                       last_move_self <- last_round[1,5]                                    # your move will be in 5th column
                       last_move_opponent <- last_round[1,4]                                # your move will be in 4th column
                     }
                     
                     history_performance_opponent <-c()
                     j = 0
                     while ( j  < number_of_meet){
                       if ( history_performance[j:1] == self$id) {                               
                         history_performance_opponent = c( history_performance_opponent,history_performance[1,5])
                       } else if ( history_performance[j:1] == self$opponent_id) {                       
                         history_performance_opponent = c( history_performance_opponent,history_performance[1,4])
                       }
                       j = j + 1
                     }
                     
                     count_cooperation = 0
                     for (w in history_performance_opponent){
                       if (w =='cooperation') {count_cooperation = count_cooperation + 1 }
                     }
                     
                     count_defect = 0
                     for (w in history_performance_opponent){
                       if (w =='cooperation') {count_defect = count_defect  + 1 }
                     }
                     
                     if( number_of_meet < 3 ){
                       self$bid <- bid_vector[2]
                     }else if((number_of_meet = 3) & ( count_cooperation >= 1)) {
                       self$bid <- bid_vector[1]
                     }else if ((number_of_meet = 3) & ( count_cooperation < 1)) {
                       self$bid <- bid_vector[2]
                     }else if ((number_of_meet = 4) & (last_move_self == bid_vector[1])){
                       self$bid <- bid_vector[1]
                     }else if ((number_of_meet = 4) & (last_move_self == bid_vector[2])){
                       self$bid <- bid_vector[2]
                     }else if ((number_of_meet > 4) & (last_move_opponent == bid_vector[2])){
                       self$bid <- bid_vector[2]
                     }else if ((number_of_meet > 4) & (last_move_opponent == bid_vector[1])){
                       self$bid <- bid_vector[1]
                     }
                     
                   },
                   
                   formulate_bid = function() {
                     self$get_bid()
                   }
                 )
)
