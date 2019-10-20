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
                     history_performance <- subset(self$book, (id1 == self$id | id2 == self$id) & 
                                                     (id1 == self$opponent_id | id2 == self$opponent_id) )
                     history_performance_sample <- head(history_performance,4)
                     number_of_meet = dim(history_performance_sample)[1]
                     last_round <- tail(history_performance,1)  
                      
                     # number of columns in last_round
                     if ((nrow(last_round)) == 0 )  {                                            # number of columns in last_round
                     } else if (last_round[1,1] == self$id) {                              
                       last_move_opponent <- last_round[1,5]                               
                     } else if (last_round[1,1] == self$opponent_id) {                       
                       last_move_self <- last_round[1,5]                                    
                       last_move_opponent <- last_round[1,4]                                
                     }
                     
                     history_performance_opponent <- c()
                     if (number_of_meet > 3 ){
                       for (i in c(1,2,3)){
                         if ( history_performance[i:1] == self$id) {                               
                           history_performance_opponent = c( history_performance_opponent,history_performance[i,5])
                         } else if ( history_performance[i:1] == self$opponent_id) {                       
                           history_performance_opponent = c( history_performance_opponent,history_performance[i,4])
                         }
                       }
                     }
                        
                     count_cooperation = 0
                     for (w in history_performance_opponent){
                       if (w =='cooperation') {count_cooperation = count_cooperation + 1 }
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
