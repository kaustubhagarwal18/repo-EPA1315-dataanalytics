Sys.setlocale('LC_CTYPE','English_United Kingdom.1252')
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
                     first_move_opponent <- NA
                     last_move_self <- NA
                     last_move_opponent <- NA
                      first_round <- head(subset(self$book, (id1 == self$opponent_id | id2 == self$opponent_id) ), 1)
                     last_round <- tail(subset(self$book, (id1 == self$id | id2 == self$id) & 
                                                 (id1 == self$opponent_id | id2 == self$opponent_id)), 1)
                     
                     #Locate  the  opponent  in its first round
                     if ((nrow(first_round)) == 0 )  {                          
                     } else if (first_round[1,1] == self$opponent_id) {                               
                       first_move_opponent <- first_round[1,4]                                
                     } else if (first_round[1,2] == self$opponent_id) {                        
                       first_move_opponent <- first_round[1,5]
                     }
                     
                     
                     if ((nrow(last_round)) == 0 )  {                                            
                     } else if (last_round[1,1] == self$id) {                             
                       last_move_self <- last_round[1,4]
                       last_move_opponent <- last_round[1,5]                                
                     } else if (last_round[1,1] == self$opponent_id) {                       
                       last_move_self <- last_round[1,5]                                    
                       last_move_opponent <- last_round[1,4]                                
                     }
                     
                     
                     if ((nrow(last_round)) == 0 & (nrow(first_round) == 0)) {      
                       self$bid <- bid_vector[1]                                                    
                     }else if ((first_move_opponent == bid_vector[1]) & (nrow(last_round) == 0)){ 
                       self$bid <- bid_vector[1]
                     }else if ((first_move_opponent == bid_vector[2]) & (nrow(last_round) == 0)){
                       self$bid <- bid_vector[2]
                     }else if ( last_move_opponent == bid_vector[2]){
                       self$bid <- bid_vector[2]
                     }else if (last_move_opponent == bid_vector[1]){
                       self$bid <- bid_vector[1]
                     }
                    },

                      formulate_bid = function() {
                      self$get_bid()
                     }
                 )
)
