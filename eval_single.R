## © 2023 Samuel Tobler

# This function allows to run the evaluation function for an individual answer. 
# Use the eval_combined function to run it over a whole data set. 

eval_single <- function(
    question, 
    solution, # example solution
    answer, # student answer
    points, 
    instruction, # on how to correct the exercise
    APIkey,
    temperature, 
    model
) {
  
  require(httr)
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", APIkey)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      temperature = temperature,
      messages = list(
        list(
          "role" = "system",
          "content" = paste("You have to give points to a user-generated answer based on the following information: 
                            This is the question the user tried to answer:", question, 
                            "This is the correct answer: ", solution, 
                            "The exercise gives", points, "Points, and 
                            this is your instruction how to give points:", instruction, 
                            "Only return the number of points. If there is no answer, return 0 points. "
                            )
        ),
        list(role = "user", content = answer)
      )
    )
  )
  
  if (status_code(response) > 200) {
    stop(content(response))
  }
  
  result <- httr::content(response)$choices[[1]]$message$content
  return(trimws(result))
}
