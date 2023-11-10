## © 2023 Samuel Tobler

# This function allows to run the evaluation function in a loop over all participants. 

eval_combined <- function(
    question, 
    solution, # example solution
    data, # student answer data
    points, 
    instruction, # on how to correct the exercise
    APIkey,
    temperature = 0.7, 
    model = "gpt-4", 
    name
) {
  
  
  score <- vector()
  for(i in 1:length(data)) {
    score[i] <- eval_single(question = question, solution = solution, answer = data[i], points = points, 
                            instruction = instruction, APIkey = APIkey, temperature = temperature, 
                            model = model)
    cat(round(i/length(data)*100, 2),"%", sep = "")
    cat("\n")
  };
  
  score.x <- as.numeric(score); #score.x
  
  df <- data.frame(Result = score, 
                   Score = score.x)
  
  if (!dir.exists("Results")) {dir.create("Results")}
  write.csv(x = df, file = paste("Results/", name, ".csv", sep = ""))
  
}
