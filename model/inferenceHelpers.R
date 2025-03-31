# from Brandon https://github.com/bwaldon/crossling_reference/blob/master/_shared/inferenceHelpers.r

# create the basic RSA model (speaker and listener functions)
# for the wonky common ground model, all production models, and the LL model directly use the file and there is no literal listener
makeModel <- function(header, modelName) {
  if (modelName == "threshold_cg") {
    # return(paste(read_file(header), read_file("./literalListener_cg.txt"), sep = "\n"))
    return(read_file(header))
  } else if (str_detect(modelName, "production")) {
    return(read_file(header))
  } else if (str_detect(modelName, "LL")) {
    return(read_file(header))
  } else if (modelName == "threshold_qud") {
    return(paste(read_file(header), read_file("./literalListener_qud.txt"), sep = "\n"))
  } else {
    return(paste(read_file(header), read_file("./literalListener.txt"), sep = "\n"))
  }
}

# load different models (different posteriors)
wrapInference <- function(model, modelName, samples, lag, burn) {
  
  if (modelName == "threshold_chemo") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_chemo_params.txt")
  } else if (modelName == "threshold_qud") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_qud_params.txt")
  }  else if (modelName == "threshold_mix") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_mix_params.txt")
  } else if (modelName == "threshold_cg") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_cg_params.txt")
  } else if (modelName == "chemo_production") {
    inferenceCommand <- read_file("./inferenceCommands/chemo_production_params.txt")
  } else if (modelName == "simple_chemo_production") {
    inferenceCommand <- read_file("./inferenceCommands/simple_chemo_production_params.txt")
  } else if (modelName == "simple_chemo") {
    inferenceCommand <- read_file("./inferenceCommands/simple_chemo_params.txt")
  } else if (modelName == "threshold_mix_production") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_mix_production_params.txt")
  } else if (modelName == "threshold_LL") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_LL_params.txt")
  }

  inferenceCommand <- gsub("NUM_SAMPLES", samples, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("LAG", lag, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("BURN_IN", burn, inferenceCommand, fixed = TRUE)

  return(paste(read_file(model), inferenceCommand, sep = "\n"))
  
}

estimate_mode <- function(s) {
  d <- density(s)
  return(d$x[which.max(d$y)])
}

getEstimates <- function(posteriors) {
  
  estimates <- posteriors %>%
    group_by(Parameter) %>%
    summarize(estimate = estimate_mode(value)) %>% 
    # for chemo_production, production prob is always in [0,1]
    mutate(estimate = ifelse(estimate < 0, 0, estimate)) %>%
    pivot_wider(names_from = Parameter, values_from = estimate)
  
  return(estimates)
  
}

wrapPrediction = function(model, modelName, estimates) {
  if (modelName == "threshold_mix") {
    literalListener <- read_file("./literalListener.txt")
    predictionCommand <- read_file("./getPredictions.txt")
  } else if (modelName == "threshold_qud") {
    literalListener <- read_file("./literalListener_qud.txt")
    predictionCommand <- read_file("./getPredictions.txt")
  } else if (modelName == "threshold_cg") {
    literalListener <- read_file("./literalListener_cg.txt")
    predictionCommand <- read_file("./getPredictions_cg.txt")
  } else if (modelName %in% c("chemo_production", "simple_chemo")) {
    literalListener <- ""
    predictionCommand <- read_file("./getPredictions_chemo.txt")
  } else if (modelName == "chemo_speaker") {
    literalListener <- ""
    predictionCommand <- read_file("./getPredictions_speaker.txt")
  } else if (modelName == "simple_chemo_production") {
    literalListener <- ""
    predictionCommand <- read_file("./getPredictions_simple_chemo_speaker.txt")
  } else if (modelName == "threshold_mix_speaker") {
    literalListener <- ""
    predictionCommand <- read_file("./getPredictions_thresholdSpeaker.txt")
  } else if (modelName == "threshold_LL") {
    literalListener <- ""
    predictionCommand <- read_file("./getPredictions_thresholdLL.txt")
  } 
  
  predictionCommand <- paste((sprintf("var estimates = %s[0]", toJSON(estimates, digits = NA))), predictionCommand, sep = "\n")
  
  return(paste(read_file(model), literalListener , predictionCommand, sep = "\n"))
}