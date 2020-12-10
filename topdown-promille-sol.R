#### Promille Rechner 
##########################

#### Function ####

tell_me_how_drunk <- function(age, sex = c("male", "female"), 
                              height, weight, drinking_time, 
                              drinks = c("hoibe", "massn", "wein", "schnaps")
                              ) {
  ### Input Checks
  
  ## age 
  assert(check_numeric(age,
                       lower = 14,
                       upper = 100), ## Assertion for age not being an integer and a scalar.
         check_scalar(age),
         combine = "and")  
  
  ## sex 
  sex <- tolower(sex)
  sex <- match.arg(sex)
  assert(check_true(sex == "male"),   ## Assertion for sex being  
         check_true(sex == "female"), ### either male or female
         combine = "or")
  assert(check_scalar(sex)) ##  Assertion for sex being a scalar 
  
  ## height 
  
  assert(check_numeric(height,        ## Assertion for height being numeric,
                       lower = 50,    ### and not too low 
                       upper = 300),  ### or not too high, 
         check_scalar(height),        ### as well as scalar. 
         combine = "and") 
  
  ## weight 
  
  assert(check_numeric(weight,       ## Assertion for weight being numeric,
                       lower = 40,   ### and not too low 
                       upper = 300), ### or not too high, 
         check_scalar(weight),       ### as well as scalar. 
         combine = "and") 
  
  ## drinking_time
  
  assert(                          ## Assertion for drinking_time
    check_posixct(drinking_time,   ### being a POSIXct-vector
                  max.len = 2),    ### and of length 2
    check_true(drinking_time[2] > drinking_time[1]), 
    combine = "and"
    )    
           
  
  ## drinks 
  drinks <- unlist(drinks, use.names = TRUE) ## saving drinks as an atomic
  assert_numeric(drinks,       ## Assertion for drinks being
                  lower = 0,   ### at least zero
                  upper = 20)  ### and at most 20.
  assert_names(names(drinks),  ## Assertion for drinks having proper names.
                subset.of = c("hoibe", 
                              "massn", 
                              "wein", 
                              "schnaps")
               )
        

  ## legality

  if (age < 16) {
    warning("illegal")
  }
  if (age < 18 & "schnaps" %in% names(drinks)) {
    warning("illegal")
  }
  
  ### alcohol_consumption
  get_alcohol_consumption <- function(drinks) {
  drink_names <- names(drinks) ## save names of drinks in a vector 
  switch_vector <- Vectorize(vectorize.args = "drink_names", ## for every entry in drink_names...  
                             FUN = function(drink_names) {   ## ... give alcohol content per drink in grams.
                               switch(as.character(drink_names), # For each drink this is calculated by...
                                      "massn" = (0.8 * 1000 * 0.06), # the denisity of alcohol * the volume in mL * the alchol percentage
                                      "hoibe" = (0.8 * 500 * 0.06),
                                      "wein" = (0.8 * 200 * 0.11), 
                                      "schnaps" = (0.8 * 40 * 0.4) 
                                      ) 
                             }
  ) 
  alcohol_content <- unlist(switch_vector(drink_names)) ## save the result as a vector: alcohol_content
  alcohol_content %*% drinks # Vector multiplication to obtain alcohol consumption 
  }
  alcohol_consumption <- get_alcohol_consumption(drinks)
  
  ### body_water
  get_body_water <- function(sex, age, height, weight) {
    ifelse(sex == "male", ## Calculating total body water depending on whether individual is... 
      2.447 - (0.09516 * age) + (0.1074 * height) + (0.3362 * weight), ## ... male 
      0.203 - (0.07 * age) + (0.1069 * height) + (0.2466 * weight) ## ... or female. 
  ) 
  } 
  body_water <- get_body_water(sex, age, height, weight) 
  
  ### bac_initial
  get_bac_initial <- function(alcohol_consumption, body_water) { ## Calculating the initial blood alcohal content...
    (0.8 * alcohol_consumption) / (1.055 * body_water) ## using the the two terms calculated in the previous functions. 
  }
  bac_initial <- get_bac_initial(alcohol_consumption, body_water)
  
  ### time_elapsed
  get_time_elapsed <- function(drinking_time) {
    time_elapsed_text <- drinking_time[2] - drinking_time[1] ## Calculating a difftime object to get both units and values.
    switch(units(time_elapsed_text), ## converting values based on whether units are...
           "hours" = as.numeric(drinking_time[2] - drinking_time[1]), ## hours (i.e. unchanged), ...
           "mins" = as.numeric(drinking_time[2] - drinking_time[1])/60, ## minutes, ...
           "days" = as.numeric(drinking_time[2] - drinking_time[1])*24 ## days. 
                  )
  }
  time_elapsed <- get_time_elapsed(drinking_time) ## calculating the time elapsed as a numeric

  ### bac_final
  
  get_bac_final <- function(time_elapsed, bac_initial) {
    round(ifelse(time_elapsed >= bac_initial/0.15, # Check if enought time has passed ...
                 0,                                ## ... such that BAC is zero. 
                 ifelse(time_elapsed >= 2,         ## Otherwise, see if the drinking time is at least 2 hours.
                        bac_initial - (0.15 * (time_elapsed - 1)), ## If so, apply this equation ... 
                        bac_initial)), digits = 3)                 ## and if not, return initial BAC
  }
  get_bac_final(time_elapsed, bac_initial)
}






