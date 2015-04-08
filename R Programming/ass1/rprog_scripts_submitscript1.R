
submit <- local({
        getOutput <- function(sid) {
                ## JUST FOR TESTING
                ## sid <- sub("-dev", "", sid, fixed = TRUE)
                if(sid == "pollutantmean-1") {
                        source("pollutantmean.R")
                        pollutantmean("specdata", "sulfate", 1:10)
                }
                else if(sid == "pollutantmean-2") {
                        source("pollutantmean.R")
                        pollutantmean("specdata", "nitrate", 70:72)
                }
                else if(sid == "pollutantmean-3") {
                        source("pollutantmean.R")
                        pollutantmean("specdata", "sulfate", 34)
                }
                else if(sid == "pollutantmean-4") {
                        source("pollutantmean.R")
                        pollutantmean("specdata", "nitrate")
                }
                else if(sid == "complete-1") {
                        source("complete.R")
                        cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
                        paste(cc$nobs, collapse = "\n")
                }
                else if(sid == "complete-2") {
                        source("complete.R")
                        cc <- complete("specdata", 54)
                        cc$nobs
                }
                else if(sid == "complete-3") {
                        source("complete.R")
                        set.seed(42)
                        cc <- complete("specdata", 332:1)
                        use <- sample(332, 10)
                        paste(cc[use, "nobs"], collapse = "\n")                
                }
                else if(sid == "corr-1") {
                        source("corr.R")
                        cr <- corr("specdata")
                        cr <- sort(cr)
                        set.seed(868)
                        out <- round(cr[sample(length(cr), 5)], 4)
                        paste(out, collapse = "\n")
                }
                else if(sid == "corr-2") {
                        source("corr.R")
                        cr <- corr("specdata", 129)
                        cr <- sort(cr)
                        n <- length(cr)
                        set.seed(197)
                        out <- c(n, round(cr[sample(n, 5)], 4))
                        paste(out, collapse = "\n")
                        
                }
                else if(sid == "corr-3") {
                        source("corr.R")
                        cr <- corr("specdata", 2000)
                        n <- length(cr)
                        cr <- corr("specdata", 1000)
                        cr <- sort(cr)
                        paste(c(n, round(cr, 4)), collapse = "\n")
                }
                else {
                        stop("invalid part number")
                }
        }
        partPrompt <- function() {
                partlist <- list("pollutantmean-1" = "'pollutantmean' part 1",
                                 "pollutantmean-2" = "'pollutantmean' part 2",
                                 "pollutantmean-3" = "'pollutantmean' part 3",
                                 "pollutantmean-4" = "'pollutantmean' part 4",
                                 "complete-1" = "'complete' part 1",
                                 "complete-2" = "'complete' part 2",
                                 "complete-3" = "'complete' part 3",
                                 "corr-1" = "'corr' part 1",
                                 "corr-2" = "'corr' part 2",
                                 "corr-3" = "'corr' part 3"
                                 )
                
                pretty_out("Which part are you submitting?")
                part <- select.list(partlist, graphics=FALSE)
                names(part)
        }
        getChallenge <- function(email, challenge.url) {
                params <- list(email_address = email,
                               response_encoding = "delim")
                result <- getForm(challenge.url, .params = params)
                s <- strsplit(result, "|", fixed = TRUE)[[1]]
                list(ch.key = s[5], state = s[7])
        }
        challengeResponse <- function(password, ch.key) {
                x <- paste(ch.key, password, sep = "")
                digest(x, algo = "sha1", serialize = FALSE)
        }
        submitSolution <- function(email, ch.resp, sid, output, signature, submit.url,
                                   src = "", http.version = NULL) {
                output <- as.character(base64(output))
                src <- as.character(base64(src))
                params <- list(assignment_part_sid = sid,
                               email_address = email,
                               submission = output,
                               submission_aux = src,
                               challenge_response = ch.resp,
                               state = signature)
                params <- lapply(params, URLencode)
                result <- postForm(submit.url, .params = params)
                s <- strsplit(result, "\\r\\n")[[1]]
                tail(s, 1)
        }
        get_courseid <- function() {
                pretty_out("The first item I need is your Course ID. For example, if the",
                           "homepage for your Coursera course was",
                           "'https://class.coursera.org/rprog-001',",
                           "then your course ID would be 'rprog-001' (without the quotes).", skip_after=TRUE)
                repeat {
                        courseid <- readline("Course ID: ")
                        ## Remove quotes if there are any
                        courseid <- gsub("\'|\"", "", courseid)
                        ## Set up test cases
                        is_url <- str_detect(courseid, "www[.]|http:|https:")
                        is_numbers <- str_detect(courseid, "^[0-9]+$")
                        is_example <- str_detect(courseid, fixed("rprog-001"))
                        
                        ## Check if courseid is none of the bad things
                        if(!any(is_url, is_numbers, is_example)){
                                break
                                ## courseid is one of the bad things
                        } else {
                                ## Check if courseid is a url
                                if(is_url) {
                                        pretty_out("It looks like you entered a web address, which is not what I'm",
                                                   "looking for.")
                                }
                                ## Check if courseid is all numbers
                                if(is_numbers) {
                                        pretty_out("It looks like you entered a numeric ID, which is not what I'm",
                                                   "looking for.")
                                }
                                ## Check if the user stole the example courseid
                                if(is_example) {
                                        pretty_out("It looks like you entered the Course ID that I used as an",
                                                   "example, which is not what I'm looking for.")
                                }
                        }
                        pretty_out("Instead, I need your Course ID, which is the last",
                                   "part of the web address for your Coursera course.",
                                   "For example, if the homepage for your Coursera course was",
                                   "'https://class.coursera.org/rprog-001',",
                                   "then your course ID would be 'rprog-001' (without the quotes).",
                                   skip_after=TRUE)
                }
                courseid
        }
        pretty_out <- function(..., skip_before=TRUE, skip_after=FALSE) {
                wrapped <- strwrap(str_c(..., sep = " "),
                                   width = getOption("width") - 2)
                mes <- str_c("| ", wrapped, collapse = "\n")
                if(skip_before) mes <- paste0("\n", mes)
                if(skip_after) mes <- paste0(mes, "\n")
                message(mes)
        }
        checkPkgs <- function() {
                pkg.inst <- installed.packages()
                pkgs <- c("RCurl", "digest", "stringr")
                have.pkg <- pkgs %in% rownames(pkg.inst)
                
                if(any(!have.pkg)) {
                        message("\nSome packages need to be installed.\n")
                        r <- readline("Install necessary packages [y/n]? ")
                        if(tolower(r) == "y") {
                                need <- pkgs[!have.pkg]
                                message("\nInstalling packages ",
                                        paste(need, collapse = ", "))
                                install.packages(need)
                        }
                }
        }
        loginPrompt <- function() {
                courseid <- get_courseid()       
                email <- readline("Submission login (email): ")
                passwd <- readline("Submission  password: ")
                r <- list(courseid = courseid, email = email, passwd = passwd)
                assign(".CourseraLogin", r, globalenv())
                invisible(r)
        }
        function(manual = FALSE, resetLogin = FALSE) {
                checkPkgs()
                suppressPackageStartupMessages(library(RCurl))
                library(digest)
                library(stringr)
                readline("\nPress Enter to continue...")
                if(!manual) {
                        confirmed <- FALSE
                        need2fix <- FALSE
                        while(!confirmed) {
                                if(exists(".CourseraLogin") && !resetLogin && !need2fix)
                                        cred <- get(".CourseraLogin")
                                else
                                        cred <- loginPrompt()
                                if(!is.list(cred) || !(names(cred) %in% c("email", "passwd", "courseid")))
                                        stop("problem with login/password")
                                
                                courseid <- cred$courseid
                                email <- cred$email
                                password <- cred$passwd
                                
                                pretty_out("Is the following information correct?",
                                           skip_after=TRUE)
                                message("Course ID: ", courseid,
                                        "\nSubmission login (email): ", email, 
                                        "\nSubmission password: ", password)
                                yn <- c("Yes, go ahead!", 
                                        "No, I need to change something.")
                                confirmed <- identical(select.list(yn, graphics=FALSE), yn[1])
                                if(!confirmed) need2fix <- TRUE
                        }
                        
                        ## Set urls based on confirmed courseid
                        challenge.url <- paste("http://class.coursera.org", courseid,
                                               "assignment/challenge", sep = "/")
                        submit.url <- paste("http://class.coursera.org", courseid,
                                            "assignment/submit", sep = "/")
                }
                ## Prompt Submission Part
                sid <- partPrompt()
                
                ## Get output
                output <- getOutput(sid)        
                
                if(!manual) {
                        ## Get challenge
                        ch <- try(getChallenge(email, challenge.url), silent=TRUE)
                        ## Check if url is valid, i.e. challenge received
                        ch_ok <- !is(ch, "try-error") && exists("ch.key", ch) && !is.na(ch$ch.key)
                        if(!ch_ok) {
                                stop("Either the course ID you entered is not valid or your course site ", 
                                     "is unreachable at this time. If you'd like to submit manually, you ", 
                                     "can run submit(manual=TRUE).")
                        }
                        
                        ## Attempt submission with challenge
                        ch.resp <- challengeResponse(password, ch$ch.key)
                        results <- submitSolution(email, ch.resp, sid, output, ch$state, 
                                                  submit.url = submit.url)
                        if(!length(results))
                                results <- "Incorrect!"
                        cat("Result: ", results, "\n")
                }
                else {
                        outfile <- paste(sid, "output.txt", sep = "-")
                        writeLines(as.character(output), outfile)
                        cat(sprintf("Please upload the file '%s' to Coursera\n",
                                    outfile))
                }
                invisible()
        }
})

