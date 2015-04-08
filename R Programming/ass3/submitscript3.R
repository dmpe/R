submit <- local({
        checkResult <- function(r, name = c("best", "rankhospital", "rankall")) {
                name <- match.arg(name)
                if(name == "best" || name == "rankhospital") {
                        if(length(r) == 1L && is.na(r))
                                return(r)
                        if(!is.character(r))
                                stop(sprintf("'%s' did not return a character vector",
                                             name))
                        if(!length(r))
                                stop(sprintf("'%s' returned character vector of length 0", name))
                        if(length(r) > 1)
                                stop(sprintf("'%s' returned a character vector of length > 1", name))
                }
                else if(name == "rankall") {
                        if(!is.data.frame(r))
                                stop(sprintf("'%s' did not return a data frame", name))
                        if(ncol(r) != 2L)
                                stop(sprintf("'%s' should return data frame with exactly 2 columns", name))
                        if(!all(names(r) %in% c("hospital", "state")))
                                stop("column names of data frame should be 'hospital' and 'state'")
                }
                r
        }
        getOutput <- function(sid) {
                ## JUST FOR TESTING
                ## sid <- sub("-dev", "", sid, fixed = TRUE)
                if(sid == "best-1") {
                        source("best.R", local = TRUE)
                        cat("Running test:\n")
                        cat("best(\"SC\", \"heart attack\")\n")
                        r <- best("SC", "heart attack")
                        checkResult(r, "best")
                }
                else if(sid == "best-2") {
                        source("best.R", local = TRUE)
                        cat("Running test:\n")
                        cat("best(\"NY\", \"pneumonia\")\n")
                        r <- best("NY", "pneumonia")
                        checkResult(r, "best")
                }
                else if(sid == "best-3") {
                        source("best.R", local = TRUE)
                        cat("Running test:\n")
                        cat("best(\"NN\", \"pneumonia\")\n")
                        r <- tryCatch(best("NN", "pneumonia"), error = function(e) e)
                        if(!inherits(r, "error"))
                                stop("'best' should throw an error via the 'stop' function in this case")
                        tolower(conditionMessage(r))
                }
                else if(sid == "rankhospital-1") {
                        source("rankhospital.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankhospital(\"NC\", \"heart attack\", \"worst\")\n")
                        r <- rankhospital("NC", "heart attack", "worst")
                        checkResult(r, "rankhospital")
                }
                else if(sid == "rankhospital-2") {
                        source("rankhospital.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankhospital(\"WA\", \"heart attack\", 7)\n")
                        r <- rankhospital("WA", "heart attack", 7)
                        checkResult(r, "rankhospital")
                }
                else if(sid == "rankhospital-3") {
                        source("rankhospital.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankhospital(\"WA\", \"pneumonia\", 1000)\n")
                        rankhospital("WA", "pneumonia", 1000)
                }
                else if(sid == "rankhospital-4") {
                        source("rankhospital.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankhospital(\"NY\", \"heart attak\", 7)\n")
                        r <- tryCatch({
                                rankhospital("NY", "heart attak", 7)
                        }, error = function(e) {
                                e
                        })
                        if(!inherits(r, "error"))
                                stop("'rankhospital' should throw an error via 'stop' in this case")
                        tolower(conditionMessage(r))
                }
                else if(sid == "rankall-1") {
                        source("rankall.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankall(\"heart attack\", 4)\n")
                        r <- rankall("heart attack", 4)
                        r <- checkResult(r, "rankall")
                        as.character(subset(r, state == "HI")$hospital)
                }
                else if(sid == "rankall-2") {
                        source("rankall.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankall(\"pneumonia\", \"worst\")\n")
                        r <- rankall("pneumonia", "worst")
                        r <- checkResult(r, "rankall")
                        as.character(subset(r, state == "NJ")$hospital)
                }
                else if(sid == "rankall-3") {
                        source("rankall.R", local = TRUE)
                        cat("Running test:\n")
                        cat("rankall(\"heart failure\", 10)\n")
                        r <- rankall("heart failure", 10)
                        r <- checkResult(r, "rankall")
                        as.character(subset(r, state == "NV")$hospital)
                }
                else {
                        stop("invalid part number")
                }
        }
        partPrompt <- function() {
                partlist <- list("best-1" = "'best' part 1",
                                 "best-2" = "'best' part 2",
                                 "best-3" = "'best' part 3",
                                 "rankhospital-1" = "'rankhospital' part 1",
                                 "rankhospital-2" = "'rankhospital' part 2",
                                 "rankhospital-3" = "'rankhospital' part 3",
                                 "rankhospital-4" = "'rankhospital' part 4",
                                 "rankall-1" = "'rankall' part 1",
                                 "rankall-2" = "'rankall' part 2",
                                 "rankall-3" = "'rankall' part 3"
                                 )
                pretty_out("Which part are you submitting?")
                part <- select.list(partlist, graphics=FALSE)
                names(part)
        }
        getChallenge <- function(email, challenge.url) {
                params <- list(email_address = email, response_encoding = "delim")
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
                                        # Remove quotes if there are any
                        courseid <- gsub("\'|\"", "", courseid)
                                        # Set up test cases
                        is_url <- str_detect(courseid, "www[.]|http:|https:")
                        is_numbers <- str_detect(courseid, "^[0-9]+$")
                        is_example <- str_detect(courseid, fixed("rprog-001"))
                        
                                        # Check if courseid is none of the bad things
                        if(!any(is_url, is_numbers, is_example)){
                                break
                                        # courseid is one of the bad things
                        } else {
                                        # Check if courseid is a url
                                if(is_url) {
                                        pretty_out("It looks like you entered a web address, which is not what I'm",
                                                   "looking for.")
                                }
                                        # Check if courseid is all numbers
                                if(is_numbers) {
                                        pretty_out("It looks like you entered a numeric ID, which is not what I'm",
                                                   "looking for.")
                                }
                                        # Check if the user stole the example courseid
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
                        
                                        # Set urls based on confirmed courseid
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
                                        # Check if url is valid, i.e. challenge received
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
