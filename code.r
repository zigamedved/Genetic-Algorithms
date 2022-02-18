# we are given the numbers 10,25,100,5,3 and operators +,−,/,∗
data <- c(3, 5, 10, 25, 100, "+", "-", "/", "*")
target <- 2512

# data <- c(1,60,22,15,100,12,10,56,11,4,7,9,3,"*","/","-","+")
# target <- 42069

# data <- c(1,60,22,15,100,12,10,56,11,4,7,9,3,111,123,"*","/","-","+")
# target <- 1-60*22+15/100+12*10-9-7+3*4*7*56+11-111-123

exprMaxLen <- length(data)
operator_indices <- seq(exprMaxLen + 1 - floor(exprMaxLen / 2), exprMaxLen)
operand_indices <- seq(1, exprMaxLen - floor(exprMaxLen / 2))


library(R.utils)

#RS implementation
random_search <- function(maxiter=1000) {
    len <- ceiling(exprMaxLen/2)
    best <- c(1, exprMaxLen, 1)
    best_fitness <- -target

    for (i in seq(1, maxiter)) {
        #generate random expression
        ioperands <- sample(operand_indices, len, replace = FALSE)
        ioperators <- sample(operator_indices, len - 1, replace = TRUE)
        p <- insert(ioperands, 2:length(ioperands), ioperators)
        #calculate its fitness
        fitness <- myFitness(p)
        if (fitness > best_fitness) {#check if this is the best expression yet
            best <- p
            best_fitness <- fitness
        }
        if (best_fitness >= 0) {
            return(list(best, best_fitness, i))
        }
    }
    return(list(best, best_fitness, maxiter))
}

myInitPopulation <- function(object) {
    maxLen <- object@popSize
    pop <- matrix(0, maxLen, exprMaxLen)

    for (i in 1:maxLen) {
        # select length on a given interval
        len <- ceiling(exprMaxLen/2)

        #randomly select len operands without repeating
        ioperands <- sample(operand_indices, len, replace = FALSE)

        #randomly select len-1 operators
        ioperators <- sample(operator_indices, len - 1, replace = TRUE)

        #if there arent any operators just keep operands,
        #otherwise merge operators and operands
        if (!length(ioperators)) {
            p <- ioperands
        }else {
            p <- insert(ioperands, 2:length(ioperands), ioperators) # nolint
        }

        #add expression to population
        pop[i, seq(1, length(p))] <- p
    }
    pop
}

#fitness function
myFitness <- function(expression) {
    # print(expression)
    expr_string <- paste(data[expression], collapse = "")
    # print(expr_string)
    # print(parse(text = expr_string))
    expr_value <- eval(parse(text = expr_string))
    penalty <- abs(target - expr_value)
    score <- ((-penalty)/100) 
    score
}

#mutate by swapping two elements of same type(operand/operator)
myMutation1 <- function(object, parent) {
    # Select a parent from the population
    mutate <- parent <- as.vector(object@population[parent, ])
    # print(object@population[parent, ])
    # If there are enough elements randomly decide between swapping
    # operands or operators but only if there are at least two operators
    if (sample(0:1, 1)) {
        # Sample two operators
        m <- sample(seq(2, exprMaxLen, by = 2), size = 2, replace = FALSE)
    }else {
        # Sample two operands
        m <- sample(seq(1, exprMaxLen, by = 2), size = 2, replace = FALSE)
    }
    # Swap the elements in the selected numbers
    mutate[m[1]] <- parent[m[2]]
    mutate[m[2]] <- parent[m[1]]
    # print(c("parent:", object@population[parent, ],"mutate:",mutate))
    return(mutate)
}

# We must take a random snippet from expr1 and a random snippet from expr2,
#   which start the same "type" of index(odd/even) and have the same type
#   of length(odd/even) and swap them
# EDGE CASE: the snippets end with an even index(operand) and they are
#   added to the end of the expression leading to an invalid expression
# We therefore have four cases:
#   1) the snippets are of even length
#       1.1) and start at even index -> (operator + expression)
#       1.2) and start at odd index -> (expression + operator)
#   2) the snippets are of odd length
#       1.1) and start at even index -> (operator + expression + operator)
#       1.2) and start at odd index -> (expression)
#assuming equal length of exprMaxLen-1:
myCrossover1 <- function(object, parents) {
    expr <- object@population[parents, ]
    # print(expr)
    childs <- matrix(0, 2, exprMaxLen)
    #numbers on interval [1,exprMaxLen]
    snip_index <- c(0, 0)
    snip_index[1] <- sample(seq(1, exprMaxLen), 1)
    snip_length <- sample(seq(1, exprMaxLen - snip_index[1] + 1), 1)

    snip_index_type <- (snip_index[1] + 1) %% 2 + 1 #1~odd or 2~even

    # set index in second expression that leaves at least sniplength space
    snip_index[2] <- snip_index_type
    if (snip_index_type > (exprMaxLen - snip_length + 1)) {
        snip_index[2] <- sample(seq(snip_index_type, exprMaxLen - snip_length + 1, by=2), 1) # nolint
    }

    # CROSS CHILDREN
    for (i in seq(1, length(parents))) {
        other_parent <- i %% 2 + 1
        snippet <- expr[other_parent, snip_index[other_parent]:(snip_index[other_parent] + snip_length - 1)] # nolint
        childs[i, snip_index[i]:(snip_index[i] + snip_length - 1)] <- snippet

        insert_at <- which(childs[i, ] == 0)
        # print(insert_at)

        take_index <- 1
        take <- expr[i, seq(1, exprMaxLen, by = 2)]

        for (j in insert_at) {
            if (((j %% 2) == 0)) {
                childs[i, j] <- expr[i, j]
            }else {
                while (TRUE) {
                    if (!(take[take_index] %in% snippet)) {
                        childs[i, j] <- take[take_index]
                        take_index <- take_index + 1
                        break
                    }
                    take_index <- take_index + 1
                    if (take_index > length(take)) {
                        print("FUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU")
                    }
                }
            }
        }
        tryCatch(
            expr = {
                myFitness(childs[i, ])
            },
            error = function(e) {
                print(c("~~~~~~~~~~~~~~~~jok.:')~~~~~~~~~~~~~~~~"))
                print(c("parent 1: ", paste(expr[i, ], collapse = "-")))
                print(c("parent 2: ", paste(expr[other_parent, ], collapse = "-")))
                print(c("child: ", paste(childs[i, ], collapse = "-")))
                print(c("insert at: ", paste(insert_at, collapse = "-")))
                print(c("take: ", paste(take, collapse = "-")))
                print(c("snip_insert: ", snip_index[i]))
                print(c("snip_length: ", snip_length))
                print(c("snippet: ", paste(snippet, collapse = "-")))
            }
        )
    }
    return(list(children = childs, fitness = rep(NA, 2)))
}

# swap operators from parent1 with operators from parent2 and vice versa
myCrossover2 <- function(object, parents) {
    expr <- object@population[parents, ]
    childs <- expr
    childs[1, seq(2, exprMaxLen, by = 2)] <- expr[2, seq(2, exprMaxLen, by = 2)]
    childs[2, seq(2, exprMaxLen, by = 2)] <- expr[1, seq(2, exprMaxLen, by = 2)]

    # print(c("parent 1: ", paste(expr[1, ], collapse = "-")))
    # print(c("parent 2: ", paste(expr[2, ], collapse = "-")))
    # print(c("child 1: ", paste(childs[1, ], collapse = "-")))
    # print(c("take: ", paste(expr[2, seq(2, exprMaxLen, by = 2)], collapse = "-")))
    return(list(children = childs, fitness = rep(NA, 2)))
}

# change 'swapnumber' of operators in expression into random other operators
myMutation2 <- function(object, parent) {
    # Select a parent from the population
    mutate <- parent <- as.vector(object@population[parent, ])
    swapnumber <- 3
    # Sample 'swapnumber' operators
    m <- sample(seq(2, exprMaxLen, by = 2), size = swapnumber, replace = FALSE)
    # Change the elements in the selected numbers into any *other* operators
    for (i in seq(1, swapnumber)) {
        mutate[m[i]] <- sample(setdiff(operator_indices, parent[m[i]]), 1)
    }

    # print(c("parent: ", paste(parent, collapse = "-")))
    # print(c("mutate: ", paste(mutate, collapse = "-")))
    # print(c("pick: ", paste(m, collapse = "-")))
    return(mutate)
}

# RUNNING
library(GA)



#comparison of RS an GA elapsed time and effectiveness
compare <- function(n=30) {
    ga_times <- c()
    ga_results <- c()
    rs_times <- c()
    rs_results <- c()

    t <- seq(1, n)
    prevStep <- 0 
    for (k in t) {
        ga_times <- append(ga_times, system.time({
            GA <- ga(type = "permutation",population = myInitPopulation, fitness = myFitness,
                lower = 1, upper = exprMaxLen, popSize = 50, maxiter = 500,
                run = 20, pmutation = 0.05, mutation = myMutation2, pcrossover = 0.5, crossover = myCrossover1, monitor = FALSE)
            ga_results <- c(ga_results, GA@fitnessValue)
        })["elapsed"])

        rs_times <- append(rs_times, system.time({
            rs_results <- c(rs_results, unlist(random_search(100)[2]))
        })["elapsed"])

        nextStep <- floor((k / n * 100))
        if(nextStep != prevStep){
            print(prevStep)
            prevStep = nextStep
        }
    }

    par(mfrow=c(1,2))

    y_max <- max(c(ga_times, rs_times))
    plot(t,ga_times, type="b", col="green", lwd=2, pch=15, xlab="Attempt", ylab="Time elapsed (s)", ylim=c(0,y_max))
    lines(t, rs_times, type="b", col="red", lwd=2, pch=15)
    title("Run time measurements of GA and RS")
    legend("bottomright", c("GA","RS"), lwd=c(2,2), col=c("green","red"), pch=c(15,15), y.intersp=1.5) # nolint

    y_max <- min(c(ga_results, rs_results))
    plot(t,ga_results, type="b", col="green", lwd=2, pch=15, xlab="Attempt", ylab="Fitness", ylim=c(y_max,0))
    lines(t, rs_results, type="b", col="red", lwd=2, pch=15)
    title("Fitness measurements of GA and RS")
    legend("bottomright", c("GA","RS"), lwd=c(2,2), col=c("green","red"), pch=c(15,15), y.intersp=1.5) # nolint

    # y_max <- max(ga_times)
    # plot(t,ga_times, type="b", col="green", lwd=2, pch=15, xlab="Attempt", ylab="Time elapsed (s)", ylim=c(0,y_max))
    # title("Run time measurements of GA and RS")

    # y_max <- min(ga_results)
    # plot(t,ga_results, type="b", col="green", lwd=2, pch=15, xlab="Attempt", ylab="Fitness", ylim=c(y_max,0))
    # title("Fitness measurements of GA and RS")

}

# data
# target
# compare(100)

# GA <- ga(type = "permutation",population = myInitPopulation, fitness = myFitness,
#             lower = 1, upper = exprMaxLen, popSize = 100, maxiter = 1000,
#             run = 100, pmutation = 0.99, mutation = myMutation2, pcrossover = 0.7, crossover = myCrossover1)

# summary(GA)