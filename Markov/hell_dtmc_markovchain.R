library(markovchain)

# transition matrix
# i -> j
# row sum must be 1
tmA <- matrix(c(0,0.4,0.6,
                0.4,0.4,0.2,
                0,0,1), nrow=3, byrow=T)

# create the DTMC
dtmcA <- new("markovchain", 
             transitionMatrix=tmA,
             states=c("a","b","c"),
             name="MarkovChainA")

# plot it
plot(dtmcA)

# probability from state 1 to state 2
dtmcA[1,2]

# transition probability by state names  (b->c)
transitionProbability(dtmcA, "b","c")

# simulation: from "initial state", after 4 steps
inicialState <- c(0,1,0)
steps <- 4
finalState <- inicialState * dtmcA^steps

# result
finalState

# steadyState (equilibrium)
steadyStates(dtmcA)

# conversoins
as(dtmcA, "data.frame")
as(dtmcA, "igraph")

# resumo
summary(dtmcA)

# state types
absorbingStates(dtmcA)
transientStates(dtmcA)
conditionalDistribution(dtmcA,"a")
canonicForm(dtmcA)
recurrentClasses(dtmcA)
communicatingClasses(dtmcA)

# estimating the transition probabilities
data(rain)
mysequence <- rain$rain
createSequenceMatrix(mysequence)
myFit<-markovchainFit(data=mysequence,confidencelevel = .9,method = "mle")
myFit
