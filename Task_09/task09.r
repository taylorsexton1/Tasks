setwd("/Users/taylorsexton/Desktop/Evolution/Tasks/Task_09")
library(diversitree)
library(phytools)
transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.1
speciation_1 <- 0.2
extinction_1 <- 0.1
maxN <- 1e3
maxT <- 50
Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
?tree.bisse
simTree
plot(simTree)
stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)
speciation_1range <- seq(0.1, 0.9, 0.1)
sim_results <- list()
for (i in seq_along(speciation_1range)) {
  Pars[2] <- speciation_1range[i]
  simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
  sim_results[[i]] <- simTree
}
state_tables <- lapply(sim_results, function(tree) table(tree$tip.state) / sum(tree$tip.state))
state_tables
df <- data.frame(speciation_1 = speciation_1range, tip_state_0 = rep(0, length(speciation_1range)), tip_state_1 = rep(0, length(speciation_1range)))
for (i in seq_along(sim_results)) {
  state_table <- state_tables[[i]]
  df$tip_state_0[i] <- state_table[1]
  df$tip_state_1[i] <- state_table[2]
}
# Change thr speciation range
speciation_1range <- seq(0.2, 0.8, 0.1)
sim_results <- list()
for (i in seq_along(speciation_1range)) {
  Pars[2] <- speciation_1range[i]
  simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
  sim_results[[i]] <- simTree
}
state_tables <- lapply(sim_results, function(tree) table(tree$tip.state) / sum(tree$tip.state))
state_tables
df <- data.frame(speciation_1 = speciation_1range, tip_state_0 = rep(0, length(speciation_1range)), tip_state_1 = rep(0, length(speciation_1range)))
for (i in seq_along(sim_results)) {
  state_table <- state_tables[[i]]
  df$tip_state_0[i] <- state_table[1]
  df$tip_state_1[i] <- state_table[2]
}

nona_df <- na.omit(df)
nona_df
library(ggplot2)
ggplot(nona_df, aes(x = speciation_1)) +
  geom_line(aes(y = tip_state_0, color = "Tip State 0")) +
  geom_line(aes(y = tip_state_1, color = "Tip State 1")) +
  scale_color_manual(values = c("Tip State 0" = "blue", "Tip State 1" = "black")) +
  xlab("Speciation Rate for State 1") +
  ylab("Tip State Frequency") +
  ggtitle("Effect of Varying Speciation on Tip State Frequency") +
  theme_bw() +
  labs(color = "Tip States")

#Change beginning parameters
transition_0to1 <- 0.3
transition_1to0 <- 0.3
speciation_0 <- 0.4
extinction_0 <- 0.3
speciation_1 <- 0.4
extinction_1 <- 0.2
maxN <- 1e2
maxT <- 75
Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
simTree
plot(simTree)
stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)

speciation_1range <- seq(0.2, 0.8, 0.1)
sim_results <- list()
for (i in seq_along(speciation_1range)) {
  Pars[2] <- speciation_1range[i]
  simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
  sim_results[[i]] <- simTree
}
state_tables <- lapply(sim_results, function(tree) table(tree$tip.state) / sum(tree$tip.state))
state_tables
df <- data.frame(speciation_1 = speciation_1range, tip_state_0 = rep(0, length(speciation_1range)), tip_state_1 = rep(0, length(speciation_1range)))
for (i in seq_along(sim_results)) {
  state_table <- state_tables[[i]]
  df$tip_state_0[i] <- state_table[1]
  df$tip_state_1[i] <- state_table[2]
}

nona_df <- na.omit(df)
nona_df
library(ggplot2)
ggplot(nona_df, aes(x = speciation_1)) +
  geom_line(aes(y = tip_state_0, color = "Tip State 0")) +
  geom_line(aes(y = tip_state_1, color = "Tip State 1")) +
  scale_color_manual(values = c("Tip State 0" = "blue", "Tip State 1" = "black")) +
  xlab("Speciation Rate for State 1") +
  ylab("Tip State Frequency") +
  ggtitle("Effect of Varying Speciation on Tip State Frequency") +
  theme_bw() +
  labs(color = "Tip States")

# When the speciation rate for state 1 increases, the tip state 0 decreases in frequency. 
# The frequency of tip state 1 doesn't change when the speciation rate is changed.

