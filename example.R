###############################################################################
#                                                                             #
#                          simulating kelly systems                           #
#                                                                             #
###############################################################################

library(tidyverse)
source("kelly.R")



###############################################################################
#                                                                             #
#                                 single wager                                #
#                                                                             #
###############################################################################

INITIAL_FORTUNE = 100
W = 0 # proportion of fortune withheld

set.seed(1)
exampleHorses <- initializeHorses()
example <- initializeRaceSimulation(exampleHorses)
example # bet on 4

newFortune <- betKellySingle(horseToBet = example[4,],
                             horses = exampleHorses,
                             fortune = INITIAL_FORTUNE,
                             payout = 5)
newFortune

for (i in 1:50000) {
  newFortune <- betKellySingle(horseToBet = example[4,],
                               horses = exampleHorses,
                               fortune = newFortune,
                               payout = 5)
  if (newFortune <= 0) break
  
  if (i %% 500 == 0) {
    print(newFortune)
  }
}



###############################################################################
#                                                                             #
#                          simultaneous wager (w = 0)                         #
#                                                                             #
###############################################################################

INITIAL_FORTUNE = 100
W = 0 # proportion of fortune withheld

set.seed(99999)
exampleHorses <- initializeHorses()
example <- initializeRaceSimulation(exampleHorses)
example

newFortune <- betKellySimul(horses = exampleHorses,
                            horseData = example,
                            fortune = INITIAL_FORTUNE,
                            w = W,
                            payout = 10)
newFortune

for (i in 1:1000) {
  newFortune <- betKellySimul(horses = exampleHorses,
                              horseData = example,
                              fortune = INITIAL_FORTUNE,
                              w = W,
                              payout = 10)
  
  if (newFortune <= 0) {
    print(newFortune)
    break
  }
  
  print(newFortune)
}



###############################################################################
#                                                                             #
#                          simultaneous wager (w = .2)                        #
#                                                                             #
###############################################################################

INITIAL_FORTUNE = 100
W = .2 # proportion of fortune withheld

set.seed(99999)
exampleHorses <- initializeHorses()
example <- initializeRaceSimulation(exampleHorses)
example

newFortune <- betKellySimul(horses = exampleHorses,
                            horseData = example,
                            fortune = INITIAL_FORTUNE,
                            w = W,
                            payout = 10)
newFortune

for (i in 1:1000) {
  newFortune <- betKellySimul(horses = exampleHorses,
                              horseData = example,
                              fortune = INITIAL_FORTUNE,
                              w = W,
                              payout = 10)
  
  if (newFortune <= 0) {
    print(newFortune)
    break
  }
  
  print(newFortune)
}
