# An Eye for an Eye

Evolution simulator for the prisoner's dilemma.

## Purpose

The point of this project is to simulate scenarios in which `Agent`s
play out the prisoner's dilemma but with a conception of memory (so
they respond to what their counteragents last did to them). In order
to test the "fitness" of various strategies, the simulator also has
an appreciation of reproduction, such that `Agent`s achieving certain
scores will be able to reproduce.

The basic idea is that an `Agent` has two possible actions, `Cooperate`
and `Defect`. In the simulation, there is a `RewardsVector` which will
configure the calculation for what happens in the following scenarios,
for two `Agent`s `A` and `B`:

* `A` cooperates, `B` cooperates;
* `A` cooperates, `B` defects;
* `A` defects, `B` cooperates;
* `A` defects, `B` defects.

These outcomes will have different weightings, which will be used on
each round to calculate the `Agent`'s score, and therefore give it a
probability of reproducing.

The simplest behaviour is a "tit-for-tat" algorithm (or "an eye for an
eye"), in which the `Agent` will, by default, cooperate, but if the
counteragent defects, then it will defect in retaliation.

## Nuances in the simulation

### Imperfect world

No natural `Agent` is perfect, so the world will introduce a "fuck-up
factor" or "mistake factor". This will be the probability of an `Agent`
which, intending to do one action, does the opposite.

### Beg forgiveness

`Agent`s can also have a tolerance of defections in their counteragents,
by adjusting the `generosity` factor, which is the likelihood that
an `Agent` will cooperate even in the face of a defection from the
counteragent.

### El vivo vive del bobo

Some people might believe that by being selfish, one can get ahead, and
therefore one should defect more often in the hope that some sucker
cooperates anyway. The `selfishness` factor of an `Agent` is the
propensity to defect no matter what.

### Fitness inheritance

In this model, children inherit scores from their parents, to mimic how
advantage and disadvantage can be inherited in the real world.
