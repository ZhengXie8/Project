# Tube-Ball Sorting Optimization

## Table of Contents
- [Introduction](#introduction)
- [Algorithm](#algorithm)
- [Weighting System](#weighting-system)
- [Increasing Efficiency](#increasing-efficiency)


## Introduction
![ced5da09adc78d9eec922a50db81f55d](https://user-images.githubusercontent.com/132629734/236593798-d4271bad-4bf3-401a-a1b9-300641c92e18.jpg)
In the Ball sorting game ⬆️, we have the height of each tube and the number of tubes. Given the color of each ball, we need to check if the game is sovable or not. The tubes.rkt does it in a slow algorithm, which takes an incredibly long time for checking games that have more than 4 tubes (sometimes 3 minutes). The program in tubes-bonus.rkt can do all games that have less than 10 tubes in 7 seconds.


## Algorithm
We define "block" to be the part that the balls have the same color, and the length of block is the number of balls in that part. For example, if the tube has balls from bottom to top: Red Red Blue Yellow Yellow, then it has three parts: (Red Red) (Blue) (Yellow Yellow). The length for each block should be two one two respectively. Since the longer the block at the bottom is, the easier for a tube to have balls that are all the same color, we always move the balls at the top that has a longer block at the bottom to the empty tube. Ex. if we have three tubes, one is (Red Red) (Blue Blue Blue), and one is (Yellow) (Red) (Blue) (Yellow Yellow), and one is empty, then we move (Red Red) to the empty tube, since the bottom at the bottom is longer.
### Weighting System
To apply this method on code, we use an algorithm that we called weighting system with using an accumulator. We assign each block a value according to their length (longer the block at the bottom is, higher the value of the block has). The next move is always about the tube that has the greatest value.
### Increasing Efficiency
If we estimate the avergae time consumed for the original program to be 45s, then the efficiency is increased by 84%.
