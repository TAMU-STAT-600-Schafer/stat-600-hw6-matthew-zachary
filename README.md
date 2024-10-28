[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/42maKIji)
## Homework 6 - Kmeans and Multi-logictic Rcpp with Armadillo implementation via R package

# Introduction
In this homework, you will practice basics of R package development, and the use of Rcpp Armadillo within R package based on your HWs 2 and 3. You will be working with another student on this assignment. To split the load, it is expected that you will

1) Discuss both of your R codes and change your own implementation to be more optimal (make it easier to transfer your code to C++) based on mutual feedback;

2) Decide who will do first tackle of HW2 (C++, R wrapper, documentation) and who will do first tackle of HW3 (C++, R wrapper, documentation). In case you can't decide on your own, the person whose last name comes first alphabetically will be taking HW2.

3) Discuss with each other any issues that arise as you code in C++. You can start editing code from each other already at this point or wait till 4)

4) After the first draft is complete, discuss your current code with each other, and then switch to make adjustments (to optimize the code, to add comments, to add documentation, etc). Thus, the person who did initial coding for HW2 will then switch to make adjustments/polish HW3, and vice versa.

In the end, we expect to see a similar amount of commits/code from both of you with
- one person doing major commits on Kmeans, and then some commits on LRmulticlass
- another person doing major commits on LRmulticlass, and then some commits on Kmeans

The goal is to learn from each other, and to gain experience working on the same repository in a group. Because of this, you will lose points if only one person does all of the work, but you will also lose points if you never touch the other files.

## Starter code

This github repository is already setup as an R package with NAMESPACE being generated using roxygen2, and all the necessary setup for C++ to work. Both R wrappers and C++ wrappers are provided. For each algorithm (kmeans or multi-class), you should

* Move applicable compatibility checks and initialization from HW into R wrapper (in **R** folder)
* Translate the main algorithm into C++ (fill in corresponding function)
* Write basic documentation for the R wrapper function explaining input parameters, output, and providing a **SIMPLE** example (based on simulated data, no extra datasets)

**Slight modification on HW3:**

Because HW3 code is slightly bigger, we will simplify it a bit for this assignment. Specifically, the function **LRMultiClass** now only takes training data **X**, **y** (no test data), and only returns the matrix $\beta$ and the vector of objective function values (so no need to calculate training errors).

**R package DESCRIPTION file:**

Both of you should fill the missing information in DESCRIPTION file

- Authors (it doesn't matter who you put as maintainer)
- Title and Description in your own words
- Maintainer (it doesn't matter who but it needs to be a real person with real email, if in doubt go alphabetically)
- License
- Updated version

In the end, you should have function **MyKmeans** and **LRMultiClass** that are exported to the user, and all other functions being internal. The exported functions should be documented with examples so make sure **Check** does not return warning or errors.

## Grading for this assignment

Because this is a team assignment, you both accept responsibility for correctness of assignment and its documentation. So if you lose points on HW2 but not on HW3, both students will still lose points. However, 

Your assignment will be graded based on 

 * correctness _(50% of the grade)_

Take advantage of the fact that you have a correct R functions to extensively test your C++ code. 
 
 * speed _(10% of the grade)_ 
 
You should expect to see multiple folds speed improvement from moving your code to C++. To make the assignment simpler, you will get full points if your correct C++ code is at least three folds faster than your correct R code.

 * **(NEW)** R package checks/documentation _(20% of the grade)_
 
We will expect to see a reasonable DESCRIPTION file, all inputs/outputs being adequately documented, good examples that run without errors and don't take a lot of time (< 10 seconds). Use **Check** within R packages build to automatically check for some of these things

  * code style/documentation _(10% of the grade)_

You need to comment different parts of the code so it's clear what they do, have good indentation, readable code with names that make sense. See guidelines on R style (similar guidelines apply to C++ code), and posted grading rubric.

* **(NEW)** version control/commit practices in a team _(10% of the grade)_
 
 I expect you to start early on this assignment, and work gradually. You want to commit often, have logically organized commits with short description that makes sense. See guidelines on good commit practices, and posted grading rubric. Please **review Topic 1** on collaborative workflow in Github, and make sure you always **pull** changes first before you **push**.
 
