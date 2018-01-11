# Genetic-Algorithm-
This program is to solve the problem of optimization by GA  <br/>
*Date: 10:17-Dec-8th-2017, by Moyu Deng at UESTC*  <br/>
*Email: beyondduncan@foxmail.com*   <br/>
## Input information
 The input file is $information.txt$   <br/>
 Noting: Another indispensabel input information is in the subroutine $source$, the function $func$ is the evaluate function   <br/>
## The subroutines
+ $main$: main routine of the program. To avoid caught in the local extrem value, catastrophe algorithm，fitness scaling method and partition method are used.
  At first, it calls subroutines $initial$ and $evaluate$ to generate the initial group. Then, in the circulation part, different choosing strategies are used. Next, it calls $cross$ and $mutation$ to generate new chromosome.  <br/>
+ $module$: 1. all the globle parameters in this program 2.  use $parameter_globle$ or $gene$ allows to use all the parameters. <br/>
+ $source$: 1. some operations used in this program: sorting, maximum, minimum, random number. 2. the evaluate function $func$， some test func(s) are presented
+ $catastrophe$: simulate the phenomenon 'catastrophe' in the nature 
+ $choose1$: the traditional choosing method -- Roulette-wheel selection
+ $choose2$: elitist proportionate selection
+ $choose3$: tournament selection
+ $cross$: the cross operation 
+ $evaluate$: caculate the fitness of each chromosome
+ $fitness_linear$: linearly change the fitness 
+ $initilise$: 1. input the information 2. initialise the chromosome, floating nubmber coding method is used.
+ $mutation$: the mutation operation
+ $update_fitness$: update the cross-chromosome's or mutation-chromosome's fitness.
## Output
Output file is the $result.txt$. The fitness and gene of each generation's elite are recorded
