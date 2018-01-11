module parameter_globle
    implicit none
    integer,parameter           :: nub  = 150                         !! the number of chromosome(population), generally 20-200
    integer,parameter           :: max  = 81                          !! the maximal generation of population
    integer                     :: vars                               !! the number of variable  
    integer                     :: BgnSeparateN = 10                  !! the initial generation for chromosome assembling 
    integer                     :: generation                         !! current generation 
    real                        :: pro_cross    = 0.8                 !! the probablity of cross  (0.4 - 0.99)
    real                        :: pro_mutation = 0.1                 !! the probablity of mutation  (0.0001 - 0.1)
    real                        :: p            = 0.8                 !! the constant of mutation 
    real                        :: cross_rate   = 0.5                 !! the factor of cross 
    real                        :: dif          = 1e0                 !! the difference of current and former generation 
    real                        :: average,newaverage                 !! the average of fitness 
    real                        :: time1,time2                        !! the computing time
    integer,parameter           :: part         = 5                   !! the number of the partition of variable's bound  (1 - 10) £¬different problem and condition need diffenret partition
    real,parameter              :: PI = 3.1415926535897932384626
    real,parameter              :: e0 = 2.71828182845904523536028747  !! natural logarithm
    real,parameter              :: Miu0 = 4*PI*1.0E-7
    real,parameter              :: Eta0 = 120*Pi
    real,parameter              :: C0 = 3.E8                          !! velocity of light
    real ,parameter             :: Eps0 = 8.854*1.0E-12
    end module
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
    
module gene
use parameter_globle
implicit none 
    type :: genechrome
        real,allocatable         :: gene(:)                           !! floating number coding
        real                     :: fitness                           !! the fitness of chromosome
        real,allocatable         :: upper(:),lower(:)                 !! the bound of variable
        real                     :: rfitness                          !! the choosen chance of chromosome 
        real                     :: a_fitness                         !! the amendatory fitness of chromosome
    end type genechrome
    end module
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
    


