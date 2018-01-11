subroutine evaluate(pop,newpop)                                     !! calculate the fitness
    use gene
    implicit none
    integer             :: best_one                                 !! the index of the biggest-fitness chromosome 
    type(genechrome)    :: pop(nub+1)
    type(genechrome)    :: newpop(nub+1)
    real                :: x(vars)
    real                :: fitness_average                          !! averagefitness
    integer             :: i,j
    real,external       :: func                                     !! fitness function
    integer,external    :: selectmin

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    do j =1,nub
        do i = 1,vars
            x(i)=pop(j)%gene(i)
        end do 
         pop(j)%fitness = func(x)
    end do

!%%%%%%%%%%%%%%%%%%%%%%%%%Amend the fitnes to positive%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    best_one = 1
	do i=1,nub
        if (abs(pop(best_one)%fitness)<abs(pop(i)%fitness)) then
            best_one=i
		end if 
    end do		
    do j = 1,nub 
        pop(j)%a_fitness = pop(j)%fitness + abs(pop(best_one)%fitness)
    end do
    
newpop = pop

!%%%%%%%%%%%%%%%%%%%%%%%%calculate the average fitness%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    fitness_average = 0                                             
    do i = 1,nub
        fitness_average = fitness_average+pop(i)%fitness
    end do
        fitness_average = fitness_average/nub
    newaverage=fitness_average

    end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


