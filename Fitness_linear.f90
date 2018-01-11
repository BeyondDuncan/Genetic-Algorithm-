subroutine fitness_linear(pop)                                      !! linear scale the fitness -- evenly distribute the fitness of population,
    use gene                                                        !! in order to reduce the difference of fitness£¬on condition that large-probablity select elite,   
    implicit none                                                   !! increase chosen probablity of small-fitness chromosome.
    type(genechrome)    :: pop(nub+1)                               
    real                :: C=1.7                                    !! the constant of fitness scale
    real                :: a,b                                      !! the coefficient of fitness scale
    integer             :: maxx,min
    integer             :: i,j
    real,external       :: func
    integer,external    :: selectmin
    integer,external    :: selectmax
    
 !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   

    min = selectmin(pop)
    maxx = selectmax(pop)
    if (pop(min)%fitness > (C*newaverage - pop(maxx)%fitness)/C-1) then 
        a = (C-1 ) * newaverage / (pop(maxx)%fitness - newaverage)
        b = (pop(maxx)%fitness - C * newaverage) / (pop(maxx)%fitness-newaverage) * newaverage
    else
        a = newaverage/(newaverage-pop(min)%fitness)
        b = pop(min)%fitness * newaverage / (newaverage-pop(min)%fitness)
    end if
    
    do i = 1,nub
        pop(i)%a_fitness = a * pop(i)%a_fitness + b
    end do
    
    end 
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       