subroutine choose3(pop,newpop)                          !! tournament selection 
    use gene                                            !! choose a pair of chromosome according to Roulette-wheel selection,
    implicit none                                       !! then choose the greater fitness one until, another new population
    integer             :: i,j,m
    integer             :: k
    type(genechrome)    :: pop(nub+1)
    type(genechrome)    :: newpop(nub+1)
    real                :: sum,temp
    real                :: b(2)                         !! random numbers  (0 - 1)
    integer             :: times(nub) = 0               !! the choosen times of the chromosome
    integer             :: chosen(2)  = 0               !! the index of the two choosen chromosome 
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%calculate the rate%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    sum = 0
    do i = 1,nub
        sum = sum + pop(i)%a_fitness
    end do 
    do i = 1,nub 
        pop(i)%rfitness = pop(i)%a_fitness/sum
    end do
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    call paixu(pop)
    call RANDOM_SEED()
    do k = 1,nub!-1
        sum  = pop(1)%rfitness
        temp = 0
        m = 0
        do i = 1,2
            call random_number(b(i))
        end do
        do i=1,nub-1
            do j=1,2
                if (b(j)<=sum .and. b(j)>temp) then     !! calculate the cumulative probablity and then compare   
                    m = m + 1  
                    times(i) = times(i)+1
                    chosen(m) = i
                end if
            end do
            temp = sum
	        sum=sum+pop(i+1)%rfitness
        end do
    
        if(chosen(2)==0)  then 
            chosen(2) = nub
        end if
        if(chosen(1)==0)  then
            chosen(1) = nub
        end if
        newpop(k) = pop(chosen(1))
        
        if(pop(chosen(1))%fitness < pop(chosen(2))%fitness) then
            newpop(k) = pop(chosen(2))
        end if 
    end do 
    end subroutine
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%