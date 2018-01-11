subroutine choose1(pop,newpop)                                   !! Roulette-wheel selection
    use gene
    implicit none
    type(genechrome)    :: pop(nub+1)
    type(genechrome)    :: newpop(nub+1)
    integer             :: i,j,k,m 
    real                :: sum,temp
    real                :: b(nub)                                !! random numbers (0 - 1)
    integer             :: times(nub)                            !! the choosen times of the chromosome  
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%calculate the probability%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    sum = 0
    do i = 1,nub
        sum = sum + pop(i)%a_fitness
    end do 
    do i = 1,nub 
        pop(i)%rfitness = pop(i)%a_fitness/sum
    end do
    call paixu(pop)
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
  call RANDOM_SEED()
    do i=1,nub
        call random_number(b(i))
    end do
    
  sum  = pop(1)%rfitness
  temp = 0 
  do i=1,nub-1
        do j=1,nub
            if (b(j)<=sum .and. b(j)>temp) then                     !! calculate the cumulative probablity and then compare 
            end if
        end do
        temp = sum
	    sum=sum+pop(i+1)%rfitness
    end do
    temp=0
    do i = 1,nub-1
        temp=temp+times(i)
	end do 
    times(nub)=nub-temp
    
    
    j=1
    do i = 1,nub
        m=0
        do while(m<times(i) .and. j<21) 
            newpop(j)=pop(i)
            j = j+1
            m = m+1

        end do
    end do  
    
    call paixu(newpop)
    end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%