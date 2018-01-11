subroutine initial(pop,newpop,lowbound,upbound)
    use gene
    use parameter_globle
    implicit none 
    type(genechrome)                :: pop(nub+1)                                                
    type(genechrome)                :: newpop(nub+1)                                              
    integer                         :: i,j,k
    real,external                   :: randval,uniform 
    real                            :: lowbound(vars),upbound(vars)
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%initialize the population%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    call RANDOM_SEED()
    do i=1,vars
         do j = 1,nub
             pop(j)%fitness  = 0
             pop(j)%rfitness = 0
             pop(j)%lower(i) = lowbound(i)
             pop(j)%upper(i) = upbound(i)
!             pop(j)%gene(i)  = randval(pop(j)%upper(i),pop(j)%lower(i))            !! random coding 
             pop(j)%gene(i)  = uniform(pop(j)%upper(i),pop(j)%lower(i),j,nub)       !! evenly coding
         end do 
    end do
    end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%random coding%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    real function randval(upper,lower)
        implicit none
        real  :: upper,lower
        real  :: val
        real  :: rand
        call RANDOM_NUMBER(rand)
        val = rand*(upper-lower)+lower
        randval = val
        return
    end 
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%evenly coding%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    real function uniform(upper,lower,j,nub)
        implicit none
        real  :: upper,lower
        real  :: val
        real  :: l
        integer :: i,j,nub

        l = upper - lower
        l = l/nub
        i = j -1
        uniform = lower + i * l
        return
    end
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%