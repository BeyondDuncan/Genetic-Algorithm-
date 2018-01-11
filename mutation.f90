subroutine mutation(newpop)                             !! mutation
    use gene
    implicit none
    type(genechrome) :: newpop(nub)
    integer          :: i,j,k
    real             :: b1,b2                           !! random numbers  
    integer          :: r
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    call RANDOM_SEED()
    do i = 1, nub                                   
        call RANDOM_NUMBER(b1)
        if ( pro_mutation > b1) then
            do j = 1,vars                               
                call RANDOM_NUMBER(b2) 
                b2 = b2*10
                r = floor(b2)
                if (mod(r,2)==0) then
                    call RANDOM_NUMBER(b2)
                    newpop(i)%gene(j) = newpop(i)%gene(j) + p*(newpop(i)%upper(j)-newpop(i)%gene(j))*b2
                    if(newpop(i)%gene(j) > newpop(i)%upper(j)) then
                        newpop(i)%gene(j) = newpop(i)%lower(j)
                    else if(newpop(i)%gene(j) < newpop(i)%upper(j))  then
                        newpop(i)%gene(j) = newpop(i)%upper(j)
                    end if  
                else 
                    call RANDOM_NUMBER(b2)
                    newpop(i)%gene(j) = newpop(i)%gene(j) - p*(newpop(i)%gene(j)-newpop(i)%lower(j))*b2    
                    if(newpop(i)%gene(j) > newpop(i)%upper(j)) then
                        newpop(i)%gene(j) = newpop(i)%lower(j)
                    else if(newpop(i)%gene(j) < newpop(i)%upper(j))  then
                        newpop(i)%gene(j) = newpop(i)%upper(j)
                    end if  
                end if 
            end do
            call update_fitness(newpop(i),i)
        end if 
    end do
    end 
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    