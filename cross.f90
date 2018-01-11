subroutine cross(newpop)                                      !! cross the chromosome   
    use gene                                                  !! choose two chromosome randomly and then cross,
    implicit none                                             !! notice that cross the same point gene of two 
    type(genechrome)    :: newpop(nub)
    integer             :: i,j
    integer             :: node(2)                            !! the index of the two cross chromosome
    real                :: b                                  !! random numbers (0 - 1)
    integer,external    :: random2
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    do i =1,nub
        call RANDOM_NUMBER(b)
        if (pro_cross>b) then
            do j = 1,2
                node(j) = random2(1,nub)                     
            end do
            do j = 1,vars
                newpop(node(1))%gene(j)=newpop(node(2))%gene(j)*cross_rate+(1-cross_rate)*newpop(node(1))%gene(j)
                newpop(node(2))%gene(j)=newpop(node(1))%gene(j)*cross_rate+(1-cross_rate)*newpop(node(2))%gene(j)
            end do
            call update_fitness(newpop(node(1)),node(1))
            call update_fitness(newpop(node(2)),node(2))
        end if 
    end do 
  
    end subroutine
   
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%