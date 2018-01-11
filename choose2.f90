subroutine choose2(pop,newpop)                                   !! elitist proportionate selection
    use gene
    implicit none
    type(genechrome)    :: pop(nub+1)
    type(genechrome)    :: newpop(nub+1)
    integer             :: i,j,k,m ,temp
    real                :: n
    real                :: sum
    real                :: b(nub)                                !! random numbers  (0 - 1)
    integer             :: times(nub)                            !! the choosen times of the chromosome
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

     k = 0
     temp = mod(nub,2)
     if( temp == 0 ) then
         m = nub/4
        do i = nub,3*m+1,-1
               do j = 1,2
                   k = k + 1
                   newpop(k) = pop(i)
               end do 
        end do
        do i =3*m, m,-1
            newpop(k) = pop(i)
        end do
     else
          n = nub/4
          m = floor(n)
          do i = nub,4*m,-1
              do j = 1,2
                  k = k + 1
                  newpop(k) = pop(i)
              end do
          end do
          do i = 4*m-1,m,-1
              k = k + 1
              newpop(k) = pop(i)
          end do
     end if
     call paixu(pop)
     newpop(nub) = pop(nub)
    end subroutine
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%