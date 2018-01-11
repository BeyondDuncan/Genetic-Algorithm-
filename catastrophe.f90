subroutine catastrophe(population)                            !! catastrophe -- simulate the phenomenon of the nature
    use gene                                                  !! recoding all the current chromosome, except the elite 
    implicit none                                               
    type(genechrome)    :: population(nub)                               
    integer             :: i,j
    real,external       :: randval
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    do i=1,vars
         do j = 1,nub
             population(j)%gene(i)  = randval(population(j)%upper(i),population(j)%lower(i))
         end do
    end do
    end subroutine
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%