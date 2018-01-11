subroutine update_fitness(pop,index)                      !!  update the chromsome's fitness 
    use gene
    implicit none
    integer             ::    index                       !! the index of the chromsome whose fitness will renew 
    type(genechrome)    ::    pop
    real                ::    x(vars)
    integer             ::    i,j,k
    real,external       ::    func

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do i = 1,vars
    x(i)=pop%gene(i)
end do

pop%fitness = func(x)

    end subroutine
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%