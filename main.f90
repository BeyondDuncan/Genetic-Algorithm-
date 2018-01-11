program main                                                                  !! Date: 10:17-Jan-8th-2017, by Moyu Deng At UESTC 
    use gene                                                                  !! Email: beyondduncan@foxmail.com 
    use parameter_globle
    implicit none                                   
type(genechrome) :: population(nub+1)
type(genechrome) :: newpopulation(nub+1)
integer          :: max_index                                                 !! the index of the biggest-fitness chromosome
integer          :: i,k 
integer,external :: selectmax
real             :: post_maxfitness                                           !! the max fitness of the post population
real,allocatable :: low_bound(:),up_bound(:)
real             :: lowbound(part),upbound(part)
real             :: l
generation      = 1
newaverage      = 0
average         = 0
post_maxfitness = 0

open(unit=10,file='information.txt')
    read(10,*) vars
    allocate(low_bound(vars))
    allocate(up_bound(vars))
     do i = 1,vars 
            read(10,*) low_bound(i),up_bound(i)                               !! read the bound of variable
    end do
    read(10,*) pro_cross                                                      !! read the probability of cross
    read(10,*) pro_mutation                                                   !! read the probability of mutation
    close(10)
    do k =1,nub+1
        allocate(population(k)%gene(vars))
        allocate(population(k)%upper(vars))
        allocate(population(k)%lower(vars))
    end do                                      
    call CPU_TIME(time1)
    l = up_bound(1)-low_bound(1)                                              !! up_bound(1),low_bound(1) is the max-difference bound of variable
    l = l/part
    do k = 1,part
        lowbound(k) = low_bound(1) + (k-1)*l
        upbound(k) = low_bound(1) + k*l
    end do
    do k = 1,part                                                             !! Partition GA
        low_bound(1) = lowbound(k)
        up_bound(1)  = upbound(k)
        call initial(population,newpopulation, low_bound, up_bound)
        call evaluate(population,population)                                  
        max_index               = selectmax(population)
        population(nub+1)       = population(max_index)
        newpopulation(nub+1)    = population(max_index)                             
        post_maxfitness         = population(max_index)%fitness               !! save the max fitness
        open(unit=20,file='result.txt')
        write(20,*)  'the',k,'district'
        do while(generation<max )
            if (generation < BgnSeparateN) then                               !! evolve to gather preliminarily
                call fitness_linear(population)
                call choose3(population,newpopulation)                        !! tournament selection
            else
                if(generation > 50 .and.(abs(newaverage - average) < dif .and. (newpopulation(nub+1)%fitness - post_maxfitness) < dif) ) then
                    call fitness_linear(population)
                    call choose1(population,newpopulation)                    !! tournament selection
                else
                    call choose2(population,newpopulation)                    !! elitist proportionate selection
                end if
            end if 
                call cross(newpopulation)                                     !! cross
                max_index = selectmax(newpopulation)                                
                newpopulation(nub+1) = newpopulation(max_index)
                call mutation(newpopulation)                                  !! mutation
                max_index = selectmax(newpopulation)
                newpopulation(nub+1) = newpopulation(max_index)
                newpopulation(nub)   = newpopulation(max_index)                      
                post_maxfitness      = newpopulation(max_index)%fitness
                average = newaverage
                call evaluate(newpopulation,population)
                call paixu(population)
                call paixu(newpopulation)
                write(20,*) generation,population(nub)%fitness,population(nub)%gene
                generation = generation+1
                if(generation == 50 .or. generation == 70 ) then
                    call catastrophe(population)                              !! catastrophe occur
                    call evaluate(population,population)
                end if
        end do
        write(*,*) k,'district, the elite is '
        do i =1,vars
            write(*,*) newpopulation(nub)%gene(i)
        end do
        write(*,*) 'fitness is'
        write(*,*) newpopulation(nub)%fitness
        generation = 1
    end do
    close(20)

    call CPU_TIME(time2)
    write(*,*) 'the global elite is '
    do i =1,vars
        write(*,*) newpopulation(nub)%gene(i)
    end do
    write(*,*) 'the fitness is'
    write(*,*) newpopulation(nub)%fitness
    write(*,*) 'time consuming'
    write(*,*) time2-time1
        end program main