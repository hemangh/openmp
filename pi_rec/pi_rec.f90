module pi_calc
use OMP_LIB
implicit none

integer, parameter :: num_steps = 1024*1024*1024!100000000
integer, parameter :: min_blk   = 1024*1024*256!10000000

contains

recursive function pi_comp(nstart, nfinish, step)result(sum)

integer :: nstart, nfinish, i, iblk
real(8) :: step
real(8) :: sum, sum1, sum2, x

sum = 0.d0

if(nfinish - nstart < min_blk)then

   do i = nstart, nfinish
     
     x = (i + 0.5d0)*step
     sum = sum + 4.d0/(x*x + 1.d0)  
   
   end do
   
else
    
    iblk = nfinish -nstart
     
     !$omp task shared(sum1)
       sum1 = pi_comp(nstart, nfinish - iblk/2, step)
     !$omp end task
     
     !$omp task shared(sum2)
        sum2 = pi_comp(nfinish-iblk/2, nfinish, step)
     !$omp end task
    
     !$omp taskwait
        sum = sum1+ sum2
     
     
endif

end function pi_comp

end module

program main
use pi_calc
use OMP_LIB
implicit none


real(8)::step, pi, sum, init_time, final_time
integer:: i

step = 1.d0/num_steps

       init_time = OMP_GET_WTIME()
!$omp parallel
    !$omp single
        sum = pi_comp(0, num_steps, step)
    !$omp end single
!$omp end parallel
pi = sum * step

       final_time = OMP_GET_WTIME() - init_time

       WRITE(*,100) num_steps, pi, final_time 
100    FORMAT(' for ', i14,' steps, pi = ', f15.8,' in ', f8.3,' secs')

end program main