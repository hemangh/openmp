      module params
        use, intrinsic :: iso_fortran_env
        implicit none
        integer, parameter  :: sp = REAL32
        integer, parameter  :: dp = selected_real_kind(19,307)!REAL64
        integer, parameter  :: qp = REAL128
        real(dp), parameter :: numsteps = 40000000
      end module params

        program pi_mp
        use params
        use OMP_LIB
        implicit none
        
        real(dp) :: i,j
        real(dp) :: step_size, pi, sum_pi, x
        real(sp) :: tdata
		integer  :: id, nthreads
        step_size = 1._dp/numsteps
         
        print*, 'step size:', step_size
        
        tdata = omp_get_wtime()
		
        pi     = 0._dp		
		
!$omp parallel default(none) private(sum_pi,x,i,id,nthreads) shared(pi,step_size)
        id = omp_get_thread_num()
		print*,id
        nthreads = omp_get_num_threads()        

		sum_pi = 0._dp
		i = real(id,dp)*numsteps/nthreads
        do while(i<(real(id,dp)+1._dp)*numsteps/nthreads)
                i=i+1._dp
                x=(i-0.5_dp)*step_size
                sum_pi = sum_pi + 4._dp/(x*x+1._dp)
        end do
        !$omp critical
         pi = pi + sum_pi * step_size
        !$omp end critical
!$omp end parallel
		tdata = omp_get_wtime() - tdata
		
        print'(A3,x,F20.16,x,A15,F20.16)', 'pi:',pi,'as compared to:',atan(1._dp)*4._dp

		print*,'in', tdata, 'secs'
		
        end program pi_mp
