        program pi_mp
        !use params
        use OMP_LIB
        implicit none
        integer, parameter  :: dp = selected_real_kind(19,307)
        real(dp) :: j
        real(dp) :: step_size, pi, sum_pi, x
        real(8)  :: tdata
        integer  :: id, nthreads, i
        integer, parameter :: numsteps = 80000000
        step_size = 1._dp/numsteps
         
        print*, 'step size:', step_size
        pi= 0._dp
        tdata = omp_get_wtime()
!$omp parallel default(none) private(sum_pi,x,i,id,nthreads) shared(pi,step_size)
        id = omp_get_thread_num()
		print*,id
        nthreads = omp_get_num_threads()
  		if(id == 0) print*,nthreads
        sum_pi = 0._dp
        do i=id,numsteps,nthreads!while(i<numsteps)

                x=(real(i,dp)+0.5_dp)*step_size
                sum_pi = sum_pi + 4._dp/(x*x+1._dp)

		end do
        !$omp critical
         pi = pi + sum_pi * step_size
        !$omp end critical
!$omp end parallel        
        tdata = omp_get_wtime() - tdata
        !write(*,'(A3,x,F20.19)')'pi:',pi
        write(*,*)'pi:',pi
		print*,'as compared to:'
        !write(*,'(F20.19)')atan(1.d0)*4.d0
		write(*,*)atan(1._dp)*4._dp
        print*,'in', tdata, 'secs'
        end program pi_mp