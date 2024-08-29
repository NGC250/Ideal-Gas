program Main
	
	use constants
	use dynamics
	
	implicit none
	
	double precision , dimension(N,2) :: positions
	double precision , dimension(N,2) :: velocities
	
	integer :: i , t , iostat
	integer :: unit_num = 10
	character(len=256) :: filename
	
	call InitialCondition(positions , velocities)
	
	time_evolution: do t=1,iterations
	
		write(filename , "(A,I3.3,A)") "../data/particle_status" , t , ".dat"
		open (unit = unit_num , file = filename , status="unknown" , iostat = iostat)
		
		if(iostat /= 0) then
			write(* , "(A,I3.3)") "Unable to write file " , t
			exit time_evolution
		end if
		
		write(unit_num , "(A,', ',A,', ',A,', ',A)") "x-position" , "y-position" , "x-velocity" , "y-velocity" 
		
		call update(positions , velocities) ! Entire time evolution is encoded in this call statement
		
		writing_loop: do i = 1,N
			write(unit_num , "(F12.6,', ',F12.6,', ',F12.6,', ',F12.6)") positions(i,1) , positions(i,2) , velocities(i,1) , velocities(i,2)
		end do writing_loop
		
		close(unit_num)
		
	end do time_evolution

end program Main