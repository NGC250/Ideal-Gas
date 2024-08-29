subroutine InitialCondition(initial_positions , initial_velocities)
	
	use constants
	
	implicit none
	
	double precision , dimension(N,2) , intent(inout) :: initial_positions
	double precision , dimension(N,2) , intent(inout) :: initial_velocities
	
	call random_number(initial_positions)
	
	initial_positions(:,1) = initial_positions(:,1) * (L-1) + 1
	initial_positions(:,2) = initial_positions(:,2) * (B-1) + 1
	
	!Random initial velocity of particles
	! call random_number(initial_velocities)
	! initial_velocities = initial_velocities * (2 * v_max) - v_max
	
	!Only one particle given a nudge for a more pronounced change in the distribution function
	initial_velocities = 0.0
	initial_velocities(75,1) = v_initial
	initial_velocities(75,2) = 0.0
	
end subroutine InitialCondition