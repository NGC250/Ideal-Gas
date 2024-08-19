module constants
	implicit none
	
	double precision , parameter :: pi = 4.0 * atan(1.0)
	
	double precision , parameter :: L = 100.0 !Length of the box
	double precision , parameter :: B = 100.0 !Breadth of the box
	
	integer , parameter :: N = 200 !Number of particles/molecules
	
	double precision , parameter :: tick = 0.1 !update rate
	integer , parameter :: iterations = 200
	
	double precision , parameter :: radius = 2.0 !entity size
	
	double precision , parameter :: v_initial = 100.0 !Maximum initial velocity of particles
	
end module constants