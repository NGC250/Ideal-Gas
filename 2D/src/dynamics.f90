module dynamics

	use constants
	
	implicit none
	
	contains
		
		subroutine collision(positions , velocities)
			implicit none
			
			double precision , dimension(N,2) , intent(inout) :: positions
			double precision , dimension(N,2) , intent(inout) :: velocities
			
			double precision , dimension(iterations) :: distance_history
			
			double precision :: distance , update_distance , update_x_1 , update_x_2 , update_y_1 , update_y_2
			double precision ,  dimension(2) :: exchange_1 , exchange_2 , v_diff , pos_diff , projection
			
			integer :: i , j , k
			
			wall_collision: do k = 1,N
				
				if ((positions(k,1) < radius) .and. (velocities(k,1) < 0.0)) then
					velocities(k,1) = -1 * velocities(k,1)

				else if ((positions(k,1) > L-radius) .and. (velocities(k,1) > 0.0)) then
					velocities(k,1) = -1 * velocities(k,1)
				end if
				
				if ((positions(k,2) < radius) .and. (velocities(k,2) < 0.0)) then
					velocities(k,2) = -1 * velocities(k,2)
					
				else if ((positions(k,2) > B-radius) .and. (velocities(k,2) > 0.0)) then
					velocities(k,2) = -1 * velocities(k,2)
				end if
				
			end do wall_collision
			
			! particle collision
			particle1: do i = 1,(N-1)
				particle2: do j = (i+1),N
					
					distance = sqrt((positions(i,1) - positions(j,1))**2 + (positions(i,2) - positions(j,2))**2)
					
					!Figuring out whether the particles are moving away from each other or not
					update_x_1 = positions(i,1) + velocities(i,1) * tick * 0.5 !0.5 is chosen arbitrarily. In theory, the number can be arbitrarily small(cont.)
					update_y_1 = positions(i,2) + velocities(i,2) * tick * 0.5 !as long as we get the information about whether the particles are moving towards each other.
					
					update_x_2 = positions(j,1) + velocities(j,1) * tick * 0.5
					update_y_2 = positions(j,2) + velocities(j,2) * tick * 0.5
					
					update_distance = sqrt((update_x_1 - update_x_2)**2 + (update_y_1 - update_y_2)**2)
					!-------------------------------------------------------------------------
					
					if ((update_distance < distance) .and. (distance <= 2 * radius)) then
						
						v_diff = velocities(i,:) - velocities(j,:)
						pos_diff = positions(i,:) - positions(j,:)
						
						projection = (dot_product(v_diff , pos_diff)/dot_product(pos_diff , pos_diff)) * pos_diff
						
						exchange_1 = projection + velocities(j,:)
						exchange_2 = -projection + velocities(i,:)
						
						velocities(i,:) = exchange_1
						velocities(j,:) = exchange_2
						
					end if
					
				end do particle2
			end do particle1
		
		end subroutine collision
		
		subroutine update(positions , velocities)
			implicit none
			
			double precision , dimension(N,2) , intent(inout) :: positions
			double precision , dimension(N,2) , intent(inout) :: velocities
			double precision , dimension(N,2) :: update_positions
			
			integer :: i
			
			update_positions = positions + velocities * tick
			
			call collision(update_positions , velocities)
			
			positions = update_positions
			
		end subroutine update
	
end module dynamics