	subroutine hllSolver(uL,uR,yL,yR,yStar,sL,sR)
	implicit none
	real:: gravit
	real:: sL,sR,qL,qR,yStar,cR,cL,uL,uR,yL,yR

!f2py intent(in) uL
!f2py intent(in) uR
!f2py intent(in) yL
!f2py intent(in) yR
!f2py intent(out) yStar
!f2py intent(out) sL
!f2py intent(out) sR

	gravit = 9.81d0

	cL = sqrt(gravit*yL)
	cR = sqrt(gravit*yR)

	yStar = ((0.5d0*(cL+cR)+0.25d0*(uL-uR))**2)/gravit

	if(yStar .gt. yL)then
		qL = sqrt(0.5d0*((yStar+yL)*yStar/(yL**2))) 
	else
		qL=1.d0
	end if
	if(yStar .gt. yR)then
		qR = sqrt(0.5d0*((yStar+yR)*yStar/(yR**2))) 
	else
		qR=1.d0
	end if
	sL = uL - cL*qL
	sR = uR + cR*qR
	
	end subroutine hllSolver
