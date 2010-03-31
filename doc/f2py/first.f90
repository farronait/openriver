module test
	
	contains 

	subroutine first(a,b,c)
	implicit none
	integer:: a,b,c,d,e

!f2py intent(in) a
!f2py intent(in) b
!f2py intent(out) c

	d = a+1
	e = b+2

	call multiplication(d,e,c)

	end subroutine first
	
	subroutine multiplication(a,b,c)
	implicit none
	integer:: a,b,c

!f2py intent(in) a
!f2py intent(in) b
!f2py intent(out) c

	c = a*b
	
	end subroutine

end module test
