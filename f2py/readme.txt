Here you find 2 examples of fortran wrapping by f2py.
Examples were implemented using http://cens.ioc.ee/projects/f2py2e/usersguide/index.html
as literature. They may contain errors, if you find some just correct them!.

-----------------------------------------------------
-------------  Example 1 ----------------------------
-----------------------------------------------------
We have a subroutine in file hllSolver.f90 where the HLL approximate
Riemann Problem Solver is implemented.

To the original fortran file we added lines:

!f2py intent(in) uL
!f2py intent(in) uR
!f2py intent(in) yL
!f2py intent(in) yR
!f2py intent(out) yStar
!f2py intent(out) sL
!f2py intent(out) sR

In order to tell f2py which are i/o variables.

We then build the module:

$ f2py -c -m hllSolver hllSolver.f90

And hllSolver.so is generated.

In a python terminal (idle or some other):

>>> import hllSolver

We can see documentation of our function by:

>>> print hllSolver.hllsolver.__doc__
hllsolver - Function signature:
  ystar,sl,sr = hllsolver(ul,ur,yl,yr)
Required arguments:
  ul : input float
  ur : input float
  yl : input float
  yr : input float
Return objects:
  ystar : float
  sl : float
  sr : float
  
  And use it:

>>> yStar,sL,sR = hllSolver.hllsolver(uL,uR,yL,yR)

If uL=uR=0. and yL=yR=1.

We get the trivial solution:

>>> 1.0 -3.13209199905 3.13209199905 # for yStar, sL and sR

-------------------------------------------------------
---------------------  Example 2  ---------------------
-------------------------------------------------------
What if we have more subroutines, some of them nested for instance.
Not a problem at all, we include then in a module.
take into consideration file first.f90. We have a subroutine "first" that operates changes
on input variables and then passes them to subroutine "multiplication"
We just put both into a module (we could also define global variables) as in file first.f90.

We build the module:

$ f2py -c -m first first.f90

And the module is ready to be used:

>>> import first
>>> print first.test.__doc__
first - Function signature:
  c = first(a,b)
Required arguments:
  a : input int
  b : input int
Return objects:
  c : int
multiplication - Function signature:
  c = multiplication(a,b)
Required arguments:
  a : input int
  b : input int
Return objects:
  c : int
>>> first.test.first(1,2)
8

  


