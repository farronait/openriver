!*******************************************************************************************
MODULE precisione
IMPLICIT NONE

INTEGER, PARAMETER	::rk	=SELECTED_REAL_KIND  (p =  12)	 ! 0.000000000001  = 12
INTEGER, PARAMETER	::ik	=SELECTED_INT_KIND   (r =  6)    ! 1000000         = 6	

END MODULE precisione
!*******************************************************************************************
MODULE comuni
USE precisione	
IMPLICIT NONE


 INTEGER   (kind=ik), PARAMETER   :: maxc              = 80_ik
 CHARACTER (maxc)		  :: nome, fiume, nulla, nomesez,testo
 INTEGER   (kind=ik)              :: indsez
 INTEGER   (kind=ik)              :: nscab
 INTEGER   (kind=ik)              :: nsezi
 REAL      (kind=rk)              :: idum
 INTEGER   (kind=ik), ALLOCATABLE :: ji2jj               (:)
 INTEGER   (kind=ik), ALLOCATABLE :: jj2ji               (:)
 REAL      (kind=rk), ALLOCATABLE :: noms                (:)
 REAL      (kind=rk), ALLOCATABLE :: si                  (:)
 INTEGER   (kind=ik), ALLOCATABLE :: nrci                (:)
 INTEGER   (kind=ik), ALLOCATABLE :: npi                 (:)
 INTEGER   (kind=ik), ALLOCATABLE :: flagi               (:)
 REAL      (kind=rk), ALLOCATABLE :: zargr               (:)
 REAL      (kind=rk), ALLOCATABLE :: zargl               (:)
 INTEGER   , ALLOCATABLE 	  :: ibw_sx              (:)
 INTEGER   , ALLOCATABLE          :: ibw_dx              (:)
 REAL      (kind=rk), ALLOCATABLE :: yi                  (:,:)
 REAL      (kind=rk), ALLOCATABLE :: zi                  (:,:)
 REAL      (kind=rk), ALLOCATABLE :: rks                 (:,:)
 INTEGER   (kind=ik)              :: pt_max
 REAL      (kind=rk), ALLOCATABLE :: dx                  (:)
 


END MODULE comuni
!##############################################################################################################################
SUBROUTINE readingsection
USE comuni
IMPLICIT NONE

INTEGER (kind=ik)              :: ji,jj,j,ierror,iostat

WRITE(*,*) 'Write the name of the file to be converted'
READ(*,*) nomesez
nome=TRIM(TRIM(nomesez)//".di")  

OPEN (UNIT=1,FILE=nome,STATUS='old',ACTION='read',IOSTAT=ierror)

READ(1,'(A)')  fiume
READ(1,*)      indsez 
READ(1,*)      nscab,nsezi,idum,idum,idum

 CALL alloca

ji=1
DO WHILE (ji <= nsezi)
	  
   READ(1,*)jj   !number of the section, 0 id interpolated section   
   ji2jj(ji)=jj
   IF(jj.ne.0)THEN
      READ(1,'(a)') noms(jj)     !ID of the section
      READ(1,*)     jj2ji(jj)    !identification number
   ENDIF
	   
   READ(1,*) si(ji),nrci(ji),npi(ji),flagi(ji)
   npi(ji)=npi(ji)
      
   IF(flagi(ji) == 0)THEN
      CALL read_sez(ji)
   ENDIF

   ji=ji+1
ENDDO
CLOSE(1)

DO j = 1,nsezi-1 
    dx(j) = si(j+1)-si(j)
ENDDO


RETURN
END SUBROUTINE readingsection
!##############################################################################################################################
SUBROUTINE read_sez(ji)
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik):: npp,ji,k,j
npp= npi(ji) 

READ(1,*) nulla,zargl(ji),zargr(ji),ibw_sx(ji),ibw_dx(ji)

DO k=1,npp
 
   READ(1,*)yi(k,ji),zi(k,ji),rks(k,ji)  
   
ENDDO
END SUBROUTINE read_sez
!##############################################################################################################################
SUBROUTINE alloca
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: iostat,i

ALLOCATE (ji2jj				(nsezi)					, stat = iostat)
ALLOCATE (jj2ji				(nsezi)					, stat = iostat)
ALLOCATE (noms				(nsezi)					, stat = iostat)
ALLOCATE (si				(nsezi)					, stat = iostat)
ALLOCATE (nrci				(nsezi)					, stat = iostat)
ALLOCATE (npi				(nsezi)					, stat = iostat)
ALLOCATE (flagi				(nsezi)					, stat = iostat)
ALLOCATE (zargl				(nsezi)					, stat = iostat)
ALLOCATE (zargr				(nsezi)					, stat = iostat)
ALLOCATE (ibw_sx			(nsezi)					, stat = iostat)
ALLOCATE (ibw_dx			(nsezi)					, stat = iostat)
ALLOCATE (yi				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (zi				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (rks				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (dx				(nsezi)					, stat = iostat)


RETURN
END SUBROUTINE alloca
!##############################################################################################################################
SUBROUTINE writingsection
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: ji, k


OPEN (UNIT=2,FILE='sections.ori',STATUS='unknown',ACTION='write')
WRITE(2,'(i4)') nsezi

DO ji=1,nsezi
WRITE(testo,'(i4.4)') ji
WRITE(2,'(i4,1x,a8)') npi(ji), trim("sez"//testo)
WRITE(2,'(2(i3,1x,f10.5,1x))') ibw_sx(ji), zargl(ji), ibw_dx(ji), zargr(ji)
END DO

CLOSE(2)

OPEN (UNIT=3,FILE='points.ori',STATUS='unknown',ACTION='write')

DO ji=1,nsezi
	DO k=1,npi(ji)
	WRITE(3,'(4(f10.5),1x)') si(ji), yi(k,ji),zi(k,ji),rks(k,ji)  
	END DO
END DO

CLOSE(3)

RETURN
END SUBROUTINE writingsection
!##############################################################################################################################
PROGRAM geom_converter
! Geometry conversion program
  USE comuni
  IMPLICIT NONE


  pt_max=300

  CALL readingsection

  CALL writingsection


END PROGRAM geom_converter


