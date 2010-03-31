
!*******************************************************************************************
MODULE precisione
IMPLICIT NONE

INTEGER, PARAMETER	::rk	=SELECTED_REAL_KIND  (p =  12)	 ! 0.000000000001  = 12
INTEGER, PARAMETER	::ik	=SELECTED_INT_KIND   (r =  6)    ! 1000000         = 6	

END MODULE precisione
!*******************************************************************************************
!*******************************************************************************************
MODULE comuni
USE precisione	
IMPLICIT NONE

!parametri del codice
CHARACTER(255)          	 :: path, path_in
INTEGER   (kind=ik), PARAMETER   :: maxc              = 80_ik
CHARACTER (maxc)				 :: nome
INTEGER   (kind=ik), PARAMETER   :: maxidr            = 1500_ik
REAL      (kind=rk), PARAMETER   :: gi                = 9.81_rk
REAL      (kind=rk), PARAMETER   :: rhow              = 1000.0_rk
REAL      (kind=rk), PARAMETER   :: Cp                = 4186.0_rk
REAL      (kind=ik), PARAMETER   :: delta_z           = 1.e-10
REAL      (kind=ik), PARAMETER   :: st	              = 7.0_rk/3.0_rk
REAL      (kind=ik), PARAMETER   :: ct	              = 5.0_rk/3.0_rk
REAL      (kind=ik), PARAMETER   :: ut	              = 1.0_rk/3.0_rk                 
CHARACTER (maxc)				 :: nulla
REAL      (kind=rk)              :: Q_input
INTEGER   (kind=ik)              :: nn,itout,itout_sez
INTEGER   (kind=ik)				 :: controllo

!file input "modeldata.ori"					        parametri simulazione

REAL      (kind=rk)				 :: cflcoe
REAL      (kind=rk)				 :: dstar_m
REAL      (kind=rk)				 :: dstar_v
INTEGER   (kind=ik)              :: ifilein
CHARACTER (maxc)				 :: nomesez
INTEGER   (kind=ik)              :: pt_max
REAL      (kind=rk)				 :: qstar,Taria_star,netsolar_star,wind_vel_star,wet_rel_star
REAL      (kind=rk)				 :: t0
REAL      (kind=rk)				 :: timeout
INTEGER   (kind=ik)              :: usaidr
INTEGER   (kind=ik)              :: usaterm
INTEGER   (kind=ik)              :: limite
INTEGER   (kind=ik)              :: ntmaxi
REAL      (kind=rk)              :: ks_alveo
REAL      (kind=rk)              :: ks_isola
REAL      (kind=rk)              :: ks_arboreo
REAL      (kind=rk)              :: ks_erbaceo


!caratteristiche geometriche dell'intero tratto
CHARACTER (maxc)				 :: fiume

INTEGER   (kind=ik)              :: indsez
INTEGER   (kind=ik)              :: nscab
INTEGER   (kind=ik)              :: nsezi
INTEGER   (kind=ik)              :: icont_sou_T

REAL      (kind=rk)              :: idum
REAL      (kind=rk)              :: ifo
!caratteristiche geometriche della sezione
INTEGER   (kind=ik), ALLOCATABLE ::ji2jj               (:)
INTEGER   (kind=ik), ALLOCATABLE ::jj2ji               (:)
INTEGER   (kind=ik), ALLOCATABLE ::nrci                (:)
INTEGER   (kind=ik), ALLOCATABLE ::npi                 (:)
INTEGER   (kind=ik), ALLOCATABLE ::flagi               (:)
INTEGER   (kind=ik), ALLOCATABLE ::ibw_sx              (:)
INTEGER   (kind=ik), ALLOCATABLE ::ibw_dx              (:)

CHARACTER(maxc), ALLOCATABLE ::noms                (:)
REAL      (kind=rk), ALLOCATABLE ::si                  (:)
REAL      (kind=rk), ALLOCATABLE ::e                   (:)
REAL      (kind=rk), ALLOCATABLE ::h                   (:)
REAL      (kind=rk), ALLOCATABLE ::zfmaxi              (:)
REAL      (kind=rk), ALLOCATABLE ::xi                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::yi                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::zi                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::rks                 (:,:)
REAL      (kind=rk), ALLOCATABLE ::zargr               (:)
REAL      (kind=rk), ALLOCATABLE ::zargl               (:)
REAL      (kind=rk), ALLOCATABLE ::dx                  (:)
REAL      (kind=rk), ALLOCATABLE ::ksort               (:,:)
REAL      (kind=rk), ALLOCATABLE ::etai                (:,:)
REAL      (kind=rk), ALLOCATABLE ::sigm                (:,:)
REAL      (kind=rk), ALLOCATABLE ::omi                 (:,:)

REAL      (kind=rk), ALLOCATABLE ::ca                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::j1i				   (:,:)

             
! caratteristiche temperatura
REAL      (kind=rk)              :: Temp_cost
!caratteristiche moto della sezione
REAL      (kind=rk)              ::time,hstar    !HSTAR PROVVISORIA                      
REAL      (kind=rk), ALLOCATABLE ::q                   (:)
REAL      (kind=rk), ALLOCATABLE ::d                   (:)
REAL      (kind=rk), ALLOCATABLE ::A                   (:)
REAL      (kind=rk), ALLOCATABLE ::ATemp                (:)
REAL      (kind=rk), ALLOCATABLE ::Temp                (:)
REAL      (kind=rk), ALLOCATABLE ::Fd                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::FLUXL                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::FLUXM                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::FLUXR                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::STATE                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::STATE_L                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::STATE_R                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::STATE_M                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::Cs                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::CS_star                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::Fs                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::WJ                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::WS                  (:,:)
REAL      (kind=rk), ALLOCATABLE ::C                   (:)
REAL      (kind=rk), ALLOCATABLE ::U                   (:)
REAL      (kind=rk), ALLOCATABLE ::CN                  (:)
REAL      (kind=rk), ALLOCATABLE ::FLUX                (:,:)
REAL      (kind=rk), ALLOCATABLE ::sou				   (:,:)
REAL      (kind=rk), ALLOCATABLE ::FSL				   (:,:)
REAL      (kind=rk), ALLOCATABLE ::FSR				   (:,:)
!------------------------------------------------------------------------------------------
!caratteristiche della simulazione
REAL      (kind=rk)              ::dtime
!------------------------------------------------------------------------------------------
!caratteristiche degli idrogrammi
INTEGER   (kind=ik)              ::idr_campi
INTEGER   (kind=ik)              ::idr_campi_aff
INTEGER   (kind=ik)              ::meteo_campi
INTEGER   (kind=ik)              ::totale_affluenti    
CHARACTER (maxc)   , ALLOCATABLE ::nome_affluente                         (:)
CHARACTER (maxc)   , ALLOCATABLE ::nome_sensore                           (:)
INTEGER   (kind=ik), ALLOCATABLE ::indice_sez_affluente                   (:)
INTEGER   (kind=ik), ALLOCATABLE ::indice_sez_sensore                     (:)
REAL      (kind=rk), ALLOCATABLE ::td(:)
REAL      (kind=rk), ALLOCATABLE ::qd(:)
REAL      (kind=rk), ALLOCATABLE ::tmeteo(:),Taria(:),netsolar(:),wind_vel(:),wet_rel(:)
REAL      (kind=rk), ALLOCATABLE ::td_aff(:,:)
REAL      (kind=rk), ALLOCATABLE ::qd_aff(:,:)
REAL      (kind=rk), ALLOCATABLE ::qaff(:)

INTEGER   (kind=ik)              ::temp_campi
INTEGER   (kind=ik)              ::temp_campi_aff
REAL      (kind=rk)		 ::temp_star
REAL      (kind=rk), ALLOCATABLE ::td_temp(:)
REAL      (kind=rk), ALLOCATABLE ::tempd(:)
REAL      (kind=rk), ALLOCATABLE ::tempaff(:)
REAL      (kind=rk), ALLOCATABLE ::td_aff_temp(:,:)
REAL      (kind=rk), ALLOCATABLE ::tempd_aff(:,:)

INTEGER   (kind=ik)              ::sensori								

END MODULE comuni
!*******************************************************************************************
SUBROUTINE reader

!C     Purpose: leggere i parametri iniziali del problema

USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: ierror
!------------------------------------------------------------------------

OPEN (UNIT=47,FILE='../path_conf',STATUS='old',ACTION='read',IOSTAT=ierror)
READ(47,*) path_in
CLOSE(47)

WRITE(path,'(2a)') trim(path_in),'/modeldata.ori'

OPEN (UNIT=11,FILE=trim(path),STATUS='old',ACTION='read',IOSTAT=ierror)
READ (11,'(a)')  nomesez
READ (11,*)		ifilein
READ (11,*)		cflcoe
READ (11,*)		timeout
READ (11,*)		itout
READ (11,*)		itout_sez
READ (11,*)		dstar_m
READ (11,*)		dstar_v
READ (11,*)		qstar
READ (11,*)		usaidr
READ (11,*)		t0
READ (11,*)		pt_max
READ (11,*)		limite
READ (11,*)		ntmaxi
READ (11,*)		ks_alveo
READ (11,*)		ks_isola
READ (11,*)		ks_arboreo
READ (11,*)		ks_erbaceo
CLOSE(UNIT=1)

	
WRITE (*,*) 'nomesez =',nomesez
WRITE (*,*)	'ifilein =',ifilein
WRITE (*,*)	'cflcoe  =',cflcoe
WRITE (*,*)	'timeout  =',timeout
WRITE (*,*)	'dstar_m =',dstar_m
WRITE (*,*)	'dstar_v =',dstar_v
WRITE (*,*)	'qstar   =',qstar
WRITE (*,*)	'usaidr  =',usaidr
WRITE (*,*)	't0	     =',t0
WRITE (*,*)	'pt_max  =',pt_max
WRITE (*,*)	'limite =',limite
WRITE (*,*)	'ntmaxi =',ntmaxi
WRITE (*,*)	'ks_alveo =',ks_alveo
WRITE (*,*)	'ks_isola  =',ks_isola
WRITE (*,*)	'ks_arboreo =',ks_arboreo
WRITE (*,*)	'ks_erbaceo =',ks_erbaceo


RETURN
END SUBROUTINE reader
!----------------------------------------------------
SUBROUTINE read_sez(ji)
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik):: npp,ji,k,j
npp= npi(ji) 
e(ji)= 1.E36 
zfmaxi(ji)=-1.E+36

READ(2,*) nulla,zargl(ji),zargr(ji),ibw_sx(ji),ibw_dx(ji)

DO k=2,npp-1
 
   READ(2,*)yi(k,ji),zi(k,ji),rks(k,ji)  
   e(ji)=MIN(e(ji),zi(k,ji))  !trovo il minimo
   zfmaxi(ji)=MAX(zfmaxi(ji),zi(k,ji)) !trovo il massimo 
   
   ! ATTENZIONE
   IF (rks(k,ji)==1.) THEN
   rks(k,ji)=ks_alveo
   ELSE IF (rks(k,ji)==3.) THEN
   rks(k,ji)=ks_isola
   ELSE IF (rks(k,ji)==5.) THEN
   rks(k,ji)=ks_arboreo
   ELSE IF (rks(k,ji)==7.) THEN
   rks(k,ji)=ks_erbaceo
   ENDIF
   ! ATTENZIONE  
ENDDO





END SUBROUTINE read_sez
!----------------------------------------------------------------------------
SUBROUTINE alloca

!C		purpose: allocare le variabili ed inizializzarle

USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: iostat,i,k

!C	Alloco

ALLOCATE (ji2jj				(nsezi)					, stat = iostat)
ALLOCATE (noms				(nsezi)					, stat = iostat)
ALLOCATE (jj2ji				(nsezi)					, stat = iostat)
ALLOCATE (si				(nsezi)					, stat = iostat)
ALLOCATE (nrci				(nsezi)					, stat = iostat)			
ALLOCATE (npi				(nsezi)					, stat = iostat)
ALLOCATE (h					(nsezi)					, stat = iostat)
ALLOCATE (flagi				(nsezi)					, stat = iostat)
ALLOCATE (e					(nsezi)					, stat = iostat)
ALLOCATE (zfmaxi			(nsezi)					, stat = iostat)
ALLOCATE (zargl				(nsezi)					, stat = iostat)
ALLOCATE (zargr				(nsezi)					, stat = iostat)
ALLOCATE (ibw_sx			(nsezi)					, stat = iostat)
ALLOCATE (ibw_dx			(nsezi)					, stat = iostat)
ALLOCATE (xi				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (yi				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (zi				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (rks				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (dx				(nsezi)					, stat = iostat)
ALLOCATE (ksort				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (etai				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (sigm				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (d					(nsezi)					, stat = iostat)
ALLOCATE (q					(nsezi)					, stat = iostat)
ALLOCATE (A					(nsezi)					, stat = iostat)
ALLOCATE (ATemp				(nsezi)					, stat = iostat)
ALLOCATE (Temp				(nsezi)					, stat = iostat)
ALLOCATE (omi				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (j1i				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (ca				(pt_max,nsezi)			, stat = iostat)
ALLOCATE (sou				(3,     nsezi)			, stat = iostat)
!C variabili implementate per il WAF
ALLOCATE (Fd(3,nsezi), stat = iostat)
ALLOCATE (FLUXL	(3,nsezi), stat = iostat)
ALLOCATE (FLUXM	(3,nsezi), stat = iostat)
ALLOCATE (FLUXR	(3,nsezi), stat = iostat)
ALLOCATE (STATE(3,nsezi) , stat = iostat)
ALLOCATE (STATE_L(3,nsezi) , stat = iostat)
ALLOCATE (STATE_R(3,nsezi) , stat = iostat)
ALLOCATE (STATE_M(3,nsezi) , stat = iostat)
ALLOCATE (CS_star(3,nsezi) , stat = iostat)
ALLOCATE (Cs(3,nsezi) , stat = iostat)	
ALLOCATE (Fs(3,nsezi), stat = iostat)
ALLOCATE (WS				(3,nsezi)				, stat = iostat)
ALLOCATE (WJ				(3,nsezi)				, stat = iostat)
ALLOCATE (FLUX				(3,nsezi)				, stat = iostat)
ALLOCATE (C					(nsezi)					, stat = iostat)
ALLOCATE (U					(nsezi)					, stat = iostat)
ALLOCATE (CN				(3)						, stat = iostat)
ALLOCATE (FSL				(2,nsezi)				, stat = iostat)
ALLOCATE (FSR				(2,nsezi)				, stat = iostat)
ALLOCATE (qaff				(nsezi)					, stat = iostat)
ALLOCATE (tempaff			(nsezi)					, stat = iostat)

IF (iostat/=0) THEN
     WRITE(*,*) "Errore allocazione"
     STOP
ENDIF

!C Inizializzo

DO i=1,nsezi
   ji2jj(i)		= 0
   jj2ji(i)		= 0
  ! noms(i)		= 0
   si(i)		= 0._rk
   nrci(i)		= 0
   npi(i)		= 0
   flagi(i)		= 0
   e(i)			= 0._rk
   h(i)			= 0._rk
   zfmaxi(i)	= 0._rk
   zargr(i)		= 0._rk
   zargl(i)		= 0._rk
   ibw_sx(i)	= 0._rk
   ibw_dx(i)	= 0._rk
   dx(i)		= 0._rk
   C(i)			= 0._rk
   U(i)			= 0._rk
   A(i)			= 0._rk
   Q(i)			= 0._rk
   ATemp(i)		= 0._rk
   Temp(i)		= 0._rk
ENDDO

DO i=1,nsezi
   DO k=1,pt_max
      j1i   (k,i)   = 0._rk
      ca    (k,i)   = 0._rk
   ENDDO
ENDDO

DO i=1,2
   DO k=1,nsezi
      Fd(i,k)		=0._rk
      FLUXL(i,k)	=0._rk
      FLUXR(i,k)	=0._rk
      FLUXM(i,k)	=0._rk
      STATE(i,k)=0._rk
      STATE_L(i,k)=0._rk
      STATE_R(i,k)=0._rk
      STATE_M(i,k)=0._rk
      Cs(i,k)=0._rk
      CS_star(i,k)=0._rk
      Fs(i,k)		=0._rk
      WS(i,k)		=0._rk
      WJ(i,k)		=0._rk
      FLUX(i,k)		=0._rk
      sou (i,k)		=0._rk
      FSL(i,k)		=0._rk
      FSR(i,k)		=0._rk
   ENDDO
ENDDO

DO i=1,3
   DO k=1,nsezi
   Fd(i,k)=0._rk
   ENDDO
ENDDO


DO i=1,2
   CN(i) = 0.
ENDDO


RETURN
END SUBROUTINE alloca
!----------------------------------------------------------------
SUBROUTINE letturasez
USE comuni
IMPLICIT NONE

INTEGER (kind=ik)              :: ji,jj,j,ierror

nome=TRIM(TRIM(nomesez)//".di")     ! apro il file in base alla geometria scelta

OPEN (UNIT=2,FILE=nome,STATUS='old',ACTION='read',IOSTAT=ierror)

READ(2,'(A)')  fiume
READ(2,*)      indsez 
READ(2,*)	   nscab,nsezi,idum,idum,idum
WRITE(*,*)     "tratto considerato:",fiume
WRITE(*,*)     "Totale sezioni da leggere:",nsezi

! noto il numero delle sezioni

CALL alloca


!C  Lettura di ogni sezione
ji=1
DO WHILE (ji <= nsezi)
	  
   READ(2,*)jj   !numero della sezione oppure 0 se sez. interpolata    
   ji2jj(ji)=jj
   IF(jj.ne.0)THEN
      READ(2,'(a)') noms(jj)     !nome sezione
      READ(2,*)     jj2ji(jj)    !numero progressivo
   ENDIF
	   
   READ(2,*) si(ji),nrci(ji),npi(ji),flagi(ji)
   npi(ji)=npi(ji)+2
   ! SEZIONE NORMALE
   
   IF(flagi(ji) == 0)THEN
      CALL read_sez(ji)
   ENDIF

   ji=ji+1
ENDDO
CLOSE(2)


DO j = 1,nsezi-1 
    dx(j) = si(j+1)-si(j)
!write(*,*) j,dx(j),si(j)
ENDDO
!dx(nsezi)=100000.
!write(*,*) minval(dx),minloc(dx)
!stop


!C rendo gli argini molto alti per fare andare a
!C convergenza il programma

DO j = 1,nsezi  
   yi(1,j)		= 0.0_rk
   yi(2,j)		= yi(1,j) +1.0E-07
   zi(1,j)		= zi(2,j) +4.0_rk
   rks(1,j)		= rks(2,j)

   yi(npi(j),j)		= yi(npi(j)-1,j)+1.0E-07
   zi(npi(j),j)		= zi(npi(j)-1,j) +4.0_rk
   rks(npi(j),j)	= rks(npi(j)-1,j)

   zfmaxi(j)=MIN(zi(1,j),zi(npi(j),j)) 
ENDDO


RETURN
END SUBROUTINE letturasez
!----------------------------------------------------------------
SUBROUTINE letturasez_new
USE comuni
IMPLICIT NONE

INTEGER (kind=ik)              :: ji,k,ierror,j

WRITE(path,'(2a)') trim(path_in),'/sections.ori'
OPEN (UNIT=311,FILE=trim(path),STATUS='old',ACTION='read',IOSTAT=ierror)

READ(311,*)      nsezi 
WRITE(*,*)     "Totale sezioni da leggere:",nsezi
CALL alloca


ji=1
DO WHILE (ji <= nsezi)
	READ(311,*) npi(ji), nome
	npi(ji)=npi(ji)
	READ(311,*) ibw_sx(ji), zargl(ji), ibw_dx(ji), zargr(ji)
	noms(ji) = trim(nome)
	noms(ji)=trim(noms(ji))
	!write(*,'(i4,1x,a)') npi(ji),noms(ji)
	!write(*,*) ibw_sx(ji), zargl(ji), ibw_dx(ji), zargr(ji)
	ji=ji+1
END DO      
CLOSE(311)

WRITE(path,'(2a)') trim(path_in),'/points.ori'
OPEN (UNIT=312,FILE=trim(path),STATUS='old',ACTION='read',IOSTAT=ierror)


DO ji=1,nsezi
e(ji)= 1.E36 
zfmaxi(ji)=-1.E+36
	DO k=1,npi(ji)
		READ(312,*)  xi(k,ji), yi(k,ji),zi(k,ji),rks(k,ji)
		!write(*,*)  xi(k,ji), yi(k,ji),zi(k,ji),rks(k,ji)
		e(ji)=MIN(e(ji),zi(k,ji))  !trovo il minimo
   		zfmaxi(ji)=MAX(zfmaxi(ji),zi(k,ji)) !trovo il massimo 
	END DO
	! 1D case
	si(ji)=xi(2,ji)
END DO

DO ji = 1,nsezi-1 
    dx(ji) = si(ji+1)-si(ji)
    WRITE(*,*) ji,dx(ji),si(ji)
ENDDO

RETURN
END SUBROUTINE letturasez_new
!-----------------------------------------------------------------------------------------


SUBROUTINE vect
USE comuni
IMPLICIT NONE

! C purpose : creare dei vettori legati alle larghezze ed altezze di
! C			  ogni punto di ogni sezioni.
! C			  vedi: pag:82-85 tesi PhD

INTEGER   (kind=ik) :: j,i

DO  j=1,nsezi 
   CALL sorting(j) 
   CALL riarrangia_sezione(j)
   CALL vect_e_g1(j)
ENDDO


RETURN
END SUBROUTINE vect

!-------------------------------------------------------------------------------------

SUBROUTINE sorting(j)
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: j,i

REAL(kind=rk),        DIMENSION(npi(j)+1) :: altezze
INTEGER(kind=ik),     DIMENSION(npi(j)+1) :: posizione

DO i=1,npi(j) 
   altezze(i)    =zi(i,j)-e(j) 
   posizione(i)  =i 
ENDDO

CALL piksrt((npi(j)),altezze,posizione)

DO i=1,npi(j) 
    ksort(i,j) = posizione(i)
    etai(i,j)  = altezze  (i) 
ENDDO 


RETURN
END SUBROUTINE sorting

!-------------------------------------------------------------------------------------

SUBROUTINE piksrt(n,ra,pos)
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik),               INTENT(in)  :: n 
INTEGER   (kind=ik), DIMENSION(n), INTENT(out) :: pos 
REAL      (kind=rk), DIMENSION(n), INTENT(out) :: ra

INTEGER   (kind=ik) ::i,j,rpos 
REAL      (kind=ik) ::rra

  DO j=2,n
     rra=ra(j)
     rpos=pos(j)
     DO i=j-1,1,-1
        IF(ra(i) <= rra) GOTO 10
        ra(i+1)=ra(i)
        pos(i+1)=pos(i)
     ENDDO
     i=0
10   ra(i+1)=rra
     pos(i+1)=rpos
  ENDDO


  RETURN
END SUBROUTINE piksrt

!-------------------------------------------------------------------------------------

SUBROUTINE riarrangia_sezione(j)
  USE comuni
  IMPLICIT NONE

! C purpuse: Evita che due punti sul fondo siano alla stessa quota

  INTEGER (kind=ik), INTENT(in) :: j 

  INTEGER (kind=ik) :: k

!delta_z    = 1.e-10

  DO k=1,npi(j)-1 	 
     IF (zi(ksort(k+1,j),j)< zi(ksort(k,j),j)+delta_z)THEN 
        !       WRITE(*,'(" Modificata sez: ",i5," in punto: ",i5,2f12.6)') j,k
		zi(ksort(k+1,j),j) = zi(ksort(k,j),j)+delta_z
ENDIF
ENDDO


RETURN
END SUBROUTINE riarrangia_sezione

!-------------------------------------------------------------------------------------
SUBROUTINE vect_e_g1(j)

!C purpose:		Ricava le larghezze sigma e le altezze eta
!				con le z(quote) e i punti ordinati da piksrt
!				utilizza pelolibero per ricavarsi le altezze

  USE comuni
  IMPLICIT NONE

  INTEGER   (kind=ik) :: i,j
  REAL      (kind=rk) :: dd
     

  INTERFACE	 
     REAL (kind=rk) FUNCTION peloliberoi(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION peloliberoi
  END INTERFACE



!calcolo delle eta(altezze)	
  DO i = 1 , npi(j) 
     etai(i,j) = zi(ksort(i,j),j) - zi(ksort(1,j),j)   
  ENDDO

!calcolo delle sigma(larghezza)
  DO i = 1 , npi(j)
     dd=etai(i,j)
     sigm(i,j)=peloliberoi(j,dd)
  ENDDO
	

  DO i = 1 , npi(j)-1 
     IF (etai(i+1,j)/=etai(i,j)) then 
        ca(i,j) = (sigm(i+1,j)-sigm(i,j))/(etai(i+1,j)-etai(i,j)) 
     ELSE
        write(6,*)j,si(j),etai(i,j),etai(i+1,j),delta_z,ksort(i,j),&
             ksort(i+1,j),zi(ksort(i,j),j),zi(ksort(i+1,j),j)
        stop "ca=0"
     ENDIF
  ENDDO


!calcolo delle aree Ai (per il calcolo dell area bagnata) 
!calcolo delle aree Ji (per il calcolo di I1) 	
   
  omi(1,j) = 0.
  j1i(1,j) = 0.	
  DO i = 1,npi(j)-1	
     omi(i+1,j) = omi(i,j) + 0.5*(sigm(i+1,j)+sigm(i,j))*&
          (etai(i+1,j)-etai(i,j))
   
     j1i(i+1,j) = j1i(i,j) + (1./3.)*ca(i,j)*(etai(i+1,j)**3.-etai(i,j)**3.)&
          + 0.5*(sigm(i,j)-ca(i,j)*etai(i,j))*&
          (etai(i+1,j)**2.-etai(i,j)**2.)
  ENDDO
 


  RETURN
END SUBROUTINE vect_e_g1

!-------------------------------------------------------------------------------------

SUBROUTINE initia

! purpose: imporre le condizioni iniziali

  USE comuni
  IMPLICIT NONE

  INTEGER (kind=ik)						:: j,n,i_delta_temp,j_in
  INTEGER (kind=ik)						:: status
  INTEGER (kind=ik),	PARAMETER		:: nmax        =   1000       
  REAL    (kind=rk)						:: a_1,b_1,hu,null   , slope      
  CHARACTER(20)							:: niet
real (kind=rk) :: talv							

  INTERFACE
     REAL (kind=rk) FUNCTION depth2area(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION depth2area
  END INTERFACE

WRITE(path,'(2a)') trim(path_in),'/initial.ori'
OPEN(unit=12,file=trim(path),status='old',action='read')
!OPEN(unit=12,file='tagliamento.prs',status='old',action='read',form='unformatted')
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)
!READ(12,*)

DO j=1,nsezi
!   READ(12,*) h(j), q(j)
!      write(*,'(2(f12.6,1x))') q(j),h(j)-e(j)
 !write(333,*) e(j)
! write(*,*) h(j)
! h(j)=12.0_rk

!MAUREL GOTAL
 q(j)=0.0_rk
 h(j)=12.0_rk

!BUMP
! q(j)=4.42_rk
! h(j)=2.5_rk
ENDDO





!  DO j=1,nsezi
!     READ(12) q(j),e(j),h(j)
!     write(*,'(5(f12.6,1x))') q(j),h(j)-e(j)
!	 121  FORMAT (9(1x,e12.6))
!  ENDDO

! write(*,*) 'stop'
! stop

 CLOSE(12)





DO j=1,nsezi
   d(j)=h(j)-e(j)
   !d(j) = 227.0-e(j)
   !d(j) = 20.0
   !write(*,*) d(j)
ENDDO


!SISTEMARE QUI PARTE DOVE LEGGE IL FILE PER ci
  DO j=1,nsezi             
     A(j)=depth2area(j,d(j))

     CS(1,j) = A(j)         !varibile conservativa
     CS(2,j) = q(j)         !varibile conservativa
!write(*,*) q(j)
  END DO

!stop
  RETURN
END SUBROUTINE initia
!-------------------------------------------------------------------------
SUBROUTINE bcondi

!C     Purpose: impongo le condizioni al contorno

  USE comuni
  IMPLICIT NONE

  INTEGER   (kind=ik)::j,jj

  INTERFACE	 
     REAL (kind=rk) FUNCTION depth2area(j,dnn)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dnn
     END FUNCTION depth2area
  END INTERFACE



!C portata in ingresso calcolata dall'idrogramma

q(1)=qstar
  	

! CALL bcmonte(1,d(1),q(1),d(1))

!BUMP
!q(1)=qstar
!d(1)=d(2)


!MAUREL GOTAL
d(1)=d(2)
q(1)=q(2)


a(1)=depth2area(1,d(1))


! CALL bcvalle(d(nsezi),q(nsezi),nsezi)

!BUMP
!d(nsezi)=2.0_rk
!q(nsezi)=q(nsezi-1)

!MAUREL GOTAL
d(nsezi)=d(nsezi-1)
q(nsezi)=q(nsezi-1)
  

 a(nsezi)=depth2area(nsezi,d(nsezi))

!write(*,*) q(nsezi)

RETURN
END SUBROUTINE bcondi
!-----------------------------------------------------------------------
SUBROUTINE bcmonte(j,dmonte,qmonte,dd)
USE comuni
IMPLICIT NONE
!AIM:
!	calcola la condizione iniziale di monte
!	DATA : 05/02/2007				DD 	
!	UTILIZZA: dalla sub mccormack/ sub froude / bcmonte_len

INTEGER    (kind=ik)				:: j
REAL       (kind=rk)				:: dmonte,qmonte,dd
REAL       (kind=rk)				:: srj2(6),wrj2(3)
!REAL       (kind=rk)				:: spinta1,spinta2
REAL       (kind=rk)				:: frjp1

INTERFACE	 
   REAL (kind=rk) FUNCTION spinta(j,qmonte,dd)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: j
     REAL    (kind=rk), INTENT (in)  :: qmonte
     REAL    (kind=rk), INTENT (in)  :: dd
   END FUNCTION spinta
END INTERFACE

INTERFACE	 
   REAL (kind=rk) FUNCTION depth2area(j,dd)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: j
     REAL    (kind=rk), INTENT (in)  :: dd
   END FUNCTION depth2area
END INTERFACE


CALL froude(j+1,q(j+1),d(j+1),frjp1)
IF(frjp1 >= 1.)THEN
   dd = dmonte
    RETURN
ENDIF


CALL bcmonte_len(j,qmonte,d(1))

!temp(1)=temp_cost

RETURN
END SUBROUTINE bcmonte
!--------------------------------------------------
SUBROUTINE bcmonte_len(j,qmonte,dd)
  USE comuni
  IMPLICIT NONE
!AIM:
!	calcola la condizione iniziale di monte
!	DATA : 05/02/2007				DD 	
!	UTILIZZA: calcolaswj / dcritica

INTEGER (kind=ik)			    :: j
REAL    (kind=rk)				:: srj1(6),wrj1(3),srj2(3),wrj2(3),sr(3)
REAL    (kind=rk)				:: deltas(3),deltaw(3),wr(3),sm(3),wm(3)
REAL    (kind=rk)				:: rlamp2,rlamm2
REAL    (kind=rk)				:: deltax,dxr
REAL    (kind=rk)				:: djc,dj,dd,qmonte,qq
INTEGER (kind=ik)				:: k
REAL    (kind=rk)				:: rlamp1,rlamm1
REAL    (kind=rk)				:: deltad,deltaq,domdx,dedx
REAL    (kind=rk)				:: dr,qr,omm,omv
REAL    (kind=rk)				:: g1r,rlampr,rlammr,a1r,b1r,d1r


INTERFACE	 
   REAL (kind=rk) FUNCTION dcritica(ji,qq)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: ji
     REAL    (kind=rk), INTENT (in)  :: qq
   END FUNCTION dcritica
END INTERFACE
INTERFACE	 
   REAL (kind=rk) FUNCTION depth2area(j,dd)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: j
     REAL    (kind=rk), INTENT (in)  :: dd
   END FUNCTION depth2area
END INTERFACE


 
CALL calcolaswj(j+1,e(j+1)+d(j+1),q(j+1),srj2,wrj2)
CALL eigenvalues(rlamp2,rlamm2,srj2)

deltax = dx(j)
dxr=-rlamm2*dtime/deltax

djc=dcritica(j,q(j))
dj=max(d(j),djc)

CALL calcolaswj(j,e(j)+dj,q(j),srj1,wrj1)
CALL eigenvalues(rlamp1,rlamm1,srj1)

deltad=d(j+1)-dj
deltaq=q(j+1)-q(j)

DO k=1,3
   deltas(k)=srj2(k)-srj1(k)
ENDDO
DO k=1,3
   deltaw(k)=wrj2(k)-wrj1(k)
ENDDO

dr=dj+dxr*deltad
qr=q(j)+dxr*deltaq
DO k=1,3
   sr(k)=srj1(k)+dxr*deltas(k)
ENDDO
DO k=1,3
   wr(k)=wrj1(k)+dxr*deltaw(k)
ENDDO


omm=depth2area(j,dr)
omv=depth2area(j+1,dr)

domdx=(omv-omm)/deltax
dedx=(e(j+1)-e(j))/deltax
g1r=wr(2)*domdx-wr(1)-wr(3)*dedx
CALL eigenvalues(rlampr,rlammr,sr)
a1r=sr(2)
b1r=rlammr
d1r=rlammr*g1r

dd=dr+b1r*(qr-qmonte)/a1r+d1r*dtime/a1r

RETURN
END SUBROUTINE bcmonte_len
!-----------------------------------------------------------------------
SUBROUTINE bcvalle(dd,qq,j)
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: j
REAL      (kind=rk) :: hv,dvalle,frjm1
REAL      (kind=rk) :: qq,dd,area
REAL      (kind=rk) :: srj2(6),wrj2(3)


INTERFACE
 REAL (kind=rk) FUNCTION depth2area(j,dd)
   USE comuni
   INTEGER (kind=ik), INTENT (in)  :: j
   REAL    (kind=rk), INTENT (in)  :: dd
 END FUNCTION depth2area
END INTERFACE


j=nsezi

CALL profondita_valle(Q(nsezi),dvalle,j)



CALL froude(j-1,q(j-1),d(j-1),frjm1)
		
IF(frjm1 < 1.)THEN
   dd=dvalle
   CALL bcvalle_len(j,dvalle,qq)
   dd=dvalle

ELSE
   CALL bcvalle_vel(j,dd,qq)
   dd = dvalle
ENDIF
!pause'2'		

a(j)   = depth2area   (j,dd)
atemp(j)=temp_cost*a(j)
temp(j) =temp_cost
	
RETURN
END SUBROUTINE bcvalle

!--------------------------------------------------------------------
SUBROUTINE bcvalle_len(j,dvalle,qq)
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: j,k
REAL      (kind=rk) :: qq 
REAL      (kind=rk) :: srj1(6),wrj1(3),srj2(6),wrj2(3)
REAL      (kind=rk) :: deltas(6),deltaw(3)
REAL      (kind=rk) :: sr(6),wr(6)

REAL      (kind=rk) :: rlamp2,rlamm2,rlamp1,rlamm1
REAL      (kind=rk) :: rlampr,rlammr
REAL      (kind=rk) :: deltax,dxr,deltad,dr,qr
REAL      (kind=rk) :: djc,dj,deltaq
REAL      (kind=rk) :: domdx,dedx
REAL      (kind=rk) :: omm,omv
REAL      (kind=rk) :: g1r,a1r,b1r,d1r
REAL      (kind=rk) :: dvalle

INTERFACE	 
	 REAL (kind=rk) FUNCTION dcritica(ji,qq)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: ji
       REAL    (kind=rk), INTENT (in)  :: qq
     END FUNCTION dcritica
END INTERFACE
INTERFACE	 
	 REAL (kind=rk) FUNCTION depth2area(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION depth2area
END INTERFACE

CALL calcolaswj(j-1,e(j-1)+d(j-1),q(j-1),srj2,wrj2)
CALL eigenvalues(rlamp2,rlamm2,srj2)

deltax = dx(j-1)
dxr=rlamp2*dtime/deltax

djc=dcritica(j,q(j))
dj=MAX(d(j),djc)

CALL calcolaswj(j,e(j)+dj,q(j),srj1,wrj1)
CALL eigenvalues(rlamp1,rlamm1,srj1)

deltad=dj-d(j-1)
deltaq=q(j)-q(j-1)

	DO k=1,3
	   deltas(k)=srj1(k)-srj2(k)
	ENDDO
	DO k=1,3
	   deltaw(k)=wrj1(k)-wrj2(k)
	ENDDO

dr=dj-dxr*deltad
qr=q(j)-dxr*deltaq

DO k=1,3
   sr(k)=srj1(k)-dxr*deltas(k)
ENDDO
DO k=1,3
   wr(k)=wrj1(k)-dxr*deltaw(k)
ENDDO

CALL eigenvalues(rlampr,rlammr,sr)
omm=depth2area(j-1,dr)
omv=depth2area(j,dr)
domdx=(omv-omm)/deltax
dedx=(e(j)-e(j-1))/deltax

g1r=wr(2)*domdx-wr(1)-wr(3)*dedx
a1r=sr(2)
b1r=rlampr
d1r=rlampr*g1r

qq=qr+a1r*(dr-dvalle)/b1r+d1r*dtime/b1r


RETURN
END SUBROUTINE bcvalle_len
!-------------------------------------------------------
SUBROUTINE bcvalle_vel(j,dd,qq)
USE comuni
IMPLICIT NONE


INTEGER   (kind=ik) :: j,k
REAL      (kind=rk) :: qq,dd

REAL      (kind=rk) :: srj1(6),wrj1(3),srj2(6),wrj2(3)
REAL      (kind=rk) :: sr1(6),wr1(6),sr2(6),wr2(3),sm(6),wm(3)
REAL      (kind=rk) :: deltas(3),deltaw(3)
REAL      (kind=rk) :: dr1,qr1,dr2,qr2
REAL      (kind=rk) :: rlampr1,rlammr1,rlampr2,rlammr2
REAL      (kind=rk) :: g1r1,g1r2,a1r1,a2r2,b1r1,b2r2,d1r1,d2r2
REAL      (kind=rk) :: am(2,2),f(2),x(2)

REAL      (kind=rk) :: rlamp2,rlamm2,rlamp1,rlamm1
REAL      (kind=rk) :: rlampr,rlammr
REAL      (kind=rk) :: deltax,dxr,deltad,dr,qr
REAL      (kind=rk) :: djc,dj,deltaq
REAL      (kind=rk) :: domdx,dedx,dxr1,dxr2
REAL      (kind=rk) :: omm,omv
REAL      (kind=rk) :: g1r,a1r,b1r,d1r
REAL      (kind=rk) :: dvalle

INTERFACE	 
	 REAL (kind=rk) FUNCTION dcritica(ji,qq)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: ji
       REAL    (kind=rk), INTENT (in)  :: qq
     END FUNCTION dcritica
END INTERFACE
INTERFACE	 
	 REAL (kind=rk) FUNCTION depth2area(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION depth2area
END INTERFACE


CALL calcolaswj(j-1,e(j-1)+d(j-1),q(j-1),srj2,wrj2)
CALL eigenvalues(rlamp2,rlamm2,srj2)

deltax = dx(j-1)
dxr1=rlamp2*dtime/deltax
dxr2=rlamm2*dtime/deltax

djc=dcritica(j,q(j))
dj=MIN(d(j),djc)


CALL calcolaswj(j,e(j)+dj,q(j),srj1,wrj1)
CALL eigenvalues(rlamp1,rlamm1,srj1)


deltad=dj-d(j-1)
deltaq=q(j)-q(j-1)

DO k=1,3
   deltas(k)=srj1(k)-srj2(k)
ENDDO
DO k=1,3
   deltaw(k)=wrj1(k)-wrj2(k)
ENDDO

dr1=dj-dxr1*deltad
qr1=q(j)-dxr1*deltaq
dr2=dj-dxr2*deltad
qr2=q(j)-dxr2*deltaq


DO k=1,3
   sr1(k)=srj1(k)-dxr1*deltas(k)
   sr2(k)=srj1(k)-dxr2*deltas(k)
ENDDO
DO k=1,3
   wr1(k)=wrj1(k)-dxr1*deltaw(k)
   wr2(k)=wrj1(k)-dxr2*deltaw(k)
ENDDO

CALL eigenvalues(rlampr1,rlammr1,sr1)
CALL eigenvalues(rlampr2,rlammr2,sr2)

	omm=depth2area(j-1,dr1)
	omv=depth2area(j,dr1)
	domdx=(omv-omm)/deltax
	dedx=(e(j)-e(j-1))/deltax
	g1r1=wr1(2)*domdx-wr1(1)-wr1(3)*dedx

	omm=depth2area(j-1,dr2)
	omv=depth2area(j,dr2)
	domdx=(omv-omm)/deltax
	dedx=(e(j)-e(j-1))/deltax
	g1r2=wr2(2)*domdx-wr2(1)-wr2(3)*dedx

	a1r1=sr1(2)
	a2r2=sr2(2)
	b1r1=rlampr1
	b2r2=rlammr2
	d1r1=rlampr1*g1r1
	d2r2=rlammr2*g1r2

	am(1,1)=a1r1
	am(1,2)=b1r1
	f(1)=am(1,1)*dr1+am(1,2)*qr1+d1r1*dtime
	am(2,1)=a2r2
	am(2,2)=b2r2
	f(2)=am(2,1)*dr2+am(2,2)*qr2+d2r2*dtime
 CALL rgauss(am,f,x,2)

dd = x(1)
qq = x(2)


RETURN
END SUBROUTINE bcvalle_vel
!-----------------------------------------------------------
SUBROUTINE cflcon

!C     Purpose: to apply the CFL condition to find a stable time
!C              step size DT

USE comuni
IMPLICIT NONE

INTEGER (kind=ik) :: j
REAL    (kind=rk) :: dtmin,umed,bj,cmed,deltaxfv
REAL    (kind=rk) :: cel,det,dd,speloc,ssmax

INTERFACE	 
	 REAL (kind=rk) FUNCTION peloliberoi(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION peloliberoi
END INTERFACE

dtmin = 1.0e36
ssmax = 0.

DO j = 1,nsezi

   bj = peloliberoi(j,d(j))
		
   !calcolo celerità

   c(j) = SQRT(gi*A(j)/bj)

   !calcolo velocità media nella sezione
	
   u(j) = q(j)/A(j)
	
   speloc = ABS(u(j)) + c(j)

   IF(j==1)THEN
      deltaxfv = dx(j)
   ELSE
      deltaxfv = 0.5*(dx(j)+dx(j-1))
   ENDIF


   det = cflcoe*deltaxfv/speloc

   IF(det>0.)THEN           
      dtmin = MIN(dtmin,det)
   ENDIF

ENDDO


!C calcolo del time step

dtime  = dtmin

IF(nn <= 5) dtime = 0.2*dtime

IF((time + dtime) >= timeout)THEN
      dtime = timeout - time
   ENDIF

   time = time + dtime


   RETURN
 END SUBROUTINE cflcon
!-------------------------------------------------------------------------------------------
SUBROUTINE eigenvalues(rlam1,rlam2,sr) 
USE comuni
IMPLICIT NONE
!AIM:
!	calcola gli autovalori di una matrice
!	DATA : 05/02/2007				DD 	
!	UTILIZZA: 

REAL    (kind=rk)			     :: sr(6)
REAL    (kind=rk)			     :: a0
REAL    (kind=rk)			     :: a1
REAL    (kind=rk)			     :: rlam1
REAL    (kind=rk)			     :: rlam2


   
a0=-sr(1)*sr(2) 
a1=sr(3) 
 
IF ((a1*a1-4.*a0).lt.0) THEN 
  WRITE(*,*) "       ----IL PROGRAMMA NON PUO PROCEDERE----" 
  WRITE(*,*) "ATTENZIONE NELLA SUB.EIGENVALUES IL RADICANDO è NEGATIVO!" 
  WRITE(*,*) "POTREBBE DIPENDERE DA SEZIONI POCO FITTE..." 
  WRITE(*,*) "                   DIMINUIRE DX            " 
  STOP 
ENDIF 
 
rlam1=0.5*(a1+SQRT(a1*a1-4.*a0)) 
rlam2=0.5*(a1-SQRT(a1*a1-4.*a0)) 

RETURN
END SUBROUTINE eigenvalues
!-------------------------------------------------------------------------
SUBROUTINE wafstate

!C		purpose: computation of a weighted average state version

USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: i,j,K
REAL      (kind=rk) :: dtodx,deltax,iupw
REAL      (kind=rk) :: DL,DR,UL,UR,CL,CR,TL,TR
REAL      (kind=rk) :: SL,SR,DLOC,DUPW
REAL      (kind=rk) :: WL,WR,WM
REAL      (kind=rk) :: RATIO
REAL      (kind=rk),DIMENSION(2):: FHLL
REAL      (kind=rk),DIMENSION(2):: WAFLIM
REAL      (kind=rk) :: US,DS,AS,QL,QR,D0,GL,GR
REAL      (kind=rk)   , PARAMETER   :: TOLLIM= 1.0E-6


INTERFACE	 
   REAL (kind=rk) FUNCTION depth2area(j,dd)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: j
     REAL    (kind=rk), INTENT (in)  :: dd
   END FUNCTION depth2area
   REAL (kind=rk) FUNCTION area2depth(j,aa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: j
     REAL    (kind=rk), INTENT (in)  :: aa
   END FUNCTION area2depth
END INTERFACE





ciclo_j:DO j = 1,nsezi-1

!C si definiscono i valori right e left per il problema di Riemann

 !   dtodx = dt/dx(j)
!write(*,*) cs(1,j),cs(1,j+1)
   DL = area2depth(j,CS(1,J))
   UL = CS(2,J)/CS(1,J)
   CL = C(j)

   DR = area2depth(j+1,CS(1,J+1))
   UR = CS(2,J+1)/CS(1,J+1)
   CR = C(j+1)

!C calcolo delle velocità nel caso di dry bed
!(aggiungere in seguito)


!C calcolo della profondità nella regione star (10.18)

!EXACT DEPTH POSITIVITY RIEMANN SOLVER
!   DS = 0.5_rk*(DL+DR)-0.25_rk*(UR-UL)*(DL+DR)/(CL+CR) 
!   US = 0.5_rk*(UL+UR)-(DR-DL)*(CL+CR)/(DL+DR)
!TWO-RAREFACTION RIEMANN SOLVER 
!   DS = 1.0_rk/gi*(0.5_rk*(CL+CR)+0.25_rk*(UL-UR))**2
!   US = 0.5_rk*(UL+UR)+(CL-CR)
!TWO-SHOCK RIEMANN SOLVER
   D0 = 0.5_rk*(DL+DR)-0.25_rk*(UR-UL)*(DL+DR)/(CL+CR) 
   GL=sqrt(0.5_rk*gi*(D0+DL)/(D0*DL))
   GR=sqrt(0.5_rk*gi*(D0+DR)/(D0*DR))

   DS = (GL*DL+GR*DR+UL-UR)/(GL+GR)
   US = 0.5_rk*(UL+UR)+0.5_rk*((DS-DR)*GR-(DS-DL)*GL)

 
   AS =depth2area(J,DS)

   CS_star(1,j)=AS
   CS_star(2,j)=US*AS
!C calcolo delle velocità d'onda right e left nel caso di wet bed

   IF(DS <= DL)THEN
      ! rarefazione
      SL = UL - CL 
   ELSE  
	 ! shock      
      SL = UL - CL*SQRT(0.5*DS*(DS + DL))/DL 		    
   ENDIF


   !LEFT
   IF(DS <= DR)THEN
      ! rarefazione
      SR = UR + CR	 
   ELSE
      ! shock
      SR = UR + CR*SQRT(0.5*DS*(DS + DR))/DR 
   ENDIF

!
!C        Compute HLL intercell flux in the STAR REGION for first two
!C        flux components
!
!
!C        -------------------------------------------------------------
!C        HLL Approximate Riemann Solver ends
!C        ------------------------------------------------------------- 
!
   DO k=1,2
      STATE_L(K,J)=CS(K,J)
      STATE_M(K,J)=CS_star(K,J)
      STATE_R(K,J)=CS(K,J+1)
  ENDDO


!C        Store wave speeds, needed for TVD WAF flux. 
!C        Note order of waves

!C wave speeds

     WS(1,J) = SL
     WS(2,J) = SR
     WS(3,J) = US
!C calcolo della discontinuità nella profondità per la condizione TVD

!     WJ(1,J) = DS - DL
!     WJ(2,J) = DR-DS
     WJ(1,J) = AS - A(J)
     WJ(2,J) = A(j+1)-AS

  ENDDO ciclo_j

!C      calcolo del flusso tra le celle in accordo con il WAF

  ciclo_tvd:DO j = 1,nsezi-1

! condizione TVD

    ciclo_k:DO k=1,2
	! numerero di Courant per ogni onda
        IF(j==1)THEN
           deltax = dx(j)
        ELSE
           deltax = 0.5*(dx(j-1)+dx(j))
        ENDIF
        dtodx = dTime/deltax
!        CN(k) = WS(k,j)*dtodx
        CN(k) = WS(k,j)


      !C Identificazione upwind direction

      IF(CN(K) >= 0.0_rk)THEN

            !C Wave k is positive and upwind direction is on the Left
         IUPW = -1
      ELSE

         !C Wave k is negative and upwind direction is on the right

         IUPW =  1
      ENDIF

      !C calcolo della discontinuità
      
      
      no_tvd_bc :if(j /= 1) then

         DLOC = WJ(K,j)
         DUPW = WJ(K,j+IUPW)

      !C si modifica le piccole discontinuità

         IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0,DUPW)
         IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0,DLOC)

      !C Calcolo RATIO di upwind per le discontinuità locali

         RATIO = DUPW/DLOC

      !C Select limiter function WAFLIM

      !C     LIMITE = 1, Godunov's Method
      !C     LIMITE = 2, Upwind Second Order Method (non-monotone)
      !C     LIMITE = 3, Upwind TVD, with SUPERBEE type limiter
      !C     LIMITE = 4, Upwind TVD, with VAN LEER type limiter
      !C     LIMITE = 5, Upwind TVD, with VAN ALBADA type limiter
      !C     LIMITE = 6, Upwind TVD, with MINMOD type limiter

         if(limite==6.and.k==3)then
            CALL SUPERA(RATIO, ABS(CN(K)), WAFLIM(K))
         else
            IF(LIMITE == 1)WAFLIM(K) = 1.0_rk
            IF(LIMITE == 2)WAFLIM(K) = ABS(CN(K))
            IF(LIMITE == 3)CALL SUPERA(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE == 4)CALL VANLEE(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE == 5)CALL VANALB(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE == 6)CALL MINAAA(RATIO, ABS(CN(K)), WAFLIM(K))
         end if

      endif no_tvd_bc

      if(J==1)THEN
         WAFLIM(K) = ABS(CN(K))
      ELSEIF(J==NSEZI-1)THEN
         WAFLIM(K) = ABS(CN(K))
      END if


      WAFLIM(K) = WAFLIM(K)*SIGN(1.0, CN(K))

      
   ENDDO ciclo_k


   !C Calcolo dei pesi per il WAF con il TVD

   WL = 0.5*(1.0       + WAFLIM(1))
   WM = 0.5*(WAFLIM(2) - WAFLIM(1))
   WR = 0.5*(1.0       - WAFLIM(2))

   DO k=1,2
      STATE(K,J)= WL*STATE_L(K,J) + WM*STATE_M(K,J) + WR*STATE_R(K,J)
   ENDDO


   CALL fluxes(j,STATE(2,j),STATE(1,j),FLUX(1,j),FLUX(2,j))

!
!     -----------------------------------------------------------------
!     Computation of the TVD WAF intercell flux ends
!     -----------------------------------------------------------------
!
END DO ciclo_tvd
!pause 'ciclo_tvd'

RETURN
END SUBROUTINE wafstate
!----------------------------------------------------------------
!----------------------------------------------------------------

SUBROUTINE wafflu

!C purpose: calcolare i flussi tra le celle usando un
! medodo basato sull'approssimazione di Riemann

USE comuni
IMPLICIT NONE

INTEGER (kind=ik) :: i,j,K
REAL (kind=rk) :: di, bi, beta, dbd, betaj, betap
REAL (kind=rk) :: dtodx,deltax,iupw, V, sf
REAL (kind=rk) :: DL,DR,UL,UR,CL,CR,TL,TR
REAL (kind=rk) :: SL,SR,DLOC,DUPW
REAL (kind=rk) :: WL,WR,WM
REAL (kind=rk) :: RATIO,DELTACS,DELTAFLUX
REAL (kind=rk),DIMENSION(2):: FHLL,ni,FIWAF,VST
REAL (kind=rk),DIMENSION(2):: WAFLIM, source, CSmid, invJACxsou,sourcemod
REAL (kind=rk),DIMENSION(2,2):: invJAC, JAC, result, R, invR, aproxinvJAC
REAL (kind=rk) :: US,DS,AS,QL,QR,D0,GL,GR
REAL (kind=rk) , PARAMETER :: TOLLIM= 1.0E-6
REAL (KIND=rk), DIMENSION (nsezi):: SLNI,SRNI

INTERFACE
REAL (kind=rk) FUNCTION depth2area(j,dd)
USE comuni
INTEGER (kind=ik), INTENT (in) :: j
REAL (kind=rk), INTENT (in) :: dd
END FUNCTION depth2area
REAL (kind=rk) FUNCTION area2depth(j,aa)
USE comuni
INTEGER (kind=ik), INTENT (in) :: j
REAL (kind=rk), INTENT (in) :: aa
END FUNCTION area2depth
REAL (kind=rk) FUNCTION peloliberoi(j,dd)
USE comuni
INTEGER (kind=ik), INTENT (in)  :: j  
REAL    (kind=rk), INTENT (in)  :: dd
END FUNCTION peloliberoi
END INTERFACE


DO j = 1,nsezi
!C calcolo dei flussi
 CALL fluxes(j,CS(2,j),CS(1,j),Fd(1,j),Fd(2,j))
END DO


 ciclo_j:DO j = 1,nsezi-1


!C si definiscono i valori right e left per il problema di Riemann

! dtodx = dt/dx(j)
DL = area2depth(j,CS(1,J))
UL = CS(2,J)/CS(1,J)

 CL = C(j)

DR = area2depth(j+1,CS(1,J+1))
UR = CS(2,J+1)/CS(1,J+1)

 CR = C(j+1)

!C calcolo delle velocità nel caso di dry bed
!(aggiungere in seguito)


!C calcolo della profondità nella regione star (10.18)
!EXACT DEPTH POSITIVITY RIEMANN SOLVER
! DS = 0.5_rk*(DL+DR)-0.25_rk*(UR-UL)*(DL+DR)/(CL+CR)
! US = 0.5_rk*(UL+UR)-(DR-DL)*(CL+CR)/(DL+DR)
!TWO-RAREFACTION RIEMANN SOLVER
! DS = 1.0_rk/gi*(0.5_rk*(CL+CR)+0.25_rk*(UL-UR))**2
! US = 0.5_rk*(UL+UR)+(CL-CR)
!TWO-SHOCK RIEMANN SOLVER
D0 = 0.5_rk*(DL+DR)-0.25_rk*(UR-UL)*(DL+DR)/(CL+CR)
GL=sqrt(0.5_rk*gi*(D0+DL)/(D0*DL))
GR=sqrt(0.5_rk*gi*(D0+DR)/(D0*DR))
DS = (GL*DL+GR*DR+UL-UR)/(GL+GR)
US = 0.5_rk*(UL+UR)+0.5_rk*((DS-DR)*GR-(DS-DL)*GL)

   AS =depth2area(J,DS)

IF(DS <= DL)THEN
! rarefazione
SL = UL - CL
ELSE
! shock
SL = UL - CL*SQRT(0.5*DS*(DS + DL))/DL
ENDIF


!LEFT
IF(DS <= DR)THEN
! rarefazione
SR = UR + CR
ELSE
! shock
SR = UR + CR*SQRT(0.5*DS*(DS + DR))/DR
ENDIF

!
!C Compute HLL intercell flux in the STAR REGION for first two
!C flux components
!
!C calcolo dei flussi nella regione STAR 

FHLL(1) = (SR*FD(1,J)-SL*FD(1,J+1)+SL*SR*(CS(1,J+1)-CS(1,J)))/(SR-SL)
FHLL(2) = (SR*FD(2,J)-SL*FD(2,J+1)+SL*SR*(CS(2,j+1)-CS(2,J)))/(SR-SL)

!
!C -------------------------------------------------------------
!C HLL Approximate Riemann Solver ends
!C -------------------------------------------------------------
!
DO k=1,2
FLUXL(K,J)=FD(K,J)
FLUXM(K,J)=FHLL(K)
FLUXR(K,J)=FD(K,J+1)   

ENDDO


!C Store wave speeds, needed for TVD WAF flux.
!C Note order of waves

!C wave speeds

WS(1,J) = SL
WS(2,J) = SR
WS(3,J) = US
!C calcolo della discontinuità nella profondità per la condizione TVD

! WJ(1,J) = DS - DL
! WJ(2,J) = DR-DS
WJ(1,J) = AS - A(J)
WJ(2,J) = A(j+1)-AS


SLNI(J)=SL
SRNI(J)=SR
ENDDO ciclo_j

!C calcolo del flusso tra le celle in accordo con il WAF

 ciclo_tvd:DO j = 1,nsezi-1

! condizione TVD

 ciclo_k:DO k=1,2
! numero di Courant per ogni onda
IF(j==1)THEN
deltax = dx(j)
ELSE
deltax = 0.5*(dx(j-1)+dx(j))
ENDIF
dtodx = dTime/deltax
 CN(k) = WS(k,j)*dtodx
! CN(k) = WS(k,j)


!C Identificazione upwind direction

IF(CN(K) >= 0.0_rk)THEN

!C Wave k is positive and upwind direction is on the Left
IUPW = -1
ELSE

!C Wave k is negative and upwind direction is on the right

IUPW = 1
ENDIF

!C calcolo della discontinuità


no_tvd_bc :if(j /= 1) then

DLOC = WJ(K,j)
DUPW = WJ(K,j+IUPW)

!C si modifica le piccole discontinuità

IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0,DUPW)
IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0,DLOC)

!C Calcolo RATIO di upwind per le discontinuità locali

RATIO = DUPW/DLOC

!C Select limiter function WAFLIM

!C LIMITE = 1, Godunov's Method
!C LIMITE = 2, Upwind Second Order Method (non-monotone)
!C LIMITE = 3, Upwind TVD, with SUPERBEE type limiter
!C LIMITE = 4, Upwind TVD, with VAN LEER type limiter
!C LIMITE = 5, Upwind TVD, with VAN ALBADA type limiter
!C LIMITE = 6, Upwind TVD, with MINMOD type limiter


IF(LIMITE == 1)WAFLIM(K) = 1.0_rk
IF(LIMITE == 2)WAFLIM(K) = ABS(CN(K))
IF(LIMITE == 3)CALL SUPERA(RATIO, ABS(CN(K)), WAFLIM(K))
IF(LIMITE == 4)CALL VANLEE(RATIO, ABS(CN(K)), WAFLIM(K))
IF(LIMITE == 5)CALL VANALB(RATIO, ABS(CN(K)), WAFLIM(K))
IF(LIMITE == 6)CALL MINAAA(RATIO, ABS(CN(K)), WAFLIM(K))

!write(*,*) CN(K),'CN'
endif no_tvd_bc

if(J==1)THEN
WAFLIM(K) = CN(K)
ELSEIF(J==NSEZI-1)THEN
WAFLIM(K) = CN(K)
END if

!if(J==1)THEN
!WAFLIM(K) = 1.0_rk
!ELSEIF(J==NSEZI-1)THEN
!WAFLIM(K) = 1.0_rk
!END if


WAFLIM(K) = WAFLIM(K)*SIGN(1.0, CN(K))
!write(*,*)  WAFLIM(k), 'LIMITATORE'

ENDDO ciclo_k


!C Calcolo dei pesi per il WAF con il TVD

WL = 0.5*(1.0 + WAFLIM(1))
WM = 0.5*(WAFLIM(2) - WAFLIM(1))
WR = 0.5*(1.0 - WAFLIM(2))

!Calcolo i coefficienti di viscosità numerica

ni(1)= -0.5_rk*SRNI(J)*SLNI(J)/(SRNI(J)-SLNI(J))*(WAFLIM(1)-WAFLIM(2))
ni(2)= -0.5_rk*(WAFLIM(2)*SRNI(J)-WAFLIM(1)*SLNI(J))/(SRNI(J)-SLNI(J))


!write(*,*) -0.5*SRNI(J)*SLNI(J)
!write(*,*) -0.5*(SRNI(J)+SLNI(J))

!Calcolo la matrice inversa della Jacobiana
 
 ! Calcolo U i+1/2
 CSmid(1)=(CS(1,J)+CS(1,J+1))*0.5_rk
 CSmid(2)=(CS(2,J)+CS(2,J+1))*0.5_rk
 
 di=0.5_rk*(d(j)+d(j+1))
  !di = area2depth(j,CSmid(1))

 CALL calcolabeta(j,e(j)+d(j),betaj)
 CALL calcolabeta(j+1,e(j+1)+d(j+1),betap)

 beta=0.5_rk*(betaj+betap)
 

 bi = (peloliberoi(j,d(j))+ peloliberoi(j+1,d(j+1)))*0.5_rk

 dbd=(peloliberoi(j+1,d(j+1)) - peloliberoi(j,d(j)))/(si(j+1)-si(j))

JAC(1,1) = 0.0_rk
JAC(1,2) = 1.0_rk
JAC(2,1) = gi*CSmid(1)/bi-beta*(CSmid(2)/CSmid(1))**2
JAC(2,2) = 2.0_rk*CSmid(2)/CSmid(1)


if(abs(sqrt(beta)*CSmid(2)/CSmid(1)/(sqrt(gi*CSmid(1)/bi)) -1._rk) .lt. 0.01) then

write(*,*) j, nn
invJAC(1,1) = 0.0_rk
invJAC(1,2) = 0.0_rk
invJAC(2,1) = 0.0_rk
invJAC(2,2) = 0.0_rk

ELSE
invJAC(1,1) = -2.0_rk*beta*CSmid(2)*CSmid(1)*bi/(gi*CSmid(1)**3-beta*bi*CSmid(2)**2)
invJAC(1,2) = bi*CSmid(1)**2/(gi*CSmid(1)**3-bi*beta*CSmid(2)**2)
invJAC(2,1) = 1.0_rk
invJAC(2,2) = 0.0_rk
END IF

!R(1,1) = 1.0_rk
!R(1,2) = 1.0_rk
!R(2,1) = CSmid(2)/CSmid(1)-sqrt(gi*di)
!R(2,2) = CSmid(2)/CSmid(1)+sqrt(gi*di)

!invR(1,1) = CSmid(2)/CSmid(1)+sqrt(gi*di)/(2.0_rk*sqrt(gi*di))
!invR(1,2) = -1.0_rk/(2.0_rk*sqrt(gi*di))
!invR(2,1) = -CSmid(2)/CSmid(1)+sqrt(gi*di)/(2.0_rk*sqrt(gi*di))
!invR(2,2) = 1.0_rk/(2.0_rk*sqrt(gi*di))

!IF (abs(CSmid(2)/CSmid(1)-sqrt(gi*di)) .gt. 1.0E-2) THEN
!aproxinvJAC(1,1) = 1.0_rk/(CSmid(2)/CSmid(1)-sqrt(gi*di))
!ELSE
!aproxinvJAC(1,1) = 0.0_rk
!write(*,*) j,1.0_rk/abs(CSmid(2)/CSmid(1)-sqrt(gi*di)),nn
!END IF
!aproxinvJAC(1,2) = 0.0_rk
!aproxinvJAC(2,1) = 0.0_rk
!IF (abs(CSmid(2)/CSmid(1)+sqrt(gi*di)) .gt. 1.0E-2) THEN
!aproxinvJAC(2,2) = 1.0_rk/(CSmid(2)/CSmid(1)+sqrt(gi*di))
!ELSE
!aproxinvJAC(2,2) = 0.0_rk
!write(*,*) j,1.0_rk/abs(CSmid(2)/CSmid(1)+sqrt(gi*di)),nn
!END IF

!invJAC=MATMUL(R,aproxinvJAC)
!invJAC=MATMUL(invJAC,invR)

!result=matmul(R,invR)

!write(*,*) result(1,1),result(1,2),result(2,1),result(2,2)
!read(*,*)



!Chiamo la subroutine per il calcolo del termine sorgente in i+1/2

 CALL sourceswb(j,CS(2,J),di,CS(1,J),1,CSmid,bi,dbd,source,V,sf)
 
 
 sourcemod(1)=source(1)
 sourcemod(2)=source(2)-V

 invJACxSOU=MATMUL(invJAC,sourcemod)

! Calcolo i flussi WAF Well-Balanced
DO k=1,2
!write(*,*) fluxl(k,j),fluxr(k,j)
!FLUX(K,J) = WL*FLUXL(K,J) + WM*FLUXM(K,J) + WR*FLUXR(K,J)

DELTACS   = CS(K,j+1)-CS(K,J)
DELTAFLUX = FLUXR(K,J)-FLUXL(K,J)
!FIWAF(K)  = 0.5_rk*(FLUXL(K,J)+FLUXR(K,J))+(ni(1)*DELTACS+ni(2)*DELTAFLUX)
!write(*,'(2(i4,1x),6(f14.6,1x))') k,j,flux(k,j),fiwaf(k),ni(1),ni(2),deltaCS,deltaflux
!write(*,*) SR, SL, WAFLIM(2), WAFLIM(1)
VST(K) = ni(1)*(DELTACS-dx(J)*invJACxSOU(k))+ni(2)*(DELTAFLUX-dx(J)*source(k))

!IF (abs(VST(K)) .lt. 1.0E-7) VST(K)=0.0_rk 

FLUX(K,J)  = 0.5_rk*(FLUXL(K,J)+FLUXR(K,J)) + VST(k)
!write(*,*) FLUXL(K,J),FLUXR(K,J)
!write(*,*) invJAC(1,1), invJAC(1,2)
!write(*,'(2(i4,1x),6(f14.6,1x))') k,j,FLUX(k,j),VST(K),DELTACS,dx(J)*invJACxSOU(k),DELTAFLUX,dx(J)*source(k)

!read(*,*)
!write(*,*) source(2)*invJAC(1,2)

!result=matmul(jac,invjac)

!write(*,*) result(1,1),result(1,2),result(2,1),result(2,2)

ENDDO

!read(*,*)

! CALL fluxeswb(j,FLUX(1,j),FLUX(2,j), SL, SR, WAFLIM)


!
! -----------------------------------------------------------------
! Computation of the TVD WAF intercell flux ends
! -----------------------------------------------------------------
!
END DO ciclo_tvd
!pause 'ciclo_tvd'

RETURN
END SUBROUTINE wafflu
!----------------------------------------------------------------


SUBROUTINE CS_evolution
!AIM:
!Evolution of the conservative variabiles
!UTILIZZA: 
!DATA : 29/01/2007				DD  
USE comuni
IMPLICIT NONE

INTEGER(ik)::k,j
REAL(rk), DIMENSION(3)::sourcei

DO J=2,nsezi-1
   CALL sources(j,q(j),d(j),a(j),1,sourcei) 
   DO K=1,2
      CS(K,J) = CS(K,J) + 0.5_rk*dTime*sourcei(K)
   END DO
END DO


END SUBROUTINE CS_evolution

!-------------------------------------------------------------------------


SUBROUTINE fluxes(j,qq,aa,FFA,FFQ)

!C	purpose: valutazione dei flussi

  USE comuni
  IMPLICIT NONE 

  INTEGER (kind=ik), INTENT (in)  :: j
  REAL    (kind=rk), INTENT (in)  :: aa,qq
  REAL    (kind=rk), INTENT (out)  :: FFA,FFQ

  REAL    (kind=rk) :: I1,yu,beta

  INTERFACE	 
     REAL (kind=rk) FUNCTION area2depth(j,aa)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: aa
     END FUNCTION area2depth
END INTERFACE


yu = area2depth(j,aa)

 CALL calcolabeta(j,yu+e(j),beta)

 CALL int1(j,aa,I1)

FFA = qq 
FFQ = beta*qq*abs(qq)/aa + gi*I1
 
 !FFQ = qq*abs(qq)/aa + gi*I1

 
RETURN
END SUBROUTINE  fluxes

!-----------------------------------------------------------------------------------------------

SUBROUTINE calcolabeta(j,hu,beta) 
!AIM:
!	calcolare beta 
!	j  = sezione
!	hu = altezza
!UTILIZZA:
!DATA : 29/01/2007				DD 
USE comuni
IMPLICIT NONE
 
REAL    (kind=rk) :: Int1stqq,beta,hu,betanum
REAL    (kind=rk) :: du,Int1st,area
REAL    (kind=rk) :: za,zb,dy,dz
INTEGER (kind=ik) :: j,i

du=hu-e(j)
betanum=0. 
Int1st=0. 
area=0.

DO i=1,npi(j)-1 
     za=hu-zi(i,j) 
     zb=hu-zi(i+1,j) 
     dy=yi(i+1,j)-yi(i,j) 
     dz=zb-za 
     IF(za.lt.0.) THEN 
        IF(dz.ne.0.) dy=dy*zb/dz 
        dz=zb 
        za=0. 
     ENDIF
     IF(zb.lt.0.) THEN 
        dy=-dy*za/dz 
        dz=-za 
        zb=0. 
     ENDIF
     area = area+0.5*(za+zb)*dy 
     betanum=betanum + 0.5*(rks(i,j)**2*za**st + rks(i+1,j)**2.*zb**st)*dy
     Int1st = Int1st   + 0.5*(rks(i,j)*za**ct+rks(i,j)*zb**ct)*dy      
 ENDDO
     
  beta=(betanum/(Int1st)**2)*area

     
RETURN
END SUBROUTINE calcolabeta
!-------------------------------------------------------
SUBROUTINE int1(j,aa,i1) 
!AIM:
!    calcola integrale I1 eq moto
!UTILIZZA: 
!DATA : 29/01/2007				DD 
USE comuni
IMPLICIT NONE

INTEGER (kind=ik) :: j,i
REAL    (kind=rk) :: dy,dz,aa
REAL    (kind=rk) :: dd,j1y,i1 
      
INTERFACE
 REAL (kind=rk) FUNCTION area2depth(j,aa)
   USE comuni
   INTEGER (kind=ik), INTENT (in)  :: j
   REAL    (kind=rk), INTENT (in)  :: aa
 END FUNCTION area2depth
END INTERFACE  

dd=area2depth(j,aa) 

i=1
DO WHILE(etai(i,j)<=dd.and.i<=npi(j))
    i=i+1
ENDDO
   i=i-1
  
  IF(dd < etai(i,j)) STOP 'int1'

  j1y  = j1i(i,j) + ut*ca(i,j)*(dd**3.-etai(i,j)**3.) +&
       0.5*(sigm(i,j)-ca(i,j)*etai(i,j))*(dd**2.-etai(i,j)**2.) 
 
  i1 = aa*dd - j1y 
   
RETURN 
END SUBROUTINE int1 
!----------------------------------------------------
!*
SUBROUTINE SUPERA(R_F, C_F, A_F)
!*
!C     Purpose: to evaluate a WAF limiter A based on the
!C              SUPERB flux limiter B
!*
  USE comuni      
  IMPLICIT NONE

!*
!C     Declaration of variables
!*
  REAL  (kind=rk)                ::B_F
  REAL  (kind=rk), INTENT(in)    ::R_F
  REAL  (kind=rk), INTENT(in)    ::C_F
  REAL  (kind=rk), INTENT(out)   ::A_F


!*
  B_F = MAX(0.0, MIN(2.0_rk*R_F, 1.0_rk), MIN(R_F, 2.0_rk))
!*
!C     Transform to WAF limiter
!*
  A_F = 1.0_rk - (1.0_rk - C_F)*B_F
!*
END SUBROUTINE SUPERA
!*
!*----------------------------------------------------------------------*
!*
SUBROUTINE VANLEE(R_F, C_F, A_F)
!*
!C     Purpose: to evaluate a WAF limiter A based on the
!C               van Leer flux limiter B
!*
  USE comuni
  IMPLICIT NONE
!*
!C     Declaration of variables
!*
  REAL  (kind=rk)                ::B_F
  REAL  (kind=rk), INTENT(in)    ::R_F
  REAL  (kind=rk), INTENT(in)    ::C_F
  REAL  (kind=rk), INTENT(out)   ::A_F
!*
  IF(R_F.LE.0.0)THEN
     B_F = 0.0
  ELSE
     B_F = 2.0*R_F/(1.0 + R_F)
  ENDIF
!*
!C     Transform to WAF limiter
!*
  A_F = 1.0 - (1.0 - C_F)*B_F
!*
END SUBROUTINE VANLEE
!*
!*----------------------------------------------------------------------*
!*
SUBROUTINE VANALB(R_F, C_F, A_F)
!*
!C     Purpose: to evaluate a WAF limiter A based on the
!C               van Albada flux limiter B
!*
  USE comuni
  IMPLICIT NONE
!*
!C     Declaration of variables
!*
  REAL  (kind=rk)                ::B_F
  REAL  (kind=rk), INTENT(in)    ::R_F
  REAL  (kind=rk), INTENT(in)    ::C_F
  REAL  (kind=rk), INTENT(out)   ::A_F
!*
  B_F = MAX(0.0, R_F*(1.0 + R_F)/(1.0 + R_F*R_F))
!*
!C     Transform to WAF limiter
!*
  A_F = 1.0 - (1.0 - C_F)*B_F
!*
END SUBROUTINE VANALB
!*
!*----------------------------------------------------------------------*
!*----------------------------------------------------------------------*
!*
SUBROUTINE MINAAA(R_F, C_F, A_F)
!*
!C     Purpose: to evaluate a WAF limiter A based on the
!C              MINMOD flux limiter B
!*
  USE comuni
  IMPLICIT NONE

!C     Declaration of variables
!*
  REAL  (kind=rk)                ::B_F
  REAL  (kind=rk), INTENT(in)    ::R_F
  REAL  (kind=rk), INTENT(in)    ::C_F
  REAL  (kind=rk), INTENT(out)   ::A_F
!*
  B_F = MAX(0.0, MIN(R_F, 1.0))
!*
!C     Transform to WAF limiter
!*
  A_F = 1.0 - (1.0 - C_F)*B_F
!*
END SUBROUTINE MINAAA
!*

!*----------------------------------------------------------------------*

SUBROUTINE update

!C purpose: update la soluzione attraverso la formula conservativa


  USE comuni
  IMPLICIT NONE

  INTEGER   (kind=ik) :: j,k
  REAL      (kind=rk) :: deltax,dtodx,dader, di, bi, dbd,V, sf
  REAL(rk),DIMENSION(2)::source, CSmid
  REAL(rk),DIMENSION(2,nsezi)::sourceterm, sourcemid

  INTERFACE	 
     REAL (kind=rk) FUNCTION area2depth(j,aa)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: aa
     END FUNCTION area2depth
   REAL (kind=rk) FUNCTION peloliberoi(passo_j,etaa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j   
     REAL    (kind=rk), INTENT (in)  :: etaa
   END FUNCTION peloliberoi
  END INTERFACE


  DO j=1,nsezi-1
     ! Calcolo U i+1/2
     CSmid(1)=0.5_rk*(CS(1,J)+CS(1,J+1))
     CSmid(2)=0.5_rk*(CS(2,J)+CS(2,J+1))
    
    di=0.5_rk*(d(j)+d(j+1))
    !di = area2depth(j,CSmid(1))

 bi = (peloliberoi(j,d(j))+ peloliberoi(j+1,d(j+1)))*0.5_rk



     dbd=(peloliberoi(j+1,d(j+1)) - peloliberoi(j,d(j)))/(si(j+1)-si(j))

     CALL sourceswb(j,CS(2,J),di,CS(1,J),1,CSmid,bi,dbd,source,V, sf)
     
     sourceterm(1,J)=source(1)
     sourceterm(2,J)=source(2)   

  !   IF(MOD(nn,itout) == 0) THEN
!     write(*,*) sf,- (e(j+1)-e(j))/(si(j+1)-si(j)),j
!read(*,*)
!     END IF

  END DO


  DO j=2,nsezi-1

       DO K=1,2
	sourcemid(K,J)=(sourceterm(K,J)+sourceterm(K,J-1))*0.5_rk
      END DO
!write(*,*) sourceterm(1,j),sourcemid(1,J),sourceterm(2,j),sourcemid(2,J)



!     write(*,'(i4,1x,5(f12.6,1x))') j,source(1),source(2),source(3) 
     deltax = 0.5*(dx(j-1)+dx(j))
     dtodx  = dTime/deltax

     a(j) = a(j) -dtodx*(FLUX(1,j) - FLUX(1,j-1)) + dTime*sourcemid(1,j)
     q(j) = q(j) -dtodx*(FLUX(2,j) - FLUX(2,j-1)) + dTime*sourcemid(2,j)
     d(j) = area2depth(j,a(j))
     u(j) = q(j)/a(j)
!write(*,'(i4,1x,6(f12.6,1x))') j,sourcemid(1,j) ,sourcemid(2,j),FLUX(1,j),FLUX(2,j),FLUX(1,j-1),FLUX(2,j-1)
!write(*,*) j,a(j),q(j)
!write(*,*) FLUX(1,j) - FLUX(1,j-1),sourcemid(1,J)*dx(j),FLUX(2,j) - FLUX(2,j-1),sourcemid(2,J)*dx(j)
!read(*,*)
  ENDDO
!
!read(*,*)
!pause'update'


RETURN
END SUBROUTINE update
!-------------------------------------------------------------------
!-------------------------------------------------------------------
SUBROUTINE ADER_state(i,qi,ai,tempi,CSADER1,CSADER2) 
!AIM:
!    Valutazione dei flussi e dei termini sorgente per la forma  
!    conservativa delle equazioni 
!
!UTILIZZA: 
!DATA : 29/01/2007				DD  
USE comuni
IMPLICIT NONE

INTEGER(ik), INTENT(in) :: i
REAL(rk), INTENT(in)::qi,ai,tempi
REAL(rk), INTENT(out)::CSADER1,CSADER2
INTEGER(ik)::k,j
REAL(rk), DIMENSION(2)::deltai,sourcei,AQD
REAL(rk), DIMENSION(2,2)::JAC
REAL(rk) :: bin,din,dxm

INTERFACE	 
   REAL (kind=rk) FUNCTION area2depth(j,aa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: j
     REAL    (kind=rk), INTENT (in)  :: aa
   END FUNCTION area2depth
   REAL (kind=rk) FUNCTION peloliberoi(passo_j,etaa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j   
     REAL    (kind=rk), INTENT (in)  :: etaa
   END FUNCTION peloliberoi
END INTERFACE

din = area2depth(i,ai) 
bin = peloliberoi(i,din)
JAC(1,1) = 0.0_rk
JAC(1,2) = 1.0_rk
JAC(2,1) = gi*ai/bin-(qi/ai)**2
JAC(2,2) = 2.0_rk*qi/ai

IF(i == 1)THEN
   dxm = dx(1)
do k=1,2
   deltai(k)=0.0_rk
end do
ELSE
   dxm = 0.5*(dx(i-1)+dx(i))
do k=1,2
   deltai(k)=(STATE(k,i)-STATE(k,i-1))/dxm
end do
ENDIF


DO J=1,2
   AQD(J) = 0.0_rk
   DO K=1,2
      AQD(J) = AQD(J) + JAC(J,K)*deltai(K)
   END DO
END DO

controllo=1
CALL sources(i,q(i),d(i),a(i),1,sourcei) 

CSADER1 = ai       + 0.5_rk*dTime*(-AQD(1) + sourcei(1))
CSADER2 = qi       + 0.5_rk*dTime*(-AQD(2) + sourcei(2))

END SUBROUTINE ADER_state
!-------------------------------------------------------------------
SUBROUTINE sourceswb(i,qi,di,ai,tipo,CSint,bi,dbd,source,VV, sf) 
!AIM:
!    Valutazione dei flussi e dei termini sorgente per la forma  
!    conservativa delle equazioni 
!
!UTILIZZA: 
!DATA : 29/01/2007				DD  
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik), INTENT (in) :: tipo,i
REAL      (kind=rk) :: di,qi,ai,bi,dbd
REAL      (kind=rk), DIMENSION(2), INTENT (in):: CSint
REAL      (kind=rk), DIMENSION(2), INTENT (out):: source
REAL      (kind=rk) :: I2d,I2u,I2,I1,I2p,I2m,Iv,Ivi,Ivp,VV,Ivimed,Ivimedi,Ivimedp
REAL      (kind=rk) :: s0,s0u,s0d,sfi, sfp
REAL      (kind=rk) :: dxm,sf,I1i,I1p,I1m,I1u,I1d,dI1,dYd,dYu,dY
REAL      (kind=rk) :: hsource
REAL      (kind=rk) :: DHA     !HEAT FLUX AIR-WATER

  INTERFACE	 
     REAL (kind=rk) FUNCTION peloliberoi(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION peloliberoi
  END INTERFACE

! Calcolo termine frizionale con valori medi

!NAIF
 hsource=0.5_rk*(e(i)+e(i+1))+di
 CALL calcolaSf(i,hsource,CSint(2),sf) 

!MAURELGOTAL+BUMP
sf=0.0_rk

!NAIF FLUSSI
! hsource=0.5_rk*(e(i)+e(i+1))+di
!if (nn .lt. 100) then
! CALL calcolaSf(i,hsource,CSint(2),sf) 
!else
!if (i==nsezi-1) then
! CALL calcolaSf(i,hsource,CSint(2),sf) 
! else
!CALL calcolaSf(i,hsource,0.5_rk*(flux(1,i)+flux(1,i+1)),sf) 
!end if
!end if


!NAIF 2 FLUSSI
!if (nn==1) then
! CALL calcolaSf(i,e(i)+di,CSint(2),sf) 
!else
!if (i==nsezi-1) then
! CALL calcolaSf(i,e(i)+di,CSint(2),sf) 
! else
! CALL calcolaSf(i,e(i)+di,0.5_rk*(flux(1,i)+flux(1,i+1)),sf) 
!end if
!end if


!MEDIATA FLUSSI (grandezze medie)
!if (nn==1) then
! CALL calcolaSf(i,e(i)+di,CSint(2),sfi) 
! CALL calcolaSf(i+1,e(i+1)+di,CSint(2),sfp) 
! sf=0.5_rk*(sfi+sfp)
!else
!if (i==nsezi-1) then
! CALL calcolaSf(i,e(i)+di,CSint(2),sfi) 
!CALL calcolaSf(i+1,e(i+1)+di,CSint(2),sfp) 
! sf=0.5_rk*(sfi+sfp)
!else
! CALL calcolaSf(i,e(i)+di,0.5_rk*(flux(1,i)+flux(1,i+1)),sfi) 
! CALL calcolaSf(i+1,e(i+1)+di,0.5_rk*(flux(1,i)+flux(1,i+1)),sfp) 
! sf=0.5_rk*(sfi+sfp)
!end if
!end if


!NAIF SQUARE
!hsource=0.5_rk*(e(i)+e(i+1))+di
!if (nn .lt. 2000) then
! CALL calcolaSf(i,hsource,CSint(2),sf) 
!else
!if (i==nsezi-1) then
! CALL calcolaSf(i,hsource,CSint(2),sfi) 
! CALL calcolaSf(i+1,hsource,CSint(2),sfp) 
! sf=0.5_rk*(sfi+sfp)
!else
! CALL calcolaSf(i,hsource,CSint(2),sfi) 
! CALL calcolaSf(i+1,hsource,CSint(2),sfp) 
! sf=0.5_rk*(sfi+sfp)
!end if
!end if

!MEDIATA FLUSSI
!if (nn==1) then
! CALL calcolaSf(i,e(i)+d(i),CS(2,i+1),sfi) 
! CALL calcolaSf(i+1,e(i+1)+d(i+1),CS(2,i+1),sfp) 
! sf=0.5_rk*(sfi+sfp)
!else
!if (i==nsezi-1) then
! CALL calcolaSf(i,e(i)+d(i),CS(2,i+1),sfi) 
!CALL calcolaSf(i+1,e(i+1)+d(i+1),CS(2,i+1),sfp) 
! sf=0.5_rk*(sfi+sfp)
!else
! CALL calcolaSf(i,e(i)+d(i),0.5_rk*(flux(1,i)+flux(1,i+1)),sfi) 
! CALL calcolaSf(i+1,e(i+1)+d(i+1),0.5_rk*(flux(1,i)+flux(1,i+1)),sfp) 
! sf=0.5_rk*(sfi+sfp)
!end if
!end if



!if (nn==5000) then
!write(*,*) sf, csint(2)**2/(csint(1)**2*rks(1,1)**2*(csint(1)/(20._rk+2._rk*di))**(4._rk/3._rk))
!read(*,*)
!end if
!write(*,*) sf
!if(nn ==3000) then
!write(*,*) sf
!read(*,*)
!end if


   call int1(i,a(i),I1i)
   call int1(i+1,a(i+1),I1p)
   I1d=(I1p-I1i)/(si(i+1)-si(i))
   dYd=(d(i+1)-d(i))/(si(i+1)-si(i))

!RETTANGOLARE NON CILINDRICO
!    I2=0.5_rk*CSint(1)**2/bi**2*dbd

   I2=I1d-CSint(1)*dYd

 !   call intv(i,a(i),Ivi)
 !  call intvmed(i,a(i),Ivimedi)
 !  call intvmed(i,a(i+1),Ivimedp)
 !  Ivimed=0.5_rk*(Ivimedi+Ivimedp)
 
  Ivimed=(a(i+1)-a(i))/(si(i+1)-si(i))-bi*dYd

 !write(*,*) 	Ivi
!   call intv(i+1,a(i+1),Ivp)
!   Iv=Ivi
!  Iv=dbd*0.5_rk*(d(i)+d(i+1))

!write(*,*) Ivi, 'Iv',ivimed, 'ivimed', Iv, 'Iv_rett'
!read(*,*)
!   VV=-0.5_rk*gi*CSint(1)**2/bi**2*dbd  !VVrett

   VV=gi*I2-gi*CSint(1)/bi*Ivimed

!   VV=gi*I2-gi*(0.5_rk*(d(i)+d(i+1)))*Iv
!write(*,*) CSint(1),bi, 0.5_rk*(d(i)+d(i+1))
!read(*,*)

!write(*,*) gi*I2, -gi*CSint(1)/bi*Iv, Iv
!write(*,*) VV, 'VV', -0.5_rk*gi*csint(1)**2/bi**2*dbd,'VVrett'

!IF (i==2 .or. i==nsezi-1) then
!  s0u = - (e(i)-e(i-1))/(si(i) - si(i-1))
   
   s0 = - (e(i+1)-e(i))/(si(i+1) - si(i))

!   s0=0.5_rk*(s0u+s0d)
!else 
! s0= - (-e(i+2)+8_rk*e(i+1)-8_rk*e(i-1)+e(i-2))/(12_rk*(si(i) - si(i-1)))
!end if


source(1)  = 0. !qaff(i)/dxm
						     
source(2)  = gi*(I2 + CSint(1)*(s0 - sf)) !+ 0.2*(flux(1,i)/Ai)*(qaff(i)/dxm)


!source(2)  = gi*(I2 + ai*(s0 - sf)) + (Q(i)/A(i))*(qaff(i)/dxm)


RETURN
END SUBROUTINE  sourceswb
!-----------------------------------------------------

SUBROUTINE sources(i,qi,di,ai,tipo,source) 
!AIM:
!    Valutazione dei flussi e dei termini sorgente per la forma  
!    conservativa delle equazioni 
!
!UTILIZZA: 
!DATA : 14/07/2009				DD  
USE comuni
IMPLICIT NONE

INTEGER   (kind=ik) :: tipo,i
REAL      (kind=rk) :: di,qi,ai,I2d,I2u,I2,I1,I2p,I2m
REAL      (kind=rk) :: s0,s0u,s0d
REAL      (kind=rk) :: dxm,sf,I1i,I1p,I1m,I1u,I1d,dI1,dYd,dYu,dY
REAL      (kind=rk) :: source(2)
REAL      (kind=rk) :: DHA     !HEAT FLUX AIR-WATER

  INTERFACE	 
     REAL (kind=rk) FUNCTION peloliberoi(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION peloliberoi
  END INTERFACE

!CALL calcolaSf(i,e(i)+di,qi,sf)
CALL calcolaSf(i,e(i)+di,flux(1,i),sf)


IF(i == 1)THEN
   dxm = dx(1)
   s0 = - (e(i+1)-e(i))/(si(i+1) - si(i))
   CALL int2(i,ai,1,I2)
ELSE
   dxm = 0.5*(dx(i-1)+dx(i))
!   s0 = - (e(i+1)-e(i))/(si(i+1) - si(i))
!   CALL int2(i,ai,1,I2)
   call int1(i,a(i),I1i)
   call int1(i+1,a(i+1),I1p)
   call int1(i-1,a(i-1),I1m)
   I1u=(I1i-I1m)/(si(i)-si(i-1))
   I1d=(I1p-I1i)/(si(i+1)-si(i))
   dI1=0.5_rk*(I1u+I1d)
   dYd=(d(i+1)-d(i))/(si(i+1)-si(i))
   dYu=(d(i)-d(i-1))/(si(i)-si(i-1))
   dY=0.5_rk*(dYd+dYu)
   I2=dI1-ai*dY
!write(*,*) 'up',I2
!   CALL int2(i,ai,-1,I2u)
!   CALL int2(i,ai,1,I2d)
!   I2=0.5_rk*(I2u+I2d)
!write(*,*) I2
!   s0u = - (e(i)-e(i-1))/(si(i) - si(i-1))
!   s0d = - (e(i+1)-e(i))/(si(i+1) - si(i))
!   s0=0.5_rk*(s0u+s0d)
!   s0=S0u
!   s0=S0d
!   s0=- (e(i+1)-e(i-1))/(si(i+1) - si(i-1))

IF (i==2 .or. i==nsezi-1) then
   s0u = - (e(i)-e(i-1))/(si(i) - si(i-1))
   s0d = - (e(i+1)-e(i))/(si(i+1) - si(i))
   s0=0.5_rk*(s0u+s0d)

else 
 !if (i .gt. 65 .and. i .lt. 75) then
 s0= - (-e(i+2)+8_rk*e(i+1)-8_rk*e(i-1)+e(i-2))/(12_rk*(si(i) - si(i-1)))
 !ELSE
 !s0u = - (e(i)-e(i-1))/(si(i) - si(i-1))
 !s0d = - (e(i+1)-e(i))/(si(i+1) - si(i))
 !s0=0.5_rk*(s0u+s0d)
 !END IF
!s0u = - (3_rk*e(i)-4_rk*e(i-1)+e(i-2))/(si(i) - si(i-2))
!s0d = - (-3_rk*e(i)+4_rk*e(i+1)-e(i+2))/(si(i+2) - si(i))
!s0=s0d!0.5_rk*(s0u+s0d)
end if


ENDIF

source(1)  = 0. !qaff(i)/dxm
						     
source(2)  = gi*(I2 + ai*(s0 - sf)) !+ 0.2*(flux(1,i)/Ai)*(qaff(i)/dxm)
!source(2)  = gi*(I2 + ai*(s0 - sf)) + (Q(i)/A(i))*(qaff(i)/dxm)

!IF (controllo==1 .AND. 	MOD(nn,itout) == 0) THEN
!	 WRITE(111,1111)  i, dI1, I2, s0*100, e(i), ai
!	 1111 format (i3,3(3x,f6.3),2(3x,f7.2))
!END IF

RETURN
END SUBROUTINE  sources

!-----------------------------------------------------------------------------------

SUBROUTINE int2(j,aa,jc,I2)
!AIM:
!    calcola integrale I2 eq moto
!	 DATA : 26/01/2007				DD
!UTILIZZA: FUNCTION area2depth FUNCTION pelolibero 
USE comuni
IMPLICIT NONE
!

INTEGER (kind=ik) :: j,jc,il,i,passo_j
REAL    (kind=rk) :: aa,i2
REAL    (kind=rk) :: dGB,j2ay,j2by,i2n,dd
REAL    (kind=rk) :: etaa,etab,deltaeta,dsdxa,dsdxb

INTERFACE	 
   REAL (kind=rk) FUNCTION area2depth(passo_j,aa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j 
     REAL    (kind=rk), INTENT (in)  :: aa
   END FUNCTION area2depth
   REAL (kind=rk) FUNCTION peloliberoi(passo_j,etaa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j   
     REAL    (kind=rk), INTENT (in)  :: etaa
   END FUNCTION peloliberoi
END INTERFACE 

  dd=area2depth(j,aa)

  il=1 		                ! stabilisco tra quali eta
  DO WHILE(etai(il,j)<=dd.and.il<=npi(j))		! si triva in tirante
     il=il+1
  ENDDO
  il=il-1

  i2n=0.
  DO i=1,il-1
     etaa=etai(i,j)
     etab=etai(i+1,j)
     passo_j=j+jc
	 deltaeta=etab-etaa
     dsdxa=(sigm(i,j)-peloliberoi(passo_j,etaa))/(si(j)-si(j+jc))
     dsdxb=(sigm(i+1,j)-peloliberoi(passo_j,etab))/(si(j)-si(j+jc))
     i2n=i2n+.5*((dd-etaa)*dsdxa+(dd-etab)*dsdxb)*deltaeta
  ENDDO
  passo_j=j+jc
  etaa=etai(il,j)
  deltaeta=dd-etaa
  dsdxa=(sigm(il,j)-peloliberoi(passo_j,etaa))/(si(j)-si(j+jc))
  i2n=i2n+.5*(dd-etaa)*dsdxa*deltaeta
  i2=i2n
RETURN
END SUBROUTINE int2
!-----------------------------------------------------------------------------------

SUBROUTINE intv(j,aa,Iv)
!AIM:
!    calcola integrale I2 eq moto
!	 DATA : 26/01/2007				DD
!UTILIZZA: FUNCTION area2depth FUNCTION pelolibero 
USE comuni
IMPLICIT NONE
!

INTEGER (kind=ik) :: j,jc,il,i,passo_j
REAL    (kind=rk) :: aa,Iv
REAL    (kind=rk) :: dGB,jvay,jvby,ivn,dd
REAL    (kind=rk) :: etaa,etab,deltaeta,dsdxa,dsdxb

INTERFACE	 
   REAL (kind=rk) FUNCTION area2depth(passo_j,aa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j 
     REAL    (kind=rk), INTENT (in)  :: aa
   END FUNCTION area2depth
   REAL (kind=rk) FUNCTION peloliberoi(passo_j,etaa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j   
     REAL    (kind=rk), INTENT (in)  :: etaa
   END FUNCTION peloliberoi
END INTERFACE 

  dd=area2depth(j,aa)
  jc=1
  il=1 		                                        ! stabilisco tra quali eta
  DO WHILE(etai(il,j)<=dd.and.il<=npi(j))		! si trova il tirante
     il=il+1
  ENDDO
  il=il-1

  ivn=0.
  DO i=1,il-1
     etaa=etai(i,j)
     etab=etai(i+1,j)
     passo_j=j+jc
	 deltaeta=etab-etaa
     dsdxa=(sigm(i,j)-peloliberoi(passo_j,etaa))/(si(j)-si(j+jc))
     dsdxb=(sigm(i+1,j)-peloliberoi(passo_j,etab))/(si(j)-si(j+jc))
     ivn=ivn+(dsdxa+dsdxb)*deltaeta
  ENDDO
  passo_j=j+jc
  etaa=etai(il,j)
  deltaeta=dd-etaa
  dsdxa=(sigm(il,j)-peloliberoi(passo_j,etaa))/(si(j)-si(j+jc))
  ivn=ivn+dsdxa*deltaeta
  iv=ivn
RETURN
END SUBROUTINE intv
!------------------------------------------------------------------
!-----------------------------------------------------------------------------------

SUBROUTINE intvmed(j,aa,Iv)
!AIM:
!    calcola integrale I2 eq moto
!	 DATA : 26/01/2007				DD
!UTILIZZA: FUNCTION area2depth FUNCTION pelolibero 
USE comuni
IMPLICIT NONE
!

INTEGER (kind=ik) :: j,jc,il,i,passo_j
REAL    (kind=rk) :: aa,Iv
REAL    (kind=rk) :: dGB,jvay,jvby,ivn,dd
REAL    (kind=rk) :: etaa,etab,deltaeta,dsdxa,dsdxb

INTERFACE	 
   REAL (kind=rk) FUNCTION area2depth(passo_j,aa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j 
     REAL    (kind=rk), INTENT (in)  :: aa
   END FUNCTION area2depth
   REAL (kind=rk) FUNCTION peloliberoi(passo_j,etaa)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: passo_j   
     REAL    (kind=rk), INTENT (in)  :: etaa
   END FUNCTION peloliberoi
END INTERFACE 

  dd=area2depth(j,aa)
  jc=1
  il=1 		                                        ! stabilisco tra quali eta
  DO WHILE (etai(il,j)<=dd.and.il<=npi(j))		! si trova il tirante
     il=il+1
  ENDDO
  il=il-1

  ivn=0.
  DO i=1,il-1
     etaa=etai(i,j)
     etab=etai(i+1,j)
     passo_j=j+jc
	 deltaeta=etab-etaa
     dsdxa=(sigm(i,j)-peloliberoi(passo_j,etaa))/(si(j)-si(j+jc))
     dsdxb=(sigm(i+1,j)-peloliberoi(passo_j,etab))/(si(j)-si(j+jc))
     ivn=ivn+(dsdxa+dsdxb)*deltaeta
  ENDDO
  passo_j=j+jc
  etaa=etai(il,j)
  deltaeta=dd-etaa
  dsdxa=(sigm(il,j)-peloliberoi(passo_j,etaa))/(si(j)-si(j+jc))
  ivn=ivn+dsdxa*deltaeta
  iv=ivn
RETURN
END SUBROUTINE intvmed
!------------------------------------------------------------------
SUBROUTINE calcolaSf(j,hu,qu,sf_sub) 
!AIM: 
!	Calcolo delle perdite di carico Sf
!	
!UTILIZZA: 
!DATA : 28/01/2007				DD  
USE comuni
IMPLICIT NONE

INTEGER    (kind=ik) :: i,j
REAL       (kind=rk) :: za,zb,hu,dy,dz,qu
REAL       (kind=rk) :: Int1st,sf_sub
REAL(kind=rk)::cosal(npi(j)) 

DO i=1,npi(j) -1		   !C calcolo cos(a)

cosal(i)	=	ABS((yi(i+1,j)-yi(i,j))/  &
				(SQRT((yi(i+1,j)-yi(i,j))**2+(zi(i+1,j)-zi(i,j))**2)))
cosal(i)=1.0_rk
ENDDO 


Int1st=0. 
DO i=1,npi(j)-1 
   za=hu-zi(i,j) 
   zb=hu-zi(i+1,j) 
   dy=yi(i+1,j)-yi(i,j) 
   dz=zb-za 
   IF(za.lt.0.) THEN
      if(dz.ne.0.) dy=dy*zb/dz 
      dz=zb 
      za=0. 
   ENDIF
   IF(zb.lt.0.) THEN 
      dy=-dy*za/dz 
      dz=-za 
      zb=0. 
   ENDIF
   Int1st = Int1st + 0.5_rk*(rks(i,j)*cosal(i)**(2.0_rk/3.0_rk)*za**ct+rks(i+1,j)*cosal(i)**(2.0_rk/3.0_rk)*zb**ct)*dy 
ENDDO   

sf_sub = Qu*abs(Qu)/(Int1st**2)   

RETURN 
END SUBROUTINE calcolaSf
!----------------------------------------------------------------
SUBROUTINE output
!AIM:
!Scrive file di output per disegnera in gnuplot
!UTILIZZA:
!DATA : 08/01/2007				
USE comuni
IMPLICIT NONE

CHARACTER (7)					 ::testo 
INTEGER   (kind=ik)              ::j
REAL      (kind=rk)				 ::Fr,dcrit
REAL      (kind=rk)				 ::bj
REAL      (kind=rk)				 ::fluxa,fluxq

INTERFACE	 
   REAL (kind=rk) FUNCTION dcritica(ji,qq)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: ji
     REAL    (kind=rk), INTENT (in)  :: qq
   END FUNCTION dcritica
END INTERFACE
INTERFACE	 
	 REAL (kind=rk) FUNCTION peloliberoi(j,dd)
       USE comuni
       INTEGER (kind=ik), INTENT (in)  :: j
       REAL    (kind=rk), INTENT (in)  :: dd
     END FUNCTION peloliberoi
END INTERFACE


WRITE(313,*) nn, time


WRITE(testo,'(i7.7)') nn
WRITE(path,'(1a)') trim(path_in)
nome=trim(path)//"/res"//testo//".oro"//char(0) 
OPEN(unit=314,file=nome,action='write',status='unknown')
DO j=1,nsezi
WRITE(314,*) h(j), q(j)
END DO
CLOSE(314)

!WRITE(testo,'(i7.7)') nn
!nome="./output/"//testo//".dat"//char(0) 
!OPEN(unit=201,file=nome)

!nome="./output/"//testo//".datb"//char(0) 
!OPEN(unit=203,file=nome,form='unformatted')

!WRITE (201,&
!      '(2x,"#t[h]:",f6.3," t[s]:",f10.1," dt[s]:",f10.5)') &
!      time/3600.,time,dtime
!WRITE(201,*)'#1 waf'


DO j=1,nsezi 
   CALL froude(j,q(j),d(j),Fr) 

!   dcrit	= dcritica(j,q(j))
   bj		= peloliberoi(j,d(j))

!   WRITE (201,'(11(1x,e12.6))')si(j),q(j),d(j),e(j),e(j)+d(j),a(j),Fr,flux(1,j)
!   WRITE (203)q(j),e(j),e(j)+d(j)
ENDDO

!CLOSE(201)
!CLOSE(203)

RETURN
END SUBROUTINE output
!------------------------------------------------------------
SUBROUTINE froude(j,qq,dd,Fr)
USE comuni
IMPLICIT NONE
!AIM:
!	calcola il numero di froude di una sezione
!	DATA : 05/02/2007				DD 	
!	UTILIZZA: 

INTEGER (kind=ik)				:: j
REAL    (kind=rk)				:: aa,bb,uF,qq,cel
REAL    (kind=rk)				:: Fr,dd
  
INTERFACE
 REAL (kind=rk) FUNCTION depth2area(j,dd)
   USE comuni
   INTEGER (kind=ik), INTENT (in)  :: j
   REAL    (kind=rk), INTENT (in)  :: dd
 END FUNCTION depth2area
END INTERFACE

INTERFACE
 REAL (kind=rk) FUNCTION peloliberoi(j,dd)
   USE comuni
   INTEGER (kind=ik), INTENT (in)  :: j
   REAL    (kind=rk), INTENT (in)  :: dd
 END FUNCTION peloliberoi
END INTERFACE
aa=depth2area(j,dd)
bb=peloliberoi(j,dd)
uF = qq/aa
cel = SQRT(gi*aa/bb)
Fr = ABS(uF)/cel
RETURN
END SUBROUTINE froude
!----------------------------------------------------------
REAL (kind=rk) FUNCTION depth2area (j,dd)
!purpose:
!	da l'altezza di una sezione si ricava l'Area bagnata 
!	j  = sezione
!	dd = altezza
!UTILIZZA: Suboutine calcola_area
!DATA : 26/01/2007				DD 

USE comuni
IMPLICIT NONE

INTEGER (kind=ik), INTENT (in)  :: j
REAL    (kind=rk), INTENT (in)  :: dd

  REAL (kind=rk) :: area


CALL calcola_area (j,dd+e(j),area)

  depth2area = area

END FUNCTION depth2area
!------------------------------------------------------------------------------------------
REAL (kind=rk) FUNCTION area2depth (j,area)
!AIM:
!	data l'area bagnata di una sezione calcola l'altezza del moto
!	j  = sezione
!	aa = area
!UTILIZZA: Suboutine calcola_area
!DATA : 29/01/2007				DD 
USE comuni
IMPLICIT NONE

INTEGER (kind=ik), INTENT (in)  :: j
REAL    (kind=rk), INTENT (in)  :: area

INTEGER (kind=ik)   :: iact,i
REAL    (kind=rk)   :: coefficiente_A,coefficiente_B,coefficiente_C
REAL    (kind=rk)   :: soluz1,soluz2
REAL    (kind=rk)   :: tolleranza = 1e-6
REAL    (kind=rk)   :: disc

IF(area < 0.) THEN
     write(*,*)j,si(j),area,nn
     stop 'area < 0'
ENDIF
  
  soluz1         = 0.0 
  soluz2         = 0.0 
  coefficiente_A = 0.0 
  coefficiente_B = 0.0 
  coefficiente_C = 0.0 
  area2depth     = 0.0
  
  
  iact = 1 
  DO i = 2 , npi(j)       
    IF(area >= omi(i,j))THEN 
      iact = i 
    ENDIF 
  ENDDO    

  coefficiente_A = ca(iact,j)/2.0 
  coefficiente_B = sigm(iact,j) - ca(iact,j)*etai(iact,j) 
  coefficiente_C = (ca(iact,j)/2.0)*(etai(iact,j)**2.0) - &
                   sigm(iact,j)*etai(iact,j) + omi(iact,j) - area
             
  IF (coefficiente_A>=tolleranza) THEN
    !equazione di secondo grado 
    disc=coefficiente_B**2.0 - 4*coefficiente_A*coefficiente_C
    IF(disc<0.) THEN
       write(6,*)'disc < 0',disc,si(j),area
    ENDIF
    soluz1 = (-coefficiente_B + sqrt(disc))/(2*coefficiente_A) 
    soluz2 = (-coefficiente_B - sqrt(disc))/(2*coefficiente_A) 
    IF (soluz1>=soluz2) THEN 
      area2depth = soluz1 
    ELSE 
      area2depth = soluz2 
    ENDIF 
    IF (area2depth<0.0) THEN 
       write(6,*)j,area,area2depth
       stop "area2depth: SOLUZIONE CON TIRANTE NEGATIVO." 
    ENDIF 
  ELSE 
    IF (coefficiente_B>=tolleranza) then
      area2depth = -coefficiente_C/coefficiente_B 
    ELSE
      write(*,*) "PROF COEFFICIENTE_B NULLO"
      STOP
    ENDIF
  ENDIF 
                       


RETURN
END FUNCTION area2depth
!--------------------------------------------------------
SUBROUTINE calcola_area (j,hu,area)
!AIM:
!	calcola l'area nota l'altezza 
!	j  = sezione
!	hu = quota
!UTILIZZA:
!DATA : 26/01/2007				DD 

USE comuni
IMPLICIT NONE

INTEGER (kind=ik), INTENT (in)  :: j
REAL    (kind=rk), INTENT (in)  :: hu
REAL    (kind=rk), INTENT (out) :: area
  
INTEGER (kind=ik) :: i
REAL    (kind=rk) :: za
REAL    (kind=rk) :: zb
REAL    (kind=rk) :: dy
REAL    (kind=rk) :: dz
  

  area    = 0.0_rk
  
  DO i = 1,npi(j)-1

     za      = hu-zi(i,j)
     zb      = hu-zi(i+1,j)

     IF ((za.GT.0.0_rk).OR.(zb.GT.0.0_rk)) THEN

        dy = yi(i+1,j)-yi(i,j)
        dz = zb-za

        IF (za.LT.0.0_rk) THEN

           dy = dy*zb/dz
           dz = zb
           za = 0.0_rk

        ENDIF

        IF (zb.LT.0.0_rk) THEN

           dy = -dy*za/dz
           dz = -za
           zb = 0.0_rk

        ENDIF

        area    = area    + 0.5_rk*(za+zb)*dy

     ENDIF

  ENDDO

END SUBROUTINE calcola_area
!-----------------------------------------------------------------------------
SUBROUTINE calcola_Rh (j,hu,Rh)
!AIM:
!	calcola l'area nota l'altezza 
!	j  = sezione
!	hu = quota
!UTILIZZA:
!DATA : 26/01/2007				DD 

USE comuni
IMPLICIT NONE

INTEGER (kind=ik), INTENT (in)  :: j
REAL    (kind=rk), INTENT (in)  :: hu
REAL    (kind=rk), INTENT (out) :: Rh
  
INTEGER (kind=ik) :: i
REAL    (kind=rk) :: za,zb,dy,dz,area,contorno_B

  area    = 0.0_rk
  contorno_B= 0.0_rk
  
  DO i = 1,npi(j)-1

     za      = hu-zi(i,j)
     zb      = hu-zi(i+1,j)

     IF ((za.GT.0.0_rk).OR.(zb.GT.0.0_rk)) THEN

        dy = yi(i+1,j)-yi(i,j)
        dz = zb-za

        IF (za.LT.0.0_rk) THEN

           dy = dy*zb/dz
           dz = zb
           za = 0.0_rk

        ENDIF

        IF (zb.LT.0.0_rk) THEN

           dy = -dy*za/dz
           dz = -za
           zb = 0.0_rk

        ENDIF

        area    = area    + 0.5_rk*(za+zb)*dy
        contorno_B = contorno_B + sqrt((za-zb)**2+dy**2)
     ENDIF

  ENDDO
  Rh = area/contorno_B 


END SUBROUTINE calcola_Rh
!------------------------------------------------------------------------------------------
REAL FUNCTION  peloliberoi(j,dd)

!purpose:	  Data la profondità dd in una sezione j
!	          calcola il pelo libero  	

USE comuni
IMPLICIT NONE

INTEGER (kind=ik),INTENT(in) :: j			!numero della sezione
REAL    (kind=rk),INTENT(in) :: dd		!profondità nella sezione


INTEGER   (kind=ik) :: i
REAL      (kind=rk) :: hu,za,zb,dy,dz

	
hu=e(j)+dd
peloliberoi  = 0.0 
DO i=1,npi(j)-1 
   za=hu-zi(i,j) 
   zb=hu-zi(i+1,j) 
   dy=yi(i+1,j)-yi(i,j) 
   dz=zb-za 
   IF(za.lt.0.) THEN 
      if(dz.ne.0.) dy=dy*zb/dz 
      dz=zb 
      za=0. 
   ENDIF
   IF(zb.lt.0.) THEN 
      dy=-dy*za/dz 
      dz=-za 
      zb=0. 
   ENDIF
   peloliberoi = peloliberoi + dy 
ENDDO


END FUNCTION peloliberoi 
!-------------------------------------------------------------------------------------
SUBROUTINE rgauss(aaa,f,x,n)
USE comuni
IMPLICIT NONE 
 
INTEGER   (kind=ik) :: ip(2),itemp
REAL      (kind=rk) :: aaa(2,2),f(2),x(2)
REAL      (kind=rk) :: t_r,sum


INTEGER   (kind=ik) :: nm1,np1,n,i,k,kp1
INTEGER   (kind=ik) :: j


nm1=n-1 
np1=n+1 
do i=1,n 
   ip(i)=i 
enddo
do k=1,nm1 
   kp1=k+1 
   do i=kp1,n 
      if(abs(aaa(ip(k),k)).lt.abs(aaa(ip(i),k)))then 
         itemp=ip(k) 
         ip(k)=ip(i) 
         ip(i)=itemp 
      endif
   enddo
   do i=kp1,n 
      t_r=aaa(ip(i),k) 
      if(abs(t_r)/=0.) then
         t_r=t_r/aaa(ip(k),k) 
         f(ip(i))=f(ip(i))-t_r*f(ip(k)) 
         do j=kp1,n 
            aaa(ip(i),j)=aaa(ip(i),j)-t_r*aaa(ip(k),j) 
         enddo
      endif
   enddo
enddo
do k=n,1,-1 
   sum=0.d+0 
   if(k.ne.n) then 
      kp1=k+1 
      do j=kp1,n 
         sum=sum+aaa(ip(k),j)*x(j) 
      enddo
   endif
   x(k)=(f(ip(k))-sum)/aaa(ip(k),k) 
enddo

RETURN
END SUBROUTINE rgauss
!---------------------------------------------------------

REAL (kind=rk) FUNCTION dcritica(ji,qq)
USE comuni
IMPLICIT NONE
!AIM:
!	calcola la critica in una sezione
!	DATA : 05/02/2007				DD 	
!	UTILIZZA: 

INTEGER (kind=ik)   :: i,ji
REAL    (kind=rk)   :: dd0,fr0,qq,fr1,dd
REAL    (kind=rk)   :: f0,f1,dd1
REAL    (kind=rk)   :: eps
REAL    (kind=rk)   :: rm,iter,fr,fz

dd0 = etai(npi(ji),ji)
	CALL froude(ji,qq,dd0,fr0)

f0=1.-fr0
IF(f0 < 0.)THEN
   WRITE(*,*) ji,'dcritica: froude a piene rive supercritico!'
   STOP 
ENDIF
  
i = 0
f1=1.e+36

DO WHILE (f1 > 0.)
   i=i+1
   rm=2.**(-i)
   dd1 = rm*dd0
		CALL froude(ji,qq,dd1,fr1)
    f1=1.-fr1
   IF(i>1000)STOP 'dcritica non convergente: ricerca estremo'
ENDDO

eps=1.e-3
iter = 0
dd=dd1-dd0

DO WHILE (ABS(dd) >= eps)
  iter = iter+1
  dd=.5*(dd0+dd1)
     CALL froude(ji,qq,dd,fr)
  fz=1.-fr
     IF(f0*fz > 0.)THEN
        dd0=dd
        f0=fz
     ELSE
        dd1=dd
        f1=fz
     ENDIF
     dd=dd1-dd0
     IF (iter>100) STOP 'dcritica non convergente: bisezione'
  ENDDO
  dcritica=.5*(dd0+dd1)

RETURN
END FUNCTION dcritica
!-----------------------------------------------------------------------
SUBROUTINE calcolaswj(j,hu,qu,su,wu)
USE comuni
IMPLICIT NONE
!AIM:
!	Calcolo coefficienti S per h,q,d,zt (soluzione completa)
!	DATA : 05/02/2007				DD 	
!	UTILIZZA:


INTEGER (kind=ik)				::i,j
REAL    (kind=rk)				::za,zb,dy,dz
REAL    (kind=rk)				::du,hu,area,b
REAL    (kind=rk)				::betum,beta,qu,betanum

REAL    (kind=rk)				::su(6),wu(3) 
REAL    (kind=rk)				::Int1st
 
  du=hu-e(j)
  area=0. 
  b=0. 
  betanum=0. 
  Int1st=0. 
  DO i=1,npi(j)-1 
     za=hu-zi(i,j)
     zb=hu-zi(i+1,j) 
     dy=yi(i+1,j)-yi(i,j) 
     dz=zb-za 
     IF(za.lt.0.) THEN
        IF(dz.ne.0.) dy=dy*zb/dz 
        dz=zb 
        za=0. 
     ENDIF
     if(zb.lt.0.) then 
        dy=-dy*za/dz 
        dz=-za 
        zb=0. 
     ENDIF
     area=area+0.5*(za+zb)*dy 
     b=b+dy 
     betanum=betanum + 0.5*(rks(i,j)**2*za**st + rks(i+1,j)**2.*zb**st)*dy     
     Int1st = Int1st   + 0.5*(rks(i,j)*za**ct+rks(i,j)*zb**ct)*dy      
  ENDDO
     
  beta=(betanum/(Int1st)**2)*area
     
     su(1)=1./b 
     su(2)=gi*area-beta*b*(1./area)**2.*qu*ABS(qu) 
     su(3)=2.*beta*Qu/area 
     su(4)=qu/area*SQRT(b/(gi*area)) 
     wu(1)=gi*area*Qu*ABS(Qu)/(Int1st**2.) 
     wu(2)=beta*(Qu/area)**2. 
     wu(3)=gi*area 
     
     
RETURN 
END SUBROUTINE calcolaswj
!-------------------------------------------------------------------------------------------

SUBROUTINE lettura_idrogramma

!C     Purpose: leggere idrogramma in ingresso

USE comuni
IMPLICIT NONE
INTEGER			(kind=ik) :: i
INTEGER			(kind=ik) :: status,j
INTEGER			(kind=ik) :: nvals = 0
REAL			(kind=rk) :: valore
CHARACTER(20)			  :: niet

WRITE(path,'(2a)') trim(path_in),'/discharge.ori'
!C lettura del numero di righe
OPEN(UNIT=33,FILE=trim(path),STATUS='old',ACTION='read',IOSTAT=status)

openif:IF (status == 0) THEN
   DO
      READ(33,*,IOSTAT=status)
      IF (status /= 0) EXIT
      nvals = nvals +1
   ENDDO
   readif:IF (status > 0) THEN
      WRITE(*,1020) nvals+1
1020  FORMAT ('0','errore di lettura nella riga idro',I6)
   ELSE
      
   ENDIF readif

ELSE openif
   WRITE(*,1040) nvals
1040 FORMAT ('','errore di apertura del file: IOSTAT = ',I6)
END IF openif
			
CLOSE(33)

nvals=nvals-1   !dipende da file 

ALLOCATE (td(nvals),STAT=status)
ALLOCATE (qd(nvals),STAT=status)
write(*,*) nvals, 'nvals idrogramma'
IF (status/=0) THEN
     WRITE(*,*) "Errore allocazione"
     STOP
ENDIF


OPEN(UNIT=33,FILE=trim(path),STATUS='old',ACTION='read',IOSTAT=status)
READ(33,*)
DO j=1,nvals
   if(allocated(qd))then
      READ(33,*) td(j),qd(j)
   else
      write(*,*)'Warning Array not allocated'
   end if
!   write(*,'(i6,1x,2(f18.12,1x))') j,td(j),qd(j)
ENDDO
!pause'td'
CLOSE(33)

idr_campi=nvals         !intervalli dell'idrogramma 
RETURN
END SUBROUTINE lettura_idrogramma
!--------------------------------------------------------------------------------------

SUBROUTINE letturaAff

!C     Purpose: leggere gli affluenti e lerelative sezioni di immisione

USE comuni
IMPLICIT NONE
INTEGER   (kind=ik) :: status,j,i
INTEGER   (kind=ik) :: iostat
INTEGER   (kind=ik) :: nvals = 0
REAL      (kind=rk) :: valore

nome=TRIM("adige.aff")//char(0) 

OPEN(UNIT=33,FILE=nome,STATUS='old',ACTION='read',IOSTAT=status)

openif:IF (status == 0) THEN
   DO
      READ(33,*,IOSTAT=status)
      IF (status /= 0) EXIT
      totale_affluenti = totale_affluenti +1
   ENDDO
   readif:IF (status > 0) THEN
      WRITE(*,1020) totale_affluenti+1
1020  FORMAT ('0','errore di lettura nella riga aff',I6)
   ELSE
      !				WRITE(*,1030) totale_affluenti
   ENDIF readif

ELSE openif
   WRITE(*,1040) totale_affluenti
1040 FORMAT ('','errore di apertura del file: IOSTAT = ',I6)
END IF openif
			
CLOSE(33)


totale_affluenti=totale_affluenti-1

ALLOCATE (nome_affluente		(totale_affluenti)		, stat = iostat)
ALLOCATE (indice_sez_affluente	(totale_affluenti)		, stat = iostat)


OPEN(UNIT=33,FILE=nome,STATUS='old',ACTION='read',IOSTAT=status)
READ(33,*)
WRITE(*,132) 'AFFLUENTE','SEZIONE DI IMMISSIONE'
132 FORMAT(1X,A10,5X,A20)

DO j=1,totale_affluenti
   READ(33,133) nome_affluente(j),indice_sez_affluente(j)
133 FORMAT(A10,I15)
   WRITE(*,233) nome_affluente(j),indice_sez_affluente(j)
233 FORMAT(A10,I15)
ENDDO

CLOSE(33)

nome=TRIM("noce.txt")//char(0)
OPEN(UNIT=33,FILE=nome,STATUS='old',ACTION='read',IOSTAT=status)
IF (status == 0) THEN
   DO
      READ(33,*,IOSTAT=status)
      IF (status /= 0) EXIT
      nvals = nvals +1
   ENDDO
   IF (status > 0) THEN
      WRITE(*,1021) nvals+1
1021  FORMAT ('0','errore di lettura nella riga idro',I6)
   ELSE
      
   ENDIF
   
ELSE 
   WRITE(*,1041) nvals
1041 FORMAT ('','errore di apertura del file: IOSTAT = ',I6)
END IF
			
CLOSE(33)

nvals=nvals-2   !dipende da file 

idr_campi_aff=nvals


!SPORCATA IN REALTà DOVREI LEGGERE OGNI IDROGRAMMA IN INGRESSO DEGLI AFFLUENTI
!CAPENDO IN NURODO RIGHE E I VALORI
ALLOCATE (td_aff		        (totale_affluenti,idr_campi_aff), stat = iostat)
ALLOCATE (Qd_aff		        (totale_affluenti,idr_campi_aff), stat = iostat)

!C  lettura idrogrammi degli affluenti in ingresso
DO j=1,totale_affluenti


   nome=TRIM(nome_affluente(j))//".txt"//char(0)
   OPEN(UNIT=33,FILE=nome,STATUS='old',ACTION='read',IOSTAT=status)
   READ(33,*)
   READ(33,*)
   DO i=1,idr_campi_aff
      READ(33,*) td_aff(j,i), qd_aff(j,i)
!      qd_aff(j,i)= qd_aff(j,i)/3.0_rk
   ENDDO
   CLOSE(33)

ENDDO

RETURN
END SUBROUTINE letturaAff
!-------------------------------------------------------------------------------------------
SUBROUTINE nuovo_contorno 
 
USE comuni
IMPLICIT NONE
INTEGER(ik):: i,it,ji
real(rk)::tratio,dstar

INTERFACE	 
   REAL (kind=rk) FUNCTION dcritica(ji,qq)
     USE comuni
     INTEGER (kind=ik), INTENT (in)  :: ji
     REAL    (kind=rk), INTENT (in)  :: qq
   END FUNCTION dcritica
END INTERFACE

IF(usaidr==1) THEN  	
   it=1
   DO WHILE((td(it)*60<=time).and.it<=idr_campi)
      it=it+1
   ENDDO
   tratio=((time/60.)-td(it-1))/(td(it)-td(it-1))
   qstar = (Qd(it)-Qd(it-1))*tratio + Qd(it-1)
   dstar=dcritica(1,qstar)
!   if(nn>8000)then
!      write(*,'(3(i8,1x),6(f18.12,1x))') nn,it,it-1,qd(it),qd(it-1),qstar
!   end if
   DO i=1,nsezi 
      qaff(i)=0. 
   ENDDO
   DO i=1,totale_affluenti 
      ji = indice_sez_affluente(i) !sez originaria dell'affluente 
      qaff(ji)=(Qd_aff(i,it)-Qd_aff(i,it-1))*tratio+Qd_aff(i,it-1)
!      qaff(ji)=20.0_rk
   ENDDO
ELSE
   q(1) = qstar     
   DO i=1,nsezi 
      qaff(i)=0. 
   ENDDO
ENDIF
!stop
!write(*,*) qstar

RETURN
ENDSUBROUTINE nuovo_contorno
!-------------------------------------------------------------------------------------------

SUBROUTINE profondita_valle(Q_nota,dvalle,j) 

!purpose: calcolo della profonità

USE comuni
IMPLICIT NONE

INTEGER (kind=ik):: j,n,icont
INTEGER (kind=ik),PARAMETER:: nmax=1000       
REAL    (kind=rk):: z1,z2,z3,dvalle
REAL    (kind=rk):: Q1,Q2,Q3,Q_nota,toll

z1		=	zi	(1,j)	
z2		=	e	(j)
Q1		=	10000
Q2		=	0.1
toll	=	ABS(Q1-Q2)

icont=1
DO WHILE(toll > 1.0E-5)
   icont=icont+1
   if(icont>100)then
      write(*,*) 'profondita_valle non convergente'
      stop
   end if
   CALL moto_uniforme(z1,Q1,j)
   
   CALL moto_uniforme(z2,Q2,j)

   z3=0.5*(z2+z1)

   CALL moto_uniforme(z3,Q3,j)
   
   IF( (Q_nota-Q1)*(Q_nota-Q3) < 0._rk) THEN
      z2=z3
   ELSE
      z1=z3
   ENDIF

   toll=ABS(Q2-Q3)

ENDDO

dvalle=z3- e(j)

RETURN
END SUBROUTINE profondita_valle

!------------------------------------------------------------------------------------------

SUBROUTINE moto_uniforme(zz,qq,j)

!purpose: data la portata si calcola la profondità

USE comuni
IMPLICIT NONE
INTEGER (kind=ik)						:: i,n,j
REAL    (kind=rk)						:: z1,z2,Q1,Q2,Qfit
REAL    (kind=rk)						:: zz,qq,larg,ifo_loc

REAL    (kind=rk)						:: zp		(npi(j))  
REAL    (kind=rk)						:: cosa		(npi(j))  
REAL    (kind=rk)						:: deltab	(npi(j)-1) 


DO i=1,npi(j)              !C quote
	zp(i)=zz-zi(i,j)
	IF(zp(i) <= 0) THEN
	zp(i)=zz
	ELSE
	zp(i)=zi(i,j)
	ENDIF			
ENDDO

DO i=1,npi(j)			   !C profondità 
	zp(i)=zz-zp(i)	
	IF(zp(i) <= 0._rk) THEN				
	zp(i)=0
	ENDIF		
ENDDO



DO i=1,npi(j) -1		   !C calcolo cos(a)
  cosa(i)	=	ABS((yi(i+1,j)-yi(i,j))/  &
				(SQRT((yi(i+1,j)-yi(i,j))**2+(zi(i+1,j)-zi(i,j))**2)))
!cosa(i)=1.0_rk
ENDDO 

DO i=1,npi(j)-1
  IF (zp(i)*zp(i+1).eq.0) THEN
		IF(zp(i).eq.0)   THEN
		deltab(i)=(zp(i+1)*(yi(i+1,j)-yi(i,j)))/(ABS(zi(i,j)-zi(i+1,j)))
		END IF
		IF(zp(i+1).eq.0) THEN
		deltab(i)=(zp(i)*(yi(i+1,j)-yi(i,j)))/(ABS(zi(i,j)-zi(i+1,j)))
		END IF
	ELSE
	deltab(i)=(yi(i+1,j)-yi(i,j))	
  END IF	  
ENDDO

larg=0._rk						!C largezza sez
DO i=1,npi(j)-1
	larg=larg+deltab(i)
ENDDO

IF(j >= 5)THEN
ifo_loc=(e(j-4)-e(j))/(si(j)-si(j-4))
ELSE
write(*,*) 'modifica per calcolo M.U. DI monte'
ENDIF


qq=0							!C  calcolo della portata
DO i=1,npi(j)-1							
Qfit=SQRT((ifo_loc))*(rks(i,j)*(zp(i)**(5./3.)+zp(i+1)**(5./3.))* &
	(cosa(i)**(2./3.))*deltab(i))/2.
	qq=qq+Qfit			
ENDDO


RETURN
END SUBROUTINE moto_uniforme
!-------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------
!
!              MAIN Driver program

PROGRAM hydroWaf
  USE comuni
  IMPLICIT NONE

  CHARACTER(30)						   :: string
  CHARACTER(7)						   :: testoout, outputname
  INTEGER   (kind=ik)                  :: j,status, i
  REAL      (kind=rk)    , PARAMETER   :: timeto		      = 1.0E-07
  REAL      (kind=rk)                  :: temp_con,c1, c2, c3, c4, c5, c6, c7, c8
  !--------------------------------------------------------------------------------------

  !C I parametri del problema sono letti nel file "modeldata.ori"

  CALL reader

  !C Lettura della geometria delle sezioni e allocazione vettori

  !CALL letturasez

  CALL letturasez_new


  !C Lettura idrogramma in ingresso da monte

  CALL lettura_idrogramma

  !C Lettura affluenti e sezioni di immissione

!  CALL letturaAff

  !C Crea i vettori sigma(profondità) ed eta(larghezze) per i punti 
  !C delle sezioni

  CALL vect

  !C Impongo le condizioni iniziali
! open (unit=333, file='talvegue.dat', action='write')
  CALL initia
! close (333)
  !tempo a cui inizia la simulazione
	
WRITE(path,'(2a)') trim(path_in),'/times.oro'
OPEN(unit=313,file=trim(path),action='write',status='unknown')

  time=t0
  CALL output

  DO nn = 1,ntmaxi

	
	!OPEN(unit=111,file='termini_sorgente.dat', action='write')
	
!	IF(MOD(nn,itout) == 0) THEN
!	WRITE(111,*) 'time =',nn
!	END IF

     !C Si richiama nuovo contorno per calcolo portata in ingresso
     CALL nuovo_contorno
     
     !C impongo le condizioni al contorno
     CALL bcondi

     
     !C Si impone la condizione di Courant-Friedrichs-Lewy (CFL)

     CALL cflcon

!C Si calcolano i flussi usando il metodo WAF
!Evoluzione variabili conservative per WAF+SOURCES 2nd ordine
!--------------CONVECTIVE PART--------------------------------
     icont_sou_T = 1
 !    CALL CS_evolution
     icont_sou_T = icont_sou_T +1


     CALL wafflu
     !CALL wafstate

!C Aggiorna le soluzioni mediante formula conservativa   
     CALL update

     DO j = 1,nsezi
        CS(1,j) = A(j)         !varibile conservativa
        CS(2,j) = q(j)         !varibile conservativa
     ENDDO

     IF(MOD(nn,itout) == 0) THEN
        CALL output
        WRITE(*,*)'----------------------------------------' 
        WRITE(*,'(i12,3x,f15.4,3x,f8.5)') nn,time,dtime 
        WRITE(*,*) q(1),q(nsezi)
        WRITE(*,*)'----------------------------------------' 
     ENDIF


     IF(ABS(time - timeout) <= timeto)THEN
        CALL output
        EXIT
     ENDIF
     CLOSE(80)



  ENDDO

CLOSE(313)
!CLOSE (111)


!outputname='output'
!OPEN(unit=301, file='./output/'//outputname//".dat",action='write')
!DO j=0,int(nn/itout)
!write(*,*) nn, itout, j
!	WRITE(testoout,'(i7.7)') j*itout
!	write(*,*) testoout
!	nome="./output/"//testoout//".dat"//char(0) 
!	OPEN(unit=201,file=nome)
!		READ(201,'(a)') string
!		READ(201,'(a)') string
!		DO i=1,nsezi
!			READ(201,*) c1, c2, c3, c4, c5, c6, c7, c8
!			WRITE(301,'(11(1x,f12.6))') c1, c2, c3, c4, c5, c6, c7, c8
!		END DO
!	CLOSE(201)
!	WRITE(301,*) ''
!END DO

!WRITE(testoout,'(i7.7)') nn
!nome="./output/"//testoout//".dat"//char(0) 
!OPEN(unit=201,file=nome)
!READ(201,'(a)') string
!READ(201,'(a)') string
!DO i=1,nsezi
!		READ(201,*) c1, c2, c3, c4, c5, c6, c7, c8
!		WRITE(301,'(11(1x,f12.6))') c1, c2, c3, c4, c5, c6, c7, c8
!END DO
!CLOSE(201)
!CLOSE(301)
     
END PROGRAM hydroWaf
