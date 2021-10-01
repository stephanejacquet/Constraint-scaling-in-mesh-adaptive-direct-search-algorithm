C*************************************************************************
!     WRAPPER AROUND 'FEVAL' SUBROUTINE
!
C*************************************************************************
!     Created by Gilles Couture 04/29/03
C*************************************************************************

      program main
      implicit double precision (a-h,o-z)
      DOUBLE PRECISION X(17)
C      INTEGER Y(10)
      DOUBLE PRECISION V(18)
      CHARACTER FILENAME*100
      INTEGER FLAG

      call getarg(1,FILENAME)

C      WRITE(*,*) FILENAME

      open (unit=88, file=FILENAME, status='old')
C      do i = 1,18
         read(88,*) X
C         read(88,*) X(i)
C         read(88,*) Y(i)
C      end do
      close (unit=88)

      do i = 1,17
         V(i) = X(i)
      end do

      V(18) = -0.031998 - V(17) - V(16) - V(15) - V(14) - V(13)
      
      IF ( V(18) < -0.0063996 .or. V(18) > 0.002 ) then
	do i = 1,13
	  WRITE(*,*) '9.9e307 '
	end do
      else

	call feval(18,V,FV,FLAG)

	IF (FLAG == 0) THEN
	  WRITE(*,'(e18.12)') FV
	ELSE IF (FLAG == 1) THEN
	  WRITE(*,*) '9.9e307'
	ENDIF
      endif

      end program main

      SUBROUTINE feval(DIM,V,FV,FLAG)
      IMPLICIT NONE

C************************************************************************* 
!     SUBROUTINE TO EVALUATE THE OBJECTIVE FUNCTION FOR OPTIMIZATION 
!
!     THIS OBJECTIVE FUNCTION WILL:
!       1. SET THE VALUES FOR THE SIMULATION
!       2. CALL FLOW AND TRANSPORT SIMULATORS
!       3. EXCTRACT NECESSARY INFORMATION FROM OUTPUT FILES 
!          BY CALLING THE SUBROUTINES WRITEWEL AND READHEAD
!       4. COMPUTE THE COST BY CALLING THE SUBROUTINE COSTA(C,O)
C************************************************************************
!     Program created by Katie Kavanagh 9/11/02
!     Last modified: 2/10/03
C***********************************************************************
!     INPUT:
!             DIM: DIMENSION OF THE PROBLEM
!               V: POINT TO EVALUATE THE FUNCTION AT 
!     OUTPUT:
!             FV: FUNCTION VALUE
!           FLAG: 0 FOR SUCCESS, >0 FOR FAILURE 
C************************************************************************
C Description of variables:
!
! NROW, NLAY, NCOL: number of rows, layers, columns    
! WELRATE: array of pumping rates
! COSTC, COST) capital and operation costs
! TOTALCOST = COSTC + COSTO
! HEAD array containing values of head, output from modflow
C**********************************************************************
      CHARACTER*80 modflw96
    
      INTEGER DIM,I,J,K,FLAG
      INTEGER NROW,NLAY,NCOL, it

C!!!! NROW, NCOL, NLAY must match the values in otpmain.f
      PARAMETER(NROW = 50)
      PARAMETER(NCOL = 50)
      PARAMETER(NLAY = 10)

      COMMON it
      DOUBLE PRECISION MINRATE, QTMIN, QT
      DOUBLE PRECISION V(DIM),FV,WELRATE(DIM), COSTC,COSTO,TOTALCOST
      REAL*8 HEAD(NROW,NCOL,NLAY)
      INTEGER X(DIM), Y(DIM), Z(DIM)
      character*80 mfnfile    
  
C Count the function evaluations
       it = it + 1 

C Set values
C MINRATE: the shut off pumping rate value   
C QTMIN is the constraint on the net pumping rate
       MINRATE = 0.0001
       QTMIN = -0.031998
       QT = 0.0
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
C Set your well locations
       DO 90 , I=1,DIM/3
         Z(I) = 10
         X(I) = V(2*I-1)
         Y(I) = V(2*I)
 90      CONTINUE

C       X(1) = NINT((1000-V(2))/20.0) 
C       Y(1) = NINT(V(1)/20.0)
C       WRITE(*,*) '(X1, Y1) =',X(1), Y(1)
  
C       X(2) = NINT((1000-V(4))/20.0) 
C       Y(2) = NINT(V(3)/20.0)
C       WRITE(*,*) '(X2, Y2) =',X(2), Y(2)

C       X(3) = NINT((1000-V(6))/20.0) 
C       Y(3) = NINT(V(5)/20.0)
C       WRITE(*,*) '(X3, Y3) =',X(3), Y(3)
  
C       X(4) = NINT((1000-V(8))/20.0) 
C       Y(4) = NINT(V(7)/20.0)
C       WRITE(*,*) '(X4, Y4) =',X(4), Y(4)

C       X(5) = NINT((1000-V(10))/20.0) 
C       Y(5) = NINT(V(9)/20.0)
C       WRITE(*,*) '(X5, Y5) =',X(5), Y(5)

C       X(6) = NINT((1000-V(12))/20.0) 
C       Y(6) = NINT(V(11)/20.0)
C       WRITE(*,*) '(X6, Y6) =',X(6), Y(6)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
C Set the pumping rates at the 5 wells
       DO 666 , I=1,DIM/3
       WELRATE(I) = V(I+12)
       IF(ABS(WELRATE(I)) .LE. MINRATE)THEN
       WELRATE(I) = 0.0
       ENDIF
C       WRITE(*,*) 'WELRATE(', I, ') =', WELRATE(I)
 666   CONTINUE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
C Set FLAG = 0 for success
      FLAG = 0
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
C Check that 2 wells do not occupy the same location
      call location_constraint(DIM/3,X,Y,FLAG)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C Check the constraints on the net pumping rate
      call linear_constraint(DIM/3,X,Y,Z,WELRATE,NLAY,
     & NROW, NCOL,FLAG)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C Compute the cost:           
      IF(FLAG .EQ. 0)THEN

C Change the .wel file before calling MODFLOW
      call writewel(DIM/3,X,Y,Z,WELRATE)
      CLOSE(13)

C Call MODFLOW
      call system("./modflw96_MAC")

C Read in the heads from the .hed file that MODFLOW outputs
      call readhead(NROW,NCOL,NLAY,HEAD)
C      DO 10 , K=1,NLAY
C 10   CONTINUE


C Compute the installation cost
      call costac(DIM/3,X,Y,Z,WELRATE,NLAY,NROW,NCOL,COSTC,FLAG)
C      WRITE(*,*) 'costc', COSTC

C Compute the operational cost
      call costao(DIM/3,X,Y,Z,WELRATE,HEAD,NLAY,NROW,NCOL,COSTO,FLAG)
C      WRITE(*,*) 'costo', COSTO

C Check to see if the constraints are violated
      call head_constraint(DIM/3,X,Y,Z,WELRATE,HEAD,NLAY,NROW,
     & NCOL,COSTO,FLAG)

      FV = COSTC + COSTO

C      WRITE(*,*) '********************************'
C      WRITE(*,100) ' Function value =', FV
C      WRITE(*,*) 'FLAG=', FLAG
C      WRITE(*,*) 'No.function evaluations:', it
C      WRITE(*,*) '********************************'

100   FORMAT(a17,E13.5)
      ENDIF

      END



