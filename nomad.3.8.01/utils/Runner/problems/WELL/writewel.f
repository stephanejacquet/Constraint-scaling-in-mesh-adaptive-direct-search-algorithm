      SUBROUTINE writewel(DIM,X,Y,Z,WELRATE)
C*****************************************************************
! WARNING: You must hardwire in the appropriate well file name
! FILE = 'foo.wel'
! and know the format of the input file
C***************************************************************

      IMPLICIT NONE 
      
      INTEGER DIM,I
      DOUBLE PRECISION WELRATE(DIM)
      INTEGER X(DIM),Y(DIM),Z(DIM)
      CHARACTER*80 welfile

      OPEN(UNIT = 13, FILE = 'A1.wel' ,STATUS = 'UNKNOWN')
      WRITE(13,*) DIM,0
      WRITE(13,*) DIM
      DO 10 , I=1,DIM
      WRITE(13,100)  Z(I),X(I),Y(I),WELRATE(I) 
 10   CONTINUE
C FORMATS
 100  FORMAT(I3,I3,I3,F12.7)
 101  FORMAT(I3)
      
      END

