

      SUBROUTINE readhead(NROW,NCOL,NLAY,HEAD)
C***************************************************************
! WARNING: You must hardwire in the name of the head file
! 'blah.hed'
! and know how it is formatted to be read in properly
!*************************************************************
      IMPLICIT NONE
  
      INTEGER I,J,K,NROW,NCOL,NLAY
      REAL*8 HEAD(NROW,NCOL,NLAY)
      CHARACTER*80 HEADFILE
      

      OPEN(UNIT = 30, FILE = 'A1.hed', STATUS = 'OLD')
      DO 10 , K=1,NLAY
      DO 20 , I=1,NROW
      READ(30,203) (HEAD(I,J,K), J=1,10)
C      WRITE(*,203) (HEAD(I,J,K), J=1,10)
      READ(30,203) (HEAD(I,J,K), J=11,20)
C      WRITE(*,203) (HEAD(I,J,K), J=11,20)
      READ(30,203) (HEAD(I,J,K), J=21,30)
C      WRITE(*,203) (HEAD(I,J,K), J=21,30)
      READ(30,203) (HEAD(I,J,K), J=31,40)
C      WRITE(*,203) (HEAD(I,J,K), J=31,40)
      READ(30,203) (HEAD(I,J,K), J=41,50)
C      WRITE(*,203) (HEAD(I,J,K), J=41,50)

 20   CONTINUE
 10   CONTINUE
 203  FORMAT(10(F10.3))
      CLOSE(30)
      END

