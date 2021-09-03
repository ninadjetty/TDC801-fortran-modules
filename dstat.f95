!=============================================================================
MODULE dstat
CONTAINS

!=============================================================================
!- Subroutine for performing statistical analysis on data in the different columns of a file; each column is assumed to be a separate data stream.
!=============================================================================

  SUBROUTINE stats(fn, ext, cnti, stt)
    CHARACTER, INTENT(IN) :: fn*99, ext*4
    CHARACTER :: fn4*99
    INTEGER :: f4, i, nr, nc
    INTEGER*8, INTENT(IN) :: cnti(:,:)
    REAL, ALLOCATABLE :: cntr(:,:)
    REAL, ALLOCATABLE, INTENT (INOUT) :: stt(:,:)

    f4=4
    nr=SIZE(cnti,DIM=1)
    nc=SIZE(cnti,DIM=2)
    ALLOCATE(cntr(nr,nc))
    ALLOCATE(stt(5,nc))
    cntr=cnti
    stt=0
    DO i=1,nc
       stt(1,i)=SUM(cntr(1:nr,i))/nr                       !Average
       stt(2,i)=SUM((cntr(1:nr,i)-stt(1,i))**2.0)/(nr-1)   !Variance
       stt(3,i)=SQRT(stt(2,i))                             !Stdev
       stt(4,i)=stt(3,i)/SQRT(REAL(nr))                    !Sterr
       stt(5,i)=(stt(2,i)/stt(1,i))-1.0                    !MandelQ
    END DO
    !PRINTING RESULTS
    WRITE(fn4,"(A,A)") TRIM(fn),"_Stat.dat"
    PRINT '(A,I0,A,A)', "Writing statistical properties of data in the ", nc, " columns to file... ", TRIM(fn4)
    PRINT *, " "
    PRINT "(A,I0,A)", "A table summarizing the statistical properties of data in the ", nc, " columns:"
    PRINT *, " "
    PRINT *, "No.    AVG      VAR       STDEV    STERR      MANQ"
    OPEN(f4, FILE=fn4)
    DO j=1,nc
       PRINT 10, j, (stt(i,j),i=1,5)
       WRITE(f4,*) j, (stt(i,j),i=1,5)
    END DO
10  FORMAT(I2.2,2x,5(ES9.2,1x))
    PRINT *, " "
    CLOSE(f4)
  END SUBROUTINE stats

!=============================================================================
END MODULE dstat
!=============================================================================
