!=============================================================================
MODULE wcnts
CONTAINS

!=============================================================================
!- Subroutine for writing a separate data file with count values
!=============================================================================

  SUBROUTINE sepfl(fn, ext, raw, nr, nc, hr, tmeas, ns)
    CHARACTER, INTENT(IN) :: fn*99, ext*4
    CHARACTER :: fn1*99
    REAL*8, ALLOCATABLE :: tm(:)
    INTEGER :: f1, i, j
    INTEGER, INTENT (INOUT) :: nr, nc, hr, ns
    INTEGER*8, INTENT(INOUT) :: raw(:,:)
    INTEGER*8, ALLOCATABLE :: splttg(:,:), tms(:)
    REAL*8 :: dt
    REAL :: tmeas
    INTEGER*8 :: dti, mf

    f1=1
    DO i=1,nr                      ! to limit the array to points that are well behaved.
       IF (ABS(raw(i,2)) .GT. 2)THEN
          PRINT '(A)', "Caution! Corrupt data!"
          PRINT *, i-1, raw(i-1,1), raw(i-1,2)
          PRINT *, i, raw(i,1), raw(i,2)
          EXIT
       ELSE
       END IF
    END DO
    ALLOCATE(tm(nr))
    ALLOCATE(tms(nr))
    tm=0;tms=0
    mf=NINT(1./tmeas)
    PRINT '(A,ES7.1,A,I0)', "Measurement time interval is: ", tmeas, ", so the multiplication factor is: ", mf
    PRINT *, " "
    DO i=nr,1,-1
       raw(i,1)=raw(i,1)-raw(1,1)  ! zero shifting
       tm(i)=raw(i,1)*80.995E-12   ! array of real, zero-shifted time values
       tms(i)=CEILING(mf*tm(i))    ! array of integer values corresponding to time in multiples of tmeas
    ENDDO
    tms(1)=1                       ! otherwise it remains zero
    PRINT '(A,ES7.1,A)', "The total time duration of recorded data in the file is: ", tm(nr), "s."
    PRINT *, " "
    IF (tm(nr) .LT. 16.0) THEN
       PRINT '(A)', "By default, 8s worth of data is trimmed away from either end of the array&
            & (total 16s), to account for the time required to switch on and off the monitor or other&
            & sources of light. You have recorded data for a duration less than this, which will lead to&
            & problem with further analysis. Either record data for longer or change the amount of data to be&
            & trimmed by changing the variable ns."
       PRINT *, " "
    END IF
    ALLOCATE(splttg(tms(nr),4))
    splttg=0
    !$OMP PARALLEL DO              ! in case you want to execute the do loop parallely
    DO i=1,nr-1,1
       dti=raw(i+1,1)-raw(i,1)
       IF(raw(i,2)==1)THEN
          splttg(tms(i),1)=splttg(tms(i),1)+1
       ELSEIF(raw(i,2)==2)THEN
          splttg(tms(i),2)=splttg(tms(i),2)+1
       ELSE
       ENDIF
       IF(dti>0 .AND. dti<=100)THEN
          splttg(tms(i),3)=splttg(tms(i),3)+1
       ENDIF
    ENDDO
    !$OMP END PARALLEL DO
    splttg(1:tms(nr),4)=splttg(1:tms(nr),1)-splttg(1:tms(nr),2)

    ! WRITING THE COUNTS TO A FILE
    ! ns=8                         ! uncomment if you want only a fixed number of data points to be deleted, and don't want 'ns' secs worth of data to be trimmed from either side.
    ns=ns*mf                       ! to trim 'ns' secs worth of data from either side of array.
    tms(nr)=tms(nr)-(2*ns)

    WRITE(fn1,"(A,A,ES7.1,A,I0,A)") TRIM(fn),"_",tmeas,"_",ns,".dat"

    IF(ns/=0)THEN
       PRINT '(A,I0,A,A)', "Writing trimmed data-subset with ",tms(nr)," data points for analysis to file... ", TRIM(fn1)
       PRINT *, " "
    ELSE
       PRINT '(A,I0,A,A)', "Writing full data-set with ",tms(nr)," data points for analysis to file... ", TRIM(fn1)
       PRINT *, " "
    END IF

    OPEN(f1, FILE=fn1)
    REWIND(f1)
    DO i=ns+1,ns+tms(nr),1
       WRITE (f1, *) splttg(i,1), splttg(i,2), splttg(i,3), splttg(i,4)
    END DO
    CLOSE(f1)

    DEALLOCATE(splttg)
    DEALLOCATE(tms)
    DEALLOCATE(tm)
  END SUBROUTINE sepfl

!=============================================================================
END MODULE wcnts
!=============================================================================
