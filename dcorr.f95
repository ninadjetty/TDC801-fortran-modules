!=============================================================================
MODULE dcorr
CONTAINS

!=============================================================================
!- Subroutine for calculating the autocorrelation of various singles channels.
!=============================================================================

  SUBROUTINE correl(fn, ext, cnti, tmeas, nsm, crstr)
    CHARACTER, INTENT(IN) :: fn*99, ext*4, crstr*99
    CHARACTER :: fn5*99, fn6*99
    INTEGER :: i, nr, nc, n1, n2, f5, nsm, ncr, j1, j2, f6
    INTEGER*8, INTENT(IN) :: cnti(:,:)
    REAL :: avg1, avg2, var1, var2, auavg, auvar, aucov, cravg, crvar, crcov, tmeas
    REAL, ALLOCATABLE, DIMENSION (:) :: auto, cros
    REAL, ALLOCATABLE, DIMENSION (:,:) :: cntr, aucorr, crcorr
    CHARACTER, ALLOCATABLE, DIMENSION (:) :: strn(:)*3

    f5=5
    nr=SIZE(cnti,DIM=1)
    nc=SIZE(cnti,DIM=2)
    ALLOCATE(cntr(nr,nc))
    cntr=cnti
    ! nsm=NINT(5./tmeas)  !smallest size of the crossed array; limiting auto and cross correlatins to larger than 5s.
    ! This implementation uses the counts to calculate correlations. It will be forced to use whichever tmeas is used to generate the counts.
    ALLOCATE(aucorr(1:nsm,nc));aucorr=0

    ! CALCULATING AUTO CORRELATION
    DO j=1,nc
       DO i=1,nsm
          ! Array subset 1
          n1=nr-i
          avg1=SUM(cntr(1:n1,j),DIM=1)/n1
          var1=SUM(((cntr(1:n1,j)-avg1)**2.0),DIM=1)/n1
          ! Array (displaced) subset 2
          n2=1+i
          avg2=SUM(cntr(n2:nr,j),DIM=1)/n1
          var2=SUM(((cntr(n2:nr,j)-avg2)**2.0),DIM=1)/n1
          ! Covariance array
          ALLOCATE(auto(1:n1));auto=0
          auto(1:n1)=cntr(1:n1,j)*cntr(n2:nr,j)
          auavg=SUM(auto(1:n1))/n1
          aucov=auavg-(avg1*avg2)
          aucorr(i,j)=aucov/sqrt(var1*var2)
          DEALLOCATE(auto)
       END DO
    END DO
    ! WRITING AUTOCORRELATION DATA FOR ALL COLUMNS TOGETHER IN A FILE
    WRITE(fn5,"(A,A,A)") TRIM(fn),"_Stat_AuCor",TRIM(ext)
    PRINT '(A,I0,A,A)', "Writing auto correlation properties of data in the ", nc, " columns to file... ", TRIM(fn5)
    PRINT *, " "
    OPEN (f5, FILE=fn5)
    DO i=1,nsm
       WRITE (f5, *) i*tmeas, (1+aucorr(i,j),j=1,nc)
    END DO
    CLOSE(f5)
    DEALLOCATE(aucorr)

    f6=6
    ncr=(LEN(TRIM(crstr))+1)/3
    ! PRINT *, ncr
    ALLOCATE(strn(ncr))
    DO i=0,ncr-1
       READ(crstr(3*i+1:3*i+2),*) strn(i+1)
       ! PRINT *, strn(i+1)
    ENDDO
    ! PRINT *, strn(1)(1:1)

    ALLOCATE(crcorr(1:nsm,ncr));crcorr=0

    ! CALCULATING CROSS CORRELATION
    DO j=1,ncr
       READ(strn(j)(1:1),'(I1)') j1
       READ(strn(j)(2:2),'(I1)') j2
       DO i=1,nsm
          ! Array subset 1
          n1=nr-i
          avg1=SUM(cntr(1:n1,j1),DIM=1)/n1
          var1=SUM(((cntr(1:n1,j1)-avg1)**2.0),DIM=1)/n1
          ! Array (displaced) subset 2
          n2=1+i
          avg2=SUM(cntr(n2:nr,j2),DIM=1)/n1
          var2=SUM(((cntr(n2:nr,j2)-avg2)**2.0),DIM=1)/n1
          ! Covariance array
          ALLOCATE(cros(1:n1));cros=0
          cros(1:n1)=cntr(1:n1,j1)*cntr(n2:nr,j2)
          cravg=SUM(cros(1:n1))/n1
          crcov=cravg-(avg1*avg2)
          crcorr(i,j)=crcov/sqrt(var1*var2)
          DEALLOCATE(cros)
       END DO
    END DO
    ! WRITING CROSS CORRELATION DATA FOR ALL COLUMNS TOGETHER IN A FILE
    WRITE(fn5,"(A,A,A)") TRIM(fn),"_Stat_CrCor",TRIM(ext)
    PRINT '(A,I0,A,A)', "Writing cross correlation properties of data in the ", ncr, " columns to file... ", TRIM(fn5)
    PRINT *, " "
    OPEN (f5, FILE=fn5)
    DO i=1,nsm
       WRITE (f5, *) i*tmeas, (1+crcorr(i,j),j=1,ncr)
    END DO
    CLOSE(f5)
    DEALLOCATE(crcorr)

  END SUBROUTINE correl

!=============================================================================
END MODULE dcorr
!=============================================================================
