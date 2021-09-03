!=============================================================================
MODULE finfo
CONTAINS

!=============================================================================
!- Subroutine for finding the no of rows (data points) in a file
!=============================================================================

  SUBROUTINE nrows(fn, ext, nr, hr)
    CHARACTER, INTENT(IN) :: fn*99, ext*4
    CHARACTER :: fn1*99, line*99, cmd*99
    INTEGER :: f1, i, n, nr, toks
    INTEGER :: nd, hr

    f1=1
    WRITE(fn1,"(A,A)") TRIM(fn),TRIM(ext)
    OPEN(f1, FILE=fn1)
    PRINT *, "Analysing file... ", TRIM(fn1)
    PRINT *, " "
    hr=0;nd=0
    REWIND(f1)
    DO i=1,100
       READ(f1,*,IOSTAT=IO) line
       IF (IO/=0) EXIT
       IF (line(1:1)=='#') THEN
          hr=hr+1
       ELSE
       ENDIF
    END DO
    WRITE(cmd,'(A,A,A)') "wc -l ",TRIM(fn1)," | cut -d ' ' -f1 > wc.dat"
    CALL EXECUTE_COMMAND_LINE(cmd)
    OPEN(UNIT=10,file="wc.dat")
    READ (10,*) nr
    REWIND(10)
    PRINT '(A,A,I0,A,I0,A,I0,A)', TRIM(fn1), " has ", nr, " rows, of which ", hr, " are&
         & header lines and ", nr-hr, " are data points."
    PRINT *, " "
    REWIND(f1)
    CLOSE(f1)
  END SUBROUTINE nrows

!=============================================================================
!- Subroutine for finding the no of data columns in a file
!=============================================================================

  SUBROUTINE ncols(fn, ext, nc, sprtr, hr)
    CHARACTER, INTENT(IN) :: fn*99, ext*4
    CHARACTER :: fn1*99, line*99, sprtr
    INTEGER :: f1, i, n, nc, toks, hr

    f1=1
    WRITE(fn1,"(A,A)") TRIM(fn),TRIM(ext)
    OPEN(f1, FILE=fn1)
    REWIND(f1)
    DO ilines = 1,hr
       READ(1,*)
    END DO
    READ(f1,'(a)') line
    i = 1;
    n = LEN_TRIM(line)
    DO WHILE(i <= n)
       DO WHILE(line(i:i) == sprtr)
          i = i + 1
          IF (n < i) EXIT
       END DO
       nc=nc+1
       DO
          i = i + 1
          IF (n < i) EXIT
          IF (line(i:i) == sprtr) EXIT
       END DO
    END DO
    PRINT '(A,A,I0,A,A,A)', TRIM(fn1), " has ", nc, " columns of data separated by '", sprtr, "'."
    PRINT *, " "
    REWIND(f1)
    CLOSE(f1)
  END SUBROUTINE ncols

!=============================================================================
END MODULE finfo
!=============================================================================
