!=============================================================================
MODULE rdata
CONTAINS

!=============================================================================
!- Subroutine for obtaining the header and raw data separately from a file
!=============================================================================
  
  SUBROUTINE readf(fn, ext, raw, nr, nc, hdr, hr)
    CHARACTER, INTENT(IN) :: fn*99, ext*4
    CHARACTER :: fn1*99
    CHARACTER, ALLOCATABLE, INTENT (INOUT) :: hdr(:)*99
    INTEGER :: f1, i, j, nc
    INTEGER, INTENT (INOUT) :: nr, hr
    INTEGER*8, ALLOCATABLE, INTENT(INOUT) :: raw(:,:)

    !    hr=5
    nr=nr-hr
    f1=1
    ALLOCATE(hdr(hr))
    ALLOCATE(raw(nr,nc))
    raw=0
    WRITE(fn1,"(A,A)") TRIM(fn),TRIM(ext)
    OPEN(f1, FILE=fn1)
    REWIND(f1)
    IF(hr==0)THEN
       READ(f1,*) ((raw(i,j),j=1,nc),i=1,nr)
    ELSE
       READ(f1,'(A)') (hdr(i),i=1,hr)
       ! Comment the above line and un-comment the loop below not to read the header at all.
       ! DO i = 1,hr
       !    READ(1,*)
       ! END DO
       READ(f1,'(I21,2X,I20)') ((raw(i,j),j=1,nc),i=1,nr)
    ENDIF
    REWIND(f1)
    CLOSE(f1)
  END SUBROUTINE readf

!=============================================================================
END MODULE rdata
!=============================================================================
