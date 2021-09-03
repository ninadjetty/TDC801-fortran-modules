!PROGRAM FOR DATA ANALYSIS

PROGRAM DA
  USE finfo
  USE rdata
  USE wcnts
  USE dstat
  USE dcorr

  IMPLICIT NONE

  INTEGER :: f5, fno
  INTEGER :: nc, nr, hr, i, j, k, ns, nsm, k1, k2
  INTEGER*8, ALLOCATABLE :: raw(:,:), cnti(:,:)
  REAL :: tc, T, var, tmeas
  REAL, ALLOCATABLE :: cntr(:,:), stt(:,:), fnas(:), datarr(:,:,:), fna(:)
  CHARACTER :: fn*99, sprtr*9, outf*99, ext*4, tmeasch*20, crstr*99
  CHARACTER, ALLOCATABLE :: hdr(:)*99

  ! FILE UNITS
  f5=1;
  ext=".txt"

  CALL GET_COMMAND_ARGUMENT(1, fn)
  CALL GET_COMMAND_ARGUMENT(2, tmeasch)
  READ (tmeasch,*) tmeas
  CALL GET_COMMAND_ARGUMENT(3, crstr)

  ! READING NUMBER OF ROWS IN THE FILE
  nr=0;hr=0
  CALL nrows(fn, ext, nr, hr)
  ! READING NUMBER OF COLUMNS IN THE FILE
  nc=0
  sprtr=";"
  ! sprtr = char(9) !use this when data columns are separated by tabs
  ! sprtr = " " !use this when data columns are separated by spaces
  ! sprtr = "," !for csv files
  CALL ncols(fn, ext, nc, sprtr, hr)
  ! READING THE FILE AFTER APPROPRIATELY ALLOCATING THE ARRAY
  CALL readf(fn, ext, raw, nr, nc, hdr, hr)
  ! WRITING COUNT DATA IN A SEPARATE FILE
  ns=8
  CALL sepfl(fn, ext, raw, nr, nc, hr, tmeas, ns)
  ! DEALLOCATING ARRAYS
  DEALLOCATE(hdr)
  DEALLOCATE(raw)
  CLOSE(f5)

  ! FILE UNITS
  f5=2;
  ext=".dat"

  WRITE(fn,"(A,A,ES7.1,A,I0)") TRIM(fn),"_",tmeas,"_",ns

  ! READING NUMBER OF ROWS IN THE FILE
  nr=0;hr=0
  CALL nrows(fn, ext, nr, hr)
  ! READING NUMBER OF COLUMNS IN THE FILE
  nc=0
  sprtr=" "
  ! sprtr = char(9) !use this when data columns are separated by tabs
  ! sprtr = " " !use this when data columns are separated by spaces
  ! sprtr = "," !for csv files
  CALL ncols(fn, ext, nc, sprtr, hr)
  ! READING THE FILE AFTER APPROPRIATELY ALLOCATING THE ARRAY
  CALL readf(fn, ext, raw, nr, nc, hdr, hr)
  ! STATISTICS
  CALL stats(fn, ext, raw, stt)
  !AUTOCORRELATION
  nsm=15
  CALL correl(fn, ext, raw, tmeas, nsm, crstr)
  ! DEALLOCATING ARRAYS
  DEALLOCATE(hdr)
  DEALLOCATE(raw)
  CLOSE(f5)

END PROGRAM DA
