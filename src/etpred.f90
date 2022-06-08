!--predictk.f
!--mofified by S. Kudryavtsev for using the KSM03 tidal potential catalogue, Feb.10, 13
!--lines modified or added for Kudryavtsev potential contain 'c&'
!--change all \eterna33... to \eterna34... D. Crossley, Feb 26, 2009
!--complete overhaul of code for use with F2PY (Gabriel C. Rau, October 2017)
!-- changes have been marked with GCR throughout the file
!#######################################################################
!     Program PREDICT, version 3.31 1997.03.03 Fortran 90.
!
!     This file has 3707 records.
!
!     === Version for MS-DOS using LAHEY LF90 Fortran compiler  ===
!
!     === To run this program under UNIX, you have to modify     ===
!         routine GEOEXT.
!
!     The program PREDICT computes model tides using different tidal
!     potential catalogues:
!     Doodson (1921)                 with   378 waves,
!     Cartwright-Tayler-Edden (1973) with   505 waves,
!     Buellesfeld (1985)             with   656 waves,
!     Tamura (1987)                  with  1200 waves,
!     Xi (1989)                      with  2933 waves,
!     Roosbeek (1996)                with  6499 waves,
!     Hartmann and Wenzel (1995)     with 12935 waves
!&    Kudryavtsev (2004)             with 28806 waves
!     for a number of different tidal components using observed or
!     estimated (e.g. by a body tide and ocean tide model) tidal
!     parameters.
!     Reference:
!     ----------
!     Wenzel, H.-G. (1996): The nanogal software: Earth tide data
!         processing package ETERNA 3.3. Bulletin d'Informations
!         Marees Terrestres vol. 124, 9425-9439, Bruxelles 1996.
!     Disc file description:
!     ----------------------
!     project:      Formatted file, on which the project name 'CPROJ'
!                   has to be stored in the first record (8 characters
!                   at maximum). The project name 'CPROJ' will be used
!                   to define the print file and the output file.
!                   This file has to be stored in the directory, from
!                   which program PREDICT is executed.
!     'CPROJ'.ini:  Formatted unit, on which the input parameters
!                   have to be stored before the execution of program
!                   PREDICT. This file has to be stored in the
!                   directory, from which program PREDICT is executed.
!     'CPROJ'.prn:  Formatted print file. This file will be written in
!                   the directory, from which program PREDICT is
!                   executed.
!     'CPROJ'.prd:  Formatted output file in ETERNA format. This file
!                   will be written in the directory, from which
!                   program PREDICT is executed.
!     doodsehw.dat: Formatted file, on which the Doodson (1921) tidal
!                   potential catalogue has to be stored before the
!                   execution of program PREDICT.
!                   The path for this file is
!                   File: doodsehw.dat.
!     doodsehw.bin: Unformatted file, on which the Doodson (1921)
!                   tidcommdatal potential catalogue will be stored by the
!                   first execution of program predict, if it does not
!                   yet exist. The path for this file is
!                   File: doodsehw.bin.
!     cted73hw.dat: Formatted file, on which the Cartwright and Tayler
!                   (1971) and Cartwright and Edden (1973) tidal
!                   potential catalogue has to be stored before the
!                   execution of program PREDICT.
!                   The path for this file is
!                   File: cted73hw.dat.
!     cted73hw.bin: Unformatted file, on which the Cartwright and
!                   Tayler (1971) and Cartwright and Edden (1973)
!                   tidal potential catalogue will be stored by the
!                   first execution of program predict, if it does not
!                   yet exist. The path for this file is
!                   File: cted73hw.bin.
!     buellehw.dat: Formatted file, on which the Buellesfeld (1985)
!                   tidal potential catalogue has to be stored before
!                   the execution of program PREDICT.
!                   The path for this file is
!                   File: buellehw.dat.
!     buellehw.bin: Unformatted file, on which the Buellesfeld (1985)
!                   tidal potential catalogue will be stored by the
!                   first execution of program predict, if it does not
!                   yet exist. The path for this file is
!                   File: buellehw.bin.
!     tamurahw.dat: Formatted file, on which the Tamura (1987)
!                   tidal potential catalogue has to be stored before
!                   the execution of program PREDICT.
!                   The path for this file is
!                   File: tamurahw.dat.
!     tamurahw.bin: Unformatted file, on which the Tamura (1987)
!                   tidal potential catalogue will be stored by the
!                   first execution of program predict, if it does not
!                   yet exist. The path for this file is
!                   File: tamurahw.bin.
!     xi1989hw.dat: Formatted file, on which the Xi (1989) tidal
!                   potential catalogue has to be stored before the
!                   execution of program PREDICT.
!                   The path for this file is
!                   File: xi1989hw.dat.
!     xi1989hw.bin: Unformatted file, on which the Xi (1989) tidal
!                   potential catalogue will be stored by the first
!                   execution of program predict, if it does not yet
!                   exist. The path for this file is
!                   File: xi1989hw.bin.
!     ratgp95.dat:  Formatted file, on which the Roosbeek (1986) tidal
!                   potential catalogue has to be stored before the
!                   execution of program PREDICT.
!                   The path for this file is
!                   File: ratgp95.dat.
!     ratgp95.bin:  Unformatted file, on which the Roosbeek (1986)
!                   tidal potential catalogue will be stored by the
!                   first execution of program predict, if it does not
!                   yet exist. The path for this file is
!                   File: ratgp95.bin.
!     hw95s.dat:    Formatted file, on which the Hartmann and Wenzel
!                   (1995) tidal potential catalogue has to be stored
!                   before the execution of program PREDICT.
!                   The path for this file is
!                   File: hw95.dat.
!     hw95s.bin:    Unformatted file, on which the Hartmann and Wenzel
!                   (1995) tidal potential catalogue will be stored by
!                   the first execution of program PREDICT, if it does
!                   not yet exist. The path for this file is
!                   File: hw95.bin.
!     ksm03.dat:    Formatted file, on which the Kudryavtsev (2004)
!                   tidal potential catalogue has to be stored before
!                   the execution of program PREDICT.
!                   The path for this file is
!                   File: ksm03.dat.
!     ksm03.bin:    Unformatted file, on which the Kudryavtsev (2004)
!                   tidal potential catalogue will be stored by the
!                   first execution of program PREDICT, if it does
!                   not yet exist. The path for this file is
!                   File: ksm03.bin.
!     etpolut1.dat: Formatted file, on which the pole coordinates and
!                   DUT1 corrections have to be stored before the
!                   execution of program PREDICT.
!                   The path for this file is
!                   File: etpolut1.dat.
!     etpolut1.bin: Unformatted direct access file, on which the pole
!                   coordinates and DUT1 corrections will be stored by
!                   the first execution of program PREDICT, if it does
!                   not yet exist. The path for this file is
!                   File: etpolut1.bin.
!     Used routines:
!     --------------
!     ETASTN: computes astronomical elements.
!     PREDIN: reads control parameters.
!     ETDDTA: reads tabel of DDT = TDT - UTC.
!     ETDDTB: interpolates   DDT = TDT - UTC from table.
!     ETGCON: computes geodetic coefficients.
!     ETGREN: computes date from Julian date
!     ETJULN: computes JULIAN date.
!     ETLEGN: computes fully normalized Legendre spherical harmonics.
!     ETLOVE: computes elastic parameters from Wahr-Dehant model.
!     ETPOLC: computes DUT1
!     ETPHAS: computes the phases and frequencies of the tidal waves.
!     ETPOTS: computes amplitudes, frequencies and phases of tidal
!             waves.
!     GEOEXT: computes JOBTIME.
!     Numerical accuracy:
!     -------------------
!     The program has been tested on IBM-At compatible computers under
!     MS-DOS operating system with different compilers using DOUBLE
!     PRECISION for all variables (15 digits) and on a SUN SPARC2
!     under UNIX operating system with SUN F77 compiler, and gave
!     identical results to 0.0001 nm/s**2.
!     Execution time:
!     ---------------
!     The CPU execution time depends mainly on the number of waves of
!     the choosen tidal potential development, and the number of model
!     tide values to be computed.
!     The execution on a PENTIUM 100 MHz using LF90 compiler for 8760
!     hourly samples including pole tide and LOD tide (parameter file
!     KAHW9501.INI) are
!     catalogue              threshold   nwave  rms error  ex. time
!                                             [nm/s**2]  [sec]
!     Tamura (1987)            2.D-06     1200    0.070    11.86
!     Hartmann + Wenzel (1995) 1.D-01        9   88.40      3.90
!     Hartmann + Wenzel (1995) 1.D-02       42   14.40      4.06
!     Hartmann + Wenzel (1995) 1.D-03      155    2.25      4.94
!     Hartmann + Wenzel (1995) 1.D-04      434    0.44      7.20
!     Hartmann + Wenzel (1995) 1.D-05     1248    0.068    13.51
!     Hartmann + Wenzel (1995) 1.D-06     3268    0.011    33.01
!     Hartmann + Wenzel (1995) 1.D-07     7761    0.002    83.82
!     Hartmann + Wenzel (1995) 1.D-08    11462    0.001   132.86
!     Hartmann + Wenzel (1995) 1.D-09    12000    0.001   139.95
!     Hartmann + Wenzel (1995) 1.D-10    12011    0.001   140.11
!     Program creation:  1973.06.23 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082307,
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1997.03.03 by Hans-Georg Wenzel.
!#######################################################################

!#######################################################################
!     COMMON /CONST/:
!     DPI...        3.1415....  DPI2...       2.D0*DPI
!     DRAD...       DPI/180.D0  DRO...        180.D0/DPI
!#######################################################################
module PARAMS
    ! initialise numeric values
    ! OS dependent pointer to muted output, will be set from inside python
    CHARACTER(10) :: NULLFILE
    ! commdat directory, will be set from inside python
    CHARACTER(256) :: COMDIR
    ! file i/o streams
    INTEGER, PARAMETER :: STDIN=5,STDOUT=6,STDERR=0,VOID=11
    INTEGER SCR,IC2
    DATA SCR/STDOUT/
    ! names of the i/o files
    CHARACTER(17), PARAMETER :: CFPRN='pygtide.out.prn',CFOUT='pygtide.out.prd',&
    ETDDTDAT='etddt.dat',ETPOLUTDAT='etpolut1.dat',ETPOLUTBIN='etpolut1.bin'
    ! numerical parameters
    REAL(8), PARAMETER :: DPI=3.141592653589793D0
    REAL(8), PARAMETER :: DPI2=2*DPI,DRAD=DPI/180,DRO=180/DPI
    ! initialise string values
    CHARACTER(20) :: CMODEL(8) =(/'Doodson 1921        ','CTED 1973           ','Buellesfeld 1985    ',&
    'Tamura 1987         ','Xi 1989             ','Roosbeek 1995       ','Hartmann+Wenzel 1995', &
    'Kudryavtsev 2004    '/)
    CHARACTER(12) :: CFFILE(8)=(/ 'doodsehw.dat', 'cted73hw.dat', 'buellehw.dat',&
    'tamurahw.dat', 'xi1989hw.dat', 'ratgp95.dat ', 'hw95s.dat   ', 'ksm03.dat   '/)
    CHARACTER(12) :: CUFILE(8)=(/ 'doodsehw.bin', 'cted73hw.bin', 'buellehw.bin', 'tamurahw.bin', &
    'xi1989hw.bin', 'ratgp95.bin ', 'hw95s.bin   ', 'ksm03.bin   '/)
    CHARACTER(8) :: CUNIT(11)=(/'(m/s)**2','nm/s**2 ',' mas    ',' mm     ',&
    ' mm     ',' nstr   ',' nstr   ',' nstr   ',' nstr   ',&
    ' nstr   ',' mm     '/)
    CHARACTER(10) :: CHANNEL(4)=(/'Signal    ','Tide      ','Pole tide ','LOD tide  '/)
    CHARACTER(24) :: COMPON(11)=(/'Potential               ', 'Gravity                 ','Tilt                    ',&
    'Vertical displacement   ','Horizontal displacement ','Vertical strain         ','Horizontal strain       ',&
    'Aereal strain           ','Shear  strain           ','Volume strain           ','Ocean tide              '/)
    INTEGER, PARAMETER :: C88=88888888,C99=99999999
    CHARACTER(10), PARAMETER :: CENDT='C*********'
    save
end module PARAMS

!#######################################################################
!     The following dimension statement is concerning the number of
!     meteorological parameters, which is 8 in the current program
!     version.
!     The following dimension statements are concerning the number of
!     wavegroups to be used, which is 85 in the current program
!     version (parameter MAXWG).
!     The following dimension statements are concerning the number of
!     waves of the tidal potential catalogue, which is 30 000 in the
!     current program version (parameter MAXNW).
!#######################################################################
module MAX_PARS
    INTEGER, PARAMETER :: MAXWG=85, MAXNW=30000
    save
end module MAX_PARS

!#######################################################################
!     The following DIMENSION statement is concerning the tabel of
!     differences DDT = TDT - UTC:
!#######################################################################
module DDT_MOD
    ! DDTTAB needs to be allocatable because it depends on rows in etddt.dat
    REAL(8), DIMENSION(3, 500) :: DDTTAB
    INTEGER NDDTAB
    save
end module DDT_MOD

!#######################################################################
!     The following common blocks are used to transfer the control
!     parameters from routine PREDIN to main program.
!#######################################################################
module CONTROLMOD
    use MAX_PARS
    ! number of wave groups
    INTEGER NGR
    !     DFRA:        Lowest  frequency within wave group in deg/h.
    !     DFRE:        Highest frequency within wave group in deg/h.
    !     DG0:         amplitude factor.
    !     DPHI0:       phase lead in degree.
    REAL(8) DFRA(MAXWG),DFRE(MAXWG),DG0(MAXWG),DPHI0(MAXWG)
    ! GCR could change this to REAL(16) for higher precision wave detection
    ! but then execution time is longer
    REAL(8) NA(MAXWG),NE(MAXWG),DAM(MAXWG),DBOD(MAXWG)
    REAL(8) DLAT,DLON,DH,DGRAV,DAZ,DATLIM,DAMIN,DPOLTC,DLODTC
    INTEGER IDTSEC
    CHARACTER CINST*10,CHEAD(10)*64
    INTEGER IC,IR,ITY,ITM,ITD,ITH,IDA,IMODEL,IPROBS,ISPANH
    REAL(16) KFILT,IPRLF,IRIGID,IHANN,IQUICK,NF
    SAVE
end module CONTROLMOD

module TIDPHAS
    REAL(8) DPK(25)
    save
end module TIDPHAS

module TIDWAVE
    use MAX_PARS
    INTEGER NW,IWNR(MAXNW),IAARG(MAXNW,12)
    REAL(8) DX0(MAXNW),DX1(MAXNW),DX2(MAXNW),DY0(MAXNW),&
    DY1(MAXNW),DY2(MAXNW),DTHPH(MAXNW),DTHFR(MAXNW),DBODY(MAXNW),&
    DC0(MAXNW),DS0(MAXNW),DDC(MAXNW),DDS(MAXNW)
    save
end module TIDWAVE

!#######################################################################
!     COMMON /LOVE/ contains gravimeter factors, LOVE-numbers, SHIDA-
!     numbers and tilt factors for degree 2...4 at latitude DLAT:
!#######################################################################
module LOVE
    REAL(8) DGLAT(12),DHLAT(12),DKLAT(12),DLLAT(12),DTLAT(12)
    REAL(8) DOM0,DOMR,DGR,DHR,DKR,DLR,DTR
    save
end module LOVE
! ##################################################################

! ##################################################################
!-GCR  this subroutne sets the variables in module 'INOUT' for access in Python
module INOUT
    use MAX_PARS
    ! Python handover arguments controlling the Fortran program
    REAL(8), DIMENSION(18) :: ARGSIN
    ! The data storage container read out by Python
    REAL(8), DIMENSION(:, :), ALLOCATABLE :: ETPDATA
    ! Wave groups parameters
    INTEGER NUMWG
    REAL(8), DIMENSION(MAXWG) :: FQMIN, FQMAX, AMPF, PHASEF
    ! global storage of the execution time
    REAL(8) :: EXECTIME,ETD_START,ETD_END
    INTEGER :: FILEPRD,FILEPRN,SCROUT,ETPOL_START,ETPOL_END
    LOGICAL :: ISINIT
    ! CHARACTER(255) :: MESSAGE
    CHARACTER(25), DIMENSION(6,25) :: HEADER
    CHARACTER(8) :: ETPUNIT
    !CHARACTER(6), PARAMETER :: CREST='PyGTide'
    CHARACTER(7), PARAMETER :: CPROJ='pygtide'
    CHARACTER(3), PARAMETER :: VERSION='0.5'
    CHARACTER(32), PARAMETER :: VERS='ETERNA PREDICT v3.4 (10/02/2013)'
    CHARACTER(10), PARAMETER :: FORTVERS='3.4 130210'
    save
end module INOUT
! ##################################################################

! ##################################################################
!-GCR  this subroutne initialises the module
subroutine INIT
    use PARAMS
    use DDT_MOD
    use INOUT
    INTEGER IUN16,IUN27,IUN30,MYPOS,IOS,IPRINT
    CHARACTER(100) TXT
    DATA IUN16/16/,IUN30/30/,IUN27/27/,IPRINT/0/
    ! find out the date limitations from the files for f2py to prevent calculation
    ! SUCCESS IN READING THE LAST LINE FROM ETPOLUT1.DAT FORMATTED
    OPEN(UNIT=IUN30,FILE=TRIM(COMDIR)//TRIM(ETPOLUTDAT),FORM='FORMATTED',&
        ACTION='READ',STATUS='OLD',ACCESS='STREAM',IOSTAT=IOS)
    IF (IOS == 0) THEN
        do
            read (IUN30,"(A10)",iostat=IOS) TXT
            if (IOS /= 0) exit
            if (TXT == CENDT) then ! found search string at beginning of line
                  READ(IUN30, '(I8,A)') ETPOL_START, TXT
                  !WRITE(*,*) "etpolut1.dat:",ETPOL_START
            end if
        end do
        ! get file size to estimate where to read dates from
        INQUIRE(UNIT=IUN30, SIZE=MYPOS)
        !INQUIRE(IUN30, POS=MYPOS)
        !WRITE(*,*) MYPOS
        ! go back by a specific number to the beginning of the previous line
        READ(IUN30, '(I8,A)', POS=MYPOS-74) ETPOL_END, TXT
        !WRITE(SCR,*) "Last line:" // TXT
        !WRITE(*,*) "etpolut1.dat:",ETPOL_END
        CLOSE(IUN30)
    ENDIF
    ! read etddt.dat for start end end values
    NDDTAB = 0
    CALL ETDDTA(IUN16,IUN27,IPRINT)
    ETD_START=DDTTAB(1,1)
    ETD_END=DDTTAB(1,NDDTAB)
    !WRITE(*,*) ETD_START,NDDTAB,ETD_END
    !-GCR initiate wave groups to: everything in tidal catalogue
    NUMWG = 1
    FQMIN(1) = 0.D0
    FQMAX(1) = 100.D0
    AMPF(1) = 1.D0
    PHASEF(1) = 0.D0
END SUBROUTINE
! ##################################################################

! ##################################################################
!-GCR this is to hand over the wave group parameters
SUBROUTINE WAVES(FREQFROM, FREQTO, AMPFAC, PHASEFAC, N)
    use INOUT
    use MAX_PARS
    INTEGER, INTENT(IN) :: N
    REAL(8), INTENT(IN) :: FREQFROM(N), FREQTO(N), AMPFAC(N), PHASEFAC(N)
    NUMWG = N
    FQMIN(1:N) = FREQFROM
    FQMAX(1:N) = FREQTO
    AMPF(1:N) = AMPFAC
    PHASEF(1:N) = PHASEFAC
END SUBROUTINE

! ##################################################################
!-GCR this is the main interface for f2py
SUBROUTINE PREDICT(ARGS)
      use MAX_PARS
      use PARAMS
      use DDT_MOD
      use CONTROLMOD
      use TIDWAVE
      use INOUT
      IMPLICIT REAL(8) (D)
      REAL(8), DIMENSION(18), INTENT(IN) :: ARGS
!-GCR store control arguments
      DIMENSION DGI(6)
!#######################################################################
!     The following DIMENSION statements are concerning the number of
!     output channels:
!#######################################################################
      PARAMETER (MAXCHAN=4)
!      CHARACTER CHANNEL(MAXCHAN)*10
      DIMENSION DCOUT(MAXCHAN),DZERO(MAXCHAN)
!-GCR new 2D array declaration to store data for return to Python
      INTEGER ROWS, ROWI
      LOGICAL OPENSTAT
      ! open a void stream to redirect output (if required)
      OPEN(UNIT=VOID,FILE=TRIM(NULLFILE),STATUS='OLD')
!-GCR calculate row number and allocate new array
      HEADER(1,:)='Date [UTC]'
      HEADER(2,:)='Time [UTC]'
!-GCR VERY IMPORTANT DIRECTIVE FOR F2PY (handover of control inputs from Python)
      ARGSIN = ARGS
!#######################################################################
!     The following dimension statements are concerning the number of
!     wavegroups to be used, which is restricted to  85 in the current
!     program version (parameter MAXWG).
!     The following list of wavegroups may be used for the analyis and
!     prediction of tides (frequencies in cycle per day referring to
!     epoch J2000):
!     no.   from        to          frequency of  name of
!           freqency    frequency   main wave     main wave
!      1    0.000000    0.001369    0.000000      long
!      2    0.000133    0.004107    0.002738      SA
!      3    0.004108    0.020884    0.005476      SSA
!      4    0.020885    0.054747    0.036292      MM
!      5    0.054748    0.091348    0.073202      MF
!      6    0.091349    0.501369    0.109494      MTM
!      7    0.501370    0.911390    0.893244      Q1
!      8    0.911391    0.947991    0.929536      O1
!      9    0.947992    0.981854    0.966446      M1
!     10    0.981855    0.998631    0.997262      P1
!     11    0.998632    1.001369    1.000000      S1
!     12    1.001370    1.004107    1.002738      K1
!     13    1.004108    1.006845    1.005476      PSI1
!     14    1.006846    1.023622    1.008214      PHI1
!     15    1.023623    1.057485    1.039030      J1
!     16    1.057486    1.470243    1.075940      OO1
!     17    1.470244    1.880264    1.864547      2N2
!     18    1.880265    1.914128    1.895982      N2
!     19    1.914129    1.950419    1.932274      M2
!     20    1.950420    1.984282    1.968565      L2
!     21    1.984283    2.002736    2.000000      S2
!     22    2.002737    2.451943    2.005476      K2
!     23    2.451944    7.000000    2.898410      M3M6
!#######################################################################
      DATA IUN14/14/,IUN16/16/,IUN23/23/,IUN24/24/,IUN27/27/
      DATA IUN30/30/,IUN31/31/
      DATA DZERO/4*0.D0/
!#######################################################################
!     Read project name:
!#######################################################################
!-GCR-del      OPEN(UNIT=IUN15,FILE='project')
!-GCR-del      READ(IUN15,'(A8)') CPROJ
!-GCR-del      CLOSE(IUN15)
!-GCR check for screen output option, default: silent (0)
      SCROUT = INT(ARGSIN(18))
      ! redirect screen output to void
      IF (SCROUT.EQ.1) THEN
        SCR=STDOUT
      ELSE
        SCR=VOID
      ENDIF
!-GCR 'FILEPRD' writes the PRD file if set to 1
      FILEPRD = INT(ARGSIN(16))
      IF (FILEPRD.EQ.1) THEN
          IUN23=23
          OPEN(IUN23, FILE=TRIM(CFOUT),FORM='FORMATTED')
      ELSE
          IUN23=VOID
      ENDIF
!-GCR 'FILEPRN' writes the PRN file if set to 1
      FILEPRN = INT(ARGSIN(17))
      IF (FILEPRN.EQ.1) THEN
          IUN16=16
          OPEN(IUN16, FILE=TRIM(CFPRN),FORM='FORMATTED')
      ELSE
          IUN16=VOID
      ENDIF
!-GCR read wave group parameters from ini file
!      OPEN(IUN15, FILE=TRIM(COMDIR)//TRIM(CFINI),ACTION='READ',FORM='FORMATTED')
!-GCR continue ...
      WRITE(IUN16,17002) FORTVERS,CPROJ
      WRITE(SCR,17002) FORTVERS,CPROJ
      IRESET=1
!-GCR set row counter for output array
      ROWI=1
      CALL GEOEXT(IUN16,IRESET,DEXTIM,DEXTOT)
!#######################################################################
!     Store array of differences DDT = TDT - UTC:
!#######################################################################
      IPRINT=0
!-GCR modified: only call this function if it hasn't been called before
      IF (NDDTAB.LT.10) CALL ETDDTA(IUN16,IUN27,IPRINT)
!#######################################################################
!     Read control parameters:
!#######################################################################
      IPRINT=1
!-GCR PREDIN has been modified to deal with ARGSIN
      CALL PREDIN(IUN16,IPRINT)
!-GCR define the size of the output matrix
      ROWS = INT(CEILING((DBLE(ISPANH)*3600)/DBLE(IDTSEC)))
      IF (ALLOCATED(ETPDATA)) DEALLOCATE (ETPDATA)
      ALLOCATE (ETPDATA(ROWS,6))
      ETPDATA = 0.0D0
!-GCR end modification
      DDTH=DBLE(IDTSEC)/3600.D0
      DDTD=DDTH/24.D0
      DTH=0.D0
      IF(IRIGID.EQ.1) WRITE(IUN16,17009)
      WRITE(IUN23,17018) FORTVERS
!#######################################################################
!     Define output channels:
!#######################################################################
      IPOLTC=0
      ILODTC=0
      NC=1
      IF(ABS(DPOLTC).GT.1.D-6) IPOLTC=1
      IF(ABS(DLODTC).GT.1.D-6) ILODTC=1
      IF(IPOLTC.EQ.1.OR.ILODTC.EQ.1) THEN
         NC=NC+1
      ENDIF
      IF(IPOLTC.EQ.1) THEN
         NC=NC+1
      ENDIF
      IF(ILODTC.EQ.1) THEN
         NC=NC+1
      ENDIF
      IPRINT=1
!#######################################################################
!     Initialize direct access file for polecoordinates and DUT1:
!#######################################################################
      OPEN(UNIT=IUN30,FILE=TRIM(COMDIR)//TRIM(ETPOLUTDAT),FORM='FORMATTED',STATUS='OLD')
      DCLAT=COS(DLAT*DRAD)
      DSLAT=SIN(DLAT*DRAD)
      DCLON=COS(DLON*DRAD)
      DSLON=SIN(DLON*DRAD)
      IPRINT=1
      CALL ETJULN(IUN16,ITY,ITM,ITD,DTH,DJULD)
      CALL ETPOLC(IUN16,IUN30,IUN31,IPRINT,DJULD,DCLAT,DSLAT,&
        DCLON,DSLON,DPOLX,DPOLY,DUT1,DTAI,DLOD,DGPOL,DGPOLP,DGLOD,NERR)
      CLOSE(IUN30)
!#######################################################################
!     Compute amplitudes, frequencies and phases of the tidal waves.
!#######################################################################
      IPRINT=0
      CALL ETPOTS(IUN14,IUN16,IUN24,IPRINT,IMODEL,DLAT,DLON,DH,DGRAV,&
        DAZ,IC,DJULD,DAMIN)
      IC2=IC+2
!-GCR fixed conversion
      ITH=INT(DTH)
!#######################################################################
!     Print output channel table:
!#######################################################################
      DO 100 JC=1,NC
      WRITE(IUN16,17003)  JC,CHANNEL(JC),CUNIT(IC2)
  100 WRITE(IUN23,17003)  JC,CHANNEL(JC),CUNIT(IC2)
!#######################################################################
!     Print alphanumeric comment:
!#######################################################################
      WRITE(IUN16,17010) FORTVERS
      DO 900 I=1,10
!-GCR fix to avoid writing empty TEXTHEADER into output file
  900 IF (.NOT.TRIM(CHEAD(I))=='') WRITE(IUN16,17012)  CHEAD(I)
!#######################################################################
!     Check wave groups and observed tidal parameters:
!     DG0:     amplitude factor.
!     DPHI0... phase lead in degree.
!#######################################################################
      WRITE(IUN16,17013) CUNIT(IC2)
      WRITE(IUN23,17013) CUNIT(IC2)
      DO 910 IG=1,NGR
!#######################################################################
!     Convert frequencies from cpd to rad per hour:
!#######################################################################
      DFRA(IG)=DFRA(IG)*15.D0*DRAD
      DFRE(IG)=DFRE(IG)*15.D0*DRAD
      DO 930 IW=1,NW
      IF(DTHFR(IW).LT.DFRA(IG)-1.D-10) NA(IG)=IW+1
      IF(DTHFR(IW).LT.DFRE(IG)+1.D-10) NE(IG)=IW
!-GCR: this is a bug fix: reset lowest wave number to 1 if frequency is zero!
      IF (DFRA(IG).EQ.0) NA(IG) = 1
!-GCR end bug fix
  930 CONTINUE
      IF(NA(IG).EQ.0) NA(IG)=1
!-GCR fixed conversion
      NAK=INT(NA(IG))
      NEK=INT(NE(IG))
!#######################################################################
!     Search for main wave of the group:
!#######################################################################
      DAM(IG)=0.D0
      DO 970 IW=NAK,NEK
      IF(IRIGID.EQ.1) DBODY(IW)=1.D0
      DTHAM=SQRT(DX0(IW)**2+DY0(IW)**2)
      IF(DTHAM.LT.DAM(IG)) GOTO 970
        DAM(IG)=DTHAM
        DBOD(IG)=DBODY(IW)
  970 CONTINUE
  910 CONTINUE
!#######################################################################
!     Check last group:
!#######################################################################
  980 IF(NE(NGR).GE.NA(NGR)) GOTO 990
      NGR=NGR-1
      GOTO 980
  990 CONTINUE
      DO 995 IG=1,NGR
      NANR=IWNR(NA(IG))
      NENR=IWNR(NE(IG))
      WRITE(IUN16,17015) IG,NANR,NENR,DG0(IG),DPHI0(IG),DAM(IG),DBOD(IG)
      WRITE(IUN23,17015) IG,NANR,NENR,DG0(IG),DPHI0(IG),DAM(IG),DBOD(IG)
      DPHI0(IG)=DPHI0(IG)*DRAD
  995 CONTINUE
      DO 996 IG=1,NGR
      DFAC=DG0(IG)/DBOD(IG)
      DO 996 IW=INT(NA(IG)),INT(NE(IG))
      IF(IRIGID.EQ.1) DBODY(IW)=1.D0
      DX0(IW)=DX0(IW)*DFAC*DBODY(IW)
      DX1(IW)=DX1(IW)*DFAC*DBODY(IW)
      DY0(IW)=DY0(IW)*DFAC*DBODY(IW)
      DY1(IW)=DY1(IW)*DFAC*DBODY(IW)
!&  (4 lines)
    IF(IMODEL.EQ.8) THEN
        DX2(IW)=DX2(IW)*DFAC*DBODY(IW)
        DY2(IW)=DY2(IW)*DFAC*DBODY(IW)
      ENDIF
  996 CONTINUE
      CALL GEOEXT(IUN16,IRESET,DEXTIM,DEXTOT)
!#######################################################################
!     Print hourly model tides with format 6F13.6:
!#######################################################################
 1000 WRITE(IUN16,17016) FORTVERS,COMPON(IC2),CUNIT(IC2)
      WRITE(IUN23,17019)
      WRITE(IUN23,17020) (DZERO(J),J=1,NC)
      ITMIN=0
      ITSEC=0
      DDAT=DBLE(ISPANH)/DDTH
!-GCR fixed conversion
      NDAT=INT(DDAT)
      CALL ETJULN(IUN16,ITY,ITM,ITD,DTH,DJULD0)
!#######################################################################
!     Loop over NDAT samples:
!     DTLIM is the time interval in hours for updatinmg the phases.
!     Depending on amplitude threshold DAMIN, the phases will be
!     updated
!     at each midnight for             DAMIN <= 1.D-8 m**2/s**2
!     at monthly interval for 1.D-8 <  DAMIN <= 1.D-6 m**2/s**2)
!     at yearly interval  for 1.D-6 <  DAMIN.
!#######################################################################
      DT=1.D99
      DTLIM=1.D0
      IF(DAMIN.GT.1.D-8) DTLIM=720.D0
      IF(DAMIN.GT.1.D-6) DTLIM=8760.D0
      DO 1090 I=1,NDAT,6
      DO 1020 J=1,6
      DJULD=DJULD0+DBLE(I+J-2)*DDTD
      DT2000=(DJULD-2451544.D0)/36525.0D0
      CALL ETGREN(IUN16,DJULD,ITY,ITM,ITD,DTH,NERR)
      IF(DT.LT.DTLIM) GOTO 1033
      IF(DTLIM.LT.10.D0.AND.DTH.GT.0.0001D0) GOTO 1033
!#######################################################################
!     Recompute phases at interval DTLIM:
!#######################################################################
      IPRINT=1
      CALL ETPHAS(IUN16,IPRINT,IMODEL,DLON,DJULD)
      DO 1025 IG=1,NGR
      DO 1025 IW=INT(NA(IG)),INT(NE(IG))
      DTHPH(IW)=DTHPH(IW)+DPHI0(IG)
!#######################################################################
!     Prepare arrays for recursion algorithm:
!#######################################################################
      DC0(IW)=COS(DTHPH(IW))
      DS0(IW)=SIN(DTHPH(IW))
      DDC(IW)=COS(DTHFR(IW)*DDTH)
 1025 DDS(IW)=SIN(DTHFR(IW)*DDTH)
      DT=0.D0
 1033 CONTINUE
      DGT=0.D0
      DO 1030 IG=1,NGR
      DO 1030 IW=INT(NA(IG)),INT(NE(IG))
      DGT=DGT+(DX0(IW)+DT2000*DX1(IW))*DC0(IW)+&
        (DY0(IW)+DT2000*DY1(IW))*DS0(IW)
! ksm (4 lines)
      IF(IMODEL.EQ.8) THEN
        DGT=DGT+DT2000*DT2000*DX2(IW)*DC0(IW)+&
        DT2000*DT2000*DY2(IW)*DS0(IW)
      ENDIF
      DUMMY  =DC0(IW)*DDC(IW)-DS0(IW)*DDS(IW)
      DS0(IW)=DS0(IW)*DDC(IW)+DC0(IW)*DDS(IW)
 1030 DC0(IW)=DUMMY
!#######################################################################
!     Compute pole tide correction and length of day tide correction:
!#######################################################################
      DPOLT=0.D0
      DLODT=0.D0
      IF(IC.NE.0) GOTO 1040
      IF(IPOLTC.EQ.1.OR.ILODTC.EQ.1) THEN
         CALL ETPOLC(IUN16,IUN30,IUN31,IPRINT,DJULD,DCLAT,DSLAT,DCLON,&
            DSLON,DPOLX,DPOLY,DUT1,DTAI,DLOD,DGPOL,DGPOLP,DGLOD,IKENN)
         DPOLT=DGPOL*DPOLTC
         DLODT=DGLOD*DLODTC
      ENDIF
 1040 CONTINUE
!-GCR rearranged the signals to properly reflect the flags
!-GCR reset DCOUT in case the input changes
      DCOUT=0D0
      DCOUT(1)=DGT+DPOLT+DLODT
!-GCR fixed to reflect output
      IF(IPOLTC.EQ.1.OR.ILODTC.EQ.1) THEN
          DCOUT(2)=DGT
      ENDIF
      JC=2
      IF(IPOLTC.EQ.1) THEN
          JC=JC+1
          DCOUT(JC)=DPOLT
      ENDIF
      IF(ILODTC.EQ.1) THEN
          JC=JC+1
          DCOUT(JC)=DLODT
      ENDIF
      DGI(J)=DCOUT(1)
      IDAT=ITY*10000+ITM*100+ITD
!-GCR fixed a bug whereby seconds and minutes wouldn't be rounded
! correctly in the original code
      ! calculate time
      ITH=INT(FLOOR(DTH))
      DTMIN=(DTH-DBLE(ITH))*60.D0
      ! modified to work with minutes rounding errors
      ITMIN=FLOOR(DTMIN+0.001388D0)
      IF(ITMIN.GE.60) THEN
         ITMIN=0
         ITH=ITH+1
      ENDIF
      DTSEC=DTH*3600.D0-DBLE(ITH)*3600.D0-DBLE(ITMIN)*60.D0
      ! modified to work with seconds rounding errors
      ITSEC=FLOOR(DTSEC+2.3148D-05)
      IF(ITSEC.GE.60) THEN
         ITSEC=0
         ITMIN=ITMIN+1
      ENDIF
      ITIM=ITH*10000+ITMIN*100+ITSEC
!-GCR end time fix
      WRITE(IUN23,17021) IDAT,ITIM,(DCOUT(JC),JC=1,NC)
      IF(ITIM.EQ.0) WRITE(SCR,17021) IDAT,ITIM,(DCOUT(JC),JC=1,NC)
!-GCR fill new array for handback to Python
      IF (ROWI.LE.ROWS) THEN
        ETPDATA(ROWI,1)=IDAT
        ETPDATA(ROWI,2)=ITIM
        ETPDATA(ROWI,3:6)=DCOUT
      ENDIF
      ! set header at first row once only
      IF (ROWI.EQ.1) THEN
        ETPUNIT=TRIM(ADJUSTL(CUNIT(IC2)))
        HEADER(3,:)=TRIM(CHANNEL(1)) //' [' // TRIM(ADJUSTL(ETPUNIT)) // ']'
        HEADER(4,:)=TRIM(CHANNEL(2)) //' [' // TRIM(ADJUSTL(ETPUNIT)) // ']'
        HEADER(5,:)=TRIM(CHANNEL(3)) //' [' // TRIM(ADJUSTL(ETPUNIT)) // ']'
        HEADER(6,:)=TRIM(CHANNEL(4)) //' [' // TRIM(ADJUSTL(ETPUNIT)) // ']'
        !WRITE (*,'(A20)') HEADER
      ENDIF
      ! advance row counter
      ROWI=ROWI+1
!-GCR end modification
      DT=DT+DDTH
 1020 CONTINUE
      DJULD=DJULD0+DBLE(I-1)*DDTD
      CALL ETGREN(IUN16,DJULD,ITY,ITM,ITD,DTH,NERR)
      IDAT=ITY*10000+ITM*100+ITD
!-GCR fixed a bug whereby seconds and minutes wouldn't be rounded
! correctly in the original code
      ! calculate time
      ITH=INT(FLOOR(DTH))
      DTMIN=(DTH-DBLE(ITH))*60.D0
      ! modified to work with minutes rounding errors
      ITMIN=FLOOR(DTMIN+0.001388D0)
      IF(ITMIN.GE.60) THEN
         ITMIN=0
         ITH=ITH+1
      ENDIF
      DTSEC=DTH*3600.D0-DBLE(ITH)*3600.D0-DBLE(ITMIN)*60.D0
      ! modified to work with seconds rounding errors
      ITSEC=FLOOR(DTSEC+2.3148D-05)
      IF(ITSEC.GE.60) THEN
         ITSEC=0
         ITMIN=ITMIN+1
      ENDIF
!-GCR end time fix
      ITIM=ITH*10000+ITMIN*100+ITSEC
      WRITE(IUN16,17017) IDAT,ITIM,(DGI(J),J=1,6)
 1090 CONTINUE
      WRITE(IUN23,'(I8)') C99
      WRITE(IUN23,'(I8)') C88
      CALL GEOEXT(IUN16,IRESET,DEXTIM,DEXTOT)
      WRITE(IUN16,17030) CPROJ,CFPRN,CFOUT,DEXTIM
      WRITE(SCR,17030)     CPROJ,CFPRN,CFOUT,DEXTIM
!-GCR close all the i/o files if still open
!      INQUIRE(UNIT=IUN15, OPENED=OPENSTAT)
!      IF (OPENSTAT) CLOSE(IUN15)
      INQUIRE(UNIT=IUN16, OPENED=OPENSTAT)
      IF (OPENSTAT) CLOSE(IUN16)
      INQUIRE(UNIT=IUN23, OPENED=OPENSTAT)
      IF (OPENSTAT) CLOSE(IUN23)

!#######################################################################
!     Format statements:
!#######################################################################
17002 FORMAT(&
     '     ******************************************************'/&
     '     *                                                    *'/&
     '     *  Program PREDICT, version ',A15,' F90.     *'/&
     '     *                                                    *'/&
     '     *         Prediction of earthtide signals.           *'/&
     '     *         for project ',A8,  '                       *'/&
     '     *                                                    *'/&
     '     *    The  Black  Forest  Observatory  Schiltach      *'/&
     '     *    wishes you much success when using PREDICT.     *'/&
     '     *                                                    *'/&
     '     ******************************************************'/)
17003 FORMAT(6x,'output channel no. ',I5,2X,A10,2X,A10)
17009 FORMAT(//' ***** Parameter IRIGID = 1 has been input. '/&
     ' ***** IRIGID =1 should only be used for comparison with model'/&
     ' ***** tides computed from ephemeris programs. For real world '/&
     ' ***** computations, use always IRIGID=0.'/)
17010 FORMAT(//6x,'Program PREDICT, version ',A11,' Fortran 90'//)
17012 FORMAT(1X,A64)
17013 FORMAT(///&
        6x,'Wave groups and observed tidal parameters'//&
        6x,'  no.  from    to ampl.fac. phase lead      ampl.  WD body '/&
        6x,'                               [deg]      [',A10,']'/)
17015 FORMAT(6x,3I6,2F10.4,1X,2F10.4)
17016 FORMAT(////&
        6x,'Program PREDICT, version ',A11,' Fortran 90'//&
        6x,'Component ',A24,' IN ',1X,A8//6X,'Date in UT'//)
17017 FORMAT(5X,I8.8,1X,I6.6,6F10.3)
17018 FORMAT(' Model tides from program PREDICT, version',A11/)
17019 FORMAT(/'C******************************************************'/&
!-GCR modified the output precision (is 6F13.6, was 6F10.3)
        'PREDICT              1.000000     1.000000     0.000000     3',&
        'PREDICT')
17020 FORMAT('77777777',8X,4F13.6)
!-GCR modified the output precision (is 6F13.6, was 6F10.3)
!17021 FORMAT(A19,1X,6F13.6)
17021 FORMAT(I8.8,1X,I6.6,1X,4F13.6)
17022 FORMAT(I9,1X,I2,4F13.6)
!-GCR end modified
17030 FORMAT(/&
        '      **********************************************'/&
        '      *   Program PREDICT finished the execution   *'/&
        '      *   for project ',A8,  '                     *'/&
        '      *   (Hopefully it was successfull).          *'/&
        '      *   Print  file is:            ',A13,' *'/&
        '      *   Output file is:            ',A13,' *'/&
        '      **********************************************'//&
        '      Execution time: ',F10.3,'  seconds'/)
END SUBROUTINE


SUBROUTINE ETASTN(IUN16,IPRINT,IMODEL,DLON,DJULD,DUT1,DAS,DASP,DDT)
!#######################################################################
!     Routine ETASTN, version 1996.05.25 Fortran 90.
!     The routine ETASTN computes the astronomical elements for
!     different tidal potential catalogues at a specific epoch, given
!     in UTC. The formulas for the astronomical elements have been
!     taken from Tamura (1987) and Simon et al. (1994).
!     Reference:
!     ----------
!     Simon, J.L., P. Bretagnon, J. Chapront, M. Chapront-Touze,
!        G. Francou and J. Laskar (1994): Numerical expressions for
!        precession formulae and mean elements for the Moon and the
!        planets. Astronomy and Atsrohysics, vo. 282, 663-683, 1994.
!     Tamura, Y. (1987): A harmonic development of the tide
!        generating potential. Bulletin d'Informations Marees
!        Terrestres vol. 99, 6813-68755, Bruxelles 1987.
!     All variables with D as first character are double precision.
!     Input parameter description:
!     ----------------------------
!     IUN16:       Unit number of formatted printout file.
!     IPRINT:      Printout parameter. For IPRINT=0, nothing will
!                  be printed on unit IUN16.
!     IMODEL:      Parameter describing the tidal potential catalogue.
!                  IMODEL = 1: Doodson (1921) catalogue.
!                  IMODEL = 2: Cartwright et al. (1973) catalogue.
!                  IMODEL = 3: Buellesfeld (1985) catalogue.
!                  IMODEL = 4: Tamura (1987) catalogue.
!                  IMODEL = 5: Xi (1989) catalogue.
!                  IMODEL = 6: Roosbeek (1996) catalogue.
!                  IMODEL = 7: Hartmann and Wenzel (1995) catalogue.
!&                 IMODEL = 8: Kudryavtsev (2004) catalogue.
!                  For IMODEL = 1...5, arguments are computed from
!&                 Tamura (1987) formulas. For IMODEL = 6, 7 and 8
!                  arguments are computed from Simon et al. (1994)
!                  formulas.
!     DJULD:       Julian date of the epoch in UTC.
!     Output parameter description:
!     -----------------------------
!     DAS(1):      Mean local Moontime in degree.
!     DAS(2):      Mean longitude of the Moon in degree.
!     DAS(3):      Mean longitude of the Sun  in degree.
!     DAS(4):      Mean longitude of the perigee of the Moon's orbit
!                  in degree.
!     DAS(5):      Negative mean longitude of the ascending node of
!                  the Moon's orbit in degree.
!     DAS(6):      Mean longitude of the perigee of the Suns's orbit
!                  in degree.
!     DAS(7):      Mean longitude of the Mercury in degree.
!     DAS(8):      Mean longitude of the Venus   in degree.
!     DAS(9):      Mean longitude of the Mars    in degree.
!     DAS(10):     Mean longitude of the Jupiter in degree.
!     DAS(11):     Mean longitude of the Saturn  in degree.
!     DASP(1...11): Time derivatives of the corresponding variables
!                  DAS in degree per hour.
!     Used routines:
!     --------------
!     ETDDTB: interpolates DDT = DTD - UTC from table.
!     Routine creation:  1994.07.30 by Hans-Georg Wenzel,
!                        Geodaetisches Institut,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE 1,
!                        Germany.
!                        Tel.: 0721-6082307,
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel.
!#######################################################################
      use PARAMS
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      REAL(8) DAS(11),DASP(11)
      SAVE
!      DATA DRAD/0.174532925197721D-001/
      D1MD=1.D0/(365250.D0*24.D0)
      DMJD=DJULD-2400000.5D0
!-GCR fixed conversion issue
      IMJD=INT(DMJD)
      DTH=(DMJD-DBLE(IMJD))*24.D0
!#######################################################################
!     Compute Universal Time epoch DTUT in Julian Centuries referring
!     to J2000:
!#######################################################################
      DTUT=(DJULD-2451545.0D0)/36525.D0
!#######################################################################
!     Correct DTH to UT1:
!#######################################################################
      DTH=DTH+DUT1/3600.D0
!#######################################################################
!     Compute epoch DT in Julian Centuries TDB referring to J2000
!     (1. January 2000 12 h.):
!#######################################################################
      DT=(DMJD-51544.5D0)/36525.0D0
!#######################################################################
!     Correct time from UTC to TDT:
!#######################################################################
      CALL ETDDTB(IUN16,IPRINT,DJULD,DDT)
      DT=DT+DDT/3155760000.D0
      IF(IPRINT.GT.0) WRITE(IUN16,17001) DMJD
      DT2=DT*DT
      DTC1=DT
      DTC2=DTC1*DTC1
      DTC3=DTC2*DTC1
      DTC4=DTC3*DTC1
      DTM1=DT/10.D0
      DTM2=DTM1*DTM1
      DTM3=DTM2*DTM1
      DTM4=DTM3*DTM1
      DTM5=DTM4*DTM1
      DTM6=DTM5*DTM1
      IF(IMODEL.GE.6) GOTO 2000
!#######################################################################
!     Compute astronomical elements from TAMURA's 1987 formulas:
!#######################################################################
      DTUT2=DTUT*DTUT
      DTUT3=DTUT2*DTUT
      DAL=280.4606184D0 + 36000.7700536D0*DTUT + 0.00038793D0*DTUT2 -0.0000000258D0*DTUT3
      DALP=(36000.7700536D0 +2.0D0*0.00038793D0*DTUT-3.0D0*0.0000000258D0*DTUT2)/(24.0D0*36525.D0)
      DS=218.316656D0+481267.881342D0*DT-0.001330D0*DT2
      DSP=(481267.881342D0-2.0D0*0.001330D0*DT)/(24.D0*36525.0D0)
      DH=280.466449D0+36000.769822D0*DT+0.0003036D0*DT2
      DHP=(36000.769822D0+2.0D0*0.0003036D0*DT)/(24.D0*36525.0D0)
      DDS=0.0040D0*COS((29.D0+133.0D0*DT)*DRAD)
      DDSP=(-0.0040D0*133.0D0*DRAD*SIN((29.D0+133.0D0*DT)*DRAD))/(24.0D0*36525.0D0)
      DDH=0.0018D0*COS((159.D0+19.D0*DT)*DRAD)
      DDHP=(-0.0018D0*19.0D0*DRAD*SIN((159.D0+19.D0*DT)*DRAD))/(24.0D0*36525.0D0)
      DAS(1)=DAL-DS+DLON+DTH*15.0D0
      DAS(2)=DS+DDS
      DAS(3)=DH+DDH
      DAS(4)=83.353243D0  +4069.013711D0*DT -0.010324D0*DT2
      DAS(5)=234.955444D0 +1934.136185D0*DT -0.002076D0*DT2
      DAS(6)=282.937348D0 +   1.719533D0*DT +0.0004597D0*DT2
!#######################################################################
!     Compute the speeds in degree per hour:
!#######################################################################
      DASP(1)=DALP-DSP+15.0D0
      DASP(2)=DSP+DDSP
      DASP(3)=DHP+DDHP
      DASP(4)=(4069.013711D0-2.0D0*0.010324D0*DT)/(24.0D0*36525.0D0)
      DASP(5)=(1934.136185D0-2.0D0*0.002076D0*DT)/(24.0D0*36525.0D0)
      DASP(6)=(1.719533D0+2.0D0*0.0004597D0*DT)/(24.0D0*36525.0D0)
      GOTO 3000
 2000 CONTINUE
!#######################################################################
!     Mean longitude of the Moon (from Simon et al. 1994):
!#######################################################################
      DS =218.3166456300D0+481267.8811957500D0*DTC1 &
                               -0.0014663889D0*DTC2 &
                               +0.0000018514D0*DTC3 &
                               -0.0000000153D0*DTC4
      DSP=(+481267.8811957500D0 &
               -2.D0*0.0014663889D0*DTC1 &
               +3.D0*0.0000018514D0*DTC2 &
               -4.D0*0.0000000153D0*DTC3)/(36525.D0*24.D0)
!#######################################################################
!     Mean longitude of the Sun (from Simon et al. 1994):
!#######################################################################
      DH=280.46645016D0+   360007.6974880556D0*DTM1 &
                               +0.0303222222D0*DTM2 &
                               +0.0000200000D0*DTM3 &
                               -0.0000653611D0*DTM4
      DHP=      (360007.6974880556D0 &
                +2.D0*0.0303222222D0*DTM1 &
                +3.D0*0.0000200000D0*DTM2 &
                -4.D0*0.0000653611D0*DTM3)*D1MD
      DAS(1) =DH -DS +DLON+DTH*15.0D0
      DASP(1)=DHP-DSP+15.0D0
!#######################################################################
!     Modification for Roosbeek (1996) tidal potential catalogue:
!     This modification has been programmed by Roosbeek himself.
!#######################################################################
      IF(IMODEL.EQ.6) THEN
        DGMST=280.460618375D0+360007.700536D0*DTM1 &
                            +0.038793333333D0*DTM2 &
                            -0.000025833333D0*DTM3
        DGMSTP=               (360007.700536D0 &
                            +2.D0*0.038793333333D0*DTM1 &
                            -3.D0*0.000025833333D0*DTM2)*D1MD
        DAS(1) =DGMST-DS+DLON+DTH*15.D0
        DASP(1)=DGMSTP-DSP+15.D0
      ENDIF
!#######################################################################
!     This correction is necessary because for the determination of
!     the HW95 tidal potential catalogue the difference DDT=TDT-UTC
!     has been neglected. If the GMST would have been computed with
!     with the correct DDT, the effect in GMST would be 1.0027*DDT.
!     This effect is corrected below.
!#######################################################################
      DAS(1)=DAS(1)-0.0027D0*DDT*15.D0/3600.D0
      DAS(2) =DS
      DASP(2)=DSP
      DAS(3) =DH
      DASP(3)=DHP
!#######################################################################
!     Mean longitude of lunar perigee (from Simon et al. 1994):
!#######################################################################
      DAS(4)= 83.35324312D0+40690.1363525000D0*DTM1 &
                               -1.0321722222D0*DTM2 &
                               -0.0124916667D0*DTM3 &
                               +0.0005263333D0*DTM4
      DASP(4)=            (+40690.1363525000D0 &
                               -2.D0*1.0321722222D0*DTM1 &
                               -3.D0*0.0124916667D0*DTM2 &
                               +4.D0*0.0005263333D0*DTM3)*D1MD
!#######################################################################
!     Negative mean longitude of the ascending node of the Moon
!     in degree (from Simon et al. 1994):
!#######################################################################
      DAS(5)=234.95544499D0+19341.3626197222D0*DTM1 &
                               -0.2075611111D0*DTM2 &
                               -0.0021394444D0*DTM3 &
                               +0.0001649722D0*DTM4
      DASP(5)=            (+19341.3626197222D0 &
                               -2.D0*0.2075611111D0*DTM1 &
                               -3.D0*0.0021394444D0*DTM2 &
                               +4.D0*0.0001649722D0*DTM3)*D1MD
!#######################################################################
!    Mean longitude of solar perigee computed from
!    argument no. 2 - D -l':
!#######################################################################
      DAS(6)=282.93734098D0       +17.1945766666D0*DTM1 &
                                   +0.0456888889D0*DTM2 &
                                   -0.0000177778D0*DTM3 &
                                   -0.0000334444D0*DTM4
      DASP(6)=                        (+17.1945766666D0 &
                                   +2.D0*0.0456888889D0*DTM1 &
                                   -3.D0*0.0000177778D0*DTM2 &
                                   -4.D0*0.0000334444D0*DTM3)*D1MD
 3000 CONTINUE
!#######################################################################
!     Longitudes of the planets from Simon et al. 1994:
!     Mercury = 7, Venus = 8, Mars = 9, Jupiter = 10, Saturn = 11.
!#######################################################################
      DAS( 7)=252.25090552D0+1494740.7217223248D0*DTM1 &
                                  +0.0303498417D0*DTM2 &
                                  +0.0000181167D0*DTM3 &
                                  -0.0000652778D0*DTM4 &
                                  -0.0000004972D0*DTM5 &
                                  +0.0000000556D0*DTM6
      DASP( 7)=            (+1494740.7217223248D0 &
                             +2.D0*0.0303498417D0*DTM1 &
                             +3.D0*0.0000181167D0*DTM2 &
                             -4.D0*0.0000652778D0*DTM3 &
                             -5.D0*0.0000004972D0*DTM4 &
                             +6.D0*0.0000000556D0*DTM5)*D1MD
      DAS( 8)=181.97980085D0+ 585192.1295333027D0*DTM1 &
                                  +0.0310139472D0*DTM2 &
                                  +0.0000149111D0*DTM3 &
                                  -0.0000653222D0*DTM4 &
                                  -0.0000004972D0*DTM5 &
                                  +0.0000000556D0*DTM6
      DASP( 8)=             (+585192.1295333027D0 &
                             +2.D0*0.0310139472D0*DTM1 &
                             +3.D0*0.0000149111D0*DTM2 &
                             -4.D0*0.0000653222D0*DTM3 &
                             -5.D0*0.0000004972D0*DTM4 &
                             +6.D0*0.0000000556D0*DTM5)*D1MD
      DAS( 9)=355.43299958D0+ 191416.9637029695D0*DTM1 &
                                  +0.0310518722D0*DTM2 &
                                  +0.0000156222D0*DTM3 &
                                  -0.0000653222D0*DTM4 &
                                  -0.0000005000D0*DTM5 &
                                  +0.0000000556D0*DTM6
      DASP( 9)=             (+191416.9637029695D0 &
                             +2.D0*0.0310518722D0*DTM1 &
                             +3.D0*0.0000156222D0*DTM2 &
                             -4.D0*0.0000653222D0*DTM3 &
                             -5.D0*0.0000005000D0*DTM4 &
                             +6.D0*0.0000000556D0*DTM5)*D1MD
      DAS(10)= 34.35151874D0+  30363.0277484806D0*DTM1 &
                                  +0.0223297222D0*DTM2 &
                                  +0.0000370194D0*DTM3 &
                                  -0.0000523611D0*DTM4 &
                                  +0.0000011417D0*DTM5 &
                                  -0.0000000389D0*DTM6
      DASP(10)=              (+30363.0277484806D0 &
                             +2.D0*0.0223297222D0*DTM1 &
                             +3.D0*0.0000370194D0*DTM2 &
                             -4.D0*0.0000523611D0*DTM3 &
                             +5.D0*0.0000011417D0*DTM4 &
                             -6.D0*0.0000000389D0*DTM5)*D1MD
      DAS(11)= 50.07744430D0+  12235.1106862167D0*DTM1 &
                                  +0.0519078250D0*DTM2 &
                                  -0.0000298556D0*DTM3 &
                                  -0.0000972333D0*DTM4 &
                                  -0.0000045278D0*DTM5 &
                                  +0.0000002861D0*DTM6
      DASP(11)=              (+12235.1106862167D0 &
                             +2.D0*0.0519078250D0*DTM1 &
                             -3.D0*0.0000298556D0*DTM2 &
                             -4.D0*0.0000972333D0*DTM3 &
                             -5.D0*0.0000045278D0*DTM4 &
                             +6.D0*0.0000002861D0*DTM5)*D1MD
      DO 3110 I=1,11
      DAS(I)=MOD(DAS(I),360.0D0)
      IF(DAS(I).LT.0.D0) DAS(I)=DAS(I)+360.0D0
 3110 CONTINUE
      IF(IPRINT.EQ.0) RETURN
!#######################################################################
!     Print astronomical elements:
!#######################################################################
      WRITE(IUN16,17004) (DAS(K),DASP(K),K=1,11)

! 5000 CONTINUE
      WRITE(IUN16,17030)
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(//6x,'Routine ETASTN, version 1996.05.25.'// &
        6x,'Astronomic elements for initial epoch '/ &
        6x,'Modified Julian date (TDT)    : ',F15.4/)
17004 FORMAT(// &
        6x,'local Moontime      F01',F20.11,' deg  F01.',F18.11,' deg/h'/ &
        6x,'lunar longitude     F02',F20.11,' deg  F02.',F18.11,' deg/h'/ &
        6x,'solar longitude     F03',F20.11,' deg  F03.',F18.11,' deg/h'/ &
        6x,'lunar perigee       F04',F20.11,' deg  F04.',F18.11,' deg/h'/ &
        6x,'lunar node longit.  F05',F20.11,' deg  F05.',F18.11,' deg/h'/ &
        6x,'solar perigee       F06',F20.11,' deg  F06.',F18.11,' deg/h'/ &
        6x,'longitude   Mercury F07',F20.11,' deg  F07.',F18.11,' deg/h'/ &
        6x,'longitude   Venus   F08',F20.11,' deg  F08.',F18.11,' deg/h'/ &
        6x,'longitude   Mars    F09',F20.11,' deg  F09.',F18.11,' deg/h'/ &
        6x,'longitude   Jupiter F10',F20.11,' deg  F10.',F18.11,' deg/h'/ &
        6x,'longitude   Saturn  F11',F20.11,' deg  F11.',F18.11,' deg/h'/)
17030 FORMAT(/6x,'***** Routine ETASTN finished the execution.'/)
END SUBROUTINE


SUBROUTINE ETDDTA(IUN16,IUN27,IPRINT)
!#######################################################################
!     Routine ETDDTA, version 1996.05.29 Fortran 90.
!     The routine ETDDTA reads a table of DDT = ET-UTC or TDT-UTC
!     from file etddt.dat. The file will be opened and after use
!     closed by the routine.
!     The table on file etddt.dat has to be extended, when new data
!     are available.
!     Input parameter description:
!     ----------------------------
!     IUN16:       Unit number of formatted printer unit.
!     IUN27:       Unit number of formmated unit, on which the table
!                  of DDT has to be stored before the call of routine
!                  ETDDTA. This unit will be opened by routine ETDDTA
!                  as File: etddt.dat
!     IPRINT:      Printout parameter. For IPRINT=0, nothing will be
!                  written to IUN16.
!     COMMON /DDT/:
!     -------------
!     DDTTAB:      Array (1..3,1..100) containing the table of year,
!                  Julian date and DDT.
!     NDDTAB:      Number of defined entries in table DDTTAB.
!     Routine creation:  1995.12.20 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082307,
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.29 by Hans-Georg Wenzel,
!#######################################################################
      use DDT_MOD
      use PARAMS
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      CHARACTER*10 CTEXT(8)
      SAVE
      OPEN(UNIT=IUN27,FILE=TRIM(COMDIR)//TRIM(ETDDTDAT),STATUS='OLD')
  100 READ(IUN27,17001) (CTEXT(I),I=1,8)
      IF(IPRINT.GT.0)  WRITE(IUN16,17002) (CTEXT(I),I=1,8)
      IF(CTEXT(1).NE.CENDT) GOTO 100
      NDDTAB=1
  200 READ(IUN27,17003,END=1000) DDTTAB(1,NDDTAB),DDTTAB(2,NDDTAB),DDTTAB(3,NDDTAB)
      IF(IPRINT.NE.0) THEN
        WRITE(IUN16,17004) DDTTAB(1,NDDTAB),DDTTAB(2,NDDTAB),DDTTAB(3,NDDTAB)
      ENDIF
      NDDTAB=NDDTAB+1
      GOTO 200
 1000 NDDTAB=NDDTAB-1
      CLOSE(IUN27)
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(8A10)
17002 FORMAT(1X,7A10,A8)
17003 FORMAT(F10.5,F15.6,F10.3)
17004 FORMAT(F15.5,F15.6,F15.3)
      RETURN
END SUBROUTINE


SUBROUTINE ETDDTB(IUN16,IPRINT,DTUJD,DDT)
!#######################################################################
!     Routine ETDDTB, version 1996.05.25 Fortran 90.
!     All variables with D as first character are DOUBLE PRECISION.
!     Input parameter description:
!     ----------------------------
!     IUN16:       Formatted printer unit.
!     IPRINT:      Printout parameter. For IPRINT=0, nothing will be
!                  written on unit IUN16.
!     DTUJD:       Julian date of epoch (Universal time).
!     Output parameter description:
!     -----------------------------
!     DDT:         Difference ET - UTC   resp. TDT - UTC in seconds
!                  from 1955.5 until now. For epochs less 1955.5, DDT
!                  is set to 31.59 s.
!                  For epochs exceeding the last tabulated epoch, DDT
!                  is set to the last tabulated DDT.
!                  ET  is Ephemeris Time.
!                  TDT is Terrestrial Dynamical Time.
!                  UTC is Universal Time Coordinated, as broadcasted
!                  by radio or GPS satellites.
!     COMMON /DDT/:
!     -------------
!     DDTTAB:      Array (1..3,1..300) containing the table of year,
!                  Julian date and DDT.
!     NDDTAB:      Number of defined entries in table DDTTAB.
!     Execution time:
!     ---------------
!     1.38 microsec per call on a 100 MHz Pentium using Lahey LF90
!                   compiler.
!     Routine creation:  1995.12.20 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082307,
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel,
!#######################################################################
      use DDT_MOD
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      DATA IWARN/1/,ITAB/1/
      IF(DTUJD.LT.DDTTAB(2,NDDTAB)) GOTO 100
!#######################################################################
!     DTUJD exceeds last tabulated epoch DDTTAB(2,NDDTAB).
!#######################################################################
      DDT=DDTTAB(3,NDDTAB)
      IF(IWARN.EQ.1) WRITE(IUN16,17003) DDTTAB(1,NDDTAB)
      IWARN=0
      RETURN
!#######################################################################
!     Look at table at position ITAB.
!#######################################################################
  100 CONTINUE
      IF(DTUJD.GE.DDTTAB(2,ITAB).AND.DTUJD.LT.DDTTAB(2,ITAB+1)) GOTO 230
      IF(DTUJD.LT.DDTTAB(2,ITAB)) THEN
        ITAB=ITAB-1
        IF(ITAB.GT.0) GOTO 100
!#######################################################################
!     Set DDT to first tabulated value and return:
!#######################################################################
        ITAB=1
        DDT=DDTTAB(3,1)
        RETURN
      ENDIF
      IF(DTUJD.GT.DDTTAB(2,ITAB+1)) THEN
         ITAB=ITAB+1
         IF(ITAB.LT.NDDTAB) GOTO 100
!#######################################################################
!     Set DDT to last tabulated value and return:
!#######################################################################
         ITAB=NDDTAB
         DDT=DDTTAB(3,NDDTAB)
         RETURN
      ENDIF
!#######################################################################
!     Interpolate table between position ITAB and ITAB+1:
!#######################################################################
!GCR-update:
  230 CONTINUE
      ! for gregorian year >= 1972: just use the tabulated leap seconds
      ! this has been added to avoid interpolation between leap seconds
      IF (DTUJD >= 2441317.5) THEN
         DDT=DDTTAB(3,ITAB)
      ! for gregorian year < 1972 use interpolation (original code!)
      ELSE
         DDT=(DDTTAB(3,ITAB+1)*(DTUJD-DDTTAB(2,ITAB))-DDTTAB(3,ITAB)* &
    		(DTUJD-DDTTAB(2,ITAB+1)))/(DDTTAB(2,ITAB+1)-DDTTAB(2,ITAB))
      ENDIF
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(//' Routine ETDDTB.FOR, version 1996.05.25.'// &
        ' List of tables:'// &
        '       No.           Juld            DTX       DTY'//)
17002 FORMAT(I10,2F15.5,F10.3)
17003 FORMAT(/ &
        ' ***** Warning from routine ETDDTB.FOR, version 1996.05.25.'/ &
        ' ***** Epoch exceeds the last tabulated value:',F10.5/ &
        ' ***** DDT of last tabulated epoch is used.'/ &
        ' ***** Please try to update tabels in file etddt.dat.'/)
END SUBROUTINE


SUBROUTINE PREDIN(IUN16,IPRINT)
!#######################################################################
!     Routine PREDIN, version 1996.05.25 Fortran 90.
!     The routine reads the control parameter file *.INI and returns
!     the control parameters via COMMON /CONTROL3/ and /CONTROL4/.
!     Description of input parameters:
!     --------------------------------
!     IUN15:       Unit number of formatted control parameter file.
!     IUN16:       Unit number of formatted printer file.
!     IPRINT:      Print parameter. For IPRINT = 0, nothing will be
!                  written on IUN16.
!     Description of COMMON block /CONTROL3/:
!     ---------------------------------------
!     DLAT:        Stations latitude in degree, referring to WGS84.
!     DLON:        Stations longitude in degree, positiv east of
!                  Greenwich, referring to WGS84.
!     DH:          Ellipsoidal height of the station in meter
!                  referring to WGS84.
!     DGRAV:       Gravity of the station in m/s**2 (necessary for
!                  tidal tilt only).
!     DAZ:         Azimuth of the earth tide sensor in degree (only
!                  for tilt and horizontal strain).
!     DFRA:        Lowest  frequency within wave group in deg/h.
!     DFRE:        Highest frequency within wave group in deg/h.
!     DG0:         amplitude factor.
!     DPHI0:       phase lead in degree.
!     DATLIM:      Threshold for data snooping.
!     DAMIN:       Amplitude threshold for the tidal potential
!                  catalogue in m**2/s**2.
!     DPOLTC:      Pole tide amplitude factor.
!     DLODTC:      LOD  tide amplitude factor.
!     IDTSEC:      Sampling interval in seconds.
!     Description of COMMON block /CONTROL4/:
!     ---------------------------------------
!     IC:          Earth tide component.
!     IR:
!     ITY:         Year       of the initial epoch.
!     ITM:         Month      of the initial epoch.
!     ITD:         Day        of the initial epoch.
!     ITH:         Hour (UTC) of the initial epoch.
!     IDA:
!     KFILT:
!     IPROBS:
!     IPRLF:
!     IMODEL:      Tidal potential catalogue.
!     IRIGID:
!     IHANN:
!     IQUICK:
!     ISPANH:      Span of prediction in hours
!     NGR:         Number of wave groups.
!     NF:          Number of meteorological parameters.
!     IREG:
!     CFY1:
!     CFY2:
!     CINST:       Earth tide sensor name (CHARACTER*10).
!     CNSY:
!     CHEAD:
!     Program creation:  1994.11.01 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE 1.
!                        Tel.: 0721-6082301.
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last Modification: 1996.05.25 by Hans-Georg Wenzel.
!#######################################################################
      use MAX_PARS
      use CONTROLMOD
      use INOUT
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
!      CHARACTER CINPUT*100,CINTERN*100,CREST*100,CONTROL*10
!#######################################################################
!     Define default parameters:
!#######################################################################
      IH=1
      IGR=0
      IF=0
      DAMIN=1.D-8
      DPOLTC=1.16D0
      DLODTC=1.16D0
      ITH=0
      IMODEL=8
      IRIGID=0
!      REWIND IUN15
!#######################################################################
!     Read control record:
!#######################################################################
!  100 CONTINUE
!      READ(IUN15,17001,END=5000) CINPUT
!      II=INDEX(CINPUT,'=')
!      IF(II.EQ.0) GOTO 100
!#######################################################################
!     Input record contains an equal sign at position II:
!#######################################################################
!      CONTROL=CINPUT(1:II-1)
!#######################################################################
!     Search for # in the same record:
!#######################################################################
      ! NLE=LEN(CINPUT)
      ! INBL=NLE
      ! DO 200 I=II+1,NLE
      ! IF(CINPUT(I:I).NE.'#') GOTO 200
      ! INBL=I
      ! GOTO 210
  ! 200 CONTINUE
  ! 210 CREST=CINPUT(II+1:INBL-1)
      ! WRITE(IUN16,17006) CREST

!-GCR set variables that were previously read from INI file
! REQUIRED PARAMETERS:
!     Latitude 'STATLATITU' DLAT
      DLAT = ARGSIN(1)
!     Longitude 'STATLONITU') DLON
      DLON = ARGSIN(2)
!     Height 'STATELEVAT' DH
      DH = ARGSIN(3)
!     Startdate 'INITIALEPO' ITY,ITM,ITD
      ITY = INT(ARGSIN(4))
      ITM = INT(ARGSIN(5))
      ITD = INT(ARGSIN(6))
!     Samprate 'SAMPLERATE' IDTSEC
      IDTSEC = INT(ARGSIN(8))
!     Duration 'PREDICSPAN'  ISPANH
      ISPANH = INT(ARGSIN(7))
! OPTIONAL PARAMETERS:
!     Gravity 'STATGRAVIT' DGRAV
      DGRAV = ARGSIN(9)
!     Azimuth 'STATAZIMUT' DAZ
      DAZ = ARGSIN(10)
!     Potential catalogue 'TIDALPOTEN' IMODEL
      IMODEL = INT(ARGSIN(11))
!     Tidal component 'TIDALCOMPO' IC
      IC = INT(ARGSIN(12))
!     Amplitude truncation 'AMTRUNCATE' DAMIN
      DAMIN = ARGSIN(13)
!     Pole tide correction: 'POLTIDECOR' DPOLTC
      DPOLTC = ARGSIN(14)
!     Length of day tide correction: 'LODTIDECOR' DLODTC
      DLODTC = ARGSIN(15)
! 	  reset the wave group parameters to avoid errors
      NGR = NUMWG
      DFRA = FQMIN
      DFRE = FQMAX
      DG0 = AMPF
      DPHI0 = PHASEF

!#######################################################################
!     Search  for textheader:
!#######################################################################
      ! IF(CONTROL.NE.'TEXTHEADER') GOTO 3400
      ! IF(IH.GT.10) GOTO 3300
      ! CHEAD(IH)=CREST
      ! IH=IH+1
      ! GOTO 100
! 3300 CONTINUE
! 3400 CONTINUE
!#######################################################################
!     Unknown control parameter name:
!#######################################################################
!      WRITE(IUN16,17005) CONTROL
!      GOTO 100
! 5000 CONTINUE
! GCR: the number of waves is set via inout handover
!      NGR=IGR
      NF=IF
      IF(IPRINT.EQ.0) RETURN
!#######################################################################
!     Print first part of control parameters:
!#######################################################################
!& NH=IH changed by ksm, otherwise NH=11, dimension of CHEAD is 10 only
      NH=IH-1
      DO 5010 IH=1,NH
 5010 WRITE(IUN16,17114) CHEAD(IH)
      WRITE(IUN16,17111) IDTSEC,DLAT,DLON,DH,DGRAV,DAZ,DAMIN
!#######################################################################
!     Print second part of control parameters:
!#######################################################################
      WRITE(IUN16,17112) IC,IR,ITY,ITM,ITD,ITH,IMODEL
      DO 5020 IGR=1,NGR
 5020 WRITE(IUN16,17113) DFRA(IGR),DFRE(IGR),DG0(IGR),DPHI0(IGR)
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(A75)
17002 FORMAT(1X,A75)
17003 FORMAT(' control parameter is: ',A10)
17004 FORMAT(' numerical parameter is: ',A10)
17005 FORMAT(' *** unknown control parameter:',A10)
17006 FORMAT(' CREST=',A15)
17111 FORMAT(/ &
        6x,'sample interval                              :',I10,' s'/ &
        6x,'stations latitude  in degree                 :',F10.4/ &
        6x,'stations longitude in degree                 :',F10.4/ &
        6x,'stations height    in meter                  :',F10.3/ &
        6x,'stations gravity   in m/s**2                 :',F10.4/ &
        6x,'stations azimuth from north in degree        :',F10.4/ &
        6x,'truncation of tidal waves at (m**2/s**2)     :',D10.3)
17112 FORMAT( &
        6x,'earth tide component                         : ',I10/ &
        6x,'print tidal component development (1=yes)    : ',I10/ &
        6x,'initial epoch for tidal development          : ',I4,I3,I3,I3/ &
        6x,'tidal potential catalogue                    : ',I10/)
17113 FORMAT(6x,'wave group : ',2F10.6,2F10.4,1X)
17114 FORMAT(1X,A64)
END


SUBROUTINE ETGCON(IUN16,IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
!#######################################################################
!     Routine ETGCON, version 1997.03.03 Fortran 90.
!     The routine ETGCON computes the geodetic coefficients for
!     the tidal potential developments, Hartmann and Wenzel
!     normalization.
!     All variables with D as first character are DOUBLE PRECISION.
!     Input parameter description:
!     ----------------------------
!     IUN16:       formatted line printer unit.
!     DLAT:        ellipsoidal latitude in degree, referring to
!                  geodetic reference system GRS80.
!     DLON:        ellipsoidal longitude in degree, referring to
!                  geodetic reference system GRS80, positiv east of
!                  Greenwhich.
!     DH:          ellipsoidal height in meter, referring to geodetic
!                  reference system GRS80.
!     DGRAV:       gravity in m/s**2. If DGRAV less than  9.50 m/s**2,
!                  DGRAV will be overwritten by normal gravity
!                  referring to geodetic reference system 1980.
!     DAZ:         azimuth in degree from north direction counted
!                  clockwise (necessary for tidal tilt only).
!     IC:          Earth tide component to be computed.
!                  IC=-1: tidal potential, geodetic coefficients
!                         in m**2/s**2.
!                  IC= 0: vertical tidal acceleration (gravity tide),
!                         geodetic coefficients in nm/s**2 (positive
!                         down).
!                  IC= 1: horizontal tidal acceleration (tidal tilt)
!                         in azimuth DAZ, geodetic coefficients in
!                         mas = arc sec/1000.
!                  IC= 2: vertical tidal displacement, geodetic
!                         coefficients in mm.
!                  IC= 3: horizontal tidal displacement in azimuth
!                         DAZ, geodetic coefficients in mm.
!                  IC= 4: vertical tidal strain, geodetic coefficients
!                         in 10**-9 = nstr.
!                  IC= 5: horizontal tidal strain in azimuth DAZ,
!                         geodetic coefficients in 10**-9 = nstr.
!                  IC= 6: areal tidal strain, geodetic coefficients
!                         in 10**-9 = nstr.
!                  IC= 7: shear tidal strain, geodetic coefficients
!                         in 10**-9 = nstr.
!                  IC= 8: volume tidal strain, geodetic coefficients
!                         in 10**-9 = nstr.
!                  IC= 9: ocean tides, geodetic coefficients in
!                         millimeter.
!     Output parameter description:
!     -----------------------------
!     DGK:         array (1...25) of geodetic coefficients.
!                  The geodetic coefficient of degree L and order M
!                  is stored in DGK(J) with J=L*(L+1)/2+M-2.
!     DPK:         array (1...25) of phases in degree.
!                  The phase for degree L and order M is stored in
!                  DPK(J) with J=L*(L+1)/2+M-2.
!     Used routines:
!     --------------
!     ETLOVE: computes latitude dependent elastic parameters.
!     ETLEGN: computes fully normalized Legendre functions and their
!             derivatives.
!     Numerical accuracy:
!     -------------------
!     The routine has been tested under operation system MS-DOS and
!     UNIX in REAL(8) (8 byte words = 15 digits) using
!     different compilers.
!     References:
!     Wilhelm, H. and W. Zuern (1984): Tidal forcing field.
!           In: Landolt-Boernstein, Zahlenwerte und Funktionen aus
!           Naturwissenschaften und Technik, New series, group V,
!           Vol. 2, Geophysics of the Solid Earth, the Moon and the
!           Planets, Berlin 1984.
!     Zuern, W. and  H. Wilhelm (1984): Tides of the solid Earth.
!           In: Landolt-Boernstein, Zahlenwerte und Funktionen aus
!           Naturwissenschaften und Technik, New series, group V, Vol.
!           2, Geophysics of the Solid Earth, the Moon and the Planets,!
!           Berlin 1984.
!     Routine creation:  1988.01.29 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082307,
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1997.03.03 by Hans-Georg Wenzel.
!#######################################################################
      use LOVE
      use PARAMS
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      REAL(8) DGK(25),DPK(25),DGX(25),DGY(25),DGZ(25)
      REAL(8) DP0(25),DP1(25)
      SAVE
!#######################################################################
!     Definition of parameters of Geodetic Reference System 1980.
!     DEA  is major semi axis in meter.
!     DEE  is square of first excentricity (without dimension).
!#######################################################################
      DATA DEA/6378136.3D0/,DEE/6.69439795140D-3/
      IF(IPRINT.GT.0) WRITE(IUN16,17000) DEA,DEE
!#######################################################################
!     DCLAT is cos and DSLAT is sin of ellipsoidal latitude.
!#######################################################################
      DCLAT=COS(DLAT*DRAD)
      DSLAT=SIN(DLAT*DRAD)
!#######################################################################
!     Compute normal gravity in m/s**2:
!#######################################################################
      IF(DGRAV.LT.9.50D0) DGRAV=9.78032677D0*(1.D0+0.001931851353D0* &
        DSLAT**2)/SQRT(1.D0-DEE*DSLAT**2)-0.3086D-5*DH
!#######################################################################
!     Compute ellipsoidal curvature radius DN in meter.
!#######################################################################
      DN=DEA/SQRT(1.D0-DEE*DSLAT**2)
!#######################################################################
!     Compute geocentric latitude DPSI in degree:
!#######################################################################
      DPSI=DRO*ATAN(((DN*(1.D0-DEE)+DH)*DSLAT)/((DN+DH)*DCLAT))
      DTHET=90.D0-DPSI
      DCT=COS(DTHET*DRAD)
      DST=SIN(DTHET*DRAD)
!#######################################################################
!     Compute fully normalized spherical harmonics:
!#######################################################################
      CALL ETLEGN(DCT,DST,LMAX,DP0,DP1)
!#######################################################################
!     Compute geocentric radius DR in meter:
!#######################################################################
      DR=SQRT((DN+DH)**2*DCLAT**2+(DN*(1.D0-DEE)+DH)**2*DSLAT**2)
      IF(IPRINT.GT.0) WRITE(IUN16,17001) DLAT,DPSI,DLON,DH,DGRAV,DR,IC,DAZ
      DF=DRO*3.600D-3/DGRAV
!#######################################################################
!     Compute latitude dependent elastic parameters from Wahr-Dehant-
!     Zschau model:
!#######################################################################
      CALL ETLOVE(IUN16,IPRINT,DLAT,DH)
!#######################################################################
!     DCPSI is cos and DSPSI is sin of geocentric latitude.
!#######################################################################
      DCPSI=COS(DPSI*DRAD)
      DSPSI=SIN(DPSI*DRAD)
!#######################################################################
!     Compute spherical geodetic coefficients.
!     DGK contains coefficients for potential              in m**2/s**2!
!     DGX contains coefficients for north    accelerations in nm/s**2.
!     DGY contains coefficients for east     accelerations in nm/s**2.
!     DGZ contains coefficients for vertical accelerations in nm/s**2.
!#######################################################################
      DRDA=DR/DEA
      DO 10 LI=2,LMAX
      DRDADL=DRDA**LI
      DO 10 MI=0,LI
      J=LI*(LI+1)/2+MI-2
      DGK(J)=      DRDADL*DP0(J)
      DGX(J)=-1.D0*DRDADL/DR*DP1(J)*1.D9
      DGY(J)=      DRDADL*DBLE(MI)/(DR*DST)*DP0(J)*1.D9
      DGZ(J)=      DRDADL*DBLE(LI)/DR*DP0(J)*1.D9
   10 CONTINUE
      DO 20 I=1,25
   20 DPK(I)=0.D0
!#######################################################################
!     Compute geodetic coefficients for tidal acceleration vector
!     orientated to ellipsoidal coordinate system stored in
!     DGX (north), DGY (east) and DGZ (upwards), all in nm/s**2.
!#######################################################################
      DCDLAT=DCLAT*DCPSI+DSLAT*DSPSI
      DSDLAT=DSLAT*DCPSI-DCLAT*DSPSI
      DO 50 I=1,25
      DUMMY =DCDLAT*DGX(I)-DSDLAT*DGZ(I)
      DGZ(I)=(DSDLAT*DGX(I)+DCDLAT*DGZ(I))
      DGX(I)=DUMMY
   50 CONTINUE
      IC2=IC+2
      DCAZ=COS(DAZ*DRAD)
      DSAZ=SIN(DAZ*DRAD)
      GOTO(100,200,300,400,500,600,700,800,900,1000,1100),IC2
!#######################################################################
!     IC=-1, compute geodetic coefficients for tidal potential.
!#######################################################################
  100 CONTINUE
      GOTO 2000
  200 CONTINUE
!#######################################################################
!     IC=0, compute geodetic coefficients for vertical component
!           (gravity tide).
!#######################################################################
      DO 210 I=1,25
      DGK(I)=DGZ(I)
  210 DPK(I)=180.0D0
      GOTO 2000
!#######################################################################
!     IC=1, compute geodetic coefficients for horizontal component
!           (tidal tilt) in azimuth DAZ, in mas.
!#######################################################################
  300 CONTINUE
      DO 310 I=1,12
      DGK(I)=SQRT((DGX(I)*DCAZ)**2+(DGY(I)*DSAZ)**2)*DF
      DPK(I)=0.D0
      IF(DGX(I)*DCAZ.EQ.0.D0.AND.DGY(I)*DSAZ.EQ.0.D0) GOTO 310
      DPK(I)=DRO*ATAN2(DGY(I)*DSAZ,DGX(I)*DCAZ)
  310 CONTINUE
      GOTO 2000
!#######################################################################
!     IC=2, compute geodetic coefficients for vertical displacement
!           in mm.
!     Attention: this component has never been tested
!#######################################################################
  400 CONTINUE
!GCR original:     DFAK=1.D3/DGRAV
	  DFAK=1.D3/DGRAV
      DO 410 I=1,12
      DGK(I)=DGK(I)*DHLAT(I)*DFAK
  410 DPK(I)=0.0D0
      WRITE(IUN16,*) '*****The component',IC,' has never been tested !'
      GOTO 2000
!#######################################################################
!     IC=3, compute geodetic coefficients for horizontal displacement
!           in azimuth DAZ in mm.
!     Attention: this component has never been tested
!#######################################################################
  500 CONTINUE
!GCR original code: DFAK=1.D3*DR/DGRAV
!GCR different solutions:
!GCR	  DFAK=1.D8/(DR*DGRAV)
	  DFAK=1.D-6*DR/DGRAV
      DO 510 I=1,12
      DGK(I)=SQRT((DGX(I)*DCAZ)**2+(DGY(I)*DSAZ)**2)*DLLAT(I)*DFAK
      DPK(I)=0.D0
      IF(DGX(I)*DCAZ.EQ.0.D0.AND.DGY(I)*DSAZ.EQ.0.D0) GOTO 510
      DPK(I)=DRO*ATAN2(DGY(I)*DSAZ,DGX(I)*DCAZ)
  510 CONTINUE
      WRITE(IUN16,*) '*****The component',IC,' has never been tested !'
      GOTO 2000
!#######################################################################
!     IC=4, compute geodetic coefficients for vertical strain at the
!           Earth's deformed surface in 10**-9 units = nstr.
!           We use a spherical approximation for the vertical strain,
!           i.e. eps(rr) , and a POISSON ratio of 0.25 (see ZUERN and
!           WILHELM 1984, p. 282).
!#######################################################################
  600 CONTINUE
      DPOISS=0.25D0
      DFAK=1.D9/(DGRAV*DR)*DPOISS/(DPOISS-1.D0)
      DO 610 I=1,3
  610 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))
      DO 620 I=4,7
  620 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))
      DO 630 I=8,12
  630 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))
      DO 640 I=1,12
  640 DPK(I)=0.0D0
      GOTO 2000
!#######################################################################
!     IC=5, compute geodetic coefficients for horizontal strain
!           in azimuth DAZ, in 10**-9 units.
!#######################################################################
  700 CONTINUE
      DTHETA=(90.D0-DPSI)*DRAD
      DAZR=(DAZ+180.D0)*DRAD
      DCAZ=COS(DAZR)
      DSAZ=SIN(DAZR)
      DSAZ2=SIN(2.D0*DAZR)
      DCSTS=-0.5D0*SIN(2.D0*DAZR)
      DCT=DSPSI
      DST=DCPSI
      DCT2=DCT*DCT
      DST2=DST*DST
      DCC2=COS(2.D0*DPSI*DRAD)
      DC2T=-DCC2
      DCOTT=1.D0/TAN(DTHETA)
      DCOTT2=1.D0/TAN(2.D0*DTHETA)
      DFAK=1.D9/(DR*DGRAV)
!#######################################################################
!     Real part is stored in DGX, imaginary part is stored in DGY.
!     Formulas were given by Dr. W. Zuern, BFO Schiltach (personal
!     communication) and tested against horizontal strain computed
!     (with lower precision) by program ETIDEL (made by Bilham).
!     Results agreed to 0.3 % and 0.1 degree for most of the waves,
!     except for 2N2 and L2 (deviation of 3 %).
!#######################################################################
      DGX(1)=(DHLAT(1)-(6.D0*DLLAT(1)*DC2T)/(3.D0*DCT2-1.D0))*DCAZ**2 &
            +(DHLAT(1)-(6.D0*DLLAT(1)*DCT2)/(3.D0*DCT2-1.D0))*DSAZ**2
      DGY(1)=0.D0
      DGX(2)=(DHLAT(2)-4.D0*DLLAT(2))*DCAZ**2+(DHLAT(2)-DLLAT(2)/DST2 &
       +2.D0*DLLAT(2)*DCOTT*DCOTT2)*DSAZ**2
      DGY(2)=2.D0*DLLAT(2)*(2.D0*DCOTT2-DCOTT)*DCSTS/DST
      DGX(3)=(DHLAT(3)+2.D0*DLLAT(3)*(DCOTT*DCOTT-1.D0))*DCAZ**2 &
       +(DHLAT(3)-4.D0*DLLAT(3)/DST2+2.D0*DLLAT(3)*DCOTT*DCOTT)*DSAZ**2
      DGY(3)=4.D0*DLLAT(3)*DCOTT*DCSTS/DST
      DGX(4)=(DHLAT(4)+DLLAT(4)*(33.D0-45.D0*DCT2)/(5.D0*DCT2-3.D0))* &
       DCAZ**2+(DHLAT(4)-DLLAT(4)*(1.D0+10.D0*DCT2/(5.D0*DCT2-3.D0)))* &
       DSAZ**2
      DGY(4)=0.D0
      DGX(5)=(DHLAT(5)-DLLAT(5)*(1.D0+10.D0*(1.D0-4.D0*DCT2)/ &
       (1.D0-5.D0*DCT2)))*DCAZ**2+(DHLAT(5)+DLLAT(5)* &
       (DCOTT*DCOTT-1.D0/DST2-10.D0*DCT2/(5.D0*DCT2-1.D0)))*DSAZ**2
      DGY(5)=-20.D0*DLLAT(5)*DCT*DCSTS/(5.D0*DCT2-1.D0)
      DGX(6)=(DHLAT(6)+DLLAT(6)*(2.D0*DCOTT*DCOTT-7.D0))*DCAZ**2 &
       +(DHLAT(6)+DLLAT(6)*(2.D0*DCOTT*DCOTT-1.D0-4.D0/DST2))*DSAZ**2
      DGY(6)=-4.D0*DLLAT(6)*(DCOTT-1.D0/DCOTT)*DCSTS/DST
      DGX(7)=(DHLAT(7)+DLLAT(7)*(6.D0*DCOTT*DCOTT-3.D0))*DCAZ**2 &
       +(DHLAT(7)+DLLAT(7)*(3.D0*DCOTT*DCOTT-9.D0/DST2))*DSAZ**2
      DGY(7)=12.D0*DLLAT(7)*DCOTT*DCSTS/DST
      DGX(8)=(DHLAT(8)-4.D0*DLLAT(8)*(4.D0-3.D0*(5.D0*DCT2-1.D0)/ &
       (35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)))*DCAZ**2+ &
       (DHLAT(8)-4.D0*DLLAT(8)*(1.D0+3.D0*(5.D0*DCT2-1.D0)/ &
       (35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)))*DSAZ**2
      DGY(8)=0.D0
      DGX(9)=  (DHLAT(9)-2.D0*DLLAT(9)*(8.D0-3.D0/(7.D0*DCT2-3.D0)))* &
       DCAZ**2+(DHLAT(9)-2.D0*DLLAT(9)*(2.D0+3.D0/(7.D0*DCT2-3.D0)))* &
       DSAZ**2
      DGY(9)=DLLAT(9)*3.D0/DCT*(1.D0+2.D0/(7.D0*DCT2-3.D0))*DSAZ2
      DGX(10)=(DHLAT(10)-4.D0*DLLAT(10)*(4.D0+3.D0*DCT2/ &
       (7.D0*DCT2**2-8.D0*DCT2+1.D0)))*DCAZ**2 &
             +(DHLAT(10)-4.D0*DLLAT(10)*(1.D0-3.D0*DCT2/ &
       (7.D0*DCT2**2-8.D0*DCT2+1.D0)))*DSAZ**2
      DGY(10)=-DLLAT(10)*6.D0*DCT/DST**2*(1.D0-4.D0/(7.D0*DCT2-1.D0))* &
       DSAZ2
      DGX(11)=(DHLAT(11)-2.D0*DLLAT(11)*(8.D0-3.D0/DST2))*DCAZ**2 &
             +(DHLAT(11)-2.D0*DLLAT(11)*(2.D0+3.D0/DST2))*DSAZ**2
      DGY(11)= DLLAT(11)*3.D0/DCT*(3.D0-2.D0/DST2)*DSAZ2
      DGX(12)=(DHLAT(12)-4.D0*DLLAT(12)*(4.D0-3.D0/DST2))*DCAZ**2 &
             +(DHLAT(12)-4.D0*DLLAT(12)*(1.D0+3.D0/DST2))*DSAZ**2
      DGY(12)= DLLAT(12)*12.D0*DCT/DST2*DSAZ2
      DO 710 I=1,12
      DGK(I)=DGK(I)*SQRT(DGX(I)**2+DGY(I)**2)*DFAK
  710 DPK(I)=DPK(I)+ATAN2(DGY(I),DGX(I))*DRO
      GOTO 2000
!#######################################################################
!     IC=6, compute geodetic coefficients for areal strain
!           in 10**-9 units = nstr.
!           We use a spherical approximation for the aereal strain,
!           i.e. eps(t,t) + eps(l,l), (see ZUERN and WILHELM 1984,
!           p. 282).
!#######################################################################
      DFAK=1.D9/(DGRAV*DR)
  800 CONTINUE
      DO 810 I=1,3
  810 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))*DFAK
      DO 820 I=4,7
  820 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))*DFAK
      DO 830 I=8,12
  830 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))*DFAK
      DO 840 I=1,12
  840 DPK(I)=0.0D0
      GOTO 2000
!#######################################################################
!     IC=7, compute geodetic coefficients for shear tidal strain
!           at the Earth's deformed surface in 10**-9 units = nstr.
!           We use a spherical approximation, i.e. eps(t,l)
!     Attention: this component has never been tested
!#######################################################################
  900 CONTINUE
      DTHETA=(90.D0-DPSI)*DRAD
      DAZR=(DAZ+180.D0)*DRAD
      DCAZ =COS(DAZR)
      DSAZ =SIN(DAZR)
      DSAZ2=SIN(2.D0*DAZR)
      DCSTS=-0.5D0*SIN(2.D0*DAZR)
      DCT=DSPSI
      DST=DCPSI
      DCT2=DCT*DCT
      DST2=DST*DST
      DCC2=COS(2.D0*DPSI*DRAD)
      DC2T=-DCC2
      DCOTT =1.D0/TAN(DTHETA)
      DCOTT2=1.D0/TAN(2.D0*DTHETA)
      DFAK=1.D9/(DR*DGRAV)
      DGY(1)=0.D0
      DGY(2)=2.D0*DLLAT(2)*(2.D0*DCOTT2-DCOTT)*DCSTS/DST
      DGY(3)=4.D0*DLLAT(3)*DCOTT*DCSTS/DST
      DGY(4)=0.D0
      DGY(5)=-20.D0*DLLAT(5)*DCT*DCSTS/(5.D0*DCT2-1.D0)
      DGY(6)=-4.D0*DLLAT(6)*(DCOTT-1.D0/DCOTT)*DCSTS/DST
      DGY(7)=12.D0*DLLAT(7)*DCOTT*DCSTS/DST
      DGY(8)=0.D0
      DGY(9)=DLLAT(9)*3.D0/DCT*(1.D0+2.D0/(7.D0*DCT2-3.D0))*DSAZ2
      DGY(10)=-DLLAT(10)*6.D0*DCT/DST**2*(1.D0-4.D0/(7.D0*DCT2-1.D0))*DSAZ2
      DGY(11)=DLLAT(11)*3.D0/DCT*(3.D0-2.D0/DST2)*DSAZ2
      DGY(12)=DLLAT(12)*12.D0*DCT/DST2*DSAZ2
      DO 910 I=1,12
      DGK(I)=DGK(I)*DGY(I)*DFAK
  910 DPK(I)=0.D0
      WRITE(IUN16,*) ' ***** The shear strain has never been tested !'
      GOTO 2000
!#######################################################################
!     IC=8, compute geodetic coefficients for volume strain
!           at the Earth's deformed surface in 10**-9 units = nstr.
!           We use a spherical approximation, i.e. eps(t,t)+eps(l,l).
!#######################################################################
 1000 CONTINUE
      DPOISS=0.25D0
      DFAK=1.D9*(1.D0-2.D0*DPOISS)/(1.D0-DPOISS)
      DO 1010 I=1,3
 1010 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR)
      DO 1020 I=4,7
 1020 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR)
      DO 1030 I=8,12
 1030 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR)
      DO 1040 I=1,12
 1040 DPK(I)=0.0D0
      GOTO 2000
!#######################################################################
!     IC=9, compute geodetic coefficients for ocean tides in mm:
!#######################################################################
 1100 CONTINUE
      DFAK=1.D3/DGRAV
      DO 1110 I=1,25
      DGK(I)=DGK(I)*DFAK
 1110 DPK(I)=0.0D0
 2000 CONTINUE
!#######################################################################
!     Print geodetic coefficients:
!#######################################################################
      IF(IPRINT.EQ.0) RETURN
      WRITE(IUN16,17003) IC,DAZ,(DGK(I),CUNIT(IC2),DPK(I),I=1,12)
      WRITE(IUN16,17004)        (DGK(I),CUNIT(IC2),DPK(I),I=13,25)
 5000 CONTINUE
      IF(IPRINT.GT.0) WRITE(IUN16,17005)
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17000 FORMAT('      Routine ETGCON, version 1997.03.03.'// &
      '      Computation of geodetic coefficients'// &
      '      Parameters of Geodetic Reference System 1980:'/ &
      '      Major semi axis                  ',F12.0,'  m'/ &
      '      1. excentricity                  ',F12.8/)
17001 FORMAT('      Station parameters:'// &
      '      Latitude                       ',F12.6,' deg'/ &
      '      Geocentric latitude            ',F12.6,' deg'/ &
      '      Longitude                      ',F12.6,' deg'/ &
      '      Height                         ',F12.3,' m'/ &
      '      Gravity                        ',F12.6,' m/s**2'/ &
      '      Geocentric radius              ',F12.3,' m'/ &
      '      Component of observations      ',I12/ &
      '      Azimuth from north direction   ',F12.6,' deg'//)
17003 FORMAT(/'      Geodetic coefficients and phases for component',I4/ &
      '      azimuth:',F12.6,' degree'// &
      '      GC 2,0',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 2,1',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 2,2',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 3,0',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 3,1',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 3,2',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 3,3',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 4,0',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 4,1',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 4,2',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 4,3',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 4,4',F14.8,2X,A8,2X,F14.6,' deg')
17004 FORMAT( &
      '      GC 5,0',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 5,1',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 5,2',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 5,3',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 5,4',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 5,5',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,0',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,1',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,2',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,3',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,4',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,5',F14.8,2X,A8,2X,F14.6,' deg'/ &
      '      GC 6,6',F14.8,2X,A8,2X,F14.6,' deg'/)
17005 FORMAT(/6x,'***** Routine ETGCON finished the execution.'/)
END


SUBROUTINE ETGREN(IUN16,DJULD,ITY,ITM,ITD,DTH,NERR)
!#######################################################################
!     Routine ETGREN, version 1996.05.25 Fortran 90.
!     The routine ETGREN computes Gregorian date from given Julian
!     date. ETGREN is a modified version of routine CALDAT given in
!     Fortran by
!     Press, W.H., S.A. Teukolsky, W.T. Vetterling and B.P. Flannery
!     (1992):  Numerical recipes in FORTRAN: the art of scientific
!              computing. Second edition, Cambridge University Press
!              1992.
!     The routine has been tested and found to be correct between
!     years -3000 and +3000.
!     Input parameter description:
!     ----------------------------
!     DJULD:       Julian date (DOUBLE PRECISION).
!     Output parameter description:
!     -----------------------------
!     ITY:         year   (INTEGER).
!     ITM:         month  (INTEGER).
!     ITD:         day    (INTEGER).
!     DTH:         hour   (DOUBLE PRECISION).
!     NERR:        error code, counts the number of errors which
!                  happened during the execution of routine ETGREN.
!     Execution time:
!     ---------------
!     2.47 microsec per call on a Pentium 100 MHz using Lahey LF90
!     compiler.
!     Routine creation:  1995.11.04 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082301.
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel,
!#######################################################################
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      SAVE
      DATA DGREG/2299160.499999D0/
      NERR=0
      IF(DJULD.LT.625307.D0.OR.DJULD.GT.2817153.D0) THEN
        NERR=1
        WRITE(IUN16,17050)  DJULD
      ENDIF
      JULIAN=INT(DJULD)
      DTH=12.D0
      DFRAC=DJULD-DBLE(JULIAN)
      DTH=DTH+DFRAC*24.D0
      IF(DTH.GE.23.9999D0) THEN
         DTH=DTH-24.D0
         JULIAN=JULIAN+1
      ENDIF
      IF(DJULD.GT.DGREG)  THEN
!#######################################################################
!     Cross over from Gregorian calendar procudes this correction:
!#######################################################################
         JALPHA=INT(((JULIAN-1867216)-0.25D0)/36524.25D0)
         JA=JULIAN+1+JALPHA-INT(0.25D0*DBLE(JALPHA))
      ELSE
         JA=JULIAN
      ENDIF
      JB=JA+1524
      JC=INT(6680.D0+((JB-2439870)-122.1D0)/365.25D0)
      JD=365*JC+INT(0.25D0*DBLE(JC))
      JE=INT((JB-JD)/30.6001D0)
      ITD=JB-JD-INT(30.6001D0*DBLE(JE))
      ITM=JE-1
      IF(ITM.GT.12) ITM=ITM-12
      ITY=JC-4715
      IF(ITM.GT.2) ITY=ITY-1
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17050 FORMAT(/' *****Error in routine ETGREN.FOR, version 1996.05.25.'/ &
      ' *****Julian date is:',F20.6/ &
      ' *****Year is less -3000 or greater +3000.'/ &
      ' *****Routine ETGREN has not been tested for this case.'/ &
      ' *****Routine ETGREN continues the execution.'/)
END


SUBROUTINE ETJULN(IUN16,ITY,ITM,ITD,DTH,DJULD)
!#######################################################################
!     Routine ETJULN, version 1996.05.25 Fortran 90.
!     The routine ETJULN computes the Julian date and the modified
!     Julian date. ETJULN is a modified version of routine MJD given
!     in PASCAL by Montenbruck and Pfleger (see below).
!     The routine is valid for every date since year -4713.
!     Comparison with reference values between years -1410 and +3200
!     from JPL was successfully.
!     Reference: Montenbruck, O. and T. Pfleger (1989): Astronomie mit
!                dem Personal Computer. Springer Verlag, Berlin 1989.
!     Input parameter description:
!     ----------------------------
!     ITY:         Year   (INTEGER).
!     ITM:         Month  (INTEGER).
!     ITD:         Day    (INTEGER).
!     DTH:         Hour   (DOUBLE PRECISION).
!     Output parameter description:
!     -----------------------------
!     DJULD:       Julian date (DOUBLE PRECISION).
!                  16. April   -1410, 0.00 H is DJULD = 1206160.5D0
!                  31. January -1100, 0.00 H is DJULD = 1319312.5D0
!                  24. January -0800, 0.00 H is DJULD = 1428880.5D0
!                  17. January -0500, 0.00 H is DJULD = 1538448.5D0
!                  10. January -0200, 0.00 H is DJULD = 1648016.5D0
!                  03. January   100, 0.00 H is DJULD = 1757584.5D0
!                  29. February  400, 0.00 H is DJULD = 1867216.5D0
!                  20. December  699, 0.00 H is DJULD = 1976720.5D0
!                  15. February 1000, 0.00 H is DJULD = 2086352.5D0
!                  08. February 1300, 0.00 H is DJULD = 2195920.5D0
!                  11. February 1600, 0.00 H is DJULD = 2305488.5D0
!                  06. February 1900, 0.00 H is DJULD = 2415056.5D0
!                  01. January  1988, 0.00 H is DJULD = 2447161.5D0
!                  01. February 1988, 0.00 H is DJULD = 2447192.5D0
!                  29. February 1988, 0.00 H is DJULD = 2447220.5D0
!                  01. March    1988, 0.00 H is DJULD = 2447221.5D0
!                  01. February 2200, 0.00 H is DJULD = 2524624.5D0
!                  27. January  2500, 0.00 H is DJULD = 2634192.5D0
!                  23. January  2800, 0.00 H is DJULD = 2743760.5D0
!                  22. December 3002, 0.00 H is DJULD = 2817872.5D0
!     To obtain the modified Julian date, subtract 2400000.5 from
!     DJULD.
!     Execution time:
!     ---------------
!     2.42 microsec per call on a Pentium 100 MHz using Lahex LF90
!     compiler.
!     Routine creation:  1992.09.19 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082301.
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel.
!#######################################################################
      IMPLICIT REAL(8) (D)
      SAVE
      ITYY=ITY
      IF(ITM.LT.1) GOTO 5000
      IF(ITM.GT.12) GOTO 5010
      ITMM=ITM
      DA=10000.0D0*ITYY+100.0D0*ITMM+ITD
      IF(ITMM.LE.2) THEN
         ITMM=ITMM+12
         ITYY=ITYY-1
      ENDIF
      IF(DA.LE.15821004.1D0) THEN
         DB=-2+(ITYY+4716)/4-1179
      ELSE
         DB=ITYY/400-ITYY/100+ITYY/4
      ENDIF
      DA=365.0D0*DBLE(ITYY)-679004.0D0
      DJULD=DA+DB+INT(30.6001D0*(ITMM+1))+DBLE(ITD)+DTH/24.D0+2400000.5D0
      RETURN
 5000 WRITE(IUN16,17050) ITY,ITM,ITD,DTH
      STOP
 5010 WRITE(IUN16,17051) ITY,ITM,ITD,DTH
      STOP
!#######################################################################
!     Format statements:
!#######################################################################
17050 FORMAT(/' *****Error in routine ETJULN, version 1996.05.25.'/ &
      ' *****Month is less 1:',2X,3I4,F12.3/ &
      ' *****Routine ETJULN stops the execution.'/)
17051 FORMAT(/' *****Error in routine ETJULN, version 1996.05.25.'/ &
      ' *****Month is greater 12:',2X,3I4,F12.3/ &
      ' *****Routine ETJULN stops the execution.'/)
END


SUBROUTINE ETLEGN(DCT,DST,LMAX,DP0,DP1)
!#######################################################################
!     Routine ETLEGN, version 1996.05.25 Fortran 90.
!     The routine computes the fully normalized Legendre functions
!     and their derivatives complete to degree and order 6 by explicit
!     formulas.
!     Input parameter description:
!     ----------------------------
!     DCT:         DOUBLE PRECISION COS of polar distance theta, for
!                  which the fully normalized associated Legendre
!                  functions will be computed.
!     DST:         DOUBLE PRECISION SIN of polar distance theta, for
!                  which the fully normalized associated Legendre
!                  functions will be computed.
!     Output parameter desription:
!     -----------------------------
!     LMAX:        Maximum degree and order, for which the fully
!                  normalized associated Legendre functions will be
!                  computed. LMAX is equal to 6.
!     DP0:         DOUBLE PRECISION array of fully normalized Legendre
!                  functions. The fully normalized Legendre function
!                  of degree L and order M is stored in
!                  DP0(J) WITH J=L*(L+1)/2+M+1.
!     DP1:         DOUBLE PRECISION array of first derivatives of the
!                  fully normalized Legendre functions to polar
!                  distance theta. The first derivative of fully
!                  normalized Legendre function of degree L and order
!                  M is stored in DP1(J) WITH J=L*(L+1)/2+M-2.
!     Example for theta = 30 degree:
!      J    L    M    DP0(L+1,M+1)        DP1(L+1,M*1)
!      1    2    0    1.39754248593737    2.90473750965556
!      2    2    1   -1.67705098312484    1.93649167310371
!      3    2    2    0.48412291827593   -1.67705098312484
!      4    3    0    0.85923294280422    5.45686207907072
!      5    3    1   -2.22775461507770    0.35078038001005
!      6    3    2    1.10926495933118   -3.20217211436237
!      7    3    3   -0.26145625829190    1.35856656995526
!      8    4    0    0.07031250000000    7.30708934443120
!      9    4    1   -2.31070453947492   -3.55756236768943
!     10    4    2    1.78186666957014   -3.63092188706945
!     11    4    3   -0.67928328497763    3.13747509950278
!     12    4    4    0.13865811991640   -0.96065163430871
!     13    5    0   -0.74051002865529    7.19033890096581
!     14    5    1   -1.85653752113519   -8.95158333012718
!     15    5    2    2.29938478949397   -1.85857059805883
!     16    5    3   -1.24653144252643    4.78747153809058
!     17    5    4    0.39826512815546   -2.52932326844337
!     18    5    5   -0.07271293151948    0.62971245879506
!     19    6    0   -1.34856068213155    4.35442243247701
!     20    6    1   -0.95021287641141  -14.00557979016896
!     21    6    2    2.47470311782905    2.56294916449777
!     22    6    3   -1.85592870532597    5.20453026842398
!     23    6    4    0.81047568870385   -4.55019988574613
!     24    6    5   -0.22704605589841    1.83519142087945
!     25    6    6    0.03784100931640   -0.39325530447417
!     Execution time:
!     ---------------
!     0.00006 sec per call of ETLEGN on 80486 DX4 100MHZ with NDEG=6.
!     Program creation:  1995.03.23 by Hans-Georg Wenzel,
!                        Geodaetisches Institut,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082301.
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel.
!#######################################################################
      IMPLICIT REAL(8) (D)
      REAL(8) DP0(25),DP1(25)
      SAVE
      LMAX=6
      DST2=DST*DST
      DCT2=DCT*DCT
      DST3=DST2*DST
      DCT3=DCT2*DCT
      DST4=DST3*DST
      DCT4=DCT3*DCT
      DST5=DST4*DST
      DCT5=DCT4*DCT
      DST6=DST5*DST
      DCT6=DCT5*DCT
!#######################################################################
!     Compute fully normalized Legendre functions:
!     Degree 2:
!#######################################################################
      DP0(01)= SQRT(5.D0/4.D0)*(3.D0*DCT2-1.D0)
      DP0(02)= SQRT(15.D0)*DCT*DST
      DP0(03)= SQRT(15.D0/4.D0)*DST2
!#######################################################################
!     Degree 3:
!#######################################################################
      DP0(04)= SQRT(7.D0/4.D0)*DCT*(5.D0*DCT2-3.D0)
      DP0(05)= SQRT(21.D0/8.D0)*DST*(5.D0*DCT2-1.D0)
      DP0(06)= SQRT(105.D0/4.D0)*DST2*DCT
      DP0(07)= SQRT(35.D0/8.D0)*DST3
!#######################################################################
!     Degree 4:
!#######################################################################
      DP0(08)= 3.D0/8.D0*(3.D0-30.D0*DCT2+35.D0*DCT4)
      DP0(09)= SQRT(45.D0/8.D0)*DST*DCT*(7.D0*DCT2-3.D0)
      DP0(10)= SQRT(45.D0/16.D0)*(-1.D0+8.D0*DCT2-7.D0*DCT4)
      DP0(11)= SQRT(315.D0/8.D0)*DST3*DCT
      DP0(12)= SQRT(315.D0/64.D0)*DST4
!#######################################################################
!     Degree 5:
!#######################################################################
      DP0(13)= SQRT(11.D0/64.D0)*DCT*(15.D0-70.D0*DCT2+63.D0*DCT4)
      DP0(14)= SQRT(165.D0/64.D0)*DST*(1.D0-14.D0*DCT2+21.D0*DCT4)
      DP0(15)= SQRT(1155.D0/16.D0)*DCT*(-1.D0+4.D0*DCT2-3.D0*DCT4)
      DP0(16)= SQRT(385.D0/128.D0)*DST3*(9.D0*DCT2-1.D0)
      DP0(17)= SQRT(3465.D0/64.D0)*DCT*DST4
      DP0(18)= SQRT(693.D0/128.D0)*DST5
!#######################################################################
!     Degree 6:
!#######################################################################
      DP0(19)= SQRT(13.D0/256.D0)*(-5.D0+105.D0*DCT2-315.D0*DCT4+231.D0*DCT6)
      DP0(20)= SQRT(273.D0/64.D0)*DST*DCT*(5.D0-30.D0*DCT2+33.D0*DCT4)
      DP0(21)= SQRT(2730.D0/1024.D0)*(1.D0-19.D0*DCT2+51.D0*DCT4-33.D0*DCT6)
      DP0(22)= SQRT(2730.D0/256.D0)*DST3*DCT*(-3.D0+11.D0*DCT2)
      DP0(23)= SQRT(819.D0/256.D0)*(-1.D0+13.D0*DCT2-23.D0*DCT4+11.D0*DCT6)
      DP0(24)= SQRT(18018.D0/256.D0)*DST5*DCT
      DP0(25)= SQRT(6006.D0/1024.D0)*DST6
!#######################################################################
!     Compute derivations with respect to theta:
!     Degree 2:
!#######################################################################
      DP1(01)=-SQRT(45.D0)*DST*DCT
      DP1(02)= SQRT(15.D0)*(1.D0-2.D0*DST2)
      DP1(03)= SQRT(15.D0)*DST*DCT
!#######################################################################
!     Degree 3:
!#######################################################################
      DP1(04)=-SQRT(63.D0/4.D0)*DST*(5.D0*DCT2-1.D0)
      DP1(05)= SQRT(21.D0/8.D0)*DCT*(4.D0-15.D0*DST2)
      DP1(06)=-SQRT(105.D0/4.D0)*DST*(1.D0-3.D0*DCT2)
      DP1(07)= SQRT(315.D0/8.D0)*DST2*DCT
!#######################################################################
!     Degree 4:
!#######################################################################
      DP1(08)=-15.D0/2.D0*(7.D0*DCT2-3.D0)*DST*DCT
      DP1(09)= SQRT(45.D0/8.D0)*(3.D0-27.D0*DCT2+28.D0*DCT4)
      DP1(10)=-SQRT(45.D0)*(4.D0-7.D0*DCT2)*DST*DCT
      DP1(11)= SQRT(315.D0/8.D0)*DST2*(4.D0*DCT2-1.D0)
      DP1(12)= SQRT(315.D0/4.D0)*DST3*DCT
!#######################################################################
!     Degree 5:
!#######################################################################
      DP1(13)=-SQRT(2475.D0/64.D0)*DST*(1.D0-14.D0*DCT2+21.D0*DCT4)
      DP1(14)= SQRT(165.D0/64.D0)*DCT*(29.D0-126.D0*DCT2+105.D0*DCT4)
      DP1(15)=-SQRT(1155.D0/16.D0)*DST*(-1.D0+12.D0*DCT2-15.D0*DCT4)
      DP1(16)= SQRT(3465.D0/128.D0)*DST2*DCT*(15.D0*DCT2-7.D0)
      DP1(17)=-SQRT(3465.D0/64.D0)*DST*(1.D0-6.D0*DCT2+5.D0*DCT4)
      DP1(18)= SQRT(17325.D0/128.D0)*DCT*DST4
!#######################################################################
!     Degree 6:
!#######################################################################
      DP1(19)=-SQRT(5733.D0/64.D0)*DST*DCT*(5.D0-30.D0*DCT2+33.D0*DCT4)
      DP1(20)=-SQRT(273.D0/64.D0)*(5.D0-100.D0*DCT2+285.D0*DCT4-198.D0*DCT6)
      DP1(21)=-SQRT(1365.D0/128.D0)*DST*DCT*(-19.D0+102.D0*DCT2-99.D0*DCT4)
      DP1(22)= SQRT(12285.D0/128.D0)*DST2*(1.D0-15.D0*DCT2+22.D0*DCT4)
      DP1(23)=-SQRT(819.D0/64.D0)*DCT*DST*(13.D0-46.D0*DCT2+33.D0*DCT4)
      DP1(24)= SQRT(9009.D0/128.D0)*DST4*(6.D0*DCT2-1.D0)
      DP1(25)= SQRT(27027.D0/128.D0)*DST5*DCT
      RETURN
END


SUBROUTINE ETLOVE(IUN16,IPRINT,DLAT,DELV)
!#######################################################################
!     Routine ETLOVE, version 1996.05.25 Fortran 90.
!     The routine computes latitude dependent LOVE-numbers DH, DK,
!     SHIDA-numbers DL, gravimeter factors DG and tilt factors DT
!     using the so-called Wahr-Dehant-Zschau model.
!     Body tide amplitude factors for Wahr-Dehant-Zschau model.
!     The NDFW resonance is approximated by
!     G(RES) = GLAT - GR*(DOM - DOM0)/(DOMR - DOM).
!     similar equations hold for the other parameters.
!     Gravimetric amplitude factors, LOVE numbers h and k for degree
!     0...3 have been taken from Dehant (1987), Table 7, 8 and 9
!     for an elliptical, uniformly rotating, oceanless Earth with
!     liquid outer core and inelastic mantle (PREM Earth model with
!     inelastic mantle from Zschau) and for the fourth degree from
!     Dehant et. al (1989), Table 6. The resonance factors GR have
!     been computed to fit the difference between body tide amplitude
!     factors at O1 and PSI1 from Dehant (1987), PREM model with
!     elastic mantle (Table 1...3). The NDFW resonance frequency is
!     15.073729 degree per hour = 1.004915267 CPD UT, taken from
!     Wahr (1981) (because it is not given in Dehant's papers).
!     Input parameter description:
!     ----------------------------
!     IUN16:       formatted line printer unit.
!     IPRINT:      printout parameter. For IPRINT=1, the computed
!                  Love- and Shida- number s will be printed.
!     DLAT:        ellipsoidal latitude in degree.
!     DELV:        ellipsoidal height in meter.
!     Description of COMMON /LOVE/:
!     -----------------------------
!     DOM0:        frequency of O1 in degree per hour.
!     DOMR:        frequency of the FCN eigenfrequency in degree per
!                  hour.
!     DGLAT:       array(1..12) containing the gravimetric factors at
!                  latitude DLAT.
!     DGR:         resonance factor for gravimetric factors.
!     DHLAT:       array(1..12) containing the Love-numbers h at
!                  latitude DLAT.
!     DHR:         resonance factor for the Love-number h(2,1).
!     DKLAT:       array(1..12) containing the Love-numbers k at
!                  latitude DLAT.
!     DKR:         resonance factor for the Love-number k(2,1).
!     DLLAT:       array(1..12) containing the Shida-numbers l at
!                  latitude DLAT.
!     DLR:         resonance factor for the Shida-number l(2,1).
!     DTLAT:       array(1..12) containing the tilt factors at
!                  latitude DLAT.
!     Reference:
!     ----------
!     Dehant, V. (1987): Tidal parameters for an inelastic Earth.
!        Physics of the Earth and Planetary Interiors, 49, 97-116,
!        1987.
!     Wahr, J.M. (1981): Body tides on an elliptical, rotating,
!        elastic and oceanless earth. Geophysical Journal of the Royal
!        Astronomical Society, vol. 64, 677-703, 1981.
!     Zschau, J. and R. Wang (1987): Imperfect elasticity in the
!        Earth's mantle. Implications for earth tides and long period
!        deformations. Proceedings of the 9th International Symposium
!        on Earth Tides, New York 1987, pp. 605-629, editor J.T. Kuo,
!        Schweizerbartsche Verlagsbuchhandlung, Stuttgart 1987.
!     Routine creation:  1993.07.03 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel: 0049-721-6082307,
!                        FAX: 0049-721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel.
!#######################################################################
      use LOVE
      use PARAMS
      IMPLICIT REAL(8) (D)
!#######################################################################
!     The following DIMENSION statement is concerning the elastic
!     Earth model for the different degree and order constituents.
!#######################################################################
      REAL(8) DG0(12),DGP(12),DGM(12)
      REAL(8) DH0(12),DHP(12),DHM(12)
      REAL(8) DK0(12),DKP(12),DKM(12)
      REAL(8) DL0(12),DLP(12),DLM(12)
      REAL(8) DLATP(12),DLATM(12)
      SAVE
!#######################################################################
!     The following DATA statements are concerning the elastic
!     Earth model for the different degree and order constituents.
!     The latitude dependency is not given for all constituents in
!     the Wahr-Dehant-Zschau model
!#######################################################################
      DATA DG0/1.1576D0,1.1542D0,1.1600D0,1.0728D0,1.0728D0,1.0728D0, &
       1.0728D0,1.0363D0,1.0363D0,1.0363D0,1.0363D0,1.0363D0/
      DATA DGP/-0.0016D0,-0.0018D0,-0.0010D0,0.D0,0.D0,0.D0,-0.0010D0, &
       0.D0,0.D0,0.D0,0.D0,-0.000315D0/
      DATA DGM/0.0054D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0, &
       0.D0,0.D0/
      DATA DH0/0.6165D0,0.6069D0,0.6133D0,0.2946D0,0.2946D0,0.2946D0, &
       0.2946D0,0.1807D0,0.1807D0,0.1807D0,0.1807D0,0.1807D0/
      DATA DHP/0.0007D0,0.0007D0,0.0005D0,0.D0,0.D0,0.D0,0.0003D0, &
       0.D0,0.D0,0.D0,0.D0,0.00015D0/
      DATA DHM/0.0018D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0, &
       0.D0,0.D0/
      DATA DK0/0.3068D0,0.3009D0,0.3034D0,0.0942D0,0.0942D0,0.0942D0, &
       0.0942D0,0.0427D0,0.0427D0,0.0427D0,0.0427D0,0.0427D0/
      DATA DKP/0.0015D0,0.0014D0,0.0009D0,0.D0,0.D0,0.D0,0.0007D0, &
       0.D0,0.D0,0.D0,0.D0,0.00066D0/
      DATA DKM/-0.0004D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0, &
       0.D0,0.D0/
!#######################################################################
!     Shida-numbers:
!#######################################################################
      DATA DL0/ 0.0840D0,0.0841D0,0.0852D0,0.0149D0,0.0149D0,0.0149D0, &
       0.0149D0,0.0100D0,0.0100D0,0.0100D0,0.0100D0,0.0100D0/
      DATA DLP/-0.002D0,-0.002D0,-0.001D0,0.0000D0,0.0000D0,0.0000D0, &
       0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0/
      DATA DLM/ 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0, &
       0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0/
      DATA DLATP/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0, &
       0.D0,0.D0/
      DATA DLATM/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0, &
       0.D0,0.D0/
!#######################################################################
!     Definition of parameters of Geodetic Reference System 1980.
!     DEA  is major semi axis in meter.
!     DEE  is square of first excentricity (without dimnension).
!     DEGM is geocentric gravitational constant in m*3/s**2.
!#######################################################################
      DATA DEA/6378137.00D0/,DEE/6.69438002290D-3/
!#######################################################################
!     Define resonance frequency and resonance factors:
!#######################################################################
      DOMR=15.073729D0
      DOM0=13.943036D0
      DGR =-0.000625D0
      DHR =-0.002505D0
      DKR =-0.001261D0
      DLR =0.0000781D0
!#######################################################################
!     DCLAT is cos and DSLAT is sin of ellipsoidal latitude.
!#######################################################################
      DCLAT=COS(DLAT*DRAD)
      DSLAT=SIN(DLAT*DRAD)
!#######################################################################
!     Compute ellipsoidal curvature radius DN in meter.
!#######################################################################
      DN=DEA/SQRT(1.D0-DEE*DSLAT**2)
!#######################################################################
!     Compute geocentric latitude DPSI in degree:
!#######################################################################
      DPSI=DRO*ATAN(((DN*(1.D0-DEE)+DELV)*DSLAT)/((DN+DELV)*DCLAT))
      DTHET=90.D0-DPSI
      DCT=COS(DTHET*DRAD)
      DCT2=DCT*DCT
      DLATP(1)=0.335410D0*(35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)/(3.D0*DCT2-1.D0)
      DLATM(1) =0.894427D0/(3.D0*DCT2-1.D0)
      DLATP(2) =0.612372D0*(7.D0*DCT2-3.D0)
      DLATP(3) =0.866025D0*(7.D0*DCT2-1.D0)
      DLATP(7) =0.829156D0*(9.D0*DCT2-1.D0)
      DLATP(12)=0.806226D0*(11.D0*DCT2-1.D0)
!#######################################################################
!     Compute latitude dependent gravimeter factors DG:
!#######################################################################
      DO 110 I=1,12
  110 DGLAT(I)=DG0(I)+DGP(I)*DLATP(I)+DGM(I)*DLATM(I)
!#######################################################################
!     Compute latitude dependent LOVE-numbers DH (for vertical
!     displacement):
!#######################################################################
      DO 120 I=1,12
  120 DHLAT(I)=DH0(I)+DHP(I)*DLATP(I)+DHM(I)*DLATM(I)
!#######################################################################
!     Compute latitude dependent LOVE-numbers DK:
!#######################################################################
      DO 130 I=1,12
  130 DKLAT(I)=DK0(I)+DKP(I)*DLATP(I)+DKM(I)*DLATM(I)
!#######################################################################
!     Compute latitude dependent SHIDA-numbers DL:
!#######################################################################
      DO 140 I=1,12
  140 DLLAT(I)=DL0(I)+DLP(I)*DLATP(I)+DLM(I)*DLATM(I)
!#######################################################################
!     Compute latitude dependent tilt factors DT:
!#######################################################################
      DO 150 I=1,12
      DTLAT(I)=1.D0+DK0(I)-DH0(I)+DLATP(I)*(DKP(I)-DHP(I))+DLATM(I)*(DKM(I)-DHM(I))
  150 CONTINUE
      DTR=DKR-DHR
      IF(IPRINT.EQ.0) RETURN
!#######################################################################
!     Print out of parameters:
!#######################################################################
      WRITE(IUN16,17001) DOM0,DOMR,DGR,DHR,DKR,DLR,DTR
      I=0
      WRITE(IUN16,17002) DLAT
      DO 300 L=2,4
      WRITE(IUN16,17004)
      DO 300 M=0,L
      I=I+1
      WRITE(IUN16,17003)  L,M,DGLAT(I),DHLAT(I),DKLAT(I),DLLAT(I),DTLAT(I)
  300 CONTINUE
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(/6x,'Routine ETLOVE, version 1996.05.25.'/ &
       6x,'Latitude dependent parameters for an elliptical, rotating,'/ &
       6x,'inelastic and oceanless Earth from Wahr-Dehant-Zschau model.' &
       // &
       6x,'frequency of wave O1:',F10.6,' deg per hour'/ &
       6x,'resonance frequency :',F10.6,' deg per hour'// &
       6x,'resonance factor for G:',F10.6/ &
       6x,'resonance factor for h:',F10.6/ &
       6x,'resonance factor for k:',F10.6/ &
       6x,'resonance factor for l:',F10.6/ &
       6x,'resonance factor for T:',F10.6/)
17002 FORMAT(// &
       6x,'Latitude dependent elastic parameters'// &
       6x,'ellipsoidal latitude:',F10.4,' deg'// &
       6x,'G    is gravimetric factor delta'/ &
       6x,'h    is LOVE-number  h'/ &
       6x,'k    is LOVE-number  k'/ &
       6x,'l    is SHIDA-number l'/ &
       6x,'T    is tilt factor gamma'// &
       6x,'degree  order         G         h         k         l', &
       '         T')
17003 FORMAT(6x,2I7,5F10.6)
17004 FORMAT(' ')
      RETURN
END SUBROUTINE


SUBROUTINE ETPHAS(IUN16,IPRINT,IMODEL,DLON,DJULD)
!#######################################################################
!     Routine ETPHAS, version 1996.08.03 Fortran 90.
!     The routine ETPHAS computes phases and frequencies for the tidal
!     waves using different tidal potential catalogues which use
!     the Hartmann and Wenzel (1995) normalization.
!     All variables with D as first character are DOUBLE PRECISION.
!     Input parameter description:
!     ----------------------------
!     IUN16:       Formatted line printer unit.
!     IPRINT:      Printout parameter.
!                  for IPRINT = 0, nothing will be printed.
!                  for IPRINT = 1, a short list will be printed.
!                  for IPRINT = 2, a long list will be printed
!                  (including the tidal potential development).
!     IMODEL:      Parameter for selecting the tidal potential
!                  development.
!                  IMODEL = 1: Doodson (1921) tidal potential develop-
!                              ment with 378 waves.
!                  IMODEL = 2: Cartwright-Taylor-Edden (1973) tidal
!                              potential development with 505 waves.
!                  IMODEL = 3: Buellesfeld (1985) tidal potential
!                              development with 656 waves.
!                  IMODEL = 4: Tamura (1987) tidal potential develop-
!                              ment with 1200 waves.
!                  IMODEL = 5: Xi (1989) tidal potential catalogue
!                              2933 waves.
!                  IMODEL = 6: Roosbeek (1995) tidal potential
!                              catalogue with ?? waves.
!                  IMODEL = 7: Hartmann and Wenzel (1995) tidal
!                              potential catalogue with 12935 waves.
!&                 IMODEL = 8: Kudryavtsev (2004) tidal potential
!&                             catalogue with 28806 waves.
!     DLON:        Ellipsoidal longitude referring to Geodetic
!                  Reference System 1980 in degree, positive east of
!                  Greenwhich.
!     DJULD:       Julian date of the initial epoch of tidal force
!                  development.
!     Output parameter description:
!     -----------------------------
!     There are no output parameters. The computes phases are trans-
!&    to the calling program unit by COMMON /TIDWAVE1/ and
!&    COMMON /TIDWAVE2/.
!&    COMMON /TIDWAVE1/: contains tidal waves
!     NW:          Number of defined tidal waves.
!&    IWNR:        INTEGER array (1:30000) of wave numbers.
!&    IAARG:       INTEGER array (1:30000,1:12) of astronomical
!                  argument numbers.
!     COMMON /TIDWAVE2/: contains tidal waves
!&    DX0:         DOUBLE PRECISION array (1:30000) of cos-coeffi-
!                  cients of the tidal component in units of the tidal
!                  component.
!&    DX1:         DOUBLE PRECISION array (1:30000) of time deriva-
!                  tives of cos-coefficients of the tidal component.
!&    DX2:         DOUBLE PRECISION array (1:30000) of second time
!&                 derivatives of cos-coefficients of the tidal
!&                 component.
!&    DY0:         DOUBLE PRECISION array (1:30000) of sin-coeffi-
!                  cients of the tidal component in units of the tidal
!                  component.
!&    DY1:         DOUBLE PRECISION array (1:30000) of time deriva-
!                  tives of sin-coefficients of the tidal component.
!&    DY2:         DOUBLE PRECISION array (1:30000) of second time
!&                 derivatives of sin-coefficients of the tidal
!&                 component.
!                  component  unit of     unit of
!                  IC         DX0,DY0     DX1,DY1
!                  -1         m**2/s**2   m**2/s**2 per Julian century
!                   0         nm/s**2     nm/s**2   per Julina century
!                   1         mas         mas       per Julian century
!                   2         mm          mm        per Julian century
!                   3         mm          mm        per Julian century
!                   4         nstr        nstr      per Julian cenrury
!                   5         nstr        nstr      per Julian century
!                   6         nstr        nstr      per Julian century
!                   7         nstr        nstr      per Julian century
!                   8         nstr        nstr      per Julian century
!                   9         mm          mm        per Julian century
!& added ksm
!                  component  unit of
!                  IC         DX2,DY2
!                  -1         m**2/s**2 per Julian century**2
!                   0         nm/s**2   per Julina century**2
!                   1         mas       per Julian century**2
!                   2         mm        per Julian century**2
!                   3         mm        per Julian century**2
!                   4         nstr      per Julian cenrury**2
!                   5         nstr      per Julian century**2
!                   6         nstr      per Julian century**2
!                   7         nstr      per Julian century**2
!                   8         nstr      per Julian century**2
!                   9         mm        per Julian century**2
!&    DTHPH:       DOUBLE PRECISION array (1:30000) of tidal phases
!                  in radians at initial epoch.
!&    DTHFR:       DOUBLE PRECISION array (1:30000) of tidal
!                  frequencies in radian per hour.
!&    DBODY:       DOUBLE PRECISION array (1:30000) of body tide
!                  amplitude factors for tidal gravity and tidal tilt.
!                  In order to compute the body tide, the coefficients
!&                 DX0, DX1, DX2, DY0, DY1 and DY2 have to be
!&                 multiplied by DBODY.
!     Used routines:
!     --------------
!     ETASTN: computes astronomical elements.
!     ETJULN: computes Julian date.
!     ETDDTA: computes the difference TDT minus UTC (called by ETASTN).!
!     ETPOLC: computes the difference DUT1 = UT1 - UTC.
!     Numerical accuracy:
!     -------------------
!     The routine has been tested under operation systems UNIX and
!     MS-DOS with 15 digits in DOUBLE PRECISION.
!     Routine creation:  1988.04.27 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE 1,
!                        Germany.
!                        Tel: 0049-721-6082307,
!                        FAX: 0049-721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.08.04 by Hans-Georg Wenzel.
!#######################################################################
      use PARAMS
      use TIDPHAS
      use TIDWAVE
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      REAL(8) DAS(11),DASP(11)
      SAVE
      DATA IUN30/30/,IUN31/31/
      IF(IPRINT.GT.0) WRITE(IUN16,17001) CMODEL(IMODEL)
 1000 CONTINUE
!#######################################################################
!     Interpolate DUT1:
!#######################################################################
      CALL ETPOLC(IUN16,IUN30,IUN31,IPRINT,DJULD,DCLAT,DSLAT, &
        DCLON,DSLON,DPOLX,DPOLY,DUT1,DTAI,DLOD,DGPOL,DGPOLP,DGLOD,NERR)
!#######################################################################
!     Compute astronomical elements for initial epoch:
!#######################################################################
      CALL ETASTN(IUN16,IPRINT,IMODEL,DLON,DJULD,DUT1,DAS,DASP,DDT0)
!#######################################################################
!     Compute phases and frequencies:
!#######################################################################
      DO 1110 IW=1,NW
      DC2=0.D0
      DC3=0.D0
      DO 1140 J=1,11
      DC2=DC2+DBLE(IAARG(IW,J))*DAS(J)
 1140 DC3=DC3+DBLE(IAARG(IW,J))*DASP(J)
      LI=IAARG(IW,12)
      JCOF=(LI+1)*LI/2-2+IAARG(IW,1)
      DC2=DC2+DPK(JCOF)
 1160 DC2=MOD(DC2,360.D0)
      IF(DC2.GE.0.D0) GOTO 1170
      DC2=DC2+360.D0
      GOTO 1160
 1170 DTHPH(IW)=DC2*DRAD
      DTHFR(IW)=DC3*DRAD
 1110 CONTINUE
      IF(IPRINT.EQ.0) RETURN
      WRITE(IUN16,17002) NW
      WRITE(IUN16,17003)
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(' Routine ETPHAS, version 1996.08.04.'// &
      ' Tidal component development from tidal potential development.'// &
       1X,A13,' tidal potential development is used.'/)
17002 FORMAT(//' Routine ETPHAS, version 1996.08.04.'/ &
      'New phases and frequencies computes for',I6,' waves.')
17003 FORMAT(///' ***** Routine ETPHAS finished execution.'/)
END SUBROUTINE


SUBROUTINE ETPOLC(IUN16,IUN30,IUN31,IPRINT,DJULD,DCLAT,DSLAT, &
        DCLON,DSLON,DPOLX,DPOLY,DUT1,DTAI,DLOD,DGPOL,DGPOLP,DGLOD,NERR)
!#######################################################################
!     Routine ETPOLC, version 1996.05.25 Fortran 90.
!     The routine ETPOLC returns pole coordinates and correction DUT1
!     read from either formatted file on IUN30 or unformatted direct
!     access file on IUN31. In case that direct access file IUN31 does
!     not exist, it will be established by routine ETPOLC with file
!     File: etpolut1.bin.
!     All variables with D as first character are DOUBLE PRECISION.
!     Input parameter description:
!     ----------------------------
!     IUN16:       Formatted line printer unit.
!     IUN30:       Formatted file containing pole coordinates, DUT1
!                  and DTAI (e.g. file etpolut1.dat).
!     IUN31:       Unformatted direct access file containing pole
!                  coordinates, DUT1 and DTAI. This file will be
!                  opened as file etpolut1.bin during the execution of
!                  routine ETPOLC with STATUS=OLD if it exists and
!                  with STATUS=NEW, if it does not exist. If the
!                  file does not yet exist, etpolut1.bin will be
!                  established during the  execution of routine
!                  ETPOLC.
!     IPRINT:      Printout parameter.
!                  for IPRINT = 0, nothing will be printed.
!                  for IPRINT = 1, a short list will be printed.
!                  for IPRINT = 2, a long list will be printed
!     DJULD:       Julian date of the epoch, for which pole
!                  coordinates, DUT1 and DTAI will be returned.
!     DCLAT:       COS of latitude.
!     DSLAT:       SIN of latitude.
!     DCLON:       COS of longitude.
!     DSLON:       SIN of longitude.
!     Output parameter description:
!     -----------------------------
!     DPOLX:       X-pole coordinate in arc sec.
!     DPOLY:       Y-pole coordinate in arc sec.
!     DUT1:        Difference UT1 minus UTC in sec.
!     DTAI:        Difference TAI minus UT1 in sec.
!     DLOD:        Length of day - 86400 sec in sec.
!     DGPOL:       Pole tide in nm/s**2 for a rigid earth.
!     DGPOLP:      Time derivative of pole tide for a rigid earth in
!                  nm/s**2 per day.
!     DGLOD:       Gravity variation due to variation of the earth's
!                  rotation in nm/s**2.
!     NERR:        Error code, counts the number of errors which
!                  happened during the actual call of routine ETPOLC.
!                  For NERR > 0, the output parameters DPOLX, DPOLY,
!                  DUT1, DTAI, DLOD, DGPOL, DGPOLP do not contain
!                  valid information (all set to zero).
!                  For NERR=0, the output parameters DPOLX, DPOLY,
!                  DUT1 and DTAI contain valid information.
!     Execution time:
!     ---------------
!     3.02 microsec per call on a 100 MHz Pentium using Lahey LF90
!     compiler if the file ETPOLC.bin exists already.
!     Routine creation:  1993.08.31 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel: 0049-721-6082307,
!                        FAX: 0049-721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.05.25 by Hans-Georg Wenzel.
!#######################################################################
      use PARAMS
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      CHARACTER CHEAD(8)*10
      SAVE
      DATA DOM/7.292115D-5/,DA/6378137.D0/
      DATA ISTART/1/,IMJDO/0/
      NERR=0
      IF(ISTART.EQ.0) GOTO 1000
!#######################################################################
!     Test, whether there exist already unformatted file ETPOLUT1.bin:
!#######################################################################
      OPEN(UNIT=IUN31,FILE=TRIM(COMDIR)//TRIM(ETPOLUTBIN), &
        FORM='UNFORMATTED',STATUS='OLD',ACCESS='DIRECT',RECL=32,ERR=11)
!-GCR rewind removed because compiler complained
!      REWIND IUN31
      READ(IUN31,REC=1) IFIRST,ILAST
      ISTART=0
      GOTO 1000
   11 OPEN(UNIT=IUN31,FILE=TRIM(COMDIR)//TRIM(ETPOLUTBIN), &
        FORM='UNFORMATTED',STATUS='NEW',ACCESS='DIRECT',RECL=32)
!-GCR rewind removed because compiler complained
!      REWIND IUN31
!#######################################################################
!     Read file header of tidal potential file on unit IUN30:
!#######################################################################
      IF(IPRINT.EQ.0) GOTO 10
      WRITE(IUN16,17001)
   10 CONTINUE
      READ(IUN30,17002)                  (CHEAD(I),I=1,8)
      WRITE(IUN16,17003)                 (CHEAD(I),I=1,8)
  100 READ(IUN30,17002)                  (CHEAD(I),I=1,8)
      IF(IPRINT.GT.1) WRITE(IUN16,17003) (CHEAD(I),I=1,8)
      IF(CHEAD(1).NE.CENDT) GOTO 100
!#######################################################################
!     Read data:
!#######################################################################
      IREC=2
      ILAST=0
  200 READ(IUN30,17004) IDAT,ITIM,DMODJI,DPOLX,DPOLY,DUT1,DTAI
      IF(IDAT.EQ.C99) GOTO 300
      !-GCR fixed real to integer
      IF(IREC.EQ.2) IFIRST=INT(DMODJI)
      WRITE(IUN31,REC=IREC) DPOLX,DPOLY,DUT1,DTAI
      IF(IPRINT.GT.1) WRITE(IUN16,17005) IDAT,ITIM,IREC,DMODJI,DPOLX,DPOLY,DUT1,DTAI
      ILAST=IREC
      IREC=IREC+1
      GOTO 200
  300 CONTINUE
      WRITE(IUN31,REC=1) IFIRST,ILAST
      ISTART=0
!#######################################################################
!     Read pole coordinates, DUT1 and DTAI from direct access unit
!     IUN31:
!#######################################################################
 1000 DMODJD=DJULD-2400000.5D0
!-GCR fixed real to integer
      IMJD=INT(DMODJD)
!#######################################################################
!     DT is time difference referring to central sample point in days:
!#######################################################################
      DT=DMODJD-DBLE(IMJD)
      DT2=DT*DT
      IREC=IMJD-IFIRST+2
      IF(IREC.LT.2) THEN
        DPOLX=0.D0
        DPOLY=0.D0
        DUT1 =0.D0
        DTAI =0.D0
        DLOD =0.D0
        DGPOL=0.D0
        DGPOLP=0.D0
        NERR=1
        RETURN
      ENDIF
      IF(IREC.GT.ILAST-1) THEN
!#######################################################################
!     Use pole coordinates and DUT1 from last tabulated day:
!#######################################################################
        READ(IUN31,REC=ILAST)   DPOLX,DPOLY,DUT1,DTAI
        DLOD =0.D0
        DGPOL=DOM**2*DA*2.D0*DCLAT*DSLAT*(DPOLX*DCLON-DPOLY*DSLON)*DRAD/3600.D0*1.D9
        DGPOLP=0.D0
        NERR=1
        RETURN
      ENDIF
      IF(IMJD.EQ.IMJDO) GOTO 1100
      READ(IUN31,REC=IREC-1) DPOLX1,DPOLY1,DUT12,DTAI1
      READ(IUN31,REC=IREC)   DPOLX2,DPOLY2,DUT12,DTAI2
      READ(IUN31,REC=IREC+1) DPOLX3,DPOLY3,DUT13,DTAI3
      IMJDO=IMJD
!#######################################################################
!     Quadratic interpolation for pole coordinates and DTAI:
!     Linear interpolation for DUT1:
!#######################################################################
 1100 DPOLXA0=DPOLX2
      DPOLXA1=(DPOLX3-DPOLX1)*0.5D0
      DPOLXA2=(DPOLX1-2.D0*DPOLX2+DPOLX3)*0.5D0
      DPOLYA0=DPOLY2
      DPOLYA1=(DPOLY3-DPOLY1)*0.5D0
      DPOLYA2=(DPOLY1-2.D0*DPOLY2+DPOLY3)*0.5D0
      DTAIA0=DTAI2
      DTAIA1=(DTAI3-DTAI1)*0.5D0
      DTAIA2=(DTAI1-2.D0*DTAI2+DTAI3)*0.5D0
      DUT10=DUT12
      DDUT1=DUT13-DUT12
      IF(DDUT1.GT. 0.9D0) DDUT1=DDUT1-1.D0
      IF(DDUT1.LT.-0.9D0) DDUT1=DDUT1+1.D0
      DLOD = DTAIA1+2.D0*DTAIA2*DT
      DGLOD=2.D0*DLOD*DOM**2*DA*DCLAT*DCLAT*1.D9/86400.D0
      DPOLX=DPOLXA0+DT*DPOLXA1+DT2*DPOLXA2
      DPOLY=DPOLYA0+DT*DPOLYA1+DT2*DPOLYA2
      DUT1 =DUT10  +DT*DDUT1
      DTAI =DTAIA0 +DT*DTAIA1 +DT2*DTAIA2
      DGPOL=DOM**2*DA*2.D0*DCLAT*DSLAT*(DPOLX*DCLON-DPOLY*DSLON)*DRAD/3600.D0*1.D9
      DPOLXP=DPOLXA1+2.D0*DPOLXA2*DT
      DPOLYP=DPOLYA1+2.D0*DPOLYA2*DT
      DGPOLP=DOM**2*DA*2.D0*DCLAT*DSLAT*(DPOLXP*DCLON-DPOLYP*DSLON)*DRAD/3600.D0*1.D9
      RETURN
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(' Routine ETPOLC, version 1996.05.25.'// &
      ' Pole coordinates, DUT1, DTAI and pole tides from IERS data.'//)
17002 FORMAT(8A10)
17003 FORMAT(1X,8A10)
17004 FORMAT(I8,1X,I6,F10.3,5F10.5)
17005 FORMAT(I9,1X,2I6,F10.3,5F10.5)
END SUBROUTINE


SUBROUTINE ETPOTS(IUN14,IUN16,IUN24,IPRINT,IMODEL,DLAT,DLON,DH, &
        DGRAV,DAZ,IC,DJULD,DAMIN)
!#######################################################################
!     Routine ETPOTS, version 1996.08.05 Fortran 90.
!     The routine ETPOTS computes amplitudes, phases, frequencies and
!     body tide amplitude factors for a number of different Earth tide
!     components using different tidal potential catalogues which use
!     the Hartmann and Wenzel (1995) normalization.
!     Attention: This routine has finally not been tested for vertical
!                and horizontal displacements and for shear tidal
!                strain !#######################################################################
!     All variables with D as first character are DOUBLE PRECISION.
!     Input parameter description:
!     ----------------------------
!     IUN14:       Formatted unit, on which the tidal potential
!                  development has to be stored before the execution
!                  of routine ETPOTS  (e.g. file hw95s.dat).
!     IUN16:       Formatted line printer unit.
!     IUN24:       Unformatted copy of IUN14. This unit will be opened
!                  e.g. as file hw95s.bin during the execution of
!                  routine ETPOTS with STATUS=OLD if it exists and
!                  with STATUS=NEW, if it does not exist. If the file
!                  does not yet exist, it will be established during
!                  the execution of routine ETPOTS.
!     IPRINT:      Printout parameter.
!                  for IPRINT = 0, nothing will be printed.
!                  for IPRINT = 1, a short list will be printed.
!                  for IPRINT = 2, a long list will be printed
!                  (including the tidal potential development).
!     IMODEL:      Parameter for selecting the tidal potential
!                  development.
!                  IMODEL = 1: Doodson (1921) tidal potential develop-
!                              ment with 378 waves.
!                  IMODEL = 2: Cartwright-Taylor-Edden (1973) tidal
!                              potential development with 505 waves.
!                  IMODEL = 3: Buellesfeld (1985) tidal potential
!                              development with 656 waves.
!                  IMODEL = 4: Tamura (1987) tidal potential develop-
!                              ment with 1200 waves.
!                  IMODEL = 5: Xi (1989) tidal potential catalogue
!                              2933 waves.
!                  IMODEL = 6: Roosbeek (1995) tidal potential
!                              catalogue with ?? waves.
!                  IMODEL = 7: Hartmann and Wenzel (1995) tidal
!                              potential catalogue with 12935 waves.
!&                 IMODEL = 8: Kudryavtsev (2004) tidal potential
!&                             catalogue with 28806 waves.
!     DLAT:        Ellipsoidal latitude  referring to Geodetic
!                  Reference System 1980 in degree.
!     DLON:        Ellipsoidal longitude referring to Geodetic
!                  Reference System 1980 in degree, positive east of
!                  Greenwhich.
!     DH:          Ellipsoidal height referring to Geodetic Reference
!                  System 1980 in meter.
!     DGRAV:       Gravity in m/s**2. If the gravity is input below
!                  1 m/s**2, the gravity will be replaced by the
!                  computed normal gravity for reference system GRS80.
!     DAZ:         Azimuth in degree from north direction (only valid
!                  for tidal tilt, horizontal displacement, and
!                  horizontal strain).
!     IC:          Earth tide component to be computed.
!                  IC=-1: tidal potential in m**2/s**2.
!                  IC= 0: vertical tidal acceleration (gravity tide),
!                         in nm/s**2 (positive downwards).
!                  IC= 1: horizontal tidal acceleration (tidal tilt)
!                         in azimuth DAZ in mas = arc sec/1000.
!                  IC= 2: vertical tidal displacement, geodetic
!                         coefficients in mm (positive upwards).
!                  IC= 3: horizontal tidal displacement in azimuth
!                         DAZ in mm.
!                  IC= 4: vertical tidal strain in 10**-9 = nstr.
!                  IC= 5: horizontal tidal strain in azimuth DAZ
!                         in 10**-9 = nstr.
!                  IC= 6: areal  tidal strain in 10**-9 = nstr.
!                  IC= 7: shear  tidal strain in 10**-9 = nstr.
!                  IC= 8: volume tidal strain in 10**-9 = nstr.
!                  IC= 9: ocean tides, geodetic coefficients in
!                         millimeter.
!     DJULD:       Julian date of the initial epoch of tidal force
!                  development.
!     DAMIN:       Truncation parameter for the amplitude of tidal
!                  waves to be used in m**2/s**2. Only tidal waves
!                  with amplitudes greater or equal DAMIN will be
!                  used.
!                  Rms error of gravity tides compited from HW95 tidal
!                  potential catalogue versus amaplitude threshold,
!                  as computed from comparison with benchmark gravity
!                  tide series BFDE403A
!          DAMIN    no. of      rms error  min. error    max.error
!    [m**2/s**2]     waves      [nm/s**2]  [nm/s**2]     [nm/s**2]
!     1.00*10**-1      11    88.403330     -321.492678   297.866988
!     3.16*10**-2      28    27.319455     -108.174675   109.525103
!     1.00*10**-2      45    14.449139      -62.286861    67.322802
!     3.16*10**-3      85     6.020159      -32.560229    28.931931
!     1.00*10**-3     158     2.249690      -14.587415    11.931120
!     3.16*10**-4     268     0.978419       -6.780051     5.934767
!     1.00*10**-4     441     0.436992       -3.049676     2.943019
!     3.16*10**-5     768     0.173071       -1.331572     1.242490
!     1.00*10**-5   1 273     0.068262       -0.520909     0.484510
!     3.16*10**-6   2 052     0.029229       -0.217114     0.229504
!     1.00*10**-6   3 359     0.011528       -0.099736     0.085920
!     3.16*10**-7   5 363     0.004706       -0.038247     0.035942
!     1.00*10**-7   8 074     0.001999       -0.019407     0.017684
!     3.16*10**-8  10 670     0.001391       -0.012350     0.012287
!     1.00*10**-8  12 234     0.001321       -0.010875     0.011307
!     Output parameter description:
!     -----------------------------
!     There are no output parameters. The computed arrays are trans-
!&    ferred to the calling program unit by COMMON /TIDWAVE1/ and
!&    COMMON /TIDWAVE2/.
!&    COMMON /TIDWAVE1/: contains tidal waves
!     NW:          Number of defined tidal waves.
!&    IWNR:        INTEGER array (1:30000) of wave numbers.
!&    IAARG:       INTEGER array (1:30000,1:12) of astronomical
!                  argument numbers.
!&
!&    COMMON /TIDWAVE2/: contains tidal waves
!&
!&    DX0:         DOUBLE PRECISION array (1:30000) of cos-coeffi-
!                  cients of the tidal component in units of the tidal
!                  component.
!&    DX1:         DOUBLE PRECISION array (1:30000) of time deriva-
!                  tives of cos-coefficients of the tidal component.
!&    DX2:         DOUBLE PRECISION array (1:30000) of second time
!&                 derivatives of cos-coefficients of the tidal
!&                 component.
!&    DY0:         DOUBLE PRECISION array (1:30000) of sin-coeffi-
!                  cients of the tidal component in units of the tidal
!                  component.
!&    DY1:         DOUBLE PRECISION array (1:30000) of time deriva-
!                  tives of sin-coefficients of the tidal component.
!&    DY2:         DOUBLE PRECISION array (1:30000) of second time
!&                 derivatives of sin-coefficients of the tidal
!&                 component.
!                  component  unit of     unit of
!                  IC         DX0,DY0     DX1,DY1
!                  -1         m**2/s**2   m**2/s**2 per Julian century
!                   0         nm/s**2     nm/s**2   per Julian century
!                   1         mas         mas       per Julian century
!                   2         mm          mm        per Julian century
!                   3         mm          mm        per Julian century
!                   4         nstr        nstr      per Julian cenrury
!                   5         nstr        nstr      per Julian century
!                   6         nstr        nstr      per Julian century
!                   7         nstr        nstr      per Julian century
!                   8         nstr        nstr      per Julian century
!                   9         mm          mm        per Julian century
!& added
!                  component  unit of
!                  IC         DX2,DY2
!                  -1         m**2/s**2 per Julian century**2
!                   0         nm/s**2   per Julian century**2
!                   1         mas       per Julian century**2
!                   2         mm        per Julian century**2
!                   3         mm        per Julian century**2
!                   4         nstr      per Julian cenrury**2
!                   5         nstr      per Julian century**2
!                   6         nstr      per Julian century**2
!                   7         nstr      per Julian century**2
!                   8         nstr      per Julian century**2
!                   9         mm        per Julian century**2
!&    DTHPH:       DOUBLE PRECISION array (1:30000) of tidal phases
!                  in radians at initial epoch.
!&    DTHFR:       DOUBLE PRECISION array (1:30000) of tidal
!                  frequencies in radian per hour.
!     DBODY:       DOUBLE PRECISION array (1:30000) of body tide
!                  amplitude factors for tidal gravity and tidal tilt.
!                  In order to compute the body tide, the coefficients
!&                 DX0, DX1, DX2, DY0, DY1 and DY2 have to be
!&                 multiplied by DBODY.
!     Used routines:
!     --------------
!     ETASTN: computes astronomical elements.
!     ETGCON: computes geodetic coefficients.
!     ETJULN: computes Julian date.
!     ETLOVE: computes latitude dependent elastic parameters (called
!             ETGCOF).
!     ETDDTA: computes the difference TDT minus UTC (called by ETASTN).!
!     ETPOLC: computes the difference DUT1 = UT1 - UTC.
!     Numerical accuracy:
!     -------------------
!     The routine has been tested under operation systems UNIX and
!     MS-DOS with 15 digits in DOUBLE PRECISION.
!     Routine creation:  1988.04.27 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE 1,
!                        Germany.
!                        Tel: 0049-721-6082307,
!                        FAX: 0049-721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last modification: 1996.08.05 by Hans-Georg Wenzel.
!#######################################################################
      use PARAMS
      use LOVE
      use TIDPHAS
      use TIDWAVE
      IMPLICIT REAL(8) (D)
      IMPLICIT INTEGER (I-N)
      LOGICAL LEX24
      CHARACTER CHEAD(8)*10
!& increase cmodel
      CHARACTER CBOD*2,CWAVE*4
      INTEGER NS(11)
      REAL(8) DAS(11),DASP(11),DGK(25)
!#######################################################################
!     The following DIMENSION statement is concerning the elastic
!     Earth model for the different degree and order constituents.
!#######################################################################
      REAL(8) DELTA(25)
!& increase maxnw
      DATA IUN30/30/,IUN31/31/
!& increase cmodel
      IF(IPRINT.GT.0) WRITE(IUN16,17001) CMODEL(IMODEL)
      OPEN(UNIT=IUN14,FILE=TRIM(COMDIR)//TRIM(CFFILE(IMODEL)),FORM='FORMATTED',STATUS='OLD')
      REWIND(IUN14)
!#######################################################################
!     Test, whether there exist already the unformatted tidal
!     potential catalogue file:
!#######################################################################
      OPEN(UNIT=IUN24,FILE=TRIM(COMDIR)//TRIM(CUFILE(IMODEL)),FORM='UNFORMATTED',STATUS='OLD',ERR=11)
      LEX24=.TRUE.
      REWIND IUN24
      GOTO 12
   11 OPEN(UNIT=IUN24,FILE=TRIM(COMDIR)//TRIM(CUFILE(IMODEL)),FORM='UNFORMATTED',STATUS='NEW')
      LEX24=.FALSE.
      REWIND IUN14
   12 CONTINUE
!#######################################################################
!     Compute geodetic coefficients and body tide amplitude factors
!     for the WAHR-DEHANT-ZSCHAU model. The NDFW resonance is
!     approximated by
!     G0 - GR*(DOM - DOM0)/(DOMR - DOM),
!     similar equations hold for the other components.
!     Gravimetric amplitude factors, LOVE numbers h and k for zero to
!     third degree tidal potential have been taken from DEHANT 1987,
!     table 7, 8 and 9 for elliptical, uniformly rotating, oceanless
!     Earth with liquid outer core and inelastic mantle (PREM Earth
!     model with inelastic mantle from ZSCHAU) and for the fourth
!     degree from DEHANT et al. 1989, table 6). The resonance factors
!     GR have been computed to fit the difference between body tide
!     amplitude factors at waves O1 and PSI1 from DEHANT 1987, PREM
!     model with elastic mantle (table 1...3). The NDFW resonance
!     frequency is 15.073729 degree per hour  = 1.004915267 CPD UT,
!     taken from WAHR 1981 (because it is not given in any of DEHANT's
!     papers).
!#######################################################################
      CALL ETGCON(IUN16,IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
      IC2=IC+2
!#######################################################################
!     Define default body tide amplitude factors for components
!     IC=2...9.
!#######################################################################
      DO 50 I=1,25
   50 DELTA(I)=1.D0
      DELTAR=0.D0
      GOTO (100,200,300),IC2
      GOTO 1000
!#######################################################################
!     IC=-1, compute body tide amplitude factors for tidal potential:
!#######################################################################
  100 CONTINUE
      DO 110 I=1,12
  110 DELTA(I)=DKLAT(I)
      DELTAR=DKR
      GOTO 1000
!#######################################################################
!     IC=0, compute body tide amplitude factors for vertical component
!     (gravity tides):
!#######################################################################
  200 CONTINUE
      DO 210 I=1,12
  210 DELTA(I)=DGLAT(I)
      DELTAR=DGR
      GOTO 1000
!#######################################################################
!     IC=1: compute body tide amplitude factors for horizontal
!     component (tidal tilt):
!#######################################################################
  300 CONTINUE
      DO 310 I=1,12
  310 DELTA(I)=DTLAT(I)
      DELTAR=DKR-DHR
 1000 CONTINUE
      DT2000=(DJULD-2451544.D0)/36525.0D0
!#######################################################################
!     Interpolate DUT1:
!#######################################################################
      CALL ETPOLC(IUN16,IUN30,IUN31,IPRINT,DJULD,DCLAT,DSLAT, &
        DCLON,DSLON,DPOLX,DPOLY,DUT1,DTAI,DLOD,DGPOL,DGPOLP,DGLOD,NERR)
!#######################################################################
!     Compute astronomical elements for initial epoch:
!#######################################################################
      CALL ETASTN(IUN16,IPRINT,IMODEL,DLON,DJULD,DUT1,DAS,DASP,DDT0)
      IC2=IC+2
!#######################################################################
!     Read file header of tidal potential file on unit IUN14:
!#######################################################################
      IF(LEX24) THEN
          READ(IUN24) (CHEAD(I),I=1,8)
      ELSE
          READ(IUN14,17028)  (CHEAD(I),I=1,8)
          WRITE(IUN24) (CHEAD(I),I=1,8)
      ENDIF
      WRITE(IUN16,17029) (CHEAD(I),I=1,8)
 1100 CONTINUE
      IF(LEX24) THEN
          READ(IUN24)  (CHEAD(I),I=1,8)
      ELSE
          READ(IUN14,17028)  (CHEAD(I),I=1,8)
          WRITE(IUN24) (CHEAD(I),I=1,8)
      ENDIF
      IF(IPRINT.EQ.2) WRITE(IUN16,17029) (CHEAD(I),I=1,8)
      IF(CHEAD(1).NE.CENDT) GOTO 1100
!#######################################################################
!     Compute tidal development for the specific component from tidal
!     potential development:
!#######################################################################
      IW=1
      NWFILE=0
      NAMPL=0
      NTRUNC=0
 1110 CONTINUE
 1120 CONTINUE
!#######################################################################
!     Read tidal potential catalogue either from formatted or from
!     unformatted file. The format of the files is described in
!     Hartmann and Wenzel (1995a).
!#######################################################################
!& model 8 code incorporated into next 22 lines
      IF(LEX24) THEN
      IF(IMODEL.LE.7) THEN
          READ(IUN24) NRI,CBOD,LI,(NS(J),J=1,11),DFR,DC0I,DS0I,DC1I,DS1I,CWAVE
      ELSEIF(IMODEL.EQ.8) THEN
          READ(IUN24) NRI,CBOD,LI,(NS(J),J=1,11),DFR,DC0I,DS0I,DC1I,DS1I,DC2I,DS2I,CWAVE
      ENDIF
      ELSE
      IF(IMODEL.LE.7) THEN
          READ(IUN14,17006,END=2000) NRI,CBOD,LI,(NS(J),J=1,11),DFR,DC0I,DS0I,DC1I,DS1I,CWAVE
          WRITE(IUN24) NRI,CBOD,LI,(NS(J),J=1,11),DFR,DC0I,DS0I,DC1I,DS1I,CWAVE
      ELSEIF (IMODEL.EQ.8) THEN
          READ(IUN14,17007,END=2000) NRI,CBOD,LI,(NS(J),J=1,11),DFR,DC0I,DS0I,DC1I,DS1I,DC2I,DS2I,CWAVE
        IF (LI.EQ.1) LI=3
! ksm: in order to have the same LI as used in HW95
          WRITE(IUN24) NRI,CBOD,LI,(NS(J),J=1,11),DFR,DC0I,DS0I,DC1I,DS1I,DC2I,DS2I,CWAVE
        ENDIF
      ENDIF
      IF(NRI.GT.MAXNW) GOTO 2000
      NWFILE=NWFILE+1
!#######################################################################
!     Truncation of the tidal potential catalogue:
!#######################################################################
      DAM=SQRT(DC0I**2+DS0I**2)*1.D-10
      IF(DAM.LT.DAMIN) THEN
         NTRUNC=NTRUNC+1
         GOTO 1110
      ENDIF
      IF(IW.EQ.1) GOTO 1130
!#######################################################################
!     Check if the astronomical arguments are identical to those of
!     the last stored wave (for Hartmann and Wenzel 1995 potential):
!#######################################################################
      IDIFF=(LI-IAARG(IW-1,12))**2
      DO 1125 J=1,11
 1125 IDIFF=IDIFF+(NS(J)-IAARG(IW-1,J))**2
      IF(IDIFF.GT.0) GOTO 1130
!#######################################################################
!     Astronomical arguments are identical to those of last stored
!     wave. We will add up the coefficients for these two waves:
!#######################################################################
      IF(IW-1.GT.1) IWNR(IW-1)=NRI
      JCOF=(LI+1)*LI/2-2+NS(1)
      DX0(IW-1)=DX0(IW-1)+DC0I*DGK(JCOF)*1.D-10
      DY0(IW-1)=DY0(IW-1)+DS0I*DGK(JCOF)*1.D-10
      DX1(IW-1)=DX1(IW-1)+DC1I*DGK(JCOF)*1.D-10
      DY1(IW-1)=DY1(IW-1)+DS1I*DGK(JCOF)*1.D-10
!& add 4 lines
      IF (IMODEL.EQ.8) THEN
        DX2(IW-1)=DX2(IW-1)+DC2I*DGK(JCOF)*1.D-10
        DY2(IW-1)=DY2(IW-1)+DS2I*DGK(JCOF)*1.D-10
      END IF
      GOTO 1110
 1130 NAMPL=NAMPL+1
      DC2=0.D0
      DC3=0.D0
      IAARG(IW,12)=LI
      DO 1140 J=1,11
      IAARG(IW,J)=NS(J)
      DC2=DC2+DBLE(NS(J))*DAS(J)
 1140 DC3=DC3+DBLE(NS(J))*DASP(J)
      JCOF=(LI+1)*LI/2-2+NS(1)
      DC2=DC2+DPK(JCOF)
      IWNR(IW)=NRI
      DX0(IW)=DC0I*DGK(JCOF)*1.D-10
      DY0(IW)=DS0I*DGK(JCOF)*1.D-10
      DX1(IW)=DC1I*DGK(JCOF)*1.D-10
      DY1(IW)=DS1I*DGK(JCOF)*1.D-10
!& add 4 lines
      IF (IMODEL.EQ.8) THEN
        DX2(IW)=DC2I*DGK(JCOF)*1.D-10
        DY2(IW)=DS2I*DGK(JCOF)*1.D-10
      END IF
      DBODY(IW)=DELTA(JCOF)
      IF(JCOF.EQ.2) DBODY(IW)=DELTA(JCOF)+DELTAR*(DC3-DOM0)/(DOMR-DC3)
 1160 DC2=MOD(DC2,360.D0)
      IF(DC2.GE.0.D0) GOTO 1170
      DC2=DC2+360.D0
      GOTO 1160
 1170 CONTINUE
      DTHPH(IW)=DC2*DRAD
      DTHFR(IW)=DC3*DRAD
      IF(IPRINT.EQ.2) THEN
         DXTI=DX0(IW)+DX1(IW)*DT2000
         DYTI=DY0(IW)+DY1(IW)*DT2000
!& add 4 lines
      IF (IMODEL.EQ.8) THEN
           DXTI=DXTI+DX2(IW)*DT2000*DT2000
           DYTI=DYTI+DY2(IW)*DT2000*DT2000
         END IF
         DTHAM=SQRT(DXTI**2+DYTI**2)
         WRITE(IUN16,17011) IW,CBOD,LI,NS(1),DTHAM,DC2,DC3,CWAVE,DBODY(IW)
      ENDIF
      IW=IW+1
      IF(IW.GT.MAXNW) GOTO 5000
      GOTO 1110
 2000 CONTINUE
      NW=IW-1
      CLOSE(IUN14)
      IF(IPRINT.EQ.0) RETURN
      WRITE(IUN16,17010) NWFILE,NTRUNC,NW
      WRITE(IUN16,17030)
      RETURN
 5000 CONTINUE
      WRITE(IUN16,17050) NW,MAXNW
      STOP
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(//6X,'Routine ETPOTS, version 1996.08.05.'/ &
       6x,'Tidal waves from tidal potential catalogue.'/ &
       6X,A20,' tidal potential catalogue is used.'/)
17006 FORMAT(I6,1X,A2,I2,11I3,F12.8,2F12.0,2F10.0,1X,A4)
!& add 17007 format
17007 FORMAT(I6,1X,A2,I2,11I3,F12.8,2F12.0,2F10.0,2F8.0,1X,A4)
17008 FORMAT(1X,I4,10I2,4F7.5,F8.4,F9.4,F12.8,1X,A4/F7.5,F8.6,F9.6)
17010 FORMAT(//6x,' Number of waves read from file is :',I6/ &
               6x,' Number of waves above limit is    :',I6/ &
               6x,' Number of waves to be used is     :',I6/)
17011 FORMAT(I5,1X,A2,2I3,3F10.5,2X,A6,2X,F10.6)
17028 FORMAT(8A10)
17029 FORMAT(6X,8A10)
17030 FORMAT(///6x,'***** Routine ETPOTS finished execution.'/)
17050 FORMAT(/ &
       6x,'***** Error in routine ETPOTS.'/ &
       6x,'***** The current number of waves:',I5,' exceeds the ', &
       'maximum number of waves:',I5/ &
       6x,'***** Routine ETPOTS stops the execution.'/)
END SUBROUTINE


SUBROUTINE GEOEXT(IUN16,IRESET,DEXTIM,DEXTOT)
!#######################################################################
!     Routine GEOEXT, version 1996.08.05 Fortran 77/90.
!     === MS-DOS version for LAHEY-compiler ===================
!     The routine GEOEXT computes the actual job time and writes
!     the actual execution time on printer output unit IUN6.
!     For the first call of routine GEOEXT, the actual jobtime will
!     be computed (in secs since midnight) and stored. For the next
!     call(s) of routine GEOEXT, the actual jobtime will be computed
!     and the execution time (actual jobtime minus jobtime of the
!     first call of routine GEOEXT) will be printed.
!     Input parameter description:
!     ----------------------------
!     IUN16:       formatted printer unit.
!     IRESET:      DEXTIM will be resetted, if IRESET=1.
!     Output parameter description:
!     -----------------------------
!     DEXTIM:      actual jobtime in seconds (time elapsed from the
!                  last call of routine GEOEXT with IRESET=1 to the
!                  actual call of routine GEOEXT), double precision.
!     DEXTOT:      total jobtime in seconds (time elapsed from the
!                  first call of routine GEOEXT), double precision.
!     Used routines:
!     --------------
!     SYSTEM-CLOCK
!     Program creation:  1979.08.30 by Hans-Georg Wenzel,
!                        Black Forest Observatory,
!                        Universitaet Karlsruhe,
!                        Englerstr. 7,
!                        D-76128 KARLSRUHE,
!                        Germany.
!                        Tel.: 0721-6082301.
!                        FAX:  0721-694552.
!                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de
!     Last Modification: 1996.08.05 by Hans-Georg Wenzel.
!#######################################################################
      use INOUT
      IMPLICIT REAL(8) (D)
! MSFOR:      INTEGER*2 IH,IM,IS,IS100
      DATA IFIRST/1/
      SAVE DTIME1
      IF(IRESET.NE.1) GOTO 6003
! MSFOR:      CALL GETTIM(IH,IM,IS,IS100)
! MSFOR:      DTIME1=DBLE(IS+IM*60+IH*3600)+0.01*FLOAT(IS100)
! LAHEY 90:
      CALL SYSTEM_CLOCK(IC,ICR)
      DTIME1=DBLE(IC)/DBLE(ICR)
! UNIX:      DTIME1=DBLE(SECNDS(RDUMMY))
      WRITE(IUN16,17001)
      DEXTIM=0.D0
      DEXTOT=0.D0
      IF(IFIRST.EQ.1) THEN
        DTIME0=DTIME1
        IFIRST=0
      ENDIF
      IRESET=0
      RETURN
 6003 CONTINUE
! MSFOR:      CALL GETTIM(IH,IM,IS,IS100)
! MSFOR:      DTIME2=DBLE(IS+IM*60+IH*3600)+0.01*FLOAT(IS100)
! LAHEY:
      CALL SYSTEM_CLOCK(IC,ICR)
      DTIME2=DBLE(IC)/DBLE(ICR)
! UNIX: DTIME2=DBLE(SECNDS(RDUMMY))
      DEXTIM=DTIME2-DTIME1
      DEXTOT=DTIME2-DTIME0
      EXECTIME = DEXTIM
      WRITE(IUN16,17002) DEXTIM
!#######################################################################
!     Format statements:
!#######################################################################
17001 FORMAT(6x,'First call of routine GEOEXT, version 1996.08.05.')
17002 FORMAT(/6x,'Routine GEOEXT. Execution time=',F10.3,' sec'/)
      RETURN
END SUBROUTINE
