c--iers1.f
c--iers.for modified by D. Crossley, October 23, 2008
c-----------------------------------------------------------------------------
      PROGRAM IERS
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                                                                      !
C     Program IERS, version 1997.09.02 Fortran 90.                     !
C                                                                      !
C     This file has 244 records.                                       ! 
C                                                                      !
C     Transforms IERS earth rotation pole parameters.                  !
C     Input is IERS format as transferred via www from IERS Paris:     !
C                                                                      !
C               http://hpiers.obspm.fr                                 !
C                                                                      !
C     Output is ETERNA format for file etpolutc.dat on file iers.out.  !
C     Print file is iers.prn.                                          ! 
C                                                                      !
C     Input file names are read from formatted file iers.ini.          !
C                                                                      !
C     Program creation:  1996.01.01 by Hans-Georg Wenzel,              !
C                        Black Forest Observatory,                     !
C                        Universitaet Karlsruhe,                       !
C                        Englerstr. 7,                                 !
C                        D-76128 KARLSRUHE 1,                          !
C                        Germany.                                      !
C                        Tel: 0049-721-6082307,                        !
C                        FAX: 0049-721-694552.                         !
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  !
C     Last modification: 1997.09.02 by Hans-Georg Wenzel.              !
c

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER CTEXT(8)*10,CMON(12)*3,CMONI*3,CFILE*11
	character cline*80
      DATA IUN10/10/,IUN11/11/,IUN12/12/,IUN16/16/
      DATA CMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1'OCT','NOV','DEC'/
      OPEN(UNIT=IUN12,FILE='iers.out',FORM='FORMATTED')
      OPEN(UNIT=IUN16,FILE='iers.prn',FORM='FORMATTED')
      WRITE(*,17000)
      WRITE(IUN16,17000)
      IRESET=1
      ISCREEN=1
      CALL GEOEXT(IUN16,IRESET,ISCREEN,DEXTIM,DEXTOT)
      OPEN(UNIT=IUN10,FILE='iers1.ini',FORM='FORMATTED')
      DTAIOLD=0.D0
      INIT=1
  100 READ(IUN10,17003,END=1000) CFILE
      WRITE(IUN16,17004) CFILE
      WRITE(*,17004)     CFILE
      OPEN(UNIT=IUN11,FILE=CFILE,FORM='FORMATTED')
c      DO 110 J=1,15
c      READ(IUN11,17001) (CTEXT(I),I=1,8)
c      WRITE(*,17002)    (CTEXT(I),I=1,8)
c  110 CONTINUE
c      READ(IUN11,17008) IYEAR
c      READ(IUN11,17001) (CTEXT(I),I=1,8)
c  200 CONTINUE
c      READ(IUN11,17005,END=500) CMONI,IDAY,IJUL,DX,DY,DUT1
c--modifications to the IERS files require new read format
      do ii=1,14
	  read(iun11,'(a)') cline
      enddo
c	pause
 200  read(iun11,'(3i4,i7,2f11.6,f12.7)',end=500) iyear,imon,iday,
     &  ijul,dx,dy,dut1
c      write(*,'(3i4,i7,2f11.6,f12.7)') iyear,imon,iday,ijul,dx,dy,dut1
c      IMON=0
c      DO 210 M=1,12
c      IF(CMONI.NE.CMON(M)) GOTO 210
c      IMON=M
c      GOTO 220
c  210 CONTINUE
c  220 CONTINUE
      IDAT=10000*IYEAR+100*IMON+IDAY
      DMJUL=DBLE(IJUL)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Take care of leap seconds:                                       !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      DLEAPS=10.0D0
      IF(IDAT.GE.19720701) DLEAPS=11.D0
      IF(IDAT.GE.19730101) DLEAPS=12.D0
      IF(IDAT.GE.19740101) DLEAPS=13.D0
      IF(IDAT.GE.19750101) DLEAPS=14.D0
      IF(IDAT.GE.19760101) DLEAPS=15.D0
      IF(IDAT.GE.19770101) DLEAPS=16.D0
      IF(IDAT.GE.19780101) DLEAPS=17.D0
      IF(IDAT.GE.19790101) DLEAPS=18.D0
      IF(IDAT.GE.19800101) DLEAPS=19.D0
      IF(IDAT.GE.19810701) DLEAPS=20.D0
      IF(IDAT.GE.19820701) DLEAPS=21.D0
      IF(IDAT.GE.19830701) DLEAPS=22.D0
      IF(IDAT.GE.19850701) DLEAPS=23.D0
      IF(IDAT.GE.19880101) DLEAPS=24.D0
      IF(IDAT.GE.19900101) DLEAPS=25.D0
      IF(IDAT.GE.19910101) DLEAPS=26.D0
      IF(IDAT.GE.19920701) DLEAPS=27.D0
      IF(IDAT.GE.19930701) DLEAPS=28.D0
      IF(IDAT.GE.19940701) DLEAPS=29.D0
      IF(IDAT.GE.19960101) DLEAPS=30.D0
      IF(IDAT.GE.19970701) DLEAPS=31.D0
c--add new leap seconds
      if(idat.ge.19990101) dleaps=32.d0
      if(idat.ge.20060101) dleaps=33.d0
	  if(idat.ge.20090101) dleaps=34.d0
      DTAI=DLEAPS-DUT1
C
      IF(INIT.EQ.1) THEN
      WRITE(*,17006)     IDAT,DMJUL,DX,DY,DUT1,DTAI
      WRITE(IUN12,17007) IDAT,DMJUL,DX,DY,DUT1,DTAI
      WRITE(IUN16,17007) IDAT,DMJUL,DX,DY,DUT1,DTAI
         INIT=0
         DTAIOLD=DTAI
         GOTO 200
      ENDIF
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Check for leap second:                                           !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  300 DDT=DTAI-DTAIOLD
      IF(DDT.GT.0.5D0) THEN
         DLEAPS=DLEAPS-1.D0
         WRITE(*,17009)     IDAT,DLEAPS
         WRITE(IUN16,17009) IDAT,DLEAPS
      ENDIF
      IF(DDT.LT.-0.5D0) THEN
         DLEAPS=DLEAPS+1.D0
         WRITE(*,17009)     IDAT,DLEAPS
         WRITE(IUN16,17009) IDAT,DLEAPS
      ENDIF
      DTAI=DLEAPS-DUT1
      DDT=DTAI-DTAIOLD
      IF(DABS(DDT).GT.0.5D0) GOTO 300
      WRITE(*,17006)     IDAT,DMJUL,DX,DY,DUT1,DTAI
      WRITE(IUN12,17007) IDAT,DMJUL,DX,DY,DUT1,DTAI
      WRITE(IUN16,17007) IDAT,DMJUL,DX,DY,DUT1,DTAI
      DTAIOLD=DTAI
      GOTO 200
  500 GOTO 100
 1000 CONTINUE
      WRITE(*,17010)
      WRITE(IUN16,17010)
      CALL GEOEXT(IUN16,IRESET,ISCREEN,DEXTIM,DEXTOT)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Format statements:                                               !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
17000 FORMAT(' ######################################################'/
     1       ' #                                                    #'/
     2       ' #    Program IERS.FOR, version 1997.09.02 Fortran90. #'/
     3       ' #                                                    #'/
     4       ' #    Transformation of IERS EOP97C04-files into      #'/
     5       ' #    ETERNA format.                                  #'/
     6       ' #                                                    #'/
     7       ' ######################################################'/)
17001 FORMAT(8A10)
17002 FORMAT(1X,7A10,A9)
17003 FORMAT(A11,I5,F6.2)
17004 FORMAT(//' data input file is: ',A11/)
17005 FORMAT(2X,A3,I4,I7,2F9.5,F10.6)
17006 FORMAT(1X,I8,1X,'000000',F10.3,2F10.5,2F10.6)
17007 FORMAT(I8,1X,'000000',F10.3,2F10.5,2F10.6)
17008 FORMAT(10X,I5)
17009 FORMAT(' *** Leap second at: ',I8,' DLEAPS=',F10.3)
17010 FORMAT(' ######################################################'/
     1       ' #                                                    #'/
     2       ' #    Program IERS.FOR finished execution.            #'/
     3       ' #                                                    #'/
     4       ' ######################################################'/)
      END
C
      SUBROUTINE GEOEXT(IUN16,IRESET,ISCREEN,DEXTIM,DEXTOT)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                                                                      !
C     Routine GEOEXT, version 1997.08.22 Fortran 90.                   !
C                                                                      !
C     The routine GEOEXT computes the actual job time and writes       !
C     the actual execution time on printer output unit IUN6.           !
C     For the first call of routine GEOEXT, the actual jobtime will    !
C     be computed (in secs since midnight) and stored. For the next    !
C     call(s) of routine GEOEXT, the actual jobtime will be computed   !
C     and the execution time (actual jobtime minus jobtime of the      !
C     first call of routine GEOEXT) will be printed.                   !
C                                                                      !
C     Input parameter description:                                     !
C     ----------------------------                                     !
C                                                                      !
C     IUN16:       formatted printer unit.                             !
C     IRESET:      DEXTIM will be resetted, if IRESET=1.               !
C     ISCREEN:     Execution time will also be written on the screen   !
C                  of the PC.                                          !    
C                                                                      !
C     Output parameter description:                                    !
C     -----------------------------                                    !
C                                                                      !
C     DEXTIM:      actual jobtime in seconds (time elapsed from the    !
C                  last call of routine GEOEXT with IRESET=1 to the    !
C                  actual call of routine GEOEXT), double precision.   !
C     DEXTOT:      total jobtime in seconds (time elapsed from the     !
C                  first call of routine GEOEXT), double precision.    !  
C                                                                      !
C     Used routines:                                                   !
C     --------------                                                   !
C                                                                      !
C     SYSTEM-CLOCK                                                     !
C                                                                      !
C     Execution time:                                                  !
C     ---------------                                                  !
C                                                                      !
C     0.17 msec per call of GEOEXT with ISCREEN=0 on a PENTIUM 100 MHZ !
C     PC.                                                              !
C                                                                      !
C     Program creation:  1979.08.30 by Hans-Georg Wenzel,              !
C                        Black Forest Observatory,                     !
C                        Universitaet Karlsruhe,                       !
C                        Englerstr. 7,                                 !
C                        D-76128 KARLSRUHE,                            !
C                        Germany.                                      !
C                        Tel.: 0721-6082301.                           !
C                        FAX:  0721-694552.                            !
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  !
C     Last Modification: 1997.08.22 by Hans-Georg Wenzel.              !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (D)
C MSFOR:      INTEGER*2 IH,IM,IS,IS100
      DATA IFIRST/1/
      SAVE DTIME1
      IF(IRESET.NE.1) GOTO 6003
C MSFOR:      CALL GETTIM(IH,IM,IS,IS100)
C MSFOR:      DTIME1=DBLE(IS+IM*60+IH*3600)+0.01*FLOAT(IS100)
C LAHEY 90:
      CALL SYSTEM_CLOCK(IC,ICR)
      DTIME1=DBLE(IC)/DBLE(ICR)
C UNIX:      DTIME1=DBLE(SECNDS(RDUMMY))
      WRITE(IUN16,17001)
      IF(ISCREEN.EQ.1) WRITE(*,17001)
      DEXTIM=0.D0
      DEXTOT=0.D0
      IF(IFIRST.EQ.1) THEN
        DTIME0=DTIME1
        IFIRST=0
      ENDIF 
      IRESET=0
      RETURN
 6003 CONTINUE
C MSFOR:      CALL GETTIM(IH,IM,IS,IS100)
C MSFOR:      DTIME2=DBLE(IS+IM*60+IH*3600)+0.01*FLOAT(IS100)
C LAHEY:
      CALL SYSTEM_CLOCK(IC,ICR)
      DTIME2=DBLE(IC)/DBLE(ICR)
C UNIX: DTIME2=DBLE(SECNDS(RDUMMY))   
      DEXTIM=DTIME2-DTIME1
      DEXTOT=DTIME2-DTIME0
      WRITE(IUN16,17002) DEXTIM
      IF(ISCREEN.EQ.1) WRITE(*,17002) DEXTIM
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Format statements:                                               !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
17001 FORMAT(6x,'First call of routine GEOEXT, version 1997.08.22.')
17002 FORMAT(/6x,'Routine GEOEXT. Execution time=',F10.3,' sec'/)
      RETURN
      END
