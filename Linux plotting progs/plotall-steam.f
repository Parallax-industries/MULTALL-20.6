C******************************************************************C 
C                                                                  C
C                         CONTOUR7 - Version 06e                   C
C                         ======================                   C
C     I/O UNITS                                                    C
C     =========                                                    C
C                                                                  C
C     The following units are used:                                C
C                                                                  C
C     INPUT  Unit 1  -   Console input                             C
C     OUTPUT Unit 2  -   Console output                            C
C     INPUT  Unit     4  -   Data for plotting                     C
C     OUTPUT Unit     8  -   Plotter                               C
C                                                                  C
C     A global edit is required to change the string "1"           C 
C     to the desired value (e.g. "1") for the keyboard input.      C
C                                                                  C 
C     A global edit is required to change the string "2"           C
C     to the desired value (e.g. "2") for the screen output.       C
C                                                                  C   
C     To connect input and output units, the open statements may   C   
C     need to be un-commented out. Remove the string "C====>' to   C    
C     do this. A 'query replace' is best in this case.             C   
C                                                                  C
C                                                                  C
C     HGRAPH + CONTOURING:                                         C
C     ====================                                         C
C                                                                  C
C     There are several versions of HGRAPH.                        C
C                                                                  C
C     VERSION 09:  globally exchange the string "0" to "0"         C
C     VERSION 11:  globally exchange the string "0" to "0"         C
C                  but use "100" if you want labelled contours     C
C     VERSION 22+: globally exchange the string "0" to "0"         C
C                  or "100" as above but if you ALSO wish to use   C
C                  filled contours (i.e. -8d/coloured), then       C
C                  look at the lines beginning with the string     C
C                  "  " and comment these back in having           C           
C                  globally exchanged "-8" to "-8" and             C
C                  0 to " 0 "  or "100".                           C
C                                                                  C
C   ***********************************************************    C
C   * THE ABOVE MUST BE CARRIED OUT BEFORE CONTOUR7 WILL WORK *    C    
C   ***********************************************************    C
C                                                                  C
C     BTOB3d + SPLITTERS:                                          C
C     ===================                                          C
C                                                                  C
C     A special version of CONTOUR7 is needed to work with         C
C     BTOB3d output containing splitter blades. To create this     C
C     version, globally delete the string "C-SPLIT->". Note that   C
C     these changes do not apply to the Denton Codes.              C
C                                                                  C  
C     ARRAY SIZES                                                  C 
C     ===========                                                  C 
C                                                                  C 
C     This program is currently dimensioned for arrays of          C
C     95x2500x95 data points. To adjust these values, several       C
C     global exchanges are required                                C
C                                                                  C
C     (95,2500,95) to (IMAX,JMAX,KMAX)                              C
C     (95,2500)    to (IMAX,JMAX)                                   C
C        (2500,95) to      (JMAX,KMAX)                              C
C     (95)        to (IMAX)                                        C
C        (2500)    to      (JMAX)                                   C
C            (95) to           (KMAX)                              C
C        (95,2500) to      (KMAX,JMAX)                              C
C                                                                  C
C     where IMAX,JMAX and KMAX are the new values required.        C
C                                                                  C
C     Note that not only the dimensions are affected - this is     C
C     as intended. Also note that it is necessary that             C
C                                                                  C
C                     JMAX > IMAX > KMAX                           C
C                                                                  C
C                                   H.P. HODSON 17/07/96           C
C                                                                  C
C******************************************************************C
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK9 / P(95,2500,95)
      COMMON / BLK10/ RO(95,2500,95)
      COMMON / BLK11/ M_ABS(95,2500,95),M_REL(95,2500,95)
      COMMON / BLK14/ Q(95,2500,95),QQ(95,2500,95),NQ,NQQ
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK19/ XX(95,2500),YY(95,2500),ZZ(95,2500),
     &                WORK1(95,2500),WORK2(95,2500)
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
      COMMON /BLKPRIM/ ROVX(95,2500,95),ROVR(95,2500,95),
     &                 ROVT(95,2500,95),ROE(95,2500,95),
     &                 ENTPY(95,2500,95),GA_PV(95,2500,95),
     &                 H_TO_T(95,2500,95),WET(95,2500,95),
     &                 P_STAGN(95,2500,95),T_STAGN(95,2500,95),
     &                 T_STATIC(95,2500,95),PO_REL(95,2500,95),
     &                 HO(95,2500,95)
C
      DIMENSION PROP(95,2500,95)
C
      CHARACTER*32 NPROP,NQ,NQQ
      CHARACTER*8  FMT
      CHARACTER*72 JBNAME,TSNUM
C
      REAL M_ABS, M_REL, EMACH, IMACH
C
      CHARACTER*72 INFILE
C
C     Defaults to questions
C
      CHARACTER*1  CTYPE1, ANS1, ANS
      COMMON / OPT1 / DELR1, ZSCAL1, VMOD,
     &                NDIFF, I1, J1, K1, I1Z, J1Z, K1Z,IPIT1,ANS1,CTYPE1
C
      COMMON / OPT2 / ISTART, IEND, IDIFF, JSTART, JEND, JDIFF,
     &                KSTART, KEND, KDIFF
C
      CHARACTER*1  CTYPE6, ANS6
      COMMON / OPT6 / I6, J6, K6, DELR6, ZSCAL6, I6Z, J6Z, K6Z, 
     &                BETAXX, BETAYY, BETAZZ, ANS6, CTYPE6
C
      ANS1    = 'S'
      K1      = 1
      I1      = 1
      J1      = 1
      DELR1   = 0.0
      ZSCAL1  = 1.0
      I1Z     = 1
      J1Z     = 1
      K1Z     = 1
      CTYPE1  = 'N'
      IPIT1   = 0
      VMOD    = 1.0
      NDIFF   = 1
C
      ISTART  = 1
      IEND    = 1
      IDIFF   = 1
      JSTART  = 1
      JEND    = 1
      JDIFF   = 1
      KSTART  = 1
      KEND    = 1
      KDIFF   = 1
C
      ANS6    = 'S'
      K6      = 1
      I6      = 1
      J6      = 1
      DELR6   = 0.0
      ZSCAL6  = 1.0
      ZLAST   = 1.0
      I6Z     = 1
      J6Z     = 1
      K6Z     = 1
      CTYPE6  = 'N'
      BETAXX  = 0.
      BETAYY  = 0.
      BETAZZ  = 0.
C
      OPEN  (UNIT=1,FILE='/dev/tty')
C
      OPEN(UNIT=13,FILE='outlet-props')
C
C   SET 'laser.sys' TO 1.0  TO MAKE COLOUR PLOTS
      OPEN(UNIT=2,FILE='laser.sys')
      WRITE(2,*) 1
      CLOSE(2)
C
C     ANNOUNCE PROGRAM
C
      WRITE (6,2000)
C 
C
C     Find out the input file name
C
      INFILE   = 'flow_out'
      WRITE(6,*) ' THE input file  is named flow_out, is this OK ? '
      WRITE(6,*) ' Answer Y or N '
      READ(1,*) ANS
      IF(ANS.EQ.'N'.OR.ANS.EQ.'n') THEN
           WRITE(6,*) ' INPUT THE NAME OF THE INPUT FILE'
           READ(1,1080) INFILE
      END IF
C
      WRITE(6,*) ' Input file = ' ,INFILE
C
      OPEN(UNIT=7,FILE= INFILE,   FORM='unformatted')
C
C      INFILE = 'flow_out'
C      WRITE (6,2080)
C      READ  (1,1080,ERR=5,END=5) INFILE
C    5 CONTINUE
C      OPEN  (UNIT=7,FILE=INFILE,form ='unformatted')
C
      OPEN(UNIT=21,FILE='grid_out',form= 'unformatted' )
C
C***************************************************************************************
C     CALL SUB0  TO INPUT THE GRID GEOMETRY AND CONTROL DATA FROM UNIT 21 .
C
      WRITE(6,*) ' calling sub 0 '
      CALL SUB0 ( ICENT )
      WRITE(6,*) ' done sub 0 '
C
C***************************************************************************************
C
C     SET DEFAULT SIZES FOR BOX PLOTS
C
      X0B   = 50.
      X1B   = 205.
C
C
      Y0B   = 40.
      Y1B   = 180.
C
C     SET DEFAULT SIZES FOR CONTOURS
C
      X0C   = 5.
      X1C   = 262.
C
      Y0C   = 20.
      Y1C   = 192.
C**************************************************************************************
C**************************************************************************************
C**************************************************************************************

C         INPUT FLOW VARIABLES AT EACH TIME STEP
C
   99 CONTINUE
C

        ICENT = 0
C
C   CALL SUB4  TO INPUT A NEW SET OF FLOW VARIABLES.
C
        CALL SUB4
C
C**************************************************************************************
C**************************************************************************************
C      ANNOUNCE PROGRAM
C
      WRITE (6,2000)
C
C              DISPLAY MENU AND DECIDE UPON ACTION
C
C     Decide upon variable
C
   13 CONTINUE
C
      CALL SUB8(IPROP)
C
C
      IF (IPROP.EQ.14) THEN
         CALL SUB15('     COMPUTATIONAL MESH         ',
     &                             X0C,Y0C,X1C,Y1C,ID)
         GO TO 13
      ELSE IF (IPROP.EQ.15) THEN
         CALL SUB16('       VELOCITY VECTORS         ',
     &                       X0C,Y0C,X1C,Y1C,ID)
         GO TO 13
      ELSE IF (IPROP.EQ.23) THEN
         CALL SUB38('      PARTICLE TRACKS           ',
     &                       X0C,Y0C,X1C,Y1C,ID)
         GO TO 13
      ELSE IF (IPROP.EQ.24) THEN
         CALL SUB45('   PITCHWISE AVERAGES           ')
         GO TO 13
C
      ELSE IF (IPROP.EQ.99) THEN
         STOP
C
C
      ELSE IF (IPROP.EQ.98) THEN
         GO TO 99
C
C
      ELSE IF (IPROP.EQ.0) THEN
         GO TO 13
      ELSE
C
C     Generate plotting variable
C
      CALL SUB22(PROP,NPROP,FMT,IPROP)
C
   18 CALL SUB9(IREPLY)
C
      IF (IREPLY.EQ.1) THEN
          CALL SUB5(PROP,NPROP,X0C,Y0C,X1C,Y1C,FMT,ID)
      ELSE IF (IREPLY.EQ.2) THEN
          CALL SUB10(PROP,NPROP,X0B,Y0B,X1B,Y1B,FMT,ID)
      ELSE IF (IREPLY.EQ.3) THEN
          CALL SUB11(PROP,NPROP,X0B,Y0B,X1B,Y1B,FMT,ID)
      ELSE IF (IREPLY.EQ.4) THEN
          CALL SUB12(PROP,NPROP,X0B,Y0B,X1B,Y1B,FMT,ID)
      ELSE IF (IREPLY.EQ.5) THEN
          CALL SUB13(PROP,NPROP,X0B,Y0B,X1B,Y1B,FMT,ID)
      ELSE IF (IREPLY.EQ.6) THEN
          CALL SUB35(PROP,NPROP,X0C,Y0C,X1C,Y1C,FMT,ID)
      ELSE IF (IREPLY.EQ.7) THEN
          CALL SUB23(PROP,0)
          GO TO 18
      ELSE IF (IREPLY.EQ.8) THEN
          CALL SUB23(PROP,1)
          GO TO 18
C
      ELSE IF (IREPLY.EQ.98) THEN
          GO TO 13
C**************************************************************************************
C    GO TO 99 TO READ IN A NEW SET OF RESULTS
C
      ELSE IF (IREPLY.EQ.99) THEN
      STOP
C
C**************************************************************************************
C**************************************************************************************
C
      ENDIF
C
C**************************************************************************************
C
      GO TO 13
C
      ENDIF
C
C**************************************************************************************
C**************************************************************************************
C
1080  FORMAT(A80) 
C 
2000  FORMAT(20(/),79('*')// 
     +   ' PLOTALL --- THE PLOTTING PROGRAM FOR MULTALL'//
     +             79('*')//)
2080  FORMAT     (//' Enter the name of file to be processed, default =
     & "flow_out" ')
C 
C
      END
C******************************************************************C
C
C         SUBROUTINE TO INPUT COORDINATES AND CONTROL NUMBERS
C
C******************************************************************C
C
      SUBROUTINE SUB0 ( ICENT )
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
       CHARACTER*72 JBNAME,TSNUM
C
       PI     = 3.14159265
       ICENT  = 0
C
C****************************************************************************
C****************************************************************************
      WRITE(6,*) ' IN SUBROUTINE  SUB0 '
C
C****************************************************************************
C****************************************************************************
C
      WRITE(6,*) ' STARTING TO READ IN GRID GEOMETRY DATA FROM UNIT 21 '
C
      READ(21) NSTEPS
      WRITE(6,*) ' MAXIMUM NUMBER OF TIME STEPS = ', NSTEPS
      READ(21) IM,JM,KM
      WRITE(6,*) ' GRID POINT NUMBERS, IM, JM, KM = ',IM,JM,KM
      READ(21) CP,GA
      WRITE(6,*) '  CP = ', CP, ' GAMMA = ', GA
      READ(21) (IND(J),J=1,JM)
      READ(21) (W(J),J=1,JM)
      READ(21) (NBLADES(J),J=1,JM)
C 
      DO 250 J=1,JM
      DO 250 K=1,KM 
      READ(21) X(J,K),R(J,K),(T(I,J,K),I=1,IM)
  250 CONTINUE
C
      CLOSE(21)
C
      NLETE = 0
      DO 200 J=1,JM
      IF(IND(J).EQ.1) THEN
           NLETE = NLETE + 1
           NROW   = 1  + (NLETE -1)/2
           IF(MOD(NLETE,2).EQ.0)     JTEE(NROW) = J
           IF(MOD((NLETE+1),2).EQ.0) JLEE(NROW) = J
      END IF
  200 CONTINUE
C
      NROWS = NROW
      WRITE(6,*) ' TOTAL NUMBER OF BLADE ROWS = ', NROWS
      DO 205 N=1,NROWS
      WRITE(6,*) ' ROW NUMBER ',N,' JLE= ',JLEE(N),' JTE= ',JTEE(N),
     &  'JM =',JM
  205 CONTINUE
      WRITE(6,*)
C
      WRITE(6,*)
      WRITE(6,*) 'INPUT  JLE  and  JTE  FOR THE ROWS WHERE SURFACE DISTR
     &IBUTIONS ARE TO BE PLOTTED'
      READ(5,*)  JLE,JTE 
C
C
      DO 251 K=1,KM
      DO 251 J=1,JM
      PITCH(J,K) = 2*PI/NBLADES(J)
      DO 251 I=1,IM
      T(I,J,K)   = T(I,J,K)/R(J,K)
  251 CONTINUE
C
      DO 252 K = 1,KM
      SM(1,K) = 0.0
      DO 252 J = 2,JM
      XDIF    = X(J,K) - X(J-1,K)
      RDIF    = R(J,K) - R(J-1,K)
      SM(J,K) = SM(J-1,K) + SQRT(XDIF*XDIF + RDIF*RDIF)
  252 CONTINUE
C
      ISPLIT = IM
      KSPLIT = KM
      JSLE   = JM
      JSTE   = JM
      ICENT  = 0
      JBNAME = ' TITLE OF RUN '
C
C****************************************************************************
C****************************************************************************
C
      IF ( GA . EQ . 0.0 ) GA = 1.4
      IF ( CP . EQ . 0.0 ) CP = 1005.
C
C      Find range of theta coord
C      ---------------------------
C
      TMAX  = T(1,1,1)
      TMIN  = TMAX
C
      DO 17 K=1,KM
      DO 18 J=1,JM
          IF ( (T(IM,J,K)-PITCH(J,K)) .LT. TMIN ) THEN
              TMIN  = (T(IM,J,K)-PITCH(J,K))
          ENDIF
          IF ( (T(1,J,K)+PITCH(J,K)) .GT. TMAX ) THEN
              TMAX  = (T(1,J,K)+PITCH(J,K))
          ENDIF
   18 CONTINUE
   17 CONTINUE
C
C
C      TMID   = 0.5*(TMIN+TMAX)
C     JDD CHANGE 2/1/2001
      TMID = 0.5*(T(1,JLE,K) + T(1,JTE,K))
C
C      Find range of R-theta coord
C      ---------------------------
C
      RTMAX  = R(1,1)*(T(1,1,1)+PITCH(1,1)-TMID)
      RTMIN  = RTMAX
C
      DO 19 K=1,KM
      DO 20 J=1,JM
          IF ( R(J,K)*(T(IM,J,K)-PITCH(J,K)-TMID) .LT. RTMIN ) THEN
              RTMIN = R(J,K)*(T(IM,J,K)-PITCH(J,K)-TMID)
          ENDIF
          IF ( R(J,K)*(T(1,J,K)+PITCH(J,K)-TMID) .GT. RTMAX ) THEN
              RTMAX = R(J,K)*(T(1,J,K)+PITCH(J,K)-TMID)
          ENDIF
   20 CONTINUE
   19 CONTINUE
C
C
      RTMID  = 0.5*(RTMAX+RTMIN)
C
C      Find range of axial coord
C      -------------------------
C
      XMAX  = X(1,1)
      XMIN  = XMAX
C
      DO 21 K=1,KM
      DO 22 J=1,JM
          IF ( X(J,K) . GT . XMAX ) XMAX = X(J,K)
          IF ( X(J,K) . LT . XMIN ) XMIN = X(J,K)
   22 CONTINUE
   21 CONTINUE
C     JDD
      XMAX = X(JTE,KM)
      XMIN = X(JLE,KM)
C
      IF (KM.EQ.1) THEN
          IF (XMAX.EQ.XMIN) XMAX = XMIN + (RTMAX-RTMIN)/1000.
      ENDIF
C
C      Find range of Z-coord (Now redundant)
C      -------------------------------------
C
      ZMIN  = R(1,1)*COS(T(1,1,1)-TMID)
      ZMAX  = ZMIN
C
      DO 23 J=1,JM
      DO 24 I=1,IM
         IF ( R(J,KM)*COS(T(I,J,KM)-TMID) . GT . ZMAX )
     &       ZMAX = R(J,KM)*COS(T(I,J,KM)-TMID)
         IF ( R(J,1)*COS(T(I,J,1)-TMID) . LT . ZMIN )
     &       ZMIN = R(J,1)*COS(T(I,J,1)-TMID)
   24 CONTINUE
   23 CONTINUE
C
      IF (KM.EQ.1) THEN
         IF ( ZMAX.EQ.ZMIN) ZMAX = ZMIN + (RTMAX-RTMIN)/1000.
      ENDIF
C
C      Find range of Y-coord (Now redundant)
C      -------------------------------------
C
      YMAX  = R(1,1)*SIN(T(1,1,1)-TMID)
      YMIN  = YMAX
C
      DO 25 J=1,JM
      DO 26 K=1,KM
          IF ( R(J,K)*SIN(T(IM,J,K)-PITCH(J,K)-TMID) .LT. YMIN )
     &        YMIN = R(J,K)*SIN(T(IM,J,K)-PITCH(J,K)-TMID)
          IF ( R(J,K)*SIN(T(1,J,K)+PITCH(J,K)-TMID) .GT. YMAX )
     &        YMAX = R(J,K)*SIN(T(1,J,K)+PITCH(J,K)-TMID)
   26 CONTINUE
   25 CONTINUE
C
C      Find range of R-coord
C      ---------------------
C
      RMIN  = R(1,1)*COS(T(1,1,1)-TMID)
      RMAX  = RMIN
C
      DO 28 J=1,JM
         IF ( R(J,KM) . GT . RMAX )
     &       RMAX = R(J,KM)
         IF ( R(J,1) . LT . RMIN )
     &       RMIN = R(J,1)
   28 CONTINUE
C
      IF (KM.EQ.1) THEN
         IF ( RMAX.EQ.RMIN) RMAX = RMIN + (RTMAX-RTMIN)/1000.
      ENDIF
C
C      Find range of meridional coord
C      ------------------------------
C
      SMMAX = SM(1,1)
      SMMIN = SM(1,1)
C
      DO 30 K=1,KM
      DO 31 J=1,JM
         IF ( SM(J,K) . LT . SMMIN ) SMMIN = SM(J,K)
         IF ( SM(J,K) . GT . SMMAX ) SMMAX = SM(J,K)
   31 CONTINUE
   30 CONTINUE
C
C      Find range of Tangent QO-coord and establish tangent direction
C      --------------------------------------------------------------
C
      IF ( KM . GT . 1 ) THEN
        SRMAX = 0.0
        SRMIN = 0.0
C
        DO 40 J=1,JM
          DX     = X(J,KM) - X(J,1)
          DR     = R(J,KM) - R(J,1)
          COSA(J)= DX/SQRT(DX**2+DR**2)
          SINA(J)= DR/SQRT(DX**2+DR**2)
C
          DO 41 K=1,KM
               DO 42 I=1,IM
                   SRIJK = (X(J,K)-X(J,1))*COSA(J)
     &                    +(R(J,K)*COS(T(I,J,K)-TMID)-R(J,1))*SINA(J)  
                   IF ( SRIJK . LT . SRMIN ) SRMIN = SRIJK
                   IF ( SRIJK . GT . SRMAX ) SRMAX = SRIJK
   42          CONTINUE
   41     CONTINUE
   40   CONTINUE
C
      ELSE
        SRMAX = ZMAX
        SRMIN = ZMIN
      ENDIF
C
C
      RETURN
C
  999 STOP
C
4000  FORMAT(10I5)
4001  FORMAT(A80)
4002  FORMAT(3F10.4)
4003  FORMAT(5F10.4,/,(10F10.5))
C
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
C
      SUBROUTINE SUB1(XOC1,XOC2,NXOC,K)
C
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO CALCULATE XOC VALUES                       C
C                                                                  C
C         NOTE : This routine assumes that SUCTION surface is      C
C                the upper surface of the blade. Thus, the tangent C
C                chord line is "fitted" to the lower i.e. PRESSURE C
C                surface. Since this routine uses SM and RT to set C
C                X/C, it is only correct for CONICAL & CYLINDRICAL C
C                quasi-stream surfaces.                            C
C                                                                  C
C******************************************************************C

      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION XOC1(2500),XOC2(2500)
C
      CHARACTER*32 NXOC
C
      NXOC = '       FRACTION OF CHORD        '
C
      JTEM1 = JTE - 1
      JLEP1 = JLE + 1
C
      RAD   = 3.14159/180.
      DEG   = 180./3.14159
C
C     SET UP FOR PLOTTING X/C
C
C
C          Find stagger
C
      GMIN = ATAN( (R(JTEM1,K)*T(IM,JTEM1,K)-R(JTE,K)*T(IM,JTE,K))/
     &             (SM(JTEM1,K)-SM(JTE,K))  )
      GMAX = GMIN
C
      DO 5 J=JLE,JTEM1
      GAMMA = ATAN( (R(J,K)*T(IM,J,K)-R(JTE,K)*T(IM,JTE,K))/
     &              (SM(J,K)-SM(JTE,K))  )
      IF (GAMMA.LT.GMIN) GMIN = GAMMA
      IF (GAMMA.GT.GMAX) GMAX = GAMMA
    5 CONTINUE
C
      IF (GMIN.LT.0.) GAMMA = GMAX*DEG
      IF (GMAX.GT.0.) GAMMA = GMIN*DEG
C
C         ROTATE THE COORDS
C
      DO 10 J=JLE,JTE
      YSS = R(J,K)*T(1,J,K)
      XXS = SM(J,K)*COS((GAMMA)*RAD) + YSS*SIN((GAMMA)*RAD)
      YYS =-SM(J,K)*SIN((GAMMA)*RAD) + YSS*COS((GAMMA)*RAD)
C
      YPS = R(J,K)*(T(IM,J,K)-PITCH(J,K))
      XXP = SM(J,K)*COS((GAMMA)*RAD) + YPS*SIN((GAMMA)*RAD)
      YYP =-SM(J,K)*SIN((GAMMA)*RAD) + YPS*COS((GAMMA)*RAD)
C
      XOC1(J-JLE+1) = XXS
      XOC2(J-JLE+1) = XXP
   10 CONTINUE
C
C       SET SCALES
C
      XMAX = XOC1(1)
      XMIN = XOC1(1)
C
      NPTS = JTE-JLE + 1
C
      DO 20 J=1,NPTS
      IF (XOC1(J).LT.XMIN) XMIN = XOC1(J)
      IF (XOC2(J).LT.XMIN) XMIN = XOC2(J)
      IF (XOC1(J).GT.XMAX) XMAX = XOC1(J)
      IF (XOC2(J).GT.XMAX) XMAX = XOC2(J)
   20 CONTINUE
C
      DO 30 J=1,NPTS
      XOC1(J) = (XOC1(J)-XMIN)/(XMAX-XMIN)
      XOC2(J) = (XOC2(J)-XMIN)/(XMAX-XMIN)
   30 CONTINUE
C
      RETURN
C
      END
C
C******************************************************************C
C
C          SUBROUTINE TO CALCULATE X/CX VALIUES
C
C******************************************************************C
C
      SUBROUTINE SUB2(XOCX1,XOCX2,NXOCX,K)
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION XOCX1(2500),XOCX2(2500)
C
      CHARACTER*32 NXOCX
C
      NXOCX = '    FRACTION OF AXIAL CHORD     '
C
      JTEM1 = JTE - 1
      JLEP1 = JLE + 1
C
      NPTS  = JTE - JLE + 1
C
C         STORE THE X-COORDS
C
      DO 110 N=1,NPTS
      J = N + JLE-1
      XOCX1(N) = X(J,K)
      XOCX2(N) = X(J,K)
  110 CONTINUE
C
C       SET SCALES
C
      XMAX = XOCX1(1)
      XMIN = XOCX1(1)
C
      DO 120 N=1,NPTS
      IF (XOCX1(N).LT.XMIN) XMIN = XOCX1(N)
      IF (XOCX2(N).LT.XMIN) XMIN = XOCX2(N)
      IF (XOCX1(N).GT.XMAX) XMAX = XOCX1(N)
      IF (XOCX2(N).GT.XMAX) XMAX = XOCX2(N)
  120 CONTINUE
C
      DO 130 N=1,NPTS
      XOCX1(N) = (XOCX1(N)-XMIN)/(XMAX-XMIN)
      XOCX2(N) = (XOCX2(N)-XMIN)/(XMAX-XMIN)
  130 CONTINUE
C
      RETURN
C
      END
C
C******************************************************************C
C                                                                  C
C            SUBROUTINE TO CALCULATE S/S0
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB3(SOS01,SOS02,NSOS0,K)
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION SOS01(2500),SOS02(2500)
C
      CHARACTER*32 NSOS0
C
      NSOS0 = '   FRACTION OF SURFACE LENGTH   '
C
      JTEM1 = JTE - 1
      JLEP1 = JLE + 1
C
      NPTS  = JTE - JLE + 1
C
      RAD   = 3.14159/180.
      DEG   = 180./3.14159
C
      SOS01(1) = 0.0
      SOS02(1) = 0.0
C
      DO 210 N=2,NPTS
      J   = N + JLE - 1
      SSS = SQRT((SM(J,K)-SM(J-1,K))**2+
     &           (R(J,K)*T(1,J,K)-R(J,K)*T(1,J-1,K))**2)
      SPS = SQRT((SM(J,K)-SM(J-1,K))**2+
     &           (R(J,K)*T(IM,J,K)-R(J,K)*T(IM,J-1,K))**2)
C
      SOS01(N) = SOS01(N-1) +SSS
      SOS02(N) = SOS02(N-1) +SPS
  210 CONTINUE
C
C           SET SCALES
C
      SMAX = SOS01(NPTS)
      SMIN = SOS01(1)
      PMAX = SOS02(NPTS)
      PMIN = SOS02(1)
C
      DO 230 N=1,NPTS
      SOS01(N) = (SOS01(N)-SMIN)/(SMAX-SMIN)
      SOS02(N) = (SOS02(N)-PMIN)/(PMAX-PMIN)
  230 CONTINUE
C
      RETURN
C
      END
C
C*******************************************************************C
C
C            SUBROUTINE TO INPUT FLOW VARIABLES
C
C******************************************************************C
C
      SUBROUTINE SUB4
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK9 / P(95,2500,95)
      COMMON / BLK10/ RO(95,2500,95)
      COMMON / BLK11/ M_ABS(95,2500,95),M_REL(95,2500,95)
      COMMON / BLK14/ Q(95,2500,95),QQ(95,2500,95),NQ,NQQ
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      COMMON /BLKPRIM/ ROVX(95,2500,95),ROVR(95,2500,95),
     &                 ROVT(95,2500,95),ROE(95,2500,95),
     &                 ENTPY(95,2500,95),GA_PV(95,2500,95),
     &                 H_TO_T(95,2500,95),WET(95,2500,95),
     &                 P_STAGN(95,2500,95),T_STAGN(95,2500,95),
     &                 T_STATIC(95,2500,95),PO_REL(95,2500,95),
     &                 HO(95,2500,95)
C
      REAL        M_ABS,  M_REL
C
      CHARACTER*32 NQ,NQQ
      CHARACTER*32 NAME
      CHARACTER*72 JBNAME,TSNUM
C
C*****************************************************************************
C*****************************************************************************
C      READ IN THE PLOTTING/RESTART FILE 'flow_out' 
C
      READ(7)  NSTEP   
      WRITE(6,*)  ' READ NSTEP'  
      WRITE(6,*)  ' NSTEP =', NSTEP
      READ(7) ((( RO(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN RO'  
      READ(7) ((( ROVX(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN ROVX' 
      READ(7) ((( ROVR(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN ROVR'  
      READ(7) ((( ROVT(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN ROVT' 
      READ(7) ((( ROE(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN ROE'  
      READ(7) ((( ROSUB,I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN ROSUB'
      READ(7) (((  P(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN PRESSURE '      
      READ(7) ((( ENTPY(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN ENTROPY' 
C
C    EXTRA DATA FOR STEAM PROPERTIES.
C
      READ(7) ((( GA_PV(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN GA_PV '
      READ(7) ((( WET(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ IN WETNESS '
      READ(7) ((( H_TO_T(I,J,K),I=1,IM),J=1,JM),K=1,KM)
      WRITE(6,*) ' READ  IN H TO T RATIO '
C
C*****************************************************************************
C*****************************************************************************
      TSNUM  = ' NSTEP '
      CV    = CP/GA
      RGAS  = CP - CV
      DO 100 K=1,KM
      DO 100 J=1,JM
      DO 100 I=1,IM
      ROSTAT = RO(I,J,K)
      PSTAT  = P(I,J,K)    
      VX(I,J,K) = ROVX(I,J,K)/ROSTAT
      VT(I,J,K) = ROVT(I,J,K)/ROSTAT - W(J)*R(J,K)
      VR(I,J,K) = ROVR(I,J,K)/ROSTAT
      EINT      =  ROE(I,J,K)/ROSTAT
      VTABS     =  ROVT(I,J,K)/ROSTAT
      WTREL     =  VT(I,J,K)
      VMSQ      =  VX(I,J,K)*VX(I,J,K) + VR(I,J,K)*VR(I,J,K)
      EKE       =  VMSQ + VTABS*VTABS 
      EKEREL    =  VMSQ + WTREL*WTREL 
      UINT      =  EINT  - 0.5*EKE 
      HSTAT     =  UINT  + PSTAT/ROSTAT
      HSTAG     =  HSTAT + 0.5*EKE
      HOREL     =  HSTAT + 0.5*EKEREL
      TSTAT     =  HSTAT*H_TO_T(I,J,K)
      T_STATIC(I,J,K) = TSTAT
C      TSTAG     =  HSTAG*H_TO_T(I,J,K)
C      T_STAGN(I,J,K)  = TSTAG
      VSND_SQ      =  GA_PV(I,J,K)*PSTAT/ROSTAT
      M_ABS(I,J,K) = SQRT(EKE/VSND_SQ)
      M_REL(I,J,K) = SQRT(EKEREL/VSND_SQ)
      RPH_EXP      = (ROSTAT*HSTAT)/PSTAT
      P_STAGN(I,J,K) = PSTAT*(HSTAG/HSTAT)**RPH_EXP
      PO_REL(I,J,K)  = PSTAT*(HOREL/HSTAT)**RPH_EXP
      HO(I,J,K)      = HSTAG
      RGAS    = PSTAT/(ROSTAT*TSTAT)
      ROSTAG  = ROSTAT*(P_STAGN(I,J,K)/PSTAT)**(1.0/GA_PV(I,J,K))
      TSTAG   = P_STAGN(I,J,K)/(RGAS*ROSTAG)
      T_STAGN(I,J,K)  = TSTAG
  100 CONTINUE
C
      RETURN
C
      END
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO PLOT CONTOURS
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB5(PROP,NAME,X0,Y0,X1,Y1,IFP,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK19/ XX(95,2500),YY(95,2500),ZZ(95,2500),
     &                WORK1(95,2500),WORK2(95,2500)
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      CHARACTER*72 JBNAME,TSNUM
      CHARACTER*32 NAME
      CHARACTER*8  IFP
      CHARACTER*1  CANS,BANS
C
      DIMENSION PROP(95,2500,95),HITES(125),STOREX(2500,95),
     &          STORER(2500,95),ZZZ(95,2500),HITES2(10)
C
C
      REAL EMACH,IMACH
C
C
      LOGICAL      FAIL
      REAL         ABXLIM ( 4 ), ABYLIM ( 4 )
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT1 /  DELR, ZSCALE, VMOD,
     &                 NDIFF, I, J, K, IZ, JZ, KZ, IPIT, CTYPE, ANS
C
C     DATA STATEMENT
C
      NROW = (95)
C
C
C         TELL USER OF STATUS
C
      WRITE (6,2000) JBNAME,NAME
C
C        NOW SET SCALE FACTOR "R"
C
      RX  =  (X1-X0)/( XMAX- XMIN)
      RY  =  (Y1-Y0)/( YMAX- YMIN)
      RZ  =  (X1-X0)/( ZMAX- ZMIN)
      RA  =  (X1-X0)/( RMAX- RMIN)
      RB  =  (Y1-Y0)/( RMAX- RMIN)
      RS  =  (X1-X0)/(SMMAX-SMMIN)
      RT  =  (Y1-Y0)/(RTMAX-RTMIN)
      RR  =  (X1-X0)/(SRMAX-SRMIN)
      RR  = AMIN1 ( RX, RY, RZ, RA, RB, RS, RT, RR )
C
C         DETERMINE TYPE OF PLOT REQUIRED
C
   55 WRITE (6,2020) ANS
      READ  (1,1000) CANS
      IF ( CANS . NE . ' ' ) ANS = CANS
C
C         SET UP NEW ARRAY FOR CONTOURING
C
C         TYPE OF PLOT     1ST ELEMENT     2ND ELEMENT OF ARRAY
C         ------------     -----------     --------------------
C        'T', 'S' OR 'R'       I                    J
C              'M'             K                    J
C          'Q' OR 'X'          I                    K
C
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' . OR . 
     &     ANS . EQ . 'S' . OR . ANS . EQ . 's' . OR . 
     &     ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
   13     WRITE (6,2021) KM, K
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) K = IANS
          IF (K.GT.KM . OR . K.LT.1 ) GO TO 13
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' . OR .
     &          ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
C
   23     WRITE (6,2022) JM, J
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) J = IANS
          IF (J.GT.JM . OR . J.LT.1 ) GO TO 23
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
   33     WRITE (6,2023) IM, I
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) I = IANS
          IF (I.GT.IM . OR . I.LT.1 ) GO TO 33
C
      ELSE
C
          GO TO 55
      ENDIF
C
C          ESTABLISH CONTOUR HITES
C
      NHTS   = 15
C
      CALL SUB6 (PROP,GMAX,GMIN)
      CALL SUB6A(PROP,PMAX,PMIN,I,J,K,ANS)
C
      RANGE  = PMAX-PMIN
      DELR   = RANGE/FLOAT(NHTS)
C
C
   34 WRITE (6,2001) PMIN,GMIN,PMAX,GMAX,NHTS,DELR
      RANS = RREAD(1)
      IF ( RANS.GT.0.0 ) DELR = RANS
C
      MIN    = IFIX(PMIN/DELR+0.5)-1
      MAX    = IFIX(PMAX/DELR+0.5)+1
C
      NHTS = MAX - MIN + 1
      IF ( NHTS.GT.125 ) GO TO 34
C
      DO 40 M=MIN,MAX
      HITES(M+1-MIN) = FLOAT(M)*DELR
   40 CONTINUE
C
C      WRITE(6,*) ' INPUT NHEIGTS-2 , MAXHTS2'
C      READ(5,*) NHTS2,HTS2
      NHTS2 = 10
      HTS2  = 8.0
      DO 41 M = 1,NHTS2
      HITES2(M) = HTS2*FLOAT(M-1)/(NHTS2 -1)
   41 CONTINUE

C
C      Zoom ?
C
      WRITE (6,2024) ZLAST
      ZIN = RREAD(1)
      IF (ZIN.GT.0.01) THEN
          ZSCALE = ZIN
          ZLAST  = ZSCALE
      ELSE 
          ZSCALE = ZLAST
      ENDIF
C
      IF(ZIN.GT.0.01) THEN
C
   35     WRITE (6,2025) IM, IZ
          IANS = IREAD(1)
          IF (IANS.NE.0) IZ = IANS
          IF (IZ.LT.1.OR.IZ.GT.IM) GO TO 35
C
   36     WRITE (6,2026) JM, JZ
          IANS = IREAD(1)
          IF (IANS.NE.0) JZ = IANS
          IF (JZ.LT.1.OR.JZ.GT.JM) GO TO 36
C
   37     WRITE (6,2027) KM, KZ
          IANS = IREAD(1)
          IF (IANS.NE.0) KZ = IANS
          IF (KZ.LT.1.OR.KZ.GT.KM) GO TO 37
C
      ENDIF
C
C         DETERMINE TYPE OF CONTOURS REQUIRED
C
      CTYPE = 'N'
      WRITE (6,2060) CTYPE
      READ  (1,1000) CANS
      IF ( CANS . NE . ' ' ) CTYPE = CANS
C
C         Find number of pitches to be plotted
C
      WRITE (6,2061) IPIT
      IANS = IREAD(1)
      IF ( IANS . NE . 0 ) IPIT = IANS
      IF ( IPIT . LE . 0 ) IPIT = 0
C
          IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          ROTN = 0.0
          WRITE(6,*) ' INPUT THE ANGLE OF CLOCKWISE ROTATION  IN DEG'
          RANS = RREAD(1)
          IF(ABS(RANS).GT.0.0001) ROTN = RANS
          ROTN = ROTN*3.14159/180.
          WRITE(6,*) 'CONTOUR ALL MERIDIONAL SURFACE OR ONLY THE BLADE
     &    ANSWER A or B'
          READ(1,1000) BANS         
          ENDIF
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C        TEST TO SEE IF SHADING IS AVALIABLE
C
      CALL BOXFIL ( 0., 0., 0., 0., 0, FAIL )
C
      IF ( FAIL ) CTYPE = 'N'
C
C         FIND THE ABSOLUTE SIZE OF THE PLOT BEFORE ANYTHING ELSE
C
      CALL FIND ( ABXLIM, ABYLIM, 7 )
C
C         SET WINDOW
C
      CALL AWINDW ( X0, Y0, X1, Y1, -1 )
      CALL ORIGIN( 0.5*(X0+X1), 0.5*(Y0+Y1), 1 )
      CALL SCALE ( RR, RR )
C
C        SHIFT ORIGIN TO CENTRE PLOT
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
          CALL ORIGIN( -0.5*(XMIN+XMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( X(JZ,KZ), R(JZ,KZ)*(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          CALL ORIGIN( -0.5*(ZMIN+ZMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL ORIGIN( -0.5*(SMMIN+SMMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( SM(JZ,KZ), R(JZ,KZ)*(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL ORIGIN( -0.5*(SRMIN+SRMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( (X(JZ,KZ)-X(JZ,1))*COSA(JZ)
     &              +(R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID)-R(JZ,1))*SINA(JZ),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
          CALL ORIGIN( -0.5*(ZMIN+ZMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL ORIGIN( -0.5*(XMIN+XMAX), -0.5*(RMIN+RMAX), 0 )
          CALL ZOOM( X(JZ,KZ), R(JZ,KZ),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ENDIF
C
C         SET UP NEW ARRAY FOR CONTOURING
C
C         TYPE OF PLOT     1ST ELEMENT     2ND ELEMENT OF ARRAY
C         ------------     -----------     --------------------
C          'S' OR 'R'          I                    J
C              'M'             K                    J
C          'Q' OR 'X'          I                    K
C
C
      CALL SCLCHR ( 0.8, 0.8 )
C
C     Loop over several pitches
C
      DO 300 NPIT = -IPIT,IPIT,1
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
C
          DO 10 I=1,ISPLIT
              DO 11 J=1,JM
                  XX(I,J) = X(J,K)
                  YY(I,J) = R(J,K)
     &                      *(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                  ZZ(I,J) = PROP(I,J,K)
   11         CONTINUE
   10     CONTINUE
C
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C 
          IF ( ISPLIT . NE . IM ) THEN
C
              DO 110 I=ISPLIT+1,IM
                  DO 111 J=1,JM
                      XX(I-ISPLIT,J) = X(J,K)
                      YY(I-ISPLIT,J) = R(J,K)
     &                            *(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      ZZ(I-ISPLIT,J) = PROP(I,J,K)
  111             CONTINUE
  110         CONTINUE
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
              ELSE
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,100,WORK1,WORK2)
              ENDIF
          ENDIF
C
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
          DO 14 I=1,ISPLIT
              DO 15 J=1,JM
                  XX(I,J) = R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                  YY(I,J) = R(J,K)*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                  ZZ(I,J) = PROP(I,J,K)
   15         CONTINUE
   14     CONTINUE
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C
          IF ( ISPLIT . NE . IM ) THEN
C
              DO 114 I=ISPLIT+1,IM
                  DO 115 J=1,JM
                      XX(I-ISPLIT,J) = R(J,K)
     &                             *COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      YY(I-ISPLIT,J) = R(J,K)
     &                             *SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      ZZ(I-ISPLIT,J) = PROP(I,J,K)
  115             CONTINUE
  114         CONTINUE
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
              ELSE
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,100,WORK1,WORK2)
              ENDIF
          ENDIF
C
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
C
          DO 16 I=1,ISPLIT
              DO 17 J=1,JM
                  XX(I,J) = SM(J,K)
                  YY(I,J) = R(J,K)*(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                  ZZ(I,J) = PROP(I,J,K)
   17         CONTINUE
   16     CONTINUE
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE  
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C
          IF ( ISPLIT . NE . IM ) THEN
C
              DO 116 I=ISPLIT+1,IM
                  DO 117 J=1,JM
                      XX(I-ISPLIT,J) = SM(J,K)
                      YY(I-ISPLIT,J) = R(J,K)
     &                                 *(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      ZZ(I-ISPLIT,J) = PROP(I,J,K)
  117             CONTINUE
  116         CONTINUE
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
              ELSE
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,100,WORK1,WORK2)
              ENDIF
          ENDIF
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
          DO 20 K=1,KM
              DO 21 I=1,ISPLIT
                  XX(I,K) = (X(J,K)-X(J,1))*COSA(J)
     &                     +(R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                     -R(J,1))*SINA(J)
                  YY(I,K) = R(J,K)*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                  ZZ(I,K) = PROP(I,J,K)
   21         CONTINUE
   20     CONTINUE
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,KM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE 
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,KM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C
          IF ( ISPLIT . NE . IM ) THEN
C
              DO 220 K=1,KM
                  DO 221 I=ISPLIT+1,IM
                      XX(I-ISPLIT,K) = (X(J,K)-X(J,1))*COSA(J)
     &                   +(R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                   -R(J,1))*SINA(J)
                      YY(I-ISPLIT,K) = R(J,K)
     &                   *SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      ZZ(I-ISPLIT,K) = PROP(I,J,K)
  221             CONTINUE
  220         CONTINUE
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),KM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
              ELSE 
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),KM,
     &                             HITES,NHTS,100,WORK1,WORK2)
              ENDIF
          ENDIF
C
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
C
          DO 25 K=1,KM
              DO 26 I=1,ISPL    
              XX(I,K) = R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
              YY(I,K) = R(J,K)*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
              ZZ(I,K) = PROP(I,J,K)
   26         CONTINUE
   25     CONTINUE
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,KM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE 
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,KM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C
          IF ( ISPLIT . NE . IM ) THEN
C
              DO 225 K=1,KM 
                  DO 226 I=ISPLIT+1,IM
                      XX(I-ISPLIT,K) = R(J,K)
     &                            *COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      YY(I-ISPLIT,K) = R(J,K)
     &                            *SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      ZZ(I-ISPLIT,K) = PROP(I,J,K)
  226             CONTINUE
  225         CONTINUE
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),KM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
              ELSE 
                  CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),KM,
     &                             HITES,NHTS,100,WORK1,WORK2)
              ENDIF
          ENDIF
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
          IF ( NPIT . EQ . 0 ) THEN
C    JDD
          XMIDD = 0.5*(X(JLE,KM)   + X(JTE,KM))
          TMIDD = 0.5*(T(1,JLE,KM) + T(1,JTE,KM))
C              
C    JDD
C
          J1 = 1
          J2 =JM
          IF(BANS.EQ.'B'.OR.BANS.EQ.'b') THEN
          J1 = JLE
          J2 = JTE
          ENDIF
C************************************************************************
C   SET THE CONTOUR LEVELS TO COLOUR THE BLADE '
C
              DO 30 K=1,KM
                  F_IND = 10.0
                  DO 31 JJ=J1,J2
                  J = JJ+1-J1
                      STOREX(J,K) = X(J,K)
                      STORER(J,K) = R(J,K)
                      XX(K,J)     = X(JJ,K)
                      YY(K,J)     = R(JJ,K)
                      ZZ(K,J)  = PROP(I,JJ,K)
                      IF(IND(JJ).EQ.1.AND.F_IND.GT.9.9)THEN
                      F_IND = 7.0
                      GO TO 337
                      END IF
                      IF(IND(JJ).EQ.1.AND.F_IND.LT.10.0) F_IND = 10.0
  337                 CONTINUE
                      ZZZ(K,J) = F_IND
C
C*************************************************************************
      IF(ABS(ROTN).GT.0.001) THEN
                      YY(K,J) = R(JJ,K)*COS(T(I,JJ,K) - TMIDD)
                      RTDIF = R(JJ,K)*SIN(T(I,JJ,K) - TMIDD)
                      XDIF  = X(JJ,K) - XMIDD
                      ANG   = ATAN2(RTDIF,XDIF)
                      DIST  = SQRT(XDIF*XDIF + RTDIF*RTDIF)
                      ANEW  = ANG-ROTN
                      XX(K,J) = XMIDD + DIST*COS(ANEW)
       ENDIF
                      X(J,K)  = XX(K,J)
                      R(J,K)  = YY(K,J)
C    JDD
C
   31             CONTINUE
   30         CONTINUE
C  JDD PUT JC in the call to CONTOR
               JC = J2 -J1 + 1
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                  CALL contor(XX,YY,ZZ,NROW,KM,JC,HITES,
     &                                     NHTS,-8,WORK1,WORK2)
                  CALL sclchr(.8,.8)
                  CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
                  CALL sclchr(1.25,1.25)
C                
              ELSE
C     JDD ADDED TO COLOUR THE BLADE ROWS
                  CALL contor(XX,YY,ZZZ,NROW,KM,JC,HITES2,
     &                                     NHTS2,-8,WORK1,WORK2) 
C    END JDD ADDITION
                  CALL contor(XX,YY,ZZ,NROW,KM,JC,HITES,
     &                                     NHTS,100,WORK1,WORK2)
C

              ENDIF
          ENDIF
      ENDIF
C
  300 CONTINUE
C
      CALL SCLCHR ( 1./0.8, 1./0.8 )
C
C   JDD
C
      IF ((ANS.EQ. 'M' .OR.ANS.EQ. 'm' ).AND.(BANS.EQ.'B'.
     &                                     OR.BANS.EQ.'b')) THEN
      JLESTORE = JLE
      JTESTORE = JTE
      JMSTORE  = JM
      JLE = 1
      JTE = JC
      JM  = JC
      ENDIF
C
C    JDD
C
C      DRAW BLADE
C
      CALL SUB7
      CALL SUB28
C
C     JDD
C
      IF (ANS.EQ. 'M' .OR.ANS.EQ. 'm' ) THEN
       DO 1234 K=1,KM
       DO 1234 J=1,JC
       R(J,K) = STORER(J,K)
 1234  X(J,K) = STOREX(J,K)
        IF(BANS.EQ.'B'.OR.BANS.EQ.'b') THEN
         JLE = JLESTORE
         JTE = JTESTORE
         JM  = JMSTORE
        ENDIF
      ENDIF
C
C  END JDD
C
C      TURN WINDOW BACK ON FOR ADDING TITLES
C
      CALL AWINDW ( X0, Y0, X1, Y1, 0 )
C
C      Blank the area for labelling in case something written here by shading
C
      CALL CTUSER ( ABXLIM ( 1 ), ABYLIM ( 1 ), X0F, Y0F )
      CALL CTUSER ( ABXLIM ( 4 ), Y0,           X1F, Y1F )
C
      CALL BOXFIL ( X0F, Y0F, X1F, Y1F, 0, FAIL )
C
C      ADD TITLES
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
      XX6 = 175.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 15.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
      CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
C
C
      S = (XX1-XX0)/80./3.0
      CALL SCLCHR( S, S )
      CALL BGNWRT(XX6,YY2,1)
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
          CALL WRITEA('*','(A30)',' RADIAL PROJECTION OF S-S. No.')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          CALL WRITEA('*','(A30)','  AXIAL PROJECTION OF S-S. No.')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL WRITEA('*','(A30)','         STREAM SURFACE NUMBER')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
          CALL WRITEA('*','(A30)','  AXIAL PROJECTION OF Q-O. No.')
          CALL WRITEI('*','(I4)',J)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL WRITEA('*','(A30)','       QUASI-ORTHOGONAL NUMBER')
          CALL WRITEI('*','(I4)',J)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL WRITEA('*','(A30)','     MERIDIONAL SURFACE NUMBER')
          CALL WRITEI('*','(I4)',I)
      ENDIF
C
      CALL WRITEA(' ','(A20)',' LOCAL/GLOBAL MAX. =')
      CALL WRITEF('*',IFP,PMAX)
      CALL WRITEC('/')
      CALL WRITEF('*',IFP,GMAX)
      CALL WRITEA(' ','(A20)',' LOCAL/GLOBAL MIN. =')
      CALL WRITEF('*',IFP,PMIN)
      CALL WRITEC('/')
      CALL WRITEF('*',IFP,GMIN)
      CALL WRITEA(' ','(A20)','         INCREMENT =')
      CALL WRITEF('*',IFP,DELR)
      CALL WRITEA(' ','(A6)','ZOOM =')
      CALL WRITEF('*','(F6.1)',ZSCALE)
      CALL WRITEA('*','(A7)',' AT I =')
      CALL WRITEI('*','(I4)',IZ)
      CALL WRITEA('*','(A4)',' J =')
      CALL WRITEI('*','(I4)',JZ)
      CALL WRITEA('*','(A4)',' K =')
      CALL WRITEI('*','(I4)',KZ)
      CALL SCLCHR ( 1.0/1.0, 1.0/1.0 )
C
C
C      WRITE(6,*) ' INPUT B TO CALL BREAKPLOT '
C      READ(5,*)  BABS
C      IF(BANS.EQ.'B'.OR.BANS.EQ.'b')
       CALL BRKPLT
C
C
C     Make sure no bad defaults
C
      I = MAX0 ( MIN0 ( IM, I ), 1 )
      J = MAX0 ( MIN0 ( JM, J ), 1 )
      K = MAX0 ( MIN0 ( KM, K ), 1 )
C
      RETURN
C
C
1000  FORMAT(A1)
C
C
C
2000  FORMAT(' '//' ',A80//' ',A32//' CONTOURING SELECTED '/)
2001  FORMAT(/' LOCAL MINIMUM VALUE ............',F13.4/
     &        ' GLOBAL MINIMUM VALUE ...........',F13.4/
     &        ' LOCAL MAXIMUM VALUE ............',F13.4/
     &        ' GLOBAL MAXIMUM VALUE ...........',F13.4/
     &        ' NUMBER OF CONTOURS .......',I8,' (Max = 125)'/
     &        ' CONTOUR INTERVAL .........',F13.4,' <=== DEFAULT'/
     &        ' ENTER REQUIRED INTERVAL ---> '/)
2020  FORMAT('    TYPE :-'//
     &       '   "M"  for Meridional Plot'/
     &       '   "Q"  for Quasi-Orthogonal Plot'/
     &       '   "R"  for Radial projection of Quasi Stream-surface'/
     &       '   "S"  for Quasi Stream-surface'/
     &       '   "T"  for Axial projection of Quasi Stream-surface'/
     &       '   "X"  for Axial projection of Quasi-Orthogonal'//
     &       '   "',A1,'" <=== DEFAULT'/)
2021  FORMAT(//'  ENTER integer value (K) of quasi-stream surface for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2022  FORMAT(//'  ENTER integer value (J) of quasi-orthogonal for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2023  FORMAT(//'  ENTER integer value (I) of quasi-meridional for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2024  FORMAT(//'  ENTER Zoom magnification that you require'/
     +         '        DEFAULT ======>',F5.1/)
2025  FORMAT(//'  ENTER integer value (I) of pitchwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2026  FORMAT(//'  ENTER integer value (J) of streamwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2027  FORMAT(//'  ENTER integer value (K) of quasi-stream surface on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2060  FORMAT(//'  ENTER "Y" if you want filled in contours'/
     +         '        "N" if you do not '/
     +         '        DEFAULT ======> ',A1/)
2061  FORMAT(//'  ENTER integer value for number of EXTRA pitches'/
     +         '        you wish to be plotted on each side of '/
     +         '        the actual computational domain.'//
     +         '        Enter a negative value to reset to zero'//
     +         '        DEFAULT ======> ',I4/)
      END
C******************************************************************C
C                                                                  C
C           SUBROUTINE TO FIND MAX & MIN OF 3-D ARRAY
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB6(A,AMAX,AMIN)
C
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION A(95,2500,95)
C
      AMAX = A(1,1,1)
      AMIN = AMAX
C
      DO 1 K=1,KM
      DO 2 J=1,JM
      DO 3 I=1,IM
      AIJ = A(I,J,K)
      IF (AIJ.GT.AMAX) AMAX = AIJ
      IF (AIJ.LT.AMIN) AMIN = AIJ
    3 CONTINUE
    2 CONTINUE
    1 CONTINUE
C
      RETURN
      END
C******************************************************************C
C                                                                  C
C           SUBROUTINE TO FIND MAX & MIN OF a plane in a 3-D ARRAY
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB6A(A,AMAX,AMIN,II,JJ,KK,ANS)
C
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION A(95,2500,95)
C
      CHARACTER*1 ANS,REPLY
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' . OR .
     &     ANS . EQ . 'S' . OR . ANS . EQ . 's' . OR .
     &     ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          AMAX = A(1,1,KK)
          AMIN = AMAX
C
          DO 2 J=1,JM
          DO 3 I=1,IM
              AIJ = A(I,J,KK)
              IF (AIJ.GT.AMAX) AMAX = AIJ
              IF (AIJ.LT.AMIN) AMIN = AIJ 
    3     CONTINUE
    2     CONTINUE
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' . OR .
     &          ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          AMAX = A(1,JJ,1)
          AMIN = AMAX
C
          DO 12 K=1,KM
          DO 13 I=1,IM
              AIJ = A(I,JJ,K)
              IF (AIJ.GT.AMAX) AMAX = AIJ
              IF (AIJ.LT.AMIN) AMIN = AIJ 
   13     CONTINUE
   12     CONTINUE
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          AMAX = A(II,1,1)
          AMIN = AMAX
C
          DO 22 K=1,KM
          DO 23 J=1,JM
              AIJ = A(II,J,K)
              IF (AIJ.GT.AMAX) AMAX = AIJ
              IF (AIJ.LT.AMIN) AMIN = AIJ 
   23     CONTINUE
   22     CONTINUE
      ENDIF
C
      IF(ICALL.EQ.1) GO TO 999
C
      WRITE(6,*) 'THE MAX AND MIN VALUES ON THE 2D SURFACE ARE ',
     & AMAX,AMIN
      WRITE(6,*) ' DO YOU WANT TO CHANGE THESE, ANSWER  Y  or N  '
      WRITE(6,*) ' DEFAULT = N '
      READ(1,111)  REPLY
  111 FORMAT(A1)
      IF(REPLY.EQ.'Y'.OR.REPLY.EQ.'y') THEN
      WRITE(6,*) ' INPUT NEW PMAX,PMIN '
      READ(1,*) AMAX,AMIN
      ELSE
      ENDIF
C
  999 CONTINUE
C
      RETURN
      END
C******************************************************************C
C                                                                  C
C           SUBROUTINE TO DRAW BLADE                               C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB7
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT1 /  DELR, ZSCALE, VMOD,
     &                 NDIFF, I, J, K, IZ, JZ, KZ, IPIT, CTYPE, ANS
C
C
C     Loop over pitches
C
      DO 300 NPIT = -IPIT,IPIT,1
C
C
C        DRAW BLADE
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
C
C         Draw lower blade
C
          CALL MOVETO(X(JLE,K),
     &      R(JLE,K)*(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 160 J=JLE,JTE
              CALL DRAWTO(X(J,K),R(J,K)*(T(1,J,K)-TMID+NPIT*PITCH(J,K)))
  160     CONTINUE
          CALL DRAWTO(X(JTE,K),
     &      R(JTE,K)*(T(IM,JTE,K)-PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)))
C
          CALL MOVETO(X(JLE,K),
     &      R(JLE,K)*(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 161 J=JLE,JTE
              CALL DRAWTO(X(J,K),
     &            R(J,K)*(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  161     CONTINUE
C
C         Draw upper blade
C
          CALL MOVETO(X(JLE,K),
     &      R(JLE,K)*(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 162 J=JLE,JTE
              CALL DRAWTO(X(J,K),
     &                    R(J,K)*(T(IM,J,K)-TMID+NPIT*PITCH(J,K)))
  162     CONTINUE
          CALL DRAWTO(X(JTE,K),
     &      R(JTE,K)*(T(1,JTE,K)+PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)))
C
          CALL MOVETO(X(JLE,K),
     &      R(JLE,K)*(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 163 J=JLE,JTE
              CALL DRAWTO(X(J,K),
     &            R(J,K)*(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  163     CONTINUE
C
C
C
C        DRAW BLADE
C
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
C         Draw lower blade
C
          CALL MOVETO(
     &    R(JLE,K)*COS(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)),
     &    R(JLE,K)*SIN(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 170 J=JLE,JTE
              CALL DRAWTO(R(J,K)*COS(T(1,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(1,J,K)-TMID+NPIT*PITCH(J,K)))
  170     CONTINUE
          CALL DRAWTO(
     &    R(JTE,K)*COS(T(IM,JTE,K)-PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)),
     &    R(JTE,K)*SIN(T(IM,JTE,K)-PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)))
C
          CALL MOVETO(
     &    R(JLE,K)*COS(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)),
     &    R(JLE,K)*SIN(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 171 J=JLE,JTE
              CALL DRAWTO(
     &            R(J,K)*COS(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  171     CONTINUE
C
C         Draw upper blade
C
          CALL MOVETO(
     &     R(JLE,K)*COS(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)),
     &     R(JLE,K)*SIN(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 172 J=JLE,JTE
              CALL DRAWTO(R(J,K)*COS(T(IM,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(IM,J,K)-TMID+NPIT*PITCH(J,K)))
  172     CONTINUE
          CALL DRAWTO(
     &     R(JTE,K)*COS(T(1,JTE,K)+PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)),
     &     R(JTE,K)*SIN(T(1,JTE,K)+PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)))
C
          CALL MOVETO(
     &     R(JLE,K)*COS(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)),
     &     R(JLE,K)*SIN(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 173 J=JLE,JTE
              CALL DRAWTO(
     &            R(J,K)*COS(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  173     CONTINUE
C
C
C
C        DRAW BLADE
C
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
C
C         Draw lower blade
C
          CALL MOVETO(SM(JLE,K),
     &      R(JLE,K)*(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 180 J=JLE,JTE
              CALL DRAWTO(SM(J,K),
     &                    R(J,K)*(T(1,J,K)-TMID+NPIT*PITCH(J,K)))
  180     CONTINUE
          CALL DRAWTO(SM(JTE,K),
     &      R(JTE,K)*(T(IM,JTE,K)-PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)))
C
          CALL MOVETO(SM(JLE,K),
     &      R(JLE,K)*(T(IM,JLE,K)-PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 181 J=JLE,JTE
              CALL DRAWTO(SM(J,K),
     &            R(J,K)*(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  181     CONTINUE
C
C         Draw upper blade
C
          CALL MOVETO(SM(JLE,K),
     &      R(JLE,K)*(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 182 J=JLE,JTE
              CALL DRAWTO(SM(J,K),
     &                    R(J,K)*(T(IM,J,K)-TMID+NPIT*PITCH(J,K)))
  182     CONTINUE
          CALL DRAWTO(SM(JTE,K),
     &      R(JTE,K)*(T(1,JTE,K)+PITCH(JTE,K)-TMID+NPIT*PITCH(JTE,K)))
C
          CALL MOVETO(SM(JLE,K),
     &      R(JLE,K)*(T(1,JLE,K)+PITCH(JLE,K)-TMID+NPIT*PITCH(JLE,K)))
          DO 183 J=JLE,JTE
              CALL DRAWTO(SM(J,K),
     &            R(J,K)*(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  183     CONTINUE
C
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN  
        IF ( J . GE . JLE . AND . J . LE . JTE ) THEN    
C
C
C         Draw left blade
C
          CALL MOVETO(
     &        R(J,1)*COS(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1)),
     &        R(J,1)*SIN(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 190 K=1,KM
              CALL DRAWTO(
     &            R(J,K)*COS(T(1,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(1,J,K)-TMID+NPIT*PITCH(J,K)))
  190     CONTINUE
          CALL DRAWTO(
     &        R(J,KM)*COS(T(IM,J,KM)-PITCH(J,KM)-TMID+NPIT*PITCH(J,KM)),
     &        R(J,KM)*SIN(T(IM,J,KM)-PITCH(J,KM)-TMID+NPIT*PITCH(J,KM)))
C
          CALL MOVETO(
     &        R(J,1)*COS(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1)),
     &        R(J,1)*SIN(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 191 K=1,KM
             CALL DRAWTO(
     &           R(J,K)*COS(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)),
     &           R(J,K)*SIN(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  191     CONTINUE
C
C         Draw right blade
C
          CALL MOVETO(
     &        R(J,1)*COS(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1)),
     &        R(J,1)*SIN(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 192 K=1,KM
              CALL DRAWTO(
     &            R(J,K)*COS(T(IM,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(IM,J,K)-TMID+NPIT*PITCH(J,K)))
  192     CONTINUE
          CALL DRAWTO(
     &        R(J,KM)*COS(T(1,J,KM)+PITCH(J,KM)-TMID+NPIT*PITCH(J,KM)),
     &        R(J,KM)*SIN(T(1,J,KM)+PITCH(J,KM)-TMID+NPIT*PITCH(J,KM)))
C
          CALL MOVETO(
     &        R(J,1)*COS(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1)),
     &        R(J,1)*SIN(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 193 K=1,KM
             CALL DRAWTO(
     &           R(J,K)*COS(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)),
     &           R(J,K)*SIN(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  193     CONTINUE
C
        ENDIF
C
C       Draw hub & casing
C
        CALL LINTYP ( 6 )
C
        CALL MOVETO(R(J,1)*COS(T(1,J,1)-TMID+NPIT*PITCH(J,1)),
     &              R(J,1)*SIN(T(1,J,1)-TMID+NPIT*PITCH(J,1)))
        DO 194 I=2,IM
            CALL DRAWTO(R(J,1)*COS(T(I,J,1)-TMID+NPIT*PITCH(J,1)),
     &                  R(J,1)*SIN(T(I,J,1)-TMID+NPIT*PITCH(J,1)))
  194   CONTINUE
C
        CALL MOVETO(R(J,KM)*COS(T(1,J,KM)-TMID+NPIT*PITCH(J,KM)),
     &              R(J,KM)*SIN(T(1,J,KM)-TMID+NPIT*PITCH(J,KM)))
        DO 195 I=2,IM
            CALL DRAWTO(R(J,KM)*COS(T(I,J,KM)-TMID+NPIT*PITCH(J,KM)),
     &                  R(J,KM)*SIN(T(I,J,KM)-TMID+NPIT*PITCH(J,KM)))
  195   CONTINUE
C
        CALL LINTYP ( 0 )
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
        IF ( J . GE . JLE . AND . J . LE . JTE ) THEN    
C
C
C         Draw left blade
C
          CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &        +(R(J,1)*COS(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1))
     &         -R(J,1))*SINA(J),
     &        R(J,1)*SIN(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 200 K=1,KM
              CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &            +(R(J,K)*COS(T(1,J,K)-TMID+NPIT*PITCH(J,K))
     &             -R(J,1))*SINA(J),
     &            R(J,K)*SIN(T(1,J,K)-TMID+NPIT*PITCH(J,K)))
  200     CONTINUE
          CALL DRAWTO((X(J,KM)-X(J,1))*COSA(J)
     &       +(R(J,KM)*COS(T(IM,J,KM)-PITCH(J,KM)-TMID+NPIT*PITCH(J,KM))
     &        -R(J,1))*SINA(J),
     &       R(J,KM)*SIN(T(IM,J,KM)-PITCH(J,KM)-TMID+NPIT*PITCH(J,KM)))
C
          CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &        +(R(J,1)*COS(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1))
     &         -R(J,1))*SINA(J),
     &        R(J,1)*SIN(T(IM,J,1)-PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 201 K=1,KM
             CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &           +(R(J,K)*COS(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K))
     &            -R(J,1))*SINA(J),
     &           R(J,K)*SIN(T(IM,J,K)-PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  201     CONTINUE
C
C         Draw right blade
C
          CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &        +(R(J,1)*COS(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1))
     &         -R(J,1))*SINA(J),
     &        R(J,1)*SIN(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 202 K=1,KM
              CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &                    +(R(J,K)*COS(T(IM,J,K)-TMID+NPIT*PITCH(J,K))
     &                     -R(J,1))*SINA(J),
     &                    R(J,K)*SIN(T(IM,J,K)-TMID+NPIT*PITCH(J,K)))
  202     CONTINUE
          CALL DRAWTO((X(J,KM)-X(J,1))*COSA(J)
     &        +(R(J,KM)*COS(T(1,J,KM)+PITCH(J,KM)-TMID+NPIT*PITCH(J,KM))
     &         -R(J,1))*SINA(J),
     &        R(J,KM)*SIN(T(1,J,KM)+PITCH(J,KM)-TMID+NPIT*PITCH(J,KM)))
C
          CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &        +(R(J,1)*COS(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1))
     &         -R(J,1))*SINA(J),
     &        R(J,1)*SIN(T(1,J,1)+PITCH(J,1)-TMID+NPIT*PITCH(J,1)))
          DO 203 K=1,KM
             CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &           +(R(J,K)*COS(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K))
     &            -R(J,1))*SINA(J),
     &           R(J,K)*SIN(T(1,J,K)+PITCH(J,K)-TMID+NPIT*PITCH(J,K)))
  203     CONTINUE
C
        ENDIF
C
C       Draw hub & casing
C
        CALL LINTYP ( 6 )
C
        CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &              +(R(J,1)*COS(T(1,J,1)-TMID+NPIT*PITCH(J,1))
     &               -R(J,1))*SINA(J),
     &              R(J,1)*SIN(T(1,J,1)-TMID+NPIT*PITCH(J,1)))
        DO 204 I=2,IM
            CALL DRAWTO((X(J,1)-X(J,1))*COSA(J)
     &                  +(R(J,1)*COS(T(I,J,1)-TMID+NPIT*PITCH(J,1))
     &                   -R(J,1))*SINA(J),
     &                  R(J,1)*SIN(T(I,J,1)-TMID+NPIT*PITCH(J,1)))
  204   CONTINUE
C
        CALL MOVETO((X(J,KM)-X(J,1))*COSA(J)
     &              +(R(J,KM)*COS(T(1,J,KM)-TMID+NPIT*PITCH(J,KM))
     &               -R(J,1))*SINA(J),
     &              R(J,KM)*SIN(T(1,J,KM)-TMID+NPIT*PITCH(J,KM)))
        DO 205 I=2,IM
            CALL DRAWTO((X(J,KM)-X(J,1))*COSA(J)
     &                  +(R(J,KM)*COS(T(I,J,KM)-TMID+NPIT*PITCH(J,KM))
     &                   -R(J,1))*SINA(J),
     &                  R(J,KM)*SIN(T(I,J,KM)-TMID+NPIT*PITCH(J,KM)))
  205   CONTINUE
C
        CALL LINTYP ( 0 )
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
          IF ( NPIT . EQ . 0 ) THEN
C
C             Draw hub & shroud lines
C
              CALL LINTYP ( 6 )
C 
              CALL MOVETO(X(1,1),R(1,1))
C
              DO 210 J=2,JM
                  CALL DRAWTO(X(J,1),R(J,1))
  210         CONTINUE
C
              CALL MOVETO(X(1,KM),R(1,KM))
C
              DO 211 J=2,JM
                  CALL DRAWTO(X(J,KM),R(J,KM))
  211         CONTINUE
C
C
C             Draw l.e. & t.e. lines
C
      DO 214 J=1,JM
      IF(IND(J).EQ.1) THEN
              CALL MOVETO(X(J,1),R(J,1))
C
              DO 212 K=2,KM
                  CALL DRAWTO(X(J,K),R(J,K))
  212         CONTINUE
      END IF
  214 CONTINUE
C
C
C              CALL MOVETO(X(JTE,1),R(JTE,1))
C
C              DO 213 K=2,KM
C                  CALL DRAWTO(X(JTE,K),R(JTE,K))
C  213         CONTINUE
C
              CALL LINTYP ( 0 )
C
          ENDIF
C
      ENDIF
C
  300 CONTINUE
C
C
      RETURN
C
      END
C*****************************************************************C
C                                                                 C
C              SUBROUTINE TO SELECT PLOTTING VARIABLE
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB8(IREPLY)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK14/ Q(95,2500,95),QQ(95,2500,95),NQ,NQQ
C
      CHARACTER*32 NQ,NQQ
C
      CHARACTER*72 JBNAME,TSNUM, ANS
C
C
   10 CONTINUE
      WRITE(6,*) ' TIME STEP NUMBER ', NSTEP
      WRITE (6,2000)
      WRITE (6,2001)
C
      IREPLY = IREAD(1)
      IF (IREPLY.LE.0) GO TO 10
      IF (IREPLY.GT.34.AND.IREPLY.LT.95) GO TO 10
C
      IF(IREPLY.EQ.20) THEN
      WRITE(6,*)     ' THE VALUES OF STAGNATION TEMPERATURE ARE NOT VERY 
     & ACCURATE WHEN USING STEAM.'
      WRITE(6,*)  ' IT MIGHT BE BETTER TO PLOT STAGNATION ENTHALPY WHICH 
     & IS ACCURATE.'
      WRITE(6,*) ' INPUT "C" TO CONTINUE WITH STAGNATION TEMPERATURE '
      READ(1,*)  ANS
      IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
          RETURN
      ELSE
          GO TO 10
      END IF
      END IF
C
      IF(IREPLY.EQ.28) THEN
      WRITE(6,*)    '  THE CALCULATION OF ISENTROPIC MACH NUMBER IS NOT 
     &ACCURATE FOR STEAM BECAUSE CP AND GAMMA VARY OVER THE FLOW FIELD.'
      WRITE(6,*) ' INPUT "C" TO CONTINUE WITH ISENTROPIC MACH No.'
      READ(1,*) ANS
      IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
          RETURN
      ELSE
          GO TO 10
      END IF
      END IF

C     
      IF(IREPLY.EQ.31) THEN
      WRITE(6,*)     '  THE CALCULATION OF POLYTROPIC EFFICIENCY IS NOT 
     &ACCURATE FOR STEAM BECAUSE CP AND GAMMA VARY OVER THE FLOW FIELD.'
      WRITE(6,*) ' INPUT "C" TO CONTINUE WITH POLYTROPIC EFFICIENCY'
      READ(1,*) ANS
      IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
          RETURN
      ELSE
          GO TO 10
      END IF
      END IF
C
C
      RETURN
C
2000  FORMAT(/' TYPE :'/
     + '  "0"  -  REDISPLAY MENU '//
     + '  "1"  -  AXIAL VELOCITY           "17"  -  RADIAL VELOCITY   '/
     + '  "2"  -  RELATIVE TANGENTIAL VEL-y"18"  -  R * VTHETA-ABS    '/
     + '  "3"  -  DENSITY                  "19"  -  TRUE ENTROPY      '/ 
     + '  "4"  -  STATIC PRESSURE, N/m**2  "20"  -  ABSOLUTE To       '/
     + '  "5"  -  RELATIVE STAGn. PRESSURE "21"  -  ABSOLUTE Po       '/
     + '  "6"  -  RELATIVE MACH NUMBER     "22"  -  REDUCED STATIC P  '/
     + '  "7"  -  ( RO * VX )              "23"  -  PARTICLE TRACKS   '/
     + '  "8"  -  ABSOLUTE TANGENTIAL VEL-y"24"  -  PITCHWISE AVEs.   '/  
     + '  "9"  -  STATIC PRESSURE IN BAR    OR      VISCOSITY RATIO   '/                  
     + ' "10"  -  VELOCITY                 "25"  -  ABSOLUTE VELOCITY '/
     + ' "11"  -  STATIC TEMPERATURE       "26"  -  ABSOLUTE MACH No. '/ 
     + ' "12"  -  ABS STAGNATION ENTHALPY  "27"  -  ABS. PITCHWISE ANG'/ 
     + ' "13"  -  PITCHWISE FLOW ANGLE     "28"  -  ISENTROPIC MACH No'/ 
     + ' "14"  -  MESH                     "29"  -  MASS FLOW RATE    '/
     + ' "15"  -  VELOCITY VECTORS         "30"  -  ENTROPY LOSS COEFF'/
     + ' "16"  -  RADIAL FLOW ANGLE        "31"  -  AVG ETA POLY      '/
     + ' "33"  -  WETNESS                  "34"  -  ST.TUBE THICKNESS'/)
C
2001  FORMAT(   
     + ' "98"  -  READ IN MORE DATA        "99"  -  STOP             '/)
C
      END

C*****************************************************************C
C                                                                 C
C              SUBROUTINE TO SELECT PLOTTING TYPE
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB9(ICH)
C
   10 WRITE (6,2000)
      WRITE (6,2001)
C
      ICH = IREAD(1)
C
      IF (ICH.LE.0) GO TO 10
      IF (ICH.GT.8.AND.ICH.NE.98.AND.ICH.NE.99) GO TO 10
C
      RETURN
C
C
C
2000  FORMAT('   TYPE :-'//
     + '  "0" TO REDISPLAY MENU '/
     + '  "1" TO PLOT CONTOURS '/
     + '  "2" TO PLOT SURFACE DISTRIBUTIONS '/
     + '  "3" TO PLOT STREAM-LINE VARIATIONS'/
     + '  "4" TO PLOT PITCH-WISE VARIATIONS'/
     + '  "5" TO PLOT SPAN-WISE VARIATIONS'/
     + '  "6" TO PLOT PROJECTED SURFACES WITH CONTOURS'//
     + '  "7" TO GIVE PITCHWISE MASS AVERAGE OF THE VARIABLE'/
     + '  "8" TO GIVE AREA MASS AVERAGE OF THE VARIABLE'//)
C
2001  FORMAT(/   
     + ' "98"  -  RETURN TO PREVIOUS MENU'/
     + ' "99"  -  READ IN MORE DATA OR STOP'/)
C
      END
C******************************************************************C
C                                                                  C
C            SUBROUTINE to plot blade surface varations            C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB10(PROP,NAME,X0,Y0,X1,Y1,IFP,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
C
      DIMENSION PROP(95,2500,95),XSS1(2500),XSS2(2500)
C
      CHARACTER*32 NAME,NX
      CHARACTER*8  IFP
      CHARACTER*1  ITYPE,NCHAR(15)
      CHARACTER*72 JBNAME,TSNUM
C
      REAL EMACH,IMACH
C
      COMMON / OPT2 / ISTART, IEND, IDIFF, JSTART, JEND, JDIFF,
     &                KSTART, KEND, KDIFF
C
      DATA NCHAR(1)  / 'C' /,
     &     NCHAR(2)  / ' ' /,
     &     NCHAR(3)  / 'T' /,
     &     NCHAR(4)  / 'F' /,
     &     NCHAR(5)  / 'I' /,
     &     NCHAR(6)  / 'B' /,
     &     NCHAR(7)  / 'D' /,
     &     NCHAR(8)  / 'c' /,
     &     NCHAR(9)  / 's' /,
     &     NCHAR(10) / 't' /,
     &     NCHAR(11) / 'f' /,
     &     NCHAR(12) / 'i' /,
     &     NCHAR(13) / 'b' /,
     &     NCHAR(14) / 'd' /,
     &     NCHAR(15) / 'S' /
C
C         TELL USER OF STATUS
C
      WRITE (6,2002) JBNAME,NAME
C
C         Decide upon type of x axis
C
      WRITE (6,2003)
      READ  (1,1000) ITYPE
C
C
      WRITE(6,*)
      DO 205 N=1,NROWS
      WRITE(6,*) ' ROW NUMBER ',N,' JLE = ',JLEE(N),' JTE = ',JTEE(N),
     & 'JM= ', JM
  205 CONTINUE
C
C      WRITE(6,*)
C      WRITE(6,*) 'INPUT  JLE  and  JTE  FOR THE ROWS WHERE SURFACE DISTR
C     &IBUTIONS ARE TO BE PLOTTED'
C      READ(5,*)  JLE,JTE 
C
C
C         Decide upon K-surface
C
    4 WRITE (6,2031) KM,KSTART
      IANS = IREAD(1)
      IF (IANS.NE.0) KSTART = IANS
      IF (KSTART.LT.1.OR.KSTART.GT.KM) GO TO 4
C
      IF (IEND.LT.ISTART) THEN
	IEND  = ISTART
      ENDIF
C      
    5 WRITE (6,2032) KSTART,KM,KEND
      IANS = IREAD(1)
      IF (IANS.NE.0) KEND = IANS
      IF (KEND.LT.KSTART.OR.KEND.GT.KM) GO TO 5

      IF (KSTART.EQ.KEND) THEN
        KDIFF = 1
      ELSE
        KDMAX = KEND - KSTART
C
    6   WRITE (6,2033) KDMAX,KDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) KDIFF = IANS
        IF (KDIFF.LE.0.OR.KDIFF.GT.KDMAX) GO TO 6
      ENDIF
C
C         Find max & min values of whole of array "PROP"
C
      CALL SUB6(PROP,PMAX,PMIN)
C
C         Set limits for Y-axis
C
      CALL SUB20(PMAX,PMIN,PINC)
C
      WRITE (6,2034) PMIN
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMIN = RANS
C
      WRITE (6,2035) PMAX
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMAX = RANS
C
      WRITE (6,2036) PMAX, PMIN 
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMIN = RANS
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMAX = RANS      
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C          LABEL
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 12.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
C
C        LABEL PLOT
C
C        Set up axes
C
      XTIC = 3.
      YTIC = 3.
C
      NXTIC = 4
      NYTIC = 1
C
      XMAX = 1.0
      XMIN = 0.0
      XINC = 0.5
C
C
      CALL BGNWRT( X1+5., 0.75*(Y1+Y0)+10., 0 )
      IBOX = 0
      IPLT = 0
C
      DO 200 K=KSTART,KEND,KDIFF
C
          IPLT = IPLT + 1
C
          CALL WRITEA(' ','(A3)','K =')
          CALL WRITEI('*','(I4)',K)
          CALL WRITEX(1)
          CALL WRITES(NCHAR(MOD(IPLT,15)+1))
C
C             Build up a 1d array of surface values
C
          NPTS = JTE-JLE + 1
C
          DO 10 N=1,NPTS
              YS1(N) = PROP(1,N+JLE-1,K)
              YS2(N) = PROP(IM,N+JLE-1,K)
   10     CONTINUE
C
C
      IF (ITYPE . EQ . 'C'  . OR . ITYPE . EQ . 'c' ) THEN
          CALL SUB1(XS1,XS2,NX,K)
      ELSE IF (ITYPE . EQ . 'M' . OR . ITYPE . EQ . 'm' ) THEN
          CALL SUB27(XS1,XS2,NX,K)
      ELSE IF (ITYPE . EQ . 'S' . OR . ITYPE . EQ . 's' ) THEN
          CALL SUB3(XS1,XS2,NX,K)
      ELSE
          CALL SUB2(XS1,XS2,NX,K)
      ENDIF
C
C
          CALL SUB19(X0,Y0,XMIN,PMIN,X1,Y1,XMAX,PMAX,XINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.2)',IFP,XS1,YS1,NPTS,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,NX,NAME)
C
          IBOX = 1
C
          CALL SUB19(X0,Y0,XMIN,PMIN,X1,Y1,XMAX,PMAX,XINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.2)',IFP,XS1,YS1,NPTS,
     +     ' ',
     +     IBOX,NX,NAME)
C
          CALL SUB19(X0,Y0,XMIN,PMIN,X1,Y1,XMAX,PMAX,XINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.2)',IFP,XS2,YS2,NPTS,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,NX,NAME)
C
          CALL SUB19(X0,Y0,XMIN,PMIN,X1,Y1,XMAX,PMAX,XINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.2)',IFP,XS2,YS2,NPTS,
     +     ' ',
     +     IBOX,NX,NAME)
  200 CONTINUE
C
      CALL BRKPLT
      RETURN
C
C
1000  FORMAT(A1)
C
C
2002  FORMAT(//' ',A80,/' ',A32//' SURFACE DISTRIBUTIONS SELECTED'/)
2003  FORMAT('  ENTER -'//
     +       '     "C" to plot against true chord'/
     +       '     "X" to plot against axial chord <==== DEFAULT'/
     +       '     "M" to plot against meridional coordinate'/
     +       '     "S" to plot against surface length'//)
2031  FORMAT(//'  ENTER first integer value (K) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2032  FORMAT(//'  ENTER last integer value (K) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2131  FORMAT(//'  ENTER first integer value (J) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2132  FORMAT(//'  ENTER last integer value (J) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2033  FORMAT(//'  ENTER increment for integer value of (K)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2034  FORMAT(//'  Minimum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2035  FORMAT(//'  Maximum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2036  FORMAT(//'  THE MAXIMUM AND MINIMUM VALUES ARE ',2F13.4,//
     +         '  Press "RETURN" twice if OK '/
     8         '  if not enter two new values'/)
C
      END
C******************************************************************C
C                                                                  C
C          SUBROUTINNE TO PLOT STREAMWISE VARIATIONS               C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB11(PROP,NAME,X0,Y0,X1,Y1,IFP,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      DIMENSION PROP(95,2500,95)
C
      CHARACTER*32 NAME
      CHARACTER*8  IFP
      CHARACTER*1  ANS,NCHAR(15)
      CHARACTER*72 JBNAME,TSNUM
C
      REAL EMACH,IMACH
C
      COMMON / OPT2 / ISTART, IEND, IDIFF, JSTART, JEND, JDIFF,
     &                KSTART, KEND, KDIFF
C
      DATA NCHAR(1)  / 'C' /,
     &     NCHAR(2)  / ' ' /,
     &     NCHAR(3)  / 'T' /,
     &     NCHAR(4)  / 'F' /,
     &     NCHAR(5)  / 'I' /,
     &     NCHAR(6)  / 'B' /,
     &     NCHAR(7)  / 'D' /,
     &     NCHAR(8)  / 'c' /,
     &     NCHAR(9)  / 's' /,
     &     NCHAR(10) / 't' /,
     &     NCHAR(11) / 'f' /,
     &     NCHAR(12) / 'i' /,
     &     NCHAR(13) / 'b' /,
     &     NCHAR(14) / 'd' /,
     &     NCHAR(15) / 'S' /
C
C         TELL USER OF STATUS
C
      WRITE (6,2002) JBNAME,NAME
C
C      Find coordinate type
C
      WRITE (6,2024)
      READ  (1,1000) ANS
C
C         Decide upon I-surface
C
   14 WRITE (6,2041) IM,ISTART
      IANS = IREAD(1)
      IF (IANS.NE.0) ISTART = IANS
      IF (ISTART.LT.1.OR.ISTART.GT.IM) GO TO 14
C
      IF (IEND.LT.ISTART) THEN
	IEND  = ISTART
      ENDIF
C      
   15 WRITE (6,2042) ISTART,IM,IEND
      IANS = IREAD(1)
      IF (IANS.NE.0) IEND = IANS
      IF (IEND.LT.ISTART.OR.IEND.GT.IM) GO TO 15
C
      IF (ISTART.EQ.IEND) THEN
        IDIFF = 1
      ELSE
        IDMAX = IEND - ISTART
C
   16   WRITE (6,2043) IDMAX,IDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) IDIFF = IANS
        IF (IDIFF.LE.0.OR.IDIFF.GT.IDMAX) GO TO 16
      ENDIF
C
C         Decide upon K-surface
C
    4 WRITE (6,2031) KM,KSTART
      IANS = IREAD(1)
      IF (IANS.NE.0) KSTART = IANS
      IF (KSTART.LT.1.OR.KSTART.GT.KM) GO TO 4
C
      IF (IEND.LT.ISTART) THEN
	IEND  = ISTART
      ENDIF
C      
    5 WRITE (6,2032) KSTART,KM,KEND
      IANS = IREAD(1)
      IF (IANS.NE.0) KEND = IANS
      IF (KEND.LT.KSTART.OR.KEND.GT.KM) GO TO 5
C
      IF (KSTART.EQ.KEND) THEN
        KDIFF = 1
      ELSE
        KDMAX = KEND - KSTART
C
    6   WRITE (6,2033) KDMAX,KDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) KDIFF = IANS
        IF (KDIFF.LE.0.OR.KDIFF.GT.KDMAX) GO TO 6
      ENDIF
C
C         Find max & min values of whole of array "PROP"
C
      CALL SUB6(PROP,PMAX,PMIN)
C
C         Set limits for Y-axis
C
      CALL SUB20(PMAX,PMIN,PINC)
C
      WRITE (6,2034) PMIN
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMIN = RANS
C
      WRITE (6,2035) PMAX
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMAX = RANS
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C          LABEL
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 12.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
C
C        LABEL PLOT
C
C        Set up axes
C
      XTIC = 3.
      YTIC = 3.
C
      NXTIC = 4
      NYTIC = 1
C
C         Build up a 1d array of surface values
C
      IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          AMAX = SMMAX + 0.1*(SMMAX-SMMIN)
          AMIN = SMMIN - 0.1*(SMMAX-SMMIN)
C
          CALL SUB20(AMAX,AMIN,AINC)
C
          AMAX = SMMAX + 0.1*(SMMAX-SMMIN)
          AMIN = SMMIN - 0.1*(SMMAX-SMMIN)
C
      ELSE 
          AMAX = XMAX + 0.1*(XMAX-XMIN)
          AMIN = XMIN - 0.1*(XMAX-XMIN)
C
          CALL SUB20(AMAX,AMIN,AINC)
C
          AMAX = XMAX + 0.1*(XMAX-XMIN)
          AMIN = XMIN - 0.1*(XMAX-XMIN)
      ENDIF
C
C
      CALL BGNWRT( X1+5., 0.75*(Y1+Y0)+10., 0 )
      IBOX = 0
      IPLT = 0
C
      DO 200 K=KSTART,KEND,KDIFF
        DO 201 I=ISTART,IEND,IDIFF
C
          IPLT = IPLT + 1
C
          CALL WRITEA(' ','(A3)','I =')
          CALL WRITEI('*','(I4)',I)
          CALL WRITEX(2)
          CALL WRITEA('*','(A3)','K =')
          CALL WRITEI('*','(I4)',K)
          CALL WRITEX(2)
          CALL WRITES(NCHAR(MOD(IPLT,15)+1))
C
C         Build up a 1d array of surface values
C
          IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
              DO 11 J=1,JM
                  XS1(J) =    SM(J,K)
                  YS1(J) = PROP(I,J,K)
   11         CONTINUE
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,JM,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,'      MERIDIONAL DISTANCE       ',NAME)
C
              IBOX = 1
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,JM,
     +     ' ',
     +     IBOX,'      MERIDIONAL DISTANCE       ',NAME)
          ELSE 
              DO 10 J=1,JM
                  XS1(J) =    X(J,K)
                  YS1(J) = PROP(I,J,K)
   10         CONTINUE
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,JM,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,'          AXIAL DISTANCE        ',NAME)
C
              IBOX = 1
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,JM,
     +     ' ',
     +     IBOX,'          AXIAL DISTANCE        ',NAME)
          ENDIF
  201   CONTINUE
  200 CONTINUE
C
C
      CALL BRKPLT
      RETURN
C
C
C
1000  FORMAT(A1)
C
C
2002  FORMAT(//' ',A80,/' ',A32//' QUASI-STREAMLINES SELECTED'/)
2024  FORMAT(//'  ENTER -'//
     +         '     "M" for Meridional coordinate'/
     +         '     "X" for Axial coordinate  <===== Default'/)
2031  FORMAT(//'  ENTER first integer value (K) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2032  FORMAT(//'  ENTER last integer value (K) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2033  FORMAT(//'  ENTER increment for integer value of (K)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2034  FORMAT(//'  Minimum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2035  FORMAT(//'  Maximum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2041  FORMAT(//'  ENTER first integer value (I) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2042  FORMAT(//'  ENTER last integer value (I) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2043  FORMAT(//'  ENTER increment for integer value of (I)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
C
      END
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO PLOT PITCHWISE VARIATIONS                  C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB12(PROP,NAME,X0,Y0,X1,Y1,IFP,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      DIMENSION PROP(95,2500,95)
C
      CHARACTER*32 NAME
      CHARACTER*8  IFP
      CHARACTER*1  ANS,NCHAR(15)
      CHARACTER*72 JBNAME,TSNUM
C
      REAL EMACH,IMACH
C
      COMMON / OPT2 / ISTART, IEND, IDIFF, JSTART, JEND, JDIFF,
     &                KSTART, KEND, KDIFF
C
      DATA NCHAR(1)  / 'C' /,
     &     NCHAR(2)  / ' ' /,
     &     NCHAR(3)  / 'T' /,
     &     NCHAR(4)  / 'F' /,
     &     NCHAR(5)  / 'I' /,
     &     NCHAR(6)  / 'B' /,
     &     NCHAR(7)  / 'D' /,
     &     NCHAR(8)  / 'c' /,
     &     NCHAR(9)  / 's' /,
     &     NCHAR(10) / 't' /,
     &     NCHAR(11) / 'f' /,
     &     NCHAR(12) / 'i' /,
     &     NCHAR(13) / 'b' /,
     &     NCHAR(14) / 'd' /,
     &     NCHAR(15) / 'S' /
C
C         TELL USER OF STATUS
C
      WRITE (6,2002) JBNAME,NAME
C
      WRITE (6,2024)
      READ  (1,1000) ANS
C
C         Decide upon J-surface
C
   24 WRITE (6,2051) JM,JSTART
      IANS = IREAD(1)
      IF (IANS.NE.0) JSTART = IANS
      IF (JSTART.LT.1.OR.JSTART.GT.JM) GO TO 24
C
      IF (JEND.LT.JSTART) THEN
	JEND  = JSTART
      ENDIF
C      
   25 WRITE (6,2052) JSTART,JM,JEND
      IANS = IREAD(1)
      IF (IANS.NE.0) JEND = IANS
      IF (JEND.LT.JSTART.OR.JEND.GT.JM) GO TO 25
C
      IF (JSTART.EQ.JEND) THEN
        JDIFF = 1
      ELSE
        JDMAX = JEND - JSTART
C
   26   WRITE (6,2053) JDMAX,JDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) JDIFF = IANS
        IF (JDIFF.LE.0.OR.JDIFF.GT.JDMAX) GO TO 26
      ENDIF
C
C         Decide upon K-surface
C
    4 WRITE (6,2031) KM,KSTART
      IANS = IREAD(1)
      IF (IANS.NE.0) KSTART = IANS
      IF (KSTART.LT.1.OR.KSTART.GT.KM) GO TO 4
C
      IF (IEND.LT.ISTART) THEN
	IEND  = ISTART
      ENDIF
C      
    5 WRITE (6,2032) KSTART,KM,KEND
      IANS = IREAD(1)
      IF (IANS.NE.0) KEND = IANS
      IF (KEND.LT.KSTART.OR.KEND.GT.KM) GO TO 5
C
      IF (KSTART.EQ.KEND) THEN
        KDIFF = 1
      ELSE
        KDMAX = KEND - KSTART
C
    6   WRITE (6,2033) KDMAX,KDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) KDIFF = IANS
        IF (KDIFF.LE.0.OR.KDIFF.GT.KDMAX) GO TO 6
      ENDIF
C
C         Find max & min values of whole of array "PROP"
C
      CALL SUB6(PROP,PMAX,PMIN)
C
C         Set limits for Y-axis
C
      CALL SUB20(PMAX,PMIN,PINC)
C
      WRITE (6,2034) PMIN
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMIN = RANS
C
      WRITE (6,2035) PMAX
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMAX = RANS
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C          LABEL
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 12.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
C
C        LABEL PLOT
C
C        Set up axes
C
      XTIC = 3.
      YTIC = 3.
C
      NXTIC = 4
      NYTIC = 1
C
C         Build up a 1d array of surface values
C
      IF ( ANS . EQ . 'Y' ) THEN
C
C          Set limits for x-axis
C
          AMAX = YMAX + 0.1*(YMAX-YMIN)
          AMIN = YMIN - 0.1*(YMAX-YMIN)
C
          CALL SUB20(AMAX,AMIN,AINC)
C
          AMAX = YMAX + 0.1*(YMAX-YMIN)
          AMIN = YMIN - 0.1*(YMAX-YMIN)
C
      ELSE          
C
C          Set limits for x-axis
C
          AMAX = RTMAX + 0.1*(RTMAX-RTMIN)
          AMIN = RTMIN - 0.1*(RTMAX-RTMIN)
C
          CALL SUB20(AMAX,AMIN,AINC)
C
          AMAX = RTMAX + 0.1*(RTMAX-RTMIN)
          AMIN = RTMIN - 0.1*(RTMAX-RTMIN)
C
      ENDIF
C
C
      CALL BGNWRT( X1+5., 0.75*(Y1+Y0)+10., 0 )
      IBOX = 0
      IPLT = 0
C
      DO 200 K=KSTART,KEND,KDIFF
        DO 201 J=JSTART,JEND,JDIFF
C
          IPLT = IPLT + 1
C
          CALL WRITEA(' ','(A3)','J =')
          CALL WRITEI('*','(I4)',J)
          CALL WRITEX(2)
          CALL WRITEA('*','(A3)','K =')
          CALL WRITEI('*','(I4)',K)
          CALL WRITEX(2)
          CALL WRITES(NCHAR(MOD(IPLT,15)+1))
C
C         Build up a 1d array of surface values
C
          IF ( ANS . EQ . 'Y' ) THEN
              DO 11 I=1,IM
                  XS1(I) = R(J,K)*SIN(T(I,J,K)-TMID)
                  YS1(I) = PROP(I,J,K)
   11         CONTINUE
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,IM,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,' DISTANCE IN RADIAL PROJECTION  ',NAME)
C
              IBOX = 1
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,IM,
     +     ' ',
     +     IBOX,' DISTANCE IN RADIAL PROJECTION  ',NAME)
          ELSE
              DO 10 I=1,IM
                  XS1(I) = R(J,K)*(T(I,J,K)-TMID)
                  YS1(I) = PROP(I,J,K)
   10         CONTINUE
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,IM,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,'   CIRCUMFERENTIAL DISTANCE     ',NAME)
C
              IBOX = 1
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,IM,
     +     ' ',
     +     IBOX,'   CIRCUMFERENTIAL DISTANCE     ',NAME)
          ENDIF
  201   CONTINUE
  200 CONTINUE
C
C
      CALL BRKPLT
      RETURN
C
C
C
1000  FORMAT(A1)
C
C
2002  FORMAT(//' ',A80,/' ',A32//' PITCH-WISE  LINES SELECTED'/)
2024  FORMAT('  ENTER -'//
     +  '     "Y" to plot against coord. in radial projection'/
     +  '     "T" to plot against tangential coord. <=== DEFAULT'/)       
2031  FORMAT(//'  ENTER first integer value (K) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2032  FORMAT(//'  ENTER last integer value (K) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2033  FORMAT(//'  ENTER increment for integer value of (K)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2034  FORMAT(//'  Minimum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2035  FORMAT(//'  Maximum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2051  FORMAT(//'  ENTER first integer value (J) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2052  FORMAT(//'  ENTER last integer value (J) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2053  FORMAT(//'  ENTER increment for integer value of (J)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
C
      END
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO PLOT RADIAL VARIATIONS
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB13(PROP,NAME,X0,Y0,X1,Y1,IFP,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      DIMENSION PROP(95,2500,95)
C
      CHARACTER*32 NAME
      CHARACTER*8  IFP
      CHARACTER*1  ANS,NCHAR(15)
      CHARACTER*72 JBNAME,TSNUM
C
      REAL EMACH,IMACH
C
      COMMON / OPT2 / ISTART, IEND, IDIFF, JSTART, JEND, JDIFF,
     &                KSTART, KEND, KDIFF
C
      DATA NCHAR(1)  / 'C' /,
     &     NCHAR(2)  / ' ' /,
     &     NCHAR(3)  / 'T' /,
     &     NCHAR(4)  / 'F' /,
     &     NCHAR(5)  / 'I' /,
     &     NCHAR(6)  / 'B' /,
     &     NCHAR(7)  / 'D' /,
     &     NCHAR(8)  / 'c' /,
     &     NCHAR(9)  / 's' /,
     &     NCHAR(10) / 't' /,
     &     NCHAR(11) / 'f' /,
     &     NCHAR(12) / 'i' /,
     &     NCHAR(13) / 'b' /,
     &     NCHAR(14) / 'd' /,
     &     NCHAR(15) / 'S' /
C
C         TELL USER OF STATUS
C
      WRITE (6,2002) JBNAME,NAME
C
C      Find type of coordinate
C
      WRITE (6,2024)
      READ  (1,1000) ANS
C
C         Decide upon I-surface
C
   14 WRITE (6,2041) IM,ISTART
      IANS = IREAD(1)
      IF (IANS.NE.0) ISTART = IANS
      IF (ISTART.LT.1.OR.ISTART.GT.IM) GO TO 14
C
      IF (IEND.LT.ISTART) THEN
	IEND  = ISTART
      ENDIF
C      
   15 WRITE (6,2042) ISTART,IM,IEND
      IANS = IREAD(1)
      IF (IANS.NE.0) IEND = IANS
      IF (IEND.LT.ISTART.OR.IEND.GT.IM) GO TO 15
C
      IF (ISTART.EQ.IEND) THEN
        IDIFF = 1
      ELSE
        IDMAX = IEND - ISTART
C
   16   WRITE (6,2043) IDMAX,IDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) IDIFF = IANS
        IF (IDIFF.LE.0.OR.IDIFF.GT.IDMAX) GO TO 16
      ENDIF
C
C         Decide upon J-surface
C
   24 WRITE (6,2051) JM,JSTART
      IANS = IREAD(1)
      IF (IANS.NE.0) JSTART = IANS
      IF (JSTART.LT.1.OR.JSTART.GT.JM) GO TO 24
C
      IF (JEND.LT.JSTART) THEN
	JEND  = JSTART
      ENDIF
C      
   25 WRITE (6,2052) JSTART,JM,JEND
      IANS = IREAD(1)
      IF (IANS.NE.0) JEND = IANS
      IF (JEND.LT.JSTART.OR.JEND.GT.JM) GO TO 25
C
      IF (JSTART.EQ.JEND) THEN
        JDIFF = 1
      ELSE
        JDMAX = JEND - JSTART
C
   26   WRITE (6,2053) JDMAX,JDIFF
        IANS = IREAD(1)
        IF (IANS.NE.0) JDIFF = IANS
        IF (JDIFF.LE.0.OR.JDIFF.GT.JDMAX) GO TO 26
      ENDIF
C
C         Find max & min values of whole of array "PROP"
C
      CALL SUB6(PROP,PMAX,PMIN)
C
C         Set limits for Y-axis
C
      CALL SUB20(PMAX,PMIN,PINC)
C
      WRITE (6,2034) PMIN
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMIN = RANS
C
      WRITE (6,2035) PMAX
      RANS = RREAD(1)
      IF ( RANS . NE . 0.0 ) PMAX = RANS
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C          LABEL
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 12.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
C
C        LABEL PLOT
C
C        Set up axes
C
      XTIC = 3.
      YTIC = 3.
C
      NXTIC = 4
      NYTIC = 1
C
C         Build up a 1d array of surface values
C
      IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
C          Set limits for x-axis
C
          AMAX = SRMAX + 0.1*(SRMAX-SRMIN)
          AMIN = SRMIN - 0.1*(SRMAX-SRMIN)
C
          CALL SUB20(AMAX,AMIN,AINC)
C
          AMAX = SRMAX + 0.1*(SRMAX-SRMIN)
          AMIN = SRMIN - 0.1*(SRMAX-SRMIN)
C
      ELSE          
C
C          Set limits for x-axis
C
          AMAX = ZMAX + 0.1*(ZMAX-ZMIN)
          AMIN = ZMIN - 0.1*(ZMAX-ZMIN)
C
          CALL SUB20(AMAX,AMIN,AINC)
C
          AMAX = ZMAX + 0.1*(ZMAX-ZMIN)
          AMIN = ZMIN - 0.1*(ZMAX-ZMIN)
      ENDIF
C
C
      CALL BGNWRT( X1+5., 0.75*(Y1+Y0)+10., 0 )
      IBOX = 0
      IPLT = 0
C
      DO 200 I=ISTART,IEND,IDIFF
        DO 201 J=JSTART,JEND,JDIFF
C
          IPLT = IPLT + 1
C
          CALL WRITEA(' ','(A3)','I =')
          CALL WRITEI('*','(I4)',I)
          CALL WRITEX(2)
          CALL WRITEA('*','(A3)','J =')
          CALL WRITEI('*','(I4)',J)
          CALL WRITEX(2)
          CALL WRITES(NCHAR(MOD(IPLT,15)+1))
C
C         Build up a 1d array of surface values
C
          IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
C     JDD CHANGED THIS TO FRACTION OF SPAN
              XS1(1) = 0.0
              DO 110 K=2,KM
              XD = X(J,K) - X(J,K-1)
              RD = R(J,K) - R(J,K-1)
              XS1(K) = XS1(K-1) + SQRT(XD*XD + RD*RD)
  110         CONTINUE
              DO 112 K=1,KM
              XS1(K) = XS1(K)/XS1(KM)
  112         CONTINUE
C
              AMAX = 1.0
              AMIN = 0.0
              AINC = 0.2
C
              DO 11 K=1,KM
C                  XS1(K) = (X(J,K)-X(J,1))*COSA(J)
C     &                    +(R(J,K)*COS(T(I,J,K)-TMID)-R(J,1))*SINA(J)  
                  YS1(K) = PROP(I,J,K)
   11         CONTINUE
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,KM,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,'          FRACTION OF SPAN      ',NAME)
C
              IBOX = 1
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,KM,
     +     ' ',
     +     IBOX,'           FRACTION OF SPAN     ',NAME)
          ELSE
              SPAN  = R(J,KM) - R(J,1)
              AMAX  = 1.0
              AMIN  = 0.0
              AINC  = 0.1
              PINC  = (PMAX - PMIN)/10.
              DO 10 K=1,KM
                  XS1(K) = (R(J,K) - R(J,1))/SPAN
                  YS1(K) = PROP(I,J,K)
   10         CONTINUE
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,KM,
     +     NCHAR(MOD(IPLT,15)+1),
     +     IBOX,'         RADIAL DISTANCE        ',NAME)
C
              IBOX = 1
C
              CALL SUB19(X0,Y0,AMIN,PMIN,X1,Y1,AMAX,PMAX,AINC,PINC,
     +     XTIC,YTIC,NXTIC,NYTIC,'(F8.3)',IFP,XS1,YS1,KM,
     +     ' ',
     +     IBOX,'         RADIAL DISTANCE        ',NAME)
          ENDIF
  201   CONTINUE
  200 CONTINUE
C
C
      CALL BRKPLT
      RETURN
C
C
C
1000  FORMAT(A1)
C
C
2002  FORMAT(//' ',A80,/' ',A32//' RADIAL LINE PLOT  SELECTED'/)
2024  FORMAT('  ENTER -'//
     +       '     "Q" to plot against FRACTION OF SPAN'/
     +       '     "R" to plot against radial coord. <=== DEFAULT'/)       
2034  FORMAT(//'  Minimum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2035  FORMAT(//'  Maximum value on Y-axis ...........',F13.4//
     +         '  Press "RETURN" if OK, if not enter new value'/)
2041  FORMAT(//'  ENTER first integer value (I) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2042  FORMAT(//'  ENTER last integer value (I) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2043  FORMAT(//'  ENTER increment for integer value of (I)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2051  FORMAT(//'  ENTER first integer value (J) of quasi-stream '/
     +         '        surface for which the variable is to    '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2052  FORMAT(//'  ENTER last integer value (J) of quasi-stream '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Start value   =',I4/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2053  FORMAT(//'  ENTER increment for integer value of (J)     '/
     +         '        surface for which the variable is to   '/
     +         '        be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
C
      END
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO PLOT MESH
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB15(NAME,X0,Y0,X1,Y1,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK19/ XX(95,2500),YY(95,2500),ZZ(95,2500),
     &                WORK1(95,2500),WORK2(95,2500)
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      CHARACTER*72 JBNAME,TSNUM
      CHARACTER*32 NAME
      CHARACTER*1  CANS
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT1 /  DELR, ZSCALE, VMOD,
     &                 NDIFF, I, J, K, IZ, JZ, KZ, IPIT, CTYPE, ANS
C
C
C         TELL USER OF STATUS
C
      WRITE (6,2000) JBNAME,NAME
C
C        NOW SET SCALE FACTOR "R"
C
      RX  =  (X1-X0)/( XMAX- XMIN)
      RY  =  (Y1-Y0)/( YMAX- YMIN)
      RZ  =  (X1-X0)/( ZMAX- ZMIN)
      RA  =  (X1-X0)/( RMAX- RMIN)
      RB  =  (Y1-Y0)/( RMAX- RMIN)
      RS  =  (X1-X0)/(SMMAX-SMMIN)
      RT  =  (Y1-Y0)/(RTMAX-RTMIN)
      RR  =  (X1-X0)/(SRMAX-SRMIN)
      RR  = AMIN1 ( RX, RY, RZ, RA, RB, RS, RT, RR )
C
C         DETERMINE TYPE OF PLOT REQUIRED
C
   55 WRITE (6,2020) ANS
      READ  (1,1000) CANS
      IF ( CANS . NE . ' ' ) ANS = CANS
C
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' . OR . 
     &     ANS . EQ . 'S' . OR . ANS . EQ . 's' . OR . 
     &     ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
   13     WRITE (6,2021) KM, K
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) K = IANS
          IF (K.GT.KM . OR . K.LT.1 ) GO TO 13
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' . OR . 
     &          ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
C
   23     WRITE (6,2022) JM, J
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) J = IANS
          IF (J.GT.JM . OR . J.LT.1 ) GO TO 23
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
   33     WRITE (6,2023) IM, I
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) I = IANS
          IF (I.GT.IM . OR . I.LT.1 ) GO TO 33
C
      ELSE
C
          GO TO 55
      ENDIF
C
C      Zoom ?
C
      WRITE (6,2024) ZLAST
      ZIN = RREAD(1)
      IF (ZIN.GT.0.01) THEN
          ZSCALE = ZIN
          ZLAST  = ZSCALE
      ELSE 
          ZSCALE = ZLAST
      ENDIF
C
      IF(ZIN.GT.0.01) THEN
C
   35     WRITE (6,2025) IM, IZ
          IANS = IREAD(1)
          IF (IANS.NE.0) IZ = IANS
          IF (IZ.LT.1.OR.IZ.GT.IM) GO TO 35
C
   36     WRITE (6,2026) JM, JZ
          IANS = IREAD(1)
          IF (IANS.NE.0) JZ = IANS
          IF (JZ.LT.1.OR.JZ.GT.JM) GO TO 36
C
   37     WRITE (6,2027) KM, KZ
          IANS = IREAD(1)
          IF (IANS.NE.0) KZ = IANS
          IF (KZ.LT.1.OR.KZ.GT.KM) GO TO 37
C
      ENDIF
C
C         Find number of pitches to be plotted
C
      WRITE (6,2061) IPIT
      IANS = IREAD(1)
      IF ( IANS . NE . 0 ) IPIT = IANS
      IF ( IPIT . LE . 0 ) IPIT = 0
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C      ADD TITLES
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
      XX6 = 175.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 15.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
      CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
C
C
      S = (XX1-XX0)/80./3.0
      CALL SCLCHR( S, S )
      CALL BGNWRT(XX6,YY2,1)
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
          CALL WRITEA('*','(A30)',' RADIAL PROJECTION OF S-S. No.')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          CALL WRITEA('*','(A30)','  AXIAL PROJECTION OF S-S. No.')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL WRITEA('*','(A30)','         STREAM SURFACE NUMBER')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
          CALL WRITEA('*','(A30)','  AXIAL PROJECTION OF Q-O. No.')
          CALL WRITEI('*','(I4)',J)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL WRITEA('*','(A30)','       QUASI-ORTHOGONAL NUMBER')
          CALL WRITEI('*','(I4)',J)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL WRITEA('*','(A30)','     MERIDIONAL SURFACE NUMBER')
          CALL WRITEI('*','(I4)',I)
      ENDIF
      CALL WRITEA(' ','(A6)','ZOOM =')
      CALL WRITEF('*','(F6.1)',ZSCALE)
      CALL WRITEA('*','(A7)',' AT I =')
      CALL WRITEI('*','(I4)',IZ)
      CALL WRITEA('*','(A5)',', J =')
      CALL WRITEI('*','(I4)',JZ)
      CALL WRITEA('*','(A5)',', K =')
      CALL WRITEI('*','(I4)',KZ)
      CALL SCLCHR ( 1.0/1.0, 1.0/1.0 )
C
C         SET WINDOW
C
      CALL AWINDW ( X0, Y0, X1, Y1, -1 )
      CALL ORIGIN( 0.5*(X0+X1), 0.5*(Y0+Y1), 1 )
      CALL SCALE ( RR, RR )
C
C        SHIFT ORIGIN TO CENTRE PLOT
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
          CALL ORIGIN( -0.5*(XMIN+XMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( X(JZ,KZ), R(JZ,KZ)*(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          CALL ORIGIN( -0.5*(ZMIN+ZMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL ORIGIN( -0.5*(SMMIN+SMMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( SM(JZ,KZ), R(JZ,KZ)*(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL ORIGIN( -0.5*(SRMIN+SRMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( (X(JZ,KZ)-X(JZ,1))*COSA(JZ)
     &              +(R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID)-R(JZ,1))*SINA(JZ),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
          CALL ORIGIN( -0.5*(ZMIN+ZMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL ORIGIN( -0.5*(XMIN+XMAX), -0.5*(RMIN+RMAX), 0 )
          CALL ZOOM( X(JZ,KZ), R(JZ,KZ),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ENDIF
C
C        PLOT MESH
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
C
C         Plot lines of constant I
C
          DO 40 I=1,IM,2
              CALL MOVETO(X(1,K),R(1,K)*(T(I,1,K)-TMID))
C
              DO 41 J=2,JM
                  CALL DRAWTO(X(J,K),R(J,K)*(T(I,J,K)-TMID))
   41         CONTINUE
C
              IF (I . EQ . IM) GO TO 40
              CALL MOVETO(X(JM,K),R(JM,K)*(T(I+1,JM,K)-TMID))
C
              DO 42 J=2,JM
                  CALL DRAWTO(X(JM-J+1,K),
     &                        R(JM-J+1,K)*(T(I+1,JM-J+1,K)-TMID))
   42         CONTINUE
   40     CONTINUE
C
C         Plot lines of constant J
C
          DO 50 J=1,JM
              CALL MOVETO(X(J,K),R(J,K)*(T(     1,J,K)-TMID))
              CALL DRAWTO(X(J,K),R(J,K)*(T(ISPLIT,J,K)-TMID))
C
              IF ( ISPLIT . NE . IM ) THEN
                  CALL MOVETO(X(J,K),R(J,K)*(T(ISPLIT+1,J,K)-TMID))
                  CALL DRAWTO(X(J,K),R(J,K)*(T(      IM,J,K)-TMID))
              ENDIF
   50     CONTINUE
C
C
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
C         Plot lines of constant I
C
          DO 140 I=1,IM,2
              CALL MOVETO(R(1,K)*COS(T(I,1,K)-TMID),
     &                    R(1,K)*SIN(T(I,1,K)-TMID))
C
              DO 141 J=2,JM
                  CALL DRAWTO(R(J,K)*COS(T(I,J,K)-TMID),
     &                        R(J,K)*SIN(T(I,J,K)-TMID))
  141         CONTINUE
C
              IF (I . EQ . IM) GO TO 140
              CALL MOVETO(R(JM,K)*COS(T(I+1,JM,K)-TMID),
     &                    R(JM,K)*SIN(T(I+1,JM,K)-TMID))
C
              DO 142 J=2,JM
                  CALL DRAWTO(R(JM-J+1,K)*COS(T(I+1,JM-J+1,K)-TMID),
     &                        R(JM-J+1,K)*SIN(T(I+1,JM-J+1,K)-TMID))
  142         CONTINUE
  140     CONTINUE
C
C         Plot lines of constant J
C
          DO 150 J=1,JM
              CALL MOVETO(R(J,K)*COS(T(     1,J,K)-TMID),
     &                    R(J,K)*SIN(T(     1,J,K)-TMID))
              DO 151 I=1,ISPLIT
                  CALL DRAWTO(R(J,K)*COS(T(I,J,K)-TMID),
     &                        R(J,K)*SIN(T(I,J,K)-TMID))
  151         CONTINUE
C
              IF ( ISPLIT . NE . IM ) THEN
                  CALL MOVETO(R(J,K)*COS(T(ISPLIT+1,J,K)-TMID),
     &                        R(J,K)*SIN(T(ISPLIT+1,J,K)-TMID))
                  DO 152 I=ISPLIT+1,IM
                      CALL DRAWTO(R(J,K)*COS(T(I,J,K)-TMID),
     &                            R(J,K)*SIN(T(I,J,K)-TMID))
  152             CONTINUE
              ENDIF
  150     CONTINUE
C
C
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
C
C         Plot lines of constant I
C
          DO 240 I=1,IM,2
              CALL MOVETO(SM(1,K),R(1,K)*(T(I,1,K)-TMID))
C
              DO 241 J=2,JM
                  CALL DRAWTO(SM(J,K),R(J,K)*(T(I,J,K)-TMID))
  241         CONTINUE
C
              IF (I . EQ . IM) GO TO 240
              CALL MOVETO(SM(JM,K),R(JM,K)*(T(I+1,JM,K)-TMID))
C
              DO 242 J=2,JM
                  CALL DRAWTO(SM(JM-J+1,K),
     &                        R(JM-J+1,K)*(T(I+1,JM-J+1,K)-TMID))
  242         CONTINUE
  240     CONTINUE
C
C         Plot lines of constant J
C
          DO 250 J=1,JM
              CALL MOVETO(SM(J,K),R(J,K)*(T(     1,J,K)-TMID))
              CALL DRAWTO(SM(J,K),R(J,K)*(T(ISPLIT,J,K)-TMID))
C
              IF ( ISPLIT . NE . IM ) THEN
                  CALL MOVETO(SM(J,K),R(J,K)*(T(ISPLIT+1,J,K)-TMID))
                  CALL DRAWTO(SM(J,K),R(J,K)*(T(      IM,J,K)-TMID))
              ENDIF
  250     CONTINUE
C
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
C
C         Plot lines of constant I
C
          DO 60 I=1,IM,2
              CALL MOVETO(R(J,1)*COS(T(I,J,1)-TMID),
     &                    R(J,1)*SIN(T(I,J,1)-TMID))
C
              DO 61 K=2,KM
                  CALL DRAWTO(R(J,K)*COS(T(I,J,K)-TMID),
     &                        R(J,K)*SIN(T(I,J,K)-TMID))
   61         CONTINUE
C
              IF (I . EQ . IM) GO TO 60
              CALL MOVETO(R(J,KM)*COS(T(I+1,J,KM)-TMID),
     &                    R(J,KM)*SIN(T(I+1,J,KM)-TMID))
C
              DO 62 K=2,KM
                  CALL DRAWTO(R(J,KM-K+1)*COS(T(I+1,J,KM-K+1)-TMID),
     &                        R(J,KM-K+1)*SIN(T(I+1,J,KM-K+1)-TMID))
   62         CONTINUE
   60     CONTINUE
C
C         Plot lines of constant K
C
          DO 70 K=1,KM
              CALL MOVETO(R(J,K)*COS(T(1,J,K)-TMID),
     &                    R(J,K)*SIN(T(1,J,K)-TMID))
C
              DO 71 I=2,ISPLIT
                  CALL DRAWTO(R(J,K)*COS(T(I,J,K)-TMID),
     &                        R(J,K)*SIN(T(I,J,K)-TMID))
   71         CONTINUE
C
              IF ( ISPLIT . NE . IM ) THEN
                  CALL MOVETO(R(J,K)*COS(T(ISPLIT+1,J,K)-TMID),
     &                        R(J,K)*SIN(T(ISPLIT+1,J,K)-TMID))
C
                  DO 72 I=ISPLIT+2,IM
                      CALL DRAWTO(R(J,K)*COS(T(I,J,K)-TMID),
     &                            R(J,K)*SIN(T(I,J,K)-TMID))
   72             CONTINUE
              ENDIF
   70     CONTINUE
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
C         Plot lines of constant I
C
          DO 260 I=1,IM,2
              CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &                   +(R(J,1)*COS(T(I,J,1)-TMID)
     &                    -R(J,1))*SINA(J),
     &                    R(J,1)*SIN(T(I,J,1)-TMID))
C
              DO 261 K=2,KM
                  CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &                       +(R(J,K)*COS(T(I,J,K)-TMID)
     &                        -R(J,1))*SINA(J),
     &                        R(J,K)*SIN(T(I,J,K)-TMID))
  261         CONTINUE
C
              IF (I . EQ . IM) GO TO 260
              CALL MOVETO((X(J,KM)-X(J,1))*COSA(J)
     &                   +(R(J,KM)*COS(T(I+1,J,KM)-TMID)
     &                    -R(J,1))*SINA(J),
     &                    R(J,KM)*SIN(T(I+1,J,KM)-TMID))
C
              DO 262 K=2,KM
              CALL DRAWTO((X(J,KM-K+1)-X(J,1))*COSA(J)
     &                   +(R(J,KM-K+1)*COS(T(I+1,J,KM-K+1)-TMID)
     &                    -R(J,1))*SINA(J),
     &                        R(J,KM-K+1)*SIN(T(I+1,J,KM-K+1)-TMID))
  262         CONTINUE
  260     CONTINUE
C
C         Plot lines of constant K
C
          DO 270 K=1,KM
              CALL MOVETO((X(J,K)-X(J,1))*COSA(J)
     &                   +(R(J,K)*COS(T(1,J,K)-TMID)
     &                    -R(J,1))*SINA(J),
     &                    R(J,K)*SIN(T(1,J,K)-TMID))
C
              DO 271 I=2,ISPLIT
              CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &                   +(R(J,K)*COS(T(I,J,K)-TMID)
     &                    -R(J,1))*SINA(J),
     &                        R(J,K)*SIN(T(I,J,K)-TMID))
  271         CONTINUE
C
              IF ( ISPLIT . NE . IM ) THEN
                  CALL MOVETO((X(J,K)-X(J,1))*COSA(J)
     &                       +(R(J,K)*COS(T(ISPLIT+1,J,K)-TMID)
     &                        -R(J,1))*SINA(J),
     &                        R(J,K)*SIN(T(ISPLIT+1,J,K)-TMID))
C
                  DO 272 I=ISPLIT+2,IM
                      CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &                           +(R(J,K)*COS(T(I,J,K)-TMID)
     &                            -R(J,1))*SINA(J),
     &                            R(J,K)*SIN(T(I,J,K)-TMID))
  272             CONTINUE
              ENDIF
  270     CONTINUE
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
C         Plot lines of constant K
C
          DO 80 K=1,KM,2
              CALL MOVETO(X(1,K),R(1,K))
C
              DO 81 J=2,JM
                  CALL DRAWTO(X(J,K),R(J,K))
   81         CONTINUE
C
              IF (K . EQ . KM) GO TO 80
              CALL MOVETO(X(JM,K+1),R(JM,K+1))
C
              DO 82 J=2,JM
                  CALL DRAWTO(X(JM-J+1,K+1),
     &                        R(JM-J+1,K+1))
   82         CONTINUE
   80     CONTINUE
C
C         Plot lines of constant J
C
          DO 90 J=1,JM,2
              CALL MOVETO(X(J,1),R(J,1))
C
              DO 91 K=2,KM
                  CALL DRAWTO(X(J,K),R(J,K))
   91         CONTINUE
C
              IF (J . EQ . JM) GO TO 90
              CALL MOVETO(X(J+1,KM),
     &                    R(J+1,KM))
C
              DO 92 K=2,KM
                  CALL DRAWTO(X(J+1,KM-K+1),
     &                        R(J+1,KM-K+1))
   92         CONTINUE
   90     CONTINUE
C
C
      ENDIF
C
C
C
C      DRAW BLADE
C
C
      CALL SUB7
      CALL SUB28
C
C
C
      CALL BRKPLT
C
C     Make sure no bad defaults
C
      I = MAX0 ( MIN0 ( IM, I ), 1 )
      J = MAX0 ( MIN0 ( JM, J ), 1 )
      K = MAX0 ( MIN0 ( KM, K ), 1 )
C
      RETURN
C
C
1000  FORMAT(A1)
C
C
2000  FORMAT(' '//' ',A80//' ',A32//' MESH PLOT SELECTED '/)
2020  FORMAT('    TYPE :-'//
     &       '   "M"  for Meridional Plot'/
     &       '   "Q"  for Quasi-Orthogonal Plot'/
     &       '   "R"  for Radial projection of Quasi Stream-surface'/
     &       '   "S"  for Quasi Stream-surface'/
     &       '   "T"  for Axial projection of Quasi Stream-surface'/
     &       '   "X"  for Axial projection of Quasi-Orthogonal'//
     &       '   "',A1,'" <=== DEFAULT'/)
2021  FORMAT(//'  ENTER integer value (K) of quasi-stream surface for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2022  FORMAT(//'  ENTER integer value (J) of quasi-orthogonal for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2023  FORMAT(//'  ENTER integer value (I) of quasi-meridional for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2024  FORMAT(//'  ENTER Zoom magnification that you require'/
     +         '        DEFAULT ======>',F5.1/)
2025  FORMAT(//'  ENTER integer value (I) of pitchwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2026  FORMAT(//'  ENTER integer value (J) of streamwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2027  FORMAT(//'  ENTER integer value (K) of quasi-stream surface on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2061  FORMAT(//'  ENTER integer value for number of EXTRA pitches'/
     +         '        you wish to be plotted on each side of '/
     +         '        the actual computational domain.'//
     +         '        Enter a negative value to reset to zero'//
     +         '        DEFAULT ======> ',I4/)
C
C
C
      END
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO PLOT VECTORS
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB16(NAME,X0,Y0,X1,Y1,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      CHARACTER*72 JBNAME,TSNUM
      CHARACTER*32 NAME
      CHARACTER*1  CANS,ABSOL,SECOND
C
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT1 /  DELR, ZSCALE, VMOD,
     &                 NDIFF, I, J, K, IZ, JZ, KZ, IPIT, CTYPE, ANS
C
C
C
C         TELL USER OF STATUS
C
      WRITE (6,2000) JBNAME,NAME
C
C        NOW SET SCALE FACTOR "R"
C
      RX  =  (X1-X0)/( XMAX- XMIN)
      RY  =  (Y1-Y0)/( YMAX- YMIN)
      RZ  =  (X1-X0)/( ZMAX- ZMIN)
      RA  =  (X1-X0)/( RMAX- RMIN)
      RB  =  (Y1-Y0)/( RMAX- RMIN)
      RS  =  (X1-X0)/(SMMAX-SMMIN)
      RT  =  (Y1-Y0)/(RTMAX-RTMIN)
      RR  =  (X1-X0)/(SRMAX-SRMIN)
      RR  = AMIN1 ( RX, RY, RZ, RA, RB, RS, RT, RR )
C
C         DETERMINE TYPE OF PLOT REQUIRED
C
   10 WRITE (6,2020) ANS
      READ  (1,1000) CANS
      IF ( CANS . NE . ' ' ) ANS = CANS
C
C         Find out if to plot absolute or relative vectors
C
      WRITE (6,2008)
      READ  (1,1000) ABSOL
C
      IF ( ABSOL . EQ . 'Y' . OR . ABSOL . EQ . 'y' ) THEN
        DO 40 KK=1,KM
        DO 40 JJ=1,JM
        DO 40 II=1,IM
          VT(II,JJ,KK) = VT(II,JJ,KK)+W(JJ)*R(JJ,KK)
   40   CONTINUE
      ENDIF
C
C         Find out if to plot secondary vectors
C
      WRITE (6,2009)
      READ  (1,1000) SECOND        
C
C
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' . OR . 
     &     ANS . EQ . 'S' . OR . ANS . EQ . 's' . OR . 
     &     ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
   13     WRITE (6,2021) KM, K
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) K = IANS
          IF (K.GT.KM . OR . K.LT.1 ) GO TO 13
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' . OR . 
     &          ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
C
   23     WRITE (6,2022) JM, J
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) J = IANS
          IF (J.GT.JM . OR . J.LT.1 ) GO TO 23
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
   33     WRITE (6,2023) IM, I
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) I = IANS
          IF (I.GT.IM . OR . I.LT.1 ) GO TO 33
C
      ELSE
C
          GO TO 10
      ENDIF
C
C
C         Calculate the vector scale
C
      IMID      = (IM+1)/2
      KMID      = (KM+1)/2
      USCALE    = 0.0
C
      DO 15 JJ=2,JM
       IF((SM(JJ,KMID)-SM(JJ-1,KMID)).GT.0.0000000001)
     & US  = SQRT(VX(IMID,JJ,KMID)**2+VR(IMID,JJ,KMID)**2)
     &           /(SM(JJ,KMID)-SM(JJ-1,KMID))
      IF (US.GT.USCALE) USCALE = US
   15 CONTINUE
C
C        increase scale
C
      USCALE = 1.1 * USCALE
C
C
C          ASK USER IF ALL POINTS ARE TO BE PLOTTED
C
   16 WRITE (6,2006) NDIFF
      IDIFF = IREAD(1)
      IF (IDIFF.NE.0) NDIFF = IDIFF
      IF (NDIFF.LT.1) GO TO 16
C
C          ASK FOR MODIFIED VECTOR SCALE
C
   17 WRITE (6,2007) VMOD
      RANS = RREAD(1)
      IF (RANS.NE.0.) VMOD=RANS
      IF (RANS.LT.0.) GO TO 17
C
      VSCALE = USCALE / FLOAT(NDIFF) / VMOD
C
C      Zoom ?
C
C
      WRITE (6,2024) ZLAST
      ZIN = RREAD(1)
      IF (ZIN.GT.0.01) THEN
          ZSCALE = ZIN
          ZLAST  = ZSCALE
      ELSE 
          ZSCALE = ZLAST
      ENDIF
C
      IF(ZIN.GT.0.01) THEN
C
   35     WRITE (6,2025) IM, IZ
          IANS = IREAD(1)
          IF (IANS.NE.0) IZ = IANS
          IF (IZ.LT.1.OR.IZ.GT.IM) GO TO 35
C
   36     WRITE (6,2026) JM, JZ
          IANS = IREAD(1)
          IF (IANS.NE.0) JZ = IANS
          IF (JZ.LT.1.OR.JZ.GT.JM) GO TO 36
C
   37     WRITE (6,2027) KM, KZ
          IANS = IREAD(1)
          IF (IANS.NE.0) KZ = IANS
          IF (KZ.LT.1.OR.KZ.GT.KM) GO TO 37
C
      ENDIF
C
C         Find number of pitches to be plotted
C
      WRITE (6,2061) IPIT
      IANS = IREAD(1)
      IF ( IANS . NE . 0 ) IPIT = IANS
      IF ( IPIT . LE . 0 ) IPIT = 0
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C      ADD TITLES
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
      XX6 = 175.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 15.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
C
      IF ( SECOND  . EQ .  'Y' . OR . SECOND . EQ . 'y' ) THEN
          CALL TITLE(XX4,YY2,XX5,YY2,0,
     &             '   SECONDARY VELOCITY VECTORS   ',32)
      ELSE IF ( ABSOL  . EQ .  'Y' . OR . ABSOL . EQ . 'y' ) THEN
          CALL TITLE(XX4,YY2,XX5,YY2,0,
     &             '   ABSOLUTE VELOCITY VECTORS    ',32)
      ELSE
          CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
      ENDIF
C
C
      S = (XX1-XX0)/80./3.0
      CALL SCLCHR( S, S )
      CALL BGNWRT(XX6,YY2,1)
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
          CALL WRITEA('*','(A30)',' RADIAL PROJECTION OF S-S. No.')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          CALL WRITEA('*','(A30)','  AXIAL PROJECTION OF S-S. No.')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL WRITEA('*','(A30)','         STREAM SURFACE NUMBER')
          CALL WRITEI('*','(I4)',K)
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
          CALL WRITEA('*','(A30)','  AXIAL PROJECTION OF Q-O. No.')
          CALL WRITEI('*','(I4)',J)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL WRITEA('*','(A30)','       QUASI-ORTHOGONAL NUMBER')
          CALL WRITEI('*','(I4)',J)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL WRITEA('*','(A30)','     MERIDIONAL SURFACE NUMBER')
          CALL WRITEI('*','(I4)',I)
      ENDIF
      CALL WRITEA(' ','(A6)','ZOOM =')
      CALL WRITEF('*','(F6.1)',ZSCALE)
      CALL WRITEA('*','(A7)',' AT I =')
      CALL WRITEI('*','(I4)',IZ)
      CALL WRITEA('*','(A5)',', J =')
      CALL WRITEI('*','(I4)',JZ)
      CALL WRITEA('*','(A5)',', K =')
      CALL WRITEI('*','(I4)',KZ)
      CALL SCLCHR ( 1.0/1.0, 1.0/1.0 )
      CALL WRITEA(' ','(A8)','VSCALE =')
      CALL WRITEF('*','(F7.1)',VMOD)
      CALL SCLCHR ( 1.0/1.0, 1.0/1.0 )
C
C         SET WINDOW
C
      CALL AWINDW ( X0, Y0, X1, Y1, -1 )
      CALL ORIGIN( 0.5*(X0+X1), 0.5*(Y0+Y1), 1 )
      CALL SCALE ( RR, RR )
C
C        SHIFT ORIGIN TO CENTRE PLOT
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
          CALL ORIGIN( -0.5*(XMIN+XMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( X(JZ,KZ), R(JZ,KZ)*(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
          CALL ORIGIN( -0.5*(ZMIN+ZMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL ORIGIN( -0.5*(SMMIN+SMMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( SM(JZ,KZ), R(JZ,KZ)*(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL ORIGIN( -0.5*(SRMIN+SRMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( (X(JZ,KZ)-X(JZ,1))*COSA(JZ)
     &              +(R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID)-R(JZ,1))*SINA(JZ),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
          CALL ORIGIN( -0.5*(ZMIN+ZMAX), -0.5*(RTMAX+RTMIN), 0 )
          CALL ZOOM( R(JZ,KZ)*COS(T(IZ,JZ,KZ)-TMID),
     &               R(JZ,KZ)*SIN(T(IZ,JZ,KZ)-TMID),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL ORIGIN( -0.5*(XMIN+XMAX), -0.5*(RMIN+RMAX), 0 )
          CALL ZOOM( X(JZ,KZ), R(JZ,KZ),
     &               ZSCALE, ZSCALE )
          CALL SCLCHR (1./zscale,1./zscale)
      ENDIF
C
C     Loop over several pitches
C
      DO 300 NPIT = -IPIT,IPIT,1
C
C        PLOT MESH
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' . OR . 
     &     ANS . EQ . 'S' . OR . ANS . EQ . 's' . OR . 
     &     ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
C         Plot vectors on circumferential plane or radial projection
C
          DO 44 J=1,JM,NDIFF
C
              DO 45 I=1,IM,NDIFF
C
C                 Convert to secondary velocities if required
C
                  IF ( SECOND . EQ . 'Y' . OR . SECOND . EQ . 'y' ) THEN
                      JP1    = MIN0 ( JM, J+1 )
                      JM1    = MAX0 (  1, J-1 )
                      SX     = X(JP1,K) - X(JM1,K)
                      SR     = R(JP1,K) - R(JM1,K)
                      SRT    = R(J,K)*T(I,JP1,K) - R(J,K)*T(I,JM1,K)
                      SL     = SQRT(SX**2+SR**2+SRT**2)
C
                      VPROJ  = VX(I,J,K)*SX/SL + VR(I,J,K)*SR/SL 
     &                                         + VT(I,J,K)*SRT/SL
C
                      VVX  = VX(I,J,K) - SX /SL*VPROJ
                      VVR  = VR(I,J,K) - SR /SL*VPROJ
                      VVT  = VT(I,J,K) - SRT/SL*VPROJ
                  ELSE
                      VVX  = VX(I,J,K)
                      VVR  = VR(I,J,K)
                      VVT  = VT(I,J,K)
                  ENDIF
C
                  IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
C
C                      Note that VT is perpendicular to a line of constant T
C                     
                      JP1    = MIN0(J+1,JM)
                      JM1    = MAX0(J-1, 1)
                      CC     = (X(JP1,K)-X(JM1,K))
                      SS     = (R(JP1,K)-R(JM1,K))
     &                         *(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      VPROJ  = VVX
                      DVX    = (VPROJ*CC/SQRT(CC**2+SS**2)          )
     &                         /VSCALE 
                      DVY    = (VPROJ*SS/SQRT(CC**2+SS**2)+VVT)
     &                         /VSCALE 
                      XS     = X(J,K)-DVX*0.5
                      YS     = R(J,K)*(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -DVY*0.5
                  ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
                      DVX    = (VVR*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -VVT*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K)))
     &                         /VSCALE
                      DVY    = (VVT*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         +VVR*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K)))
     &                         /VSCALE
                      XS     = R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -DVX*0.5
                      YS     = R(J,K)*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -DVY*0.5
                  ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
                      JP1    = MIN0(J+1,JM)
                      JM1    = MAX0(J-1, 1)
C
C                      Project meridional velocity onto grid line
C
                      CC     = (X(JP1,K)-X(JM1,K))
                      SS     = (R(JP1,K)-R(JM1,K))
                      VPROJ  = (VVX*CC+VVR*SS)
     &                         /SQRT(CC**2+SS**2)
C
C                      Note that VT is perpendicular to a line of constant T
C                     
                      CC     = (SM(JP1,K)-SM(JM1,K))
                      SS     = (R(JP1,K)-R(JM1,K))
     &                         *(T(I,J,K)-TMID+NPIT*PITCH(J,K))
                      DVX    = (VPROJ*CC/SQRT(CC**2+SS**2)          )
     &                         /VSCALE 
                      DVY    = (VPROJ*SS/SQRT(CC**2+SS**2)+VVT)
     &                         /VSCALE 
                      XS     = SM(J,K) - DVX*0.5
                      YS     = R(J,K)*(T(I,J,K)-TMID+NPIT*PITCH(J,K)) 
     &                         - DVY*0.5
                  ENDIF
C
                  XF     = XS + DVX
                  YF     = YS + DVY
C
C                    ARRL=ARROW HEAD LENGTH
C
                  ARRL   = SQRT(DVX*DVX+DVY*DVY)/6.
C
                  IF (DVX . EQ . 0.0) THEN
                      IF (DVY.LT.0.0) THEN
                          BETA = -1.57080
                      ELSE IF (DVY.GT.0.0) THEN
                          BETA =  1.57080
                      ELSE
                          GO TO 45
                      ENDIF
                  ELSE
                      BETA  = ATAN2(DVY,DVX)
                  ENDIF
C
                  X3     = XF-ARRL*COS(.4-BETA)
                  Y3     = YF+ARRL*SIN(.4-BETA)
                  X5     = XF-ARRL*COS(.4+BETA)
                  Y4     = YF-ARRL*SIN(.4+BETA)
C
C                    DRAW ARROWS
C
                  CALL MOVETO(XS,YS)
                  CALL DRAWTO(XF,YF)
                  CALL MOVETO(X3,Y3)
                  CALL DRAWTO(XF,YF)
                  CALL DRAWTO(X5,Y4)
C
   45         CONTINUE
   44     CONTINUE
C
C
      ELSE IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' . OR . 
     &          ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
C         Plot vectors on quasi orthogonal plane or its axial projection
C
          DO 54 K=1,KM,NDIFF
C
              DO 55 I=1,IM,NDIFF
C
C                 Convert to secondary velocities if required
C
                  IF ( SECOND . EQ . 'Y' . OR . SECOND . EQ . 'y' ) THEN
                      JP1    = MIN0 ( JM, J+1 )
                      JM1    = MAX0 (  1, J-1 )
                      SX     = X(JP1,K) - X(JM1,K)
                      SR     = R(JP1,K) - R(JM1,K)
                      SRT    = R(J,K)*T(I,JP1,K) - R(J,K)*T(I,JM1,K)
                      SL     = SQRT(SX**2+SR**2+SRT**2)
C
                      VPROJ  = VX(I,J,K)*SX/SL + VR(I,J,K)*SR/SL 
     &                                         + VT(I,J,K)*SRT/SL
C
                      VVX  = VX(I,J,K) - SX /SL*VPROJ
                      VVR  = VR(I,J,K) - SR /SL*VPROJ
                      VVT  = VT(I,J,K) - SRT/SL*VPROJ
                  ELSE
                      VVX  = VX(I,J,K)
                      VVR  = VR(I,J,K)
                      VVT  = VT(I,J,K)
                  ENDIF
C
                  IF ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' ) THEN
                      DVX    = (VVR*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -VVT*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K)))
     &                         /VSCALE
                      DVY    = (VVT*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         +VVR*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K)))
     &                         /VSCALE
                      XS     = R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -DVX*0.5
                      YS     = R(J,K)*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -DVY*0.5
                  ELSE 
                      DVX    = (VVR*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -VVT*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K)))
     &                         /VSCALE
                      DVX    = VVX*COSA(J)/VSCALE + DVX*SINA(J)
                      DVY    = (VVT*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         +VVR*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K)))
     &                         /VSCALE
                      XS     = (X(J,K)-X(J,1))*COSA(J)
     &                       +(R(J,K)*COS(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -R(J,1))*SINA(J)-DVX*0.5
                      YS     = R(J,K)*SIN(T(I,J,K)-TMID+NPIT*PITCH(J,K))
     &                         -DVY*0.5
                  ENDIF
C
                  XF     = XS + DVX
                  YF     = YS + DVY
C
C                    ARRL=ARROW HEAD LENGTH
C
                  ARRL   = SQRT(DVX*DVX+DVY*DVY)/6.
C
                  IF (DVX . EQ . 0.0) THEN
                      IF (DVY.LT.0.0) THEN
                          BETA = -1.57080
                      ELSE IF (DVY.GT.0.0) THEN
                          BETA =  1.57080
                      ELSE
                          GO TO 55
                      ENDIF
                  ELSE
                      BETA  = ATAN2(DVY,DVX)
                  ENDIF
C
                  X3     = XF-ARRL*COS(.4-BETA)
                  Y3     = YF+ARRL*SIN(.4-BETA)
                  X5     = XF-ARRL*COS(.4+BETA)
                  Y4     = YF-ARRL*SIN(.4+BETA)
C
C                    DRAW ARROWS
C
                  CALL MOVETO(XS,YS)
                  CALL DRAWTO(XF,YF)
                  CALL MOVETO(X3,Y3)
                  CALL DRAWTO(XF,YF)
                  CALL DRAWTO(X5,Y4)
C
   55         CONTINUE
   54     CONTINUE
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
C         Plot vectors on circumferential plane
C
          IF ( NPIT . EQ . 0 ) THEN
            DO 64 J=1,JM,NDIFF
C
              DO 65 K=1,KM,NDIFF
C
C                 Convert to secondary velocities if required
C
                  IF ( SECOND . EQ . 'Y' . OR . SECOND . EQ . 'y' ) THEN
                      JP1    = MIN0 ( JM, J+1 )
                      JM1    = MAX0 (  1, J-1 )
                      SX     = X(JP1,K) - X(JM1,K)
                      SR     = R(JP1,K) - R(JM1,K)
                      SRT    = R(J,K)*T(I,JP1,K) - R(J,K)*T(I,JM1,K)
                      SL     = SQRT(SX**2+SR**2+SRT**2)
C
                      VPROJ  = VX(I,J,K)*SX/SL + VR(I,J,K)*SR/SL 
     &                                         + VT(I,J,K)*SRT/SL
C
                      VVX  = VX(I,J,K) - SX /SL*VPROJ
                      VVR  = VR(I,J,K) - SR /SL*VPROJ
                      VVT  = VT(I,J,K) - SRT/SL*VPROJ
                  ELSE
                      VVX  = VX(I,J,K)
                      VVR  = VR(I,J,K)
                      VVT  = VT(I,J,K)
                  ENDIF
C
                  DVX    = VVX/VSCALE
                  DVY    = VVR/VSCALE
                  XS     = X(J,K)-DVX*0.5
                  YS     = R(J,K)-DVY*0.5
                  XF     = XS + DVX
                  YF     = YS + DVY
C
C                    ARRL=ARROW HEAD LENGTH
C
                  ARRL   = SQRT(DVX*DVX+DVY*DVY)/6.
C
                  IF (DVX . EQ . 0.0) THEN
                      IF (DVY.LT.0.0) THEN
                          BETA = -1.57080
                      ELSE IF (DVY.GT.0.0) THEN
                          BETA =  1.57080
                      ELSE
                          GO TO 65
                      ENDIF
                  ELSE
                      BETA  = ATAN2(DVY,DVX)
                  ENDIF
C
                  X3     = XF-ARRL*COS(.4-BETA)
                  Y3     = YF+ARRL*SIN(.4-BETA)
                  X5     = XF-ARRL*COS(.4+BETA)
                  Y4     = YF-ARRL*SIN(.4+BETA)
C
C                    DRAW ARROWS
C
                  CALL MOVETO(XS,YS)
                  CALL DRAWTO(XF,YF)
                  CALL MOVETO(X3,Y3)
                  CALL DRAWTO(XF,YF)
                  CALL DRAWTO(X5,Y4)
C
   65         CONTINUE
   64       CONTINUE
C
          ENDIF
C
      ENDIF
C
  300 CONTINUE
C
C
C
C      DRAW BLADE
C
      CALL SUB7
      CALL SUB28
C
C
C
C        TRANSFORM FROM (ABSOLUTE) VELOCITIES
C
      IF ( ABSOL . EQ . 'Y' . OR . ABSOL . EQ . 'y' ) THEN
        DO 60 K=1,KM
        DO 60 J=1,JM
        DO 60 I=1,IM
          VT(I,J,K) = VT(I,J,K)-W(J)*R(J,K)
   60   CONTINUE
      ENDIF
C
C
      CALL BRKPLT
C
C     Make sure no bad defaults
C
      I = MAX0 ( MIN0 ( IM, I ), 1 )
      J = MAX0 ( MIN0 ( JM, J ), 1 )
      K = MAX0 ( MIN0 ( KM, K ), 1 )
C
      RETURN
C
C
1000  FORMAT(A1)
C
C
2000  FORMAT(' '//' ',A80//' ',A32//' VECTOR PLOT SELECTED '/)
2006  FORMAT(' '/,' ENTER "1" TO PLOT EVERY POINT,'/
     +            ' ENTER "2" TO PLOT EVERY 2ND POINT ...ETC'/
     +            ' DEFAULT VALUE IS EVERY',I4/)
2007  FORMAT(' '/,' ENTER VECTOR SCALE MULTIPLIER '/
     &            ' DEFAULT =',F5.1/)
2008  FORMAT(' '/,' DO YOU WANT ABSOLUTE VECTORS ?'/)
2009  FORMAT(' '/
     +       ' DO YOU WANT SECONDARY FLOW VECTORS ?'/
     +       '  - the secondary flow is defined as being normal to'/
     +       '    the local quasi-streamline (J-lines). The vectors'/
     +       '    are projected onto the plane you chose and so may'/
     +       '    give a false impression of the true magnitudes in'/
     +       '    some directions'/)
2020  FORMAT('    TYPE :-'//
     &       '   "M"  for Meridional Plot'/
     &       '   "Q"  for Quasi-Orthogonal Plot'/
     &       '   "R"  for Radial projection of Quasi Stream-surface'/
     &       '   "S"  for Quasi Stream-surface'/
     &       '   "T"  for Axial projection of Quasi Stream-surface'/
     &       '   "X"  for Axial projection of Quasi-Orthogonal'//
     &       '   "',A1,'" <=== DEFAULT'/)
2021  FORMAT(//'  ENTER integer value (K) of quasi-stream surface for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2022  FORMAT(//'  ENTER integer value (J) of quasi-orthogonal for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2023  FORMAT(//'  ENTER integer value (I) of quasi-meridional for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2024  FORMAT(//'  ENTER Zoom magnification that you require'/
     +         '        DEFAULT ======>',F5.1/)
2025  FORMAT(//'  ENTER integer value (I) of pitchwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2026  FORMAT(//'  ENTER integer value (J) of streamwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2027  FORMAT(//'  ENTER integer value (K) of quasi-stream surface on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2061  FORMAT(//'  ENTER integer value for number of EXTRA pitches'/
     +         '        you wish to be plotted on each side of '/
     +         '        the actual computational domain.'//
     +         '        Enter a negative value to reset to zero'//
     +         '        DEFAULT ======> ',I4/)
C
C
C
      END
C*******************************************************************     
C                                                                  C
C         Subroutine to DRAW boxes                                 C
C         with annotation                                          C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB19(X0,Y0,XMIN,YMIN,X1,Y1,XMAX,YMAX,XINC,YINC,
     +                XTIC,YTIC,NXTIC,NYTIC,IFX,IFY,X,Y,NPTS,NCHAR,
     +                IBOX,XXLABEL,YYLABEL)
C
      DIMENSION X(NPTS),Y(NPTS)
C
      CHARACTER*32 XXLABEL,YYLABEL
      CHARACTER*6  IFX,IFY
      CHARACTER*1  NCHAR
C
C       Set up axes etc
C
      DX = X1 - X0
      DY = Y1 - Y0
C
      RX   =  DX/(XMAX-XMIN)
      RY   =  DY/(YMAX-YMIN)
C
C       Draw axes
C
      IF (IBOX . EQ . 1) GO TO 200
C
      XN    = FLOAT(NXTIC+1)
      YN    = FLOAT(NYTIC+1)
C
      XNEG  = -1.0*XTIC
      YNEG  = -1.0*YTIC
C
      CALL AXIS(X0,Y0,XMIN,X1,Y0,XMAX,(XINC/XN),XNEG,  0  ,'(F0.0)')
      CALL AXIS(X0,Y1,XMIN,X1,Y1,XMAX,(XINC/XN),XTIC,  0  ,'(F0.0)')
      CALL AXIS(X0,Y0,YMIN,X0,Y1,YMAX,(YINC/YN),YNEG,  0  ,'(F0.0)')
      CALL AXIS(X1,Y0,YMIN,X1,Y1,YMAX,(YINC/YN),XTIC,  0  ,'(F0.0)')
      CALL AXIS(X0,Y0,YMIN,X0,Y1,YMAX, YINC    ,.01 ,  0  ,IFY)
      CALL AXIS(X0,Y0,XMIN,X1,Y0,XMAX, XINC    ,.01 ,  0  ,IFX)
C
      CALL XLABEL( XXLABEL,32 )
      CALL YLABEL( YYLABEL,32 )
C
C        Draw the points 
C
  200 IF (NCHAR . EQ . (' ')) GO TO 270
C
      DO 250 I=1,NPTS
      CALL PLTSYM((X(I)-XMIN)*RX+X0,(Y(I)-YMIN)*RY+Y0,NCHAR)
  250 CONTINUE
C
      IF (IBOX.GE.0) RETURN
C
  270 CALL MOVETO((X(1)-XMIN)*RX+X0,(Y(1)-YMIN)*RY+Y0)
C
      DO 280 I=2,NPTS
      CALL DRAWTO((X(I)-XMIN)*RX+X0,(Y(I)-YMIN)*RY+Y0)
  280 CONTINUE
C
      RETURN
C
C
      END
C******************************************************************C
C                                                                 C
C                        SUB20                                    C
C                                                                 C
C******************************************************************C
C
      SUBROUTINE SUB20(AMAX,AMIN,AINCR)
C
      RANGE = AMAX-AMIN
      AINCR = RANGE/10.
C
      ALOGA = ALOG10(AINCR)
C
      K     = ALOGA
      IF (ALOGA.LT.0.) K = K-1
      AINCR = AINCR*10.**(-K)
C
      IF (AINCR.GT.1.0.AND.AINCR.LE.2.0) AINCR=2.*10.**K
      IF (AINCR.GT.2.0.AND.AINCR.LE.2.5) AINCR=2.5*10.0**K
      IF (AINCR.GT.2.5.AND.AINCR.LE.5.0) AINCR=5.0*10.0**K
      IF (AINCR.GT.5.0.AND.AINCR.LE.10.) AINCR=10.**(K+1)
C
C         CREATE GRAPH LIMITS
C
      NK     = AMIN/AINCR
      AMIN   = FLOAT(NK)*AINCR
      AMAX   = (FLOAT(NK+10))*AINCR
C
      AINCR  = AINCR * 2.0
C
      RETURN
      END
C*******************************************************************C
C
C            SUBROUTINE TO SELECT FLOW VARIABLES
C
C******************************************************************C
C
      SUBROUTINE SUB22(PROP,NPROP,FMT,IREPLY)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK9 / P(95,2500,95)
      COMMON / BLK10/ RO(95,2500,95) 
      COMMON / BLK11/ M_ABS(95,2500,95),M_REL(95,2500,95)
      COMMON / BLK14/ Q(95,2500,95),QQ(95,2500,95),NQ,NQQ
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
C
      DIMENSION PROP(95,2500,95),PROP2(95,2500,95),ROVM(2500),
     &          POINPUT(2500), TOINPUT(2500), PROP3(95,2500,95),
     &  FRACFLO(2500,95),FFLO_REF(95),TKSS_REF(95),FFLO(95),TKSS(95)
C
      COMMON /BLKPRIM/ ROVX(95,2500,95),ROVR(95,2500,95),
     &                 ROVT(95,2500,95),ROE(95,2500,95),
     &                 ENTPY(95,2500,95),GA_PV(95,2500,95),
     &                 H_TO_T(95,2500,95),WET(95,2500,95),
     &                 P_STAGN(95,2500,95),T_STAGN(95,2500,95),
     &                 T_STATIC(95,2500,95),PO_REL(95,2500,95),
     &                 HO(95,2500,95)
C
      DOUBLE PRECISION PSTAT, TSTAT
C
      CHARACTER*32 NPROP,NQ,NQQ
      CHARACTER*72 JBNAME,TSNUM
      CHARACTER*8  FMT
      CHARACTER*1  ANS, ANS_LOS
C
      REAL M_ABS, M_REL, EMACH, IMACH
C
      GC     = CP*(GA-1.0)/GA
      RAD    = 3.14159/180.
      DEG    = 1.0/RAD
      IMM1   = IM - 1
      KMID   = (KM+1)/2
      IMID   = (IM+1)/2
      JMID   = (JM+1)/2
C
C*******************************************************************************
C  CHOOSE THE VARIABLE TO BE PLOTTED
C

      IF (IREPLY . EQ . 1) THEN
        DO 1 K=1,KM
        DO 1 J=1,JM
        DO 1 I=1,IM
          PROP(I,J,K) = VX(I,J,K)
    1   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '       AXIAL VELOCITY           '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 2) THEN
        DO 2 K=1,KM
        DO 2 J=1,JM
        DO 2 I=1,IM
          PROP(I,J,K) = VT(I,J,K)
    2   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '   RELATIVE TANGENTIAL VELOCITY    '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 3) THEN
        DO 3 K=1,KM
        DO 3 J=1,JM
        DO 3 I=1,IM
          PROP(I,J,K) = RO(I,J,K)
    3   CONTINUE
        FMT         = '(F6.3)  '
        NPROP       = '      STATIC  DENSITY              '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 4) THEN
        DO 4 K=1,KM
        DO 4 J=1,JM
        DO 4 I=1,IM
          PROP(I,J,K) = P(I,J,K)
    4   CONTINUE
        FMT         = '(F9.1)  '
        NPROP       = '       STATIC PRESSURE, N/M**2 '
C
C*******************************************************************************
C        '
      ELSE IF(IREPLY . EQ . 5) THEN
C
        WRITE(6,*) ' RELATIVE STAGNATION PRESSURE '
C        
        DO 5 K=1,KM
        DO 5 J=1,JM
        DO 5 I=1,IM
           PROP(I,J,K) = PO_REL(I,J,K)
    5   CONTINUE
C
             NPROP = ' RELATIVE STAGNATION PRESSURE   '
             FMT         = '(F10.1)  '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 6) THEN
        DO 6 K=1,KM
        DO 6 J=1,JM
        DO 6 I=1,IM
          PROP(I,J,K) = M_REL(I,J,K)
    6   CONTINUE
        FMT         = '(F6.3)  '
        NPROP       = ' RELATIVE MACH NUMBER           '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 7) THEN
        DO 7 K=1,KM
        DO 7 J=1,JM
        DO 7 I=1,IM
          PROP(I,J,K) = RO(I,J,K)*VX(I,J,K)
    7   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '            RO * VX             '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 8) THEN
        DO 8 K=1,KM
        DO 8 J=1,JM
        DO 8 I=1,IM
          PROP(I,J,K) = VT(I,J,K) + W(J)*R(J,K)
    8   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '  ABSOLUTE TANGENTIAL VELOCITY '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 9) THEN
        DO 9 K=1,KM
        DO 9 J=1,JM
        DO 9 I=1,IM
           PROP(I,J,K) = P(I,J,K)/1.0E5
    9   CONTINUE
        FMT         = '(F8.5)'
        NPROP       = 'STATIC PRESSURE IN BAR    '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 10) THEN
        DO 10 K=1,KM
        DO 10 J=1,JM
        DO 10 I=1,IM
          PROP(I,J,K) = SQRT( VX(I,J,K)**2 + VT(I,J,K)**2
     &                      + VR(I,J,K)**2 )
   10   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '  RELATIVE  VELOCITY             '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 11) THEN
        DO 11 K=1,KM
        DO 11 J=1,JM
        DO 11 I=1,IM
          PROP(I,J,K) = T_STATIC(I,J,K)
   11   CONTINUE
        FMT         = '(F7.1)  '
        NPROP       = '       STATIC TEMPERATURE, deg K      '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 12) THEN
        DO 12 K=1,KM
        DO 12 J=1,JM
        DO 12 I=1,IM
          PROP(I,J,K) = HO(I,J,K)/1000.0
   12   CONTINUE
        FMT         = '(F7.1)  '
        NPROP       = 'ABSOLUTE STAGN. ENTHALPY, kJ/Kg    '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 13) THEN
        DO 13 K=1,KM
        DO 13 J=1,JM
        DO 13 I=1,IM
C
C       Calculate Flow angle
C
          VMER = SQRT ( VX(I,J,K)**2 + VR(I,J,K)**2 )
          IF (VMER . EQ . 0.0) THEN
              IF (VT(I,J,K).LT.0.0) THEN
                  BETA = -1.57080
              ELSE IF (VT(I,J,K) . EQ . 0.0) THEN
                  JP1    = J+1
                  JM1    = J-1
                  IF (JM1.LT.1) JM1    =  1
                  IF (JP1.GT.JM) JP1   = JM
                  BETA  = ATAN2((R(J,K)*T(I,JP1,K)
     &                          -R(J,K)*T(I,JM1,K)),
     &                          (SM(JP1,K)-SM(JM1,K)))
              ELSE IF (VT(I,J,K).GT.0.0) THEN
                  BETA  = 1.57080
              ENDIF
C
          ELSE
              BETA  = ATAN2(VT(I,J,K),VMER)
          ENDIF
C
          PROP(I,J,K) = BETA * DEG
   13   CONTINUE
        FMT         = '(F7.1)  '
        NPROP       = ' RELATIVE PITCHWISE FLOW ANGLE '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 16) THEN
        DO 14 K=1,KM
        DO 14 J=1,JM
        DO 14 I=1,IM
C
C       Calculate Radial Flow angle
C
          IF (VX(I,J,K) . EQ . 0.0) THEN
              IF (VR(I,J,K).LT.0.0) THEN
                  BETA = -1.57080
              ELSE IF (VR(I,J,K) . EQ . 0.0) THEN
                  JP1    = J+1
                  JM1    = J-1
                  IF (JM1.LT.1) JM1    =  1
                  IF (JP1.GT.JM) JP1   = JM
                  BETA  = ATAN2((R(JP1,K)-R(JM1,K)),
     &                          (X(JP1,K)-X(JM1,K)))
              ELSE IF (VR(I,J,K).GT.0.0) THEN
                  BETA  = 1.57080
              ENDIF
C
          ELSE
              BETA  = ATAN2(VR(I,J,K),VX(I,J,K))
          ENDIF
          PROP(I,J,K) = BETA * DEG
   14   CONTINUE
        FMT         = '(F7.1)  '
        NPROP       = '       RADIAL FLOW ANGLE        '
C
C*******************************************************************************
C
      ELSE IF (IREPLY . EQ . 17) THEN
        DO 17 K=1,KM
        DO 17 J=1,JM
        DO 17 I=1,IM
          PROP(I,J,K) = VR(I,J,K)
   17   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '      RADIAL VELOCITY           '
C
C*******************************************************************************
C
      ELSE IF (IREPLY . EQ . 18) THEN
        DO 18 K=1,KM
        DO 18 J=1,JM
        DO 18 I=1,IM
          PROP(I,J,K) = R(J,K)*(VT(I,J,K) + W(J)*R(J,K))
   18   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = '          R * VTHETA-ABS         '
C
C*******************************************************************************
C
      ELSE IF (IREPLY . EQ . 19) THEN
C
        WRITE (6,2083) IMID, KMID
C
        DO 19 K=1,KM
        DO 19 J=1,JM
        DO 19 I=1,IM
          PROP(I,J,K) = ENTPY(I,J,K)
   19   CONTINUE
        FMT         = '(F12.1)  '
        NPROP       = '      TRUE ENTROPY, J/Kg K       '
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 20) THEN
C
        DO 20 K=1,KM
        DO 20 J=1,JM
        DO 20 I=1,IM
          PROP(I,J,K) = T_STAGN(I,J,K)
   20   CONTINUE
C
        FMT         = '(F7.2)  '
        NPROP       = 'ABSOLUTE STAGN. TEMPERATURE, K   '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 21) THEN
        DO 21 K=1,KM
        DO 21 J=1,JM
        DO 21 I=1,IM
        PROP(I,J,K) = P_STAGN(I,J,K)
   21   CONTINUE
        FMT         = '(F9.1)  '
        NPROP       = 'ABSOLUTE STAGN PRESSURE N/m**2    '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 22) THEN
        DO 22 K=1,KM
        DO 22 J=1,JM
        DO 22 I=1,IM
          U           = W(J)*R(J,K)
          PROP(I,J,K) = P(I,J,K) - 0.5*RO(I,J,K)*U*U
   22   CONTINUE
        FMT         = '(F9.1)  '
        NPROP       = '  REDUCED STATIC PRESSURE (P*)  '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 25) THEN
        DO 25 K=1,KM
        DO 25 J=1,JM
        DO 25 I=1,IM
          U           = W(J)*R(J,K)
          PROP(I,J,K) = SQRT( VX(I,J,K)**2 + (VT(I,J,K)+U)**2
     &                      + VR(I,J,K)**2 )
   25   CONTINUE
        FMT         = '(F10.2)  '
        NPROP       = '       ABSOLUTE VELOCITY        '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 26) THEN
        DO 26 K=1,KM
        DO 26 J=1,JM
        DO 26 I=1,IM
        PROP(I,J,K) = M_ABS(I,J,K)
   26   CONTINUE
        FMT         = '(F9.4)  '
        NPROP       = '      ABSOLUTE MACH NUMBER      '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 27) THEN
        DO 27 K=1,KM
        DO 27 J=1,JM
        DO 27 I=1,IM
          U           = W(J)*R(J,K)
          VMER = SQRT ( VX(I,J,K)**2 + VR(I,J,K)**2 )
          VTAB = VT(I,J,K) + U
          BETA  = ATAN2(VTAB,VMER)
          PROP(I,J,K) = BETA * DEG
   27   CONTINUE
        FMT         = '(F8.2)  '
        NPROP       = 'ABSOLUTE PITCHWISE FLOW ANGLE   '
C
C*******************************************************************************
C
      ELSE IF(IREPLY . EQ . 28) THEN
C
        WRITE (6,2083) IMID, KMID
C
        FMACH  = (1.0+(GA-1.0)*0.5*M_ABS(imid,1,kmid)**2)
c
        uin   = W(1)*r(1, kmid)
        TOIN  = T_STAGN(IMID,1,KMID)      
        POIN  = P_STAGN(IMID,1,KMID)
        TOREL_IN = TOIN - 0.5*uin**2/cp
c
        DO 28 K=1,KM
        DO 28 J=1,JM
        DO 28 I=1,IM
             U       = W(J)*R(J,K)
             TOREL   = TOREL_IN + 0.5*u**2/cp
             PO_ISENT= (TOREL/TOIN)**(ga/(ga-1.0))*POIN    
             IF (PO_ISENT/p(i, j, k).LT.1.0) THEN
                 PROP(I,J,K) = 0.0
             ELSE
                 PROP(I,J,K) = SQRT(((PO_ISENT/p(i, j, k))**
     &                         ((ga-1.)/ga)-1.)*2./(ga-1.))
             ENDIF
   28   CONTINUE
c
        FMT         = '(F9.4)  '
        NPROP       = '    ISENTROPIC REL MACH No.     '
C
C*******************************************************************************
C
      ELSE IF (IREPLY . EQ . 29) THEN
C
        IMM1 = IM-1
        KMM1 = KM-1
        PI   = ACOS(-1.)
C
        DO  290  J=1,JM
C
          DO 291 I=1,IM
            PROP(I,J,1) = 0.0
  291     CONTINUE
C
          DO 292 K=1,KMM1
            SUMFLOW = 0.0
            DO 293 I=1,IMM1
              X1 = X(J,K+1)-X(J,K)
              X2 = X1
              R1 = R(J,K+1)-R(J,K)
              R2 = R1
              T1 = R(J,K+1)*(T(I+1,J,K+1)-T(I,J,K))
              T2 = R(J,K+1)*(T(I,J,K+1)-T(I,J,K))
     &            -R(J,K)*(T(I+1,J,K)-T(I,J,K))
              AQX = 0.5*(R2*T1 - R1*T2)
              AQR = 0.5*(X1*T2 - X2*T1)
              ROVXAV = RO(I,J,K)*VX(I,J,K)
     &                +RO(I,J,K+1)*VX(I,J,K+1)
     &                +RO(I+1,J,K)*VX(I+1,J,K)
     &                +RO(I+1,J,K+1)*VX(I+1,J,K+1)
              ROVRAV = RO(I,J,K)*VR(I,J,K)
     &                +RO(I,J,K+1)*VR(I,J,K+1)
     &                +RO(I+1,J,K)*VR(I+1,J,K)
     &                +RO(I+1,J,K+1)*VR(I+1,J,K+1)
              DFLOW = AQX*ROVXAV + AQR*ROVRAV
              SUMFLOW = SUMFLOW + DFLOW
  293       CONTINUE
C
            PROP(1,J,K+1) = PROP(1,J,K) + 0.25*SUMFLOW*2.*PI/PITCH(J,K)
C
            DO 294 I=2,IM
              PROP(I,J,K+1) = PROP(1,J,K+1)
  294       CONTINUE
C
  292     CONTINUE
  290   CONTINUE
C
        FMT         = '(F9.4)  '
        NPROP       = '     AVERAGE MASS FLOW RATE     '
C
C*******************************************************************************
C LOST EFFICIENCY OR ENTROPY LOSS COEFFICIENT CALCULATION
C

      ELSE IF(IREPLY.EQ.30)  THEN
C     
        DO 555 K=1,KM
        DO 555 J=1,JM
        DO 555 I=1,IM
          QQ(I,J,K)    = T_STATIC(I,J,K)
          Q(I,J,K)     = HO(I,J,K)
          PROP2(I,J,K) = ENTPY(I,J,K)
          WRELSQ = VX(I,J,K)**2 + VR(I,J,K)**2 + VT(I,J,K)**2
          PROP3(I,J,K) = 0.5*WRELSQ
  555   CONTINUE
C
         WRITE(6,*) 'DO YOU WANT TO USE A PITCHWISE AVERAGE OR A PASSAGE 
     & AVERAGE OF THE ENTROPY ?  '
         WRITE(6,*) ' ANSWER  "P"   or  "G" '
         READ(1,*)  ANS
         IFTOT = 0
         IF(ANS.EQ.'P'.OR.ANS.EQ.'p') IFTOT = 0
         IF(ANS.EQ.'G'.OR.ANS.EQ.'g') IFTOT = 1
C
         CALL SUB23(QQ,IFTOT)
         CALL SUB23(PROP2,IFTOT)
         CALL SUB23(PROP3,IFTOT)
         CALL SUB23(Q,IFTOT)
C
         HDIFF      = Q(IMID,JM,KMID) - Q(IMID,1,KMID)
         UBLADE_SQ  = (W(JM)*R(JM,KMID))**2
         IF(ABS(HDIFF).GT.0.1*UBLADE_SQ) THEN
             DENOM  = ABS(HDIFF)
         ELSE
             DENOM = PROP3(IMID,JM,KMID)
         END IF
C
         DO 556 J=1,JM
         DO 556 K=1,KM
         DO 556 I=1,IM
         PROP(I,J,K) = QQ(IMID,JM,KMID)*(PROP2(I,J,K) - PROP2(I,1,K))
     &               / DENOM
  556    CONTINUE
C
         IF(ABS(HDIFF).GT.0.1*UBLADE_SQ) THEN     
             NPROP = ' LOST EFFICIENCY          '
             FMT         = '(F10.4)  '
         ELSE
             NPROP = ' ENTROPY LOSS COEFFICIENT '
             FMT   =  '(F10.5)   '
        END IF
C
C*******************************************************************************
C
      ELSE IF(IREPLY.EQ.33) THEN
         DO 559 J=1,JM
         DO 559 K=1,KM
         DO 559 I=1,IM
         PROP(I,J,K) = WET(I,J,K)
  559    CONTINUE
             NPROP = ' WETNESS  '
             FMT   = '(F9.5)  '
C
C********************************************************************************
C
      ELSE IF(IREPLY.EQ.34) THEN
C
C   CALCULATE THE CHANGE IN STREAM SURFACE THICKNESS
C
C     FIRST CALCULATE THE VALUE OF RHO*R*VM
C
      DO 566 J=1,JM
      DO 566 K=1,KM
      DO 566 I=1,IM
          PROP2(I,J,K) = SQRT(VX(I,J,K)*VX(I,J,K)
     &                 + VR(I,J,K)*VR(I,J,K))*RO(I,J,K)*R(J,K)
  566 CONTINUE
C
C   PITCHWISE AREA AVERAGE THE SS THICKNESS
C
      DO 567 J=1,JM
      DO 567 K=1,KM
      SUMPIT = 0.0
      PAVG   = 0.0
      DO 568 I=1,IMM1
      FPITCH  =  T(I+1,J,K) - T(I,J,K)
      SUMPIT  = SUMPIT + FPITCH
      PAVG    = PAVG + (PROP2(I,J,K) + PROP2(I+1,J,K))*FPITCH
  568 CONTINUE
      PAVG =  0.5*PAVG/SUMPIT
      DO 569 I=1,IM
      PROP2(I,J,K) = PAVG
  569 CONTINUE
  567 CONTINUE
C
C     PROP2 IS NOW THE PITCHWISE AVERAGE OF RHO*R*VM
C****************************************
C   NOW CALCULATE THE PITCHWISE AVERAGED MASS FLOW RATE, SAME AS IREPLY = 29 ABOVE.
C
        IMM1 = IM-1
        KMM1 = KM-1
        PI   = ACOS(-1.)
C
        DO  2090  J=1,JM
C
          DO 2091 I=1,IM
            PROP(I,J,1) = 0.0
 2091     CONTINUE
C
          DO 2092 K=1,KMM1
            SUMFLOW = 0.0
            DO 2093 I=1,IMM1
              X1 = X(J,K+1)-X(J,K)
              X2 = X1
              R1 = R(J,K+1)-R(J,K)
              R2 = R1
              T1 = R(J,K+1)*(T(I+1,J,K+1)-T(I,J,K))
              T2 = R(J,K+1)*(T(I,J,K+1)-T(I,J,K))
     &            -R(J,K)*(T(I+1,J,K)-T(I,J,K))
              AQX = 0.5*(R2*T1 - R1*T2)
              AQR = 0.5*(X1*T2 - X2*T1)
              ROVXAV = RO(I,J,K)*VX(I,J,K)
     &                +RO(I,J,K+1)*VX(I,J,K+1)
     &                +RO(I+1,J,K)*VX(I+1,J,K)
     &                +RO(I+1,J,K+1)*VX(I+1,J,K+1)
              ROVRAV = RO(I,J,K)*VR(I,J,K)
     &                +RO(I,J,K+1)*VR(I,J,K+1)
     &                +RO(I+1,J,K)*VR(I+1,J,K)
     &                +RO(I+1,J,K+1)*VR(I+1,J,K+1)
              DFLOW = AQX*ROVXAV + AQR*ROVRAV
              SUMFLOW = SUMFLOW + DFLOW
 2093       CONTINUE
C
            PROP(1,J,K+1) = PROP(1,J,K) + 0.25*SUMFLOW*2.*PI/PITCH(J,K)
C
            DO 2094 I=2,IM
              PROP(I,J,K+1) = PROP(1,J,K+1)
 2094       CONTINUE
C
 2092     CONTINUE
 2090   CONTINUE
C
C     PROP = 2D MASS FLOW RATE , IT DOES NOT VARY WITH I VALUE .
C
      WRITE(6,*)  ' INPUT THE J VALUE TO WHICH THE STREAM TUBE THICKNESS
     & WILL BE REFERENCED.'
      WRITE(6,*) ' THE THICKNESS = 1.0 AT THIS J VALUE AT ALL SPANWISE 
     &POSITIONS.'
      READ(5,*) JREF
C
C   MAKE THE MASS FLOW VARY FROM 0  to 1.
       DO 2095 J=1,JM
       DO 2095 K=1,KM
       FRACFLO(J,K) = PROP(1,J,K)/PROP(1,J,KM)
 2095  CONTINUE
C
C  SET 1D MASS FLOW AND SS THICKNESS AT JREF
C
       DO K=1,KM
       FFLO_REF(K)   = FRACFLO(JREF,K)
       TKSS_REF(K)   = PROP2(1,JREF,K)
       END DO
C
      DO 2097  J= 1,JM
C
C   INTERPOLATE TO FIND THR LOCAL SS THICKNESS AT THE SAME MASS FRACTION
C
      DO 2096  K=1,KM
      FFLO(K) = FRACFLO(J,K)
      TKSS(K) = PROP2(1,J,K)
 2096 CONTINUE
      DO 2098 K=1,KM
      ARGG = FFLO(K)
      CALL INTP(KM,FFLO_REF,TKSS_REF,ARGG,ANSW)
      TKRAT = ANSW/TKSS(K)
      DO 2099 I=1,IM
      PROP(I,J,K) = TKRAT
 2099 CONTINUE
 2098 CONTINUE
C
 2097 CONTINUE
C
             NPROP = ' Stream Surface Thickness ratio'
             FMT         = '(F10.5)  '
C
C*******************************************************************
C
      ELSE IF(IREPLY.EQ.31) THEN 
C
C     CALCULATE THE STAGNATION PRESSURES AND TEMPERATURES
C
      DO 220 J=1,JM
      DO 220 K=1,KM
      DO 220 I=1,IM
          PROP(I,J,K)  = P_STAGN(I,J,K)
          PROP2(I,J,K) = T_STAGN(I,J,K)
  220 CONTINUE
C
C      PROP IS NOW THE STAGNATION PRESSURE
C      PROP2  IS THE STAGNATION TEMPERATURE
C
C     FIND THE INLET MASS AVERAGE  PO AND TO AT INLET
C
      J=1
C
      DO 225 K=1,KM
      DO 221 I=1,IM
  221 ROVM(I) = SQRT(ROVX(I,J,K)*ROVX(I,J,K)+ROVR(I,J,K)*ROVR(I,J,K))
      FLOW  = 0.0
      SUMPO = 0.0
      SUMTO = 0.0
      DO 222 I=1,IMM1
      DTHETA = T(I+1,J,K)-T(I,J,K)
      DFLOW = 0.5*(ROVM(I)+ROVM(I+1))*DTHETA
      FLOW = FLOW + DFLOW
      SUMPO = SUMPO + DFLOW*0.5*(PROP(I,J,K)+PROP(I+1,J,K))
  222 SUMTO = SUMTO + DFLOW*0.5*(PROP2(I,J,K)+PROP2(I+1,J,K))
      POINPUT(K) = SUMPO/FLOW
      TOINPUT(K) = SUMTO/FLOW
  225 CONTINUE
C
C      PITCHWISE MASS AVERAGE THE STAGNATION PRESSURE AND TEMPERATURE
C
      CALL SUB23(PROP,0)
      CALL SUB23(PROP2,0)
C
C      CALCULATE THE POLYTROPIC EFFICIENCY
C
      DO 226 J=1,JM
      DO 226 K=1,KM
      DO 226 I=1,IM
      RAT_PO = PROP(I,J,K)/POINPUT(K)
      IF(RAT_PO.LT.0.0) RAT_PO = 0.005
      IF(ABS(RAT_PO-1.0).LT.0.005) GO TO 227
      RAT_TO = PROP2(I,J,K)/TOINPUT(K)
      IF(RAT_TO.LT.0.0) RAT_TO = 0.005
      IF(ABS(RAT_TO-1.0).LT.0.005) GO TO 227
      POWER = ALOG(RAT_TO)/ALOG(RAT_PO)
      POLY  = POWER*GA/(GA-1.)
      IF(POLY.GT.1.0) POLY = 1./POLY
      IF(POLY.LT.0.5) POLY = 0.5
      GO TO 226
  227 POLY = 1.0
  226 PROP(I,J,K) = POLY
C
        FMT         = '(F8.4)  '
	NPROP       = ' AVERAGE  POLYTROPIC EFFICIENCY '
C
      ENDIF
C
2083  FORMAT(//'  WARNING - this variable is calculated using a'/
     &         '            reference value determined using'/
     &         '            conditions at:  I =',I5/
     &         '                            J =    1'/ 
     &         '                            K =',I5/)
      RETURN
C
      END
C
C*******************************************************************C
C
C            SUBROUTINE TO MASS AVERAGE THE VARIABLE PROP
C
C******************************************************************C
C
      SUBROUTINE SUB23 ( PROP, IFTOT )
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK10/ RO(95,2500,95)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION PROP(95,2500,95),AVG(95),FAVG(95)
C
C
      IMM1= IM-1
      KMM1 =KM-1
C
      DO  100  J=1,JM
        SUMFTOT=0.0
        SUMPTOT=0.0
C
        DO 35 K=1,KMM1
          SUMFLOW = 0.0
          SUMPROP = 0.0
          DO 31 I=1,IMM1
              X1 = X(J,K+1)-X(J,K)
              X2 = X1
              R1 = R(J,K+1)-R(J,K)
              R2 = R1
              T1 = R(J,K+1)*(T(I+1,J,K+1)-T(I,J,K))
              T2 = R(J,K+1)*(T(I,J,K+1)-T(I,J,K))
     &            -R(J,K)*(T(I+1,J,K)-T(I,J,K))
              AQX = 0.5*(R2*T1 - R1*T2)
              AQR = 0.5*(X1*T2 - X2*T1)
              ROVXAV = RO(I,J,K)*VX(I,J,K)
     &                +RO(I,J,K+1)*VX(I,J,K+1)
     &                +RO(I+1,J,K)*VX(I+1,J,K)
     &                +RO(I+1,J,K+1)*VX(I+1,J,K+1)
              ROVRAV = RO(I,J,K)*VR(I,J,K)
     &                +RO(I,J,K+1)*VR(I,J,K+1)
     &                +RO(I+1,J,K)*VR(I+1,J,K)
     &                +RO(I+1,J,K+1)*VR(I+1,J,K+1)
              PROPAV = PROP(I,J,K)+PROP(I,J,K+1)
     &                +PROP(I+1,J,K)+PROP(I+1,J,K+1)
              DFLOW = AQX*ROVXAV + AQR*ROVRAV
              SUMFLOW = SUMFLOW + DFLOW
              SUMPROP = SUMPROP + DFLOW*PROPAV*0.25
   31     CONTINUE
C
          SUMFTOT = SUMFTOT+ SUMFLOW
          SUMPTOT = SUMPTOT+ SUMPROP
          AVG(K)  = SUMPROP/SUMFLOW 
   35   CONTINUE
C
        DO 45 K=2,KMM1
          FAVG(K)= 0.5*(AVG(K)+AVG(K-1))
   45   CONTINUE
C
        FAVG(1) = 1.5*AVG(2)   - 0.5*AVG(3)
        FAVG(KM)= 1.5*AVG(KMM1)- 0.5*AVG(KM-2)
C
        DO 50 K=1,KM
        DO 50 I=1,IM
          PROP(I,J,K)= FAVG(K)
   50   CONTINUE
C
        IF ( IFTOT . EQ . 1 ) THEN
C
C         Passage Average required 
C  
          AVTOT = SUMPTOT/SUMFTOT
C
          DO 55 I=1,IM
          DO 55 K=1,KM
            PROP(I,J,K) = AVTOT
   55     CONTINUE
        ENDIF
  100 CONTINUE
C
      RETURN
      END
C
C*********************************************************************
C
C      SUBROUTINE TO READ IN DATA FOR CELL CENTRED VARIABLES
C
C********************************************************************
C
C
C
C
      SUBROUTINE SUB25 ( ICENT )
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK9 / P(95,2500,95)
      COMMON / BLK10/ RO(95,2500,95)
      COMMON / BLK11/ M_ABS(95,2500,95),M_REL(95,2500,95)
      COMMON / BLK14/ Q(95,2500,95),QQ(95,2500,95),NQ,NQQ
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK19/ XX(95,2500),YY(95,2500),ZZ(95,2500),
     &                WORK1(95,2500),WORK2(95,2500)
C
      REAL         M_ABS, M_REL
C
      CHARACTER*32 NAME,NQ,NQQ
      CHARACTER*72 JBNAME,TSNUM
C
C-SPLIT->C     -------
C-SPLIT->      IM=IM+3
C-SPLIT->C     -------
C 
      IMM1   = IM-1
      JMM1   = JM-1
      KMM1   = KM-1
      IMM2   = IM-2
      JMM2   = JM-2
      KMM2   = KM-2
C
C      INPUT flow variables
C      --------------------
C
C
          READ  (4,4001,ERR=999,END=999) TSNUM
C
C
C          Axial velocity
C          --------------
C
          READ  (4,4016,ERR=999,END=999) NAME
C
          DO 1011 K = 1,KMM1
             DO 1012 J = 1,JMM1
                 READ  (4,4002,ERR=999,END=999) (VX(I,J,K),I = 1,IMM1)
 1012        CONTINUE
 1011     CONTINUE
C
C          Tangential velocity
C          -------------------
C
          READ  (4,4016,ERR=999,END=999) NAME
C
          DO 1111 K = 1,KMM1
             DO 1112 J = 1,JMM1
                 READ  (4,4002,ERR=999,END=999) (VT(I,J,K),I = 1,IMM1)
 1112        CONTINUE
 1111     CONTINUE
C
C          Radial velocity
C          ---------------
C
          READ  (4,4016,ERR=999,END=999) NAME
C
          DO 1211 K = 1,KMM1
             DO 1212 J = 1,JMM1
                 READ  (4,4002,ERR=999,END=999) (VR(I,J,K),I = 1,IMM1)
 1212        CONTINUE
 1211     CONTINUE
C
C          Relative Mach number
C          --------------------
C
          READ  (4,4016,ERR=999,END=999) NAME
C
          DO 1311 K = 1,KMM1
             DO 1312 J = 1,JMM1
                 READ  (4,4003,ERR=999,END=999) (M_ABS(I,J,K),I= 1,IMM1)
 1312        CONTINUE
 1311     CONTINUE
C
C          Static pressure
C          ---------------
C
          READ  (4,4016,ERR=999,END=999) NAME
C
          DO 1411 K = 1,KMM1
             DO 1412 J = 1,JMM1
                 READ  (4,4004,ERR=999,END=999) (P(I,J,K),I = 1,IMM1)
 1412        CONTINUE
 1411     CONTINUE
C
C          Density
C          -------
C
          READ  (4,4016,ERR=999,END=999) NAME
C
          DO 1511 K = 1,KMM1
             DO 1512 J = 1,JMM1
                 READ  (4,4003,ERR=999,END=999) (RO(I,J,K),I = 1,IMM1)
 1512        CONTINUE
 1511     CONTINUE
C
C          Viscosity
C          ---------
C
          READ  (4,4016,ERR=999,END=999) NQ
C
          DO 1611 K = 1,KMM1
             DO 1612 J = 1,JMM1
                 READ  (4,4005,ERR=999,END=999) (Q(I,J,K),I = 1,IMM1)
 1612        CONTINUE
 1611     CONTINUE
C
C          length scales (DUMMY READ ONLY)
C          -------------
C
          DO 2380 K = 1,KMM1
              READ  (4,4005,ERR=999,END=999) (ZZ(K,J), J=1,JMM1)
              READ  (4,4005,ERR=999,END=999) (ZZ(K,J), J=1,JMM1)
              READ  (4,4005,ERR=999,END=999) (ZZ(K,J), J=1,JMM1)
              READ  (4,4005,ERR=999,END=999) (ZZ(K,J), J=1,JMM1)
 2380     CONTINUE
C
          DO 2400 J = 1,JMM1
              READ  (4,4005,ERR=999,END=999) (ZZ(I,J), I=1,IMM1)
              READ  (4,4005,ERR=999,END=999) (ZZ(I,J), I=1,IMM1)
              READ  (4,4005,ERR=999,END=999) (ZZ(I,J), I=1,IMM1)
              READ  (4,4005,ERR=999,END=999) (ZZ(I,J), I=1,IMM1)
 2400     CONTINUE
C
C      Put onto cell corners by linear extrapolation and interpolation
C
      CALL SUB26 ( VX  , ICENT )
      CALL SUB26 ( VT  , ICENT )
      CALL SUB26 ( VR  , ICENT )
      CALL SUB26 ( M_ABS, ICENT )
      CALL SUB26 ( P   , ICENT )
      CALL SUB26 ( RO  , ICENT )
      CALL SUB26 ( Q   , ICENT )
C
C-SPLIT->C     -------
C-SPLIT->      IM=IM-3
C-SPLIT->C     -------
C 
      RETURN
C
C
  999 CONTINUE
      STOP
C
C
 4001 FORMAT(A80)
 4002 FORMAT(10E12.5)
 4003 FORMAT(10E12.5)
 4004 FORMAT(10E12.5)
 4005 FORMAT(10E12.5)
 4016 FORMAT(A32)
C
      END
C********************************************************************
C
C      SUBROUTINE TO PUT CENTRED VARIABLES ONTO CORNERS
C
C********************************************************************
C
      SUBROUTINE SUB26 ( VAR, ICENT )
C
C
      DIMENSION VAR(95,2500,95), TEMP(95,2500,95)
C
C
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
C
      IMM1   = IM-1
      JMM1   = JM-1
      KMM1   = KM-1
      IMM2   = IM-2
      JMM2   = JM-2
      KMM2   = KM-2
C
      KTIP   = KM
      KHUB   =  1
C
C
      DO 1002 I = 1,IM
          II  = I
          IM1 = I-1
          IF (I . EQ . 1) IM1 = IMM1
          IF (I . EQ . IM) II = 1
C
          DO 1003 J = 2,JMM1
              DO 1004 K = 2,KMM1
                  TEMP(I,J,K) = 0.125*
     &                      (VAR(II,  J,  K)+VAR(IM1,  J,  K)
     &                      +VAR(II,J-1,  K)+VAR(IM1,J-1,  K)
     &                      +VAR(II,  J,K-1)+VAR(IM1,  J,K-1)
     &                      +VAR(II,J-1,K-1)+VAR(IM1,J-1,K-1))
 1004         CONTINUE
C
              TEMP(I,J, 1) = 0.375*(VAR(II,  J,   1)+VAR(IM1,  J,   1)
     &                           +VAR(II,J-1,   1)+VAR(IM1,J-1,   1))
     &                    -0.125*(VAR(II,  J,   2)+VAR(IM1,  J,   2)
     &                           +VAR(II,J-1,   2)+VAR(IM1,J-1,   2))
              TEMP(I,J,KM) = 0.375*(VAR(II,  J,KMM1)+VAR(IM1,  J,KMM1)
     &                           +VAR(II,J-1,KMM1)+VAR(IM1,J-1,KMM1))
     &                    -0.125*(VAR(II,  J,KMM2)+VAR(IM1,  J,KMM2)
     &                           +VAR(II,J-1,KMM2)+VAR(IM1,J-1,KMM2))
C
 1003     CONTINUE
 1002 CONTINUE
C
C      Check if multiple blade passages have been used
C
      IF ( ICENT . EQ . 1 ) THEN
          J1 = JLE
          J2 = JTE
      ELSE
          J1 =    2
          J2 = JMM1
      ENDIF
C
C      Extrapolate to I=1 and I=IMM1
C
      DO 1007 J = J1,J2
          DO 1008 K = KHUB,KTIP
              KK  = K
              KM1 = K-1
              IF (K . EQ . 1) KM1 = 1
              IF (K . EQ . KM) KK = KMM1
C
              TEMP( 1,J,K) = 0.375*(VAR(   1,  J,KK)+VAR(   1,  J,KM1)
     &                           +VAR(   1,J-1,KK)+VAR(   1,J-1,KM1))
     &                    -0.125*(VAR(   2,  J,KK)+VAR(   2,  J,KM1)
     &                           +VAR(   2,J-1,KK)+VAR(   2,J-1,KM1))
              TEMP(IM,J,K) = 0.375*(VAR(IMM1,  J,KK)+VAR(IMM1,  J,KM1)
     &                           +VAR(IMM1,J-1,KK)+VAR(IMM1,J-1,KM1))
     &                    -0.125*(VAR(IMM2,  J,KK)+VAR(IMM2,  J,KM1)
     &                           +VAR(IMM2,J-1,KK)+VAR(IMM2,J-1,KM1))
 1008     CONTINUE
 1007 CONTINUE
C
C      Extrapolate for inlet and exit boundaries
C
      DO 1009 I = 1,IM
          DO 1010 K = 1,KM
              TEMP(I, 1,K) = 2.*TEMP(I,   2,K)-TEMP(I,   3,K)
              TEMP(I,JM,K) = 2.*TEMP(I,JMM1,K)-TEMP(I,JMM2,K)
 1010     CONTINUE
 1009 CONTINUE
C
C      Re-write the array
C
      DO 1011 K = 1,KM
         DO 1012 J = 1,JM
              DO 1013 I = 1, IM
                  VAR(I,J,K) = TEMP(I,J,K)
 1013         CONTINUE
 1012    CONTINUE
 1011 CONTINUE
C-SPLIT->C
C-SPLIT->C   write "btob3dSP" data into plottable form by removing dummy cells
C-SPLIT->C   -----------------------------------------------------------------
C-SPLIT->C
C-SPLIT->      do K=1,KM
C-SPLIT->       do J=1,JM
C-SPLIT->        do I=ISPLIT+1,IM-3
C-SPLIT->C
C-SPLIT->         VAR(I,J,K)=VAR(I+3,J,K)
C-SPLIT->C
C-SPLIT->        enddo
C-SPLIT->       enddo
C-SPLIT->      enddo     
C-SPLIT->C
C-SPLIT->C   -----------------------------------------------------------------
C 
C
      RETURN
      END
C******************************************************************C
C
C          SUBROUTINE TO CALCULATE SM/SMMAX FOR BLADE SURFACE PLOTS
C
C******************************************************************C
C
      SUBROUTINE SUB27(SOSM1,SOSM2,NSOSM,K)
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
      DIMENSION SOSM1(2500),SOSM2(2500)
C
      CHARACTER*32 NSOSM
C
      NSOSM = 'FRACTION OF MERIDIONAL DISTANCE '
C
      JTEM1 = JTE - 1
      JLEP1 = JLE + 1
C
      NPTS  = JTE - JLE + 1
C
C         STORE THE X-COORDS
C
      DO 110 N=1,NPTS
      J = N + JLE-1
      SOSM1(N) = SM(J,K)
      SOSM2(N) = SM(J,K)
  110 CONTINUE
C
C       SET SCALES
C
      XMAX = SOSM1(1)
      XMIN = SOSM1(1)
C
      DO 120 N=1,NPTS
      IF (SOSM1(N).LT.XMIN) XMIN = SOSM1(N)
      IF (SOSM2(N).LT.XMIN) XMIN = SOSM2(N)
      IF (SOSM1(N).GT.XMAX) XMAX = SOSM1(N)
      IF (SOSM2(N).GT.XMAX) XMAX = SOSM2(N)
  120 CONTINUE
C
      DO 130 N=1,NPTS
      SOSM1(N) = (SOSM1(N)-XMIN)/(XMAX-XMIN)
      SOSM2(N) = (SOSM2(N)-XMIN)/(XMAX-XMIN)
  130 CONTINUE
C
      RETURN
C
      END
C*****************************************************************C
C                                                                 C
C           SUBROUTINE TO DRAW SPLITTER BLADE                      C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB28
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
C
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT1 /  DELR, ZSCALE, VMOD,
     &                 NDIFF, I, J, K, IZ, JZ, KZ, IPIT, CTYPE, ANS
C
C
C     Loop over pitches
C
      DO 300 NPIT = -IPIT,IPIT,1
C
C
C        RETURN IF NO SPLITER
C
      IF ( ISPLIT . EQ . IM ) RETURN
C
C        DRAW BLADE
C
      IF ( ANS . EQ . 'R' . OR . ANS . EQ . 'r' ) THEN
C
C         Draw lower surface
C
          CALL MOVETO(X(JSLE,K),
     &        R(JSLE,K)*(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)))
          DO 160 J=JSLE,JSTE
              CALL DRAWTO(X(J,K),
     &            R(J,K)*(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)))
  160     CONTINUE
          CALL DRAWTO(X(JSTE,K),
     &        R(JSTE,K)*(T(ISPLIT+1,JSTE,K)-TMID+NPIT*PITCH(JSTE,K)))
C
C         Draw upper surface
C
          CALL MOVETO(X(JSLE,K),
     &        R(JSLE,K)*(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)))
          DO 161 J=JSLE,JSTE
              CALL DRAWTO(X(J,K),
     &            R(J,K)*(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)))
  161     CONTINUE
C
C
C        DRAW BLADE
C
      ELSE IF ( ANS . EQ . 'T' . OR . ANS . EQ . 't' ) THEN
C
C         Draw lower surface
C
          CALL MOVETO(
     &     R(JSLE,K)*COS(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)),
     &     R(JSLE,K)*SIN(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)))
          DO 170 J=JSLE,JSTE
              CALL DRAWTO(
     &            R(J,K)*COS(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)))
  170     CONTINUE
          CALL DRAWTO(
     &     R(JSTE,K)*COS(T(ISPLIT+1,JSTE,K)-TMID+NPIT*PITCH(JSTE,K)),
     &     R(JSTE,K)*SIN(T(ISPLIT+1,JSTE,K)-TMID+NPIT*PITCH(JSTE,K)))
C
C         Draw upper surface
C
          CALL MOVETO(
     &     R(JSLE,K)*COS(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)),
     &     R(JSLE,K)*SIN(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)))
          DO 171 J=JSLE,JSTE
              CALL DRAWTO(
     &            R(J,K)*COS(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)))
  171     CONTINUE
C
C
C        DRAW BLADE
C
      ELSE IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
C
C         Draw lower surface
C
          CALL MOVETO(SM(JSLE,K),
     &        R(JSLE,K)*(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)))
          DO 180 J=JSLE,JSTE
              CALL DRAWTO(SM(J,K),
     &                  R(J,K)*(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)))
  180     CONTINUE
          CALL DRAWTO(SM(JSTE,K),
     &        R(JSTE,K)*(T(ISPLIT+1,JSTE,K)-TMID+NPIT*PITCH(JSTE,K)))
C
C         Draw upper surface
C
          CALL MOVETO(SM(JSLE,K),
     &        R(JSLE,K)*(T(ISPLIT+1,JSLE,K)-TMID+NPIT*PITCH(JSLE,K)))
          DO 181 J=JSLE,JSTE
              CALL DRAWTO(SM(J,K),
     &            R(J,K)*(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)))
  181     CONTINUE
C
      ELSE IF ( ( ANS . EQ . 'X' . OR . ANS . EQ . 'x' )
     &         . AND . ( J . GE . JSLE . AND . J . LE . JSTE ) ) THEN    
C
C
C         Draw left surface
C
          CALL MOVETO(
     &        R(J,1)*COS(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1)),
     &        R(J,1)*SIN(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1)))
          DO 190 K=1,KM
              CALL DRAWTO(
     &            R(J,K)*COS(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)),
     &            R(J,K)*SIN(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)))
  190     CONTINUE
          CALL DRAWTO(
     &        R(J,KM)*COS(T(ISPLIT+1,J,KM)-TMID+NPIT*PITCH(J,KM)),
     &        R(J,KM)*SIN(T(ISPLIT+1,J,KM)-TMID+NPIT*PITCH(J,KM)))
C
C         Draw right surface
C
          CALL MOVETO(
     &        R(J,1)*COS(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1)),
     &        R(J,1)*SIN(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1)))
          DO 191 K=1,KM
             CALL DRAWTO(
     &           R(J,K)*COS(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)),
     &           R(J,K)*SIN(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)))
  191     CONTINUE
C
      ELSE IF ( ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' )
     &         . AND . ( J . GE . JSLE . AND . J . LE . JSTE ) ) THEN    
C
C
C         Draw left surface
C
          CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &        +(R(J,1)*COS(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1))
     &         -R(J,1))*SINA(J),
     &        R(J,1)*SIN(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1)))
          DO 200 K=1,KM
              CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &            +(R(J,K)*COS(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K))
     &             -R(J,1))*SINA(J),
     &            R(J,K)*SIN(T(ISPLIT,J,K)-TMID+NPIT*PITCH(J,K)))
  200     CONTINUE
          CALL DRAWTO((X(J,KM)-X(J,1))*COSA(J)
     &        +(R(J,KM)*COS(T(ISPLIT+1,J,KM)-TMID+NPIT*PITCH(J,KM))
     &         -R(J,1))*SINA(J),
     &        R(J,KM)*SIN(T(ISPLIT+1,J,KM)-TMID+NPIT*PITCH(J,KM)))
C
C         Draw left surface
C
          CALL MOVETO((X(J,1)-X(J,1))*COSA(J)
     &        +(R(J,1)*COS(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1))
     &         -R(J,1))*SINA(J),
     &        R(J,1)*SIN(T(ISPLIT+1,J,1)-TMID+NPIT*PITCH(J,1)))
          DO 201 K=1,KM
             CALL DRAWTO((X(J,K)-X(J,1))*COSA(J)
     &           +(R(J,K)*COS(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K))
     &            -R(J,1))*SINA(J),
     &           R(J,K)*SIN(T(ISPLIT+1,J,K)-TMID+NPIT*PITCH(J,K)))
  201     CONTINUE
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
C         Draw l.e. & t.e. lines
C
          CALL LINTYP ( 6 )
C
          CALL MOVETO(X(JSLE,1),R(JSLE,1))
C
          DO 212 K=2,KM
              CALL DRAWTO(X(JSLE,K),R(JSLE,K))
  212     CONTINUE
C
          CALL MOVETO(X(JSTE,1),R(JSTE,1))
C
          DO 213 K=2,KM
              CALL DRAWTO(X(JSTE,K),R(JSTE,K))
  213     CONTINUE
C
          CALL LINTYP ( 0 )
C
      ENDIF
C
  300 CONTINUE
C
      RETURN
C
      END
C******************************************************************C
C                                                                  C
C         SUBROUTINE TO PLOT 3D CONTOURS
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB35(PROP,NAME,X0,Y0,X1,Y1,IFP,ID)
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK19/ XX(95,2500),YY(95,2500),ZZ(95,2500),
     &                WORK1(95,2500),WORK2(95,2500)
      COMMON / BLK20/ XS1(2500),YS1(2500),XS2(2500),YS2(2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      CHARACTER*72 JBNAME,TSNUM
      CHARACTER*32 NAME
      CHARACTER*8  IFP
      CHARACTER*1  CANS
C
      DIMENSION PROP(95,2500,95),HITES(125),STOREX(95,2500),
     &          STORER(95,2500)
C
      REAL EMACH,IMACH
C
      LOGICAL      FAIL
      REAL         ABXLIM ( 4 ), ABYLIM ( 4 )
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT6 / I6, J6, K6, DELR, ZSCALE, I6Z, J6Z, K6Z, 
     &                BETAXX, BETAYY, BETAZZ, ANS, CTYPE
C
C     DATA STATEMENT
C
      NROW = (95)
      RAD  = 3.14159/180.
C
C
C         TELL USER OF STATUS
C
      WRITE (6,2000) JBNAME,NAME
C
C         DETERMINE TYPE OF PLOT REQUIRED
C
   55 WRITE (6,2020) ANS
      READ  (1,1000) CANS
      IF ( CANS . NE . ' ' ) ANS = CANS
C
C         SET UP NEW ARRAY FOR CONTOURING
C
C         TYPE OF PLOT     1ST ELEMENT     2ND ELEMENT OF ARRAY
C         ------------     -----------     --------------------
C             'S'              I6                    J6
C             'M'              K6                    J6
C             'Q'              I6                    K6
C
C
      IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
C
   13     WRITE (6,2021) KM, K6
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) K6 = IANS
          IF (K6.GT.KM . OR . K6.LT.1 ) GO TO 13
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
   23     WRITE (6,2022) JM,  J6
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) J6 = IANS
          IF (J6.GT.JM . OR . J6.LT.1 ) GO TO 23
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
   33     WRITE (6,2023) IM,  I6
          IANS = IREAD(1)
          IF ( IANS . NE . 0 ) I6 = IANS
          IF (I6.GT.IM . OR . I6.LT.1 ) GO TO 33
C
      ELSE
C
          GO TO 55
      ENDIF
C
C          ESTABLISH CONTOUR HITES
C
      NHTS   = 15
C
      CALL SUB6 (PROP,GMAX,GMIN)
      CALL SUB6A(PROP,PMAX,PMIN,I6,J6,K6,ANS)
C
      RANGE  = PMAX-PMIN
      DELR   = RANGE/FLOAT(NHTS)
C
C
   34 WRITE (6,2001) PMIN,GMIN,PMAX,GMAX,NHTS,DELR
      RANS = RREAD(1)
      IF ( RANS.GT.0.0 ) DELR = RANS
C
      MIN    = IFIX(PMIN/DELR+0.5)-1
      MAX    = IFIX(PMAX/DELR+0.5)+1
C
      NHTS = MAX - MIN + 1
      IF ( NHTS.GT.125 ) GO TO 34
C
      DO 40 M=MIN,MAX
      HITES(M+1-MIN) = FLOAT(M)*DELR
   40 CONTINUE
C
C      Zoom ?
C
C
      WRITE (6,2024) ZLAST
      ZIN = RREAD(1)
      IF (ZIN.GT.0.01) THEN
          ZSCALE = ZIN
          ZLAST  = ZSCALE
      ELSE 
          ZSCALE = ZLAST
      ENDIF
C
      IF(ZIN.GT.0.01) THEN
C
   35     WRITE (6,2025) IM, I6Z
          IANS = IREAD(1)
          IF (IANS.NE.0) I6Z = IANS
          IF (I6Z.LT.1.OR.I6Z.GT.IM) GO TO 35
C
   36     WRITE (6,2026) JM, J6Z
          IANS = IREAD(1)
          IF (IANS.NE.0) J6Z = IANS
          IF (J6Z.LT.1.OR.J6Z.GT.JM) GO TO 36
C
   37     WRITE (6,2027) KM, K6Z
          IANS = IREAD(1)
          IF (IANS.NE.0) K6Z = IANS
          IF (K6Z.LT.1.OR.K6Z.GT.KM) GO TO 37
C
      ENDIF
C
C         ESTABLISH LIMITS
C   JDD MODS FOR 3D VIEW OF BLADE
C       XXMID = 0.5*(XMAX+XMIN)
C       YYMID = 0.5*(YMAX+YMIN)
C       ZZMID = 0.5*(ZMAX+ZMIN)
C   JDD
       XXMID = 0.5*(X(JLE,KM) + X(JTE,KM))
       YYMID = 0.5*(R(JLE,KM)*T(1,JLE,KM) +
     &              R(JTE,KM)*T(1,JTE,KM))
       ZZMID = 0.5*(R(JLE,KM) + R(JTE,KM))
C      END OF JDD MODS
C
      RANGE = 2.0* AMAX1( (XMAX-XMIN), (YMAX-YMIN), (ZMAX-ZMIN) )
C
      XXMIN = XXMID - 0.5*RANGE
      XXMAX = XXMID + 0.5*RANGE
      YYMIN = YYMID - 0.5*RANGE
      YYMAX = YYMID + 0.5*RANGE
      ZZMIN = ZZMID - 0.5*RANGE
      ZZMAX = ZZMID + 0.5*RANGE
C
      XC = 0.5*(X0+X1)
      YC = 0.5*(Y0+Y1)
C
C         Set axes rotations for correct view along -ve z-axis
C
      betazz = 0.0
      WRITE (6,2032) betazz
      RANS = RREAD(1)
      IF (rans.NE.0.0) betazz=rans
C
      betayy = 0.0
      WRITE (6,2031) betayy
      RANS = RREAD(1)
      IF (rans.NE.0.0) betayy=rans
C
      betaxx = 0.0
      WRITE (6,2030) betaxx
      RANS = RREAD(1)
      IF (rans.NE.0.0) betaxx=rans
C
C      adjust angles as required
C
      betaz = betazz * rad
      betay = betayy * rad
      betax = betaxx * rad
C
C
C       Determine scaling parameters
C
      RADIUS  = AMIN1 ( (X1-X0), (Y1-Y0) ) * 0.5
C
      scalep  = 2.*radius/range
C
C         DETERMINE TYPE OF CONTOURS REQUIRED
C
      WRITE (6,2060) CTYPE
      READ  (1,1000) CANS
      IF ( CANS . NE . ' ' ) CTYPE = CANS
C
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C        TEST TO SEE IF SHADING IS AVALIABLE
C
      CALL BOXFIL ( 0., 0., 0., 0., 0, FAIL )
C
      IF ( FAIL ) CTYPE = 'N'
C
C         FIND THE ABSOLUTE SIZE OF THE PLOT BEFORE ANYTHING ELSE
C
      CALL FIND ( ABXLIM, ABYLIM, 7 )
C
C         SET WINDOW & ZOOM
C
      CALL AWINDW ( X0, Y0, X1, Y1, -1 )
      CALL SUB40 ( x(J6Z,K6Z),r(J6Z,K6Z),t(I6Z,J6Z,K6Z),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XXZOOM,YYZOOM,XC,YC)
C
      CALL zoom  (XXZOOM,YYZOOM,ZSCALE,ZSCALE)
      CALL SCLCHR (1./zscale,1./zscale)
C
      CALL SCLCHR ( 0.8, 0.8 )
      IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
C
          DO 10 I=1,ISPLIT
            DO 11 J=1,JM
              CALL SUB40 ( x(j,K6),r(j,K6),t(i,j,K6),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(i,j),YY(i,j),XC,YC)
              ZZ(I,J) = PROP(I,J,K6)
   11       CONTINUE
   10     CONTINUE
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE 
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,JM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C
          IF ( ISPLIT . NE . IM ) THEN
C
            DO 110 I=ISPLIT+1,IM
              DO 111 J=1,JM
                CALL SUB40 ( x(j,K6),r(j,K6),t(i,j,K6),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(i-isplit,j),YY(i-isplit,j),XC,YC)
                ZZ(I-ISPLIT,J) = PROP(I,J,K6)
  111         CONTINUE
  110       CONTINUE
C
C      
            CALL pltgrd(XX,YY,NROW,(IM-ISPLIT),JM,3)
C
            IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
            ELSE 
                CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),JM,
     &                             HITES,NHTS,100,WORK1,WORK2)
            ENDIF
          ENDIF
C
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
C
          DO 20 K=1,KM
            DO 21 I=1,ISPLIT
              CALL SUB40 ( x(J6,k),r(J6,k),t(i,J6,k),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(i,k),YY(i,k),XC,YC)
              ZZ(I,K) = PROP(I,J6,K)
   21       CONTINUE
   20     CONTINUE
C
          IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,KM,
     &                         HITES,NHTS,-8,WORK1,WORK2)
              CALL sclchr(.8,.8)
              CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
              CALL sclchr(1.25,1.25)
          ELSE
              CALL contor(XX,YY,ZZ,NROW,ISPLIT,KM,
     &                         HITES,NHTS,100,WORK1,WORK2)
          ENDIF
C
          IF ( ISPLIT . NE . IM ) THEN
C
              DO 220 K=1,KM
                DO 221 I=ISPLIT+1,IM
                  CALL SUB40 ( x(J6,k),r(J6,k),t(i,J6,k),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(i-isplit,k),YY(i-isplit,k),XC,YC)
                  ZZ(I-ISPLIT,K) = PROP(I,J6,K)
  221           CONTINUE
  220         CONTINUE
C
              CALL pltgrd(XX,YY,NROW,(IM-ISPLIT),KM,3)
C
              IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
                CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),KM,
     &                             HITES,NHTS,-8,WORK1,WORK2)
              ELSE 
                CALL contor(XX,YY,ZZ,NROW,(IM-ISPLIT),KM,
     &                             HITES,NHTS,100,WORK1,WORK2)
              ENDIF
          ENDIF
C
C    MERIDIONAL SURFACE PLOTS
C
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
C
C      ROTATE THE BLADE
C
        DO 30 K=1,KM
          DO 31 J=1,JM
            CALL SUB40 ( x(j,k),r(j,k),t(I6,j,k),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(k,j),YY(k,j),XC,YC)
   31     CONTINUE
   30   CONTINUE

C    JDD
C
              DO 330 K=1,KM
                  DO 331 JJ=JLE,JTE
                  J = JJ+1-JLE
                      STOREX(K,J) = XX(K,JJ)
                      STORER(K,J) = YY(K,JJ)
                      ZZ(K,J) = PROP(I6,JJ,K)
  331             CONTINUE
  330         CONTINUE
C
C  JDD PUT JC in the call to CONTOR
               JC = JTE -JLE + 1
C
C
        IF ( CTYPE . EQ . 'Y' . OR . CTYPE . EQ . 'y' ) THEN
            CALL contor(STOREX,STORER,ZZ,NROW,KM,JC,HITES,NHTS,
     &                                 -8,WORK1,WORK2)
            CALL sclchr(.5,.5)
            CALL conlbl(X1-20.,Y1,1,IFP,HITES,NHTS)
            CALL sclchr(2.,2.)
        ELSE       
            CALL contor(STOREX,STORER,ZZ,NROW,KM,JC,HITES,NHTS,
     &                                 0,WORK1,WORK2)
        ENDIF
C
      ENDIF
      CALL SCLCHR ( 1./0.8, 1./0.8 )
C
C
C
C     Now, plot the pressure side of the blades
C
      DO 49 nb=1,2
      i=im
      DO 50 k=1,km
      DO 51 j=1,jm
       CALL SUB40 ( x(j,k),r(j,k),t(i,j,k)-pitch(j,k)*(nb-1),tmid,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(k,j),YY(k,j),XC,YC)
   51 CONTINUE
   50 CONTINUE
C
C   
      CALL sub39(XX,YY,NROW,0,1,KM,MAX0(1,KM/4),JLE,JTE,(JTE-JLE)/8)
C
C
C     Now, plot the suction side of the blades
C
      i=1
      DO 60 k=1,km
      DO 61 j=1,jm
       CALL SUB40 ( x(j,k),r(j,k),t(i,j,k)+pitch(j,k)*(nb-1),tmid,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(k,j),YY(k,j),XC,YC)
   61 CONTINUE
   60 CONTINUE
C
C   
      CALL sub39(XX,YY,NROW,0,1,KM,MAX0(1,KM/4),JLE,JTE,(JTE-JLE)/8)
C
   49 CONTINUE
C
C      TURN WINDOW OFF TO ADD TITLES
C
      CALL AWINDW ( X0, Y0, X1, Y1, 0 )
C
C      Blank the area for labelling in case something written here by shading
C
      CALL CTUSER ( ABXLIM ( 1 ), ABYLIM ( 1 ), X0F, Y0F )
      CALL CTUSER ( ABXLIM ( 4 ), Y0,           X1F, Y1F )
C
      CALL BOXFIL ( X0F, Y0F, X1F, Y1F, 0, FAIL )
C
C      ADD TITLES
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
      XX6 = 175.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 15.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
      CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
C
C
      S = (XX1-XX0)/80./3.0
      CALL SCLCHR( S, S )
      CALL BGNWRT(XX6,YY2,1)
      IF ( ANS . EQ . 'S' . OR . ANS . EQ . 's' ) THEN
          CALL WRITEA('*','(A30)','         STREAM SURFACE NUMBER')
          CALL WRITEI('*','(I4)',K6)
      ELSE IF ( ANS . EQ . 'Q' . OR . ANS . EQ . 'q' ) THEN
          CALL WRITEA('*','(A30)','       QUASI-ORTHOGONAL NUMBER')
          CALL WRITEI('*','(I4)',J6)
      ELSE IF ( ANS . EQ . 'M' . OR . ANS . EQ . 'm' ) THEN
          CALL WRITEA('*','(A30)','     MERIDIONAL SURFACE NUMBER')
          CALL WRITEI('*','(I4)',I6)
      ENDIF
C
      CALL WRITEA(' ','(A20)',' LOCAL/GLOBAL MAX. =')
      CALL WRITEF('*',IFP,PMAX)
      CALL WRITEC('/')
      CALL WRITEF('*',IFP,GMAX)
      CALL WRITEA(' ','(A20)',' LOCAL/GLOBAL MIN. =')
      CALL WRITEF('*',IFP,PMIN)
      CALL WRITEC('/')
      CALL WRITEF('*',IFP,GMIN)
      CALL WRITEA(' ','(A20)','         INCREMENT =')
      CALL WRITEF('*',IFP,DELR)
      CALL SCLCHR ( 1.0/1.0, 1.0/1.0 )
C
C
      CALL BRKPLT
C
C     Make sure no bad defaults
C
      I6 = MAX0 ( MIN0 ( IM, I6 ), 1 )
      J6 = MAX0 ( MIN0 ( JM, J6 ), 1 )
      K6 = MAX0 ( MIN0 ( KM, K6 ), 1 )
C
      RETURN
C
C
1000  FORMAT(A1)
C
C
C
2000  FORMAT(' '//' ',A80//' ',A32//' CONTOURING SELECTED '/)
2001  FORMAT(/' LOCAL MINIMUM VALUE ............',F13.4/
     &        ' GLOBAL MINIMUM VALUE ...........',F13.4/
     &        ' LOCAL MAXIMUM VALUE ............',F13.4/
     &        ' GLOBAL MAXIMUM VALUE ...........',F13.4/
     &        ' NUMBER OF CONTOURS .......',I8,' (Max = 125)'/
     &        ' CONTOUR INTERVAL .........',F13.4,' <=== DEFAULT'/
     &        ' ENTER REQUIRED INTERVAL ---> '/)
2020  FORMAT('    TYPE :-'//
     &       '   "M"  for Meridional Plot'/
     &       '   "Q"  for Quasi-Orthogonal Plot'/
     &       '   "S"  for Quasi Stream-surface'//
     &       '   "',A1,'" <=== DEFAULT'/)
2021  FORMAT(//'  ENTER integer value (K) of quasi-stream surface for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2022  FORMAT(//'  ENTER integer value (J) of quasi-orthogonal for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2023  FORMAT(//'  ENTER integer value (I) of quasi-meridional for'/
     +         '        which the variable is to be plotted.'/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2024  FORMAT(//'  ENTER Zoom magnification that you require'/
     +         '        DEFAULT ======>',F5.1/)
2025  FORMAT(//'  ENTER integer value (I) of pitchwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2026  FORMAT(//'  ENTER integer value (J) of streamwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2027  FORMAT(//'  ENTER integer value (K) of quasi-stream surface on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2032  FORMAT(//'  The next set of numbers control the view you'/
     +         '  will have of the bladerow. Since the orientation'/
     +         '  of the plot is not consistent from case to'/
     +         '  case, it is suggested that you first view'/
     +         '  the data using the default settings'//
     +         '  ENTER angle of rotation about page-normal direction'/
     +         '  which is the first rotation to be performed'/
     +         '        DEFAULT ======> ',F6.1,' degrees'/)
2031  FORMAT(//'  This is then followed by a rotation about the'/
     +         '  nominal tangential direction.'/
     +         '  ENTER the angle of rotation about this axis'/
     +         '        DEFAULT ======> ',F6.1,' degrees'/)
2030  FORMAT(//'  ENTER angle of final rotation about axial direction'/
     +         '        DEFAULT ======> ',F6.1,' degrees'/)
2060  FORMAT(//'  ENTER "Y" if you want filled in contours'/
     +         '        "N" if you do not '/
     +         '        DEFAULT ======> ',A1/)
      END
C
C
C--------------------------------------------------------------------------
C
C
       SUBROUTINE sub37 ( xd,yd,zd, i0,j0,k0, 
     &                     ia,ja,ka, ib,jb,kb, ic,jc,kc, inside, 
     &                     vxp, vtp, vrp )
C
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
C
       LOGICAL inside
C
       rans = 0
C
       X0           =  X(J0,K0)
       Y0           =  T(I0,J0,K0)
       Z0           =  R(J0,K0)
C
       AX           =  X(JA,KA) -  X0
       AY           =  T(IA,JA,KA) -  Y0
       AZ           =  R(JA,KA) -  Z0
C
       BX           =  X(JB,KB) -  X0
       BY           =  T(IB,JB,KB) -  Y0
       BZ           =  R(JB,KB) -  Z0
C
       CX           =  X(JC,KC) -  X0
       CY           =  T(IC,JC,KC) -  Y0
       CZ           =  R(JC,KC) -  Z0
C
       dx           =  xd -  x0
       dy           =  yd -  y0
       dz           =  zd -  z0
C
       ans1 = dx*ay*bz + dy*az*bx + dz*ax*by
     &       -dx*az*by - dy*ax*bz - dz*ay*bx
C
       ans2 = dx*by*cz + dy*bz*cx + dz*bx*cy
     &       -dx*bz*cy - dy*bx*cz - dz*by*cx
C
       ans3 = dx*cy*az + dy*cz*ax + dz*cx*ay
     &       -dx*cz*ay - dy*cx*az - dz*cy*ax
C
       ans4 = cx*ay*bz + cy*az*bx + cz*ax*by
     &       -cx*az*by - cy*ax*bz - cz*ay*bx
     &       - ans1 - ans2 - ans3
C
       sum = ans1+ans2+ans3+ans4
C
       IF ( 
     &      ans1/sum . GE . -0.0010 .AND.
     &      ans2/sum . GE . -0.0010 .AND.
     &      ans3/sum . GE . -0.0010 .AND.
     &      ans4/sum . GE . -0.0010 
     &    ) THEN
         INSIDE = . TRUE .
       ELSE
         INSIDE = . FALSE .
         vxp    =   0.0
         vtp    =   0.0
         vrp    =   0.0
       ENDIF

C
       IF ( .NOT. inside ) RETURN
C
       vxp = vx(ia,ja,ka)*ans2
     &      +vx(ib,jb,kb)*ans3
     &      +vx(ic,jc,kc)*ans1
     &      +vx(i0,j0,k0)*ans4
       vxp = vxp  / sum
C
       vtp = vt(ia,ja,ka)*ans2
     &      +vt(ib,jb,kb)*ans3
     &      +vt(ic,jc,kc)*ans1
     &      +vt(i0,j0,k0)*ans4
       vtp = vtp  / sum
C
       vrp = vr(ia,ja,ka)*ans2
     &      +vr(ib,jb,kb)*ans3
     &      +vr(ic,jc,kc)*ans1
     &      +vr(i0,j0,k0)*ans4
       vrp = vrp  / sum
C
C
       RETURN
       END
C******************************************************************C
C                                                                  C
C    PARTICLE TRACKING - main subroutine                           C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB38 ( NAME, X0, Y0, X1, Y1, ID )
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK19/ XX(95,2500),YY(95,2500),ZZ(95,2500),
     &                WORK1(95,2500),WORK2(95,2500)
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      CHARACTER*32 NAME
      CHARACTER*72 JBNAME,TSNUM
C
      REAL EMACH,IMACH
C
      LOGICAL inside
C
      CHARACTER*1  CTYPE, ANS
      COMMON / OPT6 / I6, J6, K6, DELR, ZSCALE, I6Z, J6Z, K6Z, 
     &                BETAXX, BETAYY, BETAZZ, ANS, CTYPE
C
C     DATA STATEMENT
C
      NROW = (95)
C
C
C         set constants
C
       pi    = 3.1415926
       piby2 = pi / 2.
       rad   = pi/180.
C
       kmm1=km-1
       imm1=im-1
       jmm1=jm-1
C
       imid = (im+1)/2
       jmid = (jm+1)/2
       kmid = (km+1)/2
C
       istart = 1
       iend   = im
       ijump  = (im+4)/5
C
       jstart = 1
       jend   = jm
       jjump  = (jm+4)/5
C
       kstart = 1
       kend   = km
       kjump  = (km+4)/5
C
C         TELL USER OF STATUS
C
      WRITE (6,2000) JBNAME,NAME
C
C      Zoom ?
C
C
      WRITE (6,2024) ZLAST
      ZIN = RREAD(1)
      IF (ZIN.GT.0.01) THEN
          ZSCALE = ZIN
          ZLAST  = ZSCALE
      ELSE 
          ZSCALE = ZLAST
      ENDIF
C
      IF(ZIN.GT.0.01) THEN
C
   35     WRITE (6,2025) IM, I6Z
          IANS = IREAD(1)
          IF (IANS.NE.0) I6Z = IANS
          IF (I6Z.LT.1.OR.I6Z.GT.IM) GO TO 35
C
   36     WRITE (6,2026) JM, J6Z
          IANS = IREAD(1)
          IF (IANS.NE.0) J6Z = IANS
          IF (J6Z.LT.1.OR.J6Z.GT.JM) GO TO 36
C
   37     WRITE (6,2027) KM, K6Z
          IANS = IREAD(1)
          IF (IANS.NE.0) K6Z = IANS
          IF (K6Z.LT.1.OR.K6Z.GT.KM) GO TO 37
C
      ENDIF
C
C         ESTABLISH LIMITS
C
      XXMID = 0.5*(XMAX+XMIN)
      YYMID = 0.5*(YMAX+YMIN)
      ZZMID = 0.5*(ZMAX+ZMIN)
C
      RANGE = AMAX1 ( (XMAX-XMIN), (YMAX-YMIN), (ZMAX-ZMIN) )
C
      XXMIN = XXMID - 0.5*RANGE
      XXMAX = XXMID + 0.5*RANGE
      YYMIN = YYMID - 0.5*RANGE
      YYMAX = YYMID + 0.5*RANGE
      ZZMIN = ZZMID - 0.5*RANGE
      ZZMAX = ZZMID + 0.5*RANGE
C
      XC = 0.5*(X0+X1)
      YC = 0.5*(Y0+Y1)
C
C         Set axes rotations for correct view along -ve z-axis
C
C
      WRITE (6,2032) betazz
      RANS = RREAD(1)
      IF (rans.NE.0.0) betazz=rans
C
      WRITE (6,2031) betayy
      RANS = RREAD(1)
      IF (rans.NE.0.0) betayy=rans
C
      WRITE (6,2030) betaxx
      RANS = RREAD(1)
      IF (rans.NE.0.0) betaxx=rans
C
C
C      adjust angles as required
C
       betaz = betazz * rad
       betay = betayy * rad
       betax = betaxx * rad
C
C
C       Determine scaling parameters
C
      RADIUS  = AMIN1 ( (X1-X0), (Y1-Y0) ) * 0.5
C
      scalep  = 2.*radius/range
C
C
       itmax  = 20
       ispbox =  5
C
       WRITE (6,2033) itmax
       IANS = IREAD(1)
       IF ( ians.GT.0 ) itmax=ians
C
       WRITE (6,2034) ispbox
       IANS = IREAD(1)
       IF ( ians.NE.0 ) ispbox = ians
C
C     Input particle positions
C
      WRITE (6,2035) istart
      IANS = IREAD(1)
      IF (ians.NE.0) istart=ians
C
      WRITE (6,2036) iend
      IANS = IREAD(1)
      IF (ians.NE.0) iend=ians
C
      IF ( istart.NE.iend ) THEN
        WRITE (6,2037) ijump
        IANS = IREAD(1)
        IF (ians.NE.0) ijump=ians
      ELSE
        ijump = 1
      ENDIF
C
      WRITE (6,2045) jstart
      IANS = IREAD(1)
      IF (ians.NE.0) jstart=ians
C
      WRITE (6,2046) jend
      IANS = IREAD(1)
      IF (ians.NE.0) jend=ians
C
      IF ( jstart.NE.jend ) THEN
        WRITE (6,2047) jjump
        IANS = IREAD(1)
        IF (ians.NE.0) jjump=ians
      ELSE
        jjump = 1
      ENDIF
C
      WRITE (6,2055) kstart
      IANS = IREAD(1)
      IF (ians.NE.0) kstart=ians
C
      WRITE (6,2056) kend
      IANS = IREAD(1)
      IF (ians.NE.0) kend=ians
C
      IF ( kstart.NE.kend ) THEN
        WRITE (6,2057) kjump
        IANS = IREAD(1)
        IF (ians.NE.0) kjump=ians
      ELSE
        kjump = 1
      ENDIF
C
C        BEGIN PLOTTING
C        --------------
C
      CALL SELPLT(8)
C
C      ADD TITLES
C
      XX0 = 5.
      XX1 = 190.
      XX2 = XX0
      XX3 = XX1
      XX4 = 0.25*267.
      XX5 = 0.75*267.
      XX6 = 175.
C
      YY0 = 7.
      YY1 = 1.
      YY2 = 12.
C
      CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
      CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
      CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
C
C         SET WINDOW & ZOOM
C
      CALL AWINDW ( X0, Y0, X1, Y1, -1 )
      CALL SUB40 ( x(J6Z,K6Z),r(J6Z,K6Z),t(I6Z,J6Z,K6Z),TMID,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XXZOOM,YYZOOM,XC,YC)
C
      CALL zoom  (XXZOOM,YYZOOM,ZSCALE,ZSCALE)
      CALL SCLCHR (1./zscale,1./zscale)
C
C     Plot the hub
C
      k = 1
      DO 30 j=1,jm
      DO 31 i=1,im
       CALL SUB40 ( x(j,k),r(j,k),t(i,j,k),TMID,BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(i,j),YY(i,j),XC,YC)
   31 CONTINUE
   30 CONTINUE
C
C      
CCCCC      CALL sub39(XX,YY,NROW,0,1,IM,3,1,JM,3)
C
C
C     Now, plot the casing
C
      k = km
      DO 40 j=1,jm
      DO 41 i=1,im
       CALL SUB40 ( x(j,k),r(j,k),t(i,j,k),TMID,BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(i,j),YY(i,j),XC,YC)
   41 CONTINUE
   40 CONTINUE
C
C   
CCCCCC      CALL sub39(XX,YY,NROW,0,1,IM,3,1,JM,3)
C
C
C     Now, plot the pressure side of the blades
C
      DO 49 nb=1,2
      i=im
      DO 50 k=1,km
      DO 51 j=1,jm
       CALL SUB40 ( x(j,k),r(j,k),t(i,j,k)-pitch(j,k)*(nb-1),tmid,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(k,j),YY(k,j),XC,YC)
   51 CONTINUE
   50 CONTINUE
C
C   
      CALL sub39(XX,YY,NROW,0,1,KM,MAX0(1,KM/4),JLE,JTE,(JTE-JLE)/8)
C
C
C     Now, plot the suction side of the blades
C
      i=1
      DO 60 k=1,km
      DO 61 j=1,jm
       CALL SUB40 ( x(j,k),r(j,k),t(i,j,k)+pitch(j,k)*(nb-1),tmid,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX(k,j),YY(k,j),XC,YC)
   61 CONTINUE
   60 CONTINUE
C
C   
      CALL sub39(XX,YY,NROW,0,1,KM,MAX0(1,KM/4),JLE,JTE,(JTE-JLE)/8)
C
   49 CONTINUE
C
C     Finally, start tracking particles (used 3-D interpolation)
C     ----------------------------------------------------------
C
C      Note that standard t-m convention is used for I,J,K
C
C
       dt = (smmax-smmin) / 
     &       ( 0.5*( SQRT(vx(imid, 1,kmid)**2+vr(imid, 1,kmid)**2)
     &             +SQRT(vx(imid,jm,kmid)**2+vr(imid,jm,kmid)**2) ) )
     &         / FLOAT(ispbox*jm)
C
C      initialise particle position
C
C
       DO 900 kk = kstart,kend,kjump
       DO 900 jj = jstart,jend,jjump
       DO 900 ii = istart,iend,ijump
         ip      = ii
         kp      = kk
         jp      = jj
C
         xp      = x(jp,kp)
         tp      = t(ip,jp,kp)
         rp      = r(jp,kp)
C
       CALL SUB40 ( xp,rp,tp,tmid,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,xxp,yyp,XC,YC)
C
         CALL moveto ( xxp, yyp )
C
         DO 500 it=1,itmax
          imin = MAX0( ip - 1,    1 )
          imax = MIN0( ip + 1, imm1 )
          jmin = MAX0( jp - 1,    1 )
          jmax = MIN0( jp + 1, jmm1 )
          kmin = MAX0( kp - 1,    1 )
          kmax = MIN0( kp + 1, kmm1 )
C     
          inside = .FALSE.
C
         DO 1200 K = kmin,kmax
            DO 1210 J = jmin,jmax
               DO 1220 I = imin,imax
C
C 1
                    CALL sub37 ( xp,tp,rp, i,j,k, i,j+1,k+1, 
     &                            i+1,j,k+1, i,j,k+1, inside,
     &                            vxp, vtp, vrp )
C 
                    IF ( inside ) GO TO 1300
C 2 
                    CALL sub37 ( xp,tp,rp, i,j,k, i+1,j+1,k+1, 
     &                            i+1,j,k, i+1,j,k+1, inside,
     &                            vxp, vtp, vrp )
C
                    IF ( inside ) GO TO 1300
C 3 
                    CALL sub37 ( xp,tp,rp, i,j,k, i,j+1,k+1, 
     &                            i+1,j+1,k+1, i+1,j,k+1, inside,
     &                            vxp, vtp, vrp )
C
                    IF ( inside ) GO TO 1300
C 4 
                    CALL sub37 ( xp,tp,rp, i+1,j+1,k+1, i,j+1,k, 
     &                            i+1,j,k, i+1,j+1,k, inside,
     &                            vxp, vtp, vrp )
C
                    IF ( inside ) GO TO 1300
C 5 
                    CALL sub37 ( xp,tp,rp, i+1,j+1,k+1, i,j+1,k, 
     &                            i,j+1,k+1, i,j,k, inside,
     &                            vxp, vtp, vrp )
C
                    IF ( inside ) GO TO 1300
C 6 
                    CALL sub37 ( xp,tp,rp, i+1,j+1,k+1, i,j+1,k, 
     &                            i,j,k, i+1,j,k, inside,
     &                            vxp, vtp, vrp )
C
                    IF ( inside ) GO TO 1300
C  
C  
C
 1220          CONTINUE
 1210       CONTINUE
 1200    CONTINUE
C
         IF ( .NOT. INSIDE ) THEN
             GO TO  900
         ENDIF
C
 1300    CONTINUE
C
C        Move particles to next point
C
         ip = i
         jp = j
         kp = k
C
         xp = xp + vxp*dt
         tp = tp + vtp*dt/rp
         rp = rp + vrp*dt
C
         CALL SUB40 ( xp,rp,tp,tmid,
     &                   BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,xxp,yyp,XC,YC)
C
         CALL drawto ( xxp, yyp )
C
  500  CONTINUE
C
  900 CONTINUE
C
      CALL BRKPLT
C
C     Make sure no bad defaults
C
      I6 = MAX0 ( MIN0 ( IM, I6 ), 1 )
      J6 = MAX0 ( MIN0 ( JM, J6 ), 1 )
      K6 = MAX0 ( MIN0 ( KM, K6 ), 1 )
C      
      RETURN
C
C
C
2000  FORMAT(' '//' ',A80//' ',A32//)
2024  FORMAT(//'  ENTER Zoom magnification that you require'/
     +         '        DEFAULT ======>',F5.1/)
2025  FORMAT(//'  ENTER integer value (I) of pitchwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2026  FORMAT(//'  ENTER integer value (J) of streamwise coord. on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2027  FORMAT(//'  ENTER integer value (K) of quasi-stream surface on'/
     +         '        which the zoom is to be centred.    '/
     +         '        Maximum value =',I4/
     +         '        DEFAULT ======>',I4/)
2032  FORMAT(//'  The next set of numbers control the view you'/
     +         '  will have of the bladerow. Since the orientation'/
     +         '  of the plot is not consistent from case to'/
     +         '  case, it is suggested that you first view'/
     +         '  the data using the default settings'//
     +         '  ENTER angle of rotation about page-normal direction'/
     +         '  which is the first rotation to be performed'/
     +         '        DEFAULT ======> ',F6.1,' degrees'/)
2031  FORMAT(//'  This is then followed by a rotation about the'/
     +         '  nominal tangential direction.'/
     +         '  ENTER the angle of rotation about this axis'/
     +         '        DEFAULT ======> ',F6.1,' degrees'/)
2030  FORMAT(//'  ENTER angle of final rotation about axial direction'/
     +         '        DEFAULT ======> ',F6.1,' degrees'/)
2033  FORMAT(//'  The trajectories of the particles are calculated'/
     +         '  using interpolated velocities which are integrated'/
     +         '  forwards in time to give a new position. The time'/
     +         '  step used to integrate the velocities is the same'/
     +         '  for each particle.'/
     +         '  ENTER number of time steps per particle'/
     +         '        DEFAULT ======>',I4/)
2034  FORMAT(//'  The next parameter determines the size of the time'/
     +         '  step. Specifying a larger number will reduce the'/
     +         '  time step'/
     +         '  ENTER average number of steps across a volume'/
     +         '        DEFAULT ======>',I4/)
2035  FORMAT(//'  The next set of numbers are used to control where'/
     +         '  you put the particles. All particles start at grid'/
     +         '  nodes. A single point, a line, a plane or a volume'/
     +         '  can be specified.'/
     +       //'  ENTER integer number of start position for'/
     +         '        placing particles in the pitchwise (I)'/
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2036  FORMAT(//'  ENTER integer number of end position for'/
     +         '        placing particles in the pitchwise (I)'/
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2037  FORMAT(//'  ENTER integer number for increment between the start'/
     +         '        and finish positions in the pitchwise (I)'/ 
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2045  FORMAT(//'  ENTER integer number of start position for'/
     +         '        placing particles in the streamwise (J)'/
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2046  FORMAT(//'  ENTER integer number of end position for'/
     +         '        placing particles in the streamwise (J)'/
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2047  FORMAT(//'  ENTER integer number for increment between the start'/
     +         '        and finish positions in the streamwise (J)'/ 
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2055  FORMAT(//'  ENTER integer number of start position for'/
     +         '        placing particles in the spanwise (K)'/
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2056  FORMAT(//'  ENTER integer number of end position for'/
     +         '        placing particles in the spanwise (K)'/
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
2057  FORMAT(//'  ENTER integer number for increment between the start'/
     +         '        and finish positions in the spanwise (K)'/ 
     +         '        direction.'/
     +         '        DEFAULT ======>',I4/)
C
      END
C******************************************************************C
C                                                                  C
C     Draws meshes from 2-D arrays                                 C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB39 ( XX,YY,NROW,LTYPE,
     &       I1,I2,IDEL,J1,J2,JDEL )
C
      DIMENSION XX(NROW,J2),YY(NROW,J2)
C
      imax = i2 + idel
      jmax = j2 + jdel
C
      CALL lintyp(ltype)
C
      DO 100 i=i1,imax,idel
        ii=MIN0(i,i2)
        CALL moveto(xx(ii,j1),yy(ii,j1))
        DO 101 j=j1,j2
          CALL drawto(xx(ii,j),yy(ii,j))
  101   CONTINUE
  100 CONTINUE
C
      DO 200 j=j1,jmax,jdel
        jj=min0(j,j2)
        CALL moveto(xx(i1,jj),yy(i1,jj))
        DO 201 i=i1,i2
          CALL drawto(xx(i,jj),yy(i,jj))
  201   CONTINUE
  200 CONTINUE
C
      CALL lintyp(0)
C
      RETURN
      END
C******************************************************************C
C                                                                  C
C                                                                  C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB40 ( X,R,T,TMID,BETAX,BETAY,BETAZ,
     &                   XXMID,YYMID,ZZMID,
     &                   SCALEP,XX,YY,XC,YC)
C
C       The data is now scaled to match the prescribed limits
C
       ax =  scalep * ( x             - xxmid )
       ay =  scalep * ( r*SIN(t-tmid) - yymid )
       az =  scalep * ( r*COS(t-tmid) - zzmid )
C
C       Rotate about the z-axis
C
       xa =  COS ( betaz ) * ax - SIN ( betaz ) * ay
       ya =  SIN ( betaz ) * ax + COS ( betaz ) * ay
       za =  az
C
C
C       Rotate about the y-axis
C
       ay =  ya
       az =  COS ( betay ) * za - SIN( betay ) * xa
       ax =  SIN ( betay ) * za + COS( betay ) * xa
C
C
C       Rotate about the x-axis to create paper coordinates
C
       xx =  ax                                     + xc
       yy =  COS ( betax ) * ay - SIN( betax ) * az + yc
C
       RETURN
       END
C******************************************************************C
C                                                                  C
C     Pitchwise averaging/plotting of data                         C
C                                                                  C
C******************************************************************C
C
      SUBROUTINE SUB45 ( name )
C
      COMMON / BLK1 / JBNAME,TSNUM,NSTEP
      COMMON / BLK2 / X(2500,95),T(95,2500,95),R(2500,95),
     &                SM(2500,95),PITCH(2500,95),COSA(2500),SINA(2500)
      COMMON / BLK6 / VX(95,2500,95),VT(95,2500,95),VR(95,2500,95)
      COMMON / BLK9 / P(95,2500,95)
      COMMON / BLK10/ RO(95,2500,95)
      COMMON / BLK11/ M_ABS(95,2500,95),M_REL(95,2500,95)
      COMMON / BLK14/ Q(95,2500,95),QQ(95,2500,95),NQ,NQQ
      COMMON / BLK15/ IM,JM,KM,ISPLIT,KSPLIT,JLE,JTE,JSLE,JSTE,CP,GA,
     &                W(2500),IND(2500),NBLADES(2500),PI,JLEE(50),
     &                JTEE(50),NROWS
      COMMON / BLK16/ ALPHA1,ALPHA2,EMACH,IMACH
      COMMON / BLK21/ XMIN,XMAX,RTMIN,RTMAX,YMIN,YMAX,SMMIN,SMMAX,
     &                ZMIN,ZMAX,RTMID,TMID,SRMIN,SRMAX,RMIN,RMAX,ZLAST
C
      CHARACTER*32 NAME,NQ,NQQ
      CHARACTER*72 JBNAME,TSNUM
C
      REAL M_ABS, M_REL, EMACH, IMACH
C
      REAL        F0(95),   F1(95),   F2(95),    F3(95),
     &            F4(95),   F5(95),   F6(95),    F7(95),
     &            F8(95),   F9(95),   F10(95),   F11(95),
     &            F12(95),  F13(95),
     &            PIF0(95,2500), PIF1(95,2500), PIF2(95,2500),  
     &            PIF3(95,2500), PIF4(95,2500), PIF5(95,2500), 
     &            PIF6(95,2500), PIF7(95,2500), 
     &            PIF9(95,2500), PIF10(95,2500),PIF11(95,2500),
     &            PIF12(95,2500), PIF13(95,2500),
     &            SPIF0R(2500) ,SPIF1R(2500), SPIF2R(2500), 
     &            SPIF3R(2500), SPIF4R(2500), SPIF5R(2500),
     &            SPIF6R(2500), SPIF7R(2500),
     &            SPIF9R(2500),SPIF10R(2500),SPIF11R(2500),
     &            SPIF12R(2500),SPIF13R(2500),
     &            SPIF0X(2500) ,SPIF1X(2500), SPIF2X(2500), 
     &            SPIF3X(2500), SPIF4X(2500), SPIF5X(2500),
     &            SPIF6X(2500), SPIF7X(2500), 
     &            SPIF9X(2500),SPIF10X(2500),SPIF11X(2500),
     &            SPIF12X(2500),SPIF13X(2500),
     &            dx(95,2500), dr(95,2500),
     &            xvar(95), yvar(95)
C
C
       cv = cp / ga
       gc = cp - cv
C
C
       rad    = 3.14159/180.
       deg    = 1.0/rad
C
       imid = (im+1)/2
       kmid = (km+1)/2
C
       IF ( km . LE . 1 ) RETURN
C                            
C
C
C         TELL USER OF STATUS
C
      WRITE (6,2000) JBNAME,NAME
C
      WRITE (6,2083) IMID, KMID
C
C
   23 WRITE (6,2022) JM
      JPOS = IREAD(1)
      IF (JPOS.GT.JM . OR . JPOS.LT.2 ) GO TO 23
C
C      Begin to integrate to find mean & mixed out values
C      =================================================
C
C
      DO 50 j = 1, jpos, jpos-1
       DO 60 k = 1, km
         DO 70 i = 1, im
C
           entj = cv * ALOG ( P(I,J,K)  /  P(IMID,1,KMID) )
     &          - cp * ALOG ( RO(I,J,K) / RO(IMID,1,KMID) )
C
           F0 ( i )  = r( j, k ) * 1.0
           F1 ( i )  = r( j, k ) * ro ( i, j, k )
           F2 ( i )  = r( j, k ) * ro ( i, j, k ) * vx ( i, j, k )
           F3 ( i )  = r( j, k ) * ro ( i, j, k ) * vx ( i, j, k )
     &                                         * vr ( i, j, k )
           F4 ( i )  = r( j, k ) * ro ( i, j, k ) * vx ( i, j, k )
     &                                         * vt ( i, j, k )
           F5 ( i )  = r( j, k ) * ro ( i, j, k ) * vx ( i, j, k )**2
           F6 ( i )  = r( j, k ) * ro ( i, j, k ) * vr ( i, j, k )
     &                                         * vt ( i, j, k )
           F7 ( i )  = r( j, k ) * ro ( i, j, k ) * vx ( i, j, k ) 
     &                                            * entj
           F8 ( i )  = r( j, k ) * p  ( i, j, k )
           F9 ( i )  = r( j, k ) * ro ( i, j, k ) * vr ( i, j, k )
           F10 ( i ) = r( j, k ) * ro ( i, j, k ) * vr ( i, j, k )**2
           F11 ( i ) = r( j, k ) * ro ( i, j, k ) * vr ( i, j, k )
     &                                            * entj
           F12 ( i ) = r( j, k ) * ro ( i, j, k ) * vx ( i, j, k )
     &                 * ( vt ( i, j, k ) + w ( j ) * r ( j, k ) )
           F13 ( i ) = r( j, k ) * ro ( i, j, k ) * vr ( i, j, k )
     &                 * ( vt ( i, j, k ) + w ( j ) * r ( j, k ) )
C
   70    CONTINUE
C      
C        pitchwise integrate F0 ...... F12 to give PIF0 ...... PIF12
C
         CALL sub47 ( t(1,j,k),  F0, im,  PIF0(k,j) )
         CALL sub47 ( t(1,j,k),  F1, im,  PIF1(k,j) )
         CALL sub47 ( t(1,j,k),  F2, im,  PIF2(k,j) )
         CALL sub47 ( t(1,j,k),  F3, im,  PIF3(k,j) )
         CALL sub47 ( t(1,j,k),  F4, im,  PIF4(k,j) )
         CALL sub47 ( t(1,j,k),  F5, im,  PIF5(k,j) )
         CALL sub47 ( t(1,j,k),  F6, im,  PIF6(k,j) )
         CALL sub47 ( t(1,j,k),  F7, im,  PIF7(k,j) )
         CALL sub47 ( t(1,j,k),  F9, im,  PIF9(k,j) )
         CALL sub47 ( t(1,j,k), F10, im, PIF10(k,j) )
         CALL sub47 ( t(1,j,k), F11, im, PIF11(k,j) )
         CALL sub47 ( t(1,j,k), F12, im, PIF12(k,j) )
         CALL sub47 ( t(1,j,k), F13, im, PIF13(k,j) )
C
         yvar ( k ) = r ( j, k )
         xvar ( k ) = x ( j, k )
C
         dx ( j, k )   = 0.5*( x(j,MIN0(k+1,km)) - x(j,MAX0(k-1,1) ) )
         dr ( j, k )   = 0.5*( r(j,MIN0(k+1,km)) - r(j,MAX0(k-1,1) ) )
C
   60  CONTINUE
C
C      spanwise integrate PIF0.....PIF12 to give SPIF0......SPIF12
C
       CALL sub47 ( YVAR, PIF0(1,j), km,  SPIF0R(j) )
       CALL sub47 ( YVAR, PIF1(1,j), km,  SPIF1R(j) )
       CALL sub47 ( YVAR, PIF2(1,j), km,  SPIF2R(j) )
       CALL sub47 ( YVAR, PIF3(1,j), km,  SPIF3R(j) )
       CALL sub47 ( YVAR, PIF4(1,j), km,  SPIF4R(j) )
       CALL sub47 ( YVAR, PIF5(1,j), km,  SPIF5R(j) )
       CALL sub47 ( YVAR, PIF6(1,j), km,  SPIF6R(j) )
       CALL sub47 ( YVAR, PIF7(1,j), km,  SPIF7R(j) )
       CALL sub47 ( YVAR, PIF9(1,j), km,  SPIF9R(j) )
       CALL sub47 ( YVAR,PIF10(1,j), km, SPIF10R(j) )
       CALL sub47 ( YVAR,PIF11(1,j), km, SPIF11R(j) )
       CALL sub47 ( YVAR,PIF12(1,j), km, SPIF12R(j) )
       CALL sub47 ( YVAR,PIF13(1,j), km, SPIF13R(j) )
C
       CALL sub47 ( XVAR, PIF0(1,j), km,  SPIF0X(j) )
       CALL sub47 ( XVAR, PIF1(1,j), km,  SPIF1X(j) )
       CALL sub47 ( XVAR, PIF2(1,j), km,  SPIF2X(j) )
       CALL sub47 ( XVAR, PIF3(1,j), km,  SPIF3X(j) )
       CALL sub47 ( XVAR, PIF4(1,j), km,  SPIF4X(j) )
       CALL sub47 ( XVAR, PIF5(1,j), km,  SPIF5X(j) )
       CALL sub47 ( XVAR, PIF6(1,j), km,  SPIF6X(j) )
       CALL sub47 ( XVAR, PIF7(1,j), km,  SPIF7X(j) )
       CALL sub47 ( XVAR, PIF9(1,j), km,  SPIF9X(j) )
       CALL sub47 ( XVAR,PIF10(1,j), km, SPIF10X(j) )
       CALL sub47 ( XVAR,PIF11(1,j), km, SPIF11X(j) )
       CALL sub47 ( XVAR,PIF12(1,j), km, SPIF12X(j) )
       CALL sub47 ( XVAR,PIF13(1,j), km, SPIF13X(j) )
C
   50 CONTINUE
C
C      END of input data - Start of plotting spanwise averages
C      =======================================================
C
C
C      (Re-)Initialise plotter
C
       CALL selplt ( 8 )
       CALL origin ( 286.0, 0., 0)
       CALL rotate ( 90., 1 )
C
C      ADD TITLES
C
       XX0 = 5.
       XX1 = 190.
       XX2 = XX0
       XX3 = XX1
       XX4 = 75.
       XX5 = 175.
C
       YY0 = 14.
       YY1 = 8.
       YY2 = 19.
C
       CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
       CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
       CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
C
       s = (xx1-xx0)/80./3.
       CALL sclchr ( s, s )
       CALL bgnwrt ( XX0, YY2, 0)
       CALL writea ( '*', '(A3)','J =')
       CALL writei ( '*', '(I4)',JPOS)
       CALL sclchr ( 1./s, 1./s )
C
       dh      = 45.
       dhh     = 75.
       h0      = 195.0
       h1      = h0 + dh
C
C      Set up spanwise coordinate
C
       span = 0.
       xvar ( 1 ) = span
C
       DO 5049 k = 2, km
           span = span + SQRT( ( r( jpos, k ) - r ( jpos, k-1 ) ) **2 
     &                       + ( x( jpos, k ) - x ( jpos, k-1 ) ) **2 ) 
           xvar ( k ) = span 
 5049  CONTINUE
C 
C
C      First, plot the meridional-velocity-density-ratio
C
C
C      total mass flux ratio:
C
       ave     = ABS ( ( SPIF2R ( jpos ) + SPIF9X ( jpos ) ) /       
     &                 ( SPIF2R (    1 ) + SPIF9X (    1 ) ) )
C
       DO 5050 k = 1, km
           yvar ( k ) = ( PIF2 ( k,jpos )*dr( jpos,k ) 
     &                  + PIF9 ( k,jpos )*dx( jpos,k ) )
     &                / ( PIF0 ( k,jpos )*dr( jpos,k )  
     &                  + PIF0 ( k,jpos )*dx( jpos,k ) )
     &                / ( SPIF2R ( jpos ) + SPIF9X ( jpos ) )
     &                * ( SPIF0R ( jpos ) + SPIF0X ( jpos ) )
 5050  CONTINUE
C
C
       hmax    =  1.20
       hmin    =  0.00
       hinc    =  0.20
C
C
       CALL sub46 ( xvar , yvar, km, h0, h1,
     &              hmin, hmax, hinc,
     &              '(F6.2)',
     &              '         M-V-D-R              ',
     &              ave,
     &              'Omega   = ' ,
     &              span)
C
C      then, the yaw angle
C
       h0      = h0 - dhh
       h1      = h0 + dh
       hmax    =  10.0
       hmin    = -10.0
       hinc    =   5.0
C
       ave        = ( SPIF4R ( jpos ) + SPIF6X ( jpos ) ) / 
     &            SQRT( ( SPIF5R ( jpos ) + SPIF3X ( jpos ) )**2 + 
     &                  ( SPIF3R ( jpos ) + SPIF10X( jpos ) )**2 ) 
       ave        = ATAN ( ave ) * deg
C
       DO 5051 k = 1, km
           yvar ( k ) = ( PIF4 ( k,jpos )*dr( jpos,k ) 
     &                  + PIF6 ( k,jpos )*dx( jpos,k ) ) /
     &            SQRT( ( PIF5 ( k,jpos )*dr( jpos,k)
     &                  + PIF3 ( k,jpos )*dx ( jpos,k) )**2 
     &                + ( PIF3 ( k,jpos )*dr ( jpos,k) 
     &                  + PIF10( k,jpos )*dx ( jpos,k) )**2 )
           yvar ( k ) = ATAN ( yvar ( k ) ) * deg - ave
 5051  CONTINUE
C
       CALL sub46 ( xvar , yvar, km, h0, h1,
     &              hmin, hmax, hinc,
     &              '(F6.2)',
     &              '   Yaw ( r ) - Yaw2           ',
     &              ave,
     &              'Yaw2    = ' ,
     &              span)
C
C      then, the pitch angle
C
       h0      = h0 - dhh
       h1      = h0 + dh
       hmax    =  10.0
       hmin    = -10.0
       hinc    =   5.0
C
       ave     = ATAN2 ( ( SPIF3R ( jpos ) + SPIF10X( jpos ) ) ,
     &                   ( SPIF5R ( jpos ) + SPIF3X ( jpos ) ) )
       ave     = ave * deg
C
       DO 5052 k = 1, km
           yvar ( k ) = ATAN2 ( ( PIF3 ( k,jpos )*dr ( jpos,k) 
     &                          + PIF10( k,jpos )*dx ( jpos,k) ) ,
     &                          ( PIF5 ( k,jpos )*dr( jpos,k)
     &                          + PIF3 ( k,jpos )*dx ( jpos,k) ) )
           yvar ( k ) = yvar ( k ) * deg - ave
 5052  CONTINUE
C
       CALL sub46 ( xvar , yvar, km, h0, h1,
     &              hmin, hmax, hinc,
     &              '(F6.2)',
     &              '   Pitch ( r ) - Pitch2       ',
     &              ave,
     &              'Pitch2  = ' ,
     &              span)
C
C
C
C      (Re-)Initialise plotter
C      -----------------------
C
       CALL clrplt
C
C      ADD TITLES
C
       XX0 = 5.
       XX1 = 190.
       XX2 = XX0
       XX3 = XX1
       XX4 = 75.
       XX5 = 175.
C
       YY0 = 14.
       YY1 = 8.
       YY2 = 19.
C
       CALL TITLE(XX0,YY0,XX1,YY0,1,JBNAME,80)
       CALL TITLE(XX2,YY1,XX3,YY1,1, TSNUM,80)
       CALL TITLE(XX4,YY2,XX5,YY2,1,  NAME,32)
C
       s = (xx1-xx0)/80./3.
       CALL sclchr ( s, s )
       CALL bgnwrt ( XX0, YY2, 0)
       CALL writea ( '*', '(A3)','J =')
       CALL writei ( '*', '(I4)',JPOS)
       CALL sclchr ( 1./s, 1./s )
C
       dh      = 45.
       dhh     = 75.
       h0      = 195.0
       h1      = h0 + dh
C
C      Now, plot the total pressure loss
C
C
C
C      find reference values in realtive frame
C
       v1sq    = vx(imid, 1,kmid)**2+vt(imid, 1,kmid)**2+
     &           vr(imid, 1,kmid)**2
       p1      = p(imid, 1,kmid)
       ro1     = ro(imid, 1,kmid)
       t1      = p1/(ro1*gc)
       t01     = t1 + v1sq/(2.*cp)
       p01     = p1*(t01/t1)**(ga/(ga-1.))
C
       v2sq    = vx(imid,jm,kmid)**2+vt(imid,jm,kmid)**2+
     &           vr(imid,jm,kmid)**2
       p2      = p(imid,jm,kmid)
       ro2     = ro(imid,jm,kmid)
       t2      = p2/(ro2*gc)
       t02     = t2 + v2sq/(2.*cp)
       p02     = p2*(t02/t2)**(ga/(ga-1.))
C
C
       ave     = ( SPIF7R ( jpos ) + SPIF11X ( jpos ) ) /
     &           ( SPIF2R ( jpos ) +  SPIF9X ( jpos ) )
       ave     = EXP ( -ave / gc )
       ave     = ( 1./ ave - 1. ) / 
     &               AMAX1 ( 1. - p1/p01 , 1. - p2/p02 )
C
       DO 5055 k = 1, km
           yvar ( k ) = ( PIF7 ( k,jpos )*dr( jpos,k ) 
     &                  + PIF11( k,jpos )*dx( jpos,k ) )
     &                / ( PIF2 ( k,jpos )*dr( jpos,k )  
     &                  + PIF9 ( k,jpos )*dx( jpos,k ) )
           yvar ( k ) = EXP ( -yvar ( k ) / gc )
           yvar ( k ) = ( 1. / yvar ( k ) - 1. ) / 
     &               AMAX1 ( 1. - p1/p01 , 1. - p2/p02 )
 5055  CONTINUE
C
C
       hmax    =  0.20
       hmin    = -0.05
       hinc    =  0.05
C
C
       CALL sub46 ( xvar , yvar, km, h0, h1,
     &              hmin, hmax, hinc,
     &              '(F6.2)',
     &              '(Exp(S/R)-1) / (1-Pref/P0ref) ',
     &              ave,
     &              'L Coeff.= ' ,
     &              span)
C
C      Now, the static pressure variation
C
       h0      = h0 - dhh
       h1      = h0 + dh
C
C
       ave     = p(imid,jpos,kmid) /
     &             AMAX1 ( p01 - p1 , p02 - p2 )
C
       DO 5056 k = 1, km
           yvar ( k ) =  p( imid,jpos,k ) /
     &             AMAX1 ( p01 - p1 , p02 - p2 ) - ave
 5056  CONTINUE
C
C
       hmax    =  0.3
       hmin    = -0.3
       hinc    =  0.2
C
C
       CALL sub46 ( xvar , yvar, km, h0, h1,
     &              hmin, hmax, hinc,
     &              '(F6.2)',
     &              ' ( p - pave ) / (P0ref - pref)',
     &              ave,
     &              'pave/dyh= ' ,
     &              span)
C
C      then, the absolute yaw angle
C
       h0      = h0 - dhh
       h1      = h0 + dh
       hmax    =  10.0
       hmin    = -10.0
       hinc    =   5.0
C
       ave        = ( SPIF12R ( jpos ) + SPIF13X ( jpos ) ) /
     &            SQRT( ( SPIF5R ( jpos ) + SPIF3X ( jpos ) )**2 + 
     &                  ( SPIF3R ( jpos ) + SPIF10X( jpos ) )**2 ) 
       ave        = ATAN ( ave ) * deg
C
       DO 5057 k = 1, km
           yvar ( k ) = ( PIF12 ( k,jpos )*dr( jpos,k ) 
     &                  + PIF13 ( k,jpos )*dx( jpos,k ) ) /
     &            SQRT( ( PIF5 ( k,jpos )*dr( jpos,k)
     &                  + PIF3 ( k,jpos )*dx ( jpos,k) )**2 
     &                + ( PIF3 ( k,jpos )*dr ( jpos,k) 
     &                  + PIF10( k,jpos )*dx ( jpos,k) )**2 )
           yvar ( k ) = ATAN ( yvar ( k ) ) * deg - ave
 5057  CONTINUE
C
       CALL sub46 ( xvar , yvar, km, h0, h1,
     &              hmin, hmax, hinc,
     &              '(F6.2)',
     &              ' Abs. Yaw ( r ) - Yaw2        ',
     &              ave,
     &              'Yaw2    = ' ,
     &              span)
C
       CALL brkplt
C
C      Finish
C      ======
C         
C
C
       RETURN
C
C
2000  FORMAT(' '//' ',A80//' ',A32/)
2022  FORMAT(//'  ENTER integer value (J) of quasi-orthogonal for'/
     +         '        which the data is to be plotted.'/
     +         '        Maximum value =',I4/)
2083  FORMAT(//'  WARNING - this variable is calculated using a'/
     &         '            reference value determined using'/
     &         '            conditions at:  I =',I5/
     &         '                            J =    1'/ 
     &         '                            K =',I5/)
C
       END      
C
C
C**********************************************************************C
C                                                                      C
C                      PLOTTING SUBROUTINE                             C
C                                                                      C
C**********************************************************************C
C
C
       SUBROUTINE sub46 ( x, y, npts, y0, y1,
     &                    ymin, ymax, yint,
     &                    yfmt,
     &                    ylab, ave, avelbl, span )
C
C                       
       CHARACTER*30 ylab
       CHARACTER*10 avelbl
       CHARACTER*6  yfmt
C
       REAL    x ( npts ), y ( npts )
C
C
C      set constants
C
       DATA
     &     nxtic /    1  /,
     &     nytic /    1  /,
     &     xtic  /    2. /,
     &     ytic  /    2. /,
     &     x0    /   65. /,
     &     x1    /  185. /,
     &     nxchar/   20  /,
     &     nychar/   30  /,
     &     nachar/   17  /,
     &     achar /    2. /
C
C
C
C      set up axes
C
       dx    = x1 - x0
       dy    = y1 - y0
C
C
C      find plotting range for x-axis
C
       xmax   = x ( npts )/span
       xmin   = x ( 1 )/span
       xint   = 0.1
C
C
C      set ranges
C
       xrange = ( xmax - xmin )
       yrange = ( ymax - ymin )
C
C
C      draw axes
C
       CALL sclchr( 0.8, 0.8 )
       CALL axis ( x0, y0, xmin, x1, y0, xmax, xint, xtic, nxtic,
     &                                               '(F6.1)'    )
C
C
       CALL axis ( x0, y0, ymin, x0, y1, ymax, yint, ytic, nytic,
     &                                                yfmt       )
C
C
C      Draw the points
C
       DO 5010 i = 1, npts
C
C
       CALL pltsym ( ( x(i)/span  - xmin ) * dx / xrange + x0,
     &               ( y(i)       - ymin ) * dy / yrange + y0,
     &                                                    'C' )
C
C
 5010  CONTINUE
C
C
C      join points up
C
       CALL moveto ( ( x(1)/span  - xmin ) * dx / xrange + x0,
     &               ( y(1)       - ymin ) * dy / yrange + y0 )
C
C
       DO 5020 i = 2, npts
C
C
       CALL drawto ( ( x(i)/span  - xmin ) * dx / xrange + x0,
     &               ( y(i)       - ymin ) * dy / yrange + y0 )
C
C
 5020  CONTINUE
C
C
C      Plot mean level
C
       CALL moveto( x0 , ( 0.0 - ymin ) * dy / yrange + y0 )
C
C
       CALL dashby( dx, 0.0, 6 )
C
C
C      label axes
C
       CALL xlabel ( '      SPAN          ', nxchar )
       CALL ylabel ( ylab, nychar )
C
C     output mean value
C
      CALL bgnwrt ( x0 + 0.5 * dx - FLOAT ( nachar ) * achar * 0.5,
     &               y1 , 0 )
      CALL writea ( ' ', '(A10)', avelbl )
      CALL writef ( '*', '(F8.4)', ave )
C
      CALL sclchr ( 1.0/0.8, 1.0/0.8 )
C
      RETURN
      END
C**********************************************************************C
C                                                                      C
C     Integration routine                                              C
C                                                                      C
C**********************************************************************C
C  
C
       SUBROUTINE sub47 ( x, y, npnts, intgrl )
C
C      Subroutine to find the definite integral defined by a series of
C      points.
C
C      Arguments :-
C
C      x               -  X data values.
C      y               -  Y data values.
C      npnts           -  Number of data values.
C      intgrl          -  Definite integral.
C
C
       INTEGER npnts, point
C
       REAL    intgrl, x ( npnts ), y ( npnts )
C
C
C      Integrate
C
       intgrl = 0.0
C
       DO 4 point = 2, npnts
           intgrl = intgrl + ( y ( point ) + y ( point - 1 ) ) * 0.5 *
     &                       ( x ( point ) - x ( point - 1 ) )
C
4      CONTINUE
C
       RETURN
C
       END
C**********************************************************************C
C                                                                      C
C      Read a single real number with default of zero                  C 
C                                                                      C 
C**********************************************************************C
C
C
C
       REAL*4 FUNCTION rread ( iunit ) 
C
C
C      Note that the coding is not strictly correct in that truncation
C      errors might lead to an slightly different value. To avoid this, 
C      the summation should begin with the least significant number.
C
       CHARACTER*1  achar
       CHARACTER*20 buffer
       LOGICAL      beforep
C
C
       iconv ( achar ) = ICHAR ( achar ) - ICHAR ( '0' )
C
C
       READ ( iunit, 100, end=900 ) buffer
  100  FORMAT( a20 )
C
C      
       rread = 0.0
       beforep = .TRUE.
       ipower  = 0
C
C      Check for negative sign
C
       IF ( buffer ( 1:1 ) . EQ . '-' ) THEN
            negat  = -1
            istart =  2
       ELSE
            negat  =  1
            istart =  1
       ENDIF
C
C
       DO 60 i = istart, 20
           IF ( iconv ( buffer ( i:i ) ) . GE . iconv ( '0' ) . AND .
     &          iconv ( buffer ( i:i ) ) . LE . iconv ( '9' ) . AND .
     &          beforep ) THEN
C
C               Before decimal point & valid character
C
               rread = rread*10 + 
     &               FLOAT ( negat * iconv ( buffer ( i:i ) ) ) 
           ELSE
     &     IF ( iconv ( buffer ( i:i ) ) . GE . iconv ( '0' ) . AND .           
     &          iconv ( buffer ( i:i ) ) . LE . iconv ( '9' ) . AND .
     &          .NOT. beforep ) THEN
C               
C               NOT Before decimal point & valid character
C
                ipower = ipower + 1
                rread = rread + 
     &                FLOAT ( negat * iconv ( buffer ( i:i ) ) ) 
     &                / 10** ipower    
           ELSE 
     &     IF ( buffer ( i:i ) . EQ . '.' ) THEN     
C
C              This is the decimal point
C
               beforep = .FALSE.
           ELSE 
C
C              End of the input
C
               RETURN
C
           ENDIF
   60  CONTINUE
C
C
       RETURN
C
  900  STOP
       END
C**********************************************************************C
C                                                                      C
C      Read a single Integer number with default of zero               C 
C                                                                      C 
C**********************************************************************C
C
C
C
       INTEGER*4 FUNCTION iread ( iunit ) 
C
C
       CHARACTER*1  achar
       CHARACTER*20 buffer
C
       iconv ( achar ) = ICHAR ( achar ) - ICHAR ( '0' )
C
C
       READ ( iunit, 100, end=900 ) buffer
  100  FORMAT( a20 )
C
C      
       IREAD = 0.0
       ipower  = 0
C
C      Check for negative sign
C
       IF ( buffer ( 1:1 ) . EQ . '-' ) THEN
            negat  = -1
            istart =  2
       ELSE
            negat  =  1
            istart =  1
       ENDIF
C
C
       DO 60 i = istart, 20
           IF ( iconv ( buffer ( i:i ) ) . GE . iconv ( '0' ) . AND .
     &          iconv ( buffer ( i:i ) ) . LE . iconv ( '9' ) ) THEN
C
C               Before decimal point & valid character
C
               iread = iread*10 + 
     &                     ( negat * iconv ( buffer ( i:i ) ) ) 
           ELSE
C
C              End of the input
C
               RETURN
C
           ENDIF
   60  CONTINUE
C
C
       RETURN
C
  900  STOP
       END
C******************************************************************************
C
C
       SUBROUTINE INTP(N,XN,YN,X,Y) 
C      THIS SUBROUTINE INTERPOLATES IN THE GIVEN TABLE OF YN AS A 
C      FUNCTION OF XN TO FIND THE VALUE OF Y AT THE INPUT VALUE 
C      OF X. 
C 
      DIMENSION XN(N),YN(N)
      Y=0. 
      L=1 
      NM=N 
      IF(N.LT.4) GO TO 8 
      NM=4 
    4 IF(X.LT.XN(L)) GO TO 5 
      IF(L.EQ.N) GO TO 3 
      L=L+1 
      GO TO 4 
    5 IF(L.GT.2) GO TO 6 
      L=1 
      GO TO 8 
    6 IF(L.NE.N) GO TO 7 
    3 L=N-3 
      GO TO 8 
    7 L=L-2 
    8 DO 11 L1=1,NM 
      CO=1 
      DO 10 L2=1,NM 
      IF(L1.EQ.L2) GO TO 9 
      TEMP=(X-XN(L+L2-1))/(XN(L+L1-1)-XN(L+L2-1)) 
      GO TO 10 
    9 TEMP=1 
   10 CO=CO*TEMP 
   11 Y=Y+CO*YN(L+L1-1) 
      RETURN 
      END 
 
C
C
C******************************************************************************
