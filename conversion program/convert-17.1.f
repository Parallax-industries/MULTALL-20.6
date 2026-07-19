C 
C      INCLUDE 'commall-open-15.1'
      INCLUDE 'commall-open-17.1'
      OPEN(UNIT=5,FILE='old_readin.dat')
      OPEN(UNIT=1,FILE='/dev/tty')
      OPEN(UNIT=2,FILE='new_readin.dat')
C
C
      DO 100 NCALL = 1,2
C
      IF(NCALL.EQ.1) THEN
          CALL OLD_READIN(1)
      END IF
C
      REWIND(5)
C
      IF(NCALL.EQ.2) THEN
           CALL OLD_READIN(2)
      END IF
C
  100 CONTINUE
C
      CLOSE(5)
      STOP
      END
C*******************************************************************************
C*******************************************************************************
C*******************************************************************************
C
      SUBROUTINE OLD_READIN(NCALL)
C
C       THIS SUBROUTINE READS IN THE DATA IN
C       ====================

C
      INCLUDE 'commall-open-17.1'
C
      COMMON/BKODDS/
     &           XINT(JD),YINT(JD),RINT(JD),SDIST(JD),
     &           ICUSP(NRS),LCUSP(NRS),LCUSPUP(NRS),
     &           FRACNEW(JD),BETANEW(JD),SLOPE(JD),
     &           THICKUP(JD),THICKLOW(JD),
     &           XINT1(KD),XINT2(KD),XINT3(KD),XINT4(KD)
C
      DIMENSION UPF(JD),ONF(JD),DOWNF(JD),SOLD(JD),SNEW(JD),
     & XBUF(JD),YBUF(JD),YSNEW(JD), XFRACUP(JD),RELSPUP(JD),
     & XFRACON(JD),RELSPON(JD),XFRACDWN(JD),RELSPDWN(JD)
C
C
C       START OF INPUT SECTION
C       THROUGHOUT THE INPUT SECTION THE VARIABLES ARE AS FOLLOWS
C       =====================
C
C       RT_UPP(J,K)  IS TEMPORARY STORE FOR BLADE SUCTION SURFACE CO-ORD'S
C       RT_THICK(J,K) IS THE BLADE TANGENTIAL THICKNESS
C       RCYL(K)    IS RADIUS OF K'TH CYLINDRICAL SURFACE IF INPUT IS ON
C                  CYLINDRICAL SURFACES.
C       R(J,1)     IS RADII OF HUB
C       R(J,KM)    IS RADII OF CASING
C
C       =====================
 1000 FORMAT(16I5)
 1100 FORMAT(8F10.4)
 1200 FORMAT(A72)
 1610 FORMAT(40I2)
 1700 FORMAT(8F10.3)
 1701 FORMAT(4F10.4,I5)
 1702 FORMAT(I5,F10.6)
 1703 FORMAT(3F10.6,I5)
 1704 FORMAT(5I5,F10.4)
 1705 FORMAT(I5,F10.6,I5)
 1708 FORMAT(8F10.1)
 1709 FORMAT(F10.4,I5)
 1710 FORMAT(2F10.6,I5)
 1711 FORMAT(F10.1,5F10.4)
 1715 FORMAT(8F12.7)
 1800 FORMAT(18A4)
C
      PI     = 3.14159265
      DEGRAD = PI/180.
      RADDEG = 180./PI
C
      READ(5,1200)  TITLE
      WRITE(6,1200) TITLE
C
C************FIRST READ IN MAIN INTEGER CONTROL VARIABLES ************
C
      READ(5,1000) IM,JDUM,KM,IF_ROUGH,NSTEPS_MAX,IFCOOL,IFBLEED,NOSECT,
     &             NROWS,IFMIX,ISHIFT,KIN,NEXTRAP_LE,NEXTRAP_TE,NCHANGE
      WRITE(6,1000)IM,JDUM,KM,IF_ROUGH,NSTEPS_MAX,IFCOOL,IFBLEED,NOSECT,
     &             NROWS,IFMIX,ISHIFT,KIN,NEXTRAP_LE,NEXTRAP_TE,NCHANGE
C
      READ(5,1000) INMACH,INSURF,INVR,ITIMST,IDUMY,IPOUT,INFLOW,
     &             ILOS,NLOS,IINST,IDUMY,IBOUND,IF_REPEAT
      WRITE(6,1000)INMACH,INSURF,INVR,ITIMST,IDUMY,IPOUT,INFLOW,
     &             ILOS,NLOS,IINST,IDUMY,IBOUND,IF_REPEAT
C
C**********************************************************************
C    SET THE COEFFICIENTS FOR THE SSS SCHEME
C   SAVE  THE INPUT VALUE OF ITIMST  AS IT MAY BE CHANGED IN THE NEXT SECTION.
      I_TIMST  = ITIMST
C
      IF(ITIMST.EQ.3.OR.ITIMST.EQ.5) THEN
              F1          =  2.0000
              F2          = -1.000
              F3          =  0.00
              F2EFF       = -1.0
              NRSMTH      =  0
              RSMTH       =  0.40
      END IF
C
      IF(ITIMST.EQ.4) THEN
              F1          =  2.0000
              F2          = -1.65
              F3          = -0.65
              F2EFF       = -1.0
              NRSMTH      =  1
              RSMTH       =  0.40
              ITIMST      = 3
      END IF
C
      IF(ITIMST.EQ.-4) THEN
              READ(5,*) F1, F2EFF, F3 , RSMTH, NRSMTH
              IF(F2EFF.GT.0.0) THEN 
              WRITE(6,*) 'ERROR,  F2EFF  MUST BE NEGATIVE.'
              WRITE(6,*) 'THE SIGN OF THE INPUT VALUE WILL BE CHANGED.'
              F2EFF = -F2EFF
              END IF
              IF(F3.GT.0.0) THEN 
              WRITE(6,*) 'ERROR,  F3  MUST BE NEGATIVE.'
              WRITE(6,*) 'THE SIGN OF THE INPUT VALUE WILL BE CHANGED.'
              F3  = -F3
              END IF
              F2     = F2EFF*(1.0 - F3)
              ITIMST = 3
      END IF
C******************************************************************************
C      
C  Q3D
      IF(KM.EQ.2) IBOUND = 2
C  END Q3D
C
      READ (5,1000) IR,JR,KR,IRBB,JRBB,KRBB
      WRITE(6,1000) IR,JR,KR,IRBB,JRBB,KRBB
C
C  Q3D
      IF(KM.EQ.2) THEN
           KR   = 1
           KRBB = 1
      END IF
C  END Q3D
C
      IF_KINT = 0
      IF(KIN.LT.0)     IF_KINT = 1
      KIN = ABS(KIN)      
      IF(KIN.EQ.0)     KIN  = KM
      IF(KIN.NE.KM)    IF_KINT = 1
C
      IF(NEXTRAP_LE.EQ.0)     NEXTRAP_LE  = 5
      IF(NEXTRAP_TE.EQ.0)     NEXTRAP_TE  = 5
      IF(NCHANGE.EQ.0) NCHANGE = NSTEPS_MAX/4
      IF(NLOS.EQ.0)    NLOS = 5
      IMM1 = IM-1
      IMM2 = IM-2
      KMM1 = KM-1
      KMM2 = KM-2
C
C
C     CHECK THAT THE DIMENSIONS ARE NOT TOO LARGE.
C
      IF(MAXKI.LT.KD)  WRITE(6,*)
     &      ' STOPPING BECAUSE MAXKI IS LESS THAN KD',
     &      ' MAXKI = ',MAXKI, ' KD = ',KD
      IF(MAXKI.LT.ID)  WRITE(6,*)
     &      ' STOPPING BECAUSE MAXKI IS LESS THAN ID',
     &      ' MAXKI = ',MAXKI, ' ID = ',ID
      IF(IM.GT.ID)  WRITE(6,*) ' STOPPING BECAUSE IM TOO LARGE.',
     &            ' IM= ',IM,  ' DIMENSION LIMIT = ',ID
      IF(KM.GT.KD)  WRITE(6,*) ' STOPPING BECAUSE KM TOO LARGE.',
     &            ' KM= ',KM,  ' DIMENSION LIMIT = ',KD
      IF(KIN.GT.MAXKI) WRITE(6,*) ' STOPPING BECAUSE KIN TOO LARGE.',
     &            ' KIN= ',KIN,' DIMENSION LIMIT = ',MAXKI
C
      IF(IM.GT.ID.OR.KM.GT.KD)  STOP
      IF(KIN.GT.MAXKI.OR.MAXKI.LT.KD.OR.MAXKI.LT.ID) STOP
C
      IF(NROWS.GT.NRS)  WRITE(6,*) 'STOPPING BECAUSE NROWS TOO LARGE.',
     &            ' NROWS= ',NROWS,' DIMENSION LIMIT = ',NRS
      IF(NROWS.GT.NRS) STOP
C
C     CHECK IF THERE ARE VARIABLE NUMBERS OF INPUT SECTIONS, i.e.  IF NOSECT IS NEGATIVE
C
      IF_SECTS = 0
      IF(NOSECT.LT.0) THEN
           IF_SECTS = 1
           NOSECT   = ABS(NOSECT)
      END IF
C
C
C
C***********INPUT THE BLADE GEOMETRY ON NOSECT STREAMWISE SURFACES******
C*********** THE GEOMETRY IS INPUT FOR EACH BLADE ROW SEPARATELY******
C
C            FIRST INPUT THE CONTROL PARAMETERS FOR THE BLADE ROW
C            AND SET THE VALUES OF SOME BLADE ROW VARIABLES
C
      J1        = 1
      IFSHROUD  = 0
C
      DO 1550 NR = 1,NROWS
C
C      READ IN THE ROW TITLE "ROWTYP" THIS IS NEVER USED BUT HELPS TO LAY OUT THE DATA
C
      READ(5,1200) ROWTYP
C
      IF(NCALL.EQ.2) THEN
          WRITE(2,*) ' BLANK LINE '
          WRITE(2,*) ' STARTING ROW NUMBER ', NR
          WRITE(2,*) ' BLADE ROW TITLE = ', ROWTYP
      END IF
C
C      KTIPS IS THE K VALUE OF THE POINT WHERE THE TIP GAP STARTS
C      KTIPE IS THE K VALUE OF THE POINT WHERE THE TIP GAP ENDS.
C      SET KTIPS(NR) = 0  FOR NO TIP GAP ON THIS ROW.    
C
C      SET KTIPS(NR) NEGATIVE TO USE THE SHROUD LEAKAGE MODEL ON THIS
C      ROW. EXTRA INPUT DATA IS THEN NEEDED AT THE END OF THE DATA FILE.
C
      READ(5,1000) JMROW,JLEROW,JTEROW,NBLADES_IN_ROW,
     &   KTIPS(NR),KTIPE(NR),JROTHS,JROTHE,JROTTS,JROTTE,
     &   NEW_GRID,JTRAN_I1(NR),JTRAN_IM(NR),JTRAN_K1(NR),
     &   JTRAN_KM(NR),IF_CUSP(NR)
C

C     INPUT THE NUMBER OF INPUT SECTIONS IF THIS IS NOT CONSTANT.
C
      IF(IF_SECTS.EQ.1) THEN
            READ(5,*) NSECS_NOW
      ELSE
            NSECS_NOW = NOSECT
      END IF
C    NSECS_IN  MUST BE SET HERE AS IT IS USED IN INTPOL
            NSECS_IN = NSECS_NOW
C
C     END VARIABLE INPUT SECTIONS
C   
      JLEE = J1+JLEROW-1
      JTEE = J1+JTEROW-1
C
C
      WRITE(6,1000) JMROW,JLEROW,JTEROW,NBLADES_IN_ROW,
     &   KTIPS(NR),KTIPE(NR),JROTHS,JROTHE,JROTTS,JROTTE,NEW_GRID,
     &   JTRAN_I1(NR),JTRAN_IM(NR),JTRAN_K1(NR),JTRAN_KM(NR),IF_CUSP(NR)
C
C
C     Set the cusp generation parameters if IF_CUSP is not 0.
C     Maintain the original grid and no cusp is generated if ICUSP = 99
C     The cusp is centred on the blade centre line if ICUSP = 0.
C     The cusp makes the I=1 surface continuous on the cusp if  ICUSP =  1.
C        The cusp makes the I=IM surface continuous on the cusp if ICUSP = -1.
C     The cusp is of length  LCUSP and starts LCUSPUP points before the
C     trailing edge.F
C
      IF(IF_CUSP(NR).EQ.1) THEN
	   READ(5,*) ICUSP(NR),LCUSP(NR),LCUSPUP(NR)
      ENDIF
C
C    IF  IF_CUSP = 2 a body force is used to force separation at the trailing edge.
C    The force starts NUP_I1 grid points upstream of the TE  om the I=1 blade surface, 
C    and at NUP_IM points upstream on the I=IM blade surface. It extends N_WAKE points
C    downstream of the TE. The thickness of the affected layer is determined bt SEP_THIK
C    , typical value  0.01, and the strength of the body force by SEP_DRAG, typical value 0.99.
C
      IF(IF_CUSP(NR).EQ.2) THEN
             READ(5,*) NUP_I1(NR),NUP_IM(NR),N_WAKE(NR),
     &                 SEP_THIK(NR),SEP_DRAG(NR)
             WRITE(6,*) NUP_I1(NR),NUP_IM(NR),N_WAKE(NR),
     &                  SEP_THIK(NR),SEP_DRAG(NR)
      END IF
C
C
C     READ IN THE RPM, TIP CLEARANCE AND INITIAL GUESS OF INLET AND EXIT 
C     PRESSURES FOR THIS BLADE ROW.
C
      READ(5,1100) RPMROW,PUPROW,PLEROW,PTEROW,PDNROW,FRACTIP(NR),RPMHUB
      WRITE(6,1100)RPMROW,PUPROW,PLEROW,PTEROW,PDNROW,FRACTIP(NR),RPMHUB
C
      IF(FRACTIP(NR).LT.0.0) THEN 
           READ(5,*) FRACTIP1(NR),FRACTIP2(NR)
           WRITE(6,*)' FRACTIP1,  FRACTIP2 = ',FRACTIP1(NR),FRACTIP2(NR)
      ELSE
           FRACTIP1(NR) = FRACTIP(NR)
           FRACTIP2(NR) = FRACTIP(NR)
      END IF
C
C
C      FTHICK(NR,K) IS THE MULTIPLYING FACTOR ON THE BLADE THICKNESS SO
C      THAT IT CAN BE REDUCED AT THE TIP. IT IS ASSUMED TO BE 1.0 UNLESS
C      INPUT HERE.
C
      IF(KTIPS(NR).GT.0)  READ(5,1100) (FTHICK(NR,K),K=1,KM)
      IF(KTIPS(NR).GT.0) WRITE(6,1100) (FTHICK(NR,K),K=1,KM)
C
C*****************************************************************************
C
      IF(NCALL.EQ.2) THEN
           WRITE(2,*)      ' NBLADES IN ROW'    
           WRITE(2,1000)     NBLADES_IN_ROW
C
           WRITE(2,*)     ' JMROW, JLEROW, JTEROW '
           WRITE(2,1000)    JMROW, JLEROW, JTEROW
C
           WRITE(2,*)     '  KTIP_START,  KTIP_END '
           WRITE(2,1000)     KTIPS(NR) ,  KTIPE(NR)
           IF(KTIPS(NR).NE.0.AND.KTIPE(NR).NE.0) THEN
                 WRITE(2,*)     ' FRAC_TIP1,  FRAC_TIP2 '
                 WRITE(2,1100)    FRACTIP1(NR), FRACTIP2(NR)
                 WRITE(2,*)     ' BLADE THICKNESS SCALING FACTOR '
                 WRITE(2,1100)   (FTHICK(NR,K),K=1,KM)
           END IF
C
           WRITE(2,*)    ' J VALUES FOR BL TRANSITION '
           WRITE(2,1000)   JTRAN_I1(NR),JTRAN_IM(NR),JTRAN_K1(NR),
     &                     JTRAN_KM(NR)
C
           IF(IF_CUSP(NR).EQ.1) THEN
	        WRITE(2,1000)    ICUSP(NR),LCUSP(NR),LCUSPUP(NR)
           ENDIF
           IF(IF_CUSP(NR).EQ.2) THEN
                 WRITE(2,1000)  NUP_I1(NR),NUP_IM(NR),N_WAKE(NR),
     &                          SEP_THIK(NR),SEP_DRAG(NR)
           END IF
C
      WRITE(2,*)    '  NEW GRID CHOICE '
      WRITE(2,1000)    NEW_GRID
C
      WRITE(2,*)    '  RPMROW  and RPMHUB ' 
      WRITE(2,1708)    RPMROW, RPMHUB  
C
      WRITE(2,*)     ' JROTHS, JROTHE, JROTTS, JROTTE '
      WRITE(2,1000)    JROTHS, JROTHE, JROTTS, JROTTE
C
      WRITE(2,*)     ' PUPROW, PLEROW, PTEROW, PDNROW'
      WRITE(2,1708)    PUPROW, PLEROW, PTEROW, PDNROW
C
      IF(INSURF.NE.2) THEN
          WRITE(6,*) ' WARNING ! '
          WRITE(6,*) ' CONVERSION IS ONLY POSSIBLE FOR INSURF = 2 DATA '
          WRITE(6,*) ' SETTING INSURF = 0 FOR "NEW_READIN" DATA  '
          WRITE(6,*)
      END IF
C
      IN_SURF = 0
      WRITE(2,*)     ' NSECS_NOW, IN_SURF '
      WRITE(2,1000)    NSECS_NOW, IN_SURF 
C
      WRITE(2,*)  ' IF_CUSP, IF_ANGLES '
      WRITE(2,1000) IF_CUSP(NR), IF_ANGLES(NR)
C
           IF(IF_CUSP(NR).EQ.1) THEN
	        WRITE(2,1000)    ICUSP(NR),LCUSP(NR),LCUSPUP(NR)
           ENDIF
           IF(IF_CUSP(NR).EQ.2) THEN
                 WRITE(2,1000)  NUP_I1(NR),NUP_IM(NR),N_WAKE(NR),
     &                          SEP_THIK(NR),SEP_DRAG(NR)
           END IF
C
C    END OF NCALL = 2 LOOP
      END IF
C
CC*****************************************************************************
C*******************************************************************************
C
      J2 = J1 + JMROW - 1
C
C      NOW  READ IN MAIN GEOMETRICAL DATA FOR THE BLADE ROW 
C      ON  NOSECT  BLADE SECTIONS OF THE CURRENT BLADE ROW
C
C**********************************************************************************
      ANGLEAN = 0.0
      DO 1555 K=1,NSECS_NOW
C
C
      READ(5,1700)  FAC1,XSHIFT,ROTATE,FRACX_ROT
      WRITE(6,1700) FAC1,XSHIFT,ROTATE,FRACX_ROT
C
C***********XSURF(J,K) IS AXIAL COORDINATE OF POINTS ON THE STREAMWISE SURFACE.
C
      READ(5,1700)  (XSURF(J,K),J=J1,J2)
      WRITE(6,1700) (XSURF(J,K),J=J1,J2)
C
C
C     LEAN THE WHOLE BLADE BY AN ANGLE ANGLEAN IF ANGLEAN IS GREATER THAN ZERO)
C
      IF(K.GT.1) THEN
           READ(5,1700)  FAC2,TSHIFT,FCAMBER
           WRITE(6,1700) FAC2,TSHIFT,FCAMBER
      ELSE
           READ(5,1700)  FAC2,TSHIFT,FCAMBER,ANGLEAN
           WRITE(6,1700) FAC2,TSHIFT,FCAMBER,ANGLEAN
           ANGLEAN  = ANGLEAN*3.1415926/180.
      END IF      
C
      IF_REDESGN = 0
      IF(FCAMBER.GT.0.001) IF_REDESGN = 1
C
C***********RT_UPP(J,K) IS THE R-THETA COORDINATE OF POINTS ON THE STREAMWISE
C           SURFACE ON THE BLADE SURFACE WITH LARGEST VALUE OF THETA.
C           i.e. THE UPPER SURFACE OF THE BLADE AND THE LOWER SURFACE OF THE
C                BLADE TO BLADE PASSAGE.
C
      READ(5,1700)  (RT_UPP(J,K),J=J1,J2)
      WRITE(6,1700) (RT_UPP(J,K),J=J1,J2)
C
C
      READ(5,1700)  FAC3,BETAUP(NR,K),BETADWN1(NR,K),BETADWN2(NR,K)
      IF(ABS(BETADWN2(NR,K)).LT.0.00001) BETADWN2(NR,K) = BETADWN1(NR,K)
      WRITE(6,1700) FAC3,BETAUP(NR,K),BETADWN1(NR,K),BETADWN2(NR,K)
C
C     SET IF_ANGLES FOR USE IN EARLIER OUTPUT ON NEXT CALL
           IF_ANGLES(NR) = 0
      IF(ABS(BETAUP(NR,K)).GT.0.01.OR.ABS(BETADWN1(NR,K)).GT.0.01) 
     &     IF_ANGLES(NR) = 1
C
C***********RT_THICK(J,K) IS BLADE THICKNESS DELTA R-THETA MEASURED
C           IN THE TANGENTIAL DIRECTION.
C
      READ(5,1700)  (RT_THICK(J,K),J=J1,J2)
      WRITE(6,1700) (RT_THICK(J,K),J=J1,J2)
C
C
C      IF INSURF =1 OR 2 RSURF(J,K) IS RADII OF POINTS ON THE STREAMWISE
C      SURFACES ON WHICH DATA IS INPUT.
C      IF INSURF =0 RCYL(J) IS THE RADIUS OF THE CYLINDRICAL SURFACES ON
C       DATA IS INPUT
C
      READ(5,1700)   FAC4,RSHIFT
      WRITE(6,1700)  FAC4,RSHIFT
C
C
      IF(INSURF.NE.0) READ(5,1700)  (RSURF(J,K),J=J1,J2)
      IF(INSURF.NE.0) WRITE(6,1700) (RSURF(J,K),J=J1,J2)
      IF(INSURF.EQ.0) READ(5,1700)  RCYL(K)
      IF(INSURF.EQ.0) WRITE(6,1700) RCYL(K)

C
C     END IF INPUT FOR THIS BLADE SECTION
C
C******************************************************************************
C******************************************************************************
      IF(NCALL.EQ.2) THEN
C
      WRITE(2,*)' BLANK LINE '
      WRITE(2,*)' BLADE ROW NUMBER ',  NR
      WRITE(2,*)  IF_REDESGN, 0 , 0 , 'IF_REDESGN, IFRESTAG, IFLEAN '
      WRITE(2,1710)  FAC1, XSHIFT
      WRITE(2,1715)  (XSURF(J,K),J = J1,J2)
      WRITE(2,1715)  FAC2, TSHIFT
      WRITE(2,1715)  (RT_UPP(J,K),J=J1,J2)
      WRITE(2,1715)  FAC3
      WRITE(2,1715)  (RT_THICK(J,K),J=J1,J2)
      WRITE(2,1715)  FAC4, RSHIFT
      WRITE(2,1715)  (RSURF(J,K),J=J1,J2)
C
C   INPUT THE NEW CAMBER LINE SLOPE AND UPPER AND LOWER TANGENTIAL THICKNESS.
C  AS FRACTIONS OF THE MERIDIONAL CHORD. 
      IF(IF_REDESGN.GT.0) THEN
C
      WRITE(2,*)   ROTATE, FRACX_ROT, ANGLEAN, IF_NEWGEOM
C
      WRITE(2,*) ' REDESIGN OPTION '
      IF(IF_NEWGEOM.GT.0) THEN
C
      WRITE(2,*) NNEW, NSMOOTH
      DO  NN = 1,NNEW
           WRITE(2,1100)FRACNEW(NN),BETANEW(NN),THICKUP(NN),THICKLOW(NN)
      END DO
           WRITE(2,1100)ANGROT,FTHIK
C    END OF IF_NEW_GEOM LOOP
      END IF
C    END OF IF_REDESGN LOOP
      END IF
C
C    END OF NCALL = 2 LOOP
      END IF 
C
 1555 CONTINUE
C
C*******************************************************************
C
c          INPUT THE HUB AND CASING GEOMETRY.
C          IF INSURF = 2 THE HUB AND CASING  ARE TAKEN AS THE
C          FIRST AND LAST STREAMWISE SURFACES. OTHERWISE READ IN
C          HUB AND CASING COORDINATES AT THE ENDS OF THE QUASI ORTHOGONALS.
C
      IF(INSURF.NE.2) THEN
C
      READ(5,1700)  (X(J,1),J=J1,J2)
      READ(5,1700)  (R(J,1),J=J1,J2)
      WRITE(6,1700) (X(J,1),J=J1,J2)
      WRITE(6,1700) (R(J,1),J=J1,J2)
      READ(5,1700)  (X(J,KM),J=J1,J2)
      READ(5,1700)  (R(J,KM),J=J1,J2)
      WRITE(6,1700) (X(J,KM),J=J1,J2)
      WRITE(6,1700) (R(J,KM),J=J1,J2)
C
      END IF
C
C****************************************************************************
C      CALL NEWGRID FOR THE CURRENT BLADE ROW IF REQUESTED
C      THIS READS IN MORE DATA AND GENERATES NEW STREAMWISE (J) GRID POINTS.


      IF(NEW_GRID.NE.0)  THEN

C
      WRITE(6,*) 'STARTING TO GENERATE A NEW GRID IN SUBROUTINE NEWGRID'
C
      READ(5,*)     NUP,NON,NDOWN
      WRITE(6,*)  ' NUP, NON, NDOWN = ', NUP,NON,NDOWN
C
      IF(NCALL.EQ.2) THEN
              WRITE(2,*) '  STARTING DATA FOR NEWGRID '
              WRITE(2,1000) NUP,NON,NDOWN
      END IF

C
C******************************************************************************
C     INPUT ONLY A FEW RELATIVE GRID SPACINGS
C     AND GENERATE THE GRID SPACINGS AUTOMATICALLY IF  NUP, NON OR NDOWN = 0.
C
C     READ IN  THE RELATIVE GRID SPACINGS, UPF(J) , ONF(J)  and  DOWNFJ) ,
C     if  NUP, NON or NDOWN > 0 .
C
C******************************************************************************
C******************************************************************************
C
      IF(NUP.EQ.0) THEN
           READ(5,*) NUP
           IF(NCALL.EQ.2) WRITE(2,1000) NUP
           NINUP = 0
 880       CONTINUE
           NINUP = NINUP+1
           READ(5,*) XFRACUP(NINUP),RELSPUP(NINUP)
           IF(NCALL.EQ.2) WRITE(2,1100)XFRACUP(NINUP),RELSPUP(NINUP)
           IF(XFRACUP(NINUP).GT.0.9999) GO TO 881
           GO TO 880
 881       CONTINUE
C
      ELSE
           READ(5,*)     (UPF(J),J=1,NUP)
           IF(NCALL.EQ.2) WRITE(2,1100)(UPF(J),J=1,NUP)
      ENDIF
C
C******************************************************************************
      IF(NON.EQ.0) THEN
           READ(5,*) NON
           IF(NCALL.EQ.2) WRITE(2,1000),NON
           NINON = 0
 882       CONTINUE
           NINON = NINON+1
           READ(5,*) XFRACON(NINON),RELSPON(NINON)
           IF(NCALL.EQ.2) WRITE(2,1100)XFRACON(NINON),RELSPON(NINON)
           IF(XFRACON(NINON).GT.0.9999) GO TO 883
           GO TO 882
 883       CONTINUE
C
      ELSE
           READ(5,*)     (ONF(J),J=1,NON)
           IF(NCALL.EQ.2) WRITE(2,1100)(ONF(J),J=1,NON)
      ENDIF
C
C******************************************************************************
      IF(NDOWN.EQ.0) THEN
           READ(5,*) NDOWN
           IF(NCALL.EQ.2) WRITE(2,1000) NDOWN
           NINDWN = 0
 884       CONTINUE
           NINDWN = NINDWN+1
           READ(5,*) XFRACDWN(NINDWN),RELSPDWN(NINDWN)
           IF(NCALL.EQ.2)WRITE(2,1100) XFRACDWN(NINDWN),RELSPDWN(NINDWN)
           IF(XFRACDWN(NINDWN).GT.0.9999) GO TO 885
           GO TO 884
 885       CONTINUE
C
      ELSE
           READ(5,*)     (DOWNF(J),J=1,NDOWN)
           IF(NCALL.EQ.2) WRITE(2,1100) (DOWNF(J),J=1,NDOWN)
      ENDIF
C
C******************************************************************************
C******************************************************************************
C     UPF(J), ONF(J) AND DOWNF(J) ARE NOW SET, PRINT THEM OUT .
C
      WRITE(6,1700) (UPF(J),J=1,NUP)
      WRITE(6,1700) (ONF(J),J=1,NON)
      WRITE(6,1700) (DOWNF(J),J=1,NDOWN)
C
C******************************************************************************
C      READ THE CHANGE IN THE UPSTREAM AND DOWNSTREAM EXTENT OF THE GRID
C      THESE MULTIPLY THE ORIGINAL UPSTREAM AND DOWNSTREAM
C      LENGTHS OF THE GRID BY UPEXT AND DWNEXT.
C
      READ(5,*)     UPEXT,DWNEXT
      IF(NCALL.EQ.2) WRITE(2,1100),UPEXT,DWNEXT
      WRITE(6,1700) UPEXT,DWNEXT
C
C     END OF INPUT DATA FOR GENERATING THE NEW GRID
C******************************************************************************

C   END OF NEWGRID INPUT DATA
      END IF

C*****************************************************************************

C
C        END OF DATA INPUT ON THIS BLADE ROW. RETURN TO INPUT DATA ON THE
C        NEXT ROW UNLESS THIS IS THE LAST, IE UNLESS NR=NROWS.
C
 1550 CONTINUE
C
C       END OF INPUT OF BLADE GEOMETRY DATA
C
      JM         = J2
      JMM1       = JM-1
      JMM2       = JM-2
      INDMIX(JM) = 0
C
C
C     CHECK THAT JM IS NOT TOO LARGE.
C
      IF(JM.GT.JD)  WRITE(6,*) 'STOPPING BECAUSE JM TOO LARGE.',
     &            ' JM= ',JM,' DIMENSION LIMIT = ',JD
      IF(JM.GT.JD) STOP
C

C******************************************************************************
C******************************************************************************
C**********READ IN GAS CONSTANTS ,TIME STEP LENGTH,SMOOTHING FACTOR,ETC.
C
      WRITE(6,*)'READING IN THE GAS CONSTANTS,TIME STEP LENGTH,SMOOTHING 
     &FACTOR,ETC. '
C
      READ(5,1100)  CP,GA,CFL,SFTIN,SFXIN,MACHLIM
      WRITE(6,1700) CP,GA,CFL,SFTIN,SFXIN,MACHLIM
      IF(MACHLIM.LT.1.0) MACHLIM = 2.0
C
C    USE REAL GAS PROPERTIES IF CP IS INPUT AS NEGATIVE.
C    TYPICAL VALUES FOR COMBUSTION PRODUCTS  ARE:CP1 = 1272.5, CP2 = 0.2125, CP3 = 0.000015625, RGAS = 287.15
C    AT  TREF = 1400 K.
C
      IF(CP.LT.0.0) THEN
      READ(5,*) CP1, CP2, CP3, TREF, RGAS
C
      WRITE(6,*) ' IDEAL GAS PROPERTIES READ IN '
      WRITE(6,*) ' CP1, CP2, CP3, TREF, RGAS =',CP1,CP2,CP3,TREF,RGAS
      END IF
C
C
      READ(5,1700)  DAMPIN,DUMY,FBLK1,FBLK2,FBLK3,SFEXIT,CONLIM,RFIN
      IF(SFEXIT.GT.0.0001) READ(5,*) NSFEXIT
      WRITE(6,1700) DAMPIN,DUMY,FBLK1,FBLK2,FBLK3,SFEXIT,CONLIM,RFIN
      IF(SFEXIT.GT.0.0001) WRITE(6,*) NSFEXIT
C
      IF(DAMPIN.LT.0.001)    DAMPIN = 10.
      IF(CONLIM.LT.0.000001) CONLIM = 0.005
      IF(RFIN.LT.0.000001)   RFIN = 0.10

C
C      READ IN INITIAL GUESS OF UPSTREAM AND DOWNSTREAM PRESSURES
C      ,STAGNATION TEMPERATURE , VELOCITIES,FLOW DIRECTIONS,ETC.
C
      READ(5,1700)  PUPHUB,PUPTIP,PDOWN_HUB,PDOWN_TIP,PLATE_LOSS,
     &              THROTTLE_EXIT,DUMY,F_PDOWN
      IF(THROTTLE_EXIT.GT.0.001) READ(5,*) THROTTLE_PRES, THROTTLE_MAS,
     &                                      RFTHROTL
      WRITE(6,1700) PUPHUB,PUPTIP,PDOWN_HUB,PDOWN_TIP,PLATE_LOSS,
     &              THROTTLE_EXIT,DUMY,F_PDOWN
      IF(THROTTLE_EXIT.GT.0.001) WRITE(6,*) THROTTLE_PRES,THROTTLE_MAS,
     &                                       RFTHROTL
C
      KMID=IFIX(0.5*KIN)
C  Q3D
      IF(KM.EQ.2) KMID = 1
C  END Q3D
C
C
      IF(NCALL.EQ.2) THEN
         WRITE(2,*) ' BLANK LINE '
         WRITE(2,*) ' NUMBER OF INLET BOUNDARY CONDITION POINTS '
         WRITE(2,1000)   KIN
      END IF
C
C
      WRITE(6,*)
      WRITE(6,*) ' READING IN THE INLET BOUNDARY CONDITIONS '
C
      READ(5,1708)  (PO1(K),K=1,KIN)
      WRITE(6,1708) (PO1(K),K=1,KIN)
      PO_IN_MID = PO1(KMID)
C
      IF(IPOUT.EQ.3) READ(5,1708)(PD(K),K=1,KIN)
      IF(IPOUT.EQ.3) WRITE(6,1708)(PD(K),K=1,KIN)
C
      READ(5,1700)  (TO1(K),K=1,KIN)
      WRITE(6,1700) (TO1(K),K=1,KIN)
      TO_IN_MID = TO1(KMID)
C
      READ(5,1700)  (VTIN(K),K=1,KIN)
      WRITE(6,1700) (VTIN(K),K=1,KIN)
      VT_IN_MID  = VTIN(KMID)
C
      READ(5,1700)  (VM1(K),K=1,KIN)
      WRITE(6,1700) (VM1(K),K=1,KIN)
C
      READ(5,1700)  (BS(K),K=1,KIN)
      WRITE(6,1700) (BS(K),K=1,KIN)
      YAW_IN_MID = BS(KMID)
C
      READ(5,1700)  (BR(K),K=1,KIN)
      WRITE(6,1700) (BR(K),K=1,KIN)
      PITCH_IN_MID  = BR(KMID)
C
      READ(5,1700)  (FR_IN(K),K=1,KIN-1)
      WRITE(6,1700) (FR_IN(K),K=1,KIN-1)
C
      READ(5,1700)  (FP(I),I=1,IMM1)
      WRITE(6,1700) (FP(I),I=1,IMM1)
C
      READ(5,1000)  (NOUT(L),L=1,5)
      WRITE(6,1000) (NOUT(L),L=1,5)
C
      READ(5,1610)  (IOUT(I),I=1,20)
      WRITE(6,1610) (IOUT(I),I=1,20)
C
      READ(5,1610)  (KOUT(K),K=1,KM)
      WRITE(6,1610) (KOUT(K),K=1,KM)
C
C*************************************************************************************
C*************************************************************************************
      IF(NCALL.EQ.2) THEN
C
      WRITE(2,*)      ' RELATIVE SPACING OF THE INLET BC POINTS '
      WRITE(2,1100)     (FR_IN(K),K=1,KIN-1)
      WRITE(2,*)      ' ABSOLUTE STAGNATION PRESSURE AT INLET '
      WRITE(2,1708)     (PO1(K),K=1,KIN)
      IF(IPOUT.EQ.3) THEN
           WRITE(2,*)     ' EXIT STATIC PRESSURE '
           WRITE(2,1708)    (PD(K),K=1,KIN)
      END IF
      WRITE(2,*)      ' INLET STAGNATION TEMPERATURE '
      WRITE(2,1100)     (TO1(K),K=1,KIN)
      WRITE(2,*)      ' INLET TANGENTIAL VELOCITY '
      WRITE(2,1100)     (VTIN(K),K=1,KIN)
      WRITE(2,*)      ' INLET MERIDIONAL VELOCITY '
      WRITE(2,1100)     (VM1(K),K=1,KIN)
      WRITE(2,*)      ' INLET YAW ANGLE  '
      WRITE(2,1100)     (BS(K),K=1,KIN)
      WRITE(2,*)      ' INLET MERIDIONAL PITCH ANGLE '
      WRITE(2,1100)     (BR(K),K=1,KIN)
C
      WRITE(2,*)   ' EXIT PRESSURES ON HUB AND TIP '
      WRITE(2,1708)  PDOWN_HUB, PDOWN_TIP
C
C   END OF NCALL = 2 LOOp
      END IF
C***************************************************************************************
C
C     READ IN IN THE SPECIFIED INLET FLOW AND RELAXATION FACTOR
C     IF INFLOW NOT = ZERO
C
      IF(INFLOW.NE.0) THEN
           READ(5,1100)  FLOWIN, RFLOW
           IF(NCALL.EQ.2)   WRITE(2,1100) FLOWIN, RFLOW
           WRITE(6,*)  ' FLOWIN, RFLOW ', FLOWIN, RFLOW
      END IF

C
C
C      READ IN DATA FOR VISCOUS FLOW MODELLING.
C

      REYNO   = 500000.0
      RF_VIS  = 0.5
      FTRANS  = 0.0001
      FAC_4TH  = 0.8
      PRANDTL = 1.0
      YPLUSWALL = 0.0
      TURBVIS_LIM = 3000.0
C
      IF(ILOS.NE.0) THEN
      READ(5,*,END=448,ERR=448)REYNO,RF_VIS,FTRANS,FAC_4TH,TURBVIS_LIM,
     &                         PRANDTL,YPLUSWALL
  448 CONTINUE
C
      IF(TURBVIS_LIM.LT.0.1) TURBVIS_LIM = 3000.0
C
      WRITE(6,*)
      WRITE(6,*) ' REYNOLDS NUMBER                       = ',REYNO
      WRITE(6,*) ' VISCOUS TERM RELAXATION FACTOR        = ',RF_VIS
      WRITE(6,*) ' TRANSITION FACTOR, FTRANS             = ',FTRANS
      WRITE(6,*) ' PROPORTION OF FOURTH ORDER SMOOTHING  = ', FAC_4TH
      WRITE(6,*) ' LIMIT ON TURBULENT/LAMINAR VISCOSITY  = ',TURBVIS_LIM
      WRITE(6,*) ' PRANDTL NUMBER                        = ',PRANDTL
      WRITE(6,*) ' YPLUSWALL - IF USED (NOT OFTEN USED)  = ',YPLUSWALL
      WRITE(6,*) ' MACH NUMBER  LIMITER, USUALLY 2.0     = ',MACHLIM
      WRITE(6,*)
C
      END IF
C
      IF(ILOS.GE.200)THEN
           FAC_STMIX = 0.0
           FAC_ST0   = 1.0
           FAC_ST1   = 1.0
           FAC_ST2   = 1.0
           FAC_ST3   = 1.0
           FAC_SFVIS = 2.0
           READ(5,*,END = 450,ERR= 450) FAC_STMIX, FAC_ST0, FAC_ST1,
     &                                  FAC_ST2, FAC_ST3, FAC_SFVIS
  450 CONTINUE
C
           WRITE(6,*)
           WRITE(6,*) ' SPALART-ALLMARAS TURBULENCE MODEL IS BEING USED'
           WRITE(6,*) ' THE S_A SOURCE TERM MULTIPLIERS ARE ',
     &                  FAC_STMIX, FAC_ST0, FAC_ST1, FAC_ST2, FAC_ST3 
           WRITE(6,*) ' THE TURBULENT VISCOSITY SMOOTHING FACTOR IS ', 
     &                  FAC_SFVIS
           WRITE(6,*)
      END IF
C
      WRITE(6,*)
C***************************************************************************************
C**************************************************************************************
C     READ IN THE MIXING PLANE PARAMETERS
C
      RFMIX    = 0.025
      FSMTHB   = 1.0
      FEXTRAP  = 0.95
      FANGLE   = 0.95
C
      IF(IFMIX.NE.0) THEN
      READ(5,*,END=449,ERR=449)  RFMIX,FEXTRAP,FSMTHB,
     &                           FANGLE
  449 CONTINUE
      WRITE(6,*)' THE MIXING PLANE PARAMETERS ARE '
      WRITE(6,*)' RFMIX, FEXTRAP, FSMTHB ,FANGLE',
     &            RFMIX, FEXTRAP, FSMTHB, FANGLE
      WRITE(6,*)
      END IF
C
C***************************************************************************************
C***************************************************************************************
C    SET THE SPANWISE GRID SPACINGS,  "FR(K)" .
C   THESE ARE THE SAME AS THE INLET BC SPACINGS, "FR_IN(K)" ,  IF "IF_KINT" = 0.
C
C
      IF(IF_KINT.EQ.1) THEN
C
C     READ IN THE RELATIVE SPANWISE SPACING OF THE GRID POINTS.
C     NOTE THAT THIS IS FORMATTED INPUT.
      READ(5,10)    (FR(K),K=1,KMM1)
   10 FORMAT(8F10.5)
C
      ELSE
C
      DO 655 K=1,KMM1
           FR(K) = FR_IN(K)
  655 CONTINUE
C
      ENDIF
C***************************************************************************************
C***************************************************************************************
C      READ IN THE MIXING LENGTH LIMITS IF ILOS IS NOT ZERO.
C
      IF(ILOS.NE.0) THEN
C
      IF(NCALL.EQ.2) WRITE(2,*) ' MIXING LENGTH LIMITS FOR EACH ROW.'
C
      DO 2345 N = 1,NROWS
           XLLIM_I1(N)  = 0.03
           XLLIM_IM(N)  = 0.03
           XLLIM_K1(N)  = 0.03
           XLLIM_KM(N)  = 0.03
           XLLIM_DWN(N) = 0.03
           XLLIM_UP(N)  = 0.03
           XLLIM_IN(N)  = 0.01
           XLLIM_LE(N)  = 0.03
           XLLIM_TE(N)  = 0.05
           XLLIM_DN(N)  = 0.05
           FSTURB(N)    = 1.0
           TURBVIS_DAMP(N) = 0.5
           IF(ILOS.LT.100) READ(5,*,ERR=2346)  XLLIM_I1(N),XLLIM_IM(N),
     &                 XLLIM_K1(N),XLLIM_KM(N),XLLIM_DWN(N),XLLIM_UP(N)
           IF(ILOS.GE.100) READ(5,*,ERR=2346)  XLLIM_IN(N),XLLIM_LE(N),
     &                XLLIM_TE(N),XLLIM_DN(N),FSTURB(N),TURBVIS_DAMP(N)
C
         IF(NCALL.EQ.2) THEN
           IF(ILOS.LT.100) WRITE(2,1100)  XLLIM_I1(N),XLLIM_IM(N),
     &                 XLLIM_K1(N),XLLIM_KM(N),XLLIM_DWN(N),XLLIM_UP(N)
           IF(ILOS.GE.100) WRITE(2,1100)  XLLIM_IN(N),XLLIM_LE(N),
     &                XLLIM_TE(N),XLLIM_DN(N),FSTURB(N),TURBVIS_DAMP(N)
         END IF

C           
 2346 CONTINUE
C
           WRITE(6,*) ' ROW NUMBER ', N
           IF(ILOS.LT.100) WRITE(6,*)'ILOS = 9/10 MIXING LENGTH LIMITS',
     &     XLLIM_I1(N),XLLIM_IM(N),XLLIM_K1(N),XLLIM_KM(N),XLLIM_DWN(N),
     &     XLLIM_UP(N)
           IF(ILOS.GE.100) WRITE(6,*) 'ILOS > 100 MIXING LENGTH LIMITS',
     &     XLLIM_IN(N),XLLIM_LE(N),XLLIM_TE(N),XLLIM_DN(N)
           IF(ILOS.GE.100) WRITE(6,*) 
     &     ' FREE STREAM TURBULENT VISCOSITY RATIO',FSTURB(N),
     &     ' MIXING PLANE TURBULENCE DECAY', TURBVIS_DAMP(N)
C
 2345 CONTINUE
C
C     READ IN A FACTOR TO INCREASE THE TURBULENT VISCOSITY FOR THE FIRST NMIXUP STEPS.
C
           FACMIXUP = 2.0
           NMIXUP   = 100
           READ(5,*, ERR= 2341)    FACMIXUP, NMIXUP
 2341 CONTINUE
           IF(NCALL.EQ.2) THEN
                WRITE(2,*)  ' FACMIXUP, NMIXUP '
                WRITE(2,1709) FACMIXUP, NMIXUP
           END IF
C
           WRITE(6,*) ' FACMIXUP = ',   FACMIXUP,' NMIXUP = ',NMIXUP
C
      ENDIF
C
C******************************************************************************
C  READ IN THE SURFACE ROUGHNESSES IN MICRONS IF  IF_ROUGH  >  0  .
C
      IF(IF_ROUGH.GT.0) THEN
C
	WRITE(6,*) ' Non-hydraulic smooth surfaces specified'
	WRITE(6,*) ' Input Surface Roughness in microns for ALL'
	WRITE(6,*) ' surfaces.'
      IF(NCALL.EQ.2) WRITE(2,*) ' SURFACE ROUGHNESSES IN MICRONS '
      DO 2347 N = 1,NROWS
           READ(5,*)  ROUGH_H(N),ROUGH_T(N),ROUGH_L(N),ROUGH_U(N)
           IF(NCALL.EQ.2) THEN
                WRITE(2,1100)ROUGH_H(N),ROUGH_T(N),ROUGH_L(N),ROUGH_U(N)
           END IF
C
           WRITE(6,*) ' ROW NUMBER ', N
           WRITE(6,*) ' Surface Roughnesses in microns ',
     &     ROUGH_H(N),ROUGH_T(N),ROUGH_L(N),ROUGH_U(N)
C
 2347 CONTINUE
C
      END IF
C	 
C**********************************************************************************
C
C    INPUT THE ARTIFICIAL SPEED OF SOUND IF ITIMST = 5.
C    THIS SHOULD BE ABOUT HALF THE MAXIMUM RELATIVE VELOCITY IN THE FLOW.
C
      IF(ITIMST.GE.5) THEN
      IF(ITIMST.EQ.5)  READ(5,*) VSOUND, RF_PTRU
      IF(ITIMST.EQ.6)  READ(5,*) VSOUND, RF_PTRU, DENSTY
      RF_PTRU   = 0.01
      RF_VSOUND = 0.002
      VS_VMAX   = 2.0
C
         IF(NCALL.EQ.2) THEN
               WRITE(2,*) ' ARTIFICIAL COMPESSIBILITY FACTORS'
               IF(ITIMST.EQ.5) WRITE(2,1100) VSOUND, RF_PTRU, RF_VSOUND,
     &          VS_VMAX
               IF(ITIMST.EQ.6) WRITE(2,1100) VSOUND, RF_PTRU, RF_VSOUND,
     &         VS_VMAX, DENSTY
         END IF
C
         WRITE(6,*) ' CALCULATION USING ARTIFICIAL COMPRESSIBILITY '
         WRITE(6,*) ' ARTIFICIAL SPEED OF SOUND = ', VSOUND
         WRITE(6,*) ' DENSITY RELAXATION FACTOR = ', RFROTRU
         IF(ITIMST.EQ.6) WRITE(6,*) 
     &   ' INCOMPRESSIBLE FLOW WITH DENSITY = ',DENSTY
         WRITE(6,*)
      END IF
C
C******************************************************************************
C     READ IN THE OPTION TO USE REPEATING FLOW CONDITIONS
      IF(IF_REPEAT.NE.0) THEN
           READ(5,*)    NINMOD, RFINBC

           IF(NCALL.EQ.2) THEN
                 WRITE(6,*) '  REPEATING STAGE FACTORS,NINMOD, RFINBC'
                 WRITE(6,1702) NINMOD, RFINBC
           END IF
C
           WRITE(6,*) ' REPEATING STAGE SPECIFIED, NINMOD, RFINBC = ',
     &                  NINMOD, RFINBC 
      END IF
C******************************************************************************
C
C     READ IN THE STAGE NUMBERS AND SORT OUT THE START AND END OF EACH STAGE.
C
C     NOW READ IN THE ACTUAL STAGE NUMBER FOR EACH BLADE ROW.
C
      READ(5,*,END=3456,ERR=3456)(NSTAGE(N),N=1,NROWS)
 3456 CONTINUE

C
C******************************************************************************
C   INPUT THE FORCING FACTOR IF DOING A Q3D CALCULATION
C
      IF(KM.EQ.2) THEN
      Q3DFORCE = 0.25
      READ(5,*,END=3457,Err=3457) Q3DFORCE
 3457 CONTINUE
      WRITE(6,*) ' Q3DFORCE = ', Q3DFORCE
      END IF       
C
C**********************************************************************************
C    INPUT THE RANGE OF YPLUS VALUES OVER WHICH THE TURBULENT VISCOSITY WILL BE REDUCED.
C
           YPLAM    = 5.0
           YPTURB   = 25.0
           READ(5,*, ERR= 3458,END = 3458) YPLAM,YPTURB
 3458 CONTINUE
           WRITE(6,*) ' TURBULENT VISCOSITY REDUCED OVER THE RANGE YPLUS
     & = ',YPLAM,' TO',YPTURB
C
C******************************************************************************
      WRITE(6,*)  ' Subroutine READIN completed, input data OK'
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C     WRITE OUT THE FIRST PART OF THE NEW_READIN FILE
C
      IF(NCALL.EQ.2) THEN
C
      WRITE(6,*)
      WRITE(6,*)
     & ' CONVERSION COMPLETED, DATA SET "new_readin.dat" WRITTEN OUT. '
      WRITE(6,*)
C
      STOP
C
      END IF 
C

C******************************************************************************
C******************************************************************************
C
      WRITE(2,1200)  TITLE
      WRITE(2,*) ' CP  and GA '
      WRITE(2,1100) CP,GA
      IF(CP.LT.0.0) THEN
           WRITE(2,1100) CP1, CP2, CP3, TREF, RGAS
      END IF
C
      WRITE(2,*)  ' ITIMST '
      WRITE(2,1000) I_TIMST
      IF(I_TIMST.EQ.-4) THEN
           WRITE(2,1701) F1, F2EFF, F3 , RSMTH, NRSMTH
      END IF
      IF(I_TIMST.EQ.5) THEN
           WRITE(2,1100) VSOUND, RFROTRU
      END IF
      IF(I_TIMST.EQ.6) THEN
           WRITE(2,1100) VSOUND, RFROTRU, DENSTY
      END IF
C
      WRITE(2,*)   ' CFL, DAMPIN, MACHLIM, F_PDOWN '
      WRITE(2,1100)  CFL, DAMPIN, MACHLIM, F_PDOWN
C
      WRITE(2,*)   ' IINST '
      WRITE(2,1000)  IINST
C
      WRITE(2,*)   ' NSTEPS_MAX,  CONLIM '
      WRITE(2,1702)  NSTEPS_MAX,  CONLIM
C
      WRITE(2,*)   ' SFXIN,SFTIN,FAC_4TH,NCHANGE '
      WRITE(2,1703)  SFXIN,SFTIN,FAC_4TH,NCHANGE
C
      WRITE(2,*)   ' NUMBER OF BLADE ROWS TO BE CALCULATED'
      WRITE(2,1000)  NROWS
C
      WRITE(2,*)   ' GRID POINT NUMBERS  IM, KM '
      WRITE(2,1000)  IM,KM
C
      WRITE(2,*)   ' RELATIVE PITCHWISE GRID SPACINGS '
      WRITE(2,1100)  (FP(I),I=1,IMM1)
C
      WRITE(2,*)   ' RELATIVE SPACING OF THE SPANWISE GRID POINTS '
      WRITE(2,1100)  (FR(K),K=1,KMM1)
C
      WRITE(2,*)   ' MULTIGRID BLOCK SIZES '
      WRITE(2,1000)  IR,JR,KR,IRBB,JRBB,KRBB
C
      WRITE(2,*)   ' MULTIGRID TIME STEP FACTORS '
      WRITE(2,1100)  FBLK1,FBLK2,FBLK3
C
      WRITE(2,*)   ' MIXING PLANE MARKER, IFMIX '
      WRITE(2,1000)  IFMIX
      IF(IFMIX.NE.0) THEN
           WRITE(2,*)   ' MIXING PLANE PARAMETERS '
           WRITE(2,1100)  RFMIX,FEXTRAP,FSMTHB,FANGLE
      END IF
C
      WRITE(2,*)   ' IFCOOL, IFBLEED, IF_ROUGH '
      WRITE(2,1000)  IFCOOL, IFBLEED, IF_ROUGH
C
      WRITE(2,*)   ' NUMBER OF BLADE GEOMETRY INPUT SURFACES ' 
      WRITE(2,1000)  NSECS_IN
C
      IN_PRESS = INVR
      IN_VTAN  = INMACH
      IN_VR    = INVR
      IN_FLOW  = INFLOW
      WRITE(2,*)   ' IN_PRESS,IN_VTAN,IN_VR,IN_FLOW,IF_REPEAT,RFIN '
      WRITE(2,1704)  IN_PRESS,IN_VTAN,IN_VR,IN_FLOW,IF_REPEAT,RFIN
C
      WRITE(2,*)   ' IPOUT, SFEXIT, NSFEXIT '
      WRITE(2,1705)  IPOUT, SFEXIT, NSFEXIT
C
      WRITE(2,*)   ' PLATE_LOSS, THROTTLE_EXIT '
      WRITE(2,1100)  PLATE_LOSS, THROTTLE_EXIT
      IF(THROTTLE_EXIT.GT.0.001) THEN
            WRITE(2,1100)   THROTTLE_PRES,THROTTLE_MAS,RFTHROTL
      END IF
C
      IF(INFLOW.NE.0) THEN
           WRITE(2,*)  ' FLOWIN, RFLOW '
           WRITE(2,1100) FLOWIN, RFLOW
      END IF
C
      IF(IF_REPEAT.NE.0) THEN
           WRITE(2,*)  ' NINMOD, RFINBC '
           WRITE(2,1702) NINMOD, RFINBC
      END IF
C
      WRITE(2,*)   ' CHOICE OFVISCOUS MODEL '
      WRITE(2,1000)  ILOS, NLOS, IBOUND
C
      IF(ILOS.NE.0) THEN
            WRITE(2,*)  ' REYNOLDS NUMBER, ETC ' 
            WRITE(2,1711) REYNO, RF_VIS, FTRANS, TURBVIS_LIM,
     &                    PRANDTL, YPLUSWALL
      END IF
C
      IF(ILOS.GE.200)THEN
           WRITE(2,*)  ' FACTORS FOR THE SPALART-ALMARAS MODEL'
           WRITE(2,1100) FAC_STMIX, FAC_ST0, FAC_ST1,
     &                   FAC_ST2  , FAC_ST3, FAC_SFVIS
      END IF
C
      WRITE(2,*)  ' YPLAM  and  YPTURB '
      WRITE(2,1100) YPLAM,YPTURB
C
      IF(KM.EQ.2) THEN
            WRITE(2,*)   ' Q3D FORCE '
            WRITE(2,1100)  Q3DFORCE
      END IF
C
      WRITE(2,*)  ' ISHIFT, NEXTRAP_LE, NEXTRAP_TE '
      WRITE(2,1000) ISHIFT, NEXTRAP_LE, NEXTRAP_TE
C
      WRITE(2,*)   ' STAGE NUMBERS FOR EACH ROW ' 
      WRITE(2,1000 )(NSTAGE(N),N=1,NROWS)
C
      WRITE(2,*)      ' 5 values of NOUT '
      WRITE(2,1001)     (NOUT(N),N=1,5)
 1001 FORMAT(5I10)
      WRITE(2,*)      ' CHOICE OF VARIABLES TO BE PRINTED OUT'
      WRITE(2,1610)     (IOUT(I),I=1,20)
      WRITE(2,*)      ' STREAM SURFACES FOR PRINTED OUTPUT '
      WRITE(2,1610)     (KOUT(K),K=1,KM)
C
C   END OF INPUT WHICH DOES NOT DEPEND ON THE BLADE ROW NUMBER.
C********************************************************************************
C********************************************************************************
C
      RETURN
      END












