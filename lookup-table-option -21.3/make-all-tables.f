      double precision  xmin, xmax, ymin, ymax, xnoww, ynoww, znow
      double precision out_of_range
      double precision xnow(200), ynow(200), PNOW, TNOW, RONOW, UNOW,
     &                 HNOW,CPNOW,PHEXP,PPLUS,TPLUS,ROPLUS,UPLUS,QNOW,
     &                 PMINUS,TMINUS,ROMINUS,UMINUS,HPLUS,HMINUS,DIFU,
     &                 DIFRO,URANGE,XPON,RORANGE,CONST
C
      DOUBLE PRECISION  ROOUT(200,200),UOUT(200,200),HOUT(200,200),
     &                  POUT(200,200),TOUT(200,200),CPOUT(200,200),
     &                  PHOUT(200,200),DRYF(200,200),GA_PV(200,200),
     &                  ENTPY(200,200)
C
      character(LEN=32) fluid, xname, yname, zname, fname_out
      integer i, j, ni, nj

      OPEN(UNIT=2, FILE = 'all_tables.dat')

      fluid = "water"//CHAR(0)
      fname_out = "table1.dat"//CHAR(0)
      xname = "D"//CHAR(0)
      xmin = 0.02
      xmax = 100.00
      yname = "U"//CHAR(0)
      ymin = 2000.*1e3
      ymax = 3500.*1e3
      URANGE  = YMAX - YMIN
      zname = "P"//CHAR(0)
      out_of_range = -9e9
      
      ni = 150
      nj = 150
C
      WRITE(2,*) '   NI,   NJ '
      write(2,110) ni, nj
 110  format(2I6)
 111  format(8E15.4)
 112  FORMAT(8E15.8)
 113  FORMAT(' I = ', I5,' DENSITY = ', E15.8)
 114  FORMAT(8E15.5)
C
C     SPLIT THE DENSITY INTO INTERVALS WITH EQUAL RATIO.
C
      RORANGE = XMAX - XMIN
      XPON    = 1.0/(NI-1) * DLOG(XMAX/XMIN)
      CONST   = XMIN/EXP(XPON)
      DO I = 1,NI
         XNOW(I) = CONST*EXP(XPON*I)
      END DO
C
      WRITE(2,*)  '  X VALUES'
      WRITE(2,112) (XNOW(I),I=1,NI)
C
      do j=1,nj
         ynow(j) = ymin + (ymax-ymin)*float(j-1)/float(nj-1)
      END DO
C
      WRITE(2,*) ' Y  VALUES '
      WRITE(2,112) (YNOW(J),J=1,NJ)
C
C**********************************************************************
C        PRESSURE
         DO 20 I = 1, NI  
             xnoww = xnow(i)  
             RONOW = XNOWW
        DO 10 j = 1, nj
            ynoww = ynow(j)
            znow = 99999999
            ZNAME = "P"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 100000.0
            PNOW  = ZNOW
            UNOW  = YNOWW
            znow = 99999999
C        TEMPERATURE
            ZNAME = "T"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 1000.0
            TNOW = ZNOW
C        ENTROPY
            ZNAME = "S"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 10000.0
            ENTNOW = ZNOW
C        DRYNESS FRACTION
            znow = 1.0
            ZNAME = "Q"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 1.0
            QNOW = ZNOW
            IF(QNOW.GT.0.999999) QNOW = 1.0
C**********************************************************************
C        INCREASED  P  and T
             DIFU  = 0.0005*YNOW(J)
             DIFRO = DIFU*XNOW(I)*XNOW(I)/PNOW
            XNOWW = XNOW(I) + DIFRO
            YNOWW = YNOW(J) + DIFU
            ROPLUS = XNOWW
            UPLUS  = YNOWW
            znow = 1.0
            ZNAME = "P"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 100000.0
            PPLUS = ZNOW
C
            znow = 1.0
            ZNAME = "T"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 1000.0
            TPLUS = ZNOW
C**********************************************************************
C       DECREASED  P   and T
            DIFU   = -DIFU
            DIFRO  = -DIFRO
            XNOWW = XNOW(I) + DIFRO
            YNOWW = YNOW(J) + DIFU
            ROMINUS = XNOWW
            UMINUS  = YNOWW
            znow = 1.0
            ZNAME = "P"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 100000.0
            PMINUS = ZNOW
C
            znow = 1.0
            ZNAME = "T"//CHAR(0)
            call propssi(zname, xname, xnoww, yname, ynoww, fluid, znow)
            if ((1.0/znow).eq.0.0)  znow = 1000.0
            TMINUS = ZNOW
C**********************************************************************
           HNOW   = UNOW   + PNOW/RONOW
           HPLUS  = UPLUS  + PPLUS/ROPLUS
           HMINUS = UMINUS + PMINUS/ROMINUS
           CPNOW  = (HPLUS - HMINUS)/(TPLUS - TMINUS)
           PHEXP  =  DLOG(HPLUS/HMINUS)/DLOG(PPLUS/PMINUS)
           GAPV   = DLOG(PPLUS/PMINUS)/DLOG(ROPLUS/ROMINUS)
C           IF(J.EQ.1) WRITE(2,*)
C           IF(J.EQ.1) WRITE(2,*) '  NEW DENSITY ', RONOW
C           WRITE(2,115)  I,J,QNOW, RONOW, UNOW, PNOW, TNOW, HNOW,CPNOW,
C     &                       PHEXP 
C          WRITE(2,*) ' DIFU, DIFRO      ', DIFU/1000., DIFRO
C          WRITE(2,*) ' PPLUS,PNOW,PMINUS', 
C     &                 PPLUS*1.0E-5, PNOW*1.0E-5, PMINUS*1.0E-5
C          WRITE(2,*) ' HPLUS,HNOW,HMINUS',
C     &                 HPLUS/1000.,HNOW/1000.,HMINUS/1000.
C          WRITE(2,*) ' TPLUS,TNOW,TMINUS', TPLUS,TNOW,TMINUS
C  115  FORMAT('Q,RO,U,P,T H,CP,XP',2I3,8F14.4)
      
C
         ROOUT(I,J) = RONOW
         UOUT(I,J)  = UNOW
         HOUT(I,J)  = HNOW
         POUT(I,J)  = PNOW
         TOUT(I,J)  = TNOW
         CPOUT(I,J) = CPNOW
         PHOUT(I,J) = PHEXP
         DRYF(I,J)  = QNOW
         GA_PV(I,J) = GAPV
         ENTPY(I,J) = ENTNOW
C 
   10 CONTINUE
C
   20 CONTINUE
C
C**********************************************************************
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY, DENSITY'
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*) ' DENSITY, J=1,NJ '
         WRITE(2,112)  (ROOUT(I,J), J=1,NJ)
      END DO
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY, INTERNAL ENERGY'
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*) ' INTERNAL ENERGY, J=1,NJ ' 
         WRITE(2,112)  (UOUT(I,J), J=1,NJ)
      END DO
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY,  PRESSURE '
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*) ' PRESSURE, J=1,NJ '
         WRITE(2,112)  (POUT(I,J), J=1,NJ)
      END DO
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY, TEMPERATURE '
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*) ' TEMPERATURE, Deg K, J=1,NJ '
         WRITE(2,114)  (TOUT(I,J), J=1,NJ)
      END DO
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY, ENTROPY'
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*) ' ENTROPY J.KG.K  J=1,NJ '
         WRITE(2,114)  (ENTPY(I,J), J=1,NJ)
      END DO
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY,  GA_PV'
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*) ' GAMMA FOR PRESSURE-DENSITY, J=1,NJ'
         WRITE(2,112)  (GA_PV(I,J), J=1,NJ)
      END DO
C
      WRITE(2,*) 'BLANK'
      WRITE(2,*) ' NEW PROPERTY,  DRYNESS FRACTION '
      DO  I = 1,NI
         WRITE(2,113)  I, XNOW(I)
         WRITE(2,*)  ' DRYNESS FRACTION, J=1,NJ'         
         WRITE(2,112)  (DRYF(I,J), J=1,NJ)
      END DO
C
C
      STOP
      END

