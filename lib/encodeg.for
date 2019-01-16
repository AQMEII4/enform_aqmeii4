C
C---------------------------------------------------------------ENCODEG---
C
      SUBROUTINE ENCODEG(VALUES_IN,VAL_MISSING,PREC_IN,NN,STRING)
C
C     (C) Copyright European Commission JRC IES 2008-10
C
C --- PURPOSE: This routine encodes the standard ASCII model output
C
C --- AUTHOR:  R.Bianconi - http://www.enviroware.com
C
C --- INPUT:   VALUES, NN, PREC, VAL_MISSING
C
C --- OUTPUT:  STRING
C
C --- CALLS:   none
C
C --- VERSION: 20141217 removed some unused variables
C
C---------------------------------------------------------------ENCODEG---
C
      IMPLICIT NONE
C
      INTEGER NNM
      CHARACTER STRING*(*)
      CHARACTER TMP*5, TMP1*6, FMTSTRING*30
      CHARACTER  FMTSTRING1*30
      REAL VAL_MISSING, PREC_IN, EPS
      REAL DELTA
      INTEGER I, NN, IRANGE, NEND, NBEG
      LOGICAL LMIN

      PARAMETER (NNM = 10000)
      PARAMETER (EPS = 1.E-5)
      REAL VALUES_IN(*), VALUES(NNM) 

      REAL*8 VAL, PREC
      INTEGER ISMISS(NNM)
      INTEGER*8 IWORK(NNM), IMIN, IMAX

      LMIN = .FALSE. 

      IF (NN .GT. NNM) STOP     

      STRING = ' '

      PREC = PREC_IN

      DO I = 1, NN

         IWORK(I) = 0
         VALUES(I) = VALUES_IN(I)

         IF (ABS(VALUES(I)-VAL_MISSING).LT.EPS) THEN
             ISMISS(I) = 1
         ELSE
             ISMISS(I) = 0

             ! Round to PREC precision
             VAL = VALUES(I)
             IF (VAL .LT. 0) THEN 
                 IWORK(I) = - INT( - VALUES(I) / PREC + 0.5 )
             ELSE
                 IWORK(I) = INT( VALUES(I) / PREC + 0.5 )
             ENDIF
             IF (.NOT.LMIN) THEN
                 IMIN = IWORK(I)
                 IMAX = IWORK(I)
                 LMIN = .TRUE.
             ENDIF

             ! Store min and max 
             IF (IWORK(I) .LT. IMIN) THEN
                 IMIN = IWORK(I)
             ENDIF
             IF (IWORK(I) .GT. IMAX) THEN
                 IMAX = IWORK(I)
             ENDIF
         ENDIF
      ENDDO
      IF (.NOT.LMIN) THEN
           IMIN =  0
           IMAX =  0
           LMIN = .TRUE.
      ENDIF
      
      ! Translate to positive values
      DO I = 1, NN
         IF (ISMISS(I).EQ.0) THEN
             IWORK(I) = IWORK(I) - IMIN
         ENDIF
      ENDDO

      ! Find the  data range
      DELTA = IMAX - IMIN
      ! Find the number of digits that are needed
      IF (DELTA .NE. 0.) THEN
          IRANGE = INT(ALOG10(DELTA)) + 1
      ELSE
          IRANGE = 1
      ENDIF

      WRITE(STRING(1:13),'(I13)') IMIN

      WRITE(STRING(14:15),'(I2.2)') IRANGE
      WRITE(TMP(1:5),'(I5.5)') NN

      TMP1 = 'I__.__';
      WRITE(TMP1(2:3),'(I2.2)') IRANGE
      WRITE(TMP1(5:6),'(I2.2)') IRANGE
      FMTSTRING = '('//TMP//TMP1//')'
C           --- Encode data
      NBEG = 13 + 2 + 1
      NEND = NBEG -1 + NN * IRANGE
      FMTSTRING1 = '('//TMP1//')'
 
      WRITE(STRING(NBEG:NEND),FMT=FMTSTRING) (IWORK(I),I=1,NN)
      TMP1 = 'I1';
      FMTSTRING = '('//TMP//TMP1//')'
      WRITE(STRING(NEND+1:2*NEND),FMT=FMTSTRING) (ISMISS(I),I=1,NN)
      
      RETURN
      END

