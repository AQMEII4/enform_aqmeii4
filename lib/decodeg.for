C
C---------------------------------------------------------------DECODEG---
C
      SUBROUTINE DECODEG(IREC,STRING,VAL_MISSING,PREC,NN,VALUES)
C
C     (C) Copyright ENSEMBLE Consortium 2000
C     (C) Copyright European Commission JRC IES 2008
C
C --- PURPOSE: This routine decodes the encoded model output (ENCODEG)
C
C --- AUTHOR:  R.Bianconi - http://www.enviroware.com   
C
C --- INPUT:   STRING, VAL_MISSING, PREC, NN
C
C --- OUTPUT:  VALUES
C
C --- CALLS:   none
C
C --- VERSION: 2009-10-07
C              2014-02-27 Added record tracking for error messages
C
C---------------------------------------------------------------DECODEG---
C
      IMPLICIT NONE 

      CHARACTER STRING*(*)
      CHARACTER TMP*5, TMP1*6, FMTSTRING*30
      REAL VAL_MISSING, PREC, VALUES(*)
      REAL FMIN
      INTEGER NN, IRANGE, ISMISS, KABEG, KAEND, KBBEG, KBEND, I
      INTEGER*8 IWORK, IMIN, IREC
      INTEGER NNM
C
      PARAMETER (NNM = 10000)
      READ(STRING(1:13),'(I13)') IMIN
      READ(STRING(14:15),'(I2.2)') IRANGE
      WRITE(TMP(1:5),'(I5.5)') NN

      TMP1 = 'I__.__';
      WRITE(TMP1(2:3),'(I2.2)') IRANGE
      WRITE(TMP1(5:6),'(I2.2)') IRANGE
      FMTSTRING = '('//TMP//TMP1//')'

      DO I = 1, NN
         KABEG = 16 + (I-1) * IRANGE 
         KAEND = KABEG + IRANGE - 1
         READ(STRING(KABEG:KAEND),FMTSTRING,ERR= 900) IWORK
         KBBEG = 15 + NN * IRANGE + I
         KBEND = KBBEG
         READ(STRING(KBBEG:KBEND),'(I1)') ISMISS
         IF (ISMISS.EQ.0) THEN
             VALUES(I) = (IWORK + IMIN)*PREC  
         ELSE
             VALUES(I) = VAL_MISSING
         ENDIF
      ENDDO
C
      RETURN

900   WRITE(6,*) 'ERROR decodeg.for'
      WRITE(6,*) 'RECORD: '
      WRITE(6,*) IREC
      WRITE(6,*) 'STRING: '
      WRITE(6,*) STRING
      WRITE(6,*) 'IMIN: '
      WRITE(6,*) IMIN
      WRITE(6,*) 'IRANGE: '
      WRITE(6,*) IRANGE
      WRITE(6,*) 'NN: '
      WRITE(6,*) NN
      WRITE(6,*) 'FMTSTRING: '
      WRITE(6,*) FMTSTRING
      STOP
      END
