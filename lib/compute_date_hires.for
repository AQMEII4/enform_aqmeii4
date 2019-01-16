C
C--------------------------------------------------COMPUTE_DATE_HIRES---
C
      SUBROUTINE COMPUTE_DATE_HIRES(DATE_IN,
     &      DELTA_DD,DELTA_HH,DELTA_MN,DELTA_SS,DELTA_CC,
     &      DATE_OUT)
C
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT      : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine generates a vector of NTMAX OUTPUT_DATES starting
C                    at FIRST_OUTPUT date, with time spacing of DELTA_MINUTES minutes
C
C --- INPUT        : DATE_IN
C                    DELTA_DD
C                    DELTA_HH
C                    DELTA_MN
C                    DELTA_SS
C                    DELTA_CC
C
C --- OUTPUT       : DATE_OUT
C
C --- CALLS        : DAY_NUMBER
C                    GET_DATE
C
C --- VERSION      : 20150423 - RBI
C
C--------------------------------------------------COMPUTE_DATE_HIRES---
C
      IMPLICIT NONE
C
      INTEGER DAY_NUMBER, JD
      INTEGER NEW_JD, NEW_HH, NEW_MN, NEW_SS, NEW_CC
      INTEGER NDD, NHH, NMN, NSS
      INTEGER DATE_IN(7) ,DATE_OUT(7)
      INTEGER DELTA_DD,DELTA_HH,DELTA_MN,DELTA_SS,DELTA_CC
C
C --- Find Julian day of DATE_IN
C
      JD = DAY_NUMBER(DATE_IN(1),DATE_IN(2),DATE_IN(3))

      NEW_HH = DATE_IN(4) + DELTA_HH
      NEW_MN = DATE_IN(5) + DELTA_MN
      NEW_SS = DATE_IN(6) + DELTA_SS
      NEW_CC = DATE_IN(7) + DELTA_CC

      NEW_JD = JD + DELTA_DD

      IF (NEW_CC .LT. 0) THEN
          NSS = - NEW_CC / 100
          NEW_CC = (1 + NSS) * 100 + NEW_CC
          NEW_SS = NEW_SS - (1 + NSS)
      ENDIF
      IF (NEW_CC .GE. 100) THEN
          NSS = NEW_CC / 100
          NEW_CC = NEW_CC - NSS * 100
          NEW_SS = NEW_SS + NSS
      ENDIF

      IF (NEW_SS .LT. 0) THEN
          NMN = - NEW_SS / 60
          NEW_SS = (1 + NMN) * 60 + NEW_SS
          NEW_MN = NEW_MN - (1 + NMN)
      ENDIF
      IF (NEW_SS .GE. 60) THEN
          NMN = NEW_SS / 60
          NEW_SS = NEW_SS - NMN * 60
          NEW_MN = NEW_MN + NMN
      ENDIF

      IF (NEW_MN .LT. 0) THEN
          NHH = - NEW_MN / 60
          NEW_MN = (1 + NHH) * 60 + NEW_MN
          NEW_HH = NEW_HH - (1 + NHH)
      ENDIF
      IF (NEW_MN .GE. 60) THEN
          NHH = NEW_MN / 60
          NEW_MN = NEW_MN - NHH * 60
          NEW_HH = NEW_HH + NHH
      ENDIF

      IF (NEW_HH .LT. 0) THEN
          NDD = - NEW_HH / 24
          NEW_HH = (1 + NDD) * 24 + NEW_HH
          NEW_JD = NEW_JD - (1 + NDD)
      ENDIF
      IF (NEW_HH .GE. 24) THEN
          NDD = NEW_HH / 24
          NEW_HH = NEW_HH - NDD * 24
          NEW_JD = NEW_JD + NDD
      ENDIF

      CALL GET_DATE(NEW_JD,DATE_OUT(1),DATE_OUT(2),DATE_OUT(3))
      DATE_OUT(4) = NEW_HH
      DATE_OUT(5) = NEW_MN
      DATE_OUT(6) = NEW_SS
      DATE_OUT(7) = NEW_CC

C
      END
C

