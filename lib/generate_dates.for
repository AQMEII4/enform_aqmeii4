C
C-------------------------------------------------------GENERATE_DATES---
C
      SUBROUTINE GENERATE_DATES(FIRST_OUTPUT,OUTPUT_DATES,
     &                          DELTA_MINUTES,NTMAX)
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
C --- INPUT        : FIRST_OUTPUT
C                    DELTA_MINUTES
C                    NTMAX
C
C --- OUTPUT       : OUTPUT_DATES
C
C --- CALLS        : DAY_NUMBER
C                    GET_DATE
C
C --- VERSION: November 30, 2006
C              20141217 RBI removed some unused variables
C
C-------------------------------------------------------GENERATE_DATES---
C
      IMPLICIT NONE
C
      INTEGER DAY_NUMBER, NTMAX, NT
      INTEGER FIRST_OUTPUT(5)
      INTEGER OUTPUT_DATES(5,NTMAX)
      INTEGER CURRENT_MIN, CURRENT_DAY, NEXT_OUTPUT, DELTA_MINUTES
C
      NT = 0
C
C --- Find Julian day of FIRST_OUTPUT
C
      CURRENT_DAY = DAY_NUMBER(FIRST_OUTPUT(1),FIRST_OUTPUT(2),
     &                         FIRST_OUTPUT(3))
C
C --- Find the minute of the day of FIRST_OUTPUT
C
      CURRENT_MIN = FIRST_OUTPUT(4) * 60 + FIRST_OUTPUT(5)
C
C --- Set first output
C
      NEXT_OUTPUT = CURRENT_MIN
C
      DO WHILE (NT .LT. NTMAX)
C
         NT = NT + 1
C
         CURRENT_DAY = CURRENT_DAY + (NEXT_OUTPUT / 1440)
         NEXT_OUTPUT = NEXT_OUTPUT - (NEXT_OUTPUT / 1440) * 1440
C
         CALL GET_DATE (CURRENT_DAY,
     &        OUTPUT_DATES(1,NT),OUTPUT_DATES(2,NT), OUTPUT_DATES(3,NT))
         OUTPUT_DATES(4,NT) = NEXT_OUTPUT / 60
         OUTPUT_DATES(5,NT) = NEXT_OUTPUT - (NEXT_OUTPUT / 60) * 60
         NEXT_OUTPUT = NEXT_OUTPUT + DELTA_MINUTES
      END DO
C
      END
C

