C
C-------------------------------------------------GENERATE_DATES_HIRES---
C
      SUBROUTINE GENERATE_DATES_HIRES(FIRST_OUTPUT,OUTPUT_DATES,
     &                          DELTA_DD,
     &                          DELTA_HH,
     &                          DELTA_MN,
     &                          DELTA_SS,
     &                          DELTA_CC,
     &                          NTMAX)
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
C                    DELTA_DD
C                    DELTA_HH
C                    DELTA_MN
C                    DELTA_SS
C                    DELTA_CC
C                    NTMAX
C
C --- OUTPUT       : OUTPUT_DATES
C
C --- CALLS        : COMPUTE_DATE_HIRES
C
C --- VERSION      : 20150501 - RBI Forked from GENERATE_DATES
C
C-------------------------------------------------GENERATE_DATES_HIRES---
C
      IMPLICIT NONE
C
      INTEGER NTMAX, NT, IT
      INTEGER FIRST_OUTPUT(7), DATE_IN(7) ,DATE_OUT(7)
      INTEGER OUTPUT_DATES(7,NTMAX)
      INTEGER DELTA_DD,DELTA_HH,DELTA_MN,DELTA_SS,DELTA_CC
C
C --- Set first output
C
      DO IT = 1,7
         OUTPUT_DATES(IT,1) = FIRST_OUTPUT(IT)
      ENDDO

C
C --- Set following output dates
C
      NT = 1
      DO WHILE (NT .LT. NTMAX)

         DO IT = 1,7
            DATE_IN(IT) = OUTPUT_DATES(IT,NT)
         ENDDO

         CALL COMPUTE_DATE_HIRES(
     &      DATE_IN,
     &      DELTA_DD,DELTA_HH,DELTA_MN,DELTA_SS,DELTA_CC,
     &      DATE_OUT)
                
         NT = NT + 1

         DO IT = 1,7
            OUTPUT_DATES(IT,NT) = DATE_OUT(IT)
         ENDDO
      END DO
C
      END
C
