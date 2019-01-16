C
C----------------------------------------------------------------FIX24---
C
      SUBROUTINE FIX24(IT,OUTPUT_DATES_24)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT      : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine gives back the OUTPUT_DATES(*,IT) in common block,
C                    transforming it from 00:00 to 24:00, when applicable.
C
C --- INPUT        : IT
C
C --- OUTPUT       :  OUTPUT_DATES_24
C
C --- CALLS        : DAY_NUMBER
C                    GET_DATE
C
C --- VERSION      : 2010-03-02
C
C----------------------------------------------------------------FIX24---
C
      IMPLICIT NONE
C
      INCLUDE '../inc/enform_aqr.prm'
      INCLUDE '../inc/enform_aqr.cmn'
C
      INTEGER OUTPUT_DATES_24(5)
      INTEGER DAY_NUMBER, IT, I, CURRENT_DAY
C
      DO I = 1, 5
         OUTPUT_DATES_24(I) = OUTPUT_DATES(I,IT)
      ENDDO

      IF (OUTPUT_DATES_24(4).EQ.0) THEN

          IF (OUTPUT_DATES_24(5).NE.0) THEN
              WRITE(6,*) 'Error. Minutes are different from 0.'
              STOP
          ENDIF

C --- Find Julian day of FIRST_OUTPUT
          CURRENT_DAY = DAY_NUMBER(
     &                  OUTPUT_DATES_24(1),
     &                  OUTPUT_DATES_24(2),
     &                  OUTPUT_DATES_24(3))

C --- Get the date of the day before if the hour is 00:00
          CURRENT_DAY = CURRENT_DAY - 1
          OUTPUT_DATES_24(4) = 24
          CALL GET_DATE (CURRENT_DAY,
     &    OUTPUT_DATES_24(1),OUTPUT_DATES_24(2), OUTPUT_DATES_24(3))
      ENDIF

      RETURN

      END

