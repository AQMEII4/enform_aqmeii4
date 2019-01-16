C
C----------------------------------------------------------------FIX24LST---
C
      SUBROUTINE FIX24LST(IT,IDH,OUTPUT_DATES_24)
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
C                    It adds a time shift in case the dates in UTC ar requested in LST.
C                    Based on ../lib/fix24.for 20100302
C
C --- INPUT        : IT
C                  : IDH
C
C --- OUTPUT       :  OUTPUT_DATES_24
C
C --- CALLS        : DAY_NUMBER
C                    GET_DATE
C
C --- VERSION      : 20110309
C
C----------------------------------------------------------------FIX24LST---
C
      IMPLICIT NONE
C
      INCLUDE '../inc/enform_aqr.prm'
      INCLUDE '../inc/enform_aqr.cmn'
C
      INTEGER OUTPUT_DATES_24(5)
      INTEGER DAY_NUMBER, IT, I, CURRENT_DAY, IDH, ICO
      INTEGER IYRF, IMOF, IDYF, IHRF
C
      DO I = 1, 5
         OUTPUT_DATES_24(I) = OUTPUT_DATES(I,IT)
      ENDDO

      ICO = 1
      CALL ADD_DELTA_HOURS( OUTPUT_DATES_24(I), IDH, ICO,
     &                 IYRF, IMOF, IDYF, IHRF )

      OUTPUT_DATES_24(1) = IYRF
      OUTPUT_DATES_24(2) = IMOF
      OUTPUT_DATES_24(3) = IDYF
      OUTPUT_DATES_24(4) = IHRF

      IF (OUTPUT_DATES_24(4).EQ.0) THEN

          IF (OUTPUT_DATES_24(5).NE.0) THEN
              WRITE(6,*) 'Error. Minutes are different from 0.'
              STOP
          ENDIF

      ENDIF

      RETURN

      END

