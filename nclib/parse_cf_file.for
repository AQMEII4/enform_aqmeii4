C
C---------------------------------------------------PARSE_CF_FILE---
C
      SUBROUTINE PARSE_CF_FILE(LUN)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT:     : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine parses the .cf for AQ applications
C
C --- AUTHOR       : R.Bianconi - http://www.enviroware.com
C
C --- INPUT        : LUN - CF FILE UNIT
C
C --- OUTPUT       : Output is in common block CF (see file ens2nc.cmn)
C
C --- CALLS        : readrec.for
C
C --- VERSION      : 2013-09-12
C
C---------------------------------------------------PARSE_CF_FILE---
C
      IMPLICIT NONE
C
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/ens2nc_aq.cmn'
C
      INTEGER LUN, NL, IPER, IHEI, NDIMR, I, LSTRIM, IVAR
      INTEGER IVRB, ITM, IPST
      INTEGER NITEMS, NPOSTS, NUC
C
      CHARACTER*200 DUMMY_READ
      CHARACTER     TMP_DATE*12, TMP_UTM*3
      REAL          ARRAY1( MAXVAL )
      INTEGER       ARRAY2( MAXVAL )
      CHARACTER*200 ARRAY3( MAXVAL )
      LOGICAL       ARRAY4( MAXVAL )
      CHARACTER*200 ARRAY5
      CHARACTER*200 TMP_STANDARD_NAME,TMP_DESCRIPTION,TMP_LONG_NAME
      CHARACTER*200 TMP_UNITS
      REAL TMP_PREC, TMP_MISS, TMP_FILLVALUE

      IF (LDEBUG) THEN
          WRITE(6,*)
          WRITE(6,*) "Now parsing CF metafile..."
          WRITE(6,*)
      ENDIF
C
      NL = 1

C                                                                       ***** GENERAL BLOCK *****
      
C --- ENFORM version
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_VERSION = ARRAY5

C --- ENFORM type
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_ENFORM_TYPE = ARRAY5

C --- Random key
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_RANDOM_KEY = ARRAY5

C --- Sequence number
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_SEQUENCE_NUMBER = ARRAY2(1)

C --- Case number
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_CASE_NUMBER = ARRAY2(1)

C --- Creation date (UTC)
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      TMP_DATE = ARRAY5
      READ(TMP_DATE,'(I4.4,4I2.2)') CF_CREATION_DATE

C --- Title
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      CF_TITLE = ARRAY5

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
C                                                                       ***** SPACE DOMAIN BLOCK *****

C --- Coordinates
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_COORDINATES = ARRAY5

      IF (CF_COORDINATES(1:LSTRIM(CF_COORDINATES)) .EQ. 'UTM') THEN
          
C --- Time zone UTM
          READ(LUN,'(A)') DUMMY_READ
          IF (LDEBUG) THEN
              WRITE(6,*) "CF file contains: ",DUMMY_READ
          ENDIF
          IVAR = 2
          NDIMR = 1
          CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )
          CF_UTMZONE = ARRAY2(1)

C --- Hemisphere
          READ(LUN,'(A)') DUMMY_READ
          IF (LDEBUG) THEN
              WRITE(6,*) "CF file contains: ",DUMMY_READ
          ENDIF
          IVAR = 5
          NDIMR = 1
          CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )
          CF_HEMISPHERE = ARRAY5

      ENDIF

C --- Xmin
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 1
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_X_MIN = ARRAY1(1)
      
C --- Ymin
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 1
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_Y_MIN = ARRAY1(1)

C --- Nx
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_NX = ARRAY2(1)
      IF (CF_NX .GT. NXM) GOTO 950
C --- Ny
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_NY = ARRAY2(1)
      IF (CF_NY .GT. NYM) GOTO 960

C --- Dx
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 1
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_DX = ARRAY1(1)

C --- Dy
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 1
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_DY = ARRAY1(1)

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF

C                                                                    ***** TIME DOMAIN BLOCK *****

C --- First output (UTC) 
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      TMP_DATE = ARRAY5
      READ(TMP_DATE,'(I4.4,4I2.2)') CF_FIRST_OUTPUT

C --- Time horizon (UTC)
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      TMP_DATE = ARRAY5
      READ(TMP_DATE,'(I4.4,4I2.2)') CF_TIME_HORIZON

C --- Number of outputs
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_NOUTPUTS = ARRAY2(1)
      IF (CF_NOUTPUTS .GT. NTMAX) GOTO 970

C --- Output frequency, starting averaging time within output period, 
C     ending averaging time within output period. [in minutes]
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 1
      NDIMR = 3
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      CF_DELTA_MINUTES = ARRAY1(1)
      CF_DTOS = ARRAY1(2)
      CF_DTOE = ARRAY1(3)

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF

C                                     ***** CF BLOCK *****

C --- Conventions
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      CF_CONVENTIONS = ARRAY5

C --- Longitude units
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      CF_LONGITUDE_UNITS = ARRAY5

C --- Latitude units
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      CF_LATITUDE_UNITS = ARRAY5

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF

C --- Number of variables
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) "CF file contains: ",DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      NUC = ARRAY2(1)

      IF (NUC .GT. NUC_MAX) GOTO 910

C --- Loop on variables
      DO IVRB = 1, NUC

C --- Skip separator record
         READ(LUN,'(A)') DUMMY_READ
         IF (LDEBUG) THEN
             WRITE(6,*) "CF file contains: ",DUMMY_READ
         ENDIF

C --- Read var name
         READ(LUN,'(A)') DUMMY_READ
         IVAR = 5
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         CF_VAR_NAME(IVRB) = ARRAY5
         CALL FIXNAME(CF_VAR_NAME(IVRB))

C --- Read var code
         READ(LUN,'(A)') DUMMY_READ
         IF (LDEBUG) THEN
             WRITE(6,*) "CF file contains: ",DUMMY_READ
         ENDIF
         IVAR = 2
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         IF (ARRAY2(1) .NE. IVRB) GOTO 920

C --- Read var description
         READ(LUN,'(A)') DUMMY_READ
         IVAR = 5
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         CF_VAR_DESCRIPTION(IVRB) = ARRAY5

C --- Read number of items for current variable

         READ(LUN,'(A)') DUMMY_READ
         IF (LDEBUG) THEN
             WRITE(6,*) "CF file contains: ",DUMMY_READ
         ENDIF
         IVAR = 2
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         NITEMS = ARRAY2(1)
         IF (NITEMS .GT. NITEMS_MAX) GOTO 930

         CF_VAR_NITEMS(IVRB) = NITEMS

C --- Loop on items

         DO ITM = 1, NITEMS

C           --- Read item stuff

            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 2
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
            IF (ARRAY2(1) .NE. ITM) GOTO 925

            ! CF standard_name
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 5
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_STANDARD_NAME(IVRB,ITM) = ARRAY5

            ! CF canonical_units
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 5
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_CANONICAL_UNITS(IVRB,ITM) = ARRAY5

            ! CF long_name
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 5
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_LONG_NAME(IVRB,ITM) = ARRAY5

            ! Description
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 5
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_DESCRIPTION(IVRB,ITM) = ARRAY5

            ! Units
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 5
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_UNITS(IVRB,ITM) = ARRAY5

            ! CF missing_value
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 1
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_VAL_MISSING(IVRB,ITM) = ARRAY1(1)

            ! CF filling_value
            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 1
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CF_VAR_ITEMS_FILLVALUE(IVRB,ITM) = ARRAY1(1)

C           --- Read number of postprocessing for current item

            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) "CF file contains: ",DUMMY_READ
            ENDIF
            IVAR = 2
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                     ARRAY3, ARRAY4, ARRAY5 )
            NPOSTS = ARRAY2(1)
            IF (NPOSTS .GT. NPOSTS_MAX) GOTO 940

            CF_ITEM_NPOSTS(IVRB,ITM) = NPOSTS
 
C           --- Loop on postprocessings

            DO IPST = 1, NPOSTS

C           --- Read postprocessing info
            
               READ(LUN,'(A)') DUMMY_READ
               IF (LDEBUG) THEN
                   WRITE(6,*) DUMMY_READ
               ENDIF
               IVAR = 5
               NDIMR = 1
               CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1,ARRAY2,
     &                       ARRAY3, ARRAY4, ARRAY5 )
               CALL PARSE_CF_AVERAGING(ARRAY5, 
     &                              CF_TIME_UNITS(IVRB,ITM,IPST),
     &                              CF_AVERAGING(IVRB,ITM,IPST),
     &                              CF_STATISTICS(IVRB,ITM,IPST),
     &                              CF_PERCENTILE(IVRB,ITM,IPST))


            ENDDO

         ENDDO
            
      ENDDO

      IF (LDEBUG) THEN
          WRITE(6,*) "CF file parsed..."
      ENDIF
      RETURN

910   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Number of variables exceeds parameter'
      WRITE (6,*) 'Parameter value:', NUC_MAX
      WRITE (6,*) 'Input value    :', NUC
      STOP

920   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Variable code read is different than expected.'
      STOP

925   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Item code read is different than expected.'
      STOP

930   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Number of items exceeds parameter'
      WRITE (6,*) 'Parameter value:', NITEMS_MAX
      WRITE (6,*) 'Input value    :', NITEMS
      STOP

940   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Number of postprocessings exceeds parameter'
      WRITE (6,*) 'Parameter value:', NPOSTS_MAX
      WRITE (6,*) 'Input value    :', NPOSTS
      STOP

950   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Number of NX grid nodes exceeds parameter'
      WRITE (6,*) 'Parameter value:', NXM
      WRITE (6,*) 'Input value    :', CF_NX
      STOP

960   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Number of NY grid nodes exceeds parameter'
      WRITE (6,*) 'Parameter value:', NYM
      WRITE (6,*) 'Input value    :', CF_NY
      STOP

970   WRITE (6,*) 'ERROR (PARSE_CF_FILE)'
      WRITE (6,*) 'Number of NT output times exceeds parameter'
      WRITE (6,*) 'Parameter value:', NTMAX
      WRITE (6,*) 'Input value    :', CF_NOUTPUTS
      STOP

      END


