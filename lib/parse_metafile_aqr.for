C
C--------------------------------------------------PARSE_METAFILE_AQR---
C
      SUBROUTINE PARSE_METAFILE_AQR(LUN)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT:     : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine parses the metafile for AQ applications
C
C --- AUTHOR       : R.Bianconi - http://www.enviroware.com
C
C --- INPUT        : LUN - SOURCE TERM FILE UNIT
C
C --- OUTPUT       : Output is in common block SOURCE (see file enform_aq.cmn)
C
C --- CALLS        : readrec.for
C
C --- VERSION      : 20110217 RBI added parsing of postprocessing statistics
C                  : 20130101 RBI chenged versioning system
C                  : 20141217 RBI removed some unused variables
C                  : 20150928 RBI fix to read of UTM coordinates
C
C
C--------------------------------------------------PARSE_METAFILE_AQR---
C
      IMPLICIT NONE
C
      INCLUDE '../inc/enform_aqr.prm'
      INCLUDE '../inc/enform_aqr.cmn'
C
      INTEGER LUN, NL, NDIMR, LSTRIM, IV
      INTEGER IVRB, ITM, IPST, IDR, IR
      INTEGER NITEMS, NPOSTS, IUTM, LSCR
C
      CHARACTER*128 DUMMY_READ
      CHARACTER     TMP_DATE*12, HEM*1
      REAL          ARRAY1( MAXVAL )
      INTEGER       ARRAY2( MAXVAL )
      CHARACTER*200 ARRAY3( MAXVAL )
      LOGICAL       ARRAY4( MAXVAL )
      CHARACTER*200 ARRAY5
      CHARACTER*200 TMP_DESC,TMP_UNITS
      REAL TMP_PREC, TMP_MISS

      REAL RX, RY

      PARAMETER( LSCR = 89 )

      IF (LDEBUG) THEN
          WRITE(6,*)
          WRITE(6,*) "Now parsing metafile..."
          WRITE(6,*)
      ENDIF
C
      NL = 1

C     ***** GENERAL BLOCK *****
      
C --- ENFORM version
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
!     IVAR = 1
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
!     SOURCE_VERSION = ARRAY1(1)
      SOURCE_VERSION = ARRAY5

C --- ENFORM type
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      ENFORM_TYPE = ARRAY5

C --- Random key
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      SOURCE_RANDOM_KEY = ARRAY5

C --- Sequence number
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      SOURCE_SEQUENCE_NUMBER = ARRAY2(1)

C --- Case number
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      SOURCE_CASE_NUMBER = ARRAY2(1)

C --- Creation date (UTC)
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      TMP_DATE = ARRAY5
      READ(TMP_DATE,'(I4.4,4I2.2)') CREATION_DATE

C --- Title
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                ARRAY3, ARRAY4, ARRAY5 )
      TITLE = ARRAY5

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
C                                         ***** COORDINATES DOMAIN BLOCK *****

C --- Coordinates
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      COORDINATES = ARRAY5

      IF (COORDINATES(1:LSTRIM(COORDINATES)) .EQ. 'UTM') THEN

C --- Time zone UTM
          READ(LUN,'(A)') DUMMY_READ
          IF (LDEBUG) THEN
              WRITE(6,*) DUMMY_READ
          ENDIF
          IVAR = 2
          NDIMR = 1
          CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )
          UTMZONE = ARRAY2(1)

C --- Hemisphere
          READ(LUN,'(A)') DUMMY_READ
          IF (LDEBUG) THEN
              WRITE(6,*) DUMMY_READ
          ENDIF
          IVAR = 5
          NDIMR = 1
          CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )
          HEMISPHERE = ARRAY5

      ENDIF

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF

C                              ***** TIME DOMAIN BLOCK *****

C --- First output (UTC) 
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      TMP_DATE = ARRAY5
      READ(TMP_DATE,'(I4.4,4I2.2)') SOURCE_FIRST_OUTPUT

C --- Time horizon (UTC)
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 5
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      TMP_DATE = ARRAY5
      READ(TMP_DATE,'(I4.4,4I2.2)') TIME_HORIZON

C --- Number of outputs
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      NOUTPUTS = ARRAY2(1)

C --- Output frequency, starting averaging time within output period, 
C     ending averaging time within output period. [in minutes]
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 1
      NDIMR = 3
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      DELTA_MINUTES = ARRAY1(1)
      DTOS = ARRAY1(2)
      DTOE = ARRAY1(3)

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
      ENDIF

C                                   ***** OUTPUT BLOCK *****

C --- Number of variables
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
          WRITE(6,*) DUMMY_READ
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
             WRITE(6,*) DUMMY_READ
         ENDIF

C --- Read var name
         READ(LUN,'(A)') DUMMY_READ
         IVAR = 5
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         SOURCE_VAR_NAME(IVRB) = ARRAY5
            write(6,*) SOURCE_VAR_NAME(IVRB)
         CALL FIXNAME(SOURCE_VAR_NAME(IVRB))
C --- Read var code
         READ(LUN,'(A)') DUMMY_READ
         IF (LDEBUG) THEN
             WRITE(6,*) DUMMY_READ
         ENDIF
         IVAR = 2
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         IF (ARRAY2(1) .NE. IVRB) GOTO 920

C --- Read number of items for current variable

         READ(LUN,'(A)') DUMMY_READ
         IF (LDEBUG) THEN
             WRITE(6,*) DUMMY_READ
         ENDIF
         IVAR = 2
         NDIMR = 1
         CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                 ARRAY3, ARRAY4, ARRAY5 )
         NITEMS = ARRAY2(1)
         IF (NITEMS .GT. NITEMS_MAX) GOTO 930

         SOURCE_VAR_NITEMS(IVRB) = NITEMS

C --- Loop on items

         DO ITM = 1, NITEMS

C           --- Read item index, description, units, precision and missing value

            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) DUMMY_READ
            ENDIF
            IVAR = 5
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                  ARRAY3, ARRAY4, ARRAY5 )

            CALL PARSE_ITEM(ARRAY5,
     &                     IV,TMP_DESC,TMP_UNITS, TMP_PREC, TMP_MISS)

            VAR_ITEMS_DESCRIPTION(IVRB,IV)  = TMP_DESC
            VAR_ITEMS_UNITS(IVRB,IV)        = TMP_UNITS
            VAR_ITEMS_PRECISION(IVRB,IV)    = TMP_PREC
            VAR_ITEMS_VAL_MISSING(IVRB,IV)  = TMP_MISS

            SOURCE_VAR_NITEMS(IVRB) = NITEMS

C           --- Read number of postprocessing for current item

            READ(LUN,'(A)') DUMMY_READ
            IF (LDEBUG) THEN
                WRITE(6,*) DUMMY_READ
            ENDIF
            IVAR = 2
            NDIMR = 1
            CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &                     ARRAY3, ARRAY4, ARRAY5 )
            NPOSTS = ARRAY2(1)

            IF (NPOSTS .GT. NPOSTS_MAX) GOTO 940

            SOURCE_ITEM_NPOSTS(IVRB,ITM) = NPOSTS
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

               CALL PARSE_AVERAGING(ARRAY5,
     &                              AVERAGING(IVRB,ITM,IPST),
     &                              STATISTICS(IVRB,ITM,IPST),
     &                              PERCENTILE(IVRB,ITM,IPST))


            ENDDO

         ENDDO
            
      ENDDO

C --- Skip separator record
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
         WRITE(6,*) DUMMY_READ
      ENDIF

C --- Number of discrete receptors
      READ(LUN,'(A)') DUMMY_READ
      IF (LDEBUG) THEN
         WRITE(6,*) DUMMY_READ
      ENDIF
      IVAR = 2
      NDIMR = 1
      CALL READREC( DUMMY_READ, NL, IVAR, NDIMR, ARRAY1, ARRAY2,
     &              ARRAY3, ARRAY4, ARRAY5 )
      SOURCE_NDISCREC = ARRAY2(1)
      IF (SOURCE_NDISCREC .GT. NDISCREC_MAX) GOTO 950

C --- Coordinates of discrete receptors
      DO IDR = 1, SOURCE_NDISCREC
        IF (COORDINATES(1:LSTRIM(COORDINATES)) .EQ. 'UTM') THEN
          READ(LUN,'(A)') DUMMY_READ
          OPEN(LSCR,STATUS='SCRATCH')
          !WRITE(LSCR,*) DUMMY_READ(1:LSTRIM(DUMMY_READ)-1) out
          WRITE(LSCR,'(A)') DUMMY_READ(1:LSTRIM(DUMMY_READ)-1)
          REWIND(LSCR)
          !READ(LSCR,*) IR, RX, RY, IUTM out 20150928
          READ(LSCR,*) IR, RX, RY
          CLOSE(LSCR)
          HEM = DUMMY_READ(LSTRIM(DUMMY_READ):LSTRIM(DUMMY_READ))
          !IF( IUTM .LE. 0 .OR. IUTM .GT. 60 ) GOTO 980 out 20150928
          !IF( HEM .NE. 'N' .AND. HEM .NE. 'S' ) GOTO 990 out 20150928
          SOURCE_RECEPTOR_INDEX(IDR) = IR
          SOURCE_RECEPTOR_LONGITUDE(IDR) = RX
          SOURCE_RECEPTOR_LATITUDE(IDR) = RY
          ! SOURCE_RECEPTOR_UTMZONE(IDR) = IUTM out 20150928
          ! SOURCE_RECEPTOR_HEMISPHERE(IDR) = HEM out 20150930
        ELSE
          READ(LUN,*) IR, RX, RY
          IF( RX .LT. ALON_RANGE_MIN .OR.
     &        RX .GT. ALON_RANGE_MAX ) GOTO 960
          IF( RY .LT. ALAT_RANGE_MIN .OR.
     &        RY .GT. ALAT_RANGE_MAX ) GOTO 970
          SOURCE_RECEPTOR_INDEX(IDR) = IR
          SOURCE_RECEPTOR_LONGITUDE(IDR) = RX
          SOURCE_RECEPTOR_LATITUDE(IDR) = RY
        ENDIF
      ENDDO


      IF (LDEBUG) THEN
          WRITE(6,*) "Metafile parsed..."
      ENDIF
      RETURN

910   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Number of variables exceeds parameter'
      WRITE (6,*) 'Parameter value NUC_MAX:', NUC_MAX
      WRITE (6,*) 'Input value    :', NUC
      STOP

920   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Variable code read is different than expected.'
      STOP

930   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Number of items exceeds parameter'
      WRITE (6,*) 'Parameter value NITEMS_MAX:', NITEMS_MAX
      WRITE (6,*) 'Input value    :', NITEMS
      STOP

940   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Number of postprocessings exceeds parameter'
      WRITE (6,*) 'Parameter value NPOSTS_MAX:', NPOSTS_MAX
      WRITE (6,*) 'Input value    :', NPOSTS

950   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Number of discrete receptors exceeds parameter'
      WRITE (6,*) 'Parameter value NDISCREC_MAX:', NDISCREC_MAX
      WRITE (6,*) 'Input value    :', SOURCE_NDISCREC
      STOP

960   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Longitude must be within the interval (-180,+180)'
      WRITE (6,*) 'Receptor index:', IR
      WRITE (6,*) 'Longitude     :', RX
      STOP

970   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Latitude must be within the interval (0,+180)'
      WRITE (6,*) 'Receptor index:', IR
      WRITE (6,*) 'Latitude      :', RY
      STOP

980   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'UTM zone must be within the interval (0,60)'
      WRITE (6,*) 'Receptor index:', IR
      WRITE (6,*) 'UTM zone      :', IUTM
      STOP

990   WRITE (6,*) 'ERROR (PARSE_METAFILE_AQR)'
      WRITE (6,*) 'Hemisphere can be only N or S'
      WRITE (6,*) 'Receptor index:', IR
      WRITE (6,*) 'Hemisphere    : '//HEM
      STOP

      END


