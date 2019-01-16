C
C--------------------------------------------------PARSE_STATIONS_FILE---
C
      SUBROUTINE PARSE_STATIONS_FILE(LUN,LUTM,IT2LST)
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT:     : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- PURPOSE      : This routine parses the stations file to get the UTC shift
C
C --- AUTHOR       : R.Bianconi - http://www.enviroware.com
C
C --- INPUT        : LUN - STATIONS FILE UNIT
C
C --- OUTPUT       : Output is in common block SOURCE (see file enform_aqr.cmn)
C
C --- CALLS        : readrec.for
C
C --- VERSION      : 20110309
C                  : 20151022 RBI Added stop in case of error , DUTC now real
C                                 Assuming FLOOR(DUTC)
C                  : 20151103 RBI Deal with UTM coordinates
C                                 Removed common block
C
C
C--------------------------------------------------PARSE_STATIONS_FILE---
C
      IMPLICIT NONE
C
      INTEGER LUN, NF, NF_LL, NF_UTM, NDUTC, NDUTC_LL, NDUTC_UTM
      INTEGER IUNIT, I, IR, K, LSTRIML, LSTRIM
      INTEGER IT2LST(*)
      REAL DUTC
      LOGICAL LUTM
      PARAMETER (IUNIT = 44)

      PARAMETER (NDUTC_LL = 12)
      PARAMETER (NDUTC_UTM = 17)

      PARAMETER (NF_LL = 12)
      PARAMETER (NF_UTM = 17)
      INTEGER COMMA(NF_UTM)

      CHARACTER SEP*1, STS*10
      PARAMETER (SEP = ',')

      LOGICAL LOOP
      PARAMETER (LOOP = .TRUE.)

      CHARACTER*1024 STRING

      ! read header for LL or UTM
      ! LL: ID,LCODE,LNAME,NETWORK,LON,LAT,ELEVATION,CCODE,SCODE,SITE_LAND_USE_1,SITE_LAND_USE_2,DUTC,COMMENT
      ! UTM: ID,LCODE,LNAME,NETWORK,LAT,LON,XUTM,YUTM,GEOSYS,ZONE,HEMISPHERE,ELEVATION,CCODE,SCODE,SITE_LAND_USE_1,SITE_LAND_USE_2,DUTC,COMMENT

      IF (LUTM) THEN
          NF = NF_UTM
          NDUTC = NDUTC_UTM
      ELSE
          NF = NF_LL
          NDUTC = NDUTC_LL
      ENDIF  

      READ(LUN,'(A)') STRING
    
      IR = 0

      DO WHILE (LOOP)
         READ(LUN,'(A)',END=100) STRING
         IR = IR + 1
         DO K = 1, NF
            COMMA(K) = 0
         ENDDO
         I = 0
         STRING = STRING(LSTRIML(STRING):LSTRIM(STRING))
         DO K = 1, LEN(STRING)
            IF( STRING(K:K).EQ.SEP ) THEN
                I = I + 1
                COMMA(I) = K
            ENDIF
         ENDDO

         STS = STRING(COMMA(NDUTC-1)+1:COMMA(NDUTC)-1)
         OPEN(IUNIT,STATUS='SCRATCH')
         WRITE(IUNIT,'(A)') STS
         REWIND(IUNIT)
         READ(IUNIT,*,ERR= 900,END=900) DUTC
         CLOSE(IUNIT)

         IT2LST(IR) = FLOOR(DUTC)

      ENDDO

100   RETURN

900   WRITE(6,*) ' ERROR (PARSE_STATIONS_FILE) - '//
     &           'Error parsing string '//
     &           STS
      WRITE(6,*) ' LUTM  =', LUTM,' NF = ',NF,' NDUTC = ',NDUTC
      STOP

      END
