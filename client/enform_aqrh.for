C----------------------------------------------------ENFORM_AQRH-------
C
      PROGRAM ENFORM_AQRH
C
C --- PURPOSE      : This program encodes the standard ASCII model output
C                    for the Ensemble system.
C                    Model output can be optionally be processed before
C                    encoding in order to extract some statistics of interest.
C
C              Please refer to the accompanying file ENFORM_AQR.README
C              for details.
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT      : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- INPUT        : command line
C
C --- OUTPUT       : standard output
C
C --- CALLS        : GETARG
C                    GENERATE_DATES_HIRES
C                    ENCODEG
C                    PARSE_METAFILE_AQR_HIRES
C
C --- NOTES        : In case this program is compiled under Lahey Fortran for
C                    windows, uncomment the include of getarg.for at the bottom
C                    of this file.
C
C --- VERSION      : 
C                  : 20150101 - RBI - forked from ENFORM_AQR.FOR 
C                  : 20150930 - RBI - fix to DELTA_CC calculation from input
C
C----------------------------------------------------ENFORM_AQRH-------
C
      IMPLICIT NONE
C
      INCLUDE '../inc/enform_aqrh.prm'
      INCLUDE '../inc/version.prm'
      INCLUDE '../inc/enform_aqrh.cmn'
C
      INTEGER   READ_DATE(7)
      REAL      VAL(NTMAX,NZM), VALUES(NZM), RNZ
      REAL      XREC, YREC, ZREC(NZM)
      CHARACTER STRING*8128, OUTFILE*80
      CHARACTER SOURCE_FILE*128, USER_FILE*128
      !CHARACTER DUMMY*200    OUT 20110311
      CHARACTER DUMMY*1024    ! IN 20110311
      INTEGER   USER_METEO_DATE(5)
      INTEGER   USER_FIRST_OUTPUT(7)
      INTEGER   USER_MODEL_NUMBER
      CHARACTER USER_VAR*50, USER_RANDOM_KEY*7
      CHARACTER MDATE*16, FODATE*16
      INTEGER   I, K, LUN, LUS, LOUT, IT, LSTRIM
      INTEGER   IRINDEX0, IRINDEX, IR, NZ, LSCR

      INTEGER DELTA_DD, DELTA_HH, DELTA_MN, DELTA_SS

      REAL VAL_MISSING, PREC
      INTEGER UI
      LOGICAL ALL_MISSING

      REAL VMAX_INFO

      DATA LUN, LUS, LOUT, LSCR /1,2,3,89/

      CALL GETARG(1,SOURCE_FILE)
      CALL GETARG(2,USER_FILE)

!     source_file = '9999-001.src'
!     user_file = 'original-9999-001-tsd.3.txt'

C --- Parse source file

      OPEN (LUS, FILE=SOURCE_FILE(1:LSTRIM(SOURCE_FILE)), STATUS='OLD',
     &             ERR=905)
      CALL PARSE_METAFILE_AQR_HIRES(LUS)
      CLOSE(LUS)

C --- Check Enform version consistency, with backward compatibility
      IF (SOURCE_VERSION .EQ. VERSION) THEN
      ELSE
          GOTO 500
      ENDIF

C --- Open user file

      IF (LDEBUG) THEN
          WRITE(6,*)
          WRITE(6,*) 'Reading user file (model output)...'
          WRITE(6,*)
      ENDIF

      OPEN (LUN, FILE=USER_FILE, STATUS='OLD',ERR=910)

      READ(LUN,'(I5)') USER_MODEL_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'User model number ---> ',  USER_MODEL_NUMBER
      ENDIF

      MODEL_NUMBER = USER_MODEL_NUMBER

      READ(LUN,'(A7)') USER_RANDOM_KEY
      IF (LDEBUG) THEN
          WRITE(6,*) 'User random key ---> ',  USER_RANDOM_KEY
      ENDIF

C --- Check random_key

      IF (USER_RANDOM_KEY .NE. SOURCE_RANDOM_KEY) GOTO 510

      READ(LUN,'(A)') USER_VAR
C
      CALL FIXNAME(USER_VAR)
      IF (LDEBUG) THEN
          WRITE(6,*) 'User variable name ---> ',
     &                USER_VAR(1:LSTRIM(USER_VAR))
      ENDIF

C --- Find variable index

      ICODE = 0
      DO I = 1, NUC_MAX
         IF (USER_VAR .EQ. SOURCE_VAR_NAME(I)) THEN
             ICODE = I
             IF (LDEBUG) THEN
                 WRITE(6,*) 'User variable index ---> ',
     &                       USER_VAR(1:LSTRIM(USER_VAR))
                 WRITE(6,*) 'Variable index in metafile ---> ',  ICODE
             ENDIF
         ENDIF
      ENDDO
      IF (ICODE .EQ. 0) GOTO 610

      IF (SOURCE_VAR_NITEMS(ICODE) .EQ. 0) GOTO 620

C --- Read item index

      READ(LUN,*) UI
      IF (LDEBUG) THEN
          WRITE(6,*) 'User item index ---> ',  UI
      ENDIF

C --- Read missing value

      READ(LUN,*) VAL_MISSING
      IF (LDEBUG) THEN
          WRITE(6,*) 'User missing value ---> ',  VAL_MISSING
      ENDIF

C --- Read number of output times in user's file

C     READ(LUN,'(I4)') NT
      READ(LUN,*) NT
      IF (LDEBUG) THEN
          WRITE(6,*) 'Number of time outputs in model output ---> ',  NT
      ENDIF
      IF (NT .GT. NTMAX) GOTO 710
      IF (NT .GT. NOUTPUTS) GOTO 715
      IF (NT .LE. 0) GOTO 717

C --- Read meteo date in user's file

      READ(LUN,'(A16)') MDATE
      IF (LDEBUG) THEN
          WRITE(6,*) 'User meteo date ---> ',  MDATE
      ENDIF
      READ(MDATE,'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)

C --- Read date and time of first ouptut

      READ(LUN,'(A16)') FODATE
      IF (LDEBUG) THEN
          WRITE(6,*) 'Date of first output ---> ',  FODATE
      ENDIF
      READ(FODATE,'(I4.4,6I2.2)') (USER_FIRST_OUTPUT(K),K=1,7)

C--------------------------------------------------------------------------------------
C Include aqr_processing_block
      INCLUDE '../inc/aqr_hires_processing_block.for'
C--------------------------------------------------------------------------------------

      CLOSE (LUN)

      STOP

C --- Error messages

500   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Installed ENFORM_AQRH version  : ', VERSION
      WRITE (6,*) 'Required ENFORM_AQRH version (or higher)  : ',
     &             SOURCE_VERSION
      STOP
510   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'The user-provided random key does not match with the
     &one provided in the source file.' 
      WRITE (6,*) 'RANDOM KEY (source file):', SOURCE_RANDOM_KEY 
      WRITE (6,*) 'RANDOM KEY (user file)  :', USER_RANDOM_KEY 
      STOP
610   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'The user-provided var is not listed in the metafile.'
      WRITE (6,*) 'USER VAR :', USER_VAR(1:LSTRIM(USER_VAR))
      STOP
620   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'The user-provided item number for current var ',
     &            'is not listed in the source file.'
      WRITE (6,*) 'USER VAR  :', USER_VAR(1:LSTRIM(USER_VAR))
      WRITE (6,*) 'VAR CODE  :', ICODE
      WRITE (6,*) 'USER ITEM :', UI
      STOP
C
710   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Time horizon exceeds parameter NTMAX (',NTMAX,'):',
     &NT
      STOP

715   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Time horizon exceeds value in source term file ',
     &            ' NOUTPUTS (',NOUTPUTS,'):'
      STOP

717   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Time horizon must be in the future ',
     &            ' NOUTPUTS (',NOUTPUTS,'):'
      STOP

720   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Variable name or item index could be wrong.'
      WRITE (6,*) 'Check your file headers (variable and items codes)'
      STOP

760   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Read date and'
     &                          //' expected date do not match.'
      WRITE (6,*)  'READ:     ',(READ_DATE(K),K=1,7)
      WRITE (6,*)  'EXPECTED: ',(OUTPUT_DATES(K,IT),K=1,7)
      STOP

905   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'The source file '//
     & SOURCE_FILE(1:LSTRIM(SOURCE_FILE))//' does not exist.'
      STOP

910   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'The user file',USER_FILE,' does not exist.'
      STOP

920   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Data format is not correct.'
      STOP

940   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'File is uncomplete. Error reading data record'
      WRITE (6,*) 'Receptor :', IR
      WRITE (6,*) 'Time     :', IT
      STOP

950   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Wrong receptors order.'
      WRITE (6,*) 'Receptor position within user file:', IR
      WRITE (6,*) 'Expecting: ', SOURCE_RECEPTOR_INDEX(IR)
      WRITE (6,*) 'Found    : ', IRINDEX
      STOP

960   WRITE (6,*) 'ERROR (ENFORM_AQRH)'
      WRITE (6,*) 'Wrong receptors order.'
      WRITE (6,*) 'Found receptor', IRINDEX, ' while reading'
      WRITE (6,*) 'data of receptor', IRINDEX0
      STOP


      END

      INCLUDE '../lib/compute_date_hires.for'
      INCLUDE '../lib/day_number.for'
      INCLUDE '../lib/encodeg.for'
      INCLUDE '../lib/fixname.for'
      INCLUDE '../lib/generate_dates_hires.for'
      INCLUDE '../lib/get_date.for'
C     INCLUDE '../lib/getarg.for'
      INCLUDE '../lib/leap_year.for'
      INCLUDE '../lib/lstrim.for'
      INCLUDE '../lib/lstriml.for'
      INCLUDE '../lib/parse_metafile_aqr_hires.for'
      INCLUDE '../lib/parse_item.for'
      INCLUDE '../lib/parse_averaging.for'
      INCLUDE '../lib/readrec.for'
