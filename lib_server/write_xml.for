C
C----------------------------------------------------------WRITE_XML---
C
      SUBROUTINE WRITE_XML(LXML)
C
C --- PURPOSE: This routine writes the XML file associated to the dataset
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL   
C
C --- INPUT:   LXML XML file unit
C
C --- OUTPUT:  XML file
C
C --- CALLS:   none
C
C --- VERSION: July 19, 2005
C              April 22, 2008 - Updated for:
C                                   - models with 5 digits
C                                   - sequences with 4 digits
C                                   - output times with 4 digits
C              Jan 16, 2009 - Enformg version
C
C----------------------------------------------------------WRITE_XML---
C
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/enform_aq.cmn'
C
      CHARACTER*12 M_DATE
      CHARACTER*20 ST
      WRITE (M_DATE( 1: 4),'(I4.4)') METEO_DATE(1)
      WRITE (M_DATE( 5: 6),'(I2.2)') METEO_DATE(2)
      WRITE (M_DATE( 7: 8),'(I2.2)') METEO_DATE(3)
      WRITE (M_DATE( 9:10),'(I2.2)') METEO_DATE(4)
      WRITE (M_DATE(11:12),'(I2.2)') METEO_DATE(5)
C    
      WRITE(LXML,'(A)') '<opt>'
      ST = ENFORM_TYPE
      WRITE(LXML,'(A)') '   <EnformType>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</EnformType>'
      ST = SOURCE_RANDOM_KEY
      WRITE(LXML,'(A)') '   <RandomKey>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</RandomKey>'
      ST = UPL_STATUS
      WRITE(LXML,'(A)') '   <Status>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Status>'
      ST = ERROR_MESSAGE
      WRITE(LXML,'(A)') '   <ErrorMessage>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</ErrorMessage>'
c     ST = VARIABLE_NAME
      ST = SOURCE_VAR_NAME(ICODE)    ! PG 2009/10/15
      WRITE(LXML,'(A)') '   <Variable>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Variable>'
      ST = '' 
      WRITE (ST(1:5),'(I5.5)') MODEL_NUMBER
      WRITE(LXML,'(A)') '   <ModelNumber>'//
     &                   ST(1:5)
     &                  // '</ModelNumber>'
      WRITE(LXML,'(A)') '   <ModelName>'//
     &                   MODEL_NAME(1:LSTRIM(MODEL_NAME))
     &                  // '</ModelName>'
      ST = ''
      WRITE(ST,'(I4.4)') SOURCE_SEQUENCE_NUMBER
      WRITE(LXML,'(A)') '   <SequenceId>'//
     &                   ST(1:4)
     &                  // '</SequenceId>'
      ST = ''
      WRITE(ST,'(I3.3)') SOURCE_CASE_NUMBER
      WRITE(LXML,'(A)') '   <CaseId>'//
     &                   ST(1:3)
     &                  // '</CaseId>'
      ST = ''
      WRITE(ST,'(I2.2)') ICODE  ! PG 2009/10/15
      WRITE(LXML,'(A)') '   <ReleaseId>'//
     &                   ST(1:2)
     &                  // '</ReleaseId>'
      WRITE(LXML,'(A)') '   <MeteoDate>'//
     &                   M_DATE
     &                  // '</MeteoDate>'
      WRITE(LXML,'(A)') '   <MeteoDelta>'//
     &                   DELTA_M(1:LSTRIM(DELTA_M))
     &                  // '</MeteoDelta>'
      WRITE(LXML,'(A)') '   <UploadDate>'//
     &                   UPL_DATE
     &                  // '</UploadDate>'
      WRITE(LXML,'(A)') '   <UploadDelta>'//
     &                   DELTA_U(1:LSTRIM(DELTA_U))
     &                  // '</UploadDelta>'
      WRITE(LXML,'(A)') '   <UploadUser>'//
     &                   UPL_USER(1:LSTRIM(UPL_USER))
     &                  // '</UploadUser>'
      WRITE(LXML,'(A)') '   <UploadIp>'//
     &                   UPL_IP(1:LSTRIM(UPL_IP))
     &                  // '</UploadIp>'
      WRITE(LXML,'(A)') '   <Coordinates>'//
     &                   COORDINATES(1:LSTRIM(COORDINATES))
     &                  // '</Coordinates>'
      ST = ''
      WRITE (ST(1:10),'(F10.2)') X_MIN
      WRITE(LXML,'(A)') '   <Xmin>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Xmin>'
      ST = ''
      WRITE (ST(1:10),'(F10.2)') Y_MIN
      WRITE(LXML,'(A)') '   <Ymin>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Ymin>'

      X_MAX= X_MIN + DX * (NX - 1)
      ST = ''
      WRITE (ST(1:10),'(F10.2)') X_MAX
      WRITE(LXML,'(A)') '   <Xmax>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Xmax>'

      Y_MAX= Y_MIN + DY * (NY - 1)      
      ST = ''
      WRITE (ST(1:10),'(F10.2)') Y_MAX
      WRITE(LXML,'(A)') '   <Ymax>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Ymax>'
      ST = ''
      WRITE (ST(1:9),'(F9.2)') DX
      WRITE(LXML,'(A)') '   <Dx>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Dx>'
      ST = ''
      WRITE (ST(1:9),'(F9.2)') DY
      WRITE(LXML,'(A)') '   <Dy>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Dy>'
      ST = ''
      WRITE (ST(1:3),'(I3)') NX
      WRITE(LXML,'(A)') '   <Nx>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Nx>'
      ST = ''
      WRITE (ST(1:3),'(I3)') NY
      WRITE(LXML,'(A)') '   <Ny>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Ny>'
      ST = ''
      WRITE(LXML,'(A)') '   <ZLevel>'
     &                  // '</ZLevel>'

      ST = AVGOUT
      WRITE(LXML,'(A)') '   <Averaging>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Averaging>'

      ST = STTOUT
      WRITE(LXML,'(A)') '   <Statistics>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Statistics>'
      WRITE (ST(1:5),'(F5.2)') PCTOUT
      WRITE(LXML,'(A)') '   <Percentile>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</Percentile>'

      
      ST = ''
      WRITE (ST(1:4),'(I4.4)') NTWORK
      WRITE(LXML,'(A)') '   <Nt>'//
     &                   ST(1:4)
     &                  // '</Nt>'
      DO I = 1, NTWORK
         ST = ''
         WRITE (ST(1:10),'(E10.3)') VAL_MAX(I)
         WRITE(LXML,'(A)') '   <MaxValues>'//
     &                      ST(1:LSTRIM(ST))
     &                     // '</MaxValues>'
      ENDDO
      DO I = 1, NTWORK
         ST = ''
         WRITE (ST(1:16),1000) (OUTPUT_DATES_WORK(J,I),J=1,5)
         WRITE(LXML,'(A)') '   <OutputDates>'//
     &                      ST(1:LSTRIM(ST))
     &                     // '</OutputDates>'
      ENDDO
      WRITE(LXML,'(A)') '</opt>'
C
      RETURN
C
1000  FORMAT (I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2)
C
      END
