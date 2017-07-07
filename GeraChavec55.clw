   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('EQUATES.CLW'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE

   MAP
     MODULE('Windows API')
SystemParametersInfo PROCEDURE (LONG uAction, LONG uParam, *? lpvParam, LONG fuWinIni),LONG,RAW,PROC,PASCAL,DLL(TRUE),NAME('SystemParametersInfoA')
     END
     MODULE('GERACBC.CLW')
DctInit     PROCEDURE
DctKill     PROCEDURE
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('GERAC001.CLW')
Main                   PROCEDURE   !
     END
   END

SilentRunning        BYTE(0)                         !Set true when application is running in silent mode



  MAP
    MODULE ('3DESV02.LIB')

      Function_DES  (LONG,STRING,STRING,*STRING),LONG,RAW,PASCAL
      Function_3DES (LONG,STRING,STRING,*STRING),LONG,RAW,PASCAL

    END
  END
FuzzyMatcher         FuzzyClass
GlobalErrors         ErrorClass
INIMgr               INIClass
GlobalRequest        BYTE(0),THREAD
GlobalResponse       BYTE(0),THREAD
VCRRequest           LONG(0),THREAD
lCurrentFDSetting    LONG
lAdjFDSetting        LONG

  CODE
  GlobalErrors.Init
  DctInit
  FuzzyMatcher.Init
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)
  INIMgr.Init('GeraChavec55.INI')
  SystemParametersInfo (38, 0, lCurrentFDSetting, 0)
  IF lCurrentFDSetting = 1
    SystemParametersInfo (37, 0, lAdjFDSetting, 3)
  END
  Main
  INIMgr.Update
  IF lCurrentFDSetting = 1
    SystemParametersInfo (37, 1, lAdjFDSetting, 3)
  END
  INIMgr.Kill
  FuzzyMatcher.Kill
  DctKill
  GlobalErrors.Kill


