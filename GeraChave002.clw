

   MEMBER('GeraChave.clw')                                 ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('GERACHAVE002.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! Gera uma Chave de liberação por mais 30 dias
!!! </summary>
Gera_Chave           PROCEDURE  (long p_data,string p_cnpj,string p_codigoext) ! Declare Procedure
LOC:STR                 STRING(50)
LOC:STRING              STRING(13)
LOC:KEY                 STRING(16)
LOC:LICENSA             STRING(8)
LOC:MKEY                STRING(8)
LOC:KEY1                STRING(8)
LOC:KEY2                STRING(8)
LOC:BL1                 STRING(8)
LOC:BL2                 STRING(8)
LOC:BL3                 STRING(8)

  CODE
  ! Inicia o calculo da chave DES
  ! As chaves de referencia são LOC:KEY e LOC:KEYMSB
  ! A chave final sai em GLO:strRes

  LOC:KEY    = 'g&A^h@34bHKldr98'
  LOC:MKEY   = 'h5&0oVG!'
  LOC:STR    =  P_DATA&CLIP(P_CNPJ)&CLIP(P_CODIGOEXT)

!!!! PRIMEIRA PARTE DA CHAVE ##############################################################################################################################
  LOOP PAR# = 2 TO 26 BY 2
    LOC:STRING[PAR#/2] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY1 = LOC:LICENSA
!!!! ######################################################################################################################################################

!!!! SEGUNDA PARTE DA CHAVE ###############################################################################################################################
  LOOP PAR# = 1 TO 25 BY 2
    LOC:STRING[INT(PAR#/2)+1] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY2 = LOC:LICENSA
!!!! #######################################################################################################################################################

  RETURN (LOC:KEY1&LOC:KEY2)

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
W:gerachave PROCEDURE 

loc:codigoext        STRING(3)                             !
loc:cnpj             STRING(20)                            !
loc:proxvalidade     LONG                                  !
loc:chave            STRING(16)                            !
loc:nro_fabricacaoecf STRING(30)                           !
totalizador_geral    STRING(20)                            !
loc:hora             LONG                                  !
loc:senhadia         STRING(20)                            !
Window               WINDOW('Gera Chave'),AT(,,249,247),FONT('MS Sans Serif',8,,FONT:regular),ICON('..\..\Figu' & |
  'ras\key manager.ico'),GRAY,IMM
                       ENTRY(@s3),AT(47,7,21,10),USE(loc:codigoext)
                       ENTRY(@s20),AT(47,22,88,10),USE(loc:cnpj)
                       GROUP('Prox. Validade'),AT(11,39,138,66),USE(?GROUP1),BOXED
                         ENTRY(@d6),AT(21,63,51,10),USE(loc:proxvalidade),CENTER(1)
                         ENTRY(@s10),AT(79,63,60,10),USE(loc:proxvalidade,,?loc:proxvalidade:2),CENTER(1)
                         PROMPT('Data'),AT(20,52),USE(?loc:proxvalidade:Prompt)
                         ENTRY(@T4),AT(21,88,51,10),USE(loc:hora),CENTER(1)
                         ENTRY(@s14),AT(79,88,60,10),USE(loc:hora,,?loc:hora:2),CENTER(1)
                         PROMPT('Hora'),AT(20,77),USE(?loc:proxvalidade:Prompt:2)
                       END
                       BUTTON('&Gera Chave Validade'),AT(154,39,89,14),USE(?OkButton),LEFT,TIP('Cod. Ext.+ CNP' & |
  'J + Prox. Validade (data)')
                       BUTTON('&Gera Chave TEF'),AT(154,57,89,14),USE(?OkButton:2),LEFT,TIP('Cod. Ext. + CNPJ')
                       BUTTON('&Gera Chave NFE'),AT(154,74,89,14),USE(?OkButton:3),LEFT,TIP('Cod. Ext. + CNPJ')
                       BUTTON('&Gera Cripto Validade'),AT(154,92,89,14),USE(?OkButton:4),LEFT,TIP('Prox. Valid' & |
  'ade (data)')
                       ENTRY(@s16),AT(11,115,232,31),USE(loc:chave),FONT('Courier New',18,,FONT:bold),CENTER,READONLY
                       GROUP('ECF'),AT(11,157,233,44),USE(?GROUP2),BOXED
                         ENTRY(@s30),AT(105,168,129,10),USE(loc:nro_fabricacaoecf)
                         PROMPT('Nro.Fabricação ECF:'),AT(20,169),USE(?loc:nro_fabricacaoecf:Prompt)
                         PROMPT('Totalizador Geral:'),AT(20,186),USE(?totalizador_geral:Prompt)
                         ENTRY(@n10.2),AT(105,185,60,10),USE(totalizador_geral)
                       END
                       BUTTON('Gera Cripto Totalizador'),AT(11,204,89,14),USE(?Button5),TIP('Totalizador Geral')
                       BUTTON('Gera Cripto ECF'),AT(154,204,89,14),USE(?Button4),TIP('Nro.Fabricação ECF')
                       BUTTON('&Fechar'),AT(195,228,49,14),USE(?CancelButton),LEFT,ICON('waclose.ico'),STD(STD:Close)
                       GROUP('Senha Do Dia'),AT(154,6,89,24),USE(?GROUP3),BOXED
                         ENTRY(@s20),AT(165,16,70,10),USE(loc:senhadia),FONT('Microsoft Sans Serif',10),CENTER,READONLY, |
  SKIP
                       END
                       PROMPT('Cod. Ext.:'),AT(11,7),USE(?loc:codigoext:Prompt)
                       PROMPT('CNPJ:'),AT(11,23),USE(?loc:cnpj:Prompt)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('W:gerachave')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?loc:codigoext
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(Window)                                        ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('W:gerachave',Window)                       ! Restore window settings from non-volatile store
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('W:gerachave',Window)                    ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?OkButton
        if loc:proxvalidade = '' or loc:cnpj = '' or loc:codigoext = '' THEN
          message('Cod. Ext., CNPJ e Prox. Validade (data) devem ser preenchidos!','Aviso!',ICON:Exclamation)
        else
          loc:chave = gera_chave(loc:proxvalidade,loc:cnpj,loc:codigoext)
        end
        display
    OF ?OkButton:2
        if loc:cnpj = '' or loc:codigoext = '' THEN
          message('Cod. Ext. e CNPJ devem ser preenchidos!','Aviso!',ICON:Exclamation)
        else
          loc:chave = gera_chave_tef(loc:cnpj,loc:codigoext)
        end    
        display
    OF ?OkButton:3
        if loc:cnpj = '' or loc:codigoext = '' THEN
          message('Cod. Ext. e CNPJ devem ser preenchidos!','Aviso!',ICON:Exclamation)
        else
          loc:chave = gera_chave_NFE(loc:cnpj,loc:codigoext)
        end
        display
    OF ?OkButton:4
        if loc:proxvalidade = '' THEN
          message('Prox. Validade (data) deve ser preenchida!','Aviso!',ICON:Exclamation)
        else
          loc:chave = gera_cripto_validade(format(loc:proxvalidade,@d06))
        end
        display
    OF ?Button5
        if totalizador_geral = '' THEN
          message('Totalizador Geral deve ser preenchido!','Aviso!',ICON:Exclamation)
        else
          loc:chave = Gera_CriptoECF(totalizador_geral)
        end
        DISPLAY
    OF ?Button4
        if loc:nro_fabricacaoecf = '' THEN
          message('Nro. Fabricação ECF deve ser preenchido!','Aviso!',ICON:Exclamation)
        else
          loc:chave = Gera_CriptoECF(loc:nro_fabricacaoecf)
        end
        DISPLAY
    END
  ReturnValue = PARENT.TakeAccepted()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE EVENT()
    OF EVENT:OpenWindow
      loc:senhadia = Format(MONTH(TODAY()),@P##P)&Format(MONTH(TODAY()),@P##P)+Format(YEAR(TODAY())-2000,@P##P)+Format(DAY(TODAY()),@P##P)&Format(DAY(TODAY()),@P##P)
      display
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Source
!!! Gera uma Chave TEF
!!! </summary>
Gera_Chave_NFE       PROCEDURE  (string p_cnpj,string p_codigoext) ! Declare Procedure
LOC:STR                 STRING(50)
LOC:STRING              STRING(13)
LOC:KEY                 STRING(16)
LOC:LICENSA             STRING(8)
LOC:MKEY                STRING(8)
LOC:KEY1                STRING(8)
LOC:KEY2                STRING(8)
LOC:BL1                 STRING(8)
LOC:BL2                 STRING(8)
LOC:BL3                 STRING(8)

  CODE
  LOC:KEY    = 'eNFe09%pBv)(G27@'
  LOC:MKEY   = '$EnfE6&A'
  LOC:STR    =  CLIP(p_cnpj)&CLIP(p_CODIGOEXT)

!!!! PRIMEIRA PARTE DA CHAVE ##############################################################################################################################
  LOOP PAR# = 2 TO 26 BY 2
    LOC:STRING[PAR#/2] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY1 = LOC:LICENSA
!!!! ######################################################################################################################################################

!!!! SEGUNDA PARTE DA CHAVE ###############################################################################################################################
  LOOP PAR# = 1 TO 25 BY 2
    LOC:STRING[INT(PAR#/2)+1] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY2 = LOC:LICENSA
!!!! #######################################################################################################################################################

  return (upper(loc:key1&loc:key2))
!!! <summary>
!!! Generated from procedure template - Source
!!! Gera uma Chave TEF
!!! </summary>
Gera_Chave_TEF       PROCEDURE  (string p_cnpj,string p_codigoext) ! Declare Procedure
LOC:STR                 STRING(50)
LOC:STRING              STRING(13)
LOC:KEY                 STRING(16)
LOC:LICENSA             STRING(8)
LOC:MKEY                STRING(8)
LOC:KEY1                STRING(8)
LOC:KEY2                STRING(8)
LOC:BL1                 STRING(8)
LOC:BL2                 STRING(8)
LOC:BL3                 STRING(8)

  CODE
  LOC:KEY    = 'TEFD&*g98dh^-23m'
  LOC:MKEY   = 'l)@g6u%3'
  LOC:STR    =  CLIP(p_cnpj)&CLIP(p_CODIGOEXT)

!!!! PRIMEIRA PARTE DA CHAVE ##############################################################################################################################
  LOOP PAR# = 2 TO 26 BY 2
    LOC:STRING[PAR#/2] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY1 = LOC:LICENSA
!!!! ######################################################################################################################################################

!!!! SEGUNDA PARTE DA CHAVE ###############################################################################################################################
  LOOP PAR# = 1 TO 25 BY 2
    LOC:STRING[INT(PAR#/2)+1] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY2 = LOC:LICENSA
!!!! #######################################################################################################################################################

  return (upper(loc:key1&loc:key2))
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
Gera_CriptoECF       PROCEDURE  (string p_nroecf)          ! Declare Procedure
LOC:STR                 STRING(50)
LOC:STRING              STRING(13)
LOC:KEY                 STRING(16)
LOC:LICENSA             STRING(8)
LOC:MKEY                STRING(8)
LOC:KEY1                STRING(8)
LOC:KEY2                STRING(8)
LOC:BL1                 STRING(8)
LOC:BL2                 STRING(8)
LOC:BL3                 STRING(8)

  CODE
  LOC:KEY    = 'ECFD[?g07dhp=06j'
  LOC:MKEY   = 'x(#v9f]1'
  LOC:STR    =  p_nroecf

!!!! PRIMEIRA PARTE DA CHAVE ##############################################################################################################################
  LOOP PAR# = 2 TO 26 BY 2
    LOC:STRING[PAR#/2] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY1 = LOC:LICENSA
!!!! ######################################################################################################################################################

!!!! SEGUNDA PARTE DA CHAVE ###############################################################################################################################
  LOOP PAR# = 1 TO 25 BY 2
    LOC:STRING[INT(PAR#/2)+1] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY2 = LOC:LICENSA
!!!! #######################################################################################################################################################

  return (upper(loc:key1&loc:key2))
!!! <summary>
!!! Generated from procedure template - Source
!!! Gera uma Chave TEF
!!! </summary>
Gera_Cripto_Validade PROCEDURE  (string p_validade)        ! Declare Procedure
LOC:STR                 STRING(50)
LOC:STRING              STRING(13)
LOC:KEY                 STRING(16)
LOC:LICENSA             STRING(8)
LOC:MKEY                STRING(8)
LOC:KEY1                STRING(8)
LOC:KEY2                STRING(8)
LOC:BL1                 STRING(8)
LOC:BL2                 STRING(8)
LOC:BL3                 STRING(8)

  CODE
  LOC:KEY    = 'aVCP09&ale$)b69+'
  LOC:MKEY   = 'aKvCp3%&'
  LOC:STR    = clip(p_validade)

!!!! PRIMEIRA PARTE DA CHAVE ##############################################################################################################################
  LOOP PAR# = 2 TO 26 BY 2
    LOC:STRING[PAR#/2] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY1 = LOC:LICENSA
!!!! ######################################################################################################################################################

!!!! SEGUNDA PARTE DA CHAVE ###############################################################################################################################
  LOOP PAR# = 1 TO 25 BY 2
    LOC:STRING[INT(PAR#/2)+1] = LOC:STR[PAR#]
  END
  SUM#       = LOC:STRING[1]+LOC:STRING[2]+LOC:STRING[3]+LOC:STRING[4]+LOC:STRING[5]


  LOC:bl1[1] = chr(SUM#)
  LOC:bl1[2] = LOC:STRING[1]
  LOC:bl1[3] = LOC:STRING[5]
  LOOP I# = 4 TO 8
    LOC:BL1[I#] = LOC:STRING[I#-3]
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(0)
  END

  RET# = Function_Des (1,LOC:bl1,LOC:MKEY,LOC:LICENSA)

  ID#     = 0
  LOOP VOLTAS# = (LEN(CLIP(LOC:STRING))- 5) / 7 TO 0 BY -1

    LOC:bl2[1] = chr(SUM#)
    LOOP I# = 2 TO 8
      LOC:BL2[I#] = LOC:STRING[I#+4+(ID#*7)]
    END

    LOOP I# = 1 TO 8
      LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl2[I#])))
    END

    RET# = Function_Des (1,LOC:LICENSA,LOC:MKEY,LOC:LICENSA)

    ID#     += 1
    
  END

  SOBRA# = (LEN(CLIP(LOC:STRING)) - 5) % 7
  LOC:bl3[1] = chr(SUM#)
  IF SOBRA# > 0 THEN
    LOOP I# = 2 TO (SOBRA#+1)
      LOC:bl3[I#] = LOC:STRING[I#-1+(LEN(CLIP(LOC:STRING))-SOBRA#)]
    END
    LOC:bl3[I#] = chr(080h)
    IF (I#) < 8 THEN
      LOOP X# = I#+1 TO 8
        LOC:bl3[X#] = chr(0)
      END
    END
  ELSE
    LOC:bl3[2] = chr(080h)
    LOC:bl3[3] = chr(00h)
    LOC:bl3[4] = chr(00h)
    LOC:bl3[5] = chr(00h)
    LOC:bl3[6] = chr(00h)
    LOC:bl3[7] = chr(00h)
    LOC:bl3[8] = chr(00h)
  END

  LOOP I# = 1 TO 8
    LOC:LICENSA[I#] = CHR(BXOR(Val(LOC:LICENSA[I#]),Val(LOC:bl3[I#])))
  END

  RET# = Function_3Des (1,LOC:LICENSA,LOC:KEY,LOC:LICENSA)

  LOOP POS# = 1 TO 8
    LOOP
      IF (VAL(LOC:LICENSA[POS#]) >= 48 AND VAL(LOC:LICENSA[POS#]) <= 57) OR (VAL(LOC:LICENSA[POS#]) >= 65 AND VAL(LOC:LICENSA[POS#]) <= 90) OR |
      (VAL(LOC:LICENSA[POS#]) >= 97 AND VAL(LOC:LICENSA[POS#]) <= 122) THEN
         BREAK
      ELSE
        LOC:LICENSA[POS#] = CHR(INT((VAL(LOC:LICENSA[POS#])/2)+48))
        IF VAL(LOC:LICENSA[POS#]) >= 95 AND VAL(LOC:LICENSA[POS#]) <= 99 THEN
          LOC:LICENSA[POS#] = CHR(VAL(LOC:LICENSA[POS#]) - 40)
        END
      END
    END
  END
  LOC:KEY2 = LOC:LICENSA
!!!! #######################################################################################################################################################

  return (upper(loc:key1&loc:key2))
