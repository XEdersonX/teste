

   MEMBER('GeraChave.clw')                                 ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('GERAC003.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('GERAC002.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('GERAC004.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('GERAC005.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('GERAC006.INC'),ONCE        !Req'd for module callout resolution
                     END


W:gerachave PROCEDURE                                      ! Generated from procedure template - Window

loc:codigoext        STRING(3)                             !
loc:cnpj             STRING(20)                            !
loc:proxvalidade     LONG                                  !
loc:chave            STRING(16)                            !
loc:nro_fabricacaoecf STRING(30)                           !
totalizador_geral    STRING(20)                            !
Window               WINDOW('Caption'),AT(,,395,224),FONT('MS Sans Serif',8,,FONT:regular),GRAY
                       PROMPT('loc : codigoext:'),AT(17,14),USE(?loc:codigoext:Prompt)
                       ENTRY(@s3),AT(67,13,60,10),USE(loc:codigoext)
                       PROMPT('loc : cnpj:'),AT(19,34),USE(?loc:cnpj:Prompt)
                       ENTRY(@s20),AT(69,33,172,10),USE(loc:cnpj)
                       PROMPT('loc : proxvalidade:'),AT(18,54),USE(?loc:proxvalidade:Prompt)
                       ENTRY(@d6),AT(86,53,60,10),USE(loc:proxvalidade),RIGHT(1)
                       BUTTON('&Gera Chave Validade'),AT(15,73,89,14),USE(?OkButton),LEFT,DEFAULT
                       BUTTON('&Cancelar'),AT(310,73,56,14),USE(?CancelButton),LEFT,STD(STD:Close)
                       ENTRY(@s16),AT(21,102,195,31),USE(loc:chave),FONT('Courier New',18,,FONT:bold),READONLY
                       PROMPT('loc : nro fabricacaoecf:'),AT(15,152),USE(?loc:nro_fabricacaoecf:Prompt)
                       ENTRY(@s30),AT(96,151,205,10),USE(loc:nro_fabricacaoecf)
                       PROMPT('totalizador geral:'),AT(17,169),USE(?totalizador_geral:Prompt)
                       ENTRY(@n10.2),AT(96,168,60,10),USE(totalizador_geral)
                       BUTTON('Gera Cripto ECF'),AT(21,194,89,14),USE(?Button4)
                       BUTTON('Gera Cripto Totalizador'),AT(121,194,89,14),USE(?Button5)
                       BUTTON('&Gera Chave TEF'),AT(114,73,89,14),USE(?OkButton:2),LEFT,DEFAULT
                       BUTTON('&Gera Chave NFE'),AT(211,73,89,14),USE(?OkButton:3),LEFT,DEFAULT
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED       ! Method added to host embed code
Kill                   PROCEDURE(),BYTE,PROC,DERIVED       ! Method added to host embed code
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED       ! Method added to host embed code
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
  SELF.FirstField = ?loc:codigoext:Prompt
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
        loc:chave = gera_chave(loc:proxvalidade,loc:cnpj,loc:codigoext)
        display
    OF ?Button4
        loc:chave = Gera_CriptoECF(loc:nro_fabricacaoecf)
        DISPLAY
    OF ?Button5
        loc:chave = Gera_CriptoECF(totalizador_geral)
        DISPLAY
    OF ?OkButton:2
        loc:chave = gera_chave_tef(loc:cnpj,loc:codigoext)
        display
    OF ?OkButton:3
        loc:chave = gera_chave_NFE(loc:cnpj,loc:codigoext)
        display
    END
  ReturnValue = PARENT.TakeAccepted()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

