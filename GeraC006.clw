

   MEMBER('GeraChave.clw')                                 ! This is a MEMBER module

                     MAP
                       INCLUDE('GERAC006.INC'),ONCE        !Local module procedure declarations
                     END


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
