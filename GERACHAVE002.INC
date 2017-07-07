  MODULE('GERACHAVE002.CLW')
Gera_Chave             FUNCTION(long,string,string),string,C !Gera uma Chave de liberação por mais 30 dias
W:gerachave            PROCEDURE
Gera_Chave_NFE         FUNCTION(string,string),string,C !Gera uma Chave TEF
Gera_Chave_TEF         FUNCTION(string,string),string,C !Gera uma Chave TEF
Gera_CriptoECF         FUNCTION(string),string,C
Gera_Cripto_Validade   FUNCTION(string),string,C      !Gera uma Chave TEF
  END
