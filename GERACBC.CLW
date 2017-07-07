  MEMBER('GeraChave.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('GERACBC0.CLW')
GERACBC0:DctInit             PROCEDURE
GERACBC0:DctKill             PROCEDURE
GERACBC0:FilesInit           PROCEDURE
    END
  END

DctInit PROCEDURE
  CODE
  GERACBC0:DctInit
  GERACBC0:FilesInit


DctKill PROCEDURE
  CODE
  GERACBC0:DctKill

