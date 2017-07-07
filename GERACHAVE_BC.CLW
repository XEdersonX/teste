  MEMBER('GeraChave.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('GeraChave_BC0.CLW')
GeraChave_BC0:DctInit             PROCEDURE
GeraChave_BC0:DctKill             PROCEDURE
GeraChave_BC0:FilesInit           PROCEDURE
    END
  END

DctInit PROCEDURE
  CODE
  GeraChave_BC0:DctInit
  GeraChave_BC0:FilesInit


DctKill PROCEDURE
  CODE
  GeraChave_BC0:DctKill

