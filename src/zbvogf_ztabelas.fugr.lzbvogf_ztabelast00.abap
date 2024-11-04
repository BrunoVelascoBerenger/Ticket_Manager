*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZBV_ACAO........................................*
DATA:  BEGIN OF STATUS_ZBV_ACAO                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_ACAO                      .
CONTROLS: TCTRL_ZBV_ACAO
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZBV_AMBIENTE....................................*
DATA:  BEGIN OF STATUS_ZBV_AMBIENTE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_AMBIENTE                  .
CONTROLS: TCTRL_ZBV_AMBIENTE
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZBV_CLIENTES....................................*
DATA:  BEGIN OF STATUS_ZBV_CLIENTES                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_CLIENTES                  .
CONTROLS: TCTRL_ZBV_CLIENTES
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZBV_CONSUL_TEC..................................*
DATA:  BEGIN OF STATUS_ZBV_CONSUL_TEC                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_CONSUL_TEC                .
CONTROLS: TCTRL_ZBV_CONSUL_TEC
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZBV_MODULO......................................*
DATA:  BEGIN OF STATUS_ZBV_MODULO                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_MODULO                    .
CONTROLS: TCTRL_ZBV_MODULO
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZBV_SOLICITANTE.................................*
DATA:  BEGIN OF STATUS_ZBV_SOLICITANTE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_SOLICITANTE               .
CONTROLS: TCTRL_ZBV_SOLICITANTE
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZBV_STATUS......................................*
DATA:  BEGIN OF STATUS_ZBV_STATUS                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_STATUS                    .
CONTROLS: TCTRL_ZBV_STATUS
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZBV_TECNOLOGIA..................................*
DATA:  BEGIN OF STATUS_ZBV_TECNOLOGIA                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBV_TECNOLOGIA                .
CONTROLS: TCTRL_ZBV_TECNOLOGIA
            TYPE TABLEVIEW USING SCREEN '0007'.
*.........table declarations:.................................*
TABLES: *ZBV_ACAO                      .
TABLES: *ZBV_AMBIENTE                  .
TABLES: *ZBV_CLIENTES                  .
TABLES: *ZBV_CONSUL_TEC                .
TABLES: *ZBV_MODULO                    .
TABLES: *ZBV_SOLICITANTE               .
TABLES: *ZBV_STATUS                    .
TABLES: *ZBV_TECNOLOGIA                .
TABLES: ZBV_ACAO                       .
TABLES: ZBV_AMBIENTE                   .
TABLES: ZBV_CLIENTES                   .
TABLES: ZBV_CONSUL_TEC                 .
TABLES: ZBV_MODULO                     .
TABLES: ZBV_SOLICITANTE                .
TABLES: ZBV_STATUS                     .
TABLES: ZBV_TECNOLOGIA                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
