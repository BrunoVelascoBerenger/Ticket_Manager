*&---------------------------------------------------------------------*
*& PoolMóds.        ZMENU_FABRICA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zmenu_fabrica.

*&---------------------------------------------------------------------*
*& Module STATUS_9002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS 'STATUS_9002'.
  SET TITLEBAR 'TITLE-9002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  DATA lv_okcode_9002 TYPE sy-ucomm.

  CASE lv_okcode_9002.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.

    WHEN 'TF_AC'.

      "Verificação de permissão de criação de chamado
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '01'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZFORM_FABRICA'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TF_CC'.

      "Verificação de permissão de visualizar chamado
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '03'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZCONSULTA_CHAMADO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TT_CC'.

      "Verificação de permissão de modificar chamados
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZCONSULTA_CHAMADO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TT_SD'.

      "Verificação de permissão de setar disponibilidade
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZSET_DISPO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TT_SI'.

      "Verificação de permissão de setar disponibilidade
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZSET_INDISP'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_AC'.

      "Verificação de permissão de criação de chamado
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '01'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZFORM_FABRICA'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_CC'.

      "Verificação de permissão de modificar chamados
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZCONSULTA_CHAMADO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_SD'.

      "Verificação de permissão de setar disponibilidade
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZSET_DISPO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_SI'.

      "Verificação de permissão de setar disponibilidade
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZSET_INDISP'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_RV'.

      "Verificação de permissão de visualizar relatorio de volume de chamados
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '03'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZRELAT_VOL_CHAMADOS'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_RTM'.

      "Verificação de permissão de visualizar relatorio de tempo medio
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '03'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZRELAT_TMP_MEDIO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'TG_RD'.

      "Verificação de permissão de setar disponibilidade
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION 'ZRELAT_DISPO'.
        WHEN OTHERS.
          MESSAGE 'Sem autorização para a transação!' TYPE 'W'.
      ENDCASE.

    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

  CLEAR: lv_okcode_9002.
ENDMODULE.
