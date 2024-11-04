*&---------------------------------------------------------------------*
*& Report ZSET_DISP_MULT_REG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zset_disp_mult_reg.

DATA:
  gt_zbv_disponivel TYPE TABLE OF zbv_disponivel,
  gs_zbv_disponivel TYPE zbv_disponivel.

*DATA:
*  tempo_minutos    TYPE mcwmit-be_ae,
*  tempo_disponivel TYPE p.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(18) TEXT-003.
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_co_tec TYPE zbv_disponivel-consultor_tecnico AS LISTBOX VISIBLE LENGTH 20.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 21(24) btn1 USER-COMMAND btn VISIBLE LENGTH 24.
  SELECTION-SCREEN END OF LINE.
*  p_dispon TYPE zbv_disponivel-disponivel AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN OUTPUT.
  btn1 = TEXT-002.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BTN'.
      PERFORM relatar_disponibilidade.
    WHEN OTHERS.
  ENDCASE.



*&---------------------------------------------------------------------*
*& Form relatar_disponibilidade
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM relatar_disponibilidade .

  "Validação para verificar se o usuário possui permissão de execução
  AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '16'.
  CASE sy-subrc.
    WHEN 0.

      SELECT * FROM zbv_disponivel
          INTO TABLE gt_zbv_disponivel
            WHERE consultor_tecnico = p_co_tec
            AND   data_registro = sy-datum.

      IF sy-subrc = 0.
        "Verifica a quantidade de linhas(registros) que tem na tabela
        DESCRIBE TABLE gt_zbv_disponivel LINES DATA(linha).

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_disponivel ASSIGNING FIELD-SYMBOL(<fs_zbv_disponivel>) INDEX linha.

        "Verifica o status atual do registo
        IF <fs_zbv_disponivel>-disponivel = abap_true. "p_dispon.
          MESSAGE 'Não é possível repetir o mesmo status / Você já está Disponível' TYPE 'W'.
        ENDIF.

        "Se o status atual não foi disponivel, significa que está disponivel e precisa criar um novo registro

        gs_zbv_disponivel-data_registro = <fs_zbv_disponivel>-data_registro.
        gs_zbv_disponivel-n_registro = linha + 1. "tambem pode ser <fs_zbv_disponivel>-n_registro + 1
        gs_zbv_disponivel-consultor_tecnico = <fs_zbv_disponivel>-consultor_tecnico.
        gs_zbv_disponivel-disponivel = abap_true.
        gs_zbv_disponivel-data_ini_disp = <fs_zbv_disponivel>-data_ini_disp. "Talvez nao precise (redundante)
        gs_zbv_disponivel-hora_ini_disp = sy-uzeit.


        INSERT zbv_disponivel FROM gs_zbv_disponivel.
        FREE: <fs_zbv_disponivel>.

        " Se não encontrar nenhum registro precisa criar o primeiro
      ELSE.

        gs_zbv_disponivel-data_registro = sy-datum.
        gs_zbv_disponivel-n_registro = 1.
        gs_zbv_disponivel-consultor_tecnico = p_co_tec.
        gs_zbv_disponivel-disponivel = abap_true.
        gs_zbv_disponivel-data_ini_disp = sy-datum.
        gs_zbv_disponivel-hora_ini_disp = sy-uzeit.

        INSERT zbv_disponivel FROM gs_zbv_disponivel.

      ENDIF.

    WHEN OTHERS.
      MESSAGE 'Sem permissão a transação!' TYPE 'E'.
  ENDCASE.

ENDFORM.
