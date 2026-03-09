       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-1004.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      CADASTRO DE PRODUTO                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-ESTOQUE.SL".
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-ESTOQUE.FD".
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           05 FS-VIP-ESTOQUE           PIC X(02).
              88 VALID-VIP-ESTOQUE     VALUE "00" THRU "09".
           05 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".

       01  CODIGO                      PIC ZZ EXTERNAL.
       01  NM-CODIGO                   PIC X(10) EXTERNAL.
       01  PROXIMO-MENU                PIC X(01).
       01  PRODUTOS.
           03 CODIGO-PRODUTO           PIC ZZZZZZ9.
           03 NOME-PRODUTO             PIC X(60).
           03 DESCRICAO-PRODUTO        PIC X(20).
       01  WINDOW-TESTE                PIC X(10).
       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
       01  OPC-CONFIRMA.
           03 OPC-CONF                 PIC 9(01).
       01  S-MENU.
           03 SUB                      PIC X(01).

      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  CAD-PRODUTOS.
           03 LINE 08 COL 20           VALUE "CADASTRO DE PRODUTOS".
           03 LINE 10 COL 10           VALUE "CODIGO:".
           03 LINE 11 COL 10           VALUE "NOME DO PRODUTO:".
           03 LINE 12 COL 10           VALUE "DESCRICAO:".
           03 LINE 14 COL 10           VALUE "FORNECEDOR:".
       01  NAVEGAR.
           03 LINE 16 COL 10           VALUE
           "FIRST<F1>/PREVIOUS<F2>/NEXT<F3>/LAST<F4>".
       01  OPC-CONFIRMAR.
           03 LINE 20 COL 15           VALUE "CONFIRMAR <S>/<N>:".
       01  SUB-MENU.
           03 LINE 19 COL 15           VALUE "RESET <S><N>:".
      ******************************************************************
      *                      PROCEDURE DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
           OPEN I-O VIP-CLIFOR.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-CLIFOR!"
                            X"0A" "FILES STATUS:" FS-VIP-CLIFOR
              GO FIM-PRODUTOS
           END-IF.
           OPEN I-O VIP-ESTOQUE.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-ESTOQUE!"
                            X"0A" "FILES STATUS:" FS-VIP-ESTOQUE
              GO FIM-PRODUTOS
           END-IF.
           DISPLAY WINDOW ERASE.
       PD-PRODUTOS.
           DISPLAY CAD-PRODUTOS.
           ACCEPT NOME-PRODUTO         AT 1126.
           IF ESC-KEY
              GO FIM-PRODUTOS
           END-IF.
           ACCEPT DESCRICAO-PRODUTO    AT 1220.

       ACEITA-CODIGO.
           INITIALIZE REG-ESTOQUE
           MOVE HIGH-VALUE TO ESTOQUE-CD-PROD
           START VIP-ESTOQUE KEY < ESTOQUE-CHAVE
           READ VIP-ESTOQUE PREVIOUS WITH LOCK
           IF NOT VALID-VIP-ESTOQUE
              MOVE 1                   TO ESTOQUE-CD-PROD
           ELSE
              ADD 1                    TO ESTOQUE-CD-PROD
           END-IF
           IF ESC-KEY
              GO FIM-PRODUTOS
           END-IF.
           DISPLAY NAVEGAR.
           DISPLAY CAD-PRODUTOS.
           ACCEPT PROXIMO-MENU         AT 1421 CONTROL KEY IN KEYSTATUS.
           IF ENTER-KEY
              GO ACEITA
           END-IF.
      *-----------------------------------------------------------------
           IF F1-KEY
              INITIALIZE REG-CLIFOR
              MOVE LOW-VALUE           TO CLIFOR-CD-CLIFOR
              START VIP-CLIFOR KEY > CLIFOR-CHAVE
              READ VIP-CLIFOR NEXT WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"A0" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR
              GO ACEITA-CODIGO
           END-IF.
      *-----------------------------------------------------------------
           IF F2-KEY
              READ VIP-CLIFOR PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                                X"0A""FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR
              GO ACEITA-CODIGO
           END-IF.
      *-----------------------------------------------------------------
           IF F3-KEY
              READ VIP-CLIFOR NEXT WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR
              GO ACEITA-CODIGO
           END-IF.
      *-----------------------------------------------------------------
           IF F4-KEY
              MOVE HIGH-VALUE          TO CLIFOR-CD-CLIFOR
              START VIP-CLIFOR KEY < CLIFOR-CHAVE
              READ VIP-CLIFOR PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM MOVER-FOR
              GO ACEITA-CODIGO
           END-IF.
      *-----------------------------------------------------------------
           INITIALIZE REG-CLIFOR.
           MOVE CODIGO                 TO CLIFOR-CD-CLIFOR.
           READ VIP-CLIFOR WITH NO LOCK.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              GO FIM-PRODUTOS
           END-IF.
           PERFORM MOVER-REGISTRO-PARA-TELA.
           PERFORM MOVER-FOR.
      *-----------------------------------------------------------------
       ACEITA.
           DISPLAY OPC-CONFIRMAR.
           DISPLAY SUB-MENU.
           ACCEPT SUB AT 1928.
           IF SUB EQUAL "S" OR
              SUB EQUAL "s"
              DISPLAY WINDOW ERASE
              GO PD-PRODUTOS
           ELSE
              IF SUB NOT EQUAL "N"  AND
                 SUB NOT EQUAL "n"
                 DISPLAY MESSAGE BOX "OPCAO INVALIDA"
                 GO ACEITA
              END-IF
           END-IF.
           ACCEPT OPC-CONFIRMA AT 2033.
           IF OPC-CONFIRMA EQUAL "S" OR
              OPC-CONFIRMA EQUAL "s"
              PERFORM MOVER-TELA-PARA-REGISTRO
              PERFORM MOVER-FOR
              WRITE REG-ESTOQUE
              IF NOT VALID-VIP-ESTOQUE
                 DISPLAY MESSAGE BOX "NAO FOI POSSIVEL GRAVAR!"
                               X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              ELSE
                 DISPLAY MESSAGE BOX "GRAVADO COM SUCESSO!"
                 GO FIM-PRODUTOS
              END-IF
           END-IF.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-FOR.
           DISPLAY CODIGO              AT 1420.
           DISPLAY NM-CODIGO           AT 1424.
           DISPLAY "/"                 AT 1423
           DISPLAY CODIGO-PRODUTO      AT 1017.
       MOVER-TELA-PARA-REGISTRO.
           MOVE NOME-PRODUTO           TO ESTOQUE-NM-PROD.
           MOVE DESCRICAO-PRODUTO      TO ESTOQUE-DESC-PROD.
           MOVE CODIGO                 TO ESTOQUE-CD-FOR.
       MOVER-REGISTRO-PARA-TELA.
           MOVE ESTOQUE-CD-PROD        TO CODIGO-PRODUTO
           MOVE CLIFOR-CD-CLIFOR       TO CODIGO.
           MOVE CLIFOR-NM-CLIFOR       TO NM-CODIGO.
      *=================================================================
       FIM-PRODUTOS.
           CLOSE VIP-ESTOQUE.
           CLOSE VIP-CLIFOR.
           EXIT PROGRAM.
           STOP RUN.