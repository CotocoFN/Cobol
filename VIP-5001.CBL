       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-5001.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      CONSULTA DE CADASTROS                     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           05 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".
       01  CODIGO                      PIC ZZ EXTERNAL.
       01  CLIENTE.
           03 NOME                     PIC X(60).
           03 CPF-CNPJ                 PIC Z99.999.999-99.
           03 ENDERECO                 PIC X(20).
           03 BAIRRO                   PIC X(20).
           03 CIDADE                   PIC X(20).
           03 ESTADO                   PIC X(20).
           03 CEP                      PIC Z99.999.999.
       01  OPC.
           03 COD-BUSCA                PIC 9(07).
           03 OPC-CANCELAR             PIC X(01).
       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  BUSCA-CAD.
           03 LINE 08 COL 20           VALUE "CONSULTA DE REGISTRO".
           03 LINE 08 COL 43           VALUE "CODIGO:".
           03 LINE 10 COL 20           VALUE "NOME:".
           03 LINE 11 COL 20           VALUE "CPF-CNPJ:".
           03 LINE 12 COL 20           VALUE "ENDERECO:".
           03 LINE 13 COL 20           VALUE "BAIRRO:".
           03 LINE 14 COL 20           VALUE "CIDADE:".
           03 LINE 15 COL 20           VALUE "ESTADO:".
           03 LINE 16 COL 20           VALUE "CEP:".
      ******************************************************************
      *                      PROCEDURE DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
       PD-HOME-BUSCA.
           OPEN I-O VIP-CLIFOR.
           DISPLAY WINDOW ERASE.
           DISPLAY BUSCA-CAD.
       PD-BUSCA-CAD.
           ACCEPT COD-BUSCA            AT 0850 CONTROL KEY IN KEYSTATUS.

           IF COD-BUSCA EQUAL 99
              PERFORM PD-BUSCA-NOME    THRU FIM-PD-BUSCA-NOME
           END-IF.
           IF ESC-KEY
              GO FIM-PD-BUSCA-CAD
           END-IF.
           IF F1-KEY
              INITIALIZE REG-CLIFOR
              START VIP-CLIFOR KEY > CLIFOR-CHAVE
              READ VIP-CLIFOR NEXT WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
           END-IF.
           IF F2-KEY
              READ VIP-CLIFOR PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
           END-IF.
           IF F3-KEY
              READ VIP-CLIFOR NEXT WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
           END-IF.
           IF F4-KEY
              MOVE HIGH-VALUE          TO CLIFOR-CD-CLIFOR
              START VIP-CLIFOR KEY < CLIFOR-CHAVE
              READ VIP-CLIFOR PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
           END-IF.

           INITIALIZE REG-CLIFOR.
           MOVE COD-BUSCA              TO CLIFOR-CD-CLIFOR.
           READ VIP-CLIFOR WITH NO LOCK.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              GO FIM-PD-BUSCA-CAD
           END-IF.
           PERFORM MOVER-REGISTRO-PARA-TELA.
           PERFORM DISPLAY-TELA.
           DISPLAY "PRESS - ESC TO EXIT" AT 2150.
           GO PD-BUSCA-CAD.

       PD-BUSCA-NOME.
           ACCEPT NOME                 AT 1040 CONTROL KEY IN KEYSTATUS.
              IF NOME NOT EQUAL SPACES
                 MOVE NOME             TO CLIFOR-NM-CLIFOR
              END-IF.
           IF NOME NOT EQUAL spaces
              INITIALIZE REG-CLIFOR
              MOVE NOME            TO CLIFOR-NM-CLIFOR
              START VIP-CLIFOR KEY NOT LESS CLIFOR-CHAVE-EXT-1
              READ VIP-CLIFOR NEXT WITH NO LOCK
                 IF NOT VALID-VIP-CLIFOR
                    DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                                  X"0A" "FILE STATUS: " FS-VIP-CLIFOR
                 END-IF
              DISPLAY WINDOW ERASE
              DISPLAY BUSCA-CAD
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              DISPLAY "PRESS - ESC TO EXIT" AT 1750
              ACCEPT OMITTED AT 1040
              GO PD-BUSCA-NOME
           END-IF.
           IF F1-KEY
              MOVE LOW-VALUES          TO CLIFOR-NM-CLIFOR
              START VIP-CLIFOR KEY > CLIFOR-CHAVE-EXT-1
              READ VIP-CLIFOR NEXT WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
           END-IF.
           IF F2-KEY
              DISPLAY MESSAGE BOX "F2"
              START VIP-CLIFOR KEY < CLIFOR-CHAVE-EXT-1
              READ VIP-CLIFOR PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
           END-IF.
           IF F3-KEY
              DISPLAY MESSAGE BOX "F3"
              START VIP-CLIFOR KEY > CLIFOR-CHAVE-EXT-1
              READ VIP-CLIFOR NEXT WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
               GO PD-BUSCA-CAD
            END-IF.
           IF F4-KEY
              DISPLAY MESSAGE BOX "F4"
              INITIALIZE REG-CLIFOR
              MOVE HIGH-VALUE          TO CLIFOR-NM-CLIFOR
              START VIP-CLIFOR KEY < CLIFOR-CHAVE-EXT-1
              READ VIP-CLIFOR PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-CLIFOR
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              END-IF
              PERFORM MOVER-REGISTRO-PARA-TELA
              PERFORM DISPLAY-TELA
              GO PD-BUSCA-CAD
            END-IF.
       FIM-PD-BUSCA-NOME.
           EXIT PROGRAM.
       FIM-PD-BUSCA-CAD.
           EXIT PROGRAM.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       DISPLAY-TELA.
           DISPLAY CODIGO              AT 0849.
           DISPLAY NOME                AT 1025.
           DISPLAY CPF-CNPJ            AT 1129.
           DISPLAY ENDERECO            AT 1229.
           DISPLAY BAIRRO              AT 1327.
           DISPLAY CIDADE              AT 1427.
           DISPLAY ESTADO              AT 1527.
           DISPLAY CEP                 AT 1624.
       MOVER-TELA-PARA-REGISTRO.
           MOVE NOME                   TO CLIFOR-NM-CLIFOR.
           MOVE CPF-CNPJ               TO CLIFOR-CPF-CNPJ.
           MOVE ENDERECO               TO CLIFOR-ENDERECO.
           MOVE BAIRRO                 TO CLIFOR-BAIRRO.
           MOVE ESTADO                 TO CLIFOR-ESTADO.
           MOVE CEP                    TO CLIFOR-CEP.
           MOVE CIDADE                 TO CLIFOR-CIDADE.
       MOVER-REGISTRO-PARA-TELA.
           MOVE CLIFOR-CD-CLIFOR       TO CODIGO.
           MOVE CLIFOR-NM-CLIFOR       TO NOME.
           MOVE CLIFOR-CPF-CNPJ        TO CPF-CNPJ.
           MOVE CLIFOR-ENDERECO        TO ENDERECO.
           MOVE CLIFOR-BAIRRO          TO BAIRRO.
           MOVE CLIFOR-CIDADE          TO CIDADE.
           MOVE CLIFOR-ESTADO          TO ESTADO.
           MOVE CLIFOR-CEP             TO CEP.