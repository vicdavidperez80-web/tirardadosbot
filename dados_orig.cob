000000* Resultados aleatorios a petición del usuario (tirar dados)
000010* 1d10, 2d12, 20d20, etc.
000020* Máximo 99 dados.
000030* El número de caras es libre, con un máximo de 99999.

000040 IDENTIFICATION DIVISION.
000050 PROGRAM-ID. TirarDados.

000060 DATA DIVISION.
000070 WORKING-STORAGE SECTION.
000080     77 i PIC 99 USAGE IS COMP.
000090     77 ws-des PIC 99 USAGE IS COMP.
000100     77 ws-result PIC 9(7) VALUE 0 USAGE IS COMP.
000110     01 ws-tirada.
000120         05 ws-numdados PIC 999 USAGE IS COMP.
000130         05 ws-d PIC A VALUE "d".
000140         05 ws-numcaras PIC 9(6) USAGE IS COMP.
000150     77 wn-semilla PIC 9(8) USAGE IS COMP.
000160     77 wn-dado PIC 9(5) USAGE IS COMP.
000170     77 ws-valordisp PIC ZZZZZZ9.
000180** Leer desde la línea de comandos:
000190     77 ws-arg PIC X(12) VALUE SPACES.

000200 PROCEDURE DIVISION.
000210   INICIALIZAR.
000220     ACCEPT wn-semilla FROM TIME.
000230     COMPUTE wn-dado = FUNCTION RANDOM(wn-semilla).

000240   LEER-ARGUMENTOS.
000250     ACCEPT ws-arg FROM ARGUMENT-VALUE.
000260     MOVE FUNCTION LOWER-CASE(ws-arg) TO ws-arg.
000270     UNSTRING ws-arg DELIMITED BY "d"
000280       INTO ws-numdados, ws-numcaras
000290     END-UNSTRING.
000300** Argumento incorrecto, o vacío:
000310     INSPECT ws-arg TALLYING ws-des FOR ALL "d".
000320     IF ws-numdados = 0 OR ws-numcaras = 0 OR ws-des > 1 THEN
000330       DISPLAY "¡Error! Modo de uso:"
000340       DISPLAY "./dados [numdados]d[caras]"
000350       DISPLAY "(./dados 2d6, ./dados 5d10, etc)"
000360       STOP RUN
000370     END-IF.
000380** Demasiados dados:
000390     IF ws-numdados > 99 THEN
000400       DISPLAY "Un máximo de 99 dados, gracias."
000410       STOP RUN
000420     END-IF.
000430** Demasiadas caras:
000440     IF ws-numcaras > 99999 THEN
000450       DISPLAY "Un máximo de 99999 caras, gracias."
000460       STOP RUN
000470     END-IF.

000480   TIRAR-DADOS.
000490     DISPLAY "[ " WITH NO ADVANCING.
000500     PERFORM VARYING i FROM 1 BY 1 UNTIL i > ws-numdados
000510       COMPUTE wn-dado = (FUNCTION RANDOM() * ws-numcaras) + 1
000520       ADD wn-dado TO ws-result
000530       MOVE wn-dado TO ws-valordisp
000540       IF i < ws-numdados THEN
000550         DISPLAY FUNCTION TRIM(ws-valordisp) WITH NO ADVANCING
000560         DISPLAY " + " WITH NO ADVANCING
000570       ELSE
000580         DISPLAY FUNCTION TRIM(ws-valordisp) WITH NO ADVANCING
000590       END-IF
000600     END-PERFORM.
000610     DISPLAY " ] = " WITH NO ADVANCING.
000620     MOVE ws-result TO ws-valordisp.
000630     DISPLAY FUNCTION TRIM(ws-valordisp).
000640     STOP RUN.

