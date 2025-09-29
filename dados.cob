000000* Resultados aleatorios a petición del usuario (tirar dados)
000010* 1d10, 2d12, 20d20, etc.
000020* Máximo 99 dados.
000030* El número de caras es libre, con un máximo de 99999.
000040*
000050* Esta nueva variante permite tiradas complejas como 2d6+3d4,
000060* 5d10+2, 2d4+1d6+1d10, etc. Aunque normalmente no se usen.
000070* También números negativos: 1d6-2, -3d6, etc.
000080*
000090* 20-9-2025: ¡Y también dados explosivos! Se indican con una
000100* exclamación en la expresión de la tirada. Ejemplo: "2d10!".
000110*
000120* 22-9-2025: La locura continúa: dados "fudge" a petición del
000130* mendolo de Artu. Este tipo de dados tiene 6 caras y tres posi-
000140* bles valores. De modo que Una expresión como "4df+5" debería
000150* dar un resultado como: [+, +, □, -] + 5 = 6.
000160*
000170* Escrito para un bot de Telegram, @tirardadosbot.
000180*
000190* Opciones:
000200*   --telegram: Salida adaptada para Telegram.
000210*   --nosumar: Enumerar las tiradas pero no sumarlas.

000220 IDENTIFICATION DIVISION.
000230 PROGRAM-ID.       TirarDados.
000240 AUTHOR.           Pérez.
000250 DATE-WRITTEN.     2022-2025.

000260 ENVIRONMENT DIVISION.
000270 CONFIGURATION SECTION.
000280 SPECIAL-NAMES.
000290     CLASS T_SIGNO IS "-", "+", ",".

000300 DATA DIVISION.
000310 WORKING-STORAGE SECTION.
000320     77 i                    PIC 99 VALUE 1 USAGE IS COMP.
000330     77 j                    PIC 99 VALUE 1 USAGE IS COMP.
000340     77 ds                   PIC 99 VALUE 0 USAGE IS COMP.
000350     01 temporales.
000360         05 txtemp           PIC X(4).
000370         05 txtemp2          PIC X(7).
000380         05 tx2long          PIC 9 USAGE IS COMP.
000390** ANSI:
000400     77 intro                PIC X VALUE x"0a".
000410     01 ws-colores4bytes.
000420         05 ws-colorbold     PIC X(4) VALUE x"1b5b316d".
000430         05 ws-fntnormal     PIC X(4) VALUE x"1b5b306d".
000440         05 ws-rojob         PIC X(7) VALUE x"1b5b33313b316d".
000450** Para números aleatorios:
000460     77 wn-semilla           PIC 9(8) USAGE IS COMP.
000470     77 wn-dado              PIC 9(5) USAGE IS COMP.
000480** Dados, caras, modificadores -- cálculos:
000490     77 numtiradas           PIC 99 VALUE 1 USAGE IS COMP.
000500     01 pasos.
000510         05 tirada           PIC X(10) OCCURS 6 TIMES.
000520         05 numdados         PIC 9(3) OCCURS 6 TIMES COMP.
000530         05 numcaras         PIC 9(6) OCCURS 6 TIMES COMP.
000540         05 signo            PIC X OCCURS 6 TIMES.
000550         05 mod              PIC 99 OCCURS 6 TIMES USAGE IS COMP.
000560         05 numdadosorig     PIC 9(3) OCCURS 6 TIMES COMP.
000570         05 tipo-especial    PIC 9 VALUE 0 OCCURS 6 TIMES COMP.
000580             88 explosivos         VALUE 1.
000590             88 fudge              VALUE 2.
000600     77 rparcial             PIC S9(7) VALUE 0 USAGE IS COMP.
000610     77 result               PIC S9(7) VALUE 0 USAGE IS COMP.
000620** Para mostrar los resultados:
000630     01 FILLER               PIC 9 VALUE 0 USAGE IS COMP.
000640         88 nueva-linea            VALUE 1 FALSE 0.
000650     01 FILLER               PIC 9 VALUE 0 USAGE IS COMP.
000660         88 resaltar-val           VALUE 1 FALSE 0.
000670     01 valores-a-mostrar.
000680         05 resultsigno      PIC X VALUE SPACES.
000690         05 resultdisp       PIC ZZZZZ9.
000700         05 bloquetirada     OCCURS 1 TO 6 TIMES
000710                                   DEPENDING ON numtiradas.
000720             10 moddisp      PIC Z9 VALUE IS NULL.
000730             10 fudgedisp    PIC 9 VALUE 0 OCCURS 100 TIMES COMP.
000740             10 valororg     PIC 9(5) OCCURS 100 TIMES
000750                                   USAGE IS COMP.
000760             10 valordisp    PIC ZZZZZ9 OCCURS 100 TIMES
000770                                   VALUE NULL.
000780     01 fudge-presentacion.
000790         05 fudge-menos      PIC X(4).
000800         05 fudge-neutro     PIC X(4).
000810         05 fudge-mas        PIC X(4).
000820     01 fudge-simbolos       REDEFINES fudge-presentacion.
000830         05 fudge-simb       PIC X(4) OCCURS 3 TIMES.
000840     77 relleno              PIC X(32) VALUE SPACES.
000850     77 sangria              PIC 99 USAGE IS COMP.
000860     01 frag-texto.
000870         05 expresion        PIC X(32) VALUE SPACES.
000880         05 tx-principio     PIC X(80) VALUE SPACES.
000890         05 ab-corchete      PIC XX VALUE SPACES.
000900         05 tx-result        PIC X(16) VALUE SPACES.
000910         05 separador-dados  PIC X(2) VALUE SPACES.
000920** Leer desde la línea de comandos:
000930     01 argumentos-programa.
000940         05 arg              PIC X(32) VALUE SPACES.
000950             88 arg-telegram           VALUE "--telegram".
000960             88 arg-nosum              VALUES "--nosumar",
000970                                              "--nosum".
000980         05 arg-l            PIC 99 USAGE IS COMP.
000990         05 numargs          PIC 99 USAGE IS COMP.
001000     01 opciones-programa.
001010         05 opt-telegram     PIC 9 VALUE 0 USAGE IS COMP.
001020             88 telegram           VALUE 1 FALSE 0.
001030         05 opt-nosumar      PIC 9 VALUE 0 USAGE IS COMP.
001040             88 nosumar            VALUE 1 FALSE 0.
001050** Mensajes de error:
001060     01 p-error              PIC 9 VALUE 0 USAGE IS COMP.
001070         88 err-ex-incorrecta      VALUE 1.
001080         88 err-cero-dados         VALUE 2.
001090         88 err-muchos-dados       VALUE 3.
001100         88 err-arg-vacio          VALUE 4.
001110         88 err-arg-absurdo        VALUE 5.
001120         88 err-fudge-expl         VALUE 6.
001130     77 texto-error          PIC X(256).
001140     77 cosa-erronea         PIC X(32).
001150     COPY "errores.cpy".

001160 PROCEDURE DIVISION.
001170   INICIALIZAR.
001180     ACCEPT wn-semilla FROM TIME.
001190     COMPUTE wn-dado = FUNCTION RANDOM(wn-semilla).

001200   LEER-ARGUMENTOS.
001210     INITIALIZE i, numargs.
001220     ACCEPT numargs FROM ARGUMENT-NUMBER.
001230     PERFORM VARYING i FROM 1 BY 1 UNTIL i > numargs
001240       ACCEPT arg FROM ARGUMENT-VALUE
001250       MOVE FUNCTION LOWER-CASE(arg) TO arg
001260** Comprobar las opciones de ejecución (--telegram, etc):
001270       EVALUATE TRUE
001280         WHEN arg-telegram SET telegram TO TRUE
001290         WHEN arg-nosum SET nosumar TO TRUE
001300         WHEN arg(1:1) IS NUMERIC OR arg(1:1) IS T_SIGNO
001310** ¡Primer carácter es un número, podría ser una tirada de dados!
001320** Un argumento como 6d6-3d4+1 debería interpretarse así:
001330** Subíndice    Tirada      Signo
001340**   1)           6d6         (+)   > Se sobreentiende.
001350**   2)           3d4          -
001360**   3)            1           +
001370** Para conseguir con esto con UNSTRING mientras se lee la expre-
001380** sión, hay que guardar los signos siempre un índice por encima
001390** de la cadena extraída en cada paso, porque se aplicarán a la
001400** siguiente. Si no se especifica otro, el primer signo siempre
001410** será "+".
001420**
001430** Una coma (,) como signo convierte la expresión en series de
001440** dados cuyos valores no se suman (véase opción "--nosumar").
001450**
001460** "numtiradas" cuenta tanto tiradas como modificadores:
001470** 1d4+2d6+1 --> numtiradas = 3
001480           MOVE "+" TO signo(1)
001490           MOVE arg TO expresion
001500           IF arg(1:1) IS T_SIGNO THEN
001510             MOVE arg(1:1) TO signo(1)
001520           END-IF
001530           INITIALIZE numtiradas
001540           MOVE FUNCTION STORED-CHAR-LENGTH(arg) TO arg-l
001550           MOVE 1 TO j
001560           PERFORM UNTIL j > arg-l
001570             ADD 1 TO numtiradas
001580             UNSTRING arg DELIMITED BY "+" OR "-" OR ","
001590               INTO tirada(numtiradas)
001600               DELIMITER IN signo(numtiradas + 1),
001610               WITH POINTER j
001620             END-UNSTRING
001630** Una expresión de tipo "-2+1d4", ",1d6" o similar. Es rebuscado
001640** pero intentamos procesarlas también añadiendo un cero inicial,
001650** para que el algoritmo no falle:
001660             IF tirada(numtiradas) = SPACES THEN
001670               MOVE 0 TO tirada(numtiradas)
001680             END-IF
001690** Una coma en la expresión pasa al modo de no sumar:
001700             IF signo(numtiradas + 1) = "," THEN
001710               SET nosumar TO TRUE
001720             END-IF
001730           END-PERFORM
001740         WHEN OTHER
001750           MOVE arg TO cosa-erronea
001760           SET err-arg-absurdo TO TRUE
001770           PERFORM ERROR-SALIDA
001780       END-EVALUATE
001790     END-PERFORM.
001800** No se ha escrito ninguna tirada:
001810     IF expresion = SPACES THEN
001820       SET err-arg-vacio TO TRUE
001830       PERFORM ERROR-SALIDA
001840     END-IF.

001850** ¿Hay errores en las tiradas suministradas por el usuario?   
001860** Si los hay se informa y se detiene el programa, de lo contrario
001870** se preparan los cálculos.
001880   COMPROBAR-ERRORES-EXPR.
001890     PERFORM VARYING i FROM 1 BY 1 UNTIL i > numtiradas
001900       INITIALIZE ds
001910       INSPECT tirada(i) TALLYING ds FOR ALL "d"
001920       IF ds = 1 THEN
001930** Expresión que debería ser de tipo "2d4", "10d10", etc:
001940         UNSTRING tirada(i) DELIMITED BY "d"
001950           INTO txtemp, txtemp2
001960         END-UNSTRING
001970         MOVE FUNCTION STORED-CHAR-LENGTH(txtemp2) TO tx2long
001980** ¿Y si hubiera dados explosivos? Por ejemplo, "10d10!", "3d6!":
001990** ¿Y si hubiera dados "fudge"? Por ejemplo, "6df":
002000         PERFORM COMPROBAR-DADOS-EXPLOSIVOS
002010         PERFORM COMPROBAR-DADOS-FUDGE
002020** Por fin seguimos con la evaluación normal de dados y caras:
002030         IF FUNCTION TRIM(txtemp) IS NOT NUMERIC OR
002040            FUNCTION TRIM(txtemp2) IS NOT NUMERIC THEN
002050           MOVE tirada(i) TO cosa-erronea
002060           SET err-ex-incorrecta TO TRUE
002070           PERFORM ERROR-SALIDA
002080         END-IF
002090         MOVE txtemp TO numdados(i)
002100         MOVE txtemp2 TO numcaras(i)
002110         IF numdados(i) = 0 OR numcaras(i) = 0 THEN
002120           MOVE tirada(i) TO cosa-erronea
002130           SET err-cero-dados TO TRUE
002140           PERFORM ERROR-SALIDA
002150         END-IF
002160         IF numdados(i) > 99 OR numcaras(i) > 99999 THEN
002170           MOVE tirada(i) TO cosa-erronea
002180           SET err-muchos-dados TO TRUE
002190           PERFORM ERROR-SALIDA
002200         END-IF
002210       END-IF
002220** Expresión mal formada, cosa ilegible:
002230       IF ds > 1 OR (ds = 0 AND
002240          FUNCTION TRIM(tirada(i)) IS NOT NUMERIC) THEN
002250         MOVE arg TO cosa-erronea
002260         SET err-ex-incorrecta TO TRUE
002270         PERFORM ERROR-SALIDA
002280       END-IF
002290** Números sueltos. Deberían ser modificadores a la tirada:
002300** (1d6+_1_, 3d10-_2_, etc):
002310       IF FUNCTION TRIM(tirada(i)) IS NUMERIC THEN
002320         MOVE tirada(i) TO mod(i)
002330       END-IF
002340     END-PERFORM.

002350** Si el programa llega aquí, todo es correcto.
002360** Tirar dados, sumar modificadores, calcular resultados:
002370** 
002380** En lo sucesivo:
002390** i = bloque de tiradas (ej: 2d6, 8d10, etc)
002400** j = cada dado concreto dentro de ese bloque
002410   TIRAR-DADOS.
002420     PERFORM VARYING i FROM 1 BY 1 UNTIL i > numtiradas
002430       MOVE 0 TO rparcial
002440** Si numdados = 0, no es una tirada sino un modificador:
002450       IF numdados(i) = 0 THEN
002460         MOVE mod(i) TO rparcial, moddisp(i)
002470       END-IF
002480       IF numdados(i) > 0 THEN
002490         MOVE numdados(i) TO numdadosorig(i)
002500         PERFORM VARYING j FROM 1 BY 1 UNTIL j > numdados(i)
002510           COMPUTE wn-dado = (FUNCTION RANDOM() * numcaras(i)) + 1
002520** ¿Hay dados explosivos? Entonces cada resultado máximo añade un
002530** dado extra a la tirada (pero un máximo en un dado extra no suma
002540** otro dado extra más):
002550           IF explosivos(i) THEN
002560             IF wn-dado = numcaras(i) AND
002570                j <= numdadosorig(i) THEN
002580               ADD 1 TO numdados(i)
002590             END-IF
002600           END-IF
002610** Los dados fudge son especiales: tienen 6 caras y tres valores
002620** posibles, "+" (1), "-" (-1) y "0" (en blanco, 0). En la tabla
002630** interna del programa se representan con 1, 2 y 3, que apuntan
002640** luego a los símbolos apropiados para la terminal o Telegram:
002650           IF fudge(i) THEN
002660             EVALUATE wn-dado
002670               WHEN 1 THRU 2
002680                 SUBTRACT 1 FROM rparcial
002690                 MOVE 1 TO fudgedisp(i, j)
002700               WHEN 3 THRU 4
002710                 MOVE 2 TO fudgedisp(i, j)
002720               WHEN 5 THRU 6
002730                 ADD 1 TO rparcial
002740                 MOVE 3 TO fudgedisp(i, j)
002750             END-EVALUATE
002760           ELSE
002770** Dados normales con valores que se suman:
002780             ADD wn-dado TO rparcial
002790             MOVE wn-dado TO valordisp(i, j), valororg(i, j)
002800           END-IF
002810         END-PERFORM
002820       END-IF
002830       IF signo(i) = "+" THEN
002840         ADD rparcial TO result
002850       ELSE
002860         SUBTRACT rparcial FROM result
002870       END-IF
002880     END-PERFORM.
002890     MOVE result TO resultdisp.
002900     IF result < 0 THEN MOVE "-" TO resultsigno.

002910** Hay diferencias de formato entre una terminal ANSI y Telegram.
002920** Antes de continuar es el momento de establecerlas.
002930** 
002940** Si no se suman los dados, cada serie aparecerá en una línea
002950** independiente, para eso se emplean las variables "sangria",
002960** "intro" y "relleno".
002970** 
002980** Negritas tanto para la terminal como para Telegram.
002990   ANSI-O-TELEGRAM.
003000     IF telegram THEN
003010       MOVE "✅" TO fudge-mas
003020       MOVE "◻" TO fudge-neutro
003030       MOVE "❌" TO fudge-menos
003040       MOVE "🎲" TO tx-principio
003050       MOVE "\[" TO ab-corchete
003060       MOVE 5 TO sangria
003070       STRING " = ", "*", FUNCTION TRIM(resultsigno),
003080              FUNCTION TRIM(resultdisp), "*" INTO tx-result
003090     ELSE
003100       MOVE "+" TO fudge-mas
003110       MOVE "□" TO fudge-neutro
003120       MOVE "-" TO fudge-menos
003130       MOVE 10 TO sangria
003140       ADD arg-l TO sangria
003150       STRING ws-colorbold, "Dados (", FUNCTION TRIM(expresion),
003160              "):", ws-fntnormal INTO tx-principio
003170       MOVE "[" TO ab-corchete
003180       STRING " = ", ws-colorbold, FUNCTION TRIM(resultsigno),
003190              FUNCTION TRIM(resultdisp), ws-fntnormal
003200              INTO tx-result
003210     END-IF.

003220** Mostrar las tiradas y resultados, según proceda:
003230** Ejemplos de lo que se pretende:
003240**   1d4+2d6+1: [3] + [2 + 6] + 1 = 12
003250**   2d4!: [1 + 4 + 3!]  (Tercer dado extra por el 4 máximo)
003260**   6df+1: [+, □, □, -, +, +] + 1 = 3
003270**   3d4,2d6: [1, 2, 4]
003280**            [3, 5]
003290   MOSTRAR-DADOS.
003300     DISPLAY FUNCTION TRIM(tx-principio), " " WITH NO ADVANCING.
003310     PERFORM VARYING i FROM 1 BY 1 UNTIL i > numtiradas
003320       IF NOT nosumar AND numdados(i) = 0 THEN
003330         PERFORM IMPRIMIR-SIGNO
003340         DISPLAY FUNCTION TRIM(moddisp(i)) WITH NO ADVANCING
003350       END-IF
003360       IF NOT valordisp(i, 1) = LOW-VALUES
003370          OR NOT fudgedisp(i, 1) = 0 THEN
003380         IF nosumar THEN
003390** Líneas independientes para series de dados en modo de
003400** no sumar resultados:
003410           IF nueva-linea THEN
003420             DISPLAY intro, relleno(1:sangria) WITH NO ADVANCING
003430           END-IF
003440           SET nueva-linea TO TRUE
003450           DISPLAY FUNCTION TRIM(ab-corchete) WITH NO ADVANCING
003460         ELSE
003470           PERFORM IMPRIMIR-SIGNO
003480           DISPLAY FUNCTION TRIM(ab-corchete) WITH NO ADVANCING
003490         END-IF
003500** Los dados pueden separarse con "+" o con comas, según el tipo
003510** de tirada. Los "%" sirven para :
003520         IF nosumar OR fudge(i) THEN
003530           MOVE "," TO separador-dados
003540         ELSE
003550           MOVE " +" TO separador-dados
003560         END-IF
003570         PERFORM VARYING j FROM 1 BY 1 UNTIL j > numdados(i)
003580           PERFORM IMPRIMIR-VALOR
003590           IF j < numdados(i) THEN
003600             DISPLAY FUNCTION TRIM(separador-dados, TRAILING)
003610                     WITH NO ADVANCING
003620           END-IF
003630         END-PERFORM
003640         DISPLAY " ]" WITH NO ADVANCING
003650       END-IF
003660     END-PERFORM.

003670   IMPRIMIR-RESULTADO.
003680     IF nosumar THEN
003690       MOVE ALL SPACES TO tx-result
003700     END-IF.
003710     DISPLAY FUNCTION TRIM(tx-result, TRAILING).
003720     STOP RUN.


003730** Expresiones como "6d6!", o "10d4!":
003740   COMPROBAR-DADOS-EXPLOSIVOS.
003750     IF txtemp2(tx2long:1) = "!" THEN
003760       SET explosivos(i) TO TRUE
003770       MOVE SPACE TO txtemp2(tx2long:1)
003780     END-IF.
003790     EXIT.
003800** Hecha la comprobación anterior, se elimina si lo hubiera el
003810** "!" para que continúe la ejecución normal del programa.

003820   COMPROBAR-DADOS-FUDGE.
003830     IF FUNCTION TRIM(txtemp2) = "f" THEN
003840** Dados "fudge" y explosivos a la vez (alguien ha escrito algo
003850** como "6df!" para la tirada) sería absurdo y no se acepta:
003860       IF explosivos(i) THEN
003870         MOVE tirada(i) TO cosa-erronea
003880         SET err-fudge-expl TO TRUE
003890         PERFORM ERROR-SALIDA
003900       END-IF
003910** Un dado "fudge" tiene seis caras:
003920       SET fudge(i) TO TRUE
003930       MOVE 6 TO txtemp2
003940     END-IF.
003950     EXIT.

003960** Signos entre series de dados: [...] +/- [...]
003970   IMPRIMIR-SIGNO.
003980     IF i > 1 AND NOT (i = 2 AND nosumar) THEN
003990       DISPLAY " ", signo(i), " " WITH NO ADVANCING
004000     END-IF.
004010     EXIT.

004020   IMPRIMIR-VALOR.
004030     SET resaltar-val TO FALSE.
004040** Los valores mínimos y máximos de cada dado pueden resaltarse.
004050** Esto de momento sólo se usa en Telegram, donde aparecen en
004060** cursiva (por ejemplo "_1_", etc):
004070     IF valororg(i, j) = 1 OR valororg(i, j) = numcaras(i) THEN
004080       SET resaltar-val TO TRUE
004090     END-IF.
004100     EVALUATE TRUE
004110       WHEN resaltar-val AND telegram
004120         DISPLAY " _" WITH NO ADVANCING
004130         DISPLAY FUNCTION TRIM(valordisp(i, j)) WITH NO ADVANCING
004140         PERFORM EVALUAR-DADO-EXTRA
004150         DISPLAY "_" WITH NO ADVANCING
004160       WHEN fudge(i)
004170         DISPLAY " ",
004180                 FUNCTION TRIM(fudge-simb(fudgedisp(i, j)))
004190                 WITH NO ADVANCING
004200       WHEN OTHER
004210         DISPLAY " ",
004220                 FUNCTION TRIM(valordisp(i, j)) WITH NO ADVANCING
004230         PERFORM EVALUAR-DADO-EXTRA
004240     END-EVALUATE.
004250     EXIT.

004260** En el modo de dados explosivos, los dados extra añadidos por
004270** resultados máximos anteriores se marcan con un "!":
004280   EVALUAR-DADO-EXTRA.
004290     IF j > numdadosorig(i) THEN
004300       IF telegram THEN
004310         DISPLAY "💥" WITH NO ADVANCING
004320       ELSE
004330         DISPLAY "!" WITH NO ADVANCING
004340       END-IF
004350     END-IF.
004360     EXIT.

004370** Los errores se leen de una matriz en el archivo "errores.cpy",
004380** y al texto resultante se le da formato para Telegram o la
004390** terminal:
004400   ERROR-SALIDA.
004410     MOVE mensaje-error(p-error) TO texto-error.
004420     IF telegram THEN
004430       MOVE FUNCTION SUBSTITUTE(texto-error, "<err>", "*", 
004440            "</err>", "*",
004450            "<exec />", "/tirar",
004460            "<emoji_meh />", "😒",
004470            "<expr />", FUNCTION TRIM(cosa-erronea),
004480            "<mono>", "`",
004490            "</mono>", "`") TO texto-error
004500     ELSE
004510       MOVE FUNCTION SUBSTITUTE(texto-error, "<err>", ws-rojob, 
004520            "</err>", ws-fntnormal,
004530            "<exec />", "./dados",
004540            "<emoji_meh />", " ",
004550            "<expr />", FUNCTION TRIM(cosa-erronea),
004560            "<mono>", ws-colorbold,
004570            "</mono>", ws-fntnormal) TO texto-error
004580     END-IF.
004590     DISPLAY FUNCTION TRIM(texto-error).
004600     STOP RUN.


