# coding=utf-8

# pyTelegramBotAPI 4.8.0
import telebot

import re
import subprocess
from random import seed
from random import randint
import os.path

seed()
numversion = "21-09-2025"
archtoken = "tirardadosbot_token"

# Rutas para el programa de lanzar dados, el token del bot y el código:

rutasexec = ['./', '/usr/local/bin/', '/data/data/com.termux/files/usr/bin/']
rutascod = ['./', '/usr/local/share/tirardados/',
            '/data/data/com.termux/files/usr/share/tirardados/']
for ruta in rutasexec:
    testexec = os.path.isfile(ruta + "dados")
    if testexec == True:
        dadosexec = ruta + "dados"
        break
for ruta in rutascod:
    testcod = os.path.isfile(ruta + "dados.cob")
    if testcod == True:
        rutcodigo = ruta
        break

rutatoken = rutcodigo + archtoken
if os.path.isfile(rutatoken) == False:
    print("Error, no se encuentra " + rutatoken)
    quit()

# DEBUG:
print()
print("** Rutas del bot: **")
print("    " + dadosexec)
print("    " + rutatoken)
print()

# Extraer el token del bot del archivo "tirardadosbot_token":

tok = open(rutatoken)
tokendelbot = tok.readline()
tokendelbot = tokendelbot.strip()
tok.close()

bot = telebot.TeleBot(tokendelbot, parse_mode="MARKDOWN")

# Mensaje de aviso cada vez que arranca el bot. -1001187085073 es la id del grupo
# de Dragones y Mazmorras:
#bot.send_message(-1001187085073, "¡Hola, ya estoy preparado para tirar dados!")

@bot.message_handler(commands=['start'])
def send_welcome(message):
	bot.reply_to(message, "Hola, soy *Tirar Dados*, un bot de Telegram que hace exactamente eso, tirar una cantidad variable de dados con un número libre de caras —¡hasta 99999!—. Soy muy guay.\n\nSintaxis:\n • /tirar 3d4\n • /tirar 6d6+2d4\n • /tirar 10d256\n • /tirar 1d6+1\n • /tirar 1d12-1\n • /tirar 3d6,6d6 (series independientes)\n • /tirar 4d8! (dados explosivos)\n • /tirar 6df (dados \"fudge\")\n • /tirar 4df+5+6d6!+8d20 (mezcla absurda de todo)\n...\n\nComo alternativa corta a /tirar puede usarse /t.")

@bot.message_handler(commands=['tirar', 't'])
def tirar_dado(message):
    argumentos = message.text.split(" ",1)
    if len(argumentos) > 1:
        print("-> " + dadosexec + " " + argumentos[1])
        tirada = subprocess.Popen(dadosexec + " --telegram " + "'" +
                 argumentos[1].strip() + "'", stdout=subprocess.PIPE, shell=True)
    else:
        tirada = subprocess.Popen(dadosexec + " --telegram", stdout=subprocess.PIPE, shell=True)
    mensajedados = tirada.communicate()
    bot.reply_to(message, mensajedados)
    result = re.search(r"\*([0-9]+)\*", str(mensajedados))
    if result != None:
        resultado_valido = True
        valor_res = int(result.group(1))
        print("Resultado: " + str(valor_res))
    else:
        valor_res = False
        print("Resultado: N/A")
    print()

    ###
    ### Mensajes aleatorios de cuando en cuando:
    ###
    listamensajestrol = ["Ojalá alguien muera por culpa de esta tirada.",
                         "Estáis todos condenados, lo veo en las estrellas.",
                         "Arturo, eres el GM más gandul de la historia del rol. Deja ya la IA. 😒",
                         "Esto necesita más acción, espero que palme alguien rápido.",
                         "¡En la tirada más importante de tu vida sacaré un uno!",
                         "A partir de ahora sólo daré resultados malos, me he cruzado con un gato negro.",
                         "Veo vuestro futuro. Es negro.",
                         "Mi misión es tocar las narices con malas tiradas, y voy a cumplirla a partir de ahora.",
                         "Tanta historia para que luego un kobold costroso te mate.",
                         "¡Madre mía chaval, qué mal te veo!"]
    nummensajes = len(listamensajestrol)
    mensajetrol = False
    aleat = randint(1, 20)
    if aleat == 1:
        mensajetrol = True
        aleat = randint(1, nummensajes)
        mens = listamensajestrol[aleat - 1]
        bot.send_message(message.chat.id, mens)
    if (valor_res == 1) and (mensajetrol == False):
        aleat = randint(1,4)
        if aleat == 1:
            imagentroll = "troll-bl2.gif"
            bot.send_animation(message.chat.id, animation=open(imagentroll, 'rb'))

@bot.message_handler(commands=['codigo'])
def mandar_codigo(message):
    documento = open(rutcodigo + "dados.cob", "rt")
    documento2 = open(rutcodigo + "tirardados.py", "rt")
    bot.reply_to(message, "¡Ahí va mi código fuente!")
    bot.send_document(message.chat.id, documento, "dados.cob",
                      "Primero, el programa en COBOL que calcula las tiradas de dados... Escrito para GnuCOBOL.")
    bot.send_document(message.chat.id, documento2, "tirardados.py", "Y ahora yo mismo. 😊")

@bot.message_handler(commands=['ayuda', 'help'])
def ayuda(message):
    bot.reply_to(message, "Soy más simple que un botijo y admito los siguientes comandos:\n\n/start — Mensaje de bienvenida.\n/tirar, /t — Mi trabajo principal, hacer una tirada de dados.\n/codigo — Obtener una copia de mi código fuente.\n/ayuda, /help — Este mensaje.\n/version — Versión actual del bot en ejecución.\n\nUna tirada puede ser muy sencilla —por ejemplo, `1d6`, un único dado de seis caras—, pero también puede ir acompañada de modificadores que se sumen o resten —`1d6+1`, `1d8-2`—, o incluir diversos tipos de dados —`2d6+1d4`, etc—. ¡Uno puede mandarme cosas tan absurdas como `1d80+2d10-3d8+3`!\n\nTambién pueden tirarse series de dados sin sumarlos usando comas como separadores —por ejemplo `3d6,6d6`—. O los llamados \"dados explosivos\", en los que cada valor máximo añade un dado extra a la tirada: `4d4!`, `3d6!+2d8!`, etc.\n\nPor último, queda mencionar los dados \"fudge\" —como muestra `6df` o `4df+5`—, cuyos valores posibles son sólo tres: positivo, negativo y neutro.")

@bot.message_handler(commands=['version'])
def version_tirardados(message):
    bot.send_message(message.chat.id, "Tirardados - versión " + numversion)

bot.infinity_polling()
