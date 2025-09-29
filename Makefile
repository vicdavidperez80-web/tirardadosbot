rutabin = /usr/local/bin
rutalibexec = /usr/local/libexec
rutadatos := /usr/local/share/tirardados

all:
	cobc -x dados.cob
	strip -v dados

install:
	install -v -o root -m 755 dados $(rutabin)
	install -v -o root -m 644 tirardados.py $(rutalibexec)
	ln -f -s $(rutalibexec)/tirardados.py $(rutadatos)
	install -v -o root -m 644 dados.cob $(rutadatos)
	install -v -o root -m 644 tirardadosbot_token $(rutadatos)
