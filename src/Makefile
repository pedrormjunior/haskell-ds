
# Code:
HC=ghc

code:
	make server-code
	make slave1-code
	make slave2-code
	make client-code
	make book-updater-code

server-code:
	${HC} --make ServerMain/ServerMain.hs

slave1-code:
	${HC} --make Slave1Main/Slave1Main.hs

slave2-code:
	${HC} --make Slave2Main/Slave2Main.hs

client-code:
	${HC} --make ClientMain/ClientMain.hs

book-updater-code:
	${HC} --make ServerBookUpdateMain/ServerBookUpdateMain.hs

clean-code:
	rm *~ *.hi *.o */*~ */*.hi */*.o \
		ServerMain/ServerMain Slave1Main/Slave1Main Slave2Main/Slave2Main ClientMain/ClientMain ServerBookUpdateMain/ServerBookUpdateMain

# Haddock:
haddock-type=--html

haddock:
	make server-haddock
	make slave1-haddock
	make slave2-haddock
	make client-haddock
	make book-updater-haddock

server-haddock:
	haddock ServerMain/ServerMain.hs ${haddock-type} --odir=documentation/ServerMain/

slave1-haddock:
	haddock Slave1Main/Slave1Main.hs ${haddock-type} --odir=documentation/Slave1Main/

slave2-haddock:
	haddock Slave2Main/Slave2Main.hs ${haddock-type} --odir=documentation/Slave2Main/

client-haddock:
	haddock ClientMain/ClientMain.hs ${haddock-type} --odir=documentation/ClientMain/

book-updater-haddock:
	haddock ServerBookUpdateMain/ServerBookUpdateMain.hs ${haddock-type} --odir=documentation/ServerBookUpdateMain/

clean-haddock:
	rm -r documentation/
