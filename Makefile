#Test example
SRC=/home/skywelld/mytest
DES=/home/skywelld/mytest
all:${SRC}/main.cpp
	gcc -o ${DES}/mytest.x ${SRC}/main.cpp
