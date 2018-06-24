
# CR-someday: separate libs more so you don't have to include three everywhere.

DIRS = atoms calibrate grid/wall grid/window grid/dome

all:
	for DIR in $(DIRS); do ( cd $$DIR && make ); done 
