
# CR-someday: separate libs more so you don't have to include three everywhere.

DIRS = atoms calibrate spark/wall spark/window spark/fred

# bitrotten, to be restored:
# poly/bin

all:
	for DIR in $(DIRS); do ( cd $$DIR && make ); done 
