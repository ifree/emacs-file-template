CC=clang++
CFLAGS= -Iinclude -g -Wall -std=c++11
LDFLAGS= 

SRC_DIR = src
SOURCE = $(foreach d, $(SRC_DIR), $(wildcard $(d)/*.cpp))
OBJS = $(patsubst %.cpp, %.o, $(SOURCE))

all: $(OBJS)

%.o : %.cpp
	$(CC) -c $< -o $@ $(CFLAGS)

test% : test/%.o $(OBJS)
	$(CC) -o bin/$(subst test,,$@) $^ $(LDFLAGS)

#.SECONDARY:
.PHONY: clean
clean:
	rm -rf bin/*
	rm -rf $(OBJS)
	rm -rf test/*.o
