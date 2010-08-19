CXX=g++
CXX_FLAGS=-O2 -Wall -ansi -pedantic-errors

COMMANDS=bin/unf bin/unf-test bin/unf-time bin/gen-unf-table
SRC=src/unf
BUILD_TRIES=${SRC}/trie/builder.hh ${SRC}/trie/char_stream.hh ${SRC}/trie/node_allocator.hh ${SRC}/trie/node.hh
SEARCH_TRIES=${SRC}/trie/char_stream.hh ${SRC}/trie/node.hh ${SRC}/trie/searcher.hh

all: bin ${COMMANDS}

bin:
	mkdir bin

bin/gen-unf-table: ${SRC}/gen-unf-table.cc ${BUILD_TRIES}
	${CXX} ${CXX_FLAGS} -o ${@} ${<}

bin/unf: ${SRC}/unf.cc ${SRC}/normalizer.hh ${SRC}/table.hh ${SRC}/util.hh ${SEARCH_TRIES}
	${CXX} ${CXX_FLAGS} -o ${@} ${<}

bin/unf-test: ${SRC}/unf-test.cc ${SRC}/normalizer.hh ${SRC}/table.hh ${SRC}/util.hh ${SEARCH_TRIES}
	${CXX} ${CXX_FLAGS} -o ${@} ${<}

bin/unf-time: ${SRC}/unf-time.cc ${SRC}/normalizer.hh ${SRC}/table.hh ${SRC}/util.hh ${SEARCH_TRIES}
	${CXX} ${CXX_FLAGS} -o ${@} ${<}

clean:
	rm -f ${COMMANDS}

test: bin/unf-test
	${<} < data/normalization-test.txt

gen-table: bin/gen-unf-table
	bin/gen-unf-table ${SRC}/table.hh data
