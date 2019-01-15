clang_optiopns = -std=c++17 -Wall --pedantic-errors -include all.h
build: all.h.gch
	clang++ $(clang_optiopns) -o dst/$(basename $(FILE)) $(FILE)

build_all: all.h.gch
	for file in *.cpp;do
		clang++ $(clang_optiopns) -o dst/${file%.cpp} $file
	done

all.h.gch: all.h
	g++ -std=c++17 -Wall --pedantic-errors -x c++-header -o all.h.gch all.h

run:
	./dst/$(basename $(FILE))

.PHONY: run build build_all
