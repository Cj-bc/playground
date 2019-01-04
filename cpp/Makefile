build: all.h.gch
	clang++ -std=c++17 -Wall --pedantic-errors -o dst/$(basename $(FILE)) -include all.h $(FILE)

build_all: all.h.gch
	for file in *.cpp;do
		clang++ -std=c++17 -Wall --pedantic-errors -o dst/${file%.cpp} -include all.h $file
	done

all.h.gch: all.h
	g++ -std=c++17 -Wall --pedantic-errors -x c++-header -o all.h.gch all.h

run:
	./dst/$(basename $(FILE))

.PHONY: run build build_all
