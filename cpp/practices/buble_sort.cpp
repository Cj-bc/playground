#include <list>
#include <vector>
#include <iostream>

void putArray(std::vector<int> a) {
    for (std::size_t i=0; i< a.size(); ++i) {
        std::cout << a.at(i) << ",";
    }
    std::cout << "\n";
}


int main() {
    std::vector<int> ls;
    ls= {0,2,3,562,6,23,6,3,1,40};
    std::size_t lsLen = ls.size();

    std::cout << "original: ";
    putArray(ls);
    std::cout << "size of it: " << lsLen << "\n";

    for (std::size_t i=0; i < lsLen-1;++i )
    {
        for (std::size_t j=lsLen-1; j >0; --j)
        {
            if (ls.at(j) < ls.at(j-1))
            {
                std::swap(ls.at(j), ls.at(j-1));
            }
        }
    }
    std::cout << "sorted: ";
    putArray(ls);
}
