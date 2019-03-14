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

    for (std::size_t l_limit=0; l_limit >= lsLen; ++l_limit)
    {
        for (std::size_t current=lsLen-l_limit-1; current != l_limit; ++current)
        {
            if (ls.at(current) < ls.at(current -1))
            {
                std::swap(ls.at(current), ls.at(current-1));
            } else {
                break;
            }
        }
    }

    std::cout << "sorted: ";
    putArray(ls);
}
