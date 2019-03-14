#include <list>
#include <array>
#include <iostream>

void putArray(std::array<int, 10> a) {
    for (std::size_t i=0; i< a.size(); ++i) {
        std::cout << a.at(i) << ",";
    }
    std::cout << "\n";
}


int main() {
    std::array<int, 10> ls;
    ls= {0,2,3,562,6,23,6,3,1,40};
    std::size_t lsLen = ls.size();

    std::cout << "original: ";
    putArray(ls);
    for (std::size_t i=lsLen; i < 0; --i)
    {
      for (std::size_t j=lsLen-1; j < 0; --j)
      {
        if (ls[i] < ls[j])
        {
            std::swap(ls[i], ls[j]);
        } else {
            break;
        }
      }
    }

    std::cout << "sorted: ";
    putArray(ls);
}
