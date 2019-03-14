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
    for (std::size_t i=lsLen-1; i > 0; --i)
    {
      for (std::size_t shifted=0; i-shifted-1 >= 0; ++shifted)
      {
        std::cout << "i: " << i << ", shifted: " << shifted << "\n";
        if (ls.at(i-shifted) < ls.at(i-shifted-1))
        {
            int tmp = ls.at(i-shifted);
            ls.at(i-shifted) = ls.at(i-shifted-1);
            ls.at(i-shifted-1) = tmp;
        } else {
            break;
        }
      }
    }

    std::cout << "sorted: ";
    putArray(ls);
}
