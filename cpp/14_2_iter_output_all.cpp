#include "all.h"

int main()
{
  auto output_all = [](auto first, auto last)
  {
    for (auto iter = first; iter != last; ++iter)
    {
      std::cout << *iter << "\n";
    }
  };

  std::vector<int> v = {0,1,2,3,4,5,6,7,8,9,10};
  std::vector<std::string> str = {"first", "second", "third", "fourth", "fifth"};

  auto v_begin = std::begin(v);
  auto v_end = std::end(v);
  auto str_begin = std::begin(str);
  auto str_end = std::end(str);

  output_all(v_begin, v_end);
  output_all(str_begin, str_end);
}
