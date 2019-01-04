#include <iostream>
#include <vector>

int main()
{
  std::vector v = { 1, 5, 3, 7, 2, 9, 0};
  std::size_t min_index = 0;

  for (std::size_t i=0; i != v.size(); ++i)
  {
    if (v.at(i) < v.at(min_index))
      min_index = i;
  }

  std::cout << "smallest number: " << v.at(min_index) << " (at " << min_index << ")\n";

}
