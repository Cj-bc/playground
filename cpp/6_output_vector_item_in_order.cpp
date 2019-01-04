#include <iostream>
#include <vector>

int main()
{
  std::vector<int> v;

  for (int i=0; i != 10; ++i)
  {
    v.push_back(i);
  }

  for (int i=0; i < v.size(); ++i)
  {
    std::cout << i << "th item: " << v.at(i) << "\n";
  }
}
