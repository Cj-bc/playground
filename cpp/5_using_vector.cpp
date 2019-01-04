#include <vector>
#include <iostream>

int main()
{
  std::vector<int> v;

  for (int i=0;i != 10; ++i)
  {
    std::cout << "pushing: " << i << "\n";
    v.push_back(i);
  }

  std::cout << "size of v: " << v.size();
}

