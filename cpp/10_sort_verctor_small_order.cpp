#include <iostream>
#include <vector>

int main()
{
  std::vector v = { 8, 0, 4, 2, 9, 5, 7, 1, 3};
  std::size_t min_index = 0;
  std::size_t temp;

  for (std::size_t head=0; head != v.size(); ++head)
  {
    min_index = head;

    for (std::size_t j=head; j != v.size(); ++j)
    {
      if (v.at(j) < v.at(min_index))
        min_index = j;
    }

    temp = v.at(head);
    v.at(head) = v.at(min_index);
    v.at(min_index) = temp;

  }

  std::cout << "sorted: ";
  for (std::size_t i=0; i != v.size(); ++i)
    std::cout << v.at(i) << " ";
}
