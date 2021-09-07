#ifndef Mask_hpp
#define Mask_hpp

#include <memory>
#include <utility>

namespace TilingSystem {
class Mask {
public:
  Mask(const std::pair<int, int>& size, int id);
  const std::vector<std::vector<bool>>& minimalSets(int maxGridSize);

private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}

#endif /* Mask_hpp */
