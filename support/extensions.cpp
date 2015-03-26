#include <unordered_map>

#include "extensions.hpp"

static unsigned next_extension = 1;
static std::unordered_map<std::string, unsigned> extension_ids;

unsigned
register_extension(std::string name) {
  auto it = extension_ids.find(name);

  if (it != extension_ids.end())
    return it->second;

  const unsigned result = next_extension;
  next_extension += 1;
  extension_ids.insert(std::pair<std::string, unsigned>(name, result));
  return result;
}
