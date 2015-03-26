#ifndef EXTENSIONS_HPP
#define EXTENSIONS_HPP

/* Register an extension and gets is identifier.  Multiple calls for the same
   name return the same identifier.  */
extern unsigned register_extension(std::string name);

#endif
