/*
     Created by Paul Marinescu and George Candea
     Copyright (C) 2009 EPFL (Ecole Polytechnique Federale de Lausanne)

     This file is part of LFI (Library-level Fault Injector).

     LFI is free software: you can redistribute it and/or modify it  
     under the terms of the GNU General Public License as published by the  
     Free Software Foundation, either version 3 of the License, or (at  
     your option) any later version.

     LFI is distributed in the hope that it will be useful, but  
     WITHOUT ANY WARRANTY; without even the implied warranty of  
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  
     General Public License for more details.

     You should have received a copy of the GNU General Public  
     License along with LFI. If not, see http://www.gnu.org/licenses/.

     EPFL
     Dependable Systems Lab (DSLAB)
     Room 330, Station 14
     1015 Lausanne
     Switzerland
*/

#include "ReadInspector.h"
#include <iostream>
#include <stdarg.h>

extern u_int8_t g_libfi_enabled;
/*
** After editing this list of global vars, please update the
** triggers/exported_symbols_list file
*/
u_int8_t g_libfi_ReadInspector_enabled = 1;
u_int8_t g_libfi_ReadInspector_verbose = 0;

ReadInspector::ReadInspector()
{
}

bool ReadInspector::Eval(const string* functionName, ...)
{
  /* only intended to be used when intercepting the read function */
  va_list ap;
  int fd;
  size_t size;
  bool retval;

  va_start(ap, functionName);
  fd = va_arg(ap, int);
  va_arg(ap, void*);
  size = va_arg(ap, size_t);
  va_end(ap);

  /* inject only when reading 1024 bytes from stdin */
  retval = g_libfi_enabled && g_libfi_ReadInspector_enabled &&
           (fd == 0 && size == 1024);
  if (g_libfi_ReadInspector_verbose) cerr << "ReadInspector::Eval fn=" << *functionName << ", " << retval << "\r\n";
  return retval;
}
