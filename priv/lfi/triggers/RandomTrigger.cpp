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

#include "RandomTrigger.h"
#include <stdlib.h>
#include <time.h>
#include <fstream>
#include <iostream>

extern u_int8_t g_libfi_enabled;
/*
** After editing this list of global vars, please update the
** triggers/exported_symbols_list file
*/
u_int8_t g_libfi_RandomTrigger_enabled = 1;
u_int8_t g_libfi_RandomTrigger_reseed = 0;
u_int32_t g_libfi_RandomTrigger_seed = 0;
u_int8_t g_libfi_RandomTrigger_verbose = 0;

RandomTrigger::RandomTrigger()
  : probability(0)
{
}

void RandomTrigger::Init(xmlNodePtr initData)
{
  xmlNodePtr nodeElement, textElement;
  time_t t;

  if (g_libfi_RandomTrigger_verbose) cerr << "RandomTrigger::Init\r\n";
  nodeElement = initData->children;
  while (nodeElement)
  {
    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"percent"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type)
        probability = atoi((char*)textElement->content);
    }
    nodeElement = nodeElement->next;
  }
  
  if (!g_libfi_RandomTrigger_seed)
  {
    g_libfi_RandomTrigger_seed = (unsigned)time(&t);
  }
  srand(g_libfi_RandomTrigger_seed);
}

bool RandomTrigger::Eval(const string* fn, ...)
{
    if (g_libfi_RandomTrigger_reseed) {
        srand(g_libfi_RandomTrigger_seed);
        g_libfi_RandomTrigger_reseed = 0;
    }
    if (g_libfi_enabled && g_libfi_RandomTrigger_enabled &&
        rand() % 100 < probability) {
      if (g_libfi_RandomTrigger_verbose) cerr << "RandomTrigger::Eval fn=" << *fn << ", probability=" << probability << " = true\r\n";
    return true;
    }
    if (g_libfi_RandomTrigger_verbose) cerr << "RandomTrigger::Eval fn=" << *fn << ", probability=" << probability << " = false\r\n";
    return false;
}
