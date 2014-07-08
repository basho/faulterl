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

#include "CallCountTrigger.h"
#include <iostream>

extern u_int8_t g_libfi_enabled;
/*
** After editing this list of global vars, please update the
** triggers/exported_symbols_list file
*/
u_int8_t g_libfi_CallCountTrigger_enabled = 1;
u_int8_t g_libfi_CallCountTrigger_verbose = 0;

CallCountTrigger::CallCountTrigger()
  : callCount(0)
{
}

void CallCountTrigger::Init(xmlNodePtr initData)
{
  xmlNodePtr nodeElement, textElement;

  if (g_libfi_CallCountTrigger_verbose) cerr << "CallCountTrigger::Init\r\n";
  nodeElement = initData->children;
  while (nodeElement)
  {
    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"callcount"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type)
        callCounts.push_back(atoi((char*)textElement->content));
    }
    nodeElement = nodeElement->next;
  }
}

bool CallCountTrigger::Eval(const string* fn, ...)
{
  if (! (g_libfi_enabled && g_libfi_CallCountTrigger_enabled)) {
    if (g_libfi_CallCountTrigger_verbose) cerr << "CallCountTrigger::Eval fn=" << *fn << ", false\r\n";
    return false;
  }
  ++callCount;
  // binary search? not useful for a reasonably small number of call counts
  for (vector<int>::iterator it = callCounts.begin(), itend = callCounts.end(); it != itend; ++it)
  {
    if (callCount == *it) {
      if (g_libfi_CallCountTrigger_verbose) cerr << "CallCountTrigger::Eval fn=" << *fn << ", true\r\n";
      return true;
    }
  }
  if (g_libfi_CallCountTrigger_verbose) cerr << "CallCountTrigger::Eval fn=" << *fn << ", false\r\n";
  return false;
}
