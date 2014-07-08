/*
     Created by Scott Lystig Fritchie
     Copyright (C) 2013 Basho Technologies, Inc.

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

*/

/*
** If you wanted to have a trigger than was on or off on a per-trigger
** basis, the LFI-way to do that is to have a class private instance
** variable that would be specified in an XML trigger config.  You
** wouldn't be able to change it at runtime, but it'd be very easy to
** implement.
**
** If you want the ability to enable & disable individual triggers, as
** Faulterl does, then you need to be able to have a global symbol
** that dlsym(3) can see ... but C+ class instance variables aren't
** visible to dlsym(3).  So we compromise here.
**
** Example XML specification:
**
**   <trigger id="switchpanel_4" class="SwitchPanel">
**     <args>
**       <index>4</index>
**     </args>
**   </trigger>
**
** And then if we want to enable/disable the 'switchpanel_4' trigger
** instance, then we would write a 1/0 into the array
** g_libfi_SwitchPanel_array at index 4.
*/

#include "SwitchPanel.h"
#include <stdio.h>
#include <iostream>

extern u_int8_t g_libfi_enabled;
/*
** After editing this list of global vars, please update the
** triggers/exported_symbols_list file
*/
u_int8_t g_libfi_SwitchPanel_enabled = 1;
u_int8_t g_libfi_SwitchPanel_verbose = 1;

#ifndef TRIGGER_SWITCHPANEL_SIZE
#define TRIGGER_SWITCHPANEL_SIZE 50
#endif
u_int8_t  g_libfi_SwitchPanel_array[TRIGGER_SWITCHPANEL_SIZE];

SwitchPanel::SwitchPanel()
    : index(0)
{
}

void SwitchPanel::Init(xmlNodePtr initData)
{
  xmlNodePtr nodeElement, textElement;
  time_t t;

  if (g_libfi_SwitchPanel_verbose) cerr << "SwitchPanel::Init\r\n";
  nodeElement = initData->children;
  while (nodeElement)
  {
    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"index"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type) {
        index = atoi((char*)textElement->content);
        if (g_libfi_SwitchPanel_verbose) cerr << "SwitchPanel::Init: index=" << index << "\r\n";
      }
    }
    nodeElement = nodeElement->next;
  }
}


bool SwitchPanel::Eval(const string* fn, ...)
{
  bool retval;

  if (g_libfi_enabled && g_libfi_SwitchPanel_enabled) {
      retval = g_libfi_SwitchPanel_array[index];
  } else {
    retval = false;
  }

  if (g_libfi_SwitchPanel_verbose) cerr << "SwitchPanel::Eval fn=" << *fn << ", array[" << index << "] = " << retval << "\r\n";
  return retval;
}
