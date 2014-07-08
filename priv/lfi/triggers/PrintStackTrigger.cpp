
#include "PrintStackTrigger.h"
#include <string.h>
#include <execinfo.h>
#include <iostream>

extern u_int8_t g_libfi_enabled;
/*
** After editing this list of global vars, please update the
** triggers/exported_symbols_list file
*/
u_int8_t g_libfi_PrintStackTrigger_enabled = 1;
u_int8_t g_libfi_PrintStackTrigger_verbose = 0;

PrintStackTrigger::PrintStackTrigger()
{
}

void PrintStackTrigger::Init(xmlNodePtr initData)
{
  xmlNodePtr nodeElement, textElement;

  if (g_libfi_PrintStackTrigger_verbose) cerr << "PrintStackTrigger::Init\r\n";
  nodeElement = initData->children;
  while (nodeElement)
  {
    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"file"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type)
      {
        remove((char*)textElement->content);
        file = fopen((char*)textElement->content,"a");        
      }
    }
    nodeElement = nodeElement->next;
  }
}

bool PrintStackTrigger::Eval(const string* fn, ...)
{
  void *array[10];
    size_t size;

  if (g_libfi_enabled && g_libfi_PrintStackTrigger_enabled) {
    size = backtrace(array, 10);
    backtrace_symbols_fd(array, size, fileno(file));
    fclose(file);
    if (g_libfi_PrintStackTrigger_verbose) cerr << "PrintStackTrigger::Eval fn=" << *fn << ", true\r\n";
    return true;
  } else {
    if (g_libfi_PrintStackTrigger_verbose) cerr << "PrintStackTrigger::Eval fn=" << *fn << ", false\r\n";
    return false;
  }
}
