#include "ExamineArgs.h"
#include <iostream>

extern int *examine_args_int[];
extern int examine_args_int_lastindex[];
extern char **examine_args_string[];
extern int examine_args_string_lastindex[];

extern u_int8_t g_libfi_enabled;
/*
** After editing this list of global vars, please update the
** triggers/exported_symbols_list file
*/
u_int8_t g_libfi_ExamineArgs_enabled = 1;
u_int8_t g_libfi_ExamineArgs_verbose = 0;

ExamineArgs::ExamineArgs()
    : argType(EXAMINE_ARG_UNDEFINED),
      argCompare(EXAMINE_COMPARE_UNDEFINED)
{
}

void ExamineArgs::Init(xmlNodePtr initData)
{
  xmlNodePtr nodeElement, textElement;

  if (g_libfi_ExamineArgs_verbose) cerr << "ExamineArgs::Init\r\n";
  nodeElement = initData->children;
  while (nodeElement)
  {
    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"skip"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type) {
          if (strcmp((char*)textElement->content, "int") == 0) {
              skipTypes.push_back(EXAMINE_ARG_INT);
          } else if (strcmp((char*)textElement->content, "string") == 0) {
              skipTypes.push_back(EXAMINE_ARG_STRING);
          } else if (strcmp((char*)textElement->content, "char") == 0) {
              skipTypes.push_back(EXAMINE_ARG_INT); // promoted to int
          } else {
              cerr << "BUMMER 1, what is this type: " << (char*)textElement->content << "\r\n";
          }
      }
    }

    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"argType"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type) {
          if (strcmp((char*)textElement->content, "int") == 0) {
              argType = EXAMINE_ARG_INT;
          } else if (strcmp((char*)textElement->content, "string") == 0) {
              argType = EXAMINE_ARG_STRING;
          } else if (strcmp((char*)textElement->content, "char") == 0) {
              argType = EXAMINE_ARG_INT; // promoted to int
          } else {
              cerr << "BUMMER 2, what is this type: " << (char*)textElement->content << "\r\n";
          }
      }
    }

    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"argCompare"))
    {
      textElement = nodeElement->children;
      if (XML_TEXT_NODE == textElement->type) {
          if (strcmp((char*)textElement->content, "equal") == 0) {
              argCompare = EXAMINE_COMPARE_EQUAL;
          } else if (strcmp((char*)textElement->content, "and") == 0) {
              argCompare = EXAMINE_COMPARE_AND;
          } else if (strcmp((char*)textElement->content, "strstr") == 0) {
              argCompare = EXAMINE_COMPARE_STRSTR;
          } else if (strcmp((char*)textElement->content, "strcmp") == 0) {
              argCompare = EXAMINE_COMPARE_STRCMP;
          } else {
              cerr << "BUMMER 3, what is this type: " << (char*)textElement->content << "\r\n";
          }
      }
    }

    if (XML_ELEMENT_NODE == nodeElement->type &&
      !xmlStrcmp(nodeElement->name, (const xmlChar*)"argBaseArrayChooser"))
    {
      textElement = nodeElement->children;
      argBaseArrayChooser = atoi((char *)textElement->content);
    }

    nodeElement = nodeElement->next;
  }
  if (argType == EXAMINE_ARG_UNDEFINED ||
      argCompare == EXAMINE_COMPARE_UNDEFINED) {
      cerr << "BUMMER, we are not properly configured!\r\n";
  }
}

bool ExamineArgs::Eval(const string* functionName, ...)
{
  bool retval = false;
  int i;

  if (g_libfi_enabled && g_libfi_ExamineArgs_enabled) {
      // Skip past zero or more arguments
      va_list ap;
      va_start(ap, functionName);
      for (vector<examine_arg_t>::iterator it = skipTypes.begin(),
           itend = skipTypes.end(); it != itend; ++it) {
          if (*it == EXAMINE_ARG_INT) {
              va_arg(ap, int);
          } else if (*it == EXAMINE_ARG_STRING) {
              va_arg(ap, char *);
          }
      }
      va_end(ap);

      // Check the current argument
      if (argType == EXAMINE_ARG_INT) {
          int examine;
          int *base = examine_args_int[argBaseArrayChooser];
          int last = examine_args_int_lastindex[argBaseArrayChooser];

          examine = va_arg(ap, int);
          for (i = 0; i <= last; i++) {
              if (argCompare == EXAMINE_COMPARE_EQUAL) {
                  retval = (base[i] == examine);
              } else if (argCompare == EXAMINE_COMPARE_AND) {
                  retval = (base[i] & examine);
              }
              if (retval) {
                  break;
              }
          }
      } else if (argType == EXAMINE_ARG_STRING) {
          char *examine;
          char **base = examine_args_string[argBaseArrayChooser];
          int last = examine_args_string_lastindex[argBaseArrayChooser];

          examine = va_arg(ap, char *);
          for (i = 0; i <= last; i++) {
              if (argCompare == EXAMINE_COMPARE_STRSTR) {
                  retval = strstr(examine, base[i]);
              } else if (argCompare == EXAMINE_COMPARE_STRCMP) {
                  retval = (strcmp(base[i], examine) == 0);
              }
              if (retval) {
                  break;
              }
          }
      }
  }

  if (g_libfi_ExamineArgs_verbose) cerr << "ExamineArgs::Eval fn=" << *functionName << ", " << retval << "\r\n";
  return retval;
}
