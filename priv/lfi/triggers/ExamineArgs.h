#include "../Trigger.h"

typedef enum {
    EXAMINE_ARG_INT = 1,
    EXAMINE_ARG_STRING = 2,
    EXAMINE_ARG_UNDEFINED = -1
} examine_arg_t;

typedef enum {
    EXAMINE_COMPARE_EQUAL = 50,     // int
    EXAMINE_COMPARE_AND = 51,       // int
    EXAMINE_COMPARE_STRSTR = 52,    // string
    EXAMINE_COMPARE_STRCMP = 53,    // string
    EXAMINE_COMPARE_UNDEFINED = -1
} examine_compare_t;

DEFINE_TRIGGER( ExamineArgs )
{
public:
  ExamineArgs();
  void Init(xmlNodePtr initData);
  bool Eval(const string* functionName, ...);
private:
  vector<examine_arg_t> skipTypes;
  examine_arg_t argType;
  examine_compare_t argCompare;
  int argBaseArrayChooser;
};
