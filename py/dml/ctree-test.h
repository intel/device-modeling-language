/*
  © 2013 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
*/

#include <math.h>
#include <float.h>
#include <simics/util/help-macros.h>

// Hack: juggle with macros and function prototypes to capture DMLC-generated
// assert failures and critical errors without crashing
#define assert_error capture_assert_error
static void capture_assert_error(int line, const char *file,
                                 const char *mod_date, const char *message);
static void VT_critical_error(const char *short_msg, const char *long_msg);

#include <simics/base-types.h>
#include <simics/dmllib.h>

api_function_t SIM_get_api_function(const char *name) { return NULL; }

#define IS_UINT32(x) (sizeof(x) == 4 && ((x) | INT_MIN) > 0)
#define IS_INT64(x)  (sizeof(x) == 8 && ((x) | LLONG_MIN) < 0)
#define IS_UINT64(x) (sizeof(x) == 8 && ((x) | LLONG_MIN) > 0)
#define IS_DOUBLE(x) (sizeof(x) == 8 && ((typeof(x))0.5 == 0.5))

static bool failure = false;
static const char *py_context;

static UNUSED void
expect(bool cond, const char *desc, const char *file, int line)
{
        if (!cond) {
                fprintf(stderr, "%s\n", py_context);
                fprintf(stderr, "%s:%d: error: Expectation failed: %s\n",
                        file, line, desc);
                failure = true;
        }
}

static bool capturing_assert_errors;
static int captured_assert_errors;
static void
capture_assert_error(int line, const char *file,
             const char *mod_date, const char *message)
{
        if (capturing_assert_errors) {
                ++captured_assert_errors;
        } else {
                fprintf(stderr, "%s\n%s:%d: error: assertion failed: %s\n",
                        py_context, file, line, message);
                exit(1);
        }
}

static void
VT_critical_error(const char *short_msg, const char *long_msg)
{
        if (capturing_assert_errors) {
            ++captured_assert_errors;
        } else {
            fprintf(stderr, "%s\ncritical error: %s\n", py_context, long_msg);
            exit(1);
        }
}

#define EXPECT(x) expect(x, #x, __FILE__, __LINE__)
