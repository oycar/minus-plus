@ifndef MPX_H
@define MPX_H
@include "assert.awk"

# // FIXME: What does filefuncs do?
@load "filefuncs"

# // Control Logging
@ifeq LOG 0
@undef LOG
@endif

# // Logic conventions
@define  TRUE   (1)
@define  FALSE  (0)

# // Output Streams
@define STDOUT "/dev/stdout"
@define STDERR "/dev/stderr"
@define DEVNULL "/dev/null"

# //
@define SHARED_ARRAYS "Account_Closed Account_Term Accounting_Cost Capital_Losses Carry_Offsets Cost_Basis Dividend_Date\
 Foreign_Offset_Limit Held_From Held_Until Income_Tax Leaf Lifetime Long_Gains Long_Losses Long_Name\
 Maturity_Date Method_Name No_Carry_Offsets Number_Parcels Parcel_Proceeds Parcel_Tag Parent_Name Price\
 Qualified_Units Refundable_Offsets Short_Gains Short_Losses Tax_Adjustments Tax_Bands Tax_Credits Tax_Losses\
 Taxable_Income Total_Units Underlying_Asset Units_Held "


@define SHARED_SCALARS "MPX_Version MPX_Arrays MPX_Scalars Document_Protocol\
 Document_Root Enforce_Names Enforce_Qualification EOFY_Window FY_Day FY_Length FY_Time\
 Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window\
 Start_Record ALLOCATED\
 Dividend_Qualification_Function Get_Taxable_Gains_Function\
 Gross_Up_Gains_Function Imputation_Report_Function\
 Income_Tax_Function Initialize_Tax_Function "

# // Some constants
# // @define CLEAR_ARRAY ("CLEAR_ARRAY")
@define DITTO ("^")
@define NUMBER_MONTHS (12)
@define HOUR (12)
@define CLOSING (16)
@define DATE_ERROR (-1)
@define BEFORE_EPOCH (-2)
@define PRECISION (2)
@define MAX_PRECISION (6)
@define CLASS_INDEX (1)
@define BUY ("BUY")
@define SELL ("SELL")

# // Default State Record - no time fields
@define TIME_FIELDS ("")

# // Constants for Real_Value keys
@define UNITS_KEY         (0)
@define BROKERAGE_KEY     (1)
@define LIFETIME_KEY      (1)
@define MATURITY_DATE_KEY (1)
@define TAX_CREDITS_KEY   (1)
@define LIC_DEDUCTION_KEY (2)

# // Default Cost Element
@define COST_ELEMENT ("II")

# // Output Date Formats
@define MONTH_FORMAT ("%Y %b %d") # // 2010 Jun 10
@define ISO_FORMAT   ("%F")       # // 2010-Jun-10
@define YEAR_FORMAT  ("%Y")       # // 2010
@define SHORT_FORMAT ("%Y %b")    # // 2010 Jun

# // Default Reports
@define ALL_REPORTS ("a:b:c:d:m:o:q:t:z")

# // Default Reports
@ifndef SHOW_REPORTS
@define   SHOW_REPORTS "bcot"
@endif # // SHOW_REPORTS

@define ternary(a, b, c) ((a)?(b):(c))
@define report_balance(s)      ternary(SHOW_REPORTS ~ /[bB]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_capital(s)      ternary(SHOW_REPORTS ~ /[cC]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_depreciation(s) ternary(SHOW_REPORTS ~ /[dD]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_imputation(s)   ternary(SHOW_REPORTS ~ /[iI]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_market(s)       ternary(SHOW_REPORTS ~ /[mM]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_operating(s)    ternary(SHOW_REPORTS ~ /[oO]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_dividend(s)     ternary(SHOW_REPORTS ~ /[qQ]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_tax(s)          ternary(SHOW_REPORTS ~ /[tT]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)

# // The stream to write reports to
@define eofy_stream(t)         ternary(!Show_FY || ((t) == Show_FY), STDERR, DEVNULL)

# // Default Asset Prefix for Price Lists
@define ASSET_PREFIX ("ASSET.CAPITAL.SHARES")
@define ASSET_SUFFIX ("ASX")

# // Start Defining Special Account Names
@define TAXABLE_GAINS ("SPECIAL.TAXABLE")
@define LONG_GAINS    ("SPECIAL.TAXABLE.GAINS.LONG")
@define LONG_LOSSES   ("SPECIAL.TAXABLE.LOSSES.LONG")
@define SHORT_GAINS   ("SPECIAL.TAXABLE.GAINS.SHORT")
@define SHORT_LOSSES  ("SPECIAL.TAXABLE.LOSSES.SHORT")
@define STAR          ("*")
@define star(s)       ternary(is_star(s), (s), (STAR (s)))
@define is_star(a)    ((a) ~ STAR)

# // The Epoch and minimum time difference
@define EPOCH_START        (2000)
@define EPOCH_END          (2999)
@define DELTA_T               (2)
@define MAX_TRANSACTIONS   (1000)
@define ONE_DAY           (86400)
@define ONE_HOUR           (3600)
@define ONE_WEEK    @eval (7 * ONE_DAY)
@define ONE_YEAR    @eval (366 * ONE_DAY)
@define CGT_PERIOD  @eval (366 * ONE_DAY)
# // Day Number For Feb 29
@define FEB29                (60)

# // Reserved Classes
@define RESERVED_CLASSES  /ASSET|EQUITY|EXPENSE|INCOME|LIABILITY|SPECIAL|BALANCING/

# // Useful inline functions - this may be overdoing it
@define make_array(array)  ternary(SUBSEP in array,TRUE,FALSE)

@define find_entry(array, now) ternary(__MPX_KEY__ = find_key(array, now), array[__MPX_KEY__], ternary(0 == __MPX_KEY__, array[0], 0))
@define first_entry(array) ternary(__MPX_KEY__ = first_key(array), array[__MPX_KEY__], ternary(0 == __MPX_KEY__, array[0], 0))

# // This is not efficient
@define get_delta(array, t) (find_entry(array, t) - find_entry(array, just_before(t)))

@define found_key  (__MPX_KEY__)
@define is_class(a, b) ((a) ~ ("^" (b) "[.:]"))
@define trim(s) (s)
#
# // Useful shorthands for various kinds of accounts
@define is_currency(a)  ((a) ~ /\.CURRENCY:/)
@define is_asset(a) ((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]/ || is_currency(a))
@define is_equity(a) ((a) ~ /^EQUITY[.:]/)
@define is_liability(a) ((a) ~ /^LIABILITY[.:]/)
@define is_cash(a) ((a) ~ /^ASSET\.CURRENT[.:]/)
@define is_unitized(a) ((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/ || is_currency(a))

# // Fixed asset
@define is_fixed(a) ((a) ~ /^ASSET\.FIXED[.:]/)
@define is_tax(a)  ((a) ~ /^(ASSET\.CURRENT|LIABILITY)\.TAX[.:]/)
@define is_term(a) ((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/)
@define is_current(a) ((a) ~ /^(ASSET|LIABILITY)\.CURRENT[.:]/)
@define is_capital(a) ((a) ~ /^ASSET\.CAPITAL[.:]/ || is_currency(a))
#
#
# // Contribution
@define is_contribution(a) ((a) ~ /^INCOME\.CONTRIBUTION[.:]/)
# // Benefit
@define is_benefit(a) ((a) ~ /^EXPENSE\.NON\-DEDUCTIBLE\.BENEFIT[.:]/)
#
# // Pension or Income Stream
@define is_stream(a) ((a) ~ /^LIABILITY\.MEMBER\.(PENSION|STREAM)[.:]/)
@define is_pension(a) ((a) ~ /^LIABILITY\.MEMBER\.PENSION[.:]/)

#
# // Match buy or sell transactions (pairs of accounts)
@define is_sale(t, a, b)  ((is_asset(a) && is_open(a, t)) || (is_equity(b) && is_open(b, t)))
@define is_purchase(a, b)  (is_asset(b) || is_equity(a))

# //
# // The last component of a name is the suffix
@define is_suffix(a, b) ((a) ~ ("[.:]" (b) "(_|$)"))

# // Reserved Tax Offset Classes
@define is_offset(a)      ((a) ~ /^SPECIAL\.OFFSET[.:]/)

# // Is a leaf name in a linked account format i.e. first component is
# // (DIV|DIST|FOR|GAINS).LEAF => LEAF
@define is_linked(a) ((Leaf[a]) ~ /^(DIV|DIST|FOR|GAINS)\./)

# // The current value of an asset
# // @define get_value(a, now) ternary(is_capital(a), find_entry(Price[a], now) * get_units(a, now), get_cost(a, now))

# // char code lookup
@define get_char(c) ternary(c in URL_Lookup, URL_Lookup[c], (0))

# //
@define is_individual (Journal_Type ~ /^IND$/)
@define is_smsf (Journal_Type ~ /^SMSF$/)
@define is_company (Journal_Type ~ /^(PTY|CORP|LTD)$/)
@define is_trust (Journal_Type ~ /^TRUST$/)
@define match_accounts(m,x,a,b) ternary(m=x,ternary(m=match_account(a, x), m, m=match_account(b, x)),m="")
@define set_entry(array, x, t) (array[t] = (x))

# // Rounding etc
@define near_zero(x) (less_than_or_equal(x, Epsilon) && greater_than_or_equal(x, -Epsilon))

# // Not zero
@define not_zero(x) (greater_than(x, Epsilon) || less_than(x, -Epsilon))

# // Positive?
@define above_zero(x) greater_than(x, Epsilon)

# // Yield x if x is positive, otherwise z
@define yield_positive(x, z) ternary(above_zero(x), (x), z)

# // Negative?
@define below_zero(x) less_than(x, -Epsilon)
# // Yield x if x is negative, otherwise z
@define yield_negative(x, z) ternary(below_zero(x), (x), z)

# // Round to zero
@define round_zero(x) ternary(near_zero(x), 0, x)

# // Numerical comparisons
@define greater_than(x, y) (((x) - (y)) > 0)
@define less_than(x, y)    (((x) - (y)) < 0)
@define greater_than_or_equal(x, y) (((x) - (y)) >= 0)
@define less_than_or_equal(x, y)    (((x) - (y)) <= 0)

@define account_closed(x, now) ((x) in Account_Closed && less_than(Account_Closed[x], (now)))
@define is_closed(a, now) (!is_open((a), (now)))

@define ever_held(a) (Held_From[a][0] > Epoch)
@define is_sold(a, p, now) (Held_Until[a][p] <= (now))
@define is_unsold(a, p, now) (Held_Until[a][p] > (now))
@define get_short_name(name) (Leaf[name])
@define get_adjusted_cost(a, now) (get_cost(a, now) - get_cost_adjustment(a, now))
@define get_element_cost(a, p, e, now) (find_entry(Accounting_Cost[a][p][e], (now)))
@define get_parcel_proceeds(a, p) (Parcel_Proceeds[a][p])
@define set_parcel_proceeds(a, p, x) (Parcel_Proceeds[a][p] = (x))


# // Get a single transaction from the account
@define get_delta_cost(a, now) (get_cost(a, now) - get_cost(a, just_before(now)))
@define get_latest_transaction(a, now) (find_key(Cost_Basis[a], now))
@define get_previous_transaction(a, now) (find_key(Cost_Basis[a], just_before(now)))

# // Units
@define get_units(a, now) ternary(a in Total_Units, find_entry(Total_Units[a], now), 0)
@define adjust_units(a, du, now) ternary(a in Total_Units, sum_entry(Total_Units[a], du, now), 0)

# // Qualified units - reading is simple - no window qualified units equal all units
@define get_qualified_units(a, now) ternary(Qualification_Window,  find_entry(Qualified_Units[a], now), get_units(a, now))

#// GST proportion at time (t)
@define gst_proportion(t) ternary(__MPX_H_TEMP__ = find_entry(GST_Rate,t), __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__), 0)

# //
# // Date macros
@define get_month_number(name) ternary((name) in Lookup_Month, Lookup_Month[name], 0)

@define leap_year(y) (((y) % 4 == 0 && (y) % 100 != 0) || (y) % 400 == 0)
@define next_year(t) ((t) + one_year(t,  1))
@define last_year(t) ((t) - one_year(t, -1))
@define get_year_number(t) (strftime("%Y", (t), UTC) + 0)
@define get_day_number(t)  (strftime("%j", (t), UTC) + 0)
@define YYMMDD_date(x) (substr((x), 1, 2) "-" substr((x), 3, 2) "-" substr((x), 5, 2))

@define show_date(t, format) ternary(format, (" " get_date(t, format)), "")

# //
# // The length of a year ending at time (y, d)
@define get_year_length(y, d) (ternary((d) <= FEB29, leap_year((y) - 1), leap_year(y)) + 365)
# //
# //
@define keys_in(array, key, subkey) ((key in array) && (subkey in array[key]))
@define set_key(array, key, value) ternary(key in array,value,array[key] = (value))
@define get_key(array, key) ternary(key in array, array[key], "")
@define delete_array(array, key) ternary(key in array, delete array[key], "")
@define ordered_pair(x, a, b) ternary(x[1] = a, x[2] = b, x[2] = b)
@define rational_value(x) ternary(x[2], (x[1]/x[2]), assert(FALSE, "Division by zero in rational fraction" x[1] "/" x[2]))
@define copy_pair(a,b) ((b[1] = a[1]) && (b[2] = a[2]))

@define set_scalar(var,x) (SYMTAB[var] = x)
@define abs_value(x) ternary((x) < 0, -(x),x)
@define max_value(x,y) ternary((x) - (y) > 0,x,y)
@define min_value(x,y) ternary((x) - (y) < 0,x,y)
@define accumulated_profits(t) (get_cost("*INCOME.CONTRIBUTION",t) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",t) - get_cost("*INCOME",t) - get_cost("*EXPENSE",t))

# // Control precise timings of costs
@define just_before(t) ((t) - 1)
@define just_after(t)  ((t) + 1)

# // Formatting
@define cap_string(s) ternary(s, (toupper(substr(s, 1, 1)) substr(s, 2)), (s))
@define add_field(s, field, c) ternary("" == s, field, ternary("" == field, s, (s c field)))

# // Include currency definitions
@include "currency.h"
@define carry_losses(a, t) ternary(t in a, first_entry(a[t]), 0)

# // Capital Loss Window
# // Unlimited goes all the way to the Epoch
# // The write back limit
@define write_back_limit(t) ternary(WRITE_BACK_LIMIT, (t - one_year(t, WRITE_BACK_LIMIT)), Epoch)

# // Multi-Line Macro
# // Gets the entries in the data which lie within the [block] (including end points)
@define filter_block(key, data, start, end) for (key in data) {\
  if (key - end > 0)\
    continue;\
  if (key - start >= 0)\
    stack[key] = data[key];\
  else\
    break;\
}

# // Print a block of n identical characters
@define print_block(c, n, stream) if (TRUE) {while (n-- > 1) printf "%1s", c > stream; print c > stream}
@define print_line(l, stream) ternary(l, underline(73, 8, stream), underline(47, 8, stream))

# // A local array
make_array(__MPX_ARRAY__)

# // This assumes that the file:// protocol is in use - for a network protocol this will not work - so assume document is available
@define document_missing(name) ternary("file://" == Document_Protocol, stat(name, __MPX_ARRAY__), FALSE)
@endif # !MPX_H
