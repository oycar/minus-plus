@ifndef MPX_H
@define MPX_H
@include "assert.awk"


# // Control Logging
@ifeq LOG 0
@undef LOG
@endif

# // Control Export format
@ifeq EXPORT_FORMAT 0
@undef EXPORT_FORMAT
@endif

# // Logic conventions
@define  TRUE   (1)
@define  FALSE  (0)

# // Output Streams
@define STDOUT "/dev/stdout"
@define STDERR "/dev/stderr"
@define DEVNULL "/dev/null"

# //
@define SHARED_ARRAYS "Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Leaf_Count \
Lifetime Long_Name Maturity_Date Method_Name Number_Parcels \
Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands \
Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held "

@define SHARED_SCALARS "MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length \
FY_Time Journal_Currency Journal_Title Journal_Type Last_Time Qualification_Window ALLOCATED \
Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function "

# // Some constants
@define DITTO ("^")
@define NUMBER_MONTHS (12)
@define HOUR (12)
@define CLOSING (16)
@define DATE_ERROR (-1)
@define BEFORE_EPOCH (-2)
@define PRECISION (2)
@define MAX_PRECISION (6)
@define CLASS_INDEX (1)

# // Default Import Values
@define KEY_FIELD  (1)
@define VALUE_FIELD (2)
@define KEY_DATE @eval (TRUE)
@define VALUE_DATE @eval (FALSE)

# // Output Date Formats
@define MONTH_FORMAT ("%Y %b %d") # // 2010 Jun 10
@define ISO_FORMAT   ("%F")       # // 2010-Jun-10

# // Default Reports
@define ALL_REPORTS ("a:b:c:d:f:m:o:q:t:z")

# // Default Reports
@ifndef SHOW_REPORTS
@define   SHOW_REPORTS "bcot"
@endif # // SHOW_REPORTS

@define report_balance(s)   ternary(SHOW_REPORTS ~ /[bB]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_capital(s)   ternary(SHOW_REPORTS ~ /[cC]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_deferred(s)  ternary(SHOW_REPORTS ~ /[dD]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_fixed(s)     ternary(SHOW_REPORTS ~ /[fF]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_market(s)    ternary(SHOW_REPORTS ~ /[mM]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_operating(s) ternary(SHOW_REPORTS ~ /[oO]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_dividend(s)  ternary(SHOW_REPORTS ~ /[qQ]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)
@define report_tax(s)       ternary(SHOW_REPORTS ~ /[tT]|[aA]/ && SHOW_REPORTS !~ /[zZ]/, s, DEVNULL)

# // Default Asset Prefix for Price Lists
@define ASSET_PREFIX ("ASSET.CAPITAL.SHARES")
@define ASSET_SUFFIX ("ASX")

# // The Epoch and minimum time difference
@define EPOCH_START        (2000)
@define EPOCH_END          (2999)
@define DELTA_T               (2)
@define MAX_TRANSACTIONS   (1000)
@define ONE_DAY           (86400)
@define ONE_HOUR           (3600)
@define ONE_WEEK    @eval (7 * ONE_DAY)
@define ONE_YEAR    @eval (366 * ONE_DAY)
@define CGT_PERIOD    @eval (366 * ONE_DAY)
# // Day Number For Feb 29
@define FEB29                (60)
@define FY_DATE        ("Jul-01")

# // Reserved Classes
@define RESERVED_CLASSES  /ASSET|EQUITY|EXPENSE|INCOME|LIABILITY|SPECIAL/

# // Useful inline functions - this may be overdoing it
@define ternary(a, b, c) ((a)?(b):(c))
@define make_array(array)  ternary(SUBSEP in array,TRUE,FALSE)

@define add_field(s, field) ternary("" == s, field, ternary("" == field, s, (s ", " field)))
@define find_entry(array, now) ternary(__MPX_KEY__ = find_key(array, now), array[__MPX_KEY__], ternary(0 == __MPX_KEY__, array[0], 0))
@define found_key  (__MPX_KEY__)
@define is_class(a, b) ((a) ~ ("^" (b) "[.:]"))
#
# // Useful shorthands for various kinds of accounts
@define is_asset(a) ((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]/)
@define is_equity(a) ((a) ~ /^EQUITY[.:]/)
@define is_liability(a) ((a) ~ /^LIABILITY[.:]/)
@define is_cash(a) ((a) ~ /^ASSET\.CURRENT[.:]/)
@define is_unitized(a) ((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)

# // Fixed asset
@define is_fixed(a) ((a) ~ /^ASSET\.FIXED[.:]/)
@define is_tax(a)  ((a) ~ /^(ASSET\.CURRENT|LIABILITY)\.TAX[.:]/)
@define is_term(a) ((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/)
@define is_current(a) ((a) ~ /^(ASSET|LIABILITY)\.CURRENT[.:]/)
@define is_capital(a) ((a) ~ /^ASSET\.CAPITAL[.:]/)
# //
# // The last component of a name is the suffix
@define is_suffix(a, b) ((a) ~ ("[.:]" (b) "(_|$)"))

# // Reserved Tax Offset Classes
@define is_offset(a)      ((a) ~ /^SPECIAL\.OFFSET[.:]/)
@define is_franking(a)    ((a) ~ /^SPECIAL\.OFFSET\.FRANKING[.:]/)
@define is_foreign(a)     ((a) ~ /^SPECIAL\.OFFSET\.FOREIGN[.:]/)
@define is_no_carry(a)   ((a) ~ /^SPECIAL\.OFFSET\.NO_CARRY[.:]/)
@define is_carry(a)      ((a) ~ /^SPECIAL\.OFFSET\.CARRY[.:]/)
@define is_refund(a)     ((a) ~ /^SPECIAL\.OFFSET\.REFUNDABLE[.:]/)
@define is_franking_deficit(a) ((a) ~ /^SPECIAL\.OFFSET\.FRANKING_DEFICIT[.:]/)

# // Is a leaf name in a linked account format i.e. first component is
# // (DIV|DIST|FOR).LEAF => LEAF
@define is_linked(a) ((a) ~ /^(DIV|DIST|FOR)\./)


# // char code lookup
@define get_char(c) ternary(c in URL_Lookup, URL_Lookup[c], (0))



# //
@define is_star(a) ((a) ~ /^*/)
@define is_individual (Journal_Type ~ /^IND$/)
@define is_smsf (Journal_Type ~ /^SMSF$/)
@define is_company (Journal_Type ~ /^(PTY|CORP|LTD)$/)
@define is_trust (Journal_Type ~ /^TRUST$/)
@define match_accounts(m,x,a,b) ternary(m=x,ternary(m=match_account(a, x), m, m=match_account(b, x)),m="")
@define set_entry(array, x, now) (array[(now)] = (x))

# // Rounding etc
@define near_zero(x) (((x) <= Epsilon) && ((x) >= -Epsilon))

# // Not zero
@define not_zero(x) (((x) > Epsilon) || ((x) < -Epsilon))

# // Positive?
@define above_zero(x) ((x) >  Epsilon)
# // Yield x if x is positive, otherwise z
@define yield_positive(x, z) ternary((x) >   Epsilon, (x), z)

# // Negative?
@define below_zero(x) ((x) < -Epsilon)
# // Yield x if x is negative, otherwise z
@define yield_negative(x, z) ternary((x) < - Epsilon, (x), z)

# // Round to zero
@define round_zero(x) ternary(near_zero(x), 0, x)

# // Numerical comparisons
@define greater_than(x, y) ((x - y) > 0)
@define less_than(x, y)    ((x - y) < 0)
@define greater_than_or_equal(x, y) ((x - y) >= 0)
@define less_than_or_equal(x, y)    ((x - y) <= 0)

@define is_closed(a, now) (!is_open((a), (now)))
@define is_new(a) ("" == first_key(Cost_Basis[a]))
@define ever_held(a) (Held_From[(a)][0] > Epoch)
@define is_sold(a, p, now) (Held_Until[(a)][(p)] <= (now))
@define is_unsold(a, p, now) (Held_Until[(a)][(p)] > (now))
@define get_short_name(name) (Leaf[(name)])
@define get_reduced_cost(a, now) (get_cost(a, now))
@define get_parcel_tax_adjustment(a, p, element, now) (find_entry(Tax_Adjustments[a][p][element], (now)))
@define get_parcel_proceeds(a, p) (first_entry(Accounting_Cost[a][p][0]))

# // Get a single tranaction from the account
@define get_delta_cost(a, now) (get_cost(a, now) - get_cost(a, just_before(now)))
@define get_latest_transaction(a, now) (find_key(Cost_Basis[a], now))
@define get_previous_transaction(a, now) (find_key(Cost_Basis[a], just_before(now)))

# // Units
@define get_units(a, now) find_entry(Total_Units[a], now)
@define adjust_units(a, du, now) sum_entry(Total_Units[a], du, now)

# // Qualified units - reading is simple - no window qualified units equal all units
@define get_qualified_units(a, now) ternary(Qualification_Window,  find_entry(Qualified_Units[a], now), get_units(a, now))

#// GST proportion at time (t)
@define gst_proportion(t) ternary(__MPX_H_TEMP__ = find_entry(GST_Rate,t), __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__), 0)

# //
# // Date macros
@define get_month_number(name) ternary((name) in Lookup_Month, Lookup_Month[name], 0)

@define leap_year(y) (((y) % 4 == 0 && (y) % 100 != 0) || (y) % 400 == 0)
@define next_year(t) ((t) + one_year(t,  1))
@define last_year(t) ((t) + one_year(t, -1))
@define get_year_number(t) (strftime("%Y", (t), UTC) + 0)
@define get_day_number(t)  (strftime("%j", (t), UTC) + 0)
@define YYMMDD_date(x) (substr((x), 1, 2) "-" substr((x), 3, 2) "-" substr((x), 5, 2))

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
@define max_value(x,y) ternary((x)>(y),x,y)
@define min_value(x,y) ternary((x)<(y),x,y)
@define accumulated_profits(t) (get_cost("*INCOME.CONTRIBUTION",t) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",t) - get_cost("*INCOME",t) - get_cost("*EXPENSE",t))

# // Control precise timings of costs
@define just_before(t) ((t) - 1)
@define just_after(t)  ((t) + 1)

# // Include currency definitions
@include "currency.h"


# // Capital Loss Window
# // Unlimited goes all the way to the Epoch
# // No need to compute the number of years exactly
# // The carry forward and write back limits
@define carry_forward_limit(t) ternary(CARRY_FORWARD_LIMIT, ((t) - CARRY_FORWARD_LIMIT), Epoch)
@define write_back_limit(t) ternary(WRITE_BACK_LIMIT, ((t) - WRITE_BACK_LIMIT), Epoch)

# // Multi-Line Macro
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

# // These two are not very readable
# // @define get_cash_in(a, i, now) (ternary((now >= Held_From[(a)][(i)]), find_entry(Accounting_Cost[(a)][(i)][I], Held_From[(a)][(i)]), 0))
# // @define get_cash_out(a, i, now) (ternary(is_sold(a, i, (now) + 1), find_entry(Accounting_Cost[(a)][(i)][0], (now)),0))
@endif # !MPX_H
