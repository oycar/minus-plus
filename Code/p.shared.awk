# p.shared.awk
# Copyright (C) 2018  Robert Whitehurst
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http:#www.gnu.org/licenses/>.

# Some useful awk functions
# for time sorted arrays

# Include header
@include "mpx.h"

# get the most relevant ex-dividend date
function get_exdividend_date(a, now,   value, key, discrepancy) {

  # We start at the time "now" in the accounts
  # Which should be equal to or shortly after the
  # payment date - now since  the
  # payment date must be after the qualifying date
  # search back to find the earlier entries
  # since Dividend_Date[ex_dividend_date] => now-ish
  if (a in Dividend_Date) {

    # Get the most recent payment date
    value = find_entry(Dividend_Date[a], now)
    discrepancy = now - value

    # The value cannot be later than the current time "now"
    if (below_zero(discrepancy)) {
      Read_Date_Error = "Payment date is later than current date"
      return ternary(Enforce_Qualification, DATE_ERROR, FALSE)

    } else if (near_zero(discrepancy))
      return found_key

    # Some times dividends are paid out of order, for example
    # a special or buyback dividend might have an extra
    # long qualification period - so look ahead more dividends
    # until the discrepancy increases
    #
    key = found_key
    while (key) {
      value = now - find_entry(Dividend_Date[a], just_before(key))
      if (greater_than(value, discrepancy))
        # A worse match
        break

      # A better match
      discrepancy = value
      if (near_zero(discrepancy))
        # A good match
        return found_key

      # Save  this match
      key = found_key
    }

    # Best match was key
    if (greater_than(discrepancy, ONE_WEEK)) {
      Read_Date_Error = "Failed to find a payment date within one week of current date"
      return ternary(Enforce_Qualification, DATE_ERROR, FALSE)
    }

    # Return it
    return key
  }

  # Failed to find a qualification date
  # Is enforcement strict?
  Read_Date_Error = "Failed to find any payment date"
  return ternary(Enforce_Qualification, DATE_ERROR, FALSE)
}

# read csv records
function read_csv_records(use_csv) {
  # Set the separator
  if (use_csv)
    FPAT = "([^,]*)|(\"[^\"]+\")"
  else
    FPAT = "([^[:space:]]+)|(\"[^\"]+\")|(\\[[^]]+\\])|(#(.)*)"
}

# Abstract read value out
function read_value(x,   value) {
  # Get the value first
  value = read_date(x)
  if (DATE_ERROR != value)
    # Treat as a date
    # This can return BEFORE_EPOCH
    return value

  # Is it a number?
  if (x ~ /^[0-9\.\-\+]+$/)
    return make_decimal(x)

  # Strip quotation marks if any
  return gensub(/^"([^\"]+)"$/, "\\1", 1, x)
}

# A somewhat generalized variable reading function
function read_state(name, adjust_value, first_field, last_field,    i, x, value) {
  # The end of the input
  if (first_field > last_field)
    return "" # None read

  # The fields represent the keys & value
  value = read_value($last_field)

  # Logging
@ifeq LOG read_state
  printf "%s => ", name > STDERR
@endif

  # Is this a scalar?
  if (first_field == last_field) {
    # Yes
    if (adjust_value)
      SYMTAB[name] += adjust_value * value
    else
      SYMTAB[name]  = value

@ifeq LOG read_state
      # Logging
      printf " %s\n", value > STDERR
@endif

  } else {
    # The rest of the keys
    for (i = first_field; i < last_field; i ++)
      # The code can minimize output file by
      # retaining the old key if the "ditto"
      # symbol is encountered;
      if (($i != DITTO) || !(i in Variable_Keys))
        Variable_Keys[i] = read_value($i)

    # Set the array value
    set_array(SYMTAB[name], Variable_Keys, first_field, last_field - 1, value, adjust_value, FALSE)

@ifeq LOG read_state
    # Print the whole array out
    walk_array(SYMTAB[name], 1, STDERR)
@endif
  }

  return value
}

# Delete an array
function clear_array(array) {
  delete array
}

# Set a multi-dimensional array value
# This assumes array is correctly defined
function set_array(array, keys, first_key, last_key, value, adjust_value, flag) {
  # The idea of deleting the a temporary scalar entry in this function was based on
  # Ed Morton's code found here => https://groups.google.com/forum/#!topic/comp.lang.awk/vKiSODr6Bds
  # Catch errors
@ifeq LOG DEBUG
  assert(isarray(array), "set_array: Not an array")
@endif

  # Delete temporary key
  if (flag) {
    delete array[SUBSEP]   # delete scalar element
    flag = FALSE
  }

  # Set the array recursively
  if (first_key == last_key) {
    # Set the value
    if (adjust_value)
      sum_entry(array, adjust_value * value, keys[first_key])
    else
      set_entry(array, value, keys[first_key])
  } else {
    # Yikes
    # We need to ensure the subarray exists before calling set_array() otherwise
    # inside set_array() the entry would be a scalar, but then we need to delete
    # a[][] inside set_array() before trying to create a[][][] because
    # creating a[][] below creates IT as scalar. SUBSEP used arbitrarily.
    if (!((keys[first_key] in array) && (SUBSEP in array[keys[first_key]]))) {
      array[keys[first_key]][SUBSEP]   # create array a[] + scalar a[][]
      flag = TRUE
    }

    # Recursively set the array elements
    set_array(array[keys[first_key]], keys, first_key + 1, last_key, value, adjust_value, flag)
  }
}

# Output all the data associated with the accounts to the specified data file
function write_state(array_names, scalar_names,    name) {
  # The array data
  # Keep track of keys written out
  for (name in array_names) {
    make_array(Key_Index)
    printf "<<%s%s\n", OFS, array_names[name] > Write_State
    walk_array(SYMTAB[array_names[name]], 1, Write_State)
    printf ">>\n" > Write_State
    delete Key_Index
  }

  # The scalars - compact form
  for (name in scalar_names)
    printf "<<%s%s%s%s%s>>\n", OFS, scalar_names[name], OFS, format_value(SYMTAB[scalar_names[name]]), OFS > Write_State
}

# This walks the array that we want to dump to file
function walk_array(arr, level, stream,    key, last_key, i) {
  # Start at level 1 by default
  level = ternary(level, level, 1)

  # write to output stream
  stream = ternary(stream, stream, Write_State)

  # Construct an output string
  # which will be key_1, key_2, ...., key_n, value
  # if a key is duplicated in the following line
  # abbreviate with the ditto mark '^' if DITTO is defined
  for (key in arr) {
    # Record keys
    # last_key is needed for the deepest level
    if (!(level in Key_Index) || (key != Key_Index[level]))
      last_key = Key_Index[level] = key
    else # Optionally use a DITTO mark
      last_key = ternary(DITTO, DITTO, key)

    # Go down one level or print deepest level
    if (isarray(arr[key]))
      walk_array(arr[key], level + 1, stream)
    else {
      # Finished at the base level
      # The output string
      for (i = 1; i < level; i++) {
        printf "%s%s", Key_Index[i], OFS > stream
        if (DITTO)
          Key_Index[i] = DITTO
      }

      # Complete the output - use special key for deepest level
      printf "%s%s%s\n", last_key, OFS, format_value(arr[key]) > stream
    }
  }
}

# Return either a string or floating point number
function format_value(v) {
  # Force a floating point format if  this is a floating point number
  if (v ~ /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?./) {
    # Catch some rounding issues
    if (near_zero(v))
      v = 0
    else # force floating point format
      v = sprintf("%.*f", MAX_PRECISION, v)
  }

  # Return the formatted value
  # (\"[^\"]+\")
  # Protect internal white space with quotes if necessary
  if (v ~ /[[:space:]]/ && v !~ /\"[^\"]+\"/)
    return ("\"" v "\"")
  return v
}

function print_copyleft() {
  CopyLeft = "mpx - command line accounting and financial reporting\n\
Copyright (C) 2018  Robert Whitehurst\n\
\n\
This program is free software: you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation, either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program.  If not, see <http:#www.gnu.org/licenses/>.\n"
  printf CopyLeft
}

# Sort arrays
function sort_arrays_on(type,    save_sorted) {
  if ("sorted_in" in PROCINFO)
    save_sorted = PROCINFO["sorted_in"]
  else
    save_sorted = ""

  PROCINFO["sorted_in"] = type # or whatever

  return save_sorted
}

# Restore old behaviour
function sort_arrays_off(old_sort) {
  PROCINFO["sorted_in"] = old_sort
}

# Get the key associated with entry "now"
# This returns  the first satisfactory match found
function find_key(array, now,    key, x) {
@ifeq LOG DEBUG
  assert(isarray(array), "<" $0 "> find_key:  needs an array")
@endif

  # This version checks all keys equal to now
  for (key in array) {
    x = key - now # Force numeric comparison
    if (x <= 0) # satisfactory - first hit
      return key
  }

  # If the array is empty returns ""
  return ""
}

# The first key
function first_key(array,    key) {
@ifeq LOG DEBUG
  assert(isarray(array), "<" $0 "> first_key:  needs an array")
@endif
  for (key in array)
    return key
  # Empty array
  return ""
}

# delete duplicated values
function delete_duplicate_entries(array,      k, j, v, w) {
@ifeq LOG DEBUG
  assert(isarray(array), "<" $0 "> delete_duplicate_entries:  needs an array")
@endif
  for (j in array) {
    v = array[j]
    k = find_key(array, j - 1)
    if ("" == k) # Last key
      return
    w = array[k]

    # Look for duplicates
    if (near_zero(v - w))
      delete array[j]
  }

  # All done
}

# Add an entry to a time-ordered array
# This gives a running total
function sum_entry(array, x, now,   key, delta) {
@ifeq LOG DEBUG
  assert(isarray(array), "<" $0 "> sum_entry:  needs an array")
@endif

  # Add the new value to the previous sum
  for (key in array) {
    delta = key - now # Force numeric comparison
    if (delta <= 0) # satisfactory the first key not after now
      break

    # This key is after now so adjust it
    array[key] += x
  }

  # All done
  array[now] = x + array[key]
}

# Remove the oldest entries from the reverse ordered array
function remove_entries(array, x,     key, delta) {
@ifeq LOG DEBUG
    assert(isarray(array), "<" $0 "> remove-entries:  needs an array")
@endif
  # If current sum > x  then sum -= x
  # If current sum <=x  then sum = 0
@ifeq LOG get_gains
  printf "Remove Entries: Adjust => %f\n", x > STDERR
@endif

  for (key in array) {
    delta = array[key] - x
    if (above_zero(delta)) {
@ifeq LOG get_gains
      printf "\t%s %f => %f\n", get_date(key), array[key], delta > STDERR
@endif
      array[key] = delta
    } else {
@ifeq LOG get_gains
      printf "\t%s Delete %f\n", get_date(key), array[key] > STDERR
@endif

      # Set negligible & negative entries to zero?
      array[key] = 0

      # Remove negligible and negative entries
      #delete array[key]
    }
  }

  # Return TRUE if any entries were removed that were actually negative
  return below_zero(delta)
}

function copy_entries(array, target_array, key) {
  for (key in array)
    target_array[key] = array[key]
}

# Remove all keys before limit
function remove_keys(array, limit,   key, removed_value) {
  removed_value = ""

  # Get each key
  for (key in array) {
    if (removed_value)
      delete array[key]
    else {
      # Force numeric comparison
      if (less_than(key, limit)) {
        # The first key before the limit
        # Save the first trimmed value
        removed_value = array[key]
        delete array[key]
      }
    }
  } # All keys processed

  # If removed value is set rebase all entries
  if (removed_value)
    # Correct the remaining values
    remove_entries(array, removed_value)

  # Return the adjustment
  return removed_value
}

# A function to find the maximum value in a bracketed window
function maximum_entry(array, start_bracket, end_bracket,
                                   key, max) {
  # This range excludes the endpoints (start_bracket, end_bracket)
  max = -1 # Searches for positive values

  # Get the first key
  key = find_key(array, just_before(end_bracket))
  while (key > start_bracket) {
    # Save the maximum found
    max = max_value(max, array[key])

    # Get the next key
    key = find_key(array, just_before(key))
  } # Done

  # return the maximum value
  return max
}

# Some extras
# Clear global values ready to read a new input record
function new_line(  key) {
  # Clear real values
  for (key in Real_Value)
    Real_Value[key] = 0

  Extra_Timestamp = DATE_ERROR
  Parcel_Name = ""
  Parcel_Order = "FIFO"
  Tax_Adjustment = FALSE
  Cost_Element = COST_ELEMENT # The default value
  Automatic_Depreciation = FALSE
  GST_Claimable = 0
  Depreciation_Type = ""
  Comments = ""

  Account[1] = Account[2] = ""
  Transaction_Currency = FALSE
  Translation_Rate = 1
}

# Accounting format printing
function print_cash(x,   precision) {
  precision = ternary("" == precision, PRECISION, precision)

  if ("" == x)
    return "  -  "
  if (near_zero(x))
    x = 0
  else if (x < 0)
    return sprintf("(%'.*f)", precision, -x)
  return sprintf(" %'.*f ", precision, x)
}

# Possible record styles
# Journal styles (date format is somewhat flexible)
#      2017 Aug 24, AMH.DIV, AMH.ASX, 2703.96, 3072, [1025.64,] [1655.49], # DRP & LIC
function parse_line(now,    i, j, x, number_accounts) {

  #
  # The record may be
  #     Double Entry => two accounts
  #     Single Entry => one account
  #  or No Entry => zero accounts
  #  Get next one or two fields
  for (i = 1; i < 3 && i < NF; i ++) {
    # The field id
    j = i + 1

    # Initialize this account - Screen out document strings and comments and numbers...
    if ($j !~ /^[\[#]/ && $j !~ /^[0-9\.\-]+$/)
      Account[i] = initialize_account($j)
  }

  # Check these out
  if ("" == Account[2]) {
    if ("" == Account[1])
      number_accounts = 0
    else
      number_accounts = 1
  } else { # Account[2] not empty
    assert(Account[1], "<" $0 "> Unexpected syntax, was " $2 " an account name?")
    assert(Account[2] != Account[1], "<" $0 "> Unexpected syntax, two identical accounts")
    number_accounts = 2
  }

  # Nature of  the record
  # i == 3 => Double Entry
  # i == 2 => Single Entry
  # i == 1 => Zero Entry
  i = number_accounts + 1

  #
  # The amount
  # Must exist if number accounts == 2
  if (2 == number_accounts) {
    i ++

    # The amount - usually in the default currency
    # BUT may be prefixed by an ISO 4217 Currency Code (Three Uppercase Characters.)
    amount = $i
    if ($i ~ /^[[:upper:]]{3}$/) {
      # Explicit currency given
      # Just pretend it is defined...
      initialize_account("ASSET.CURRENCY:" $i)
      Transaction_Currency = Long_Name[$i]

      # Is this a simple currency translation -
      j = i + 1
      assert($j ~ /^[0-9\.\-]+$/, "<" $0 "> Unexpected syntax: cash amount <" $j "> is not a number")

      # is this a purchase or a sale of currency units?
      if (get_currency(Account[2]) == Transaction_Currency)
        Real_Value[BUY_FOREX_KEY] = $i = make_decimal($j)  # A purchase (of forex)
      else if (get_currency(Account[1]) == Transaction_Currency && is_open(Account[1], now)) {
        $i = make_decimal($j) # A sale
        Real_Value[SELL_FOREX_KEY] = $j = - $i # A sale (of forex)
      } else { # Other cases
        # Rebuild line - remove currency code and shuffle down fields
        for (j = i; j < NF; j ++)
          $j = $(j + 1)
        NF --
      }

      # Get translation price
      if (Transaction_Currency in Price)
        Translation_Rate = find_entry(Price[Transaction_Currency], now)
      else
        # No price is an error
        assert(FALSE, "No exchange rate available for <" Transaction_Currency "> at <" get_date(now) ">")

    } else {
      # Use default currency
      Transaction_Currency = FALSE
      Translation_Rate = 1
    }

    # Check that this is a number
    amount = make_decimal($i)

    # Zero j
    j = ternary(Real_Value[BUY_FOREX_KEY] || Real_Value[SELL_FOREX_KEY], -1, 0)
  } else
    j = 1

  # From now on the fields are context dependent
  # Possibilities are:
  # * **Field 5**
  #   * **The Units** in a buy or sell
  #   * **The Cost Element** in any transaction
  #   * **Anything in Field 6 except LIC Deduction**

  # * **Field 6**
  #   * **Brokerage** in a buy or sell transaction
  #   * **Anything in Field 7**

  # * **Field 7**
  #   * **LIC (listed investment company) Deduction** in an income transaction
  #   * **Depreciation method** in a depreciating asset purchase
  #   * **A Parcel Date** in a buy or sell transaction
  #   * **Anything in Field 8**

  # * **Field 8**
  #   * **An Ex-Dividend Date** in an income transaction
  #   * **A GST Tag** in an income, expense, buy or sell transaction. If brokerage is present the GST is applied only on the brokerage.
  #   * **Anything in Field 9**

  # * **Field 9**
  #   * **A Parcel Name** in a dividend reinvestment -
  #   * **Anything in Field 10**

  # * **Field 10** and higher
  #   * **A document name** in any transaction
  #   * **A comment** in any transaction

  # Next field
  i ++
  while (i <= NF) {
    # Set x
    if (j <= 3)
      x = parse_optional_value($i)
    else # if j > 3
      x = 0

    # Shared code - save x if set
    if (x) { # x is not zero or ""
      # The zeroth case can be Units
      # If there is a non default transaction currency
      if (0 >= j) {
        if (above_zero(x) && is_purchase(Account[1], Account[2])) {
          # Interpret these as units
          Real_Value[UNITS_KEY] = x
        } else if (below_zero(x) && is_sale(now, Account[1], Account[2])) {
          Real_Value[UNITS_KEY] = x
        } else # This is not Units so it is the next possible real value, index 1
          Real_Value[j = 1] = x
      } else
        Real_Value[j] = x
    } else if ("" != x) { # Will be "" when a timestamp set
      # x not set so look for strings
      # Can reuse x
      x = parse_optional_string($i, TRUE)

      # Treat as a comment
      if (x)
        Comments = add_field(Comments, x, OFS)
    }

    # Increment i & j
    i ++
    j ++
  }

  # Comments should be signified with an octothorp
  if (Comments !~ /^#/)
    Comments = add_field("# ", Comments, OFS)

  # Documents can be added as comments
  # Some special document names are supported
  # So for example [B] expands to ABC Buy YYYY Mon
  for (x in Documents) {
    delete Documents[x]

    # Parse this document name
    i = parse_document_name(x, now)

    # Add the parsed name to the comments
    Comments = add_field(Comments, i, OFS)
  }

  # All done - return record type
  return number_accounts
}


# A document name may contain a filetype suffix
function parse_document_name(name, now,    prefix, suffix, account_name, array, seps, suffix_set, use_format) {

  # Looks for special strings accompanied by a literal string
  #
  #   [B:literal] => Account Buy  (or Sell) YYYY Mon(literal)
  #   [S:literal] => Account Sell (or Buy)
  #   [H:literal] => Account Holding Statement
  #   [I:literal]
  #   [E:literal]
  #   [:literal] => prepends the date
  #   [literal:] => appends the date
  #   [:]        => just the date
  #   [literal]  => just the literal
  #
  #   The code character "+" can be used instead in which case one extra month is added to the date -
  #   This is useful if the holding statement is in the next month
  #
  #  If the colon is needed in a string literal a different Document_Shortcut code can be set in the Journal file
  #
  # <<, Document_Shortcut, =,>>
  # 2008 Jun 30, INCOME.FOREIGN:FOR.PXUPA.ASX,          CASH,          0,      726.63, [PX:UPA Distribution=], # PX:UPA distribution
  # <<, Document_Shortcut, :,>>

  # The YYYY Mon format is standard
  use_format = SHORT_FORMAT

  # Split the code name
  # Use name component because we want to capture all the components apart from the first
  if (split(name, array, Document_Shortcut, seps) > 1) {
    suffix = get_name_component(name, 2, -1, array)
    suffix_set = TRUE

    # What was the seperator?
    if ("+" == seps[1])
      # Use year format
      use_format = YEAR_FORMAT
  } else
    suffix_set = FALSE

  #
  prefix = get_name_component(name, 1, 1, array)
  account_name = ""
    #
    switch (prefix) {
      case "B" :
      case "S":
      case "H":

        # Is this a buy, sell or holding statement?
        if (Real_Value[UNITS_KEY] < 0) {
          prefix = ternary("H" == prefix, "Holding Statement", "Sell")
          account_name = get_name_component(Leaf[Account[1]], 1)
        } else {
          prefix = ternary("H" == prefix, "Holding Statement", "Buy")
          account_name = get_name_component(Leaf[Account[2]], 1)
        }

        # Add the date
        prefix = prefix show_date(now, use_format)
      break;;

      case "D": # Distribution
        if (is_linked(Account[1]))
          account_name = get_name_component(Leaf[Account[1]], 2)
        else
          account_name = get_name_component(Leaf[Account[1]], 1)

        # The second component of the account name (unless this is accrued income)
        if (is_class(Account[1], "ASSET.CURRENT.ACCRUED"))
          prefix = "Distribution" show_date(now, use_format)
        else
          prefix = tolower(get_name_component(Account[1], 2)) show_date(now, use_format)
        break;;
      case "I":  # Income
        #account_name = get_name_component(Leaf[Account[1]], 1)

        # The second component of the account name is not used here...?
        prefix = "Income" show_date(now, use_format)
        break;;

      case "C":
      case "E": # Expense or Cost
        #account_name = get_name_component(Leaf[Account[2]], 1)

        # The second component of the account name is not used here...?
        prefix = "Expense" show_date(now, use_format)
        break;;

      case "T": # Annual Tax Statement
        account_name = get_name_component(Leaf[Account[2]], 1)
        prefix = "Annual Tax Statement" show_date(now, YEAR_FORMAT)
        break

      default: # no match - assume this is a literal string
        # When a distinct suffix is present add the date
        if (suffix_set)
          prefix = add_field(prefix, show_date(now, use_format))

        account_name = ""
        break
    } # End of switch

    # We have at this point
    #
    #  prefix => Type of transaction, eg Buy, Dividend etc
    #  account_name => BHP, AUS_BOND etc
    #  suffix => Literal string
    #
    #  Or
    #
    #  prefix => ""
    #  account_name => ""
    #  suffix => Literal string
    #

    # Is there an account name?
    if ("" != account_name)
      prefix = account_name " " cap_string(prefix)

    # Final parsed document name
    prefix = add_field(prefix, suffix)

    # Return either urlencoded version or the parsed name
    # The parsed name indicates that the document is missing
    return url_document_name(prefix)
}

# Get a version of a document name that can be used as a url
function url_document_name(string,   filename, filetype, z, i, n) {

  # How many dotted fields?
  n = split(string, z, ".")

  # If more than one last is treated as filetype
  if (n > 1)
    filetype = url_encode(z[n])
  else # Default filetype
    filetype = Document_Filetype

  # Is this document missing?
  if (document_missing(Document_Root string "." filetype))
    # No such document - leave unconverted - this indicates that file is missing
    return ("[[" string "]]")

  # Process  the filename elements
  filename = url_encode(z[1])
  for (i = 2; i < n; i ++)
    filename = filename "." url_encode(z[i])

  # return result
  return (Document_URI filename "." filetype)
}

# Parse optional value
function parse_optional_value(field,     value) {
  # Parse the optional field 'field'
  #  a timestamp => Extra_Timestamp, 0
  #  a numerical value => value
  #  a string or empty string => 0

  # Is it an empty string?
  if ("" == field)
    return 0 # Nothing to set

  # Is it a comment or document?
  if (field ~ /^[#\[]/)
    return 0

  # Now - is it a timestamp?
  # If the timestamp is already set it cannot be reset
  if (DATE_ERROR == Extra_Timestamp) {
    Extra_Timestamp = read_date(field)

    # The timestamp interpretation takes precedence
    if (DATE_ERROR != Extra_Timestamp)
      # If a time-stamp is set this will return ""
      return ""
  }

  # Next - is it a numerical value?
  value = make_decimal(field)

  # This is a value
  if (near_zero(value))
    value = 0

  # Returning the value
  return value
}

# Parse optional string
# By default do not save any document found
function parse_optional_string(field, save_document,    string, adjustment_flag) {
  # Interpret cost element
  #
  #  A blank or zero
  #    Cost Base element II (the default)
  #
  #  A string  (roman numbers I to V are meaningful but could be anything without brackets)
  #    I   => Cost Base element I
  #    II  => Cost Base element II etc
  #
  #  A bracketed string
  #    (I) => Tax Adjustment to Cost Base element I etc
  #


  ## Check for cost element first
  # Brackets?
  if (field ~ /^\((.)+\)$/) {
    # bracketed
    len = length(field)
    field = toupper(substr(field, 2, len - 2))
    if (field ~ /D/) {
      Tax_Adjustment = Automatic_Depreciation = TRUE
      Cost_Element = I
      return ""
    }

    # This is probably a tax adjustment
    adjustment_flag = TRUE
  } else # Not a tax adjustment
    adjustment_flag = FALSE

  # This should be a text flag
  switch (field) {
    # First check for a GST tag
    case "FULL-GST" :
    case "FULL_GST" :
    case "GST" : GST_Claimable = 1.0 # GST_Proportion
                 return ""

    case "REDUCED-GST" :
    case "REDUCED_GST" :
    case "PART-GST" :
    case "PART_GST" :
      GST_Claimable = Reduced_GST # * GST_Proportion
      return ""

    # Now check for a depreciation type
    case "DV" :
    case "PC" :
    case "POOL" :
      Depreciation_Type = field
      return ""

    # Cost element
    case "I" :
    case "II" :
    case "III" :
    case "IV" :
    case "V" : # Cost elements
      Tax_Adjustment = adjustment_flag
      Cost_Element = field
      return ""

    default: # this is an optional string
      break # no-op
  }

  # The string can be a document name enclosed in square brackets
  string = gensub(/(^\[)([[:print:]]+)(\]$)/, "\\2", 1, field)

  if (string == field) {
    # Parcel names are enclosed by single quotes
    if (field ~ /^"([[:print:]])+"$/) {

      # Catch special instructions FIFO, LIFO, MSTX, LSTX
      if (field ~ /^"(FIFO|LIFO|MSTX|LSTX)"$/) {
        Parcel_Order = substr(field, 2, 4) # Fixed length order tag
        Parcel_Name  = ""
      } else {
        Parcel_Order = "FIFO"
        Parcel_Name  = field # With quotes
      }
      
      return ""
    }

    # Otherwise a comment ...
    return string
  }

  # A document name if we get here - save this
  # An empty string means the input string is [] => use default string
  # First version - replicate earlier behaviour
  if ("" != string && "" != save_document)
    # Can only have one example of each unique string...
    if (!(string in Documents))
      Documents[string] = string

  # All done
  return ""
}

# Initialize special accounts
function set_special_accounts() {
  # Built in accounts are required for EOFY statements
  #
  # Check & Set use special accounts to trigger actions
  #
  initialize_account("SPECIAL.CONTROL:BALANCE")
  initialize_account("SPECIAL.CONTROL:COST")
  initialize_account("SPECIAL.CONTROL:UNITS")
  initialize_account("SPECIAL.CONTROL:VALUE")
  initialize_account("SPECIAL.CONTROL:PRICE")

  # Balancing - to simplify processing of transactions at EOFY
  # These are income/expense items not needed in the operating statement
  ADJUSTMENTS      = initialize_account("BALANCING:ADJUSTMENTS")

  ## Franking Credits
  #
  FRANKING          = initialize_account("SPECIAL.FRANKING:FRANKING") # The Franking account balance
  FRANKING_PAID     = initialize_account("SPECIAL.FRANKING:FRANKING.PAID") # Disbursed
  FRANKING_STAMPED  = initialize_account("SPECIAL.FRANKING:FRANKING.STAMPED") # Received through net tax paid

  # Taxable capital gains are in special accounts
  # Make sure the parent accounts exist
  initialize_account(LONG_GAINS  ":LONG.GAINS")
  initialize_account(LONG_LOSSES ":LONG.LOSSES")
  initialize_account(SHORT_GAINS ":SHORT.GAINS")
  WRITTEN_BACK   =   initialize_account(SHORT_LOSSES ":SHORT.LOSSES")

  # The DEPRECIATION account
  DEPRECIATION = initialize_account("EXPENSE.DEPRECIATION:DEPRECIATION")

  # When a depreciating asset is sold any profit or loss is booked as income/expense to these accounts
  SOLD_APPRECIATION = initialize_account("INCOME.APPRECIATION:APPRECIATION.SOLD")
  SOLD_DEPRECIATION = initialize_account("EXPENSE.DEPRECIATION:DEPRECIATION.SOLD")

  # When a foreign exchange asset is sold any profit or loss is booked as income/expense to these accounts
  FOREX_INCOME  =  initialize_account("INCOME.FOREX:FOREX.GAINS")
  FOREX_EXPENSE =  initialize_account("EXPENSE.FOREX:FOREX.LOSSES")

  # Built in TAX accounts - debtor like
  WITHOLDING   = initialize_account("ASSET.CURRENT.TAX:TAX.WITHOLDING")
  PAYG         = initialize_account("ASSET.CURRENT.TAX:TAX.PAYG")

  # Built in TAX accounts - creditor like
  TAX          = initialize_account("LIABILITY.TAX:TAX")
  GST          = initialize_account("LIABILITY.TAX:TAX.GST")

  # Not a Current Account
  DEFERRED     = initialize_account("LIABILITY.DEFERRED:DEFERRED.TAX")

  # Accounting capital gains accounts
  REALIZED_GAINS  = initialize_account("INCOME.GAINS.REALIZED:GAINS")
  REALIZED_LOSSES = initialize_account("EXPENSE.LOSSES.REALIZED:LOSSES")
  UNREALIZED      = initialize_account("EXPENSE.UNREALIZED:MARKET.CHANGES")

  # Extra capital gains accounts which can be manipulated independently of asset revaluations
  INCOME_LONG        = initialize_account("INCOME.GAINS.LONG.SUM:INCOME.LONG")
  INCOME_SHORT       = initialize_account("INCOME.GAINS.SHORT:INCOME.SHORT")
  EXPENSE_LONG       = initialize_account("EXPENSE.LOSSES.LONG:EXPENSE.LONG")
  EXPENSE_SHORT      = initialize_account("EXPENSE.LOSSES.SHORT:EXPENSE.SHORT")
}

# Get date
function get_date(time, format) {
  # Get the date string
  format = ternary("" == format, DATE_FORMAT, format)

  # ok - if format is zero use timestamp
  if (0 == format)
    return time
  if (time == Future)
    return "     -     "
  return strftime(format, time, UTC)
}

# Absolute hour for today, tomorrow and yesterday
function today(time, hour,    midnight) {
  hour = ternary("" == hour, HOUR, hour)

  # Need to get midnight local time
  midnight = ONE_DAY * int(time / ONE_DAY)

  return midnight + ONE_HOUR * hour
}

function tomorrow(time, hour) {
  hour = ternary("" == hour, HOUR, hour)

  return today(time, hour) + ONE_DAY
}

function yesterday(time, hour) {
  hour = ternary("" == hour, HOUR, hour)

  return today(time, hour) - ONE_DAY
}

# Length of year ending / starting (now)
function one_year(now, sense,     year, day, sum) {
  # By default one year backward!
  sense = ternary(!sense, -1, sense)
  # Sense > 0 is forward sense years
  # Sense < 0 is back sense years

  # Get the day number
  day = get_day_number(now)

  # Which Calendar year is this?
  year = get_year_number(now)
  if (sense > 0)
    year += sense
  else
    sense = - sense

  # Go back n years
  sum = 0
  while (sense -- > 0) {
    sum += get_year_length(year, day)
    year --
  }

  # Get the length in seconds
  return ONE_DAY * sum
}

# Useful account filters
function is_open(a, now,     p) {
  # An asset is open if there are unsold parcels at time 'now'
  if (is_unitized(a))
    for (p = 0; p < Number_Parcels[a]; p ++) {
      if (greater_than(Held_From[a][p], now))
        break
      if (is_unsold(a, p, now))
        return TRUE
    }
  return FALSE
}

# Is an account a an ancestor of another account b?
function is_ancestor(a, b,    p) {
  if (!is_star(a))
    return FALSE

  # Check
  p = Parent_Name[b]
  while (STAR != p) {
    if (a == p) # Found
      return TRUE
    p = Parent_Name[p]
  }

  # Not found
  return FALSE
}

# The last parcel sold before "now" - assumes an asset is open
function held_to(ac, now,     p, latest_sale) {
  # When an asset was held to
  latest_sale = Epoch

  for (p = 0; p < Number_Parcels[ac]; p++)
    if (is_sold(a, p, now))
      latest_sale = max_value(latest_sale, Held_Until[ac][p])
    else if (Held_Until[ac][p] < Future) {
      # If the asset still held now?
      latest_sale = now
      break # We are finished because it cannot be set later than now
    }

  return latest_sale # returns the date the parcel was sold
}

# This splits up a branch name or a leaf name into dotted components
function get_name_component(name, i, number_components, array,    name_length, s, dot) {
  # Just get one component
  if ("" == number_components)
    number_components = 1 # Get one word by default

  # Sometimes the array of components is already available
  if (!isarray(array))
    # Split the input name
    name_length = split(name, array, "[.:]")
  else
    name_length = length(array)

  # Convert i to modulo name_length
  # So i == 0 gets the last component
  if (name_length > 0)
    i = i % name_length
  if (i <= 0)
    i += name_length

  # Get the first word - if number_components < 0 will continue to end
  s = dot = ""
  while (i <= name_length && number_components -- != 0) {
    s = s dot array[i ++]
    dot = "."
  }

  # All done - add a line of code for debugging/trapping
  # assert("" != s, sprintf("Requested component <%d> couldn't be found in %s", i, name))
  return s
}

# Does a name match an account OR an account prefix
function match_account(a, show_name) {
  # Empty account?
  if (!a)
    return FALSE

  # A particular account matches
  if (show_name == a)
    return a

  # If the class name is another account it does not match
  if (show_name in Leaf)
    return FALSE

  # Otherwise is this an account prefix?
  if (is_class(a, show_name))
    return ("*" show_name)
}

# Basic transactions
# Adjust the cost base (including the adjusted and reduced cost bases)
function adjust_cost(a, x, now, tax_adjustment,     i, adjustment, flag) {
  # This argument should always be provided - using a global variable to pass information is confusing
  tax_adjustment = ternary("" == tax_adjustment, Tax_Adjustment, tax_adjustment)

  # if an asset is adjusted note the adjustment in
  # each active parcel
  # Adjustments for units bought
  if (is_unitized(a)) {

    # What proportion of the sum is allocated to each unit at time now?
@ifeq LOG adjust_cost
    printf "%s\n", a > STDERR
    printf "\tTimeStamp => %s\n", get_date(now) > STDERR
    printf "\tInitial Units => %.3f\n", get_units(a, now) > STDERR
    printf "\tCurrent Total Cost   => %s\n", print_cash(get_cost(a, now)) > STDERR
    printf "\tCost Base Adjustment => %s\n", print_cash(x) > STDERR
    printf "\tCost Element         => %s\n", Cost_Element > STDERR
@endif # LOG
    # Either divide adjustment between all open parcels OR
    # concentrate with a parcel with the same timestamp
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (greater_than(Held_From[a][i], now)) # All further transactions occured after (now)
        break # All done
      if (Held_From[a][i] == now) {
        if (!keys_in(Parcel_Tag, a, i) || (Parcel_Name == Parcel_Tag[a][i])) {
          # The adjustment is pooled explicitly with this parcel
          adjust_parcel_cost(a, i, now, x, Cost_Element, tax_adjustment)

@ifeq LOG adjust_cost
          # Debugging
          printf "\tCurrent Total Cost   => %s\n", print_cash(get_cost(a, now)) > STDERR
@endif # LOG
          # Also record the parents cost
          update_cost(a, x, now)
          return # Only one parcel is adjusted - it must be unsold if only just purchased
        }
      }
    }

    # The cost adjustment per unit except for depreciating assets
    if (flag = is_fixed(a))
      adjustment = x / get_cost(a, now)
    else {
      assert(get_units(a, now), "Asset <" Leaf[a] "> has zero units - ensure this transaction occurs before it was sold")
      adjustment = x / get_units(a, now)
    }

    # Debugging
@ifeq LOG adjust_cost
    printf "\tAverage Adjustment Per Unit  => %s\n",
      print_cash(x / get_units(a, now)) \
       > STDERR
@endif # LOG

    # Scan back down the parcels held and unsold at time now
    while (i -- > 0) {
      if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
        # The parcel adjustment is proportional to the parcel size unless it is a depreciating asset
        if (flag)
          adjust_parcel_cost(a, i, now, get_parcel_cost(a, i, now) * adjustment, Cost_Element, tax_adjustment)
        else
          adjust_parcel_cost(a, i, now, Units_Held[a][i] * adjustment, Cost_Element, tax_adjustment)
    } # End of each parcel

    # Debugging
@ifeq LOG adjust_cost
    printf "\tCurrent Total Cost   => %s\n", print_cash(get_cost(a, now)) > STDERR
@endif # LOG
  } else if (a in Cost_Basis)
    # This is the corresponding account
    sum_entry(Cost_Basis[a], x, now)
  else
    Cost_Basis[a][now] = x

  # Balance costs
  update_cost(a, x, now)
}

# Update the cost of the parent account
function update_cost(a, x, now,      p) {
  # Now get the parent to this account
  p = Parent_Name[a]
  if (STAR == p)
    return # Finished

  # Update the cost
  if (p in Cost_Basis)
    sum_entry(Cost_Basis[p], x, now)
  else
    Cost_Basis[p][now] = x

  # Logging
@ifeq LOG update_cost
  printf "\tp => %16s x => %11s Date => %11s sum => %11s\n", p, print_cash(x), get_date(now), print_cash(get_cost(p, now)) > STDERR
@endif # LOG
  update_cost(p, x, now)
}

function adjust_parcel_cost(a, p, now, parcel_adjustment, element, adjust_tax,
                            parcel_cost,
                            held_time) {
  # Ignore negligible adjustments
  if (near_zero(parcel_adjustment))
    return

@ifeq LOG adjust_cost
  printf "%s\n", a > STDERR
  printf "\tTimeStamp => %s\n", get_date(now) > STDERR
  printf "\t\tParcel  => %05d\n", p  > STDERR
  printf "\t\tParcel Cost         => %s\n", print_cash(get_parcel_cost(a, p, now))  > STDERR
  printf "\t\tParcel Tax Adjusted => %s\n", print_cash(get_parcel_cost(a, p, now, TRUE))  > STDERR
  printf "\t\tParcel Adjustment   => %s\n", print_cash(parcel_adjustment) > STDERR
  printf "\t\tAdjust Tax          => %s\n", ternary(adjust_tax, "TRUE", "FALSE")
  printf "\t\t\tOpening Element Cost[%s]=> %s\n", element, print_cash(get_element_cost(a, p, element, now)) > STDERR
@endif # LOG

  # save the cost adjustment/reduction related to this parcel
  # if a tax adjustment a negative cost reduces the reduced cost but leaves the adjusted cost unaltered
  # but if the cost is positive then the cost increases the adjusted cost and does not change the reduced cost
  if (adjust_tax) {
    # There are two types of adjustments
    # One is to adjust for tax paid - so parcel_adjustment < 0,
    #   here this is real money already transacted, and the tax adjustment is negative
    #   The reduced cost is decreased and the adjusted cost is unchanged
    # The other is to adjust for tax not yet paid and parcel adjustment > 0
    #   here the tax adjustment is still negative but the accounting adjustment is zero
    if (above_zero(parcel_adjustment))
      # A tax adjustment for an undeductible but legitimate expense
      sum_entry(Tax_Adjustments[a][p], - parcel_adjustment, now)
    else { # A tax adjustment for deferred tax or depreciation &c
      sum_entry(Accounting_Cost[a][p][element], parcel_adjustment, now)
      sum_entry(Tax_Adjustments[a][p], parcel_adjustment, now)
    }
  } else
    # Update the accounting cost
    sum_entry(Accounting_Cost[a][p][element], parcel_adjustment, now)

  # Equities do not have tax adjustments and can indeed have a negative cost base
  # but check instead in the EOFY processing....
  if (!is_equity(a)) {
    #   Ensure that the parcel cost base is not negative
    parcel_cost = get_parcel_cost(a, p, now)
    if (below_zero(parcel_cost)) {
@ifeq LOG adjust_cost
  printf "\t\t\tNegative Parcel Cost Base [%05d] => %s\n", p, print_cash(parcel_cost) > STDERR
@endif # LOG

      # Get the tax adjustment - this will influence the taxable gains
      parcel_adjustment = find_entry(Tax_Adjustments[a][p], now)

      # If the overall parcel cost is (P)
      # and if the cost of this element is now (E)
      # then the cost of the other elements is (P-E)
      # so that an overall zero parcel cost is achieved if this element has cost (E-P)
      sum_entry(Accounting_Cost[a][p][element], - parcel_cost, now)

      # This will create a capital gain
      adjust_cost(REALIZED_GAINS, parcel_cost, now)

@ifeq LOG adjust_cost
  printf "\t\t\tRealized Gains => %s\n",  print_cash(-parcel_cost) > STDERR
@endif # LOG

      # The capital gain needs to be balanced in the asset sums
      update_cost(a, -parcel_cost, now)

      # Update tax adjustment too
      if (less_than(parcel_adjustment, parcel_cost))
        sum_entry(Tax_Adjustments[a][p], parcel_cost, now)
      else {
        parcel_cost -= parcel_adjustment

        # This tax adjustment has been used
        set_entry(Tax_Adjustments[a][p], 0, now)
@ifeq LOG adjust_cost
        printf "\t\t\tTaxable Gains => %s\n",  print_cash(-parcel_cost) > STDERR
@endif # LOG
        # Need to record taxable gains/losses too
        held_time = get_held_time(now, Held_From[a][p])
        if (greater_than_or_equal(held_time, CGT_PERIOD)) {
          if (!(a in Long_Gains))
            Long_Gains[a] = initialize_account(LONG_GAINS ":LG." Leaf[a])
          adjust_cost(Long_Gains[a], parcel_cost, now)
        } else {
          if (!(a in Short_Gains))
            Short_Gains[a] = initialize_account(SHORT_GAINS ":SG." Leaf[a])
          adjust_cost(Short_Gains[a], parcel_cost, now)
        }

        # Balance taxable gains
        adjust_cost("*SPECIAL", - parcel_cost, now)
      }
    }
  }

@ifeq LOG adjust_cost
  # Debugging
  printf "\t\t\tClosing Element Cost[%s] => %s\n", element, print_cash(get_element_cost(a, p, element, now)) > STDERR
  printf "\t\tClosing Parcel Cost => %s\n", print_cash(get_parcel_cost(a, p, now))  > STDERR
  printf "\t\tClosing Tax Adjusted => %s\n", print_cash(get_parcel_cost(a, p, now, TRUE))  > STDERR
  printf "\t\tTotal Realized Gains => %s\n", print_cash(- get_delta_cost(REALIZED_GAINS, now))  > STDERR
@endif # LOG
} # End of adjust_parcel_cost

# The idea of the "cost" of the account
# This is the same as the reduced cost
# Returns 0 for sold assets
# What would happen if REALIZED were not populated and it returned the gains/losses for sold assets?
#
function get_cost(a, now,     i, sum_cost) {
  # Adjustments for units bought
  if (is_unitized(a)) {
    # Initial cost
    sum_cost = 0

    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (greater_than(Held_From[a][i], now)) # All further transactions occured after (now)
        break # All done
      if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
        sum_cost += sum_cost_elements(Accounting_Cost[a][i], now) # cost elements
    }
    return sum_cost
  } else if (a in Cost_Basis) # Cash-like
    return find_entry(Cost_Basis[a], now)

  return 0
}

# One liner function
function get_value(a, now) {
  return ternary(is_capital(a), find_entry(Price[a], now) * get_units(a, now), get_cost(a, now))
}


# The tax adjustments at time (now)
# Note that depreciation is always a tax adjustment
function get_cost_adjustment(a, now,   i, sum_adjustments) {
  # Initial adjustments
  sum_adjustments = 0

  # Adjustments for units bought
  # Do not apply to equities
  if (is_asset(a)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (greater_than(Held_From[a][i], now)) # All further transactions occured after (now)
        break # All done
      if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
        sum_adjustments += find_entry(Tax_Adjustments[a][i], now)
    }
  }

  return sum_adjustments
}

# # set the cost to a specified value (new_cost)
# function set_cost(a, new_cost, now,     initial_cost) {
#   # The current cost
#   initial_cost = get_cost(a, now)
#
#   # The required change in the cost is therefore (new_cost - initial_cost)
#   adjust_cost(a, new_cost - initial_cost, now, FALSE)
# }

# Get unrealized or realized gains
function get_asset_gains(gains_function, now,   sum, a) {
  sum = 0

  # Just sum the lower level function
  for (a in Leaf)
    sum += @gains_function(a, now)

  # All done - negative values are gains
  return sum
}

# Get unrealized gains at the account level - these are reduced gains
function get_unrealized_gains(a, now,
                              gains) {

  # The asset must be active
  if (is_closed(a, now))
    return 0 # No unrealized gains

  if (is_capital(a))
    gains = get_cost(a, now) - find_entry(Price[a], now) * get_units(a, now)
  else
    gains = 0

  # The result
  return gains
}

# Get realized gains at the parcel level
function get_realized_gains(a, now,
                              gains, i) {
  # The asset must be active
  if (is_open(a, now))
    return 0 # No realized gains

  # Must be a capital asset
  if (is_capital(a)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (greater_than(Held_From[a][i], now)) # All further transactions occured after (now)
        break # All done
      if (is_sold(a, i, now)) # This is a sold parcel at time (now)
        gains += get_parcel_proceeds(a, i) + sum_cost_elements(Accounting_Cost[a][i], now) # All cost elements
    }
  } else
    gains = 0

  return gains
}

# Sum only the cost elements
function sum_cost_elements(array, now,     sum_elements, e) {
@ifeq LOG DEBUG
  assert(isarray(array), "<" $0 "> sum_cost_elements:  needs an array")
@endif

  sum_elements = 0
  for (e in array) # Exclude element [0]
    sum_elements += find_entry(array[e], now)
  return sum_elements
}

# The initial cost
function get_cash_in(a, i, now) {

  # Is the account open?
  if (greater_than_or_equal(now, Held_From[a][i]))
    # Yes - always element I
    return get_element_cost(a, i, I, Held_From[a][i]) # The Held_From time ensures  that later element I costs do not impact the result

  # No - so no activity
  return 0
}

# The cost reductions
function get_cost_modifications(a, p, now,  sum) {
  # This should exclude cash_in and cash_out
  sum = sum_cost_elements(Accounting_Cost[a][p], now) # No I

  # Think about edge effects
  return sum - get_cash_in(a, p, now)
}

# A shorthand - ignores final cost
function get_parcel_cost(a, p, now, adjusted,    sum) {
  # Reduced cost by default
  sum = sum_cost_elements(Accounting_Cost[a][p], now) # No element 0
  if (adjusted)
    sum -= find_entry(Tax_Adjustments[a][p], now)

  # The parcel cost
  return sum
}

# Print out transactions
# Generalize for the case of a single entry transaction
function print_transaction(now, comments, a, b, amount, element_string, fields, n_fields,     matched, i, string) {
  if (greater_than(now, Stop_Time))
    return

  # Are we matching particular accounts?
  match_accounts(matched, Show_Account, a, b)
  if (Show_Account && !matched)
    return

  # First the date
  string = sprintf("%11s", get_date(now))

  # Is it not zero entry?
  if ("" != a)
    # At least single entry
    string = string sprintf("%s %13s ", OFS, Leaf[a])

  # Is it double entry?
  if ("" != b)
    string = string sprintf("%13s%s ", Leaf[b], OFS)

  # Amount  and cost element and or units - if at least one entry
  if (a || b) {
    string = string sprintf("%11.2f%s ", amount, OFS)
    if (element_string && element_string != COST_ELEMENT)
      string = string sprintf("%10s", element_string)
    else # Pretty print
      string = string sprintf("%10s", "")

    # Do we need to show the balance?
    if (matched)
      # From the start of the ledger
      string = string sprintf("%s %14s", OFS, print_cash(ternary(is_currency(matched), get_units(matched, just_after(now)), get_cost(matched, just_after(now)))))
    else
      # Optional Fields
      for (i = 1; i <= n_fields; i ++)
        string = string OFS " " fields[i]
  }

  # All done
  print string OFS " " comments
} # End of printing a transaction

function initialize_account(account_name,    class_name, array, p, n,
                                                         leaf_name, linked_name) {
  # We need to add code to recognize an account
  # On first use it will have to be initialized
  # This involves a long name which must have a first name component (class) which is one of:
  # Reserved_Classes =>
  #   ASSET
  #   CASH
  #   EQUITY
  #   EXPENSE
  #   INCOME
  #   LIABILITY
  #   SPECIAL
  #
  #   So an account_name passed in must be one of:
  #     1) An initialized long name (long name in Leaf)
  #     2) A short name of a used long name (long name in Leaf and short name in Long_Name)
  #     3) A short name of an as yet unused long name (long name in Leaf but short name not in Long_Name)
  #     4) An uninitialized long name (long name not in Leaf and first component in Reserved_Classes)
  #     5) Neither a long name nor a short name at all - an optional string

  # The leaf name can be reused

  # The first time this is called the "account_name" is the long name
  # On subsequent calls it can be either the long name or  the short name
  #
  if (account_name in Leaf)
    # A long name and already initialized - nothing to do
    return account_name

  # Either an uninitialized long name OR a short name
  if (account_name in Long_Name)
    return Long_Name[account_name]

  # Special cases exist which could mean this is not an uninitialized long name
  # Is this a new long name or an optional string?
  account_name = parse_optional_string(account_name)
  if ("" == account_name) # An optional string
    return "" # Not an account name

  # Still need to check the account name is in a recognized class
  class_name = get_name_component(account_name, CLASS_INDEX)

  # Initialize this account
  # Now split the account name into a branch and a leaf
  if (class_name ~ RESERVED_CLASSES) {
    assert(2 == split(account_name, array, ":"), sprintf("<%s> Account name %s is not in branch_name:leaf_name format", $0, account_name))
    leaf_name = array[2]
  } else {
    assert(!Enforce_Names, sprintf("<%s> Account name %s is not defined", $0, account_name))
    leaf_name = account_name
  }

  # Finally an uninitialized long name
  # BUT there is another trap;
  # the case of a miss-spelt name?
  # eg Leaf[A.B.C:D] => D
  #    Long[D] => A.B.C:D
  # but we passed in account_name => "A.B.Z:D"?
  #
  # Actually let's allow this - but only in very restricted casee of the account never having been used
  # This might be possible some day...
  # if (class_name ~ RESERVED_CLASSES)
  #   leaf_name = array[2]
  # else
  #   leaf_name = account_name
  if ((leaf_name in Long_Name)) {
    if (Leaf[Long_Name[leaf_name]] == leaf_name) {
      # If the existing account is new (unused) it can be deleted
      assert(!(Long_Name[leaf_name] in Cost_Basis), sprintf("Account name %s: Leaf name[%s] => %s is already taken", account_name, leaf_name, Long_Name[leaf_name]))

      # Must be a new (unused) name
      delete Long_Name[leaf_name]
    }
  }

  # Save leaf name
  Leaf[account_name] = leaf_name

  # Save long name
  Long_Name[leaf_name] = account_name
  if (Show_Account == leaf_name)
    Show_Account = account_name

  # Set extra items needed for ASSET or EQUITY class accounts
  if (is_unitized(account_name)) { # This could include EQUITY too
    # Each parcel's adjusted and reduced cost
    #   The accounting cost (== reduced cost) is Accounting_Cost
    #   The adjusted cost is                     Accounting_Cost - Tax_Adjustments
    #   NB a tax free payment adjusts the cost and stores the negative adjustment as a tax adjustment
    #      a depreciation event does the same!!
    # At a parcel's sale they determine taxable
    # capital gains and losses
    # Stored (as sums) by parcel, cost element and time
    # eg Accounting_Cost[account][parcel][element][time]

    # p=-1 is not a real parcel
    Held_From[account_name][-1] = Epoch # This is needed by buy_units - otherwise write a macro to handle case of first parcel
    Parcel_Tag[account_name][SUBSEP] ; delete Parcel_Tag[account_name][SUBSEP] #

    # Keep track of units
    Total_Units[account_name][Epoch]     = Qualified_Units[account_name][Epoch] = 0
    # End of if Unitized
  } else if (is_class(account_name, "INCOME")) {
    # Set an Underlying_Asset if the leaf name
    # is of the appropriate format
    #
    # (DIV|DIST|FOR|GAINS).LEAF => LEAF
    #
    if (is_linked(account_name)) {
      # Probably a better way to do this using a regex
      linked_name = get_name_component(leaf_name, 2, -1)

      # Call initialize account to ensure this account is initialized
      Underlying_Asset[account_name] = initialize_account(linked_name)

@ifeq LOG initialize_account
      printf "Initialize Linked Account\n" > STDERR
      printf "\tIncome Account[%s] => %s\n", leaf_name, account_name > STDERR
      printf "\tLinked Account[%s] => %s\n", linked_name, Underlying_Asset[account_name] > STDERR
@endif
    }
  }

  # refer  to the parent item eg parent[A.B.C] => *A.B (long_name minus short_name with a distinguishing prefix)
  p = Parent_Name[account_name] = STAR array[1]

  # How many components in the name "p"
  n = split(p, array, ".")

  # Initialize the cost bases for this account's parents
  while (!(p in Parent_Name)) {
    # Get p's parent - lose the last name component
    if (n > 1)
      Parent_Name[p] = get_name_component(p, 1, --n, array)
    else
      Parent_Name[p] = STAR

    # Update p
    p = Parent_Name[p]
  }

  # the account name is the long name
  return account_name
}

# Delete the links to an unused account
function unlink_account(a) {
  if (a in Leaf)
    delete Leaf[a]
}

# Split/Merge/Copy a unitized account
function split_account(now, a, b, split_factor,
                            p, key, label) {
  # This takes a capital account a
  # and splits the units by a factor split_factor
  # and creates a new account b
  #
  # Both accounts must be initialized
  # Both accounts must be of the same class
  # Both accounts must be unitized (this can be revisited)
  assert(Parent_Name[a] == Parent_Name[b], "split: Class<" a "> != Class<" b ">")
  assert(is_unitized(a), "split_account: <" a "> not a unitized account")

  # Set split factor
  split_factor = ternary(split_factor, split_factor, 1)

  # Label
  label = ternary(split_factor > 1, "Split ", ternary(split_factor < 1, "Merge ", "Change"))

  # Write to tranaction file
  printf "##\n"
  printf "## %s %s => %s by factor %7.2f\n", label, Leaf[a], Leaf[b], ternary(split_factor < 1, 1.0 / split_factor, split_factor)
  printf "##   Date => %s\n", get_date(now)
  printf "##   %s Cost            => %s\n", Leaf[a], print_cash(get_cost(a, now))
  printf "##   %s Units           => %10.3f\n", Leaf[a], get_units(a, now)
  printf "##   %s Qualified Units => %10.3f\n", Leaf[a], get_qualified_units(a, now)

  # Copy parcels
  Number_Parcels[b] = Number_Parcels[a]

  # Get each open parcel in account a and copy it to account b
  for (p = 0; p < Number_Parcels[a]; p ++) {
    # Is this parcel purchased yet?
    if (greater_than(Held_From[a][p], now))
      # This is an error
      assert(FALSE, "Cannot " tolower(label) " <" Leaf[a] "> before all its transactions are complete")

    # Copy the parcel - this invocation copies from one account to another
    copy_parcel(a, p, b, p)

    # Adjust units in q if split_factor is not unity
    if (1 != split_factor)
      Units_Held[b][p] *= split_factor

    # if accounts a and b are different record this
    if (a != b)
      Account_Closed[a] = now

    # Close down pre-split account - at cost so no gains
    if (is_unsold(a, p, now)) {
      set_parcel_proceeds(a, p, - get_parcel_cost(a, p, now))
      Held_Until[a][p] = now
    }
  }

  # Total units
  for (key in Total_Units[a]) {
      Total_Units[b][key] = split_factor * Total_Units[a][key]
      Total_Units[a][key] = 0
  }
  for (key in Qualified_Units[a]) {
      Qualified_Units[b][key] = split_factor * Qualified_Units[a][key]
      Qualified_Units[a][key] = 0
  }

  # Is this a fixed account?
  if (is_fixed(a)) {
    if (a in Method_Name)
      Method_Name[b] = Method_Name[a]
    if (a in Lifetime)
      Lifetime[b] = Lifetime[a]
  }

  # Price records
  if (a in Price) {
    for (key in Price[a])
      if (less_than_or_equal(key, now))
        # These prices are for pre-split
        # They are needed after scaling
        Price[b][key] = Price[a][key] / split_factor
      else # These prices are for pre-split asset post split date
        # No way to know if accurate or not
        # So delete them - [b] prices must be added explicitly
        delete Price[a][key]
  }

  # Also need exdividend dates
  if (key in Dividend_Date)
    for (key in Dividend_Date[a])
      if (greater_than(key, now)) {
        Dividend_Date[b][key] = Dividend_Date[a][key]
        delete Dividend_Date[a][key]
      }

  # All done
  printf "##   After %s\n", label
  printf "##   %s Cost            => %s\n", Leaf[b], print_cash(get_cost(b, now))
  printf "##   %s Units           => %10.3f\n", Leaf[b], get_units(b, now)
  printf "##   %s Qualified Units => %10.3f\n", Leaf[b], get_qualified_units(b, now)
  printf "##\n"
}

#
# Filter Data
#
# Filter out irrelevant data - data that is out-of-range
# can arise from importing records
#
function filter_data(now, variable_names, show_details,    array_names, name) {
  # Which data arrays are to be filtered
  if (!split(variable_names, array_names, ","))
    return # Nothing to filter

  # Should we log
@ifeq LOG filter_data
   show_details = TRUE
@endif

  # Filter the data arrays
  for (name in array_names)
    filter_array(now, SYMTAB[array_names[name]], array_names[name], show_details)
}

# Handle each array in turn
function filter_array(now, data_array, name, show_blocks,
                           a, p, start_block, end_block, block_id,
                           stack, key, first_key,
                           earliest_key, latest_key, s) {

  # Record the earliest and latest keys found
  if (show_blocks) {
    # Report on data held
    print Journal_Title > STDERR
    printf "%s Data Held Report for Period Ending %s\n\n", name, get_date(now)  > STDERR

    earliest_key = Future
    latest_key   = Epoch
  }

  # list holding "blocks" - ie non-overlapping holding periods
  # Each block is preceeded and/or followed by "gaps"
  for (a in Leaf)
    if ((a in data_array) && is_unitized(a))
      if (ever_held(a)) {
        # Get each parcel in turn and list the contiguous blocks of time held
        start_block = Held_From[a][0]
        end_block = Held_Until[a][0]
        block_id = 0
        for (p = 1; p < Number_Parcels[a]; p ++) {
          # This starts a new holding block if the purchase date is after the current end date
          if (greater_than(Held_From[a][p], end_block)) {
            # Check the data against each block
            filter_block(key, data_array[a], start_block, end_block)

            # Remove originals of copies data to speed up processing of remaining entries
            for (key in stack)
              delete data_array[a][key]

            # A new block
            block_id ++
            start_block = Held_From[a][p]
            end_block = Held_Until[a][p]
          } else if (greater_than(Held_Until[a][p], end_block)) # extend the old block
            end_block = Held_Until[a][p]

          # If this parcel is open we have completed all possible blocks
          if (is_unsold(a, p, now))
            break
        } # End of each parcel p

        # Check the data against each block
        filter_block(key, data_array[a], start_block, end_block)
        if (show_blocks)
          # Get first key
          for (first_key in stack) {
            # Record latest key
            latest_key = max_value(latest_key, first_key)
            break
          }

        # Some simple formatting
        if (Show_Extra && show_blocks)
          printf "\n" > STDERR

        # Delete the array
        delete data_array[a]

        # But put the kept items back
        for (key in stack) {
          data_array[a][key] = stack[key]

          # Show this data when detailed reporting is enabled
          if (Show_Extra && show_blocks)
            printf "%22s\t %s => %s\n", Leaf[a], get_date(key), format_value(stack[key]) > STDERR
        }

        # get last key and show range of keys
        if (show_blocks && (key in stack)) {
          s = ternary(is_open(a, now), "*", "")
          if (key != first_key)
            # More than one key
            printf "%22s\t[%s, %s]%s\n", Leaf[a], get_date(key), get_date(first_key), s > STDERR
          else if (!Show_Extra)
            # Only one key in this block - already recorded if Show_Extra set
            printf "%22s\t[%s]%s\n", Leaf[a], get_date(first_key), s > STDERR

          # Record earliest key
          earliest_key = min_value(earliest_key, key)
        }

        # Clean up
        delete stack

      } else {
        # Never held!
        if (show_blocks)
          printf "%22s\tNever Held!\n", Leaf[a] > STDERR

        unlink_account(a)
      }
    # End of each asset a

    # Final Summary
    if (show_blocks) {
      underline(44, 6, STDERR)
      printf "%22s\t[%s, %s]\n\n", name, get_date(earliest_key), get_date(latest_key) > STDERR
    }
}

##
## Some tax functions

##
## Depreciation
##
# Depreciate each parcel separately
function depreciate_now(a, now,       p, delta, sum_delta,
                                      open_key, open_value,
                                      first_year_factor, factor) {
  # Depreciate from the most recent snapshot to now
  assert(is_open(a, now), sprintf("depreciate_now: Can't depreciate closed asset %s", get_short_name(a)))

  # Depreciating assets only use cost elements I or II
@ifeq LOG depreciate_now
  printf "Compute Depreciation\n%16s\n\tDate => %s\n", get_short_name(a), get_date(now, LONG_FORMAT)
@endif # LOG

  # First pass at setting depreciation factor
  first_year_factor = FALSE
  if ("PC" != Method_Name[a]) {
    # So an annual depreciation rate of R% becomes
    # a lifetime L (in years) of L = 200 / R
    # Therefore standard lifetimes are
    # Aus R = 37.5 L => 16/3  = 5.3333
    # GBR R = 18.0 L => 100/9 = 11.1111
    # GBR R =  8.0 L => 100/4 = 25.0
    factor = 2.0 / Lifetime[a]
    if ("POOL" == Method_Name[a])
      # In Australia (eg) first year pool depreciation at half rate
      first_year_factor = First_Year_Factor * factor
  }

  # Get each parcel
  sum_delta = 0
  for (p = 0; p < Number_Parcels[a]; p ++) {
    # Is this parcel purchased yet?
    if (greater_than(Held_From[a][p], now))
      break # All done
    if (is_unsold(a, p, now)) {
      # First see if depreciation already was computed for time (now)
      if (now in Accounting_Cost[a][p][I]) {
        # Already depreciated
@ifeq LOG depreciate_now
        # Debugging
        printf "\tAlready Depreciated to => %s\n", get_date(now) > STDERR
@endif # LOG
        continue # Get next parcel
      }

      # Now we need the opening value for this parcel - always cost element I
      open_key = find_key(Accounting_Cost[a][p][I], just_before(now))
      assert(greater_than_or_equal(open_key, Epoch), sprintf("%s: No earlier depreciation record than %s", get_short_name(a), get_date(now)))

      # The opening value - cost element I
      open_value = Accounting_Cost[a][p][I][open_key]

@ifeq LOG depreciate_now
      # Debugging
      printf "\tParcel => %04d\n", p > STDERR
      if (keys_in(Parcel_Tag, a, p))
        printf "\tName   => %s\n", Parcel_Tag[a][p] > STDERR
      printf "\tMethod => %s\n", Method_Name[a] > STDERR
@endif # LOG

      # Refine factor at parcel level
      if (first_year_factor) {
        # First year sometimes has modified depreciation
        if (near_zero(find_entry(Tax_Adjustments[a][p], now)))
          delta = first_year_factor
        else
          delta = factor
      } else {
        # Not pooled or DV
        if ("PC"  == Method_Name[a])
          factor = get_cash_in(a, p, now) / Lifetime[a]

        # The proportion to be depreciated
        # We just need the number of days difference as
        # a fraction of the current financial year...
        delta = ((now - open_key) / ONE_DAY) * factor / FY_Length
      }

@ifeq LOG depreciate_now
      printf "\tFactor => %.3f\n", factor
@endif # LOG

      # This block is the only difference between prime cost and diminishing value
      if ("PC" != Method_Name[a]) # Diminishing Value or Pool (not Prime Cost)
        delta *= open_value

      # Check for negligible or negative value
      if (!above_zero(open_value - delta))
        delta = open_value

      # Adjust the parcel cost - element I
      adjust_parcel_cost(a, p, now, -delta, I, TRUE)
      sum_delta += delta

@ifeq LOG depreciate_now
      # Debugging
      printf "\tOpen  => %s\n", print_cash(open_value) > STDERR
      printf "\tDelta  => %s\n", print_cash(delta) > STDERR
      if (delta == open_value)
        printf "\tZero Parcel => %d\n", p > STDERR
@endif # LOG
    } # End of if unsold parcel
  } # End of each parcel

@ifeq LOG depreciate_now
  printf "%s: %s New Reduced Cost[%s] => %11.2f\n", "depreciate_now", get_short_name(a), get_date(now), get_cost(a, now) > STDERR
@endif # LOG

  # Return the depreciation
  return sum_delta
}

# get_tax payable on total income
# can be used to get other banded quantities such as the low income tax offset & the medicare levy
function get_tax(now, bands, total_income,

                      tax_payable,
                      band_width,
                      current_key, last_threshold, threshold) {

  # Tax bands can either be the bands themselves
  # eg Band[T_0] => 0.20
  #    Band[T_1] => 0.30
  #    Band[T_2] => 0.40
  #
  #    and so on
  #
  #  Or the negative integral
  #    Band[T_0] =>   0.00
  #    Band[T_1] => - 0.20 * (T_1 - T_0)
  #    Band[T_2] => - 0.20 * (T_1 - T_0) - 0.30 * (T_2 - T_1)
  #

  # Get the current tax band
  current_key = find_key(bands, now)

  # Ensure bands are listed in decreasing order
  if (current_key in bands)
    invert_array(bands[current_key])
  else # No entry
    return 0

  # Compute tax
  tax_payable = last_threshold = 0
  for (threshold in bands[current_key]) {
    # The last band's width
    band_width = last_threshold - threshold # negative thresholds stored

    # Is this threshold below the total income?
    if (less_than(total_income, -threshold))
      break

    # Update the last threshold
    if (!near_zero(band_width))
      #  The contribution to tax
      tax_payable += band_width * bands[current_key][last_threshold]

    # Get the next band
    last_threshold = threshold
  }

  # Get the total tax payable (threshold is stored as a negative quantity)
  tax_payable += (total_income + last_threshold) * bands[current_key][last_threshold]

  # The marginal tax is the extra tax
  return tax_payable
}


# the inverse function
# only uses tax bands - not levies
function get_taxable_income(now, bands, tax_left,
                                 total_income, band_width, band_tax,
                                 current_key, last_threshold, threshold) {

  # Get the current tax band
  current_key = find_key(bands, now)

  # Ensure bands are listed in decreasing order
  invert_array(bands[current_key])

  # When the tax left is zero or negative it must be the first band
  last_threshold = 0
  if (!above_zero(tax_left)) {
    # If the first band has a zero rate no income is assumed
    if (near_zero(bands[current_key][last_threshold]))
      return 0
    return tax_left / bands[current_key][last_threshold]
  }

  # Now get the tax due on the whole sum
  total_income = 0
  for (threshold in bands[current_key]) {
    # The last band's width
    band_width = last_threshold - threshold # negative thresholds stored

    # The maximum tax due in this band
    band_tax = band_width * bands[current_key][last_threshold]

    # Is the tax_payable above the amount paid?
    if (greater_than_or_equal(band_tax, tax_left)) {
      # The tax actually accruing from this band is tax_left
      # so the income lying in this band is simply x
      total_income += tax_left * band_width / band_tax
      break
    }

    # Reduce tax left
    tax_left -= band_tax
    if (near_zero(tax_left))
      break

    # Get the next band
    last_threshold = threshold
  }

  # We can still have have tax unaccounted for here
  if (greater_than(tax_left, Epsilon))
    total_income += tax_left / bands[current_key][last_threshold]

  # The minimum total income that would generate this much tax
  return total_income
}

# force an array to have indices stored in ascending order of magnitude
# even when the indices actually descend in simple numerical value
function invert_array(array,        key, last_key, value) {
  last_key = ""
  for (key in array)
    if ("" == last_key)
      last_key = key
    else
      break

  # Did we find more than one key?
  if ("" == last_key)
    # No
    return

@ifeq LOG  invert_array
  printf "Array Key Order\n" > STDERR
  printf "First Key => %s\n", last_key > STDERR
  printf "Next  Key => %s\n", key > STDERR
@endif

  # Yes - what order do these keys have?
  if (less_than(abs_value(last_key), abs_value(key)))
    # Desired order
    return

@ifeq LOG  invert_array
  printf "Invert Array\n" > STDERR
@endif

  # Wrong order!
  for (key in array)
    if (above_zero(key)) {
      # Reverse sign of keys to enforce required ordering
      value = array[key]
      delete array[key]
      array[-key] = value
@ifeq LOG  invert_array
      printf "Invert  Array[%s] => %s\n",  key, format_value(value) > STDERR
      printf "New Key Array[%s] => %s\n", -key, format_value(value) > STDERR
@endif
    }
}



# Initialize a url encoding lookup table
# Use this as a hash table too
function url_init(   i) {
  Document_Filetype = "pdf" # Default file type
  for (i = 0; i <= 255; i++)
    URL_Lookup[sprintf("%c", i)] = i
}

# urlencode a string
function url_encode(string,     c, chars, url, i) {
  # Get an array holding the characters
  split(string, chars, "")

  # the encoded string
  # loop in reverse order
  url = ""
  for (i in chars) {
    c = chars[i]

    # Just prepend plain vanilla characters
	  if (c ~ /[0-9A-Za-z/\.]/)
	    url = c url
	  else # Get the hex code
	    url = "%" sprintf("%02X", get_char(c)) url
  }

  # Tidy up array
  delete chars

  # The encoded string
  return url
}

# A hash function - returns a string
function string_hash(text,    prime, modulo, h, chars, i) {
  # These are fixed
  prime  = 104729
  modulo = 1048576

  # The hash
  h      = 0
  split(text, chars, "")
  for (i in chars)
    h = (prime * h + get_char(chars[i])) % modulo

  # Tidy up array
  delete chars

  # Return hash h
  return sprintf("%05x", h)
}

# print underline
function underline(width, margin, stream) {
 stream = ternary(stream, stream, STDOUT)
 if (margin)
   printf "%*s", margin, "" > stream
 print_block("_", width, stream)
}

### New date parser
##
## Formats which can be read
## Month is a month name or three letter abbreviation with or without an initial capital
## Month-DD-YYYY or Month-DD-YY or Month/DD/YYYY or Month/DD/YY eg August-14-18
## DD-Month-YYYY or DD-Month-YY or DD/Month/YYYY or DD/Month/YY eg 14-Aug-18
## YYYY-MM-DD or YY-MM-DD or YYYY/MM/DD or YY/MM/DD eg 2018/08/14
##
##
## Regex to get initial modification to date string
##  (20[01][0-9]) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ([0-3][0-9])
##


function read_date(date_string, hour,
                   date_fields, year, month, day, value) {
  # default time
  hour = ternary("" == hour, HOUR, hour)

  # default is YYYY-MM-DD
  # Formats are YYYY-MM-DD
  #             DD-MM-YYYY
  #             YYYY-Mon-DD
  #             Mon-DD-YYYY
  #             DD-Mon-YYYY
  # Or other separators, eg '/' etc
  if (date_string !~ /[[:alnum:]]+[-/ ][[:alnum:]]+[-/ ][[:digit:]]+/) {
    Read_Date_Error = "Illegal date string format"
    return DATE_ERROR
  }

  # Split the input date
  if (3 == split(date_string, date_fields, "[-/ ]")) {
    # Which format
    if (month = get_month_number(date_fields[1])) {
      # Mon-DD-YYYY
      day   = date_fields[2] + 0
      year  = date_fields[3] + 0
    } else if (month = get_month_number(date_fields[2])) {
      # YYYY-Mon-DD
      year  = date_fields[1] + 0
      day   = date_fields[3] + 0

      # Catch DD-Mon-YYYY
      # Will not work unless YYYY >= 32
      if (day > 31) {
        if (year <= 31) {
          day   = date_fields[1] + 0
          year  = date_fields[3] + 0
        } else {
          Read_Date_Error = "Can't parse date <" date_string "> day number inconsistent with any month"
          return DATE_ERROR
        }
      } # end of bad day number

    } else {
      # YYYY-MM-DD
      day   = date_fields[3] + 0
      month = date_fields[2] + 0
      year  = date_fields[1] + 0

      # Catch DD-MM-YYYY
      # Will not work for DD-MM-YY unless YY >= 32
      if (day > 31) {
        if (year <= 31) {
          day   = date_fields[1] + 0
          year  = date_fields[3] + 0
        } else {
          Read_Date_Error = "Can't parse date <" date_string "> day number inconsistent with any month"
          return DATE_ERROR
        }
      } # end of bad day number
    }

    # Assume year < 1000 is a reference to a relative year number
    # In practice this means the 3rd Millenium
    if (year < 1000)
      year += EPOCH_START
  } else {
    Read_Date_Error = "Can't parse date <" date_string "> wrong number of fields"
    return DATE_ERROR
  }

  # Use mktime
  value = mktime(sprintf("%4d %02d %02d %02d 00 00", year, month, day, hour), UTC)
  if (value < 0) {
    Read_Date_Error = "A negative timestamp found"
    return (DATE_ERROR)
  }

  # All ok
  delete date_fields
  return value
}

# default time formats needs a list of months
function set_months(   i, month_name, mon) {
  # Generate machine readable date fields
  split("January February March April May June July August September October November December", month_name, " ")

  # Get the reverse loookup
  # Add three letter abbreviations too
  # So for example
  #  Lookup_Month["Jan"] == Lookup_Month["January"] == Lookup_Month["jan"] = Lookup_Month["january"]
  #  in modulo 12 arithmentic
  for (i in month_name) {
    mon = substr(month_name[i], 1, 3)
    Lookup_Month[mon] = i
    Lookup_Month[month_name[i]] = i
    Lookup_Month[tolower(substr(mon, 1, 1)) substr(mon, 2, 2)] = i
    Lookup_Month[tolower(substr(month_name[i], 1, 1)) substr(month_name[i], 2)] = i
  }

  # Remove temporary array
  delete month_name
}

# Set a default epoch & future
function set_epoch() {
  # The Epoch
  # A more practical Epoch
  Carried_Loss_Limit = Epoch = mktime(EPOCH_START " 01 01 00 00 00", UTC)

  # A distant Future
  Future = mktime(EPOCH_END " 12 31 00 00 00", UTC)
}

# Get the time stamp m months in the  future
function add_months(now, number_months,   y, m, d,
                                          delta_years, delta_months) {
  # Get calendar year number, month number and day number
  y = strftime("%Y", now, UTC) + 0
  m = strftime("%m", now, UTC) + 0
  d = strftime("%d", now, UTC) + 0

  # The number of months to add
  delta_months = number_months % 12

  # The number of years to add
  delta_years = int((m + number_months) / 12)

  # The end year
  y += delta_years

  # Add the months - modulo 12
  m = (m + delta_months) % 12

  # Leap years complicate the calculation
  if (2 == m && 29 == d && !leap_year(y)) {
    # February 29 can only exist in a leap year
    # Reset m & d to March 1st
    ## This might be dealt with by mktime automatically
    m = 3
    d = 1
  }

  # Get the time stamp
  return mktime(sprintf("%4d %02d %02d %02d 00 00", y, m, d, HOUR), UTC)
}
