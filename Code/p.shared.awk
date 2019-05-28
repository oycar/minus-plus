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
  # since Payment_Date[ex_dividend_date] => now-ish
  if (a in Payment_Date) {

    # Get the most recent payment date
    value = find_entry(Payment_Date[a], now)
    discrepancy = now - value

    # The value cannot be later than the current time "now"
    if (value > now) {
      Read_Date_Error = "Payment date is later than current date"
      return DATE_ERROR
    } else if (near_zero(discrepancy))
      return found_key

    # Some times dividends are paid out of order, for example
    # a special or buyback dividend might have an extra
    # long qualification period - so look ahead more dividends
    # until the discrepancy increases
    #
    key = found_key
    while (key) {
      value = find_entry(Payment_Date[a], just_before(key))
      if ((now - value) > discrepancy)
        # A worse match
        break

      # A better match
      discrepancy = now - value
      if (near_zero(discrepancy))
        return found_key

      # Save  this match
      key = found_key
    }

    # Best match was key
    if (discrepancy > ONE_WEEK) {
      Read_Date_Error = "Failed to find a payment date within one week of current date"
      return DATE_ERROR
    }

    # Return it
    return key
  }

  # Failed to find a qualification date
  Read_Date_Error = "Failed to find any payment date"
  return DATE_ERROR
}

# read csv records
function read_csv_records() {
  # This is a CSV file
  FPAT = "([^,]*)|(\"[^\"]+\")"
}

# Abstract read value out
function read_value(x) {
  # Get the value first
  # Is it a number?
  if (x ~ /^[0-9\.\-]+$/)
    return strtonum(x)

  # Just return the field
  return x
}


# A somewhat generalized variable reading function
function read_state(nf,    i, x, value) {
  # The fields represent the keys & value
  value = read_value($nf)

  # Logging
@ifeq LOG read_state
  printf "%s", Variable_Name > STDERR
@endif

  # Is this an array?
  if (nf == 1) { # No
    SYMTAB[Variable_Name] = value
@ifeq LOG read_state
    # Logging
    printf " => %s\n", value > STDERR
@endif
  } else {
    # The rest of the keys
    for (i = 1; i < nf; i ++)
      # The code can minimize output file by
      # retaining the old key if the "ditto"
      # symbol is encountered;
      if (($i != DITTO) || !(i in Variable_Keys))
        Variable_Keys[i] = $i

@ifeq LOG read_state
    # Logging
    for (i = 1; i < nf; i++)
      printf "[%s]", Variable_Keys[i] > STDERR
    printf " => %s\n", value > STDERR
@endif

    # Set the array value
    set_array(SYMTAB[Variable_Name], Variable_Keys, 1, nf - 1, value, FALSE)
  }
}

# Set a multi-dimensional array value
# This assumes array is correctly defined
function set_array(array, keys, first_key, last_key, value, flag) {
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
  if (first_key == last_key)
    # Set the value
    set_entry(array, value, keys[first_key])
  else {
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
    set_array(array[keys[first_key]], keys, first_key + 1, last_key, value, flag)
  }
}

# Output all the data associated with the accounts to the specified data file
function write_state(array_names, scalar_names,    name) {
  # The array data
  # Keep track of keys written out
  for (name in array_names) {
    make_array(Key_Index)
    printf "<<,%s\n", array_names[name] > Write_State
    walk_array(SYMTAB[array_names[name]], 1, Write_State)
    printf ">>\n" > Write_State
    delete Key_Index
  }

  # The scalars - compact form
  for (name in scalar_names)
    printf "<<,%s,%s,>>\n", scalar_names[name], format_value(SYMTAB[scalar_names[name]]) > Write_State
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
        printf "%s,", Key_Index[i] > stream
        if (DITTO)
          Key_Index[i] = DITTO
      }

      # Complete the output - use special key for deepest level
      printf "%s,%s\n", last_key, format_value(arr[key]) > stream
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

# The first entry
function first_entry(array,   key) {
  key = first_key(array)
  if ("" == key)
    return key

  # Must be in the array
  return array[key]
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

  # Finished
  array[now] = x + array[key]
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
# White space trimming
function ltrim(s) { sub(/^[ \t\r\n]+/, "", s); return s }
function rtrim(s) { sub(/[ \t\r\n]+$/, "", s); return s }
function trim(s) { return rtrim(ltrim(s)); }
function to_number(s, default_value) {return "" == s ? default_value : strtonum(s)}

# character trimming
function ctrim(s, left_c, right_c,      string) {
  # Different chars can be trimmed from left and right
  right_c = ("" == right_c) ? left_c : right_c

  # The left trim
  string = "^\\" left_c
  sub(string, "", s)

  # The right trim
  string = "\\" right_c "$"
  sub(string, "", s)
  return s
}

# # Is a number between two others? (allow boundary cases to be inside)
# function is_between(x, low, high) {
#   return  (x - low) * (x - high) <= 0
# }

# Clear global values ready to read a new input record
function new_line() {
  Extra_Timestamp = DATE_ERROR
  Parcel_Name = ""
  Real_Value[1] = Real_Value[2] = 0
  Tax_Adjustment = FALSE
  Cost_Element = COST_ELEMENT # The default value
  Automatic_Depreciation = FALSE
  GST_Claimable = 0
  Depreciation_Type = ""
  Comments = ""
  Documents = ""
  Account[1] = Account[2] = ""
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
#      2017 Aug 24, AMH.DIV, AMH.ASX, 3072, 2703.96, [1025.64,] [1655.49], # DRP & LIC
function parse_line(now,    current_field, i, j, x, number_accounts) {
  #
  # The record may be
  #     Double Entry => two accounts
  #     Single Entry => one account
  #  or No Entry => zero accounts
  for (i = 2; i <= NF; i ++)
    # Need to remove white space from the fields
    $i = trim($i)

  # Get next one or two fields
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

  # Interpret units
  #
  # If number accounts > 0 this MUST be a legal units field
  # Units indicate whether an item is bought or sold; or the cost element or whether adjusting
  # the cost base or the reduced cost base
  #
  #  A non zero numerical value
  #   units > 0 => BUY  cost element I
  #   units < 0 => SELL cost element I
  #
  #  A blank or zero
  #    units == 0 => Cost Base elelment II (the default)
  #
  #  A string  (roman numbers I to V are meaningful but could be anything without brackets)
  #    units == I   => Cost Base element I
  #    units == II  => Cost Base element II etc
  #
  #  A bracketed string
  #    units == (I) => Tax Adjustment to Cost Base element I etc
  #
  # Finally the special value (D) or (d) means depreciate automatically

  if (number_accounts) {
    # Is units a numerical value?
    i ++
    units = parse_units($i)

    # The amount - this should be checked
    # Must be a number or "COST" if a sale transaction
    i ++
    if ("COST" == $i) {
      # In this case this must be a sale of all the units
      assert("SELL" == Write_Units, "<" $0 "> Is not a sale transaction")

      # Get the amount
      amount = get_cost(Account[1], now)

      # If this is a single entry transaction must negate the amount
      if (1 == number_accounts)
        amount = - amount
    } else {
      # Must ba number
      assert($i ~ /^[0-9\.\-]+$/, "<" $0 "> Unexpected syntax amount <" $i "> is not a number")
      amount = strtonum($i)
    }
  }

  # From now on the fields are context dependent
  # Possibilities are:
  # * **Field 6**
  #   * **A Parcel Date** in a buy or sell transaction
  #   * **An Ex-Dividend Date** in an income transaction
  #   * **Brokerage** in a buy or sell transaction
  #   * **Tax Credits** in an income transaction
  #   * **Effective Life** in a depreciating asset purchase
  #   * **A Parcel Name** in a buy or sell transaction
  #   * **A GST Tag** in an income, expense, buy or sell transaction. If brokerage is present the GST is applied only on the brokerage.
  #   * **A document name** in any transaction
  #   * **A comment** in any transaction
  # * **Field 7**
  #   * **A Parcel Date** in a buy or sell transaction
  #   * **An Ex-Dividend Date** in an income transaction
  #   * **LIC (listed investment company) Credits** in an income transaction
  #   * **A Parcel Name** in a buy or sell transaction
  #   * **Depreciation method** in a depreciating asset purchase
  #   * **A GST Tag** in an income, expense, buy or sell transaction. If brokerage is present the GST is applied only on the brokerage.
  #   * **A document name** in any transaction
  #   * **A comment** in any transaction
  # * **Field 8**
  #   * **A Parcel Name** in a dividend reinvestment -
  #   * **An Ex-Dividend Date** in an income transaction
  #   * **A GST Tag** in an income, expense, buy or sell transaction. If brokerage is present the GST is applied only on the brokerage.
  #   * **A document name** in any transaction
  #   * **A comment** in any transaction
  # * **Field 9**
  #   * **A document name** in any transaction
  #   * **A Parcel Name** in a dividend reinvestment -
  #   * **A comment** in any transaction
  # * **Field 10** and higher
  #   * **A document name** in any transaction
  #   * **A comment** in any transaction

  # Move on to the next field
  i ++

  # The next two fields can contain real numbers, time-stamps or strings
  # Check first for real numbers or time-stamps
  # Next two fields are compatible
  for (j = 1; i <= NF; j++) {
    # Set x
    if (j <= 3)
      x = parse_optional_value($i)
    else # if j > 3
      x = 0

    # Shared code - save x if set
    if (x) # x is not zero or ""
      Real_Value[j] = x
    else if ("" != x) { # Will be "" when a timestamp set
      # x not set so look for strings
      # Can reuse x
      x = parse_optional_string($i, TRUE)

      # Treat as a comment
      if (x)
        Comments = add_field(Comments, x)
    }

    # Increment i
    i ++
  }

  # Comments should be signified with an octothorpe
  if (Comments !~ /^#/)
    Comments = add_field("# ", Comments)

  # A document name is added as a final comment
  Comments = add_field(Comments, Documents)

  # All done - return record type
  return number_accounts
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
  # FIXME can do better than this
  value = abs_value(strtonum(field))

  # This is a value
  if (near_zero(value))
    value = 0

  # Returning the value
  return value
}

# Parse optional string
# By default do not save any document found
function parse_optional_string(field, save_document,    string, x) {
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

    default: # this is an optional string
      break # no-op
  }

  # The string can be a document name enclosed in square brackets
  string = ctrim(field, "[", "]")
  if (string == field) {
    # Parcel names are enclosed by double quotes
    string = ctrim(field, "\"", "\"")
    if (string != field) {
      # A Parcel name
      Parcel_Name = field # With quotes
      return ""
    }

    # Otherwise a comment ...
    return string
  }

  # Only save the document name if non trivial and if requested
  if ("" != string && "" != save_document) {
    # Add any document names
    if (!(string in Document_Name))
      Document_Name[string] = parse_document_name(string)

    # Add to Documents
    Documents = add_field(Documents, Document_Name[string])
  }

  # All done
  return ""
}


# A document name may contain a filetype suffix
function parse_document_name(string,    filename, filetype, z, i, n) {
  # How many dotted fields?
  n = split(string, z, ".")

  # If more than one last is treated as filetype
  if (n > 1)
    filetype = url_encode(z[n])
  else # Default filetype
    filetype = Document_Filetype

  # Process  the filename elements
  filename = url_encode(z[1])
  for (i = 2; i < n; i ++)
    filename = filename "." url_encode(z[i])

  # return result
  return Document_Root filename "." filetype
}

# Is units a numerical value?
function parse_units(u, units,      len) {
  units = strtonum(u)
  if (0 == units) {
    # Need to examine the original string more closely
    len = length(u)

    # Brackets?
    if (u ~ /^()/ && u ~ /)$/) {
      # This is probably a tax adjustment
      Tax_Adjustment = TRUE

      # bracketed
      if (len > 2)
        units = trim(toupper(substr(u, 2, len - 2)))
      else
        # Empty String defaults to cost element II
        units = COST_ELEMENT
    } else
      units = u

    # The units might still be I, II, III, IV, V, D or 0
    switch (units) {
      case "0" : Cost_Element = COST_ELEMENT
        # A string (0) is not a tax adjustment
        Tax_Adjustment = FALSE
        break
      case "D" : # Depreciation
        Cost_Element = I # First cost element
        Tax_Adjustment = Automatic_Depreciation = TRUE
        break
      case "I" :
      case "II" :
      case "III" :
      case "IV" :
      case "V" : # Cost elements
        Cost_Element = units # As specified
        break

      # Special case of "SELL"
      case "SELL" :
         Write_Units = units # To signify a sale
         Cost_Element = I # First cost element
         break
      default: #
        # A string such as (12) is not a tax adjustment
        Tax_Adjustment = FALSE
    }

    # Ensure units are still zero
    units = 0
  } else { # BUY or SELL transaction
    # Units should be numerical
    if (units !~ /^[0-9\.\-]+$/)
      # If we get here this assertion will fail
      assert(FALSE, "<" $0 "> Unexpected cost element field syntax <" units ">")

    Cost_Element = I # First cost element
  }

  # The output syntax
  if (Write_Units != "SELL") {
    if (0 != units)
      Write_Units  = sprintf("%10.3f", units)
    else if (Tax_Adjustment)
      Write_Units =  "(" Cost_Element ")"
    else if (COST_ELEMENT == Cost_Element)
      Write_Units = 0 # Simpler to read and most common case
    else
      Write_Units = Cost_Element
  } else
    units = -1 # This is a simple place holder

  return units
}

# optional fields
# add optional fields in the correct order to an output string
function add_optional_field(optional_fields, field_value, field_rank,
                            number_fields) {
  # Optional fields are ordered as follows
  # Rank 1  * **A Unique Timestamp**
  # Rank 2  * **Floating Point Number(s)**
  # Rank 3  * **"string" enclosed in double quotes**
  # Rank 4  * **A GST Tag**
  # Rank 5  * **A [document name]**
  # Rank 6  * **A # comment**
  #
  # so
  # optional_fields[2][2] contains the second floating point field
  # optional_fields[3][1] contains a "string"
  # optional_fields[6][4] contains the fourth comment
  #
  #
  # Initially optional_fields[X]["length"] is not set (no entries)
  # If one is added it becomes optional_fields[X][0] => 1
  if (field_rank in optional_fields)
    # One extra field
    number_fields = optional_fields[field_rank]["length"] + 1
  else {
    # Initialize this Rank
    make_array(optional_fields[field_rank])
    number_fields = 1
  }

  # Save the value
  optional_fields[field_rank][number_fields] = field_value

  # Save the number of fields of this rank
  optional_fields[field_rank]["length"] = number_fields
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
function one_year(now, sense,     n, year, day, sum) {
  # Sense == n is forward n years
  # Sense == -n is back n years

  # Get the day number
  day = get_day_number(now)

  # Which Calendar year is this?
  year = get_year_number(now)
  if (sense > 0) {
    n = sense
    year += sense
  } else
    n = - sense

  # Go back n years
  sum = 0
  while (n -- > 0) {
    sum += get_year_length(year, day)
    year --
  }

  # Get the length in seconds
  return sense * ONE_DAY * sum
}

# Useful account filters
function is_open(a, now,     p) {
  # An asset is open if there are unsold parcels at time 'now'
  for (p = 0; p < Number_Parcels[a]; p ++) {
    if (Held_From[a][p] > now)
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
  while ("" != p) {
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

# Initialize cost element arrays
function zero_costs(array, now,     e) {
  # Set all true cost elements to zero - ie elements I-V
  for (e in Elements)
    array[e][now] = 0
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
  assert("" != s, sprintf("Requested component <%d> couldn't be found in %s", i, name))
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
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if (Held_From[a][i] == now) {
        # The adjustment is pooled explicitly with this parcel
        adjust_parcel_cost(a, i, now, x, Cost_Element, tax_adjustment)

@ifeq LOG adjust_cost
        # Debugging
        printf "\tCurrent Total Cost   => %s\n", print_cash(get_cost(a, now)) > STDERR
@endif # LOG
        # Also record the parents cost
        # If this is a tax adjustment then only negative costs are significant
        if (!tax_adjustment || x < 0)
          update_cost(a, x, now)

        return # Only one parcel is adjusted - it must be unsold if only just purchased
      }
    }

    # The cost adjustment per unit except for depreciating assets
    if (flag = is_fixed(a))
      adjustment = x / get_cost(a, now)
    else
      adjustment = x / get_units(a, now)

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

    # Balance costs
    if (!tax_adjustment || x < 0)
      update_cost(a, x, now)
  } else if (!tax_adjustment || x > 0) { # This is the corresponding account - only significant if not a tax adjustment or if it is positive
    sum_entry(Cost_Basis[a], x, now)

    # Also record the parents cost
    update_cost(a, x, now)
  }
}

# Update the cost of the parent account
function update_cost(a, x, now,      p) {
  # Now get the parent to this account
  p = Parent_Name[a]
  if ("" == p)
    return # Finished

  # Update the cost
  assert(p in Cost_Basis, "Failed to find Cost_Basis of account  <" p ">")
  sum_entry(Cost_Basis[p], x, now)

  # Logging
@ifeq LOG update_cost
  printf "\tp => %16s x => %11s Date => %11s sum => %11s\n", p, print_cash(x), get_date(now), print_cash(get_cost(p, now)) > STDERR
@endif # LOG
  update_cost(p, x, now)
}

function adjust_parcel_cost(a, p, now, parcel_adjustment, element, adjust_tax,        cost_base) {
@ifeq LOG adjust_cost
  printf "%s\n", a > STDERR
  printf "\tTimeStamp => %s\n", get_date(now) > STDERR
  printf "\t\tParcel  => %05d", p  > STDERR
  printf " Opening Parcel Cost[%s]=> %s\n", element, print_cash(get_parcel_element(a, p, element, now)) > STDERR
  printf "\t\t\t\tParcel Adjustment => %s\n", print_cash(parcel_adjustment) > STDERR
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
    if (parcel_adjustment > 0)
      # A tax adjustment for an undeductible but legitimate expense
      sum_entry(Tax_Adjustments[a][p][element], - parcel_adjustment, now)
    else { # A tax adjustment for deferred tax or depreciation &c
      sum_entry(Accounting_Cost[a][p][element], parcel_adjustment, now)
      sum_entry(Tax_Adjustments[a][p][element], parcel_adjustment, now)
    }
  } else
    # Update the accounting cost
    sum_entry(Accounting_Cost[a][p][element], parcel_adjustment, now)

  # Equities do not have tax adjustments and can indeed have a negative cost base
  if (!is_equity(a)) {
    # Now this is tricky -
    #   The cost base can be negative
    #   but not after the tax adjustment
    # Also if this parcel was sold on the same day (so time==now)
    # a term will be included in cash_out - so overrule that
    cost_base =  sum_cost_elements(Accounting_Cost[a][p], now) - get_cash_out(a, p, now)
    if (cost_base < sum_cost_elements(Tax_Adjustments[a][p], now)) {
      # Cannot create a negative cost base (for long)
      save_parcel_gain(a, p, now)

      # We are cashing the tax adjustments out so adjust both cost bases to zero
      zero_costs(Accounting_Cost[a][p], now)
      zero_costs(Tax_Adjustments[a][p], now)
    }
  }

  # Debugging
@ifeq LOG adjust_cost
  printf "\t\t\t\tReduced Parcel Cost[%s] => %s\n", element, print_cash(get_parcel_element(a, p, element, now)) > STDERR
@endif # LOG
} # End of adjust_parcel_cost

# The idea of the "cost" of the account
# This is the same as the reduced cost
function get_cost(a, now,     i, sum_cost) {
  # Adjustments for units bought
  if (is_unitized(a)) {
    # Initial cost
    sum_cost = 0

    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
        sum_cost += sum_cost_elements(Accounting_Cost[a][i], now)
    }
    return sum_cost
  } else if (a in Cost_Basis) # Cash-like
    return find_entry(Cost_Basis[a], now)

  return 0
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
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
        sum_adjustments += sum_cost_elements(Tax_Adjustments[a][i], now)
    }
  }

  return sum_adjustments
}



# set the cost to a specified value (new_cost)
function set_cost(a, new_cost, now,     initial_cost) {
  # The current cost
  initial_cost = get_cost(a, now)

  # The required change in the cost is therefore (new_cost - initial_cost)
  adjust_cost(a, new_cost - initial_cost, now, FALSE)
}

# Unrealized or market gains
function sum_market_gains(now,     sum, a) {
  sum = 0

  # Cash-like assets can be ignored
  for (a in Leaf)
    if (is_capital(a) && is_open(a, now))
      # The asset must be active
      sum += get_cost(a, now) - find_entry(Price[a], now) * get_units(a, now)

  # All done - negative values are gains
  return sum
}

# Sum  the cost elements
function sum_cost_elements(array, now,     sum_elements, e) {
@ifeq LOG DEBUG
  assert(isarray(array), "<" $0 "> sum_cost_elements:  needs an array")
@endif

  sum_elements = 0
  for (e in array) # Should this include [0] or not?
    sum_elements += find_entry(array[e], now)
  return sum_elements
}

# Get the specified cost element
function get_cost_element(a, element, now,      i, sum_cost) {
  # Initial cost
  sum_cost = 0

  # Only assets have cost elements - equity is just for simplicity
  if (is_unitized(a)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
        sum_cost += find_entry(Accounting_Cost[a][i][element], now)
    }
  }

  return sum_cost
}

# The parcel cost
function get_parcel_element(a, p, element, now, adjusted) {
  # Adjusted or reduced cost?
  if (adjusted)
    # The adjusted parcel cost
    adjusted = get_parcel_tax_adjustment(a, p, element, now)
  else
    adjusted = 0

  # This elements costs
  return find_entry(Accounting_Cost[a][p][element], now) - adjusted
}

# The initial cost
function get_cash_in(a, i, now) {

  # Is the account open?
  if (now >= Held_From[a][i])
    # Yes - always element I
    return find_entry(Accounting_Cost[a][i][I], Held_From[a][i]) # The Held_From time ensures  that later element I costs do not impact the result

  # No - so no activity
  return 0
}

# The cash paid out of the asset when sold
function get_cash_out(a, i, now) {
  # We are only interested in the sale payment for this parcel - the zeroth element
  if (is_sold(a, i, now))
    # Each parcel can only be sold once - so if sold it is the first entry
    return get_parcel_proceeds(a, i)

  # Not sold yet
  return 0
}

# The cost reductions
function get_cost_modifications(a, p, now,  sum) {
  # This should exclude cash_in and cash_out
  sum = sum_cost_elements(Accounting_Cost[a][p], now)

  # Think about edge effects
  return sum - get_cash_out(a, p, now) - get_cash_in(a, p, now)
}

# A shorthand - ignores final cost
function get_parcel_cost(a, p, now, adjusted,    sum) {
  # Adjusted or reduced cost?
  # Reduced cost by default
  adjusted = ("" == adjusted) ? 0 : adjusted

  # Clunky
  if (0 != adjusted)
    # The adjusted parcel cost
    adjusted = sum_cost_elements(Tax_Adjustments[a][p], now)

  # This should exclude cash_in and cash_out
  sum = sum_cost_elements(Accounting_Cost[a][p], now) - adjusted

  # Remove the cash out component
  return sum - get_cash_out(a, p, now)
}

# Print out transactions
# Generalize for the case of a single entry transaction
function print_transaction(now, comments, a, b, u, amount, fields, n_field,     matched) {
  if (now > Stop_Time)
    return

  # Are we matching particular accounts?
  match_accounts(matched, Show_Account, a, b)
  if (!Show_Account || matched)
    # Print the transaction out
    printf "%s\n", transaction_string(now, comments, a, b, u, amount, fields, n_field, matched)
}

# Describe the transaction as a string
@ifdef EXPORT_FORMAT
# Export style
function transaction_string(now, comments, a, b, u, amount, fields, n_field, matched,      i, string, swop) {
  # Print statement
  # This could be a zero, single or double entry transaction
  #
  # # floating point precision
  # float_precision = ternary("" == float_precision, PRECISION, float_precision)

  # First the date
  string = sprintf("%11s", get_date(now))

  # Export format always makes the matched account account b - the DEBIT account
  if (match_account(a, Show_Account)) {
    # Swop accounts
    swop = b
    b = a
    a = swop

    # Reverse amount
    amount = -amount
  }

  # Is it not zero entry?
  if ("" != a)
    # At least single entry
    string = string sprintf(", %13s, ", Leaf[a])

  #
  if ("" != b)
    string = string sprintf("%13s, ", Leaf[b])

  # Cost element and cost entry
  if ("" != u)
    # This is the normal case
    string = string sprintf("%10s, %11.2f", u, amount)
  else
    # When export format is used and elements not provided
    string = string sprintf("%11.2f", amount)

  # Finish off the line
  for (i = 1; i <= n_fields; i ++)
    string = string ", " fields[i]
  string = string ", " comments

  # All done
  return string
} # End of printing a transaction

@else

# Normal style
# Describe the transaction as a string
function transaction_string(now, comments, a, b, u, amount, fields, n_fields, matched,     i, string) {
  # Print statement
  # This could be a zero, single or double entry transaction
  #

  # First the date
  string = sprintf("%11s", get_date(now))

  # Is it not zero entry?
  if ("" != a)
    # At least single entry
    string = string sprintf(", %13s, ", Leaf[a])

  # Is it double entry?
  if ("" != b)
    string = string sprintf("%13s, ", Leaf[b])

  # Cost element and cost entry - if at least one entry
  if (a || b) {
    string = string sprintf("%10s, %11.2f", u, amount)

    # Do we need to show the balance?
    if (matched)
      # From the start of the ledger
      string = string sprintf(", %14s", print_cash(get_cost(matched, now)))
    else
      # Optional Fields
      for (i = 1; i <= n_fields; i ++)
        string = string ", " fields[i]
  }

  # Finish off the line
  string = string ", " comments

  # All done
  return string
} # End of printing a transaction
@endif #// EXPORT_FORMAT

function initialize_account(account_name,     class_name, array, p, n,
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
  assert(class_name ~ RESERVED_CLASSES, "<" account_name "> is not a member of a recognized class")

  # Initialize this account
  # Now split the account name into a branch and a leaf
  assert(2 == split(account_name, array, ":"), sprintf("<%s> Account name %s is not in branch_name:leaf_name format", $0, account_name))

  # Finally an uninitialized long name
  # BUT there is another trap;
  # the case of a miss-spelt name?
  # eg Leaf[A.B.C:D] => D
  #    Long[D] => A.B.C:D
  # but we passed in account_name => "A.B.Z:D"?
  #
  # Actually let's allow this - but only in very restricted casee of the account never having been used
  # This might be possible some day...
  leaf_name = array[2]
  if ((leaf_name in Long_Name)) {
    if (Leaf[Long_Name[leaf_name]] == leaf_name) {
      # If the existing account is new (unused) it can be deleted
      assert(is_new(Long_Name[leaf_name]), sprintf("Account name %s: Leaf name[%s] => %s is already taken", account_name, leaf_name, Long_Name[leaf_name]))

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

  # refer  to the parent item eg parent[A.B.C] => *A.B (long_name minus short_name with a distinguishing prefix)
  p = Parent_Name[account_name] = "*" array[1]

  # How many components in the name "p"
  n = split(p, array, ".")

  # Initialize the cost bases for this account's parents
  while (p && !(p in Parent_Name)) {
    # a new meta-account - needs a cost-basis
    Cost_Basis[p][Epoch] = 0

    # Get p's parent - lose the last name component
    if (n > 1)
      Parent_Name[p] = get_name_component(p, 1, --n, array)
    else
      Parent_Name[p] = ""

    # Update p
    p = Parent_Name[p]
  }

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
    Accounting_Cost[account_name][0][0][SUBSEP] = 0
    zero_costs(Accounting_Cost[account_name][0], SUBSEP)
    for (p in Accounting_Cost[account_name][0])
      delete Accounting_Cost[account_name][0][p][SUBSEP]

    # Ditto for tax adjustments
    Tax_Adjustments[account_name][0][0][SUBSEP] = 0
    zero_costs(Tax_Adjustments[account_name][0], SUBSEP)
    for (p in Tax_Adjustments[account_name][0])
      delete Tax_Adjustments[account_name][0][p][SUBSEP]

    # p=-1 is not a real parcel
    Held_From[account_name][-1] = Epoch # This is needed by buy_units - otherwise write a macro to handle case of first parcel
    Parcel_Tag[account_name][SUBSEP] ; delete Parcel_Tag[account_name][SUBSEP] #

    # Keep track of units
    Total_Units[account_name][Epoch]     = 0
    Qualified_Units[account_name][SUBSEP]; delete Qualified_Units[account_name][SUBSEP]

    # Each parcel also has a number of parcels
    set_key(Number_Parcels, account_name, 0)

    # End of if ASSET
  } else if (is_class(account_name, "INCOME")) {
    # Set an Underlying_Asset if the leaf name
    # is of the appropriate format
    #
    # (DIV|DIST|FOR).LEAF => LEAF
    #
    if (is_linked(leaf_name)) {
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

  # Initialize account with common entries
  Cost_Basis[account_name][SUBSEP]; delete Cost_Basis[account_name][SUBSEP]

  # the account name is the long name
  return account_name
}

# Delete the links to an unused account
function unlink_account(a) {
  if (a in Leaf)
    delete Leaf[a]
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
    if (Held_From[a][p] > now)
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
      assert(open_key - Epoch >= 0, sprintf("%s: No earlier depreciation record than %s", get_short_name(a), get_date(now)))

      # The opening value - cost element I
      open_value = get_parcel_element(a, p, I, open_key)

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
        if (near_zero(get_parcel_tax_adjustment(a, p, I, now)))
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
  printf "%s: %s New Reduced Cost[%s] => %11.2f\n", "depreciate_now", get_short_name(a), get_date(now), get_reduced_cost(a, now) > STDERR
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

  # More than one tax band - much more tricky
  current_key = find_key(bands, now)
  tax_payable = last_threshold = 0
  for (threshold in bands[current_key]) {
    # The last band's width
    band_width = last_threshold - threshold # negative thresholds stored

    # Is this threshold below the total income?
    if (total_income < -threshold)
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
function get_taxable_income(now, tax_left,
                                 total_income, band_width, band_tax,
                                 current_key, last_threshold, threshold) {
  # Which band is the income in?
  if (near_zero(tax_left))
    return 0 # Could be any amount less than the tax free threshold

  # Now get the tax due on the whole sum
  current_key = find_key(Tax_Bands, now)
  last_threshold = 0
  total_income = 0
  for (threshold in Tax_Bands[current_key]) {
    # The last band's width
    band_width = last_threshold - threshold # negative thresholds stored

    # The maximum tax due in this band
    band_tax = band_width * Tax_Bands[current_key][last_threshold]

    # Is the tax_payable above the amount paid?
    if (band_tax >= tax_left) {
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
  if (tax_left > Epsilon)
    total_income += tax_left / Tax_Bands[current_key][last_threshold]

  # The minimum total income that would generate this much tax
  return total_income
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
	  if (c ~ /[0-9A-Za-z]/)
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
function read_date(date_string, hour,
                   date_fields, year, month, day, value) {
  # default time
  hour = ternary("" == hour, HOUR, hour)

  # default is YYYY-MM-DD

  if ("" == date_string) {
    Read_Date_Error = "Empty string"
    return DATE_ERROR
  }


  # Split the input date
  if (3 == split(trim(date_string), date_fields, "[-/ ]")) {
    # The fields are YYYY MM DD
    # or             YYYY Mon DD where Mon is a three char month abbreviation or a month name in English
    # or             Mon DD YYYY
    # or             DD MM YYYY if DD & YYYY are inconsistent with dates
    # year-month-day, monthname/day/year, monthname-day-year
    if (month = get_month_number(date_fields[1])) {
      day   = date_fields[2] + 0
      year  = date_fields[3] + 0
    } else if (month = get_month_number(date_fields[2])) {
      year  = date_fields[1] + 0
      day   = date_fields[3] + 0
    } else {
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

    # If still before the EPOCH this is an error
    if (year < EPOCH_START) {
      Read_Date_Error = "Date <" date_string "> is before epoch start <" get_date(Epoch) ">"
      return BEFORE_EPOCH
    }
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

  delete month_name
}

# Set a default epoch & future
function set_epoch() {
  # The Epoch
  # A more practical Epoch
  Epoch = mktime(EPOCH_START " 01 01 00 00 00", UTC)

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
    m = 3
    d = 1
  }

  # Get the time stamp
  return mktime(sprintf("%4d %02d %02d %02d 00 00", y, m, d, HOUR), UTC)
}
