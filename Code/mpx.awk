#!/usr/local/bin/gawk -f
# p.mpx.awk
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

# Use C include syntax for Header files


# assert --- assert that a condition is true. Otherwise, exit.

#
# Arnold Robbins, arnold@skeeve.com, Public Domain
# May, 1993

function assert(condition, string)
{
    if (! condition) {
        printf("%s:%d: assertion failed: %s\n",
            FILENAME, FNR, string) > "/dev/stderr"
        _assert_exit = 1
        exit 1
    }
}

END {
    if (_assert_exit)
        exit 1
}


# // FIXME: What does filefuncs do?
@load "filefuncs"

# // Control Logging




# // Logic conventions



# // Output Streams




# //





# // Some constants
# // @define CLEAR_ARRAY ("CLEAR_ARRAY")












# // Default State Record - no time fields


# // Constants for Real_Value keys










# // Default Cost Element


# // Output Date Formats





# // Default Reports


# // Default Reports












# // The stream to write reports to


# // Default Asset Prefix for Price Lists



# // Start Defining Special Account Names









# // The Epoch and minimum time difference









# // Day Number For Feb 29


# // Reserved Classes


# // Useful inline functions - this may be overdoing it





# // This is not efficient





#
# // Useful shorthands for various kinds of accounts
# // @define is_currency(a)  ((a) ~ /\.CURRENCY:/)
# // @define is_asset(a) ((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]/ || is_currency(a))
# // @define is_unitized(a) ((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/ || is_currency(a))
# // @define is_capital(a) ((a) ~ /^ASSET\.CAPITAL[.:]/ || is_currency(a))










# // Fixed asset




#
#
# // Contribution

# // Benefit

#
# // Pension or Income Stream



#
# // Match buy or sell transactions (pairs of accounts)



# //
# // The last component of a name is the suffix


# // Reserved Tax Offset Classes


# // Is a leaf name in a linked account format i.e. first component is
# // (DIV|DIST|FOR|GAINS).LEAF => LEAF


# // The current value of an asset
# // @define get_value(a, now) ternary(is_capital(a), find_entry(Price[a], now) * get_units(a, now), get_cost(a, now))

# // char code lookup


# //







# // Rounding etc


# // Not zero


# // Positive?


# // Yield x if x is positive, otherwise z


# // Negative?

# // Yield x if x is negative, otherwise z


# // Round to zero


# // Numerical comparisons


















# // Get a single transaction from the account




# // Units



# // Qualified units - reading is simple - no window qualified units equal all units


#// GST proportion at time (t)


# //
# // Date macros











# //
# // The length of a year ending at time (y, d)

# //
# //














# // Control precise timings of costs



# // Formatting



# // Include currency definitions
# // Include currency.h
# // Currency specification

# This is a C Style Header file
# It contains macros and definitions for the awk scripts
# MPX.h
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


# // Default Currency Symbol



#// Extra definitions for AUD
#// Add localized State Variables & Scalars
# // Add localized State Variables & Scalars




# // Carry Forward & Write Back Limits in Years









# // Capital Loss Window
# // Unlimited goes all the way to the Epoch
# // The write back limit


# // Multi-Line Macro
# // Gets the entries in the data which lie within the [block] (including end points)


# // Print a block of n identical characters



# // A local array
((SUBSEP in __MPX_ARRAY__)?((1)):((0)))

# // This assumes that the file:// protocol is in use - for a network protocol this will not work - so assume document is available



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
    value = ((__MPX_KEY__ = find_key(Dividend_Date[a],  now))?( Dividend_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Dividend_Date[a][0]):( 0))))
    discrepancy = now - value

    # The value cannot be later than the current time "now"
    if (value > now) {
      Read_Date_Error = "Payment date is later than current date"
      return ((Enforce_Qualification)?( (-1)):( (0)))

    } else if (((((discrepancy) - ( Epsilon)) <= 0) && (((discrepancy) - ( -Epsilon)) >= 0)))
      return (__MPX_KEY__)

    # Some times dividends are paid out of order, for example
    # a special or buyback dividend might have an extra
    # long qualification period - so look ahead more dividends
    # until the discrepancy increases
    #
    key = (__MPX_KEY__)
    while (key) {
      value = ((__MPX_KEY__ = find_key(Dividend_Date[a],  ((key) - 1)))?( Dividend_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Dividend_Date[a][0]):( 0))))
      if ((now - value) > discrepancy)
        # A worse match
        break

      # A better match
      discrepancy = now - value
      if (((((discrepancy) - ( Epsilon)) <= 0) && (((discrepancy) - ( -Epsilon)) >= 0)))
        return (__MPX_KEY__)

      # Save  this match
      key = (__MPX_KEY__)
    }

    # Best match was key
    if (discrepancy > 604800) {
      Read_Date_Error = "Failed to find a payment date within one week of current date"
      return ((Enforce_Qualification)?( (-1)):( (0)))
    }

    # Return it
    return key
  }

  # Failed to find a qualification date
  # Is enforcement strict?
  Read_Date_Error = "Failed to find any payment date"
  return ((Enforce_Qualification)?( (-1)):( (0)))
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
function read_value(x) {
  # Get the value first
  # Is it a number?
  if (x ~ /^[0-9\.\-]+$/)
    return strtonum(x)

  if (x ~ /^"([[:print:]])+"$/)
    x = ctrim(x, "\"")

  # Just return the field
  return x
}

# Read a state field
function read_field(field, x) {
  if ((( field in Time_Fields)?( Time_Fields[ field]):( ""))) {
    x = read_date(($field))
    assert((-1) != x, Read_Date_Error)
  } else
    x = ($field)

  return x
}


# A somewhat generalized variable reading function
##

function read_state(name, adjust_value, first_field, last_field,    i, x, value) {
  # The end of the input
  if (first_field > last_field)
    return "" # None read

  # The fields represent the keys & value
  value = read_value($last_field)

  # Logging


  # Is this a scalar?
  if (first_field == last_field) {
    # Yes
    if (adjust_value)
      SYMTAB[name] += adjust_value * value
    else
      SYMTAB[name]  = value



  } else {
    # The rest of the keys
    for (i = first_field; i < last_field; i ++)
      # The code can minimize output file by
      # retaining the old key if the "ditto"
      # symbol is encountered;
      if (($i != ("^")) || !(i in Variable_Keys))
        Variable_Keys[i] = read_field(i)

    # Set the array value
    set_array(SYMTAB[name], Variable_Keys, first_field, last_field - 1, value, adjust_value, (0))


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


  # Delete temporary key
  if (flag) {
    delete array[SUBSEP]   # delete scalar element
    flag = (0)
  }

  # Set the array recursively
  if (first_key == last_key) {
    # Set the value
    if (adjust_value)
      sum_entry(array, adjust_value * value, keys[first_key])
    else
      (array[ keys[first_key]] = ( value))
  } else {
    # Yikes
    # We need to ensure the subarray exists before calling set_array() otherwise
    # inside set_array() the entry would be a scalar, but then we need to delete
    # a[][] inside set_array() before trying to create a[][][] because
    # creating a[][] below creates IT as scalar. SUBSEP used arbitrarily.
    if (!((keys[first_key] in array) && (SUBSEP in array[keys[first_key]]))) {
      array[keys[first_key]][SUBSEP]   # create array a[] + scalar a[][]
      flag = (1)
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
    ((SUBSEP in Key_Index)?((1)):((0)))
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
  level = ((level)?( level):( 1))

  # write to output stream
  stream = ((stream)?( stream):( Write_State))

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
      last_key = ((("^"))?( ("^")):( key))

    # Go down one level or print deepest level
    if (isarray(arr[key]))
      walk_array(arr[key], level + 1, stream)
    else {
      # Finished at the base level
      # The output string
      for (i = 1; i < level; i++) {
        printf "%s%s", Key_Index[i], OFS > stream
        if (("^"))
          Key_Index[i] = ("^")
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
    if (((((v) - ( Epsilon)) <= 0) && (((v) - ( -Epsilon)) >= 0)))
      v = 0
    else # force floating point format
      v = sprintf("%.*f", (6), v)
  }

  # Return the formatted value
  if (v ~ /[[:space:]]/)
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

  for (key in array)
    return key
  # Empty array
  return ""
}

# delete duplicated values
function delete_duplicate_entries(array,      k, j, v, w) {

  for (j in array) {
    v = array[j]
    k = find_key(array, j - 1)
    if ("" == k) # Last key
      return
    w = array[k]

    # Look for duplicates
    if (((((v - w) - ( Epsilon)) <= 0) && (((v - w) - ( -Epsilon)) >= 0)))
      delete array[j]
  }

  # All done
}

# Add an entry to a time-ordered array
# This gives a running total
function sum_entry(array, x, now,   key, delta) {


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

  # If current sum > x  then sum -= x
  # If current sum <=x  then sum = 0


  for (key in array) {
    delta = array[key] - x
    if ((((delta) - ( Epsilon)) > 0)) {

      array[key] = delta
    } else {


      # Remove negligible and negative entries
      delete array[key]
    }
  }

  # Return TRUE if any entries were removed that were actually negative
  return (((delta) - ( -Epsilon)) < 0)
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
      if ((((key) - ( limit)) < 0)) {
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
  key = find_key(array, ((end_bracket) - 1))
  while (key > start_bracket) {
    # Save the maximum found
    max = (((max) - ( array[key]) > 0)?(max):( array[key]))

    # Get the next key
    key = find_key(array, ((key) - 1))
  } # Done

  # return the maximum value
  return max
}


# Some extras
# White space trimming
function ltrim(s) { sub(/^[ \t\r\n]+/, "", s); return s }
function rtrim(s) { sub(/[ \t\r\n]+$/, "", s); return s }
#function trim(s) { return rtrim(ltrim(s)); }
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

# Clear global values ready to read a new input record
function new_line(  key) {
  # Clear real values
  for (key in Real_Value)
    Real_Value[key] = 0

  Extra_Timestamp = (-1)
  Parcel_Name = ""
  Tax_Adjustment = (0)
  Cost_Element = ("II") # The default value
  Automatic_Depreciation = (0)
  GST_Claimable = 0
  Depreciation_Type = ""
  Comments = ""

  Account[1] = Account[2] = ""
  Transaction_Currency = (0)
  Translation_Rate = 1
}

# Accounting format printing
function print_cash(x,   precision) {
  precision = (("" == precision)?( (2)):( precision))

  if ("" == x)
    return "  -  "
  if (((((x) - ( Epsilon)) <= 0) && (((x) - ( -Epsilon)) >= 0)))
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
      if (Enforce_Qualification) {
        assert($i in Long_Name, "Currency Code <" $i "> not defined - provide exchange rates")
        Transaction_Currency = Long_Name[$i]
      } else if ($i in Long_Name)
        Transaction_Currency = Long_Name[$i]
      else {
        printf "## Warning Ignoring Unknown Currency Code %s - treating as Journal Currency %s\n", $i, Journal_Currency > "/dev/stderr"
        Transaction_Currency = Long_Name[Journal_Currency]
      }


      # Is this a simple currency translation -
      j = i + 1
      assert($j ~ /^[0-9\.\-]+$/, "<" $0 "> Unexpected syntax: cash amount <" $j "> is not a number")

      # is this a purchase or a sale of currency units?
      if (Account[2] == Transaction_Currency)
        Real_Value[(-1)] = $i = strtonum($j) # A purchase (of forex)
      else if ((Account[1] == Transaction_Currency) && is_open(Account[1], now)) {
        $i = strtonum($j) # A sale
        Real_Value[(-2)] = $j = - $i # A sale (of forex)
      } else { # Other cases
        # Rebuild line - remove currency code and shuffle down fields
        for (j = i; j < NF; j ++)
          $j = $(j + 1)
        NF --
      }

      # Get translation price
      if (Transaction_Currency in Price)
        Translation_Rate = ((__MPX_KEY__ = find_key(Price[Transaction_Currency],  now))?( Price[Transaction_Currency][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[Transaction_Currency][0]):( 0))))
      else
        Translation_Rate = ""

      # Did we get a price?
      assert("" != Translation_Rate, "No exchange rate available for <" Transaction_Currency "> at <" get_date(now) ">")
    } else {
      # Use default currency
      Transaction_Currency = (0)
      Translation_Rate = 1
    }

    # Check that this is a number
    amount = strtonum($i)

    # Zero j
    j = ((Real_Value[(-1)] || Real_Value[(-2)])?( -1):( 0))
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
        if ((((x) - ( Epsilon)) > 0) && ((( Account[2]) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/) || ((Account[1]) ~ /^EQUITY[.:]/))) {
          # Interpret these as units
          Real_Value[(0)] = x
        } else if ((((x) - ( -Epsilon)) < 0) && (((( Account[1]) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/) && is_open( Account[1], now)) || ((( Account[2]) ~ /^EQUITY[.:]/) && is_open( Account[2], now)))) {
          Real_Value[(0)] = x
        } else # This is not Units so it is the next possible real value, index 1
          Real_Value[j = 1] = x
      } else
        Real_Value[j] = x
    } else if ("" != x) { # Will be "" when a timestamp set
      # x not set so look for strings
      # Can reuse x
      x = parse_optional_string($i, (1))

      # Treat as a comment
      if (x)
        Comments = (("" == Comments)?(  x):( (("" ==  x)?( Comments):( (Comments  OFS  x)))))
    }

    # Increment i & j
    i ++
    j ++
  }

  # Comments should be signified with an octothorp
  if (Comments !~ /^#/)
    Comments = (("" == "# ")?(  Comments):( (("" ==  Comments)?( "# "):( ("# "  OFS  Comments)))))

  # Documents can be added as comments
  # Some special document names are supported
  # So for example [B] expands to ABC Buy YYYY Mon
  for (x in Documents) {
    delete Documents[x]

    # Parse this document name
    i = parse_document_name(x, now)

    # Add the parsed name to the comments
    Comments = (("" == Comments)?(  i):( (("" ==  i)?( Comments):( (Comments  OFS  i)))))
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
  use_format = ("%Y %b")    

  # Split the code name
  # Use name component because we want to capture all the components apart from the first
  if (split(name, array, Document_Shortcut, seps) > 1) {
    suffix = get_name_component(name, 2, -1, array)
    suffix_set = (1)

    # What was the seperator?
    if ("+" == seps[1])
      # Use year format
      use_format = ("%Y")       
  } else
    suffix_set = (0)

  #
  prefix = get_name_component(name, 1, 1, array)
  account_name = ""
    #
    switch (prefix) {
      case "B" :
      case "S":
      case "H":

        # Is this a buy, sell or holding statement?
        if (Real_Value[(0)] < 0) {
          prefix = (("H" == prefix)?( "Holding Statement"):( "Sell"))
          account_name = get_name_component(Leaf[Account[1]], 1)
        } else {
          prefix = (("H" == prefix)?( "Holding Statement"):( "Buy"))
          account_name = get_name_component(Leaf[Account[2]], 1)
        }

        # Add the date
        prefix = prefix (( use_format)?( (" " get_date(now,  use_format))):( ""))
      break;;

      case "D": # Distribution
        if (((Leaf[Account[1]]) ~ /^(DIV|DIST|FOR|GAINS)\./))
          account_name = get_name_component(Leaf[Account[1]], 2)
        else
          account_name = get_name_component(Leaf[Account[1]], 1)

        # The second component of the account name (unless this is accrued income)
        if (((Account[1]) ~ ("^" ( "ASSET.CURRENT.ACCRUED") "[.:]")))
          prefix = "Distribution" (( use_format)?( (" " get_date(now,  use_format))):( ""))
        else
          prefix = tolower(get_name_component(Account[1], 2)) (( use_format)?( (" " get_date(now,  use_format))):( ""))
        break;;
      case "I":  # Income
        #account_name = get_name_component(Leaf[Account[1]], 1)

        # The second component of the account name is not used here...?
        prefix = "Income" (( use_format)?( (" " get_date(now,  use_format))):( ""))
        break;;

      case "C":
      case "E": # Expense or Cost
        #account_name = get_name_component(Leaf[Account[2]], 1)

        # The second component of the account name is not used here...?
        prefix = "Expense" (( use_format)?( (" " get_date(now,  use_format))):( ""))
        break;;

      case "T": # Annual Tax Statement
        account_name = get_name_component(Leaf[Account[2]], 1)
        prefix = "Annual Tax Statement" (( ("%Y")       )?( (" " get_date(now,  ("%Y")       ))):( ""))
        break;;

      default: # no match - assume this is a literal string
        # When a distinct suffix is present add the date
        if (suffix_set)
          prefix = (("" == prefix)?(  (( use_format)?( (" " get_date(now,  use_format))):( ""))):( (("" ==  (( use_format)?( (" " get_date(now,  use_format))):( "")))?( prefix):( (prefix   (( use_format)?( (" " get_date(now,  use_format))):( "")))))))

        account_name = ""
        break;;
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
      prefix = account_name " " ((prefix)?( (toupper(substr(prefix, 1, 1)) substr(prefix, 2))):( (prefix)))

    # Final parsed document name
    prefix = (("" == prefix)?(  suffix):( (("" ==  suffix)?( prefix):( (prefix   suffix)))))

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
  if ((("file://" == Document_Protocol)?( stat(Document_Root string "." filetype, __MPX_ARRAY__)):( (0))))
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
  if ((-1) == Extra_Timestamp) {
    Extra_Timestamp = read_date(field)

    # The timestamp interpretation takes precedence
    if ((-1) != Extra_Timestamp)
      # If a time-stamp is set this will return ""
      return ""
  }

  # Next - is it a numerical value?
  value = strtonum(field)

  # This is a value
  if (((((value) - ( Epsilon)) <= 0) && (((value) - ( -Epsilon)) >= 0)))
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
    field = (toupper(substr(field, 2, len - 2)))
    if (field ~ /D/) {
      Tax_Adjustment = Automatic_Depreciation = (1)
      Cost_Element = I
      return ""
    }

    # This is probably a tax adjustment
    adjustment_flag = (1)
  } else # Not a tax adjustment
    adjustment_flag = (0)

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
  string = ctrim(field, "[", "]")
  if (string == field) {
    # Parcel names are enclosed by single quotes
    if (field ~ /^"([[:print:]])+"$/) {
      # A Parcel name
      Parcel_Name = field # With quotes
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
  initialize_account(("SPECIAL.TAXABLE.GAINS.LONG")  ":LONG.GAINS")
  initialize_account(("SPECIAL.TAXABLE.LOSSES.LONG") ":LONG.LOSSES")
  initialize_account(("SPECIAL.TAXABLE.GAINS.SHORT") ":SHORT.GAINS")
  WRITTEN_BACK   =   initialize_account(("SPECIAL.TAXABLE.LOSSES.SHORT") ":SHORT.LOSSES")

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
  format = (("" == format)?( DATE_FORMAT):( format))

  # ok - if format is zero use timestamp
  if (0 == format)
    return time
  if (time == Future)
    return "     -     "
  return strftime(format, time, UTC)
}

# Absolute hour for today, tomorrow and yesterday
function today(time, hour,    midnight) {
  hour = (("" == hour)?( (12)):( hour))

  # Need to get midnight local time
  midnight = (86400) * int(time / (86400))

  return midnight + (3600) * hour
}

function tomorrow(time, hour) {
  hour = (("" == hour)?( (12)):( hour))

  return today(time, hour) + (86400)
}

function yesterday(time, hour) {
  hour = (("" == hour)?( (12)):( hour))

  return today(time, hour) - (86400)
}

# Length of year ending / starting (now)
function one_year(now, sense,     year, day, sum) {
  # By default one year backward!
  sense = ((!sense)?( -1):( sense))
  # Sense > 0 is forward sense years
  # Sense < 0 is back sense years

  # Get the day number
  day = (strftime("%j", (now), UTC) + 0)

  # Which Calendar year is this?
  year = (strftime("%Y", (now), UTC) + 0)
  if (sense > 0)
    year += sense
  else
    sense = - sense

  # Go back n years
  sum = 0
  while (sense -- > 0) {
    sum += (((( day) <= (60))?( ((((year) - 1) % 4 == 0 && ((year) - 1) % 100 != 0) || ((year) - 1) % 400 == 0)):( (((year) % 4 == 0 && (year) % 100 != 0) || (year) % 400 == 0))) + 365)
    year --
  }

  # Get the length in seconds
  return (86400) * sum
}

# Useful account filters
function is_open(a, now,     p) {
  # An asset is open if there are unsold parcels at time 'now'
  if (((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/))
    for (p = 0; p < Number_Parcels[a]; p ++) {
      if ((((Held_From[a][p]) - ( now)) > 0))
        break
      if ((Held_Until[a][ p] > ( now)))
        return (1)
    }
  return (0)
}

# Is an account a an ancestor of another account b?
function is_ancestor(a, b,    p) {
  if (!((a) ~ ("*")))
    return (0)

  # Check
  p = Parent_Name[b]
  while (("*") != p) {
    if (a == p) # Found
      return (1)
    p = Parent_Name[p]
  }

  # Not found
  return (0)
}

# The last parcel sold before "now" - assumes an asset is open
function held_to(ac, now,     p, latest_sale) {
  # When an asset was held to
  latest_sale = Epoch

  for (p = 0; p < Number_Parcels[ac]; p++)
    if ((Held_Until[a][ p] <= ( now)))
      latest_sale = (((latest_sale) - ( Held_Until[ac][p]) > 0)?(latest_sale):( Held_Until[ac][p]))
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
    return (0)

  # A particular account matches
  if (show_name == a)
    return a

  # If the class name is another account it does not match
  if (show_name in Leaf)
    return (0)

  # Otherwise is this an account prefix?
  if (((a) ~ ("^" ( show_name) "[.:]")))
    return ("*" show_name)
}

# Basic transactions
# Adjust the cost base (including the adjusted and reduced cost bases)
function adjust_cost(a, x, now, tax_adjustment,     i, adjustment, flag) {
  # This argument should always be provided - using a global variable to pass information is confusing
  tax_adjustment = (("" == tax_adjustment)?( Tax_Adjustment):( tax_adjustment))

  # if an asset is adjusted note the adjustment in
  # each active parcel
  # Adjustments for units bought
  if (((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/)) {

    # What proportion of the sum is allocated to each unit at time now?

    # Either divide adjustment between all open parcels OR
    # concentrate with a parcel with the same timestamp
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if ((((Held_From[a][i]) - ( now)) > 0)) # All further transactions occured after (now)
        break # All done
      if (Held_From[a][i] == now) {
        if (!(( a in Parcel_Tag) && ( i in Parcel_Tag[ a])) || (Parcel_Name == Parcel_Tag[a][i])) {
          # The adjustment is pooled explicitly with this parcel
          adjust_parcel_cost(a, i, now, x, Cost_Element, tax_adjustment)


          # Also record the parents cost
          update_cost(a, x, now)
          return # Only one parcel is adjusted - it must be unsold if only just purchased
        }
      }
    }

    # The cost adjustment per unit except for depreciating assets
    if (flag = ((a) ~ /^ASSET\.FIXED[.:]/))
      adjustment = x / get_cost(a, now)
    else {
      assert(((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0)), "Asset <" Leaf[a] "> has zero units - ensure this transaction occurs before it was sold")
      adjustment = x / ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0))
    }

    # Debugging


    # Scan back down the parcels held and unsold at time now
    while (i -- > 0) {
      if ((Held_Until[a][ i] > ( now))) # This is an unsold parcel at time (now)
        # The parcel adjustment is proportional to the parcel size unless it is a depreciating asset
        if (flag)
          adjust_parcel_cost(a, i, now, get_parcel_cost(a, i, now) * adjustment, Cost_Element, tax_adjustment)
        else
          adjust_parcel_cost(a, i, now, Units_Held[a][i] * adjustment, Cost_Element, tax_adjustment)
    } # End of each parcel

    # Debugging

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
  if (("*") == p)
    return # Finished

  # Update the cost
  if (p in Cost_Basis)
    sum_entry(Cost_Basis[p], x, now)
  else
    Cost_Basis[p][now] = x

  # Logging

  update_cost(p, x, now)
}

function adjust_parcel_cost(a, p, now, parcel_adjustment, element, adjust_tax,
                            parcel_cost,
                            held_time) {
  # Ignore negligible adjustments
  if (((((parcel_adjustment) - ( Epsilon)) <= 0) && (((parcel_adjustment) - ( -Epsilon)) >= 0)))
    return



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
    if ((((parcel_adjustment) - ( Epsilon)) > 0))
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
  if (!((a) ~ /^EQUITY[.:]/)) {
    #   Ensure that the parcel cost base is not negative
    parcel_cost = get_parcel_cost(a, p, now)
    if ((((parcel_cost) - ( -Epsilon)) < 0)) {


      # Get the tax adjustment - this will influence the taxable gains
      parcel_adjustment = ((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  now))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))

      # If the overall parcel cost is (P)
      # and if the cost of this element is now (E)
      # then the cost of the other elements is (P-E)
      # so that an overall zero parcel cost is achieved if this element has cost (E-P)
      sum_entry(Accounting_Cost[a][p][element], - parcel_cost, now)

      # This will create a capital gain
      adjust_cost(REALIZED_GAINS, parcel_cost, now)



      # The capital gain needs to be balanced in the asset sums
      update_cost(a, -parcel_cost, now)

      # Update tax adjustment too
      if ((((parcel_adjustment) - ( parcel_cost)) < 0))
        sum_entry(Tax_Adjustments[a][p], parcel_cost, now)
      else {
        parcel_cost -= parcel_adjustment

        # This tax adjustment has been used
        (Tax_Adjustments[a][p][ now] = ( 0))

        # Need to record taxable gains/losses too
        held_time = get_held_time(now, Held_From[a][p])
        if ((((held_time) - ( 31622400)) >= 0)) {
          if (!(a in Long_Gains))
            Long_Gains[a] = initialize_account(("SPECIAL.TAXABLE.GAINS.LONG") ":LG." Leaf[a])
          adjust_cost(Long_Gains[a], parcel_cost, now)
        } else {
          if (!(a in Short_Gains))
            Short_Gains[a] = initialize_account(("SPECIAL.TAXABLE.GAINS.SHORT") ":SG." Leaf[a])
          adjust_cost(Short_Gains[a], parcel_cost, now)
        }

        # Balance taxable gains
        adjust_cost("*SPECIAL", - parcel_cost, now)
      }
    }
  }


} # End of adjust_parcel_cost

# The idea of the "cost" of the account
# This is the same as the reduced cost
# Returns 0 for sold assets
# What would happen if REALIZED were not populated and it returned the gains/losses for sold assets?
#
function get_cost(a, now,     i, sum_cost) {
  # Adjustments for units bought
  if (((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/)) {
    # Initial cost
    sum_cost = 0

    for (i = 0; i < Number_Parcels[a]; i ++) {
      if ((((Held_From[a][i]) - ( now)) > 0)) # All further transactions occured after (now)
        break # All done
      if ((Held_Until[a][ i] > ( now))) # This is an unsold parcel at time (now)
        sum_cost += sum_cost_elements(Accounting_Cost[a][i], now) # cost elements
    }
    return sum_cost
  } else if (a in Cost_Basis) # Cash-like
    return ((__MPX_KEY__ = find_key(Cost_Basis[a],  now))?( Cost_Basis[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Cost_Basis[a][0]):( 0))))

  return 0
}

# One liner function
function get_value(a, now) {
  return ((((a) ~ /^ASSET\.CAPITAL[.:]/))?( ((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0)))) * ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0))):( get_cost(a, now)))
}


# The tax adjustments at time (now)
# Note that depreciation is always a tax adjustment
function get_cost_adjustment(a, now,   i, sum_adjustments) {
  # Initial adjustments
  sum_adjustments = 0

  # Adjustments for units bought
  # Do not apply to equities
  if (((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if ((((Held_From[a][i]) - ( now)) > 0)) # All further transactions occured after (now)
        break # All done
      if ((Held_Until[a][ i] > ( now))) # This is an unsold parcel at time (now)
        sum_adjustments += ((__MPX_KEY__ = find_key(Tax_Adjustments[a][i],  now))?( Tax_Adjustments[a][i][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][i][0]):( 0))))
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
  if ((!is_open((a), ( now))))
    return 0 # No unrealized gains

  if (((a) ~ /^ASSET\.CAPITAL[.:]/))
    gains = get_cost(a, now) - ((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0)))) * ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0))
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
  if (((a) ~ /^ASSET\.CAPITAL[.:]/)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if ((((Held_From[a][i]) - ( now)) > 0)) # All further transactions occured after (now)
        break # All done
      if ((Held_Until[a][ i] <= ( now))) # This is a sold parcel at time (now)
        gains += (Parcel_Proceeds[a][ i]) + sum_cost_elements(Accounting_Cost[a][i], now) # All cost elements
    }
  } else
    gains = 0

  return gains
}

# Sum only the cost elements
function sum_cost_elements(array, now,     sum_elements, e) {


  sum_elements = 0
  for (e in array) # Exclude element [0]
    sum_elements += ((__MPX_KEY__ = find_key(array[e],  now))?( array[e][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( array[e][0]):( 0))))
  return sum_elements
}

# The initial cost
function get_cash_in(a, i, now) {

  # Is the account open?
  if ((((now) - ( Held_From[a][i])) >= 0))
    # Yes - always element I
    return (((__MPX_KEY__ = find_key(Accounting_Cost[a][ i][ I],  ( Held_From[a][i])))?( Accounting_Cost[a][ i][ I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][ i][ I][0]):( 0))))) # The Held_From time ensures  that later element I costs do not impact the result

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
    sum -= ((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  now))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))

  # The parcel cost
  return sum
}

# Print out transactions
# Generalize for the case of a single entry transaction
function print_transaction(now, comments, a, b, amount, element_string, fields, n_fields,     matched, i, string) {
  if ((((now) - ( Stop_Time)) > 0))
    return

  # Are we matching particular accounts?
  ((matched= Show_Account)?(((matched=match_account( a,  Show_Account))?( matched):( matched=match_account( b,  Show_Account)))):(matched=""))
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
    if (element_string && element_string != ("II"))
      string = string sprintf("%10s", element_string)
    else # Pretty print
      string = string sprintf("%10s", "")

    # Do we need to show the balance?
    if (matched)
      # From the start of the ledger
      string = string sprintf("%s %14s", OFS, print_cash(((((matched) ~ /^ASSET\.CURRENT\.CURRENCY[.:]/))?( ((matched in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[matched],   now))?( Total_Units[matched][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[matched][0]):( 0))))):( 0))):( get_cost(matched, now)))))
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
  class_name = get_name_component(account_name, (1))

  # Initialize this account
  # Now split the account name into a branch and a leaf
  if (class_name ~ /ASSET|EQUITY|EXPENSE|INCOME|LIABILITY|SPECIAL|BALANCING/) {
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
  if (((account_name) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/)) { # This could include EQUITY too
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
  } else if (((account_name) ~ ("^" ( "INCOME") "[.:]"))) {
    # Set an Underlying_Asset if the leaf name
    # is of the appropriate format
    #
    # (DIV|DIST|FOR|GAINS).LEAF => LEAF
    #
    if (((Leaf[account_name]) ~ /^(DIV|DIST|FOR|GAINS)\./)) {
      # Probably a better way to do this using a regex
      linked_name = get_name_component(leaf_name, 2, -1)

      # Call initialize account to ensure this account is initialized
      Underlying_Asset[account_name] = initialize_account(linked_name)


    }
  }

  # refer  to the parent item eg parent[A.B.C] => *A.B (long_name minus short_name with a distinguishing prefix)
  p = Parent_Name[account_name] = ("*") array[1]

  # How many components in the name "p"
  n = split(p, array, ".")

  # Initialize the cost bases for this account's parents
  while (!(p in Parent_Name)) {
    # Get p's parent - lose the last name component
    if (n > 1)
      Parent_Name[p] = get_name_component(p, 1, --n, array)
    else
      Parent_Name[p] = ("*")

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
  assert(((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/), "split_account: <" a "> not a unitized account")

  # Set split factor
  split_factor = ((split_factor)?( split_factor):( 1))

  # Label
  label = ((split_factor > 1)?( "Split "):( ((split_factor < 1)?( "Merge "):( "Change"))))

  # Write to tranaction file
  printf "##\n"
  printf "## %s %s => %s by factor %7.2f\n", label, Leaf[a], Leaf[b], ((split_factor < 1)?( 1.0 / split_factor):( split_factor))
  printf "##   Date => %s\n", get_date(now)
  printf "##   %s Cost            => %s\n", Leaf[a], print_cash(get_cost(a, now))
  printf "##   %s Units           => %10.3f\n", Leaf[a], ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0))
  printf "##   %s Qualified Units => %10.3f\n", Leaf[a], ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[a],   now))?( Qualified_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[a][0]):( 0))))):( ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],    now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0))))

  # Copy parcels
  Number_Parcels[b] = Number_Parcels[a]

  # Get each open parcel in account a and copy it to account b
  for (p = 0; p < Number_Parcels[a]; p ++) {
    # Is this parcel purchased yet?
    if ((((Held_From[a][p]) - ( now)) > 0))
      # This is an error
      assert((0), "Cannot " tolower(label) " <" Leaf[a] "> before all its transactions are complete")

    # Copy the parcel - this invocation copies from one account to another
    copy_parcel(a, p, b, p)

    # Adjust units in q if split_factor is not unity
    if (1 != split_factor)
      Units_Held[b][p] *= split_factor

    # if accounts a and b are different record this
    if (a != b)
      Account_Closed[a] = now

    # Close down pre-split account - at cost so no gains
    if ((Held_Until[a][ p] > ( now))) {
      (Parcel_Proceeds[a][ p] = ( - get_parcel_cost(a, p, now)))
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
  if (((a) ~ /^ASSET\.FIXED[.:]/)) {
    if (a in Method_Name)
      Method_Name[b] = Method_Name[a]
    if (a in Lifetime)
      Lifetime[b] = Lifetime[a]
  }

  # Price records
  if (a in Price) {
    for (key in Price[a])
      if ((((key) - ( now)) <= 0))
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
      if ((((key) - ( now)) > 0)) {
        Dividend_Date[b][key] = Dividend_Date[a][key]
        delete Dividend_Date[a][key]
      }

  # All done
  printf "##   After %s\n", label
  printf "##   %s Cost            => %s\n", Leaf[b], print_cash(get_cost(b, now))
  printf "##   %s Units           => %10.3f\n", Leaf[b], ((b in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[b],   now))?( Total_Units[b][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[b][0]):( 0))))):( 0))
  printf "##   %s Qualified Units => %10.3f\n", Leaf[b], ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[b],   now))?( Qualified_Units[b][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[b][0]):( 0))))):( ((b in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[b],    now))?( Total_Units[b][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[b][0]):( 0))))):( 0))))
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
    print Journal_Title > "/dev/stderr"
    printf "%s Data Held Report for Period Ending %s\n\n", name, get_date(now)  > "/dev/stderr"

    earliest_key = Future
    latest_key   = Epoch
  }

  # list holding "blocks" - ie non-overlapping holding periods
  # Each block is preceeded and/or followed by "gaps"
  for (a in Leaf)
    if ((a in data_array) && ((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/))
      if ((Held_From[a][0] > Epoch)) {
        # Get each parcel in turn and list the contiguous blocks of time held
        start_block = Held_From[a][0]
        end_block = Held_Until[a][0]
        block_id = 0
        for (p = 1; p < Number_Parcels[a]; p ++) {
          # This starts a new holding block if the purchase date is after the current end date
          if ((((Held_From[a][p]) - ( end_block)) > 0)) {
            # Check the data against each block
            for (key in  data_array[a]) {  if (key -  end_block > 0)    continue;  if (key -  start_block >= 0)    stack[key] =  data_array[a][key];  else    break;}

            # Remove originals of copies data to speed up processing of remaining entries
            for (key in stack)
              delete data_array[a][key]

            # A new block
            block_id ++
            start_block = Held_From[a][p]
            end_block = Held_Until[a][p]
          } else if ((((Held_Until[a][p]) - ( end_block)) > 0)) # extend the old block
            end_block = Held_Until[a][p]

          # If this parcel is open we have completed all possible blocks
          if ((Held_Until[a][ p] > ( now)))
            break
        } # End of each parcel p

        # Check the data against each block
        for (key in  data_array[a]) {  if (key -  end_block > 0)    continue;  if (key -  start_block >= 0)    stack[key] =  data_array[a][key];  else    break;}
        if (show_blocks)
          # Get first key
          for (first_key in stack) {
            # Record latest key
            latest_key = (((latest_key) - ( first_key) > 0)?(latest_key):( first_key))
            break
          }

        # Some simple formatting
        if (Show_Extra && show_blocks)
          printf "\n" > "/dev/stderr"

        # Copy the kept items back
        for (key in stack) {
          data_array[a][key] = stack[key]

          # Show this data when detailed reporting is enabled
          if (Show_Extra && show_blocks)
            printf "%22s\t %s => %s\n", Leaf[a], get_date(key), format_value(stack[key]) > "/dev/stderr"
        }

        # get last key and show range of keys
        if (show_blocks && (key in stack)) {
          s = ((is_open(a, now))?( "*"):( ""))
          if (key != first_key)
            # More than one key
            printf "%22s\t[%s, %s]%s\n", Leaf[a], get_date(key), get_date(first_key), s > "/dev/stderr"
          else if (!Show_Extra)
            # Only one key in this block - already recorded if Show_Extra set
            printf "%22s\t[%s]%s\n", Leaf[a], get_date(first_key), s > "/dev/stderr"

          # Record earliest key
          earliest_key = (((earliest_key) - ( key) < 0)?(earliest_key):( key))
        }

        # Clean up
        delete stack

      } else {
        # Never held!
        if (show_blocks)
          printf "%22s\tNever Held!\n", Leaf[a] > "/dev/stderr"

        unlink_account(a)
      }
    # End of each asset a

    # Final Summary
    if (show_blocks) {
      underline(44, 6, "/dev/stderr")
      printf "%22s\t[%s, %s]\n\n", name, get_date(earliest_key), get_date(latest_key) > "/dev/stderr"
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
  assert(is_open(a, now), sprintf("depreciate_now: Can't depreciate closed asset %s", (Leaf[a])))

  # Depreciating assets only use cost elements I or II


  # First pass at setting depreciation factor
  first_year_factor = (0)
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
    if ((((Held_From[a][p]) - ( now)) > 0))
      break # All done
    if ((Held_Until[a][ p] > ( now))) {
      # First see if depreciation already was computed for time (now)
      if (now in Accounting_Cost[a][p][I]) {
        # Already depreciated

        continue # Get next parcel
      }

      # Now we need the opening value for this parcel - always cost element I
      open_key = find_key(Accounting_Cost[a][p][I], ((now) - 1))
      assert((((open_key) - ( Epoch)) >= 0), sprintf("%s: No earlier depreciation record than %s", (Leaf[a]), get_date(now)))

      # The opening value - cost element I
      open_value = Accounting_Cost[a][p][I][open_key]



      # Refine factor at parcel level
      if (first_year_factor) {
        # First year sometimes has modified depreciation
        if (((((((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  now))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))) - ( Epsilon)) <= 0) && (((((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  now))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))) - ( -Epsilon)) >= 0)))
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
        delta = ((now - open_key) / (86400)) * factor / FY_Length
      }



      # This block is the only difference between prime cost and diminishing value
      if ("PC" != Method_Name[a]) # Diminishing Value or Pool (not Prime Cost)
        delta *= open_value

      # Check for negligible or negative value
      if (!(((open_value - delta) - ( Epsilon)) > 0))
        delta = open_value

      # Adjust the parcel cost - element I
      adjust_parcel_cost(a, p, now, -delta, I, (1))
      sum_delta += delta


    } # End of if unsold parcel
  } # End of each parcel



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
  invert_array(bands[current_key])

  # Compute tax
  tax_payable = last_threshold = 0
  for (threshold in bands[current_key]) {
    # The last band's width
    band_width = last_threshold - threshold # negative thresholds stored

    # Is this threshold below the total income?
    if ((((total_income) - ( -threshold)) < 0))
      break

    # Update the last threshold
    if (!((((band_width) - ( Epsilon)) <= 0) && (((band_width) - ( -Epsilon)) >= 0)))
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
  if (!(((tax_left) - ( Epsilon)) > 0)) {
    # If the first band has a zero rate no income is assumed
    if (((((bands[current_key][last_threshold]) - ( Epsilon)) <= 0) && (((bands[current_key][last_threshold]) - ( -Epsilon)) >= 0)))
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
    if ((((band_tax) - ( tax_left)) >= 0)) {
      # The tax actually accruing from this band is tax_left
      # so the income lying in this band is simply x
      total_income += tax_left * band_width / band_tax
      break
    }

    # Reduce tax left
    tax_left -= band_tax
    if (((((tax_left) - ( Epsilon)) <= 0) && (((tax_left) - ( -Epsilon)) >= 0)))
      break

    # Get the next band
    last_threshold = threshold
  }

  # We can still have have tax unaccounted for here
  if ((((tax_left) - ( Epsilon)) > 0))
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



  # Yes - what order do these keys have?
  if (((((((last_key) < 0)?( -(last_key)):(last_key))) - ( (((key) < 0)?( -(key)):(key)))) < 0))
    # Desired order
    return



  # Wrong order!
  for (key in array)
    if ((((key) - ( Epsilon)) > 0)) {
      # Reverse sign of keys to enforce required ordering
      value = array[key]
      delete array[key]
      array[-key] = value

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
	    url = "%" sprintf("%02X", ((c in URL_Lookup)?( URL_Lookup[c]):( (0)))) url
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
    h = (prime * h + ((chars[i] in URL_Lookup)?( URL_Lookup[chars[i]]):( (0)))) % modulo

  # Tidy up array
  delete chars

  # Return hash h
  return sprintf("%05x", h)
}

# print underline
function underline(width, margin, stream) {
 stream = ((stream)?( stream):( "/dev/stdout"))
 if (margin)
   printf "%*s", margin, "" > stream
 if ((1)) {while ( width-- > 1) printf "%1s", "_" >  stream; print "_" >  stream}
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
  hour = (("" == hour)?( (12)):( hour))

  # default is YYYY-MM-DD

  if ("" == date_string) {
    Read_Date_Error = "Empty string"
    return (-1)
  }


  # Split the input date
  if (3 == split((date_string), date_fields, "[-/ ]")) {
    # The fields are YYYY MM DD
    # or             YYYY Mon DD where Mon is a three char month abbreviation or a month name in English
    # or             Mon DD YYYY
    # or             DD MM YYYY if DD & YYYY are consistent with dates
    # year-month-day, monthname/day/year, monthname-day-year
    if (month = (((date_fields[1]) in Lookup_Month)?( Lookup_Month[date_fields[1]]):( 0))) {
      day   = date_fields[2] + 0
      year  = date_fields[3] + 0
    } else if (month = (((date_fields[2]) in Lookup_Month)?( Lookup_Month[date_fields[2]]):( 0))) {
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
          return (-1)
        }
      } # end of bad day number

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
          return (-1)
        }
      } # end of bad day number
    }

    # Assume year < 1000 is a reference to a relative year number
    # In practice this means the 3rd Millenium
    if (year < 1000)
      year += (2000)

    # If still before the EPOCH this is an error
    if (year < (2000)) {
      Read_Date_Error = "Date <" date_string "> is before epoch start <" get_date(Epoch) ">"
      return (-2)
    }
  } else {
    Read_Date_Error = "Can't parse date <" date_string "> wrong number of fields"
    return (-1)
  }

  # Use mktime
  value = mktime(sprintf("%4d %02d %02d %02d 00 00", year, month, day, hour), UTC)
  if (value < 0) {
    Read_Date_Error = "A negative timestamp found"
    return ((-1))
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
  Carried_Loss_Limit = Epoch = mktime((2000) " 01 01 00 00 00", UTC)

  # A distant Future
  Future = mktime((2999) " 12 31 00 00 00", UTC)
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
  if (2 == m && 29 == d && !(((y) % 4 == 0 && (y) % 100 != 0) || (y) % 400 == 0)) {
    # February 29 can only exist in a leap year
    # Reset m & d to March 1st
    ## This might be dealt with by mktime automatically
    m = 3
    d = 1
  }

  # Get the time stamp
  return mktime(sprintf("%4d %02d %02d %02d 00 00", y, m, d, (12)), UTC)
}

#!/usr/local/bin/gawk -f
# p.eofy.awk
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
# Use C include syntax for Header files




# Handle EOFY processing for mpx
function eofy_actions(now,      past, allocated_profits,
                                benefits) {


  # past is one year earlier
  past = ((now) - one_year(now, -1))

  # Do we need to check for dividend qualification
  if (Qualification_Window)
    print_dividend_qualification(now, past, 1)

  # Print Imputation report
  @Imputation_Report_Function(now, past, Show_Extra)

  # Realized gains report
  get_capital_gains(now, past, Show_Extra)

  # The following actions are only needed once;
  # Market (Unrealized) gains and losses should
  # be calulated after capital gains because
  # distributed gains can change the market gains
  # Also unrealized gains are *tax adjusted*
  # becaause otherwise they would effect the deferred tax calculation
  if (Start_Journal) {
    # No this is the first time through
    # Depreciate everything - at EOFY
    depreciate_all(now)

    # Allocated can change in the tax computations
    if (ALLOCATED != ADJUSTMENTS)
      allocated_profits = get_cost(ALLOCATED, ((now) - 1))
  }

  # Print Market Gains
  get_market_gains(now, past, Show_Extra)

  # Print the depreciation schedule
  print_depreciating_holdings(((now) + 1), past, Show_Extra)

  # We need to compute EOFY statements
  # First the operating statement (income & expenses)
  benefits = print_operating_statement(now, past, 1)

  # Compute the tax due
  @Income_Tax_Function(now, past, benefits)

  # A Super fund must allocate assets to members - this requires account balancing
  if (Start_Journal)
    @Balance_Profits_Function(now, past, allocated_profits)

  # Print the balance sheet
  print_balance_sheet(now, past, 1)

  # Allocate second element costs associated with fixed assets - at SOFY
  if (Start_Journal)
    allocate_second_element_costs(((now) + 1))
}

# Default balance journal is a no-op
function balance_journal(now, past, initial_allocation) {
  return
}


# Gains Reconciliation
# Both Realized & Unrealized Gains
#
function print_gains(now, past, is_detailed, gains_type, reports_stream, sold_time, show_capital,

                                                            is_realized_flag, found_gains,
                                                            gains_event, current_price, p, a,
                                                            key,
                                                            description,
                                                            parcel_gains, adjusted_gains,
                                                            held_time,
                                                            label, no_header_printed,
                                                            to_label, proceeds_label,

                                                            asset_width,

                                                            long_gains, short_gains,
                                                            long_losses, short_losses,
                                                            gains,

                                                            units, units_sold,
                                                            cost, reduced_cost, adjusted_cost,
                                                            sum_cost, sum_reduced, sum_adjusted,
                                                            parcel_cost, parcel_proceeds,
                                                            proceeds,
                                                            sum_proceeds,
                                                            accounting_gains,

                                                            sum_long_gains, sum_long_losses,
                                                            sum_short_gains, sum_short_losses) {

  # # Print the gains report
  # Are we printing out a detailed schedule?
  is_detailed = ((is_detailed)?( is_detailed):( (0)))

  # A flag to discriminate realized and unrealized gains
  is_realized_flag = (gains_type ~ "Realized")

  # A default sold time = the Future
  sold_time = ((sold_time)?( sold_time):( Future))

  # The proceeds label
  proceeds_label = ((is_realized_flag)?( "Proceeds"):( "  Value "))
  to_label       = ((is_realized_flag)?( "  To  "):( "Latest"))

  # No header printed
  no_header_printed = (1)

  # Capital or forex gains?
  show_capital = (("" == show_capital)?( (1)):( show_capital))

  # Record accounting gains
  accounting_gains = 0
  sum_long_gains = sum_short_gains = sum_long_losses = sum_short_losses = 0 # Tax gains/losses summed here
  sum_cost = sum_reduced = sum_proceeds = sum_adjusted = 0

  # formatting
  asset_width = 15

  # For each asset sold in the current period
  for (a in Leaf) {
    # Can choose between capital or forex gains
    if ((show_capital && ((a) ~ /^ASSET\.CAPITAL[.:]/)) || (!show_capital && ((a) ~ /^ASSET\.CURRENT\.CURRENCY[.:]/))) {
      # Are we looking for realized gains?
      if (is_realized_flag) {
        # If any of these are non-zero there was a gains event
        # These just apply for the whole account - not parcels
        if (a in Long_Gains)
          long_gains   = get_cost(Long_Gains[a], now)   - get_cost(Long_Gains[a], past)
        else
          long_gains = 0

        if (a in Long_Losses)
          long_losses  = get_cost(Long_Losses[a], now)  - get_cost(Long_Losses[a], past)
        else
          long_losses = 0

        if (a in Short_Gains)
          short_gains  = get_cost(Short_Gains[a], now)  - get_cost(Short_Gains[a], past)
        else
          short_gains = 0

        if (a in Short_Losses)
          short_losses = get_cost(Short_Losses[a], now) - get_cost(Short_Losses[a], past)
        else
          short_losses = 0
        key     = (0)
      } else {
        long_gains = short_gains = long_losses = short_losses = 0

        # The price
        current_price = ((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0))))
      }

      # The last key found
      last_key = Epoch

      # Check each parcel
      if (is_realized_flag || is_open(a, now)) {
        # Where there any found gains - this can occur even if the asset is open
        # (when a capital return makes the cost base negative)
        found_gains = (((((long_gains) - ( Epsilon)) > 0) || (((long_gains) - ( -Epsilon)) < 0)) || ((((long_losses) - ( Epsilon)) > 0) || (((long_losses) - ( -Epsilon)) < 0)) || ((((short_gains) - ( Epsilon)) > 0) || (((short_gains) - ( -Epsilon)) < 0)) || ((((short_losses) - ( Epsilon)) > 0) || (((short_losses) - ( -Epsilon)) < 0)))

        # We will examine each parcel
        gains_event = (0)
        proceeds = cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
        units_sold = 0

        # Get each parcel
        for (p = 0; p < Number_Parcels[a]; p++ ) {
          if (Held_From[a][p] > now) # All further transactions occurred after (now) - parcels are sorted in order bought
            break # All done

          # Check if sold in the (past, now) window (capital gains)
          # or if it is unsold (deferred gains)
          if (found_gains || (((Held_Until[a][ p] <= ( now)) == is_realized_flag) && (Held_Until[a][ p] > ( past)))) {

            if (!gains_event) {
              # Print the gains report
              if (no_header_printed) {
                print Journal_Title > reports_stream
                printf "%s Report for Period Ending %s\n", gains_type, get_date(yesterday(now))  > reports_stream
              }

              # Two types of header
              if (is_detailed)
                printf "%*s %*s %*s %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s %*s\n",
                        asset_width, "Asset", 10, "Parcel",
                        7, "Units", 14, "Cost",
                        11, "From", 12, to_label,
                        11, "Price", 16, proceeds_label,
                        13, "Reduced", 14, "Adjusted",
                        15, "Accounting", 9, "Type",
                        18, "Taxable", 16, "Per Unit" > reports_stream
              else if (no_header_printed) {
                printf "%*s %*s %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s\n",
                       asset_width, "Asset",
                       10, "Units", 14, "Cost", 11, "From", 12, to_label, 11, "Price", 16, proceeds_label,
                       13, "Reduced", 14, "Adjusted", 15, "Accounting", 9, "Type", 18, "Taxable" > reports_stream
                underline(151 + asset_width, 6, reports_stream)
              }

              # print Name
              label = (Leaf[a])
              gains_event = (1)
              no_header_printed = (0)
            }

            # Number of units
            units = Units_Held[a][p]

            # Parcel sale times are tricky
            if ((Held_Until[a][ p] <= ( now)) && (Held_Until[a][ p] > ( past)))
              key = Held_Until[a][p]
            else if (!is_realized_flag && !found_gains)
              key = sold_time
            else
              key = (0)

            # The held time (will be wrong when key is FALSE)
            held_time = get_held_time(key, Held_From[a][p])

            # Total gains (accounting gains)
            if ((Held_Until[a][ p] <= ( now))) {
              parcel_proceeds = (Parcel_Proceeds[a][ p])
              current_price = - parcel_proceeds / units
            } else
              parcel_proceeds = - current_price * Units_Held[a][p]
            gains = parcel_proceeds + sum_cost_elements(Accounting_Cost[a][p], now) # All elements

            # cash in and out
            parcel_cost     =   get_cash_in(a, p, now)
            cost           += parcel_cost
            if (key) {
              # Sold parcels
              proceeds      += parcel_proceeds
              reduced_cost  += get_parcel_cost(a, p, now)
              adjusted_cost += get_parcel_cost(a, p, now, (1))
              units_sold    += units
            }

            # Keep track of accounting gains
            accounting_gains += gains

            # We want taxable gains
            # Gains are relative to adjusted cost
            # Losses are relative to reduced cost (so equal accounting losses)
            if ((((gains) - ( Epsilon)) > 0)) {
              # These are losses
              parcel_gains = gains
              if (held_time >= 31622400) {
                description = "Long Losses "
                if (!found_gains)
                  long_losses += gains
              } else {
                description = "Short Losses"
                if (!found_gains)
                  short_losses += gains
              }
            } else {
              # Assume zero losses or gains
              parcel_gains = 0

              # Taxable gains
              description = "Zero Gains  "
            }

            # after application of tax adjustments
            # If there were losses then parcel_gains will be above zero
            adjusted_gains = gains - ((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  now))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))
            if ((((adjusted_gains) - ( -Epsilon)) < 0)) {
              # Adjustments are negative and reduce taxable gains
              parcel_gains = adjusted_gains
              if (held_time >= 31622400) {
                description = "Long Gains  "
                if (!found_gains)
                  long_gains += parcel_gains

              } else {
                description = "Short Gains "
                if (!found_gains)
                  short_gains += parcel_gains
              }
            }

            # Print out the parcel information
            if (is_detailed && key) {
              # If printing out in detail
              if ((( a in Parcel_Tag) && ( p in Parcel_Tag[ a]))) {
                if (label) # Deal with label first
                  printf "%*s\n", asset_width + 1, label > reports_stream
                label = Parcel_Tag[a][p]
              }

              # Print line
              printf "%*s %*d %*.3f %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s %*s\n",
                   asset_width + 1, label,
                   7, p,
                   11, units,
                   15, print_cash(parcel_cost),
                   11, get_date(Held_From[a][p]),
                   10, get_date(key),
                   12, print_cash(current_price),
                   14, print_cash(- parcel_proceeds),
                   14, print_cash(get_parcel_cost(a, p, now)),
                   14, print_cash(get_parcel_cost(a, p, now, (1))),
                   14, print_cash(- gains),
                   14, description,
                   14, print_cash(- parcel_gains),
                   14, print_cash(parcel_cost / units, 4) > reports_stream

              # Clear label
              label = ""
            }

            # Save last key printed out
            if (key)
              last_key = (((last_key) - ( key) > 0)?(last_key):( key))
          }

          # Reset key
          key = (0)
        } # End of each parcel p

        # Show any gains event
        if (gains_event) {
          if ((((last_key) - ( Epoch)) > 0)) {
            if (is_detailed)
              # Detailed format
              underline(175 + asset_width, 6, reports_stream)
          } else
            last_key = Future

         # This is account level information
         printf "%*s %*.3f %*s %*s   %*s %*s %*s %*s %*s ",
                asset_width + 1, label,
                (11 + 8 * is_detailed), units_sold,
                15, print_cash(cost),
                7, get_date(Held_From[a][0]),
                10, get_date(last_key),
                12, print_cash(current_price),
                14, print_cash(- proceeds),
                14, print_cash(reduced_cost),
                14, print_cash(adjusted_cost) > reports_stream

          # Stack the gains & losses
          if (((((long_gains) - ( Epsilon)) > 0) || (((long_gains) - ( -Epsilon)) < 0)))
            Gains_Stack[Long_Gains_Key]   = long_gains
          if (((((long_losses) - ( Epsilon)) > 0) || (((long_losses) - ( -Epsilon)) < 0)))
            Gains_Stack[Long_Losses_Key]  = long_losses
          if (((((short_gains) - ( Epsilon)) > 0) || (((short_gains) - ( -Epsilon)) < 0)))
            Gains_Stack[Short_Gains_Key]  = short_gains
          if (((((short_losses) - ( Epsilon)) > 0) || (((short_losses) - ( -Epsilon)) < 0)))
            Gains_Stack[Short_Losses_Key] = short_losses

          # Common entries
          key = ""
          for (key in Gains_Stack)
            break
          if (key) {
            printf "%*s %*s %*s",
              14, print_cash(- proceeds - reduced_cost),
              14, key,
              14, print_cash(- Gains_Stack[key]) > reports_stream
            delete Gains_Stack[key]
          } else
            printf "%14s", print_cash(- proceeds - reduced_cost) > reports_stream

          # Extra entries {
          for (key in Gains_Stack) {
            printf "\n%*s %14s", 143 + asset_width + 8 * is_detailed, key, print_cash(- Gains_Stack[key]) > reports_stream
            delete Gains_Stack[key]
          }

          # Grand totals
          sum_cost     += cost
          sum_proceeds += proceeds
          sum_reduced  += reduced_cost
          sum_adjusted += adjusted_cost
          sum_long_gains += long_gains
          sum_short_gains += short_gains
          sum_long_losses += long_losses
          sum_short_losses += short_losses

          printf "\n" > reports_stream
          if (is_detailed)
           printf "\n" > reports_stream

        } # End of gains event
      }
    } # End of if is capital
  } # End of each asset

  # Final lines
  if (!no_header_printed) {
    underline(166 + is_detailed * (9 + asset_width), 6, reports_stream)
    if (is_detailed)
      underline(166 + is_detailed * (9 + asset_width), 6, reports_stream)

    # Stack the gains & losses
    if (((((sum_long_gains) - ( Epsilon)) > 0) || (((sum_long_gains) - ( -Epsilon)) < 0)))
      Gains_Stack[Long_Gains_Key]   = sum_long_gains
    if (((((sum_long_losses) - ( Epsilon)) > 0) || (((sum_long_losses) - ( -Epsilon)) < 0)))
      Gains_Stack[Long_Losses_Key]  = sum_long_losses
    if (((((sum_short_gains) - ( Epsilon)) > 0) || (((sum_short_gains) - ( -Epsilon)) < 0)))
      Gains_Stack[Short_Gains_Key]  = sum_short_gains
    if (((((sum_short_losses) - ( Epsilon)) > 0) || (((sum_short_losses) - ( -Epsilon)) < 0)))
      Gains_Stack[Short_Losses_Key] = sum_short_losses

    # Print out a summary
    printf "%*s %*s %*s %*s %*s ",
          asset_width + 1, "Totals",
          (27 + 8 * is_detailed), print_cash(sum_cost),
          53, print_cash(- sum_proceeds),
          14, print_cash(sum_reduced),
          14, print_cash(sum_adjusted)  > reports_stream

    # Common entries
    for (key in Gains_Stack)
      break
    if (key) {
      printf "%*s %*s %*s",
        14, print_cash(- sum_proceeds - sum_reduced),
        14, key,
        14, print_cash(- Gains_Stack[key]) > reports_stream
      delete Gains_Stack[key]
    } else
      printf "%14s", print_cash(sum_proceeds - sum_reduced) > reports_stream

    # Extra entries {
    for (key in Gains_Stack) {
      printf "\n%*s %14s", 143 + asset_width + 8 * is_detailed, key, print_cash(- Gains_Stack[key]) > reports_stream
      delete Gains_Stack[key]
    }

    # If these are not realized gains
    if (!is_realized_flag) {
      printf "\n" > reports_stream
      underline(44, 8, reports_stream)
      printf "\t%27s => %14s\n", "Market Gains",
                                 print_cash(- sum_long_gains - sum_short_gains) > reports_stream
      printf "\t%27s => %14s\n", "Market Losses",
                                 print_cash(sum_long_losses + sum_short_losses) > reports_stream

      # Get the deferred taxable gains
      cost = apply_losses(now, reports_stream, "Deferred", sum_long_gains + sum_short_gains, sum_long_losses + sum_short_losses, "*ASSET", UNREALIZED)

      # Need to balance the market gains
      #cost = get_cost(UNREALIZED, now)
      #adjust_cost("*ASSET", get_cost(UNREALIZED, just_before(now)) - cost, now)

      # Only deferred gains count
      if ((((cost) - ( -Epsilon)) < 0))
         printf "\t%27s => %14s\n", "Deferred Gains", print_cash(- cost) > reports_stream

       # All done
       underline(44, 8, reports_stream)
    }
  }

  printf "\n\n" > reports_stream
  return accounting_gains
} # End of print gains

#
# Report on income gains - distributed directly or indirectly from managed funds
#
function print_income_gains(now, past, is_detailed, reports_stream,
                            a, gains, sum_gains,
                            underlying_asset,
                            no_header_printed,
                            asset_width,
                            class_list,
                            sign, label) {

  # A class list
  class_list["INCOME.GAINS.NET"] = (1)
  class_list["INCOME.GAINS.LONG"] = (1)
  class_list["INCOME.GAINS.SHORT"] = (1)
  class_list["EXPENSE.LOSSES.LONG"] = (1)
  class_list["EXPENSE.LOSSES.SHORT"] = (1)

  # Are we printing out a detailed schedule?
  is_detailed = ((is_detailed)?( is_detailed):( (0)))
  asset_width = 15

  # Report on income gains
  # This should examine LONG & SHORT & LOSSES too
  sum_gains = 0
  no_header_printed = (1)
  for (a in Leaf)
    for (c in class_list)
      if (select_class(a, c)) {
        # These are the income gains classes
        # Each account needs the income gains increased in proportion to its share of the total gains
        gains     = get_cost(a, now) - get_cost(a, past)

        # Skip negligible gains
        if (((((gains) - ( Epsilon)) <= 0) && (((gains) - ( -Epsilon)) >= 0)))
          continue

        # Print a header when required
        if (no_header_printed) {
          # Print the gains report
          printf "\n%s\n", Journal_Title > reports_stream
          printf "Distributed Capital Gains Report for Period Ending %s\n\n", get_date(yesterday(now))  > reports_stream

          # The header
          if (is_detailed) {
            printf "%*s %*s\n",
                   asset_width, "Asset",
                   14, "Distributed" > reports_stream
            printf "%*s\n", 11 + asset_width, "Gains" > reports_stream
            underline(9 + asset_width, 6, reports_stream)
          } else {
            printf "%*s\n",
                 asset_width + 15, "Distributed" > reports_stream
            printf "%*s\n", 11 + asset_width, "Gains" > reports_stream
          }

          # print Name
          no_header_printed = (0)
        }

        # Get underlying account
        if (a in Underlying_Asset)
          underlying_asset = Underlying_Asset[a]
        else
          underlying_asset = a

        # Print out the detailed information
        if (is_detailed) {
          # If printing out in detail
          printf "%*s %*s\n",
               asset_width + 1, Leaf[underlying_asset],
               11, print_cash(-gains) > reports_stream
        }

        # Sum the gains
        sum_gains += gains
      }

  # The summary
  if (!no_header_printed) {
    if (is_detailed)
      underline(9 + asset_width, 6, reports_stream)

    printf "%*s %*s\n",
       asset_width + 1, "Total",
       11, print_cash(-sum_gains) > reports_stream
    underline(9 + asset_width, 6, reports_stream)
    printf "\n" > reports_stream
  }

  # Clean up
  delete class_list

  # All done
  return
}

# Compute capital gains and losses
function get_capital_gains(now, past, is_detailed,

                                reports_stream,
                                accounting_gains, accounting_losses,
                                adjusted_gains,
                                distributed_gains,
                                income_long_gains, income_short_gains, income_net_gains,
                                expense_long_losses, expense_short_losses,
                                capital_long_gains, capital_short_gains,
                                capital_long_losses, capital_short_losses,
                                taxable_gains,
                                carried_losses,
                                losses, next_losses, key) {


    # The reports_stream is the pipe to write the schedule out to
    reports_stream = (("OTC" ~ /[cC]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

    # Print the capital gains schedule
    print Journal_Title > reports_stream
    printf "Capital Gains Report for Period Ending %s\n", get_date(yesterday(now))  > reports_stream

    # Get total capital gains
    # Exploit existing sums
    # taxable capital gains are messy
    underline(44, 8, reports_stream)

    # First the cgt gains & losses
    #
    # Total Gains
    accounting_gains = get_cost("*INCOME.GAINS", ((now) - 1)) - get_cost("*INCOME.GAINS", past)

    # The realized capital losses
    accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)

    # Print Accounting Capital Gains
    printf "\t%27s => %14s\n", "Accounting Capital Gains", print_cash(- accounting_gains) > reports_stream
    printf "\t%27s => %14s\n", "Accounting Capital Losses", print_cash(accounting_losses) > reports_stream

    # Now compute the total accounting gains
    accounting_losses += accounting_gains
    underline(44, 8, reports_stream)
    if ((((accounting_losses) - ( -Epsilon)) < 0))
      printf "\t%27s => %14s\n", "Net Accounting Gains ", print_cash(- accounting_losses) > reports_stream
    else if ((((accounting_losses) - ( Epsilon)) > 0))
      printf "\t%27s => %14s\n", "Net Accounting Losses", print_cash(accounting_losses) > reports_stream
    else
      printf "\t%27s\n", "Zero Accounting Gains" > reports_stream

    # Carried capital losses from previous years (if any)
    carried_losses = (( past in Capital_Losses)?( ((__MPX_KEY__ = first_key(Capital_Losses[ past]))?( Capital_Losses[ past][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Capital_Losses[ past][0]):( 0))))):( 0))

    # The taxable long & short gains
    # If there are other income gains (eg from distributions etc)
    # then the taxable gains will need adjustment
    income_net_gains   = get_cost("*INCOME.GAINS.NET", now)  - get_cost("*INCOME.GAINS.NET", past)
    income_long_gains  = get_cost("*INCOME.GAINS.LONG", now)  - get_cost("*INCOME.GAINS.LONG", past)
    income_short_gains = get_cost("*INCOME.GAINS.SHORT", now) - get_cost("*INCOME.GAINS.SHORT", past)

    # Simililarly for expenses
    expense_long_losses  = get_cost("*EXPENSE.LOSSES.LONG", now)  - get_cost("*EXPENSE.LOSSES.LONG", past)
    expense_short_losses = get_cost("*EXPENSE.LOSSES.SHORT", now) - get_cost("*EXPENSE.LOSSES.SHORT", past)

    # The long gains and losses first
    capital_long_gains  = get_cost(((((("SPECIAL.TAXABLE.GAINS.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.LONG"))):( (("*") (("SPECIAL.TAXABLE.GAINS.LONG"))))), ((now) - 1)) - get_cost(((((("SPECIAL.TAXABLE.GAINS.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.LONG"))):( (("*") (("SPECIAL.TAXABLE.GAINS.LONG"))))), past)
    capital_long_losses = get_cost(((((("SPECIAL.TAXABLE.LOSSES.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.LONG"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.LONG"))))), ((now) - 1)) - get_cost(((((("SPECIAL.TAXABLE.LOSSES.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.LONG"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.LONG"))))), past)

    # short gains & losses
    capital_short_gains   = get_cost(((((("SPECIAL.TAXABLE.GAINS.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.SHORT"))):( (("*") (("SPECIAL.TAXABLE.GAINS.SHORT"))))), ((now) - 1)) - get_cost(((((("SPECIAL.TAXABLE.GAINS.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.SHORT"))):( (("*") (("SPECIAL.TAXABLE.GAINS.SHORT"))))), past)
    capital_short_losses  = get_cost(((((("SPECIAL.TAXABLE.LOSSES.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.SHORT"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.SHORT"))))), ((now) - 1)) - get_cost(((((("SPECIAL.TAXABLE.LOSSES.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.SHORT"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.SHORT"))))), past)

    # The adjusted gains and losses
    printf "\n\tAdjusted Gains\n" > reports_stream
    printf "\n\t%27s => %14s\n",  "Long Adjusted Gains", print_cash(- capital_long_gains) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Adjusted Gains", print_cash(- capital_short_gains) > reports_stream

    # Finally the adjusted losses
    # Which are the same as the accounting losses
    printf "\t%27s => %14s\n", "Long Adjusted Losses", print_cash(capital_long_losses) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Adjusted Losses", print_cash(capital_short_losses) > reports_stream

    # Report all distributed gains and losses
    # Take care that the adjustments are not applied more than once
    distributed_gains = (0)
    if (((((expense_short_losses) - ( Epsilon)) > 0) || (((expense_short_losses) - ( -Epsilon)) < 0))) {
      distributed_gains = (1)
      printf "\t%27s => %14s\n", "Short Expense Losses", print_cash(expense_short_losses) > reports_stream
    }
    if (((((expense_long_losses) - ( Epsilon)) > 0) || (((expense_long_losses) - ( -Epsilon)) < 0))) {
      distributed_gains = (1)
      printf "\t%27s => %14s\n", "Long Expense Losses", print_cash(expense_long_losses) > reports_stream
    }
    if (((((income_short_gains) - ( Epsilon)) > 0) || (((income_short_gains) - ( -Epsilon)) < 0))) {
      distributed_gains = (1)
      # These should not occur for Australian returns
      printf "\t%27s => %14s\n", "Short Income Gains", print_cash(- income_short_gains) > reports_stream
    }
    if (((((income_long_gains) - ( Epsilon)) > 0) || (((income_long_gains) - ( -Epsilon)) < 0))) {
      distributed_gains = (1)
      printf "\t%27s => %14s\n", "Long Income Gains", print_cash(- income_long_gains) > reports_stream
    }
    if (((((income_net_gains) - ( Epsilon)) > 0) || (((income_net_gains) - ( -Epsilon)) < 0))) {
      distributed_gains = (1)
      printf "\t%27s => %14s\n", "Long Income Net Gains", print_cash(- income_net_gains) > reports_stream
    }

    # Need to handle distributed income & expenses in a jurisdiction agnostic way
    if (distributed_gains) {
      # Net up long gains and losses
      income_long_gains += expense_long_losses

      # Net up short gains and losses
      income_short_gains += expense_short_losses

      # Need the overall adjusted gains/losses (including available carried losses)
      adjusted_gains = capital_long_gains  + capital_short_gains  +                       capital_long_losses + capital_short_losses +                       income_long_gains   + income_short_gains   +                       income_net_gains    + carried_losses


      # Compute extra taxable gains (if any)
      # Need the less (in magnitude) of the overall adjusted gain or net gain
      if ((((adjusted_gains) - ( -Epsilon)) < 0)) {
        printf "\t%27s => %14s\n", "Available Gains", print_cash(- adjusted_gains) > reports_stream
        # There are gains
        if ((((adjusted_gains) - ( income_net_gains)) < 0))
          # The gains are greater than the income net gains, so gross up the net gains
          taxable_gains = @Gross_Up_Gains_Function(now, past, income_net_gains, income_net_gains)
        else
          # The gains are less than the income gains, so gross up the adjusted gains
          taxable_gains = @Gross_Up_Gains_Function(now, past, adjusted_gains, income_net_gains)

        # Assume grossed up taxable gains are treated as long gains
        income_long_gains += taxable_gains
      }

      if (((((taxable_gains) - ( Epsilon)) > 0) || (((taxable_gains) - ( -Epsilon)) < 0)))
        printf "\t%27s => %14s\n", "Grossed Up Long Gains", print_cash(- income_long_gains) > reports_stream
    }

    # Show total capital gains
    printf "\n\tCapital Gains Before Application of Losses or Discounts\n" > reports_stream
    printf "\n\t%27s => %14s\n",  "Total Capital Gains", print_cash(-(capital_long_gains + income_long_gains + capital_short_gains + income_short_gains)) > reports_stream

    # Apply long & short losses separately
    # This is not strictly necessary in all cases but useful
    # Save net gains or losses
    # What happens when you manipulate a parent account?
    adjusted_gains  = apply_losses(now, reports_stream, "Long",  capital_long_gains + income_long_gains,  capital_long_losses,  "*SPECIAL", ((((("SPECIAL.TAXABLE.GAINS.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.LONG"))):( (("*") (("SPECIAL.TAXABLE.GAINS.LONG"))))),  ((((("SPECIAL.TAXABLE.LOSSES.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.LONG"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.LONG"))))))
    adjusted_gains += apply_losses(now, reports_stream, "Short", capital_short_gains + income_short_gains, capital_short_losses, "*SPECIAL", ((((("SPECIAL.TAXABLE.GAINS.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.SHORT"))):( (("*") (("SPECIAL.TAXABLE.GAINS.SHORT"))))), ((((("SPECIAL.TAXABLE.LOSSES.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.SHORT"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.SHORT"))))))

    # Overall gains, losses and taxable gains
    underline(44, 8, reports_stream)
    if ((((adjusted_gains) - ( -Epsilon)) < 0))
      printf "\t%27s => %14s\n", "Net Adjusted Gains ", print_cash(- adjusted_gains) > reports_stream
    else if ((((adjusted_gains) - ( Epsilon)) > 0))
      printf "\t%27s => %14s\n", "Net Adjusted Losses", print_cash(adjusted_gains) > reports_stream
    else
      printf "\t%27s\n", "Zero Adjusted Gains" > reports_stream
    if ((((carried_losses) - ( Epsilon)) > 0)) {
      printf "\t%27s => %14s\n", "Losses Brought Forward", print_cash(carried_losses) > reports_stream

      accounting_gains = adjusted_gains + carried_losses
      if ((((accounting_gains) - ( -Epsilon)) < 0))
        printf "\t%27s => %14s\n", "Total Adjusted Gains ", print_cash(- accounting_gains) > reports_stream
      else if ((((accounting_gains) - ( Epsilon)) > 0))
        printf "\t%27s => %14s\n", "Total Adjusted Losses", print_cash(accounting_gains) > reports_stream
      else
        printf "\t%27s\n", "Zero Adjusted Gains" > reports_stream
    }

    # Compute taxable gains
    taxable_gains = @Get_Taxable_Gains_Function(now, carried_losses) # FIXME need carry forward limit
    if ((((taxable_gains) - ( -Epsilon)) < 0))
      printf "\t%27s => %14s\n",   "Taxable Gains",  print_cash(- taxable_gains) > reports_stream

    # All done
    underline(44, 8, reports_stream)
    print "\n" > reports_stream

    # If the total capital losses are non zero at the EOFY they must be carried losses
    carried_losses = get_carried_losses(now, Capital_Losses, adjusted_gains, 0, reports_stream)
    if ((((carried_losses) - ( Epsilon)) > 0)) {
      printf "\t%27s => %14s\n", "Losses Carried Forward", print_cash(carried_losses) > reports_stream
    } else {
      assert(((((carried_losses) - ( Epsilon)) <= 0) && (((carried_losses) - ( -Epsilon)) >= 0)), sprintf("Cannot carry taxable capital gains forward [%s] Gains => %14s", get_date(past), print_cash(- carried_losses, 6)))
      carried_losses = 0
    }

    # Now the gains reports
    if ("/dev/null" != reports_stream) {
      # Now the income gains report
      print_income_gains(now, past, is_detailed, reports_stream)

      # The Realized Capital Gains
      print_gains(now, past, is_detailed, "Realized Capital Gains", reports_stream)
      delete Gains_Stack

      # The Realized Foreign Exchange Gains/Losses
      print_gains(now, past, is_detailed, "Realized Foreign Exchange Gains", reports_stream, Future, (0))
      delete Gains_Stack
    }
}

# Report on the losses
function report_losses(now, losses_array, label, write_stream,
                       losses, next_losses, key) {

  # Get the losses
  losses = (( now in losses_array)?( ((__MPX_KEY__ = first_key(losses_array[ now]))?( losses_array[ now][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( losses_array[ now][0]):( 0))))):( 0))
  if ((((losses) - ( Epsilon)) > 0)) {
    printf "\n%s Report\n", label > write_stream
    printf "\t%14s  %14s\n", "Year", label  > write_stream
    underline(36, 8, write_stream)

    next_losses = losses
    for (key in losses_array[now]) {
      # The next losses
      next_losses = ((__MPX_KEY__ = find_key(losses_array[now],  ((key) - 1)))?( losses_array[now][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( losses_array[now][0]):( 0))))
      printf "\t%14s %14s\n", get_date(key, ("%Y")       ), print_cash(losses - next_losses)  > write_stream
      losses = next_losses
    }
    underline(36, 8, write_stream)

    # Inefficient
    losses = (( now in losses_array)?( ((__MPX_KEY__ = first_key(losses_array[ now]))?( losses_array[ now][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( losses_array[ now][0]):( 0))))):( 0))
    printf "\t%14s %14s\n\n", "Total", print_cash(losses) > write_stream
  }

  # Return overall losses
  return losses
}

# Default gross up gains
function gross_up_gains_def(now, past, total_gains, long_gains, short_gains) {
  # Nothing to do by default
  # Gains are alreay computed
  return 0
}

# Shared code for applying losses to taxable gains
function apply_losses(now, reports_stream, label,
                           gains, losses, balancing_account, save_gains, save_losses, x) {
  # It works for partioned long & short gains

  # Summarize starting point
  underline(44, 8, reports_stream)
  printf "\nAfter Application of %s Losses\n", label > reports_stream

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  gains += losses # Net gains / losses

  # Carried losses generated
  if (!(((gains) - ( -Epsilon)) < 0)) {
    # No overall gains
    # There could be a loss in this scenario
    losses = (((((gains) - ( Epsilon)) > 0))?( (gains)):(  0))

    # But not a gain
    gains = 0
  } else
    # No overall losses
    losses = 0

  # Save  gains
  # these could be deferred gains or taxable gains
  # Only needed on first pass
  if (save_gains) {
    if (Start_Journal) {
      x = get_cost(save_gains, now)
      adjust_cost(save_gains,          gains - x,  now)
      adjust_cost(balancing_account, -(gains - x), now)
    }
    printf "\t%27s => %14s\n", (label " Gains"), print_cash(- get_cost(save_gains, now)) > reports_stream
  }

  # Remaining options could only be for taxable gains
  if (save_losses) {
    if (Start_Journal) {
      x = get_cost(save_losses, now)
      adjust_cost(save_losses,         losses - x,  now)
      adjust_cost(balancing_account, -(losses - x), now)
    }
    printf "\t%27s => %14s\n", (label " Losses"), print_cash(get_cost(save_losses, now)) > reports_stream
  }

  # Cannot have both a net gain and a net loss simultaneously
  # So return whichever is appropriate
  if (((((losses) - ( Epsilon)) <= 0) && (((losses) - ( -Epsilon)) >= 0)))
    # A (possibly zero) net gain
    return gains

  # Must be a net loss
  return losses
}

# Get the carried losses - limit how many years a loss can be carried forward
function get_carried_losses(now, losses_array, losses, limit, reports_stream,
                            past,
                            key) {

  #
  # losses_array[Now] => Total losses (and maybe gains) in year
  # They have a non-standard double dependence on time
  #
  reports_stream = ((reports_stream)?( reports_stream):( "/dev/null"))

  # Don't use losses prior to (now - limit) if limit is set
  if (limit)
    # Set the limiting time
    # The passed limit is in units of years
    limit = now - one_year(now, - limit)

  # There would be need to be one set per currency
  past = find_key(losses_array, ((now) - 1))

  # If there are already earlier losses copy them
  if (past in losses_array) {
    for (key in losses_array[past])
      # Copy the most recent set of losses
      losses_array[now][key] = losses_array[past][key]

    # If limit is set remove any keys older than limit in latest version
    if (limit && now in losses_array) {
      key = remove_keys(losses_array[now], limit)

      # Record this
      if (key && ((((key) - ( Epsilon)) > 0) || (((key) - ( -Epsilon)) < 0))) {
        printf "\t%27s => %14s\n", "Losses Prior To",  get_date(limit) > reports_stream
        printf "\t%27s => %14s\n", "Losses Cancelled",  print_cash(key) > reports_stream
      }
    }
  }

  # If there are gains cancel the earliest losses
  if ((((losses) - ( -Epsilon)) < 0)) {
    # These are actually gains
    # The oldest losses are cancelled first
    # Remember gains are negative
    # There may be no losses available
    if (now in losses_array)
      remove_entries(losses_array[now], -losses)
  } else if ((((losses) - ( Epsilon)) > 0)) {
    # OK no old losses will be extinguished
    # A new loss is added
    if (now in losses_array)
      sum_entry(losses_array[now], losses, now)
    else
      losses_array[now][now] = losses
  }

  # Return the value of the income long gains
  return (( now in losses_array)?( ((__MPX_KEY__ = first_key(losses_array[ now]))?( losses_array[ now][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( losses_array[ now][0]):( 0))))):( 0))
}


# Print out operating statement
function print_operating_statement(now, past, is_detailed,     reports_stream,
                                                               benefits, losses,
                                                               gains, market_gains,
                                                               more_past, label, x) {

  # Set arguments
  more_past = ((past) - one_year(past, -1))
  is_detailed = ("" == is_detailed) ? 1 : 2

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = (("OTC" ~ /[oO]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

  printf "\n%s\n", Journal_Title > reports_stream
  if (is_detailed)
    printf "Detailed Operating Statement\n" > reports_stream
  else
    printf "Operating Statement\n" > reports_stream
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > reports_stream
  underline(81, 0, reports_stream)
  printf "%53s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC) > reports_stream
  printf "%53s %26s\n", "$", "$" > reports_stream

  # We start with the investment income
  label = sprintf("Income\nInvestment Income\n")

  # Exclude contributions
  label = print_account_class(reports_stream, label, "block_class", "INCOME", "INCOME.CONTRIBUTION", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Obtain the income per year
  benefits[now]  = - (get_cost("*INCOME", now) - (x = get_cost("*INCOME", past)))
  benefits[past] = - (x - get_cost("*INCOME", more_past))

  # Now the Contributions
  label = sprintf("\nContributions\n")
  print_account_class(reports_stream, label, "select_class", "INCOME.CONTRIBUTION", "", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Print a running total
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # Print grand total income
  printf "\t%22s %23s", "Total Income", print_cash(benefits[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > reports_stream
  else
    printf "\n" > reports_stream
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # the unrealized gains
  label = sprintf("\n\nInvestment Gains\nUnrealized Gains in Market Value\n")
  print_account_class(reports_stream, label, "select_class", "ASSET.CAPITAL", "", "get_unrealized_gains", now, past, past, more_past, is_detailed, -1) # Block Depreciating Assets

  # Obtain the market gains per year
  market_gains[now]  = - (get_cost(UNREALIZED, now) - (x = get_cost(UNREALIZED, past)))
  market_gains[past] = - (x - get_cost(UNREALIZED, more_past))

  # Print the total unrealized gains
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # Print any unrealized gains
  if ((((market_gains[now]) - ( Epsilon)) > 0) || (((market_gains[past]) - ( Epsilon)) > 0)) {
    printf "\t%22s %23s", "Total Market Gains", print_cash((((((market_gains[now]) - ( Epsilon)) > 0))?( (market_gains[now])):(  ""))) > reports_stream
    if (past)
      printf " %26s\n", print_cash((((((market_gains[past]) - ( Epsilon)) > 0))?( (market_gains[past])):(  ""))) > reports_stream
    else
      printf "\n" > reports_stream
    benefits[now]  += (((((market_gains[now]) - ( Epsilon)) > 0))?( (market_gains[now])):(  0))
    benefits[past] += (((((market_gains[past]) - ( Epsilon)) > 0))?( (market_gains[past])):(  0))
  }

  # Print a grand total
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # Print grand total income
  printf "\t%22s %23s", "Total of All Income", print_cash(benefits[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > reports_stream
  else
    printf "\n" > reports_stream
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  printf "\n" > reports_stream

  # Now print expenses... (exclude tax payments)
  label = sprintf("Expenses\nGeneral Expenses\n")
  label = print_account_class(reports_stream, label, "block_class", "EXPENSE", "EXPENSE.UNREALIZED", "get_cost", now, past, past, more_past, is_detailed)

  # Need to correct for market gains captured as expenses
  losses[now]  = market_gains[now]  + get_cost("*EXPENSE", now) - (x = get_cost("*EXPENSE", past))
  losses[past] = market_gains[past] + x - get_cost("*EXPENSE", more_past)

  # Print a total
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # Print grand total income
  printf "\t%22s %23s", "Total Expenses", print_cash(losses[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(losses[past]) > reports_stream
  else
    printf "\n" > reports_stream
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  printf "\n\n" > reports_stream

  # Print any unrealized losses
  if ((((market_gains[now]) - ( -Epsilon)) < 0) || (((market_gains[past]) - ( -Epsilon)) < 0)) {
    printf "\t%22s %23s", "Total Market Losses", print_cash((((((market_gains[now]) - ( -Epsilon)) < 0))?( (market_gains[now])):(  ""))) > reports_stream
    if (past)
      printf " %26s\n", print_cash((((((market_gains[past]) - ( -Epsilon)) < 0))?( (market_gains[past])):(  ""))) > reports_stream
    else
      printf "\n" > reports_stream
    losses[now]  -= (((((market_gains[now]) - ( -Epsilon)) < 0))?( (market_gains[now])):(  0))
    losses[past] -= (((((market_gains[past]) - ( -Epsilon)) < 0))?( (market_gains[past])):(  0))
  }

  # Print a total
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # Print grand total expenses
  printf "\t%22s %23s", "Total of All Expenses", print_cash(losses[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(losses[past]) > reports_stream
  else
    printf "\n" > reports_stream
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  printf "\n\n" > reports_stream

  # Print Before Tax benefits
  benefits[now]  -= losses[now]
  benefits[past] -= losses[past]
  printf "\t%27s %18s", "Benefits Accrued Before Tax", print_cash(benefits[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > reports_stream
  else
    printf "\n" > reports_stream
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  printf "\n\n" > reports_stream

  # Only need current benefits
  x = benefits[now]
  delete losses
  delete market_gains
  delete benefits

  # Return value
  return x
}

# Print balance sheet
function print_balance_sheet(now, past, is_detailed,    reports_stream,
                             current_assets, assets, current_liabilities, liabilities, equity, label, class_list) {

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = (("OTC" ~ /[bB]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

  # Return if nothing to do
  if ("/dev/null" == reports_stream)
    return

  # By default not detailed
  is_detailed = ("" == is_detailed) ? 1 : 2

  # This is an extended version of check balance but also
  # draws on information from the operating statement and tax statement
  # Let's go
  printf "\n%s\n", Journal_Title > reports_stream
  printf "Statement of Financial Position\n" > reports_stream
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > reports_stream
  underline(81, 0, reports_stream)
  printf "%53s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC) > reports_stream
  printf "%53s %26s\n", "$", "$" > reports_stream

  # We start with the current assets (cash)
  label = sprintf("Current Assets\n")
  label = print_account_class(reports_stream, label, "select_class", "ASSET.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed)
  label = print_account_class(reports_stream, label, "current_class", "ASSET.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Term assets are current if they mature within one year
  current_assets[now]  = get_cost("*ASSET.CURRENT", now) + account_sum[now]
  current_assets[past] = get_cost("*ASSET.CURRENT", past) + account_sum[past]

  # Print a nice line
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Current Assets",
          print_cash(current_assets[now]), print_cash(current_assets[past]) > reports_stream

  # Now the non-current assets
  label = sprintf("Non Current Assets\n")
  class_list["ASSET.TERM"] = (1)
  class_list["ASSET.CURRENT"] = (1)
  label = print_account_class(reports_stream, label, "block_class_list", "ASSET", class_list, "get_value", now, Epoch, past, Epoch, is_detailed)
  label = print_account_class(reports_stream, label, "not_current_class", "ASSET.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed)
  delete class_list

  # Here we need to adjust for accounting gains & losses
  assets[now]  =  get_cost("*ASSET", now)
  assets[past] =  get_cost("*ASSET", past)

  # Print a nice line
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Non Current Assets", print_cash(assets[now] - current_assets[now]), print_cash(assets[past] - current_assets[past]) > reports_stream

  # Print Total Assets
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Assets", print_cash(assets[now]), print_cash(assets[past]) > reports_stream

  # Treat tax payments/refunds as liabilities
  label = sprintf("Tax Liabilities\n")
  label = print_account_class(reports_stream, label, "select_class", "LIABILITY.TAX", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # We start with the current liabilities
  label = sprintf("Current Liabilities\n")
  label = print_account_class(reports_stream, label, "select_class", "LIABILITY.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)
  label = print_account_class(reports_stream, label, "current_class", "LIABILITY.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  current_liabilities[now]   = -(get_cost("*LIABILITY.CURRENT", now ) + get_cost("*LIABILITY.TAX", now) + account_sum[now])
  current_liabilities[past]  = -(get_cost("*LIABILITY.CURRENT", past) + get_cost("*LIABILITY.TAX", past) + account_sum[past])

  # Print a nice line
  if (!label) {
    underline(73, 8, reports_stream)
    printf "\t%24s%21s %26s\n\n", "Total Current Liabilities",
              print_cash(current_liabilities[now]), print_cash(current_liabilities[past]) > reports_stream
  }

  # Need non-current Liabilities
  label = sprintf("Non-Current Liabilities\n")

  # Now the remaining non current liabilities
  class_list["LIABILITY.TERM"] = (1)
  class_list["LIABILITY.CURRENT"] = (1)
  class_list["LIABILITY.MEMBER"] = (1)
  class_list["LIABILITY.TAX"] = (1)
  label = print_account_class(reports_stream, label, "block_class_list", "LIABILITY", class_list, "get_cost", now, Epoch, past, Epoch, is_detailed, -1)
  label = print_account_class(reports_stream, label, "not_current_class", "LIABILITY.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  liabilities[now]  = - get_cost("*LIABILITY", now)
  liabilities[past] = - get_cost("*LIABILITY", past)
  delete class_list

  # Print Member Liabilities
  label = print_account_class(reports_stream, label, "select_class", "LIABILITY.MEMBER", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # Print a nice line
  if (!label) {
    underline(73, 8, reports_stream)
    printf "\t%27s %18s %26s\n", "Total Long Term Liabilities",
      print_cash(liabilities[now] - current_liabilities[now]), print_cash(liabilities[past] - current_liabilities[past]) > reports_stream
  }

  underline(73, 8, reports_stream)
  underline(73, 8, reports_stream)
  printf "\t%27s %18s %26s\n\n", "Total Liabilities",
    print_cash(liabilities[now]), print_cash(liabilities[past]) > reports_stream

  # Now find total Equity
  label = sprintf("Share Equity\n")
  label = print_account_class(reports_stream, label, "select_class", "EQUITY", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # the equity
  equity[now]  = - get_cost("*EQUITY", now)
  equity[past] = - get_cost("*EQUITY", past)

  # Print a nice line
  if (!label) {
    underline(73, 8, reports_stream)
    printf "\t%24s %21s %26s\n\n", "Total Equity",
                print_cash(equity[now]), print_cash(equity[past]) > reports_stream
  }

  # Print Accumulated Profits (INCOME - EXPENSES) == (ASSETS - LIABILITY - EQUITY)
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n", "Accumulated Profits",
    print_cash(assets[now] - liabilities[now] - equity[now]), print_cash(assets[past] - liabilities[past] - equity[past]) > reports_stream
  underline(73, 8, reports_stream)

  # Tidy up
  delete assets
  delete liabilities
  delete equity
}

# Print the holdings at time now
function get_market_gains(now, past, is_detailed,    reports_stream) {
  # Show current gains/losses
   # The reports_stream is the pipe to write the schedule out to
   reports_stream = (("OTC" ~ /[mM]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

   # First print the gains out in detail
   print_gains(now, past, is_detailed, "Market Gains", reports_stream, now)
   delete Gains_Stack
}

# Compute annual depreciation
function depreciate_all(now,      a, current_depreciation) {
  # Depreciation is Cost Element I
  Cost_Element = I

  # Depreciate all open fixed assets
  for (a in Leaf)
    if (((a) ~ /^ASSET\.FIXED[.:]/) && is_open(a, now)) {
      # Depreciate
      current_depreciation = depreciate_now(a, now)
      update_cost(a, - current_depreciation, now)

      # Balance accounts
      adjust_cost(DEPRECIATION, current_depreciation, now)
    }

  # Restore defaults
  Cost_Element = ("II")
}

# Allocate second element costs
function allocate_second_element_costs(now,       a, p, second_element) {
  # Allocate everything
  # Cost II => Cost I
  for (a in Leaf)
    if (((a) ~ /^ASSET\.FIXED[.:]/) && is_open(a, now)) {
      # Depreciating assets only use cost elements I or II


      # Get each parcel
      for (p = 0; p < Number_Parcels[a]; p ++) {
        # Is this parcel purchased yet?
        if (Held_From[a][p] > now)
          break # All done
        if ((Held_Until[a][ p] > ( now))) {
          # Debugging


          # Get the second element of the cost
          second_element = (((__MPX_KEY__ = find_key(Accounting_Cost[a][ p][ II],  ( now)))?( Accounting_Cost[a][ p][ II][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][ p][ II][0]):( 0)))))
          if (!((((second_element) - ( Epsilon)) <= 0) && (((second_element) - ( -Epsilon)) >= 0))) {
            # The Second Element Cost is applied to the First Element
            adjust_parcel_cost(a, p, now,   second_element,  I, (0))
            adjust_parcel_cost(a, p, now, - second_element, II, (0))
          }


        } # End of if unsold parcel
      } # End of each parcel


    } # End of each fixed asset a
}


# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      reports_stream, a, p, open_key, close_key, parcel_depreciation, account_depreciation,
                                                                  open_cost, total_depreciaiton, sum_open,
                                                                  second_element, sum_second_element,
                                                                  sale_depreciation, sale_appreciation) {

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = (("OTC" ~ /[dD]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((((now) - 1)) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))
  if ("/dev/null" == reports_stream)
    return

  is_detailed = ("" == is_detailed) ? (0) : is_detailed
  total_depreciation = ""

  # Print out the assets in alphabetical order
  for (a in Leaf)
    if (((a) ~ /^ASSET\.FIXED[.:]/) && (is_open(a, now) || is_open(a, past))) {
      if ("" == total_depreciation) {
        printf "\n" > reports_stream
        print Journal_Title > reports_stream
        printf "Depreciation Schedule for the Period [%11s, %11s]\n\n", get_date(past), get_date(now) > reports_stream
        total_depreciation = 0 # Total value summed here

        # Two types of header
        printf "%16s %11s ", "Asset", "Method" > reports_stream
        if (is_detailed)
          printf "%7s ", "Parcel" > reports_stream

        # The rest of the header
        printf "%9s %11s %17s %14s %20s %9s %15s\n",
                  "From", "To", "Opening", "Closing", "Second Element", "Adjusted", "Depreciation" > reports_stream
        underline(124 + 8 * is_detailed, 6, reports_stream)
      }

      # The opening value of an asset with multiple parcels cannot be tied to a single time
      account_depreciation = sum_open = sum_second_element = 0

      # Get each parcel
      for (p = 0; p < Number_Parcels[a]; p ++) {
        # When was this parcel  opened?
        open_key = Held_From[a][p] # First parcel opened here
        if (open_key < past)
          open_key = past # This must be earlier than now for this asset to be open and considered

        # Is there is a problem if item is sold exactly at same time as depreciation occurs? (no if done carefully)
        if ((Held_Until[a][ p] <= ( now))) {
          close_key = ((Held_Until[a][p]) - 1)
        } else
          close_key = ((now) - 1)

        # parcel open cost
        open_cost = get_parcel_cost(a, p, open_key)
        sum_open += open_cost

        # Always get the parcel depreciation
        parcel_depreciation = ((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  open_key))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0)))) - ((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  close_key))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))

        #  Just track the total depreciation
        account_depreciation   += parcel_depreciation

        # Save second element cost
        sum_second_element += (second_element = (((__MPX_KEY__ = find_key(Accounting_Cost[a][ p][ II],  ( ((close_key) - 1))))?( Accounting_Cost[a][ p][ II][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][ p][ II][0]):( 0))))))

        # Record detailed statement
        # Is this a named parcel?
        if (is_detailed && Number_Parcels[a] > 1) {
          if ((( a in Parcel_Tag) && ( p in Parcel_Tag[ a])))
            printf "%28s %6d ", Parcel_Tag[a][p], p > reports_stream
          else
            printf "%35d ", p > reports_stream

          # Depreciation is the sum of the I tax adjustments
          printf " [%11s, %11s] %14s %14s %14s %14s %14s\n",
                    get_date(open_key), get_date(close_key), print_cash(open_cost),
                    print_cash(open_cost - parcel_depreciation),
                    print_cash(second_element),
                    print_cash(get_parcel_cost(a, p, close_key)),
                    print_cash(parcel_depreciation) > reports_stream
        } # End of is_detailed
      } # End of each parcel

      # Depreciation is the sum of the tax adjustments
      # When was this asset opened?
      open_key = Held_From[a][0] # First parcel opened here
      if (open_key < past)
        open_key = past # This must be less than now for this asset to be open and considered
      if ((!is_open((a), ( now))))
        close_key = ((held_to(a, now)) - 1)
      else
        close_key = ((now) - 1)

      # Two types of footer
      printf "%16s %11s ", (Leaf[a]), Depreciation_Method[Method_Name[a]] > reports_stream
      if (is_detailed)
        printf "%8s", " " > reports_stream

      # The rest of the footer
      printf "[%11s, %11s] %14s %14s %14s %14s %14s\n",
        get_date(open_key), get_date(close_key), print_cash(sum_open),
        print_cash(sum_open - account_depreciation),
        print_cash(sum_second_element),
        print_cash(get_cost(a, close_key)),
        print_cash(account_depreciation) > reports_stream

      # Track total depreciation too
      total_depreciation += account_depreciation
    } # End of a depreciating asset

  # Is there any depreciation/appreciation due to the sale of depreciating assets?
  sale_appreciation = get_cost(SOLD_APPRECIATION, now) - get_cost(SOLD_APPRECIATION, past)
  sale_depreciation = get_cost(SOLD_DEPRECIATION, now) - get_cost(SOLD_DEPRECIATION, past)
  if (!((((sale_depreciation) - ( Epsilon)) <= 0) && (((sale_depreciation) - ( -Epsilon)) >= 0)))
    printf  "\n%24s %*s\n", "Depreciation from Sales", 105 + 8 * is_detailed, print_cash(sale_depreciation) > reports_stream
  if (!((((sale_appreciation) - ( Epsilon)) <= 0) && (((sale_appreciation) - ( -Epsilon)) >= 0)))
    printf  "\n%24s %*s\n", "Appreciation from Sales", 105 + 8 * is_detailed, print_cash(-sale_appreciation) > reports_stream
  total_depreciation += sale_depreciation + sale_appreciation

  # Print a nice line
  if (!((((total_depreciation) - ( Epsilon)) <= 0) && (((total_depreciation) - ( -Epsilon)) >= 0))) {
    underline(124 + 8 * is_detailed, 6, reports_stream)
    printf "%24s %*s\n",  "Period Depreciation", 105 + 8 * is_detailed, print_cash(total_depreciation) > reports_stream
  }
} # End of print depreciating holdings



#
#
## Dividend Qualification Function
##
## Compute whether dividends are qualified or not
function print_dividend_qualification(now, past, is_detailed,

                                         reports_stream,
                                         a, underlying_asset, credit_account,
                                         qualifying_date,
                                         qualified_units, total_units, qualified_fraction, q,
                                         qualified_payment,
                                         key, next_key, payment,
                                         print_header) {

  ## Output Stream => Dividend_Report
  reports_stream = (("OTC" ~ /[qQ]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

  # For each dividend in the previous accounting period
  print Journal_Title > reports_stream
  if (is_detailed)
    printf "Detailed Dividend Qualification Report\n" > reports_stream
  else
    printf "Dividend Qualification Report\n" > reports_stream
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > reports_stream

  # A header
  print_header = (1)

  # Sum  the qualified payments
  qualified_payment = 0

  # Get each dividend/distribution
  # Start with dividends - this could be abstracted out later to include distributions
  # First key
  for (a in Leaf)
    if (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION.CLOSE") "[.:]"))) { # Only closely held distributions are considered

      # Get each payment in the target period
      key = (find_key(Cost_Basis[a],  now))

      while (key > past) {
        # A heading
        if (print_header) {
          printf "\n%12s\n", "Dividends" > reports_stream
          printf "%12s %13s %14s %14s %14s %14s %13s\n",
                  "Asset", "Ex-Div Date", "Eligible Units", "Payment Date", "Qualified", "% Qualified", "Payment"  > reports_stream
          underline(95, 6, reports_stream)
          print_header = (0)
        }

        # The current asset
        assert(a in Underlying_Asset, "Can't find underlying asset for %s" Leaf[a]) > reports_stream
        underlying_asset = Underlying_Asset[a]

        # We will need the next key
        next_key = (find_key(Cost_Basis[a], (( key) - 1)))

        # Short cut directly to the value of the dividend payment
        payment = - (get_cost(a,  key) - get_cost(a, (( key) - 1)))

        # The qualifying date is one day before the ex-dividend date
        if (qualifying_date = get_exdividend_date(underlying_asset, key))
          qualifying_date = ((yesterday(qualifying_date, (12))) + 1)

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > (-1), sprintf("%s: %s <%s>",  Leaf[a], Read_Date_Error, get_date(key)))

        # Catch  the case that no qualification date was recorded
        if (qualifying_date) {
          # These are the units that were qualified on the qualifying date
          qualified_units = ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[underlying_asset],   qualifying_date))?( Qualified_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[underlying_asset][0]):( 0))))):( ((underlying_asset in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[underlying_asset],    qualifying_date))?( Total_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[underlying_asset][0]):( 0))))):( 0))))

          # Now get the total units
          total_units = ((underlying_asset in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[underlying_asset],   qualifying_date))?( Total_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[underlying_asset][0]):( 0))))):( 0))

          # If not all units are qualified need to check the second half of the Qualification Window
          if (!((((total_units - qualified_units) - ( Epsilon)) <= 0) && (((total_units - qualified_units) - ( -Epsilon)) >= 0))) {
            q = maximum_entry(Qualified_Units[underlying_asset], qualifying_date, qualifying_date + 0.5 * Qualification_Window)
            qualified_units = (((q) - ( qualified_units) > 0)?(q):( qualified_units))
            qualified_fraction = qualified_units / total_units

            # Should never be greater than unity
            assert(!(((qualified_fraction - 1.0) - ( Epsilon)) > 0), sprintf("Qualified Units[%s] => %.3f > Units held on qualification date <%s>",
              underlying_asset, qualified_units, total_units))
          } else
            qualified_fraction = 1.0

          # The output - show ex-dividend date not qualifying date
          printf "%13s %12s %12.3f %16s %14.3f %12.2f %16s\n", Leaf[underlying_asset], get_date(tomorrow(qualifying_date)), total_units,
                  get_date(key), qualified_units, 100.0 * qualified_fraction, print_cash(payment) > reports_stream
        } else {
          # No qualification date
          qualified_units    = total_units = ((underlying_asset in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[underlying_asset],   now))?( Total_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[underlying_asset][0]):( 0))))):( 0))
          qualified_fraction = 1.0

          printf "%13s %12s %12.3f %16s %14.3f %12.2f %16s\n", Leaf[underlying_asset], "No Date", total_units,
                  get_date(key), total_units, 100.0, print_cash(payment) > reports_stream
        }

        # Sum qualified payment
        qualified_payment += qualified_fraction * payment

        # Make the appropriate changes for the current tax jurisdiction
        if (Start_Journal)
          @Dividend_Qualification_Function(a, key, 1.0 - qualified_fraction)

        # Get the next key
        key = next_key
      } # End of while each key in the window
    } # End of if a dividend

    # summary
    underline(95, 6, reports_stream)
    payment = get_cost("*INCOME.DIVIDEND", past) + get_cost("*INCOME.DISTRIBUTION.CLOSE", past) -               get_cost("*INCOME.DIVIDEND", now) - get_cost("*INCOME.DISTRIBUTION.CLOSE", now)
    if (((((payment) - ( Epsilon)) > 0) || (((payment) - ( -Epsilon)) < 0)))
      printf "%*s%*s %14s %*.2f %*s\n\n", 6, "", 16, "Qualified Dividends", print_cash(qualified_payment),
        43, 100.0 * (qualified_payment / payment), 16, print_cash(payment) > reports_stream

} # End of function print_dividend_qualification

## Helper functions


# This is the newer more compact form of this function
# But it is still carrying a lot of remnant complications
# Replace class_name, blocked_class with a
# selector function
function print_account_class(stream, heading, selector, class_name, blocked_class, income_function, now, now_past, past, past_past, print_all, sign,
  subclass, last_subclass,
  x, account_income, did_print) {

  # Bail out when nothing to print
  if ("/dev/null" == stream)
    return

  # Printing: print_all => 2  ALL
  #           print_all => 1  SOME
  #           print_all => 0  JUST THE FOOTER
  print_all = ("" == print_all) ? 2 : print_all

  # Default sign is positive
  sign = sign ? sign : 1
  account_income[now] = account_income[past] = 0
  account_sum[now] = account_sum[past] = 0
  last_subclass = ""
  did_print = 0

  # First list all the subclasses
  # Record the totals using the infrastructure for accounts
  for (x in Leaf) {
    if (@selector(x, class_name, blocked_class, now)) {

      # The required name component is the last in the parent - watch out for
      # the leading "*" if only a single component
      subclass = get_name_component(Parent_Name[x], 0)
      if (((subclass) ~ ("*")))
        subclass = substr(subclass, 2)

      # Initialize sums
      if (last_subclass != subclass) {
        # If at least one entry found print a summary
        if (did_print) {
          if (print_all > 1)
            ((past)?( underline(73, 8,  stream)):( underline(47, 8,  stream)))
          if (print_all)
            print_subclass_sum(last_subclass, account_sum[now],
                                              account_sum[past], stream)
        }

        # Initialize the subclass sums
        account_sum[now] = account_sum[past] = 0
        last_subclass = subclass
        did_print = 0
      }

      # Sum each subclass
      # Special case for unrealized gains
      # because the asset might be realized at "now" but not at "past"
      if ("get_unrealized_gains" == income_function && (!is_open((x), ( now))))
        account_income[now] = 0
      else {
        account_income[now] = sign * (@income_function(x, now) - @income_function(x, now_past))
        account_sum[now] += account_income[now]
      }

      # This is more complicated still because
      # an account can be closed due to a code change;
      # if this has occurred before "now" then do not record these gains/losses
      if (past) {
        if ("get_unrealized_gains" == income_function && ((!is_open((x), ( past))) || ((x) in Account_Closed && (((Account_Closed[x]) - ( ( now))) < 0))))
          account_income[past] = 0
        else {
          account_income[past] = sign * (@income_function(x, past) - @income_function(x, past_past))
          account_sum[past] += account_income[past]
        }
      }

      if (!((((account_income[now]) - ( Epsilon)) <= 0) && (((account_income[now]) - ( -Epsilon)) >= 0)) || !((((account_income[past]) - ( Epsilon)) <= 0) && (((account_income[past]) - ( -Epsilon)) >= 0))) {
        # Only print the heading if there was a non-zero entry
        if (0 == did_print) {
          if ("" != heading) {
            printf heading > stream

            # Only print out once
            heading = ""
          }
          if (print_all > 1)
            printf "%22s\n", subclass > stream

          # The heading has been printed out
          did_print = 1
        }

        if (print_all > 1) {
          printf "\t%24s %21s", (Leaf[x]), print_cash(account_income[now]) > stream
          if (past)
            printf " %26s", print_cash(account_income[past]) > stream
          printf "\n"> stream
        }
      }
    }
  } # End of each Leaf

  # Print a nice line (careful here when no subclasses found!)
  if (did_print) {
    if (print_all > 1)
      ((past)?( underline(73, 8,  stream)):( underline(47, 8,  stream)))
    if (print_all)
      print_subclass_sum(subclass, account_sum[now], account_sum[past], stream)
  }

  # record sums

  # return heading
  return heading
}

# Watch out for top level name
function print_subclass_sum(name, sum_now, sum_past, stream) {
  printf "\t%24s %21s", substr(name, 1, 1) tolower(substr(name, 2)), print_cash(sum_now) > stream
  if (sum_past)
    printf " %26s\n", print_cash(sum_past) > stream
  else
    printf "\n" > stream
}

# The selector functions are filters for controlling
# what is printed out
# The simplest
function select_class(a, class_name, blocked_class, now) {
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Include class blocking
function block_class(a, class_name, blocked_class, now) {
  if (((a) ~ ("^" ( blocked_class) "[.:]")))
    return (0) # Blocked!

  # Just the simple case
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Block multiple classes...
function block_class_list(a, class_name, blocked_class_list, now,     x) {
  # blocked class might actually be an array of blocked classes
  for (x in blocked_class_list)
    if (((a) ~ ("^" ( x) "[.:]"))) # Blocked!
      return (0)

  # Just the simple case
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Special purpose filter for current accounts
function current_class(a, class_name, blocked_class, now,    maturity) {
  # Is this the right class
  if (((a) ~ ("^" ( class_name) "[.:]"))) {
    # Get current maturity date
    if (a in Maturity_Date) {
      maturity = ((__MPX_KEY__ = find_key(Maturity_Date[a],  now))?( Maturity_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Maturity_Date[a][0]):( 0))))
      if (maturity > ((now) + one_year(now,  1)))
        return (0)
    }

    return (1)
  }

  return (0)
  #return is_class(a, class_name) && !(a in Maturity_Date)
}

# And its pigeon pair
function not_current_class(a, class_name, blocked_class, now,    maturity) {
  # Is this the right class
  if (((a) ~ ("^" ( class_name) "[.:]"))) {
    # Get current maturity date
    if (a in Maturity_Date) {
      maturity = ((__MPX_KEY__ = find_key(Maturity_Date[a],  now))?( Maturity_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Maturity_Date[a][0]):( 0))))
      if (maturity > ((now) + one_year(now,  1)))
        return (1)
    }
  }

  return (0)
  #return is_class(a, class_name) && (a in Maturity_Date)
}

# A write back function
function write_back_losses(future_time, now, limit, available_losses, reports_stream,

                           income_tax, taxable_income, tax_refund,
                           taxable_gains, gains_written_back) {
  #
  # Oldest losses are dealt with first
  # a recursive routine is easy - this could also be flattened into a loop
  #
  # This rewrites existing values but should be ok for repeated calls since
  # after first call one or both values are zero and usnamendable
  #
  # This works by adjusting consituent gains
  # In particular it treats written back losses as LONG losses
  if (now > limit) {
    # Keep going
    available_losses = write_back_losses(future_time, ((now) - one_year(now, -1)), limit, available_losses, reports_stream)

    # Any losses left?
    if (((((available_losses) - ( Epsilon)) <= 0) && (((available_losses) - ( -Epsilon)) >= 0)))
      return 0

    # Record the process
    # that adjusts the gains ONLY in this function
    taxable_gains  = get_cost(((((("SPECIAL.TAXABLE")) ~ ("*")))?( (("SPECIAL.TAXABLE"))):( (("*") (("SPECIAL.TAXABLE"))))), now)
    printf "\t%27s => %13s\n", "Write Back", get_date(now) > reports_stream
    printf "\t%27s => %14s\n", "Gains", print_cash(- taxable_gains) > reports_stream

    # Get the gains
    if ((((taxable_gains) - ( -Epsilon)) < 0)) {
      # There are gains which can be offset against the available losses
      #
      # We only get to here when there are available losses
      if (available_losses + taxable_gains > 0) {
        # More losses than gains
        available_losses += taxable_gains

        # Record gains written back
        gains_written_back = taxable_gains

        # Reset taxable gains
        taxable_gains = 0
      } else {
        # More gains than losses
        taxable_gains += available_losses

        # Record gains written back
        gains_written_back = - available_losses

        # Reset available losses
        available_losses = 0
      }

      # This generates a change in the total income tax - the tax refund
      tax_refund = get_tax(now, Tax_Bands, ((__MPX_KEY__ = find_key(Taxable_Income,  now))?( Taxable_Income[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Taxable_Income[0]):( 0)))) + gains_written_back) - ((__MPX_KEY__ = find_key(Income_Tax,  now))?( Income_Tax[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Income_Tax[0]):( 0))))

      # Update taxable gains
      adjust_cost(WRITTEN_BACK, - gains_written_back, now)
      adjust_cost("*SPECIAL",   + gains_written_back, now)


      # The refund is a simple refundable offset at the future time
      if ((((tax_refund) - ( -Epsilon)) < 0))
        sum_entry(Refundable_Offsets, tax_refund, future_time)

      # Record This
      printf "\t%27s => %14s\n", "Rewritten Gains", print_cash(- taxable_gains) > reports_stream
      printf "\t%27s => %14s\n", "New Available Losses", print_cash(available_losses) > reports_stream
      printf "\t%27s => %14s\n", "Tax Refund", print_cash(- tax_refund) > reports_stream
      printf "\t%27s => %14s\n", "Total Refundable Offset", print_cash(((__MPX_KEY__ = find_key(Refundable_Offsets,  future_time))?( Refundable_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Refundable_Offsets[0]):( 0))))) > reports_stream
    }
  }

  # Finish Up
  printf "\n\n" > reports_stream

  # Finished
  return available_losses
}


# Currency specific modules

#!/usr/local/bin/gawk -f
# p.aud_modules.awk
# Copyright (C) 2018, 2019  Robert Whitehurst
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
# Use C include syntax for Header files



# It needs its own begin section...
BEGIN {

  # // Extras for AUD
  ((SUBSEP in ATO_Levy)?((1)):((0)))
  ((SUBSEP in CGT_Discount)?((1)):((0)))
  ((SUBSEP in Franking_Deficit_Offsets)?((1)):((0)))
  ((SUBSEP in GST_Rate)?((1)):((0)))
  ((SUBSEP in LIC_Allowance)?((1)):((0)))
  ((SUBSEP in LIC_Deduction)?((1)):((0)))
  ((SUBSEP in Low_Income_Offset)?((1)):((0)))
  ((SUBSEP in Middle_Income_Offset)?((1)):((0)))
  ((SUBSEP in Medicare_Levy)?((1)):((0)))
  ((SUBSEP in Member_Liability)?((1)):((0)))
  ((SUBSEP in Pension_Liability)?((1)):((0)))
  ((SUBSEP in Reserve_Rate)?((1)):((0)))

  # The Epoch
  if ("" == Epoch)
    set_epoch()

  # Depreciation
  First_Year_Factor           = 0.5
  Depreciation_Method["PC"]   = "Prime Cost"
  Depreciation_Method["DV"]   = "Dim. Value"
  Depreciation_Method["POOL"] = "Pool Value"

  # Default tax rates
  if (!(Epoch in GST_Rate))
    GST_Rate[Epoch] = 0.10 # Default rate is 10%

  # Foreign Offset Limit
  if (!(Epoch in Foreign_Offset_Limit))
    Foreign_Offset_Limit[Epoch] = 1000.00

  # Initially no LIC Deduction
  LIC_Deduction[Epoch] = 0.0

  # The default tax band
  Tax_Bands[Epoch][0] = 0.15

  #  The Default Medicare Levy
  Medicare_Levy[Epoch][0] = 0.00

  # The default low and middle income offsets
  Low_Income_Offset[Epoch][0] = 0.00
  Middle_Income_Offset[Epoch][0] = 0.00

  # Kept apart to allow correct allocation of member benfits in an SMSF
  CONTRIBUTION_TAX = initialize_account("LIABILITY.TAX:CONTRIBUTION.TAX")
  #

  # For super funds the amount claimable is sometimes reduced to 75%
  Reduced_GST   = 0.75

  # Franking Deficit thresholds
  Franking_Deficit_Reduction = 0.70
  Franking_Deficit_Threshold = 0.10
  Franking_Deficit_First_Year = 0.90
}


# The tax modules for mpx
# These are the ones localized for Australia
# By far the most accurate
#
# Tax computations differ according to the entity type and the state
#
#
function initialize_tax_aud() {
  # Certain global variables are needed by the tax modules -
  # they are jurisdiction dependent and initialized here
  # Only ones which can be influenced by the configuration entries
  # in the journal file - ie anything before /START_JOURNAL/ are added here

  # Handle SMSFs Too
  if ("SMSF" == Journal_Type) {
    # Special versions of functions for SMSFs
    Check_Balance_Function   = "check_balance_smsf"
    Balance_Profits_Function = "balance_profits_smsf"
    Process_Member_Benefits      = "process_member_benefits_smsf"
    Process_Member_Contributions = "process_member_contributions_smsf"
    Update_Member_Function   = "update_member_liability_smsf"
    Update_Profits_Function  = "update_profits_smsf"

    # Special accounts for SMSFs
    ALLOCATED = initialize_account("BALANCING:ALLOCATED")

    # Reserve rate is variable over time
    (( Epoch in Reserve_Rate)?( 0.0):(Reserve_Rate[ Epoch] = ( 0.0))) # Default reserve allocation
  } else
    # Normally ALLOCATED is a synonym for ADJUSTMENTS - only needed by SMSF Journals
    ALLOCATED = ADJUSTMENTS


  # Precision is an issue - use a rational number for some allowances
  if ((Journal_Type ~ /^SMSF$/)) {
    ((CGT_Discount[1] =  1)?( CGT_Discount[2] =  3):( CGT_Discount[2] =  3)) # Capital Gains Discount
    ((LIC_Allowance[1] =  1)?( LIC_Allowance[2] =  3):( LIC_Allowance[2] =  3)) # Listed Investment Company Allowance
  } else if ((Journal_Type ~ /^IND$/)) {
    ((CGT_Discount[1] =  1)?( CGT_Discount[2] =  2):( CGT_Discount[2] =  2)) # Capital Gains Discount
    ((LIC_Allowance[1] =  2)?( LIC_Allowance[2] =  3):( LIC_Allowance[2] =  3)) # Listed Investment Company Allowance
  } else {
    ((CGT_Discount[1] =  0)?( CGT_Discount[2] =  1):( CGT_Discount[2] =  1)) # Capital Gains Discount
    ((LIC_Allowance[1] =  0)?( LIC_Allowance[2] =  1):( LIC_Allowance[2] =  1)) # Listed Investment Company Allowance
  }
}


## Income Tax
# Print the statement of taxable income
# Tax Jurisdiction AUS
function income_tax_aud(now, past, benefits,

                                        write_stream,
                                        taxable_gains, carried_losses,
                                        tax_losses,
                                        market_changes,
                                        accounting_gains, accounting_losses,
                                        foreign_income, exempt_income,
                                        foreign_expenses, extra_tax,
                                        contributions, income_due, other_expenses,
                                        lic_deduction,
                                        other_income, deferred_tax, deferred_gains,
                                        capital_losses,
                                        tax_owed, tax_paid, tax_due, tax_with, tax_cont, income_tax,
                                        franking_offsets, foreign_offsets, franking_balance,
                                        franking_deficit_offsets,
                                        no_carry_offsets, carry_offsets, refundable_offsets, no_refund_offsets,
                                        low_income_offset, middle_income_offset,
                                        taxable_income,
                                        medicare_levy, extra_levy, tax_levy, x, header) {

  # Print this out?
  write_stream = (("OTC" ~ /[tT]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

  # Get market changes
  market_changes = get_cost(UNREALIZED, now) - get_cost(UNREALIZED, past)

  # Let's go
  printf "%s\n", Journal_Title > write_stream
  printf "Statement of Taxable Income\n" > write_stream

  printf "For the year ending %s\n", get_date(yesterday(now)) > write_stream
  underline(81, 0, write_stream)
  printf "%80s\n", strftime("%Y", now, UTC) > write_stream
  printf "%80s\n", "$" > write_stream

  # First entry
  printf "%22s %38s\n", "Benefits Accrued as a Result of Operations", print_cash(benefits) > write_stream

  # Additions
  header = "ADD\n"

  # Start with market losses
  other_income = (((((market_changes) - ( Epsilon)) > 0))?( (market_changes)):(  0))
  if (!((((other_income) - ( Epsilon)) <= 0) && (((other_income) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Unrealized Losses", print_cash(other_income) > write_stream
    header = ""
  }

  # Accounting losses are added - as are taxable gains
  accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)
  if (!((((accounting_losses) - ( Epsilon)) <= 0) && (((accounting_losses) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Capital Losses", print_cash(accounting_losses) > write_stream
    other_income += accounting_losses
    header = ""
  }

  # Non deductible EXPENSES
  # This should be handled by having EXPENSE.NON-DEDUCTIBLE.BENEFIT etc
  #    EXPENSE.NON-DEDUCTIBLE.DEEMED
  #    EXPENSE.NON-DEDUCTIBLE.BENEFIT (SMSF)
  #    EXPENSE.NON-DEDUCTIBLE.DIVIDEND (PTY)
  # Deductible EXPENSE
  #    EXPENSE.DISTRIBUTION (TRUST)
  other_expenses = get_cost("*EXPENSE.NON-DEDUCTIBLE", now) - get_cost("*EXPENSE.NON-DEDUCTIBLE", past)
  if (!((((other_expenses) - ( Epsilon)) <= 0) && (((other_expenses) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Other Non Deductible Expenses", print_cash(other_expenses) > write_stream
    other_income += other_expenses
    header = ""
  }

  # taxable capital gains
  #
  #
  # Australia ignores the distinction between long & short term losses
  # The carried losses are based on the remaining losses; although
  # the carry forward limit should be applied
  taxable_gains = @Get_Taxable_Gains_Function(now, (( past in Capital_Losses)?( ((__MPX_KEY__ = first_key(Capital_Losses[ past]))?( Capital_Losses[ past][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Capital_Losses[ past][0]):( 0))))):( 0)))
  if ((((taxable_gains) - ( -Epsilon)) < 0)) {
    # Gains are a negative number
    other_income -= taxable_gains
    printf "%s\t%40s %32s\n", header, "Taxable Capital Gains", print_cash(-taxable_gains) > write_stream
    header = ""
    carried_losses = 0
  } else {
    # A loss or negligible
    # Record this loss
    carried_losses = taxable_gains
    taxable_gains = 0
  }

  # Losses might sometimes be written back against earlier gains
  # In practice this is always FALSE for Australia
  if ((0) && ((((carried_losses) - ( Epsilon)) > 0) || (((carried_losses) - ( -Epsilon)) < 0))) {
    # Try writing back losses
    printf "\n\t%27s => %14s\n", "Write Back Losses Available", print_cash(carried_losses) > write_stream

    # Rewrite refundable offsets to just before now so they can be zeroed later at a distinct timestamp
    carried_losses = write_back_losses(((now) - 1), ((now) - one_year(now, -1)), (((0))?( (now - one_year(now, (0)))):( Epoch)), carried_losses, write_stream)
  }

  # Imputation Tax Offsets
  #

  # Tax credits received during this FY
  franking_offsets = - (get_cost("*SPECIAL.FRANKING.OFFSET", now) - get_cost("*SPECIAL.FRANKING.OFFSET", past))
  if (!((((franking_offsets) - ( Epsilon)) <= 0) && (((franking_offsets) - ( -Epsilon)) >= 0))) {
    other_income += franking_offsets
    printf "%s\t%40s %32s\n", header, "Franking Offsets", print_cash(franking_offsets) > write_stream
    header = ""
  }

  if (!((((other_income) - ( Epsilon)) <= 0) && (((other_income) - ( -Epsilon)) >= 0))){
    underline(81, 0, write_stream)
    printf "%s\t%40s %32s\n\n", header, "Other Income", print_cash(other_income) > write_stream
    header = ""
  }

  # Reductions
  header = "LESS\n"

  # Expenses
  exempt_income = -(get_cost("*INCOME.EXEMPT", now) - get_cost("*INCOME.EXEMPT", past))
  if (exempt_income > Epsilon) {
    printf "%s\t%40s %32s\n", header, "Exempt Income", print_cash(exempt_income) > write_stream
    header = ""
  }

  # Market and Accounting Capital Gains
  other_expenses = - (((((market_changes) - ( -Epsilon)) < 0))?( (market_changes)):(  0))
  if (other_expenses > Epsilon) {
    printf "%s\t%40s %32s\n", header, "Unrealized Gains", print_cash(other_expenses) > write_stream
    header = ""
  }

  # Tax exempt income
  other_expenses += exempt_income

  # Accounting losses are added - as are taxable gains
  accounting_gains = -(get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past))
  if (!((((accounting_gains) - ( Epsilon)) <= 0) && (((accounting_gains) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Capital Gains", print_cash(accounting_gains) > write_stream
    other_expenses += accounting_gains
    header = ""
  }

  # And the non-concessional contributions
  # Should look at CONTRIBUTION minus the one taxed subclass because maybe more than one tax-free subclass?
  contributions = -(get_cost("*INCOME.CONTRIBUTION.TAX-FREE", now) - get_cost("*INCOME.CONTRIBUTION.TAX-FREE", past))
  if (!((((contributions) - ( Epsilon)) <= 0) && (((contributions) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Non Taxable Contributions", print_cash(contributions) > write_stream
    other_expenses += contributions
    header = ""
  }

  # Finally LIC Deductions (if eligible)
  # LIC deductions 1/3 for SMSF
  #                1/2 for individual
  #                0/3 for company
  lic_deduction = - ((LIC_Allowance[2])?( (LIC_Allowance[1]/LIC_Allowance[2])):( assert((0), "Division by zero in rational fraction" LIC_Allowance[1] "/" LIC_Allowance[2]))) * (((__MPX_KEY__ = find_key(LIC_Deduction,  now))?( LIC_Deduction[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( LIC_Deduction[0]):( 0)))) - ((__MPX_KEY__ = find_key(LIC_Deduction,  past))?( LIC_Deduction[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( LIC_Deduction[0]):( 0)))))

  # Always apply allowance at this point to catch explicit allocations to LIC
  if (!((((lic_deduction) - ( Epsilon)) <= 0) && (((lic_deduction) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header,"LIC Deduction", print_cash(lic_deduction) > write_stream
    other_expenses += lic_deduction
    header = ""
  }

  # Summarize other expenses
  if (!((((other_expenses) - ( Epsilon)) <= 0) && (((other_expenses) - ( -Epsilon)) >= 0))) {
    underline(81, 0, write_stream)
    printf "%s\t%40s %32s\n\n", header, "Other Expenses", print_cash(other_expenses) > write_stream
    header = ""
  }

  taxable_income = benefits + other_income - other_expenses
  underline(81, 0, write_stream)
  header = ""
  printf "%48s %32s\n\n", "TAXABLE INCOME OR LOSS", print_cash(taxable_income) > write_stream

  # Record this quantity
  (Taxable_Income[ now] = ( taxable_income))

  # Keep the income tax on the taxable income - the actual amount owed may change due to tax offsets etc
  income_tax = tax_owed = get_tax(now, Tax_Bands, taxable_income) # Just need total tax
  printf "%48s %32s\n", "Income Tax on Taxable Income or Loss", print_cash(tax_owed) > write_stream
  underline(81, 0, write_stream)

  # Record this quantity
  (Income_Tax[ now] = ( income_tax))

  # Also is a medicare levy payable?
  if ((Journal_Type ~ /^IND$/))
    medicare_levy = get_tax(now, Medicare_Levy, taxable_income)
  else
    medicare_levy = 0

  # Tax Offsets
  #  Apply in this order
  #    No-Refund-No-Carry (C-TAX)
  #    Foreign-Offsets    (C-TAX)
  #
  #    Franking-Offsets   (I-TAX)
  #    For companies they are similar to
  #    No-Refund-No-Carry BUT can generate a loss
  #
  #    No-Refund-Carry    (D-TAX)
  #
  #    Franking-Offsets   (I-TAX)
  #    For individuals and smsf they are refundable
  #
  #    Refund             (E-TAX)
  #
  #    Franking-Deficit   (F-TAX)

  # Tax adjustments
  header = "LESS\n"

  ## Franking deficit needs to be checked here
  if ((Journal_Type ~ /^(PTY|CORP|LTD)$/)) {

    # Franking
    # Check the franking balance (not the same as offsets, which were accumulated this FY)
    franking_balance = get_cost(FRANKING, ((now) - 1))

    # The franking deficit offsets
    franking_deficit_offsets = ((__MPX_KEY__ = find_key(Franking_Deficit_Offsets,  now))?( Franking_Deficit_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Franking_Deficit_Offsets[0]):( 0))))


    # Need to check for franking deficit tax here
    if ((((franking_balance) - ( -Epsilon)) < 0)) {
      # This is a condition for franking deficit tax - that the franking balance
      # is above zero; in fact it is not a sufficient condition; since a refund
      # within three months of the EOFY will also trigger it
      printf "\t%40s\n", "Franking Balance is Overdrawn" > write_stream

      # Compute the franking deficit tax due
      printf "\t%40s %32s\n", "Franking Deficit Tax Due", print_cash(- franking_balance) > write_stream

      # Save the franking deficit tax as a future tax offset
      #
      # Check for reduction - if deficit is greater than 10% of the available offsets
      # reduce the offset (neglect special cases)
      #
      x = Franking_Deficit_Threshold * franking_offsets
      #
      # Reduction occurs when (unless first year when rules more complex)
      # -f > 0.10 * (-x)
      # 0.1 * (x) - f > 0
      if (Show_Extra)
        printf "%48s %32s>\n", "<Franking Deficit Threshold", print_cash(x) > write_stream
      if ((((- franking_balance) - ( x)) > 0)) {
        franking_deficit_offsets -= Franking_Deficit_Reduction * franking_balance
        printf "%48s\n", "Franking Deficit Offset Reduction Applied" > write_stream
      } else
        franking_deficit_offsets -= franking_balance

      if (!((((franking_deficit_offsets) - ( Epsilon)) <= 0) && (((franking_deficit_offsets) - ( -Epsilon)) >= 0)))
        printf "%48s %32s\n\n", "New Franking Deficit Offsets", print_cash(franking_deficit_offsets) > write_stream

      # Do adjust tax owed
      tax_owed -= franking_balance

      printf "%48s %32s\n\n", "Tax Owed After Using Non-Refundable Offsets", print_cash(tax_owed) > write_stream

      # And reset franking balance
      # This is effectively a payment of stamped franking credits into the account
      if (Start_Journal) {
        adjust_cost(FRANKING, - franking_balance, now)
        adjust_cost(FRANKING_STAMPED, franking_balance, now)
      }
    }
  }

  # Report the Imputation and Foreign Offsets
  if (!((((franking_offsets) - ( Epsilon)) <= 0) && (((franking_offsets) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Franking Offsets", print_cash(franking_offsets) > write_stream
    header = ""
  }

  # Foreign offsets
  # Are no-refund-no-carry
  foreign_offsets = - (get_cost("*SPECIAL.FOREIGN.OFFSET", now) - get_cost("*SPECIAL.FOREIGN.OFFSET", past))
  if (!((((foreign_offsets) - ( Epsilon)) <= 0) && (((foreign_offsets) - ( -Epsilon)) >= 0))) {
    # Foreign offsets have complex rules too :( sigh ):
    #
    # If they are not greater than the Foreign_Offset_Limit it is ok to just use  them
    if ((((foreign_offsets) - ( ((__MPX_KEY__ = find_key(Foreign_Offset_Limit,  now))?( Foreign_Offset_Limit[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Foreign_Offset_Limit[0]):( 0)))))) > 0)) {
      # But they are greater  ....
      # we have taxable_income
      # and income_tax
      # (which are before any offsets)

      # compute the income tax that would be due if no foreign income or expenses were present
      foreign_income   = - (get_cost("*INCOME.FOREIGN", now) - get_cost("*INCOME.FOREIGN", past))
      foreign_expenses = - (get_cost("*EXPENSE.FOREIGN", now) - get_cost("*EXPENSE.FOREIGN", past))

      extra_tax = income_tax - get_tax(now, Tax_Bands, taxable_income - foreign_income + foreign_expenses)
      if ((Journal_Type ~ /^IND$/))
        extra_tax += get_tax(now, Medicare_Levy, taxable_income - foreign_income + foreign_expenses)
      if (extra_tax < foreign_offsets)
        foreign_offsets = max(((__MPX_KEY__ = find_key(Foreign_Offset_Limit,  now))?( Foreign_Offset_Limit[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Foreign_Offset_Limit[0]):( 0)))), extra_tax)

      printf "\t%40s\n", "Foreign Offset Limit Applied" > write_stream
    } else
      extra_tax = 0

    # The offsets
    printf "%s\t%40s %32s\n\n", header, "Foreign Offsets", print_cash(foreign_offsets) > write_stream
    header = ""

  } else
    foreign_offsets = 0

  # No Carry Offsets (Class C)
  # The low income and middle income tax offsets depend on income
  if ((Journal_Type ~ /^IND$/)) {
    low_income_offset = get_tax(now, Low_Income_Offset, taxable_income)
    middle_income_offset = get_tax(now, Middle_Income_Offset, taxable_income)

    # This is an Australian no-carry offset computed from the taxable income


    # Set the no_carry offsets
    no_carry_offsets = low_income_offset + middle_income_offset
    no_carry_offsets -= (((__MPX_KEY__ = find_key(No_Carry_Offsets,  now))?( No_Carry_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( No_Carry_Offsets[0]):( 0)))) - ((__MPX_KEY__ = find_key(No_Carry_Offsets,  past))?( No_Carry_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( No_Carry_Offsets[0]):( 0)))))
  } else
    # Just get the total change in the offset
    no_carry_offsets = -(((__MPX_KEY__ = find_key(No_Carry_Offsets,  now))?( No_Carry_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( No_Carry_Offsets[0]):( 0)))) - ((__MPX_KEY__ = find_key(No_Carry_Offsets,  past))?( No_Carry_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( No_Carry_Offsets[0]):( 0)))))

  # Foreign offsets are no-carry offsets
  no_carry_offsets += foreign_offsets

  # The no-carry offset
  if (((((no_carry_offsets) - ( Epsilon)) > 0) || (((no_carry_offsets) - ( -Epsilon)) < 0))) {
    printf "%s\t%40s %32s\n", header, "Total No-Carry Offsets", print_cash(no_carry_offsets) > write_stream
    header = ""
  }

  # Other offsets
  # The carry offset (Class D)
  carry_offsets = -(((__MPX_KEY__ = find_key(Carry_Offsets,  now))?( Carry_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Carry_Offsets[0]):( 0)))) - ((__MPX_KEY__ = find_key(Carry_Offsets,  past))?( Carry_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Carry_Offsets[0]):( 0)))))
  if (!((((carry_offsets) - ( Epsilon)) <= 0) && (((carry_offsets) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Total Carry Offsets", print_cash(carry_offsets) > write_stream
    header = ""
  }

  # The refundable offset (Class E)
  refundable_offsets = - (((__MPX_KEY__ = find_key(Refundable_Offsets,  now))?( Refundable_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Refundable_Offsets[0]):( 0)))) - ((__MPX_KEY__ = find_key(Refundable_Offsets,  past))?( Refundable_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Refundable_Offsets[0]):( 0)))))
  if (!((((refundable_offsets) - ( Epsilon)) <= 0) && (((refundable_offsets) - ( -Epsilon)) >= 0))) {
    printf "%s\t%40s %32s\n", header, "Total Refundable Offsets", print_cash(refundable_offsets) > write_stream
    header = ""
  }
  printf "\n" > write_stream

  # Franking offsets are (currently) refundable for SMSF and individuals
  if ((Journal_Type ~ /^SMSF$/) || (Journal_Type ~ /^IND$/)) {
    refundable_offsets += franking_offsets
    franking_offsets = 0
  } else
    no_carry_offsets += franking_offsets

  # At this stage no-carry and carry offsets behave the same
  no_refund_offsets = no_carry_offsets + carry_offsets

  # Apply the no_refund offsets (if any)
  if ((((tax_owed) - ( Epsilon)) > 0) && (((no_refund_offsets) - ( Epsilon)) > 0)) {
    # Since franking offsets can generate a loss add them to
    # both sides of the balance
    tax_owed += franking_offsets

    if ((((tax_owed) - ( no_refund_offsets)) < 0)) {
      # How many carry offsets were used?
      if ((((tax_owed) - ( no_carry_offsets)) > 0)) # Some were used
        carry_offsets -= (tax_owed - no_carry_offsets)

      # information
      printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(tax_owed - franking_offsets) > write_stream
      tax_owed = 0
    } else { # All the no_refund offsets were used
      tax_owed -= no_refund_offsets
      carry_offsets = 0
      if ((((no_refund_offsets) - ( franking_offsets)) > 0))
        printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(no_refund_offsets - franking_offsets) > write_stream
    }

    # OK now if the tax_owed is less than the amount of franking offsets
    # then the difference is transferred to tax losses
    if ((((tax_owed) - ( franking_offsets)) < 0)) {
      franking_offsets -= tax_owed

      printf "\t%40s %32s>\n", "<Franking Offsets Used", print_cash(tax_owed) > write_stream
      # Report remaining  franking offsets
      if ((((franking_offsets) - ( Epsilon)) > 0))
        printf "\t%40s %32s>\n", "<Franking Offsets Remaining", print_cash(franking_offsets) > write_stream

      tax_owed = 0
    } else {
      tax_owed -= franking_offsets
      if ((((franking_offsets) - ( Epsilon)) > 0))
        printf "\t%40s %32s>\n", "<All Franking Offsets Used", print_cash(franking_offsets) > write_stream
      franking_offsets = 0
    }

    # Report tax owed
    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Non-Refundable Offsets", print_cash(tax_owed) > write_stream
  } # End of if any attempt to apply non-refundable assets

  # Now apply refundable offsets - but note these will not generate a tax loss - since they are refunded :)
  if ((((refundable_offsets) - ( Epsilon)) > 0)) {
    tax_owed -= refundable_offsets
    printf "\t%40s %32s>\n", "<Refundable Offsets Used", print_cash(refundable_offsets) > write_stream
    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Refundable Offsets", print_cash(tax_owed) > write_stream
  }

  # Franking deficit
  # Finally franking deficit tax offsets can be applied
  if ((((tax_owed) - ( Epsilon)) > 0) && (((franking_deficit_offsets) - ( Epsilon)) > 0)) {
    if ((((tax_owed) - ( franking_deficit_offsets)) < 0)) {
      # How many franking deficit tax offsets were used?
      if ((((tax_owed) - ( franking_deficit_offsets)) > 0)) # Some were used
        franking_deficit_offsets -= tax_owed

      # Save carried franking deficit offsets
      sum_entry(Franking_Deficit_Offsets, -tax_owed, now)

      # information
      printf "\t%40s %32s>\n", "<Franking Deficit Tax Offsets Used", print_cash(tax_owed) > write_stream
      tax_owed = 0
    } else { # All the franking deficit offsets were used
      tax_owed -= franking_deficit_offsets
      printf "\t%40s %32s>\n", "<Franking Deficit Tax Offsets Used", print_cash(franking_deficit_offsets) > write_stream

      # Save carried franking deficit offsets
      sum_entry(Franking_Deficit_Offsets, -franking_deficit_offsets, now)
      franking_deficit_offsets = 0
    }
    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Franking Deficit Offsets", print_cash(tax_owed) > write_stream
    if ((((franking_deficit_offsets) - ( Epsilon)) > 0))
      printf "\t%40s %32s>\n", "<Franking Deficit Offsets Remaining", print_cash(((__MPX_KEY__ = find_key(Franking_Deficit_Offsets,  now))?( Franking_Deficit_Offsets[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Franking_Deficit_Offsets[0]):( 0))))) > write_stream
  }

  #
  # Tax Losses
  #
  # The carried tax losses should be computed using the carried losses function
  tax_losses = get_carried_losses(now, Tax_Losses, 0, 0, write_stream)

  # Losses can either be extinguished or (if there are new losses) carried forward
  # We can reduce tax_owed to zero, but not increase or generate a loss
  # Notice tax losses are stored as the income that generates the loss but
  # tax_owed is actually the tax
  if ((((tax_owed) - ( Epsilon)) > 0)) {
    # If tax is owed franking offsets must be all used
    assert(((((franking_offsets) - ( Epsilon)) <= 0) && (((franking_offsets) - ( -Epsilon)) >= 0)), "Can't have remaining franking offsets if tax is still owed")

    # Tax losses available for use - and tax is owed - compute marginal tax change
    if ((((tax_losses) - ( Epsilon)) > 0)) {
      # x is the tax that would be paid on the tax_losses
      x = get_tax(now, Tax_Bands, tax_losses + taxable_income) - income_tax
    } else # No tax owed
      x = 0

    # We have tax owed for this year (tax_owed)
    # And we have the tax that would be extinguished by the carried tax losses (x)
    # Now we need to compute the change in the tax losses this would equate to
    if ((((tax_owed) - ( x)) < 0)) {
      # Yes so some losses will be extinguished
      # Which will reduce tax_owed to zero - so the effective reduction
      # in tax losses is the income that would produce tax equal to tax_owed
      x = - get_taxable_income(now, Tax_Bands, tax_owed) # This is effectively a gain - so make it negative
      tax_owed = 0
    } else if (((((x) - ( Epsilon)) > 0) || (((x) - ( -Epsilon)) < 0))) {
      # All losses extinguished
      tax_owed -= x
      x = - get_taxable_income(now, Tax_Bands, x) # This is effectively a gain - so make it negative
    }

    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Carried Tax Losses", print_cash(tax_owed) > write_stream

    # Tax owed is negative - so losses are increased but allow for refundable offsets which were returned
  } else if (!(((tax_owed + refundable_offsets) - ( Epsilon)) > 0)) { # Increase losses
    # This is a bit tricky
    # (unused) franking offsets may still be present here
    # plus the actual tax owed is modifiable by any refundable offsets (which will be refunded)
    x = - get_taxable_income(now, Tax_Bands, tax_owed + refundable_offsets - franking_offsets)
  } else if ((((tax_owed) - ( -Epsilon)) < 0)) { # Losses recorded this FY
    x = - get_taxable_income(now, Tax_Bands, tax_owed)
    tax_owed = 0
  } else # Zero losses
    x = 0

  # Now we can update the carried tax losses at last
  tax_losses = get_carried_losses(now, Tax_Losses, x, 0, write_stream)

  # Print the tax owed
  if (!header) {
    underline(81, 0, write_stream)
    printf "%48s %32s\n\n", "CURRENT TAX OR REFUND", print_cash(tax_owed) > write_stream
  }

  #
  # Tax Due
  #
  # Compute tax paid
  tax_paid = get_cost(PAYG, ((now) - 1))

  # And tax witheld
  tax_with = get_cost(WITHOLDING, ((now) - 1))

  # If this is SMSF the levy is required
  if ((Journal_Type ~ /^SMSF$/))
    printf "\t%40s %32s\n", "Supervisory Levy", print_cash(((__MPX_KEY__ = find_key(ATO_Levy,  now))?( ATO_Levy[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( ATO_Levy[0]):( 0))))) > write_stream

  # Medicare levy (if any)
  if (!((((medicare_levy) - ( Epsilon)) <= 0) && (((medicare_levy) - ( -Epsilon)) >= 0))) {
    printf "\t%40s %32s\n", "Medicare Levy", print_cash(medicare_levy) > write_stream
    tax_owed += medicare_levy
  }

  # Any other levys
  tax_levy = - get_cost("*LIABILITY.CURRENT.LEVY", ((now) - 1))
  if (((((tax_levy) - ( Epsilon)) > 0) || (((tax_levy) - ( -Epsilon)) < 0))) {
    printf "\t%40s %32s\n", "Tax Levies", print_cash(tax_levy) > write_stream
    tax_owed += tax_levy
  }

  if (!((((tax_paid) - ( Epsilon)) <= 0) && (((tax_paid) - ( -Epsilon)) >= 0)))
    printf "\t%40s %32s\n", "Income Tax Distributions Paid", print_cash(tax_paid) > write_stream
  if (!((((tax_with) - ( Epsilon)) <= 0) && (((tax_with) - ( -Epsilon)) >= 0)))
    printf "\t%40s %32s\n", "Income Tax Withheld", print_cash(tax_with) > write_stream

  # Compute income tax due
  tax_due = tax_owed - (tax_paid + tax_with)
  underline(81, 0, write_stream)
  printf "%48s %32s\n\n\n", "AMOUNT DUE OR REFUNDABLE", print_cash(((__MPX_KEY__ = find_key(ATO_Levy,  now))?( ATO_Levy[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( ATO_Levy[0]):( 0)))) + tax_due) > write_stream

  # Clean up balance sheet - watch out for unbalanced transactions
  # Save contribution tax accounted for
  tax_cont = get_cost(CONTRIBUTION_TAX, ((now) - 1))

  # Now save quantities -
  if (Start_Journal) {
    # Overall impact of following adjustments is (- tax_owed)
    adjust_cost(TAX,   (tax_paid + tax_with) - (tax_owed), now)
    adjust_cost(PAYG, - tax_paid,            now)
    adjust_cost(WITHOLDING,      - tax_with, now)

    # Allocated is really being stored with the wrong sign...
    adjust_cost(ALLOCATED, -(tax_cont + tax_owed), now)
    adjust_cost(CONTRIBUTION_TAX, -tax_cont, now)
  }

  # Print out the tax and capital losses carried forward
  # These really are for time now - already computed
  capital_losses = (( now in Capital_Losses)?( ((__MPX_KEY__ = first_key(Capital_Losses[ now]))?( Capital_Losses[ now][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Capital_Losses[ now][0]):( 0))))):( 0))
  if (Show_Extra) {
    # Report on the losses
    report_losses(now, Capital_Losses, "Capital Losses", write_stream)
    x = (( past in Capital_Losses)?( ((__MPX_KEY__ = first_key(Capital_Losses[ past]))?( Capital_Losses[ past][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Capital_Losses[ past][0]):( 0))))):( 0))
    if ((((capital_losses) - ( x)) > 0))
      printf "\t%40s %32s\n", "Capital Losses Generated", print_cash(x - capital_losses) > write_stream
    else if ((((capital_losses) - ( x)) < 0))
      printf "\t%40s %32s\n", "Capital Losses Extinguished", print_cash(capital_losses - x) > write_stream
  }
  if (!((((capital_losses) - ( Epsilon)) <= 0) && (((capital_losses) - ( -Epsilon)) >= 0)))
    printf "\t%40s %32s\n", "Capital Losses Carried Forward", print_cash(capital_losses) > write_stream

  tax_losses = (( now in Tax_Losses)?( ((__MPX_KEY__ = first_key(Tax_Losses[ now]))?( Tax_Losses[ now][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Losses[ now][0]):( 0))))):( 0))

  if (Show_Extra) {
    report_losses(now, Tax_Losses, "Tax Losses", write_stream)
    x = (( past in Tax_Losses)?( ((__MPX_KEY__ = first_key(Tax_Losses[ past]))?( Tax_Losses[ past][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Losses[ past][0]):( 0))))):( 0))
    if ((((tax_losses) - ( x)) > 0))
      printf "\t%40s %32s\n", "Tax Losses Generated", print_cash(x - tax_losses) > write_stream
    else if ((((tax_losses) - ( x)) < 0))
      printf "\t%40s %32s\n", "Tax Losses Extinguished", print_cash(tax_losses - x) > write_stream
  }
  if (!((((tax_losses) - ( Epsilon)) <= 0) && (((tax_losses) - ( -Epsilon)) >= 0)))
    printf "\t%40s %32s\n", "Tax Losses Carried Forward", print_cash(tax_losses) > write_stream

  # Franking
  if (!((((get_cost(FRANKING, now)) - ( Epsilon)) <= 0) && (((get_cost(FRANKING, now)) - ( -Epsilon)) >= 0)))
    printf "\t%40s %32s\n", "Franking Balance Carried Forward", print_cash(get_cost(FRANKING, now)) > write_stream

  # Franking Deficit
  # Save the franking deficit offsets
  if (!((((franking_deficit_offsets) - ( Epsilon)) <= 0) && (((franking_deficit_offsets) - ( -Epsilon)) >= 0)))
    printf "%48s %32s\n\n", "Franking Deficit Offsets Carried Forward", print_cash(franking_deficit_offsets) > write_stream
  else
    franking_deficit_offsets = 0
  if (Start_Journal)
    (Franking_Deficit_Offsets[ now] = ( -franking_deficit_offsets))

  # Update carry forward offsets
  if (!((((carry_offsets) - ( Epsilon)) <= 0) && (((carry_offsets) - ( -Epsilon)) >= 0)))
    printf "\t%40s %32s\n", "Non-Refundable Offsets Carried Forwards", print_cash(carry_offsets) > write_stream
  else
    carry_offsets = 0
  if (Start_Journal)
    (Carry_Offsets[ now] = ( -carry_offsets))

  # End report
  printf "\n" > write_stream

  if (Start_Journal) {
    # Now we need Deferred Tax - the hypothetical liability that would be due if all
    # assets were liquidated today
    deferred_gains = get_cost(UNREALIZED, now)

    # Gains are negative - losses are positive
    if ((((deferred_gains) - ( -Epsilon)) < 0))
      # Deferred tax losses can reduce future tax liability so are a deferred tax asset
      deferred_tax = - get_tax(now, Tax_Bands, taxable_income - deferred_gains) - income_tax
    else
      deferred_tax = 0

    # Compute change in deferred tax
    x = deferred_tax - get_cost(DEFERRED, past)
    adjust_cost(DEFERRED, x, now)

    # Get the change this FY
    # If x < 0 EXPENSE
    # if x > 0 INCOME
    if (!((((x) - ( Epsilon)) <= 0) && (((x) - ( -Epsilon)) >= 0))) {
      # Adjust cost/receipts for deferred expense/income
      # For a none SMSF this is a synonym for ADJUSTMENTS
      # There is a sign error here...
      adjust_cost(ALLOCATED, x, now)
    }
  }

}

## This should become jurisdiction specific
## There are complications with the discounting
function get_taxable_gains_aud(now, losses,

                           discount, long_gains, short_gains) {
  # There are two uses for this function
  # One is to get the net combined gains & losses disregarding carried losses
  # The other is to compute the actual taxable gains which (in Australia) can be discounted
  if ("" == losses)
    # When no lossses are passed in get the net gains & losses
    discount = losses = 0
  else
    discount = ((CGT_Discount[2])?( (CGT_Discount[1]/CGT_Discount[2])):( assert((0), "Division by zero in rational fraction" CGT_Discount[1] "/" CGT_Discount[2])))

  # This function computes the taxable gains
  # It works for partioned long & short gains
  # And also for deferred gains when all such gains are long
  losses     += get_cost(((((("SPECIAL.TAXABLE.LOSSES.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.LONG"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.LONG"))))), now) + get_cost(((((("SPECIAL.TAXABLE.LOSSES.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.LOSSES.SHORT"))):( (("*") (("SPECIAL.TAXABLE.LOSSES.SHORT"))))), now)
  long_gains  = get_cost(((((("SPECIAL.TAXABLE.GAINS.LONG")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.LONG"))):( (("*") (("SPECIAL.TAXABLE.GAINS.LONG"))))), now)
  short_gains = get_cost(((((("SPECIAL.TAXABLE.GAINS.SHORT")) ~ ("*")))?( (("SPECIAL.TAXABLE.GAINS.SHORT"))):( (("*") (("SPECIAL.TAXABLE.GAINS.SHORT"))))), now)

  # Suppress negligible losses
  losses      = (((((losses) - ( Epsilon)) > 0))?( (losses)):(  0))

  # Summarize starting point

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  # Australian scheme & US Scheme are same
  # once short & long losses are disregarded
  if (!(((losses + short_gains + long_gains) - ( -Epsilon)) < 0)) {
    # More carried losses generated
    losses += short_gains + long_gains

    # Zero negligible losses
    if (((((losses) - ( Epsilon)) <= 0) && (((losses) - ( -Epsilon)) >= 0)))
      losses = 0

    # Zero the gains
    short_gains = long_gains = 0
  } else if (!(((losses + short_gains) - ( -Epsilon)) < 0)) {
    # This can happen if when the losses are insufficient to
    # remove all the long gains
    losses += short_gains # reduce losses
    long_gains += losses  # apply them against long gains

    # But not a long term loss
    losses = short_gains = 0

  } else {
    # Long and Short Gains
    short_gains += losses # Reduce short gains
    losses = 0

  }

  # Return either taxable gains or carried losses
  # if there are losses then the taxable gains are zero & vice-versa
  if ((((losses) - ( Epsilon)) > 0))
    return losses
  else # Taxable gains (may be zero)
    return short_gains + (1.0 - discount) * long_gains
}

# Balance the grossed up gains with underlying assets' cost bases
function gross_up_gains_aud(now, past, total_gains, long_gains, short_gains,
         a,
         extra_share, total_share,
         gains,
         extra_gains,
         fraction) {

  # No short gains by default
  short_gains = ((short_gains)?( short_gains):( 0))

  # Ensure there are gains
  if (!(((total_gains) - ( -Epsilon)) < 0))
    return 0

  # Neglect the component due to short gains
  fraction = long_gains / (long_gains + short_gains)

  # Compute the difference between the grossed up and net income long gains - short gains are disregarded
  extra_gains = ((CGT_Discount[2])?( (CGT_Discount[1]/CGT_Discount[2])):( assert((0), "Division by zero in rational fraction" CGT_Discount[1] "/" CGT_Discount[2]))) * fraction * total_gains / (1.0 - ((CGT_Discount[2])?( (CGT_Discount[1]/CGT_Discount[2])):( assert((0), "Division by zero in rational fraction" CGT_Discount[1] "/" CGT_Discount[2]))))

  # Track total share of extra gains remaining
  if (Start_Journal) {
    total_share = 1
    for (a in Leaf)
      if (select_class(a, "INCOME.GAINS.NET")) {
        # These are the income gains classes
        # Each account needs the income gains increased in proportion to its share of the total gains
        gains     = get_cost(a, now) - get_cost(a, past)

        # Skip negligible gains
        if (!(((gains) - ( -Epsilon)) < 0))
          continue

        # What share of the gains is this
        fraction = gains / long_gains

        # set new costs
        extra_share = fraction * extra_gains

        # Adjusting totals will allow swifter exit
        total_share -= fraction

        # Get underlying account and adjust its cost base
        assert(a in Underlying_Asset, "No underlying asset account to balance extra capital gains <" a ">")



        # Because this is a tax adjustment it will not impact the market gains
        adjust_cost(a,                       extra_share, now) # This is the extra taxable gain
        adjust_cost(Underlying_Asset[a],   - extra_share, now, (1)) # This is a tax adjustment because this is tax paid

        # Are we done?
        if (!(((total_share) - ( Epsilon)) > 0))
          break
      }
  }

  # Compute the difference between the grossed up and net income long gains
  long_gains += extra_gains



  # The grossed up long gains
  return long_gains
}


#
#
## Dividend Qualification Function
##
function dividend_qualification_aud(a, now, unqualified,

                                       underlying_asset,
                                       unqualified_account, imputation_credits) {

  # For Australia we need to adjust tax credits associated with an account
  #
  if (((((unqualified) - ( Epsilon)) <= 0) && (((unqualified) - ( -Epsilon)) >= 0)))
    # The payment was fully qualified
    return

  # Were there any tax credits anyway?
  if (a in Tax_Credits) {
    underlying_asset = Underlying_Asset[a]

    # Get the Imputation credits associated with this transaction - and only this transaction
    imputation_credits = (get_cost(Tax_Credits[a],  now) - get_cost(Tax_Credits[a], (( now) - 1)))
    if (!((((imputation_credits) - ( Epsilon)) <= 0) && (((imputation_credits) - ( -Epsilon)) >= 0))) {
      # Create an unqualified account
      unqualified_account = initialize_account("SPECIAL.FRANKING.OFFSET.UNQUALIFIED:U_TAX." Leaf[underlying_asset])

      # The adjustment
      unqualified *= imputation_credits


      # Now sum the unqualified credits in this account
      # This would occur when state files are used
      adjust_cost(unqualified_account, - unqualified, ((now) + 1))

      # Adjust the franking account too... (opposite sign - this is asset like)
      adjust_cost(FRANKING, unqualified, ((now) + 1))


    } # No credits at time now
  } # No tax credits for this account
} # All done


#
#
## Imputation Report Function
##
function imputation_report_aud(now, past, is_detailed,
                              reports_stream, more_past, label, x, offset_class) {
  # Set arguments
  more_past = ((past) - one_year(past, -1))
  is_detailed = ("" == is_detailed) ? 1 : 2

  # Show imputation report
  # The reports_stream is the pipe to write the schedule out to
  reports_stream = (("OTC" ~ /[iI]|[aA]/ && "OTC" !~ /[zZ]/)?( ((!Show_FY || ((now) == Show_FY))?( "/dev/stderr"):( "/dev/null"))):( "/dev/null"))

  # Let's go
  printf "%s\n", Journal_Title > reports_stream
  printf "Statement of Imputation Credits\n" > reports_stream

  printf "For the year ending %s\n", get_date(yesterday(now)) > reports_stream
  underline(81, 0, reports_stream)
  printf "%53s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC) > reports_stream
  printf "%53s %26s\n", "$", "$" > reports_stream

  # Franking Account Balance at Start of Period
  printf "Franking Account\n" > reports_stream
  printf "\t%24s%22s %26s\n\n", "Opening Balance",
            print_cash(get_cost(FRANKING, past)),
            print_cash(get_cost(FRANKING, more_past)) > reports_stream

  # Franking offsets
  offset_class = "SPECIAL.FRANKING.OFFSET"

  # If detailed print tax credits
  label = sprintf("Franking Offsets Received\n")
  label = print_account_class(reports_stream, label, "select_class", offset_class, "", "get_cost", now, past, past, more_past, is_detailed, -1)
  # Print a nice line
  if (!label) {
    ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
    x = get_cost("*" offset_class, past)
    printf "\t%24s%22s %26s\n\n", "Total Tax Offsets",
              print_cash(x - get_cost("*" offset_class, now)),
              print_cash(get_cost("*" offset_class, more_past) - x) > reports_stream
  }

  # Show the franking credits earned through tax payments
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  x = get_cost(FRANKING_STAMPED, past)
  printf "\t%24s%22s %26s\n\n", "Net Franked Tax Payments",
    print_cash(x - get_cost(FRANKING_STAMPED, now)),
    print_cash(get_cost(FRANKING_STAMPED, more_past) - x) > reports_stream

  # Franking Credits Disbursed
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  x = get_cost(FRANKING_PAID, past)
  printf "\t%24s%22s %26s\n\n", "Franking Credits Paid",
    print_cash(x - get_cost(FRANKING_PAID, now)),
    print_cash(get_cost(FRANKING_PAID, more_past) - x) > reports_stream

  # The balance
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))
  x = get_cost(FRANKING, past)
  printf "\t%24s%22s %26s\n\n", "Closing Balance",
    print_cash(get_cost(FRANKING, now)),
    print_cash(get_cost(FRANKING, past)) > reports_stream

  printf "\n\n\n" > reports_stream
}

#!/usr/local/bin/gawk -f
# p.smsf_modules.awk
# Copyright (C) 2019  Robert Whitehurst
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
# Use C include syntax for Header files




# Extra functions
# For Super Funds (Australian - SMSF - Journal Type "SMSF")
# This adjusts member balances to allow for profit/loss including
# deferred profit/loss calculated in the annual tax return
function balance_profits_smsf(now, past, initial_allocation,     delta_profits, x) {
  # Balance the books - including the reserve
  # Adjust member liability
  delta_profits = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - initial_allocation



  # Update the allocation
  if (!((((delta_profits) - ( Epsilon)) <= 0) && (((delta_profits) - ( -Epsilon)) >= 0)))
    # Update the Allocated Profits - this adds to changes made in print_tax_statement
    adjust_cost(ALLOCATED, delta_profits, now)

  # Also make adjustments to the reserve - use the updated Allocation
  x = get_cost(ALLOCATED, now) - get_cost(ALLOCATED, past)


  # Apply actual profits to the reserve
  x *= ((__MPX_KEY__ = find_key(Reserve_Rate,  now))?( Reserve_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Reserve_Rate[0]):( 0))))
  if ((((x) - ( Epsilon)) > 0)) {
    # Initialize reserve if needed
    if (!RESERVE)
      RESERVE   = initialize_account("LIABILITY.RESERVE:INVESTMENT.RESERVE")

    # The only reserve set in eofy actions so use now
    adjust_cost(RESERVE, -x, now)

  } else
    x = 0

  # By this point there are several adjustments required to
  # both redistribute liabilities and allocated profits
  delta_profits = get_cost(ALLOCATED, now) - initial_allocation - x
  if (!((((delta_profits) - ( Epsilon)) <= 0) && (((delta_profits) - ( -Epsilon)) >= 0)))
    update_member_liability_smsf(now, delta_profits, Member_Liability)

  # Unallocated expenses/income
  adjust_cost(ALLOCATED, (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now), now)

}

# This checks all is ok
function check_balance_smsf(now,        sum_assets, sum_liabilities, sum_adjustments, balance, show_balance, output_stream) {
  # The following should always be true (Equity is treated a special case of liability)
  # Assets - Liabilities = 0 (SMSFs have a simplified equation)
  # A complication exists if back payments are included so we have innstead
  # Assets - Liabilities = Future_Payments
  # This compares the cost paid - so it ignores the impact of revaluations and realized gains & losses
  sum_assets =  get_cost("*ASSET", now)

  # Work out the total assets etc
  sum_liabilities = get_cost("*LIABILITY", now)
  sum_adjustments = get_cost(ADJUSTMENTS, now) + get_cost(ALLOCATED, now) - (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now))

  # The balance should be zero
  # A super fund has only assets and liabilities since the income and expenses are attributed to members
  balance = sum_assets  + sum_liabilities + sum_adjustments


  # No default printing
  show_balance = (0)
  output_stream = "/dev/stderr"


  # Is there an error?
  if (!((((balance) - ( Epsilon)) <= 0) && (((balance) - ( -Epsilon)) >= 0))) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > output_stream
    show_balance = (1)
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > output_stream
    printf "\tAssets      => %20.2f\n", sum_assets > output_stream
    printf "\tLiabilities => %20.2f\n", sum_liabilities > output_stream
    if (((((sum_adjustments) - ( Epsilon)) > 0) || (((sum_adjustments) - ( -Epsilon)) < 0))) {
      printf "\tAdjustments => %20.2f\n", sum_adjustments > output_stream
      printf "\tIncome      => %20.2f\n",  get_cost("*INCOME", now) > output_stream
      printf "\tExpenses    => %20.2f\n", get_cost("*EXPENSE", now) > output_stream
    }
    printf "\tBalance     => %20.2f\n", balance > output_stream
    assert(((((balance) - ( Epsilon)) <= 0) && (((balance) - ( -Epsilon)) >= 0)), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}

# A wrapper function updates allocated profits when required ()
function update_profits_smsf(now,     delta_profits) {
  # Compute the profits that need to be allocated to members
  # These are the profits accumulated since the last time they were distributed to members
  delta_profits = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now)
  if (!((((delta_profits) - ( Epsilon)) <= 0) && (((delta_profits) - ( -Epsilon)) >= 0))) {

    # Update the Allocated Profits
    adjust_cost(ALLOCATED, delta_profits, now, (0))

    # Update the liabilities
    update_member_liability_smsf(now, delta_profits, Member_Liability)
  }
}

# Update a member liability
#
# How does this interact with liabilities - surely the value of pension liabilities
# should be taken into account when pro-rating; so need to revisit this
#
#  Stream/Pension Taxable/Tax-Free ratios are locked
#  Contributions other benefits go to and from the accumulation accounts
#
# This can be (i)   a contribution - specified member, taxable or tax-free
#          or (ii)  a benefit - specified member
#          or (iii) allocation amongst members - no specificiation
#          or (iv)  allocation to or from the reserve - no specification
# This function keeps the member liability up to date for a SMSF
#
function update_member_liability_smsf(now, amount, liability_array, a,

                                      share, taxable_share,
                                      member_id, member_account,
                                      target_account,
                                      sum_total, x, sum_share) {
  # Update the member liabilities with their share of the income/expenses
  # The proportions only change when a contribution is received
  # or a benefit paid;
  # plus there is no legislation specifying the precise method of proportioning
  # but this seems reasonable
  #   Income / Expenses are made in proportion to net contributions made
  #   Contributions are assigned to the member
  #   Benefits are paid proportionate to  the member's balance - so security prices influence this

  # In the various cases the following is passed in
  # Case (i)   :   account_name, now, amount
  # Case (ii)  :   account_name, now, amount
  # Case (iii) :   now, amount
  # Case (iv)  :   now, amount

  # Note if a taxable share is driven negative the value should be transferred
  # from the tax-free share - where else

  # Get the appropriate member account
  member_id = ((a)?( get_member_name(a, now, amount)):( ""))



  # Allocation to the liability accounts
  # Either no id is given - distribute amongst all accounts
  # Or a parent account - distribute amongst its offspring
  # Or a specific account - distribute solely to that account
  taxable_share = sum_total = sum_share = 0

  # Normalize amounts
  if (member_id in liability_array) { # Exact match - a contribution
    # Adjust the liability
    adjust_cost(member_id, - amount, now)
    if (member_id ~ /TAXABLE/)
      taxable_share = 1.0


  } else { # Get totals
    # We still get the share from each account
    # Don't use the accumulated totals because (rarely) a negative account balance will break the proportioning
    for (member_account in liability_array)
      if (!member_id || is_ancestor(member_id, member_account)) {
        share[member_account] = x = get_cost(member_account, now)
        sum_total += x

        # Compute what fraction of the allocation was taxable
        if (member_account ~ /TAXABLE/)
          taxable_share += x
      }

    # Normalize taxable share
    assert(((((sum_total) - ( Epsilon)) > 0) || (((sum_total) - ( -Epsilon)) < 0)), "update_member_liability: No liabilities to share")
    taxable_share /= sum_total

    # Update the liabilities - but only if account a is not a liability already
    if (!((a) ~ /^LIABILITY[.:]/)) {
      # There are two possibilities here -
      #   No member id => profit/loss everything goes to/from TAXABLE accounts
      #   A parent id  => proportioning rule applies
      for (member_account in share) {
        x = share[member_account] / sum_total

        # Target account
        if (!member_id)
          target_account = liability_array[member_account]
        else
          target_account = member_account

        # Adjust the liability
        adjust_cost(target_account, - x * amount, now)

      } # End of exact share
    }
  } # End of allocation

  # Tidy up
  delete share



  # return proportion that was taxable
  return taxable_share
}

# Obtain the member account
function get_member_name(a, now, x,   member_name, member_account, target_account, account_type, contribution_tax) {
  # This obtains the liability account that needs to be modified
  # In more detail INCOME.CONTRIBUTION.TYPE:NAME.X => LIABILITY.MEMBER.NAME:NAME.TYPE
  # And            EXPENSE.NON-DEDUCTIBLE.BENEFIT:NAME.TYPE => *LIABILITY.MEMBER.NAME (pro-rated if TYPE not specified)
  # And            LIABILITY.MEMBER.(STREAM|PENSION) => *LIABILITY.MEMBER.(STREAM|PENSION).NAME:SOME.NAME.TYPE (pro-rated if TYPE not specified)
  # In fact        X.Y:NAME.TYPE => *LIABILITY.MEMBER.NAME

  # Get the member name
  member_name = get_name_component(Leaf[a], 1) # first component

  # A member liability account can only be created by a contribution
  if (((a) ~ ("^" ( "INCOME.CONTRIBUTION") "[.:]"))) {
    # Identify the "account_type" (eg TAXABLE or TAX-FREE) - use Parent_Name because it is always available
    account_type = get_name_component(Parent_Name[a], 0) # last component

    # If a link is made in a "MEMBER" array to each members liabilities
    # then there is no need to identify this as a member liability in the
    # account name
    member_account = initialize_account(sprintf("LIABILITY.MEMBER.%s:%s.%s", member_name, member_name, account_type))

    # Ensure that this member is noted in the Member_Liability array
    if (!(member_account in Member_Liability)) {
      # Need to ensure that the target TAXABLE account is created
      # The target account can actually be the same as the member_account
      target_account = Member_Liability[member_account] = initialize_account(sprintf("LIABILITY.MEMBER.%s:%s.TAXABLE", member_name, member_name))

      # Check the target account is included too
      if (!(target_account in Member_Liability))
        Member_Liability[target_account] = target_account
    } else # Get the target account so we check if contribution tax should be computed
      target_account = Member_Liability[member_account]

    # This will change the LIABILITIES and EXPENSES equally
    if (target_account == member_account) {
      # This is a TAXABLE account
      contribution_tax = get_tax(now, Tax_Bands, x) # Always one band so ok to ignore other income

      # Save the tax expenses and adjust the liability
      adjust_cost(CONTRIBUTION_TAX, -contribution_tax, now)
      adjust_cost(target_account,  contribution_tax, now)
    }
  } else if (((a) ~ /^LIABILITY\.MEMBER\.PENSION[.:]/))
    member_account = "*LIABILITY.MEMBER.PENSION." member_name
  else if (((a) ~ /^LIABILITY\.MEMBER\.(PENSION|STREAM)[.:]/))
    member_account = "*LIABILITY.MEMBER.STREAM." member_name
  else {
    # Return the ancestral account
    member_account = "*LIABILITY.MEMBER." member_name
    assert(member_account in Parent_Name, "<" $0 "> Unknown account <" member_account ">")
  }

  # Return the account
  return member_account
}

# Process member Benefits
# Can use shortcut function names
function process_member_contributions_smsf(now, x, liability_array, a) {
  if (((a) ~ /^INCOME\.CONTRIBUTION[.:]/)) {
    # This will change proportions so update the profits first
    update_profits_smsf(now)

    # Fix up member liabilities
    update_member_liability_smsf(now, x, Member_Liability, a)
  }
}

# Pay out Member benefits into pension or other accounts
function process_member_benefits_smsf(now, array, amount,
           a, b,
           taxable_account, use_name,
           target_account, member_name,
           unrealized_gains,
           amount_taxed) {

  # Local accounts
  a = array[1]; b = array[2]

  # A complication for SMSF are transfers into a pension sub-account
  taxable_account = ""
  if (((a) ~ /^LIABILITY\.MEMBER\.(PENSION|STREAM)[.:]/)) {
    if (!((a) ~ ("[.:]" ( "TAXABLE") "(_|$)")) && !((a) ~ ("[.:]" ( "TAX-FREE") "(_|$)"))) {
      # Naming convention
      #
      # *:NAME.SUFFIX => *.NAME:NAME.SUFFIX.TAXABLE & *.NAME:NAME.SUFFIX.TAX-FREE
      #
      # Initialize accounts as needed
      member_name = get_name_component(Leaf[a], 1)
      use_name = sprintf("%s.%s:%s", substr(Parent_Name[a], 2), member_name, Leaf[a])
      taxable_account = initialize_account(sprintf("%s.TAXABLE", use_name))
      if (!(taxable_account in Pension_Liability)) {
        # Need to ensure target account is recorded too
        target_account = initialize_account(sprintf("LIABILITY.MEMBER.%s:%s.TAXABLE", member_name, member_name))
        Member_Liability[taxable_account] = Pension_Liability[taxable_account] = target_account
      } else
        target_account = Member_Liability[taxable_account]

      # Replace account a with tax-free account
      a = initialize_account(sprintf("%s.TAX-FREE", use_name))
      if (!(a in Pension_Liability))
        Member_Liability[a] = Pension_Liability[a] = target_account

      # These are Pension Liability Accounts
    } else if (((a) ~ ("[.:]" ( "TAXABLE") "(_|$)")))
      taxable_account = a
  }

  # A SMSF member benefit or pension pament
  if (((b) ~ /^EXPENSE\.NON\-DEDUCTIBLE\.BENEFIT[.:]/) || ((b) ~ /^LIABILITY\.MEMBER\.(PENSION|STREAM)[.:]/)) {

    # But there is another complication - this needs to consider
    # unrealized gains too => so important assets are priced accurately
    #
    # Save unrealized gains; notice that the asset class must be updated too for balancing
    unrealized_gains = get_asset_gains("get_unrealized_gains", now)

    # Get the change since previous transaction
    unrealized_gains -= get_cost(UNREALIZED, (find_key(Cost_Basis[UNREALIZED], (( ((now) - 1)) - 1))))

    # Adjust the market gains and the asset values
    adjust_cost("*ASSET", - unrealized_gains, now)
    adjust_cost(UNREALIZED, unrealized_gains, now)

    # This will change proportions so update the profits first
    update_profits_smsf(now)

    # Expense must be account b
    if (((b) ~ /^LIABILITY\.MEMBER\.(PENSION|STREAM)[.:]/))
      amount_taxed = amount * @Update_Member_Function(now, -amount, Pension_Liability, b)
    else
      amount_taxed = amount * @Update_Member_Function(now, -amount, Member_Liability, b)

    if (!((b) ~ ("[.:]" ( "TAXABLE") "(_|$)")) && !((b) ~ ("[.:]" ( "TAX-FREE") "(_|$)"))) {
      # Naming convention
      #
      # *:NAME.SUFFIX => *.NAME:NAME.SUFFIX.TAXABLE & *.NAME:NAME.SUFFIX.TAX-FREE
      #
      # Initialize accounts as needed
      use_name = sprintf("%s.%s:%s", substr(Parent_Name[b], 2), get_name_component(Leaf[b], 1), Leaf[b])

      # Adjust costs for taxable account
      #
      if (taxable_account)
        adjust_cost(taxable_account, -amount_taxed, now)
      else
        adjust_cost(a, -amount_taxed, now)

      # Finished with the credit taxable account
      b = initialize_account(sprintf("%s.TAXABLE", use_name))
      adjust_cost(b, amount_taxed, now)

      # Record this sub-transaction
      if (taxable_account)
        print_transaction(now, Comments, taxable_account, b, amount_taxed, Cost_Element)
      else
        print_transaction(now, Comments, a, b, amount_taxed, Cost_Element)

      # Replace account b with tax-free account
      b = initialize_account(sprintf("%s.TAX-FREE", use_name))

      # Adjust the amount for later processing
      amount -= amount_taxed
    }
  }

  ((array[1] =  a)?( array[2] =  b):( array[2] =  b))
  return amount
}



# We are nearly at Version 0.9 (Most Basic Functions)
#   This processes an mpx format file
#
#    Original style
#   2008 Apr 09, 604000, 776003,    200.000,    8382.00, BHP in specie transfer (200 units)
#   became over time
#   2008-Apr-09, CASH, BHP.ASX, 8382.00, 200, # (in specie transfer in fact)
#
# To Do =>
#   ***in progress
#
#   Accumulated profits should not include unrealized losses/gains which are classified as capital
#
#   ***Fix up wiki files
#   other tax_statement calculations (eg UK, US, NZ etc...)
#
#   Allow non-rectangular arrays, i.e. only have [a][p][e] for active elements
#   Allow other currencies or commodities (eg USD, AU, AG, etc)
#   Read single entry transactions
#   special notes for -l version
#   describe gpp properly
#   Short day-by-day performance summary (cost-value-etc.. estimated for EOFY)
#
#   Integrate with graphing/analysis package (eris)
#
BEGIN {
  # Initialize
  Start_Journal = (0)

  # An array to hold real values
  ((SUBSEP in Real_Value)?((1)):((0)))
  ((SUBSEP in account_sum)?((1)):((0)))

  # An array to hold document strings
  ((SUBSEP in Documents)?((1)):((0)))
  ((SUBSEP in Time_Fields)?((1)):((0)))

  # A Document shortcut code
  Document_Shortcut = "[:+]"

  # And a gains stack
  ((SUBSEP in Gains_Stack)?((1)):((0)))
  Long_Gains_Key   = "Long Gains  "
  Long_Losses_Key  = "Long Losses "
  Short_Gains_Key  = "Short Gains "
  Short_Losses_Key = "Short Losses"

  # Sort arrays this way...
  Array_Sort = sort_arrays_on("@ind_num_desc")
  Variable_Name = ""
  Variable_Keys[0] = ""

  # Output Fields
  if ("" != Use_Separator)
    OFS = Use_Separator

  #
  if ("" == DATE_FORMAT)
    DATE_FORMAT = ("%Y %b %d") 
  LONG_FORMAT = (DATE_FORMAT " %H::%M::%S")

  # Import Record is Off
  Import_Record = (0)
  Filter_Data = ""

  # Suppress rounding errors
  Epsilon = 0.0001

  # The Time Zone
  Time_Zone = 10
  UTC = 1

  # The Epoch
  if ("" == Epoch)
    set_epoch()

  # The allowed cost elements
  I   = "I"
  II  = "II"
  III = "III"
  IV  = "IV"
  V   = "V"
  Elements[I]   =   I
  Elements[II]  =  II
  Elements[III] = III
  Elements[IV]  =  IV
  Elements[V]   =   V

  # url encoding lookup table
  url_init()

  # Initialize arrays
  ((SUBSEP in Account_Closed)?((1)):((0)))
  ((SUBSEP in Account_Term)?((1)):((0)))
  ((SUBSEP in Accounting_Cost)?((1)):((0)))
  ((SUBSEP in Carry_Offsets)?((1)):((0)))
  ((SUBSEP in Cost_Basis)?((1)):((0)))
  ((SUBSEP in Dividend_Date)?((1)):((0)))
  ((SUBSEP in Foreign_Offset_Limit)?((1)):((0)))
  ((SUBSEP in Held_From)?((1)):((0)))
  ((SUBSEP in Held_Until)?((1)):((0)))
  ((SUBSEP in Income_Tax)?((1)):((0)))
  ((SUBSEP in Leaf)?((1)):((0)))
  ((SUBSEP in Lifetime)?((1)):((0)))
  ((SUBSEP in Long_Gains)?((1)):((0)))
  ((SUBSEP in Long_Losses)?((1)):((0)))
  ((SUBSEP in Long_Name)?((1)):((0)))
  ((SUBSEP in Maturity_Date)?((1)):((0)))
  ((SUBSEP in Method_Name)?((1)):((0)))
  ((SUBSEP in No_Carry_Offsets)?((1)):((0)))
  ((SUBSEP in Number_Parcels)?((1)):((0)))
  ((SUBSEP in Parcel_Proceeds)?((1)):((0)))
  ((SUBSEP in Parcel_Tag)?((1)):((0)))
  ((SUBSEP in Parent_Name)?((1)):((0)))
  ((SUBSEP in Price)?((1)):((0)))
  ((SUBSEP in Qualified_Units)?((1)):((0)))
  ((SUBSEP in Refundable_Offsets)?((1)):((0)))
  ((SUBSEP in Short_Gains)?((1)):((0)))
  ((SUBSEP in Short_Losses)?((1)):((0)))
  ((SUBSEP in Tax_Adjustments)?((1)):((0)))
  ((SUBSEP in Tax_Bands)?((1)):((0)))
  ((SUBSEP in Tax_Credits)?((1)):((0)))
  ((SUBSEP in Taxable_Income)?((1)):((0)))
  ((SUBSEP in Total_Units)?((1)):((0)))
  ((SUBSEP in Underlying_Asset)?((1)):((0)))
  ((SUBSEP in Units_Held)?((1)):((0)))

  # Carried Loss Arrays
  # Capital Losses
  Capital_Losses[0][SUBSEP] = 0; delete Capital_Losses[0][SUBSEP]
  Tax_Losses[0][SUBSEP] = 0; delete Tax_Losses[0][SUBSEP]

  # This is a CSV file
  read_csv_records(Use_CSV)

  # Transaction line defaults
  new_line()

  # Default Portfolio Name and document URI
  Journal_Title = "NEMO"
  Journal_Type = "IND"
  Document_Protocol = "https://"
  Document_Root = "example.com/NEMO/"

  # Default currency
  Journal_Currency = "AUD"

  # Importing CSV files
  # Default Price Record Class
  Asset_Prefix = ("ASSET.CAPITAL.SHARES")

  # A short name
  Asset_Symbol = ""

  # Default import fields
  Key_Field      = 1
  Value_Field    = 2
  Key_is_Date    = (1)
  Value_is_Date  = (0)
  Value_is_XRate = (0)
  Import_Zero  = (0)
  Import_Time  = (12)

  # Default Import Settings
  split((""), Time_Fields, " ")

  # Set special accounts
  set_special_accounts()

  # Copyleft?
  if ("" != version) {
    print_copyleft()
    exit
  }

  # Set time format
  set_months()

  # Show detailed summary
  if (!Show_Extra)
    Show_Extra = 0

  # stop time
  if (!Stop_Time)
    Stop_Time = Future
  else {
    Stop_Time = read_date(Stop_Time)
    assert((-1) < Stop_Time, "Stop_Time " Read_Date_Error)
  }

  # Which account to track
  if ("" == Show_Account)
    Show_Account = (0)

  # Last time is the most recent earlier timestamp
  # Initially set the last time to be -1
  Last_Record = - 1

  # The last recorded timestamp in a state file
  # is Last_State - also initialized to -1
  Last_State = - 1
  Last_FY = FY_Time = -1
  FY_Year = -1 # Not set yet
} # End of BEGIN block

# All records
{  # Screen for MSDOS line feeds
  sub(/\r$/, "")
}

# Comment record
/^([[:space:]])*#/  {
  next
}

# State Record
##
## Read the listed fields from a delimited text file
## For example
## << Time_Fields  4 9 >>
##
## would result in each record yielding
## An_Array[$1][read_date($4)][$7] => read_date($9)
##
##
# This reads an array from human readable data
# Program state can be given using a state pattern range
#
/^<</,/>>$/ {
  # Set =
  read_state_record($0 ~ /^<</, $0 ~ />>/)
}

function read_state_record(first_line, last_line) {
  # We need to establish if this is an array or a scalar
  if (first_line) {
    assert(NF > 1, "Syntax error <" $0 "> state record needs a variable name")
    Variable_Name = ($2)
    assert(Variable_Name in SYMTAB, "<" Variable_Name "> is not declared")
    delete Variable_Keys

    # Special case if  this of the form << Array_Name >> then clear the array
    first_line = 2
    if (last_line && (first_line > NF - 2) && isarray(SYMTAB[Variable_Name])) {
      clear_array(SYMTAB[Variable_Name])

      next
    } else if (NF > 2 && "+=" == $3) {
      Adjust_Value = 1
      first_line = 3
    } else if (NF > 2 && "-=" == $3) {
      Adjust_Value = -1
      first_line = 3
    } else
      Adjust_Value = 0
  } else
    first_line = 0

  # Syntax is
  # << Variable_Name Scalar Value >> or
  # << Variable_Name Vector Key-1 Key-2 .... Key-M Value >> or
  # << Variable_Name Vector
  #    Key-1-1 Key-1-2 ... Key-1-M Value-1
  #    ......
  #    Key-N-1 Key-N-2 ... Key-N-M Value-N >>
  read_state(Variable_Name, Adjust_Value, first_line + 1, NF - last_line)

  # Get the next record
  next
}

# Import Record
# This imports an array
# Array[Key] => Value
# Need the following variables
# array_name
# key_field
# value_field
#
##
/^%%/  {
  #
  Import_Record = !Import_Record
  if (Import_Record) {
    # Filter data
    # Currently importing Import_Array
    if (!index(Filter_Data, Import_Array_Name))
      # Make sure this array will be filtered
      if ("" != Filter_Data)
        Filter_Data = Filter_Data "," Import_Array_Name
      else
        Filter_Data = Import_Array_Name



    # Prices are given a special import time
    if ("Price" == Import_Array_Name) {
      Import_Time = (16)
      Import_Zero = (0)
    } else
      Import_Time = (12)
  } else {
    # End of block
    # Reset asset default prefix
    Asset_Prefix = ("ASSET.CAPITAL.SHARES")
  }

  # End of if importing
  next
}

1 == Import_Record {
  # identify the array or scalar being imported
  #
  import_data(SYMTAB[Import_Array_Name], Asset_Prefix ":" Asset_Symbol, Import_Array_Name)
  next
}


# Start Record
# Syntax is [Date] START_JOURNAL
/START_JOURNAL/ {

  # Allow multiple calls
  if (Start_Journal)
    next
  else if (NF > 1)
    # Interpret the date at midnight
    Start_Record = read_date($1, 0)
  else
    assert((0), "START_JOURNAL, Date: Date is required on first call of start journal")

  # Check Start_Record
  assert(Start_Record > (-1), Read_Date_Error)

  # Set FY information if required
  if (-1 == Last_State)
    set_financial_year(Start_Record)

  # Ensure agreement of Last_Record with Last_State
  if (Last_Record < Last_State)
    Last_Record = Last_State

  # Is the currency consistent
  assert("AUD" == Journal_Currency, "Incompatible journal currency <" Journal_Currency "> in journal file - expected <" "AUD" "> instead")

  # Set default functions
  Income_Tax_Function     = "income_tax_" tolower(Journal_Currency)
  Initialize_Tax_Function = "initialize_tax_" tolower(Journal_Currency)
  Dividend_Qualification_Function = "dividend_qualification_" tolower(Journal_Currency)
  Imputation_Report_Function      = "imputation_report_" tolower(Journal_Currency)
  Gross_Up_Gains_Function   = "gross_up_gains_" tolower(Journal_Currency)
  Get_Taxable_Gains_Function   = "get_taxable_gains_" tolower(Journal_Currency)

  # Set translation rate for journal currency
  initialize_account("ASSET.CURRENT.CURRENCY:" Journal_Currency)
  (Price[Long_Name[Journal_Currency]][ Epoch] = ( 1.0))


  # These functions are not dependent on currency
  Balance_Profits_Function     = "balance_journal"
  Check_Balance_Function       = "check_balance"
  Process_Member_Benefits      = "process_member_benefits"
  Process_Member_Contributions = "process_member_contributions"
  Update_Profits_Function      = "update_profits"
  Update_Member_Function       = "update_member_liability"

  # Dividend Qualification Window - is recorded
  # with the help of the auxilliary variable
  # which sets an EOFY_Window
  if ("" == EOFY_Window) {
    # Dividend Qualification Window is not yet set
    if ("" == Qualification_Window)
      Qualification_Window = (91) # Units in days

    # Qualification_Window is set, but since EOFY_Window is unset the units are days
    Qualification_Window *= (86400)

    # Still need to set EOFY_Window
    if (Qualification_Window > (86400))
      EOFY_Window = 0.5 * (Qualification_Window - (86400))
    else
      EOFY_Window = 0
  } else
    # Qualification_Window must be set
    assert("" != Qualification_Window, "Qualification Window is undefined even though EOFY_Window <" EOFY_Window "> is set")

  # Set the URI document prefix
  Document_URI = Document_Protocol url_encode(Document_Root)

  # Initialize local tax variables
  @Initialize_Tax_Function()

  #
  # Initialize state file information
  initialize_state()
  Last_FY = ((FY_Time) - one_year(FY_Time, -1))

  # Next set up FY Information
  # Show a particular FY
  if (!Show_FY)
    Show_FY = 0
  else {
    # Use a timestamp
    Show_FY = read_date(Show_FY "-" FY_Date, 0)
    if (Show_FY > Start_Record && Show_FY <= Last_State)
      eofy_actions(Show_FY)
  }

  # Flag this
  Start_Journal = (1)

  # All done
  next
}

# Default record
{
  # Skip empty lines
  if ("" == $0)
    next

 # Use a function so we can control scope of variables
 read_input_record()
 next
}





## Import Prices
## The fields
## These are the those needed for the Price import from the CBA (2019 format)
# <<,  Key_Field, 1, >>
# <<, Value_Field, 5, >>
#
# # The key field is a date
# <<, Key_is_Date, 1,>>
# <<,Value_is_Date ,  0,>>
#
# # These are price data
# <<,Import_Array_Name , Price ,>>
#
# #


# This asset is AAA.ASX
## <<,Asset_Prefix, ASSET.CAPITAL.SHARES,>>
## <<,Asset_Symbol, AAA.ASX,>>
##
##
# This reads an array from human readable data
function import_data(array, symbol, name,
                         a, key, value) {


  # Check syntax
  assert(Key_Field <= NF && Value_Field <= NF, "Illegal import record syntax <" $0 ">")

  # Ok
  a = initialize_account(symbol)

  # Get the key
  if (Key_is_Date) {
    key = read_date(($Key_Field)) # Default Hour overruled sometimes
    assert((-1) != key, Read_Date_Error)

    # Skip dates before the epoch
    if ((-2) == key)
      return
  } else
    key = ($Key_Field)

  # Get the value
  if (Value_is_Date) {
    value = read_date(($Value_Field))
    assert((-1) != value, Read_Date_Error)

    # Skip dates before the epoch
    if ((-2) == value)
      return
  } else {
    value = ($Value_Field)
    if (!Import_Zero && ((((value) - ( Epsilon)) <= 0) && (((value) - ( -Epsilon)) >= 0)))
      return # Don't import zero values

    # Exchange rates are reciprocal prices
    if (Value_is_XRate)
      value = 1.0 / value
  }

  # Logging


  # Set the price
  (array[a][ key] = ( value))

  # Done
}


function set_financial_year(now,   new_fy) {
  # Which Calendar year is this?
  FY_Year = (strftime("%Y", (now), UTC) + 0)

  # The financial year
  Last_FY = now
  assert(now > ((FY_Time) - 1), "Cannot regress financial year: Current FY => " get_date(FY_Time) " New FY => " get_date(now))
  FY_Time = ((now) + one_year(now,  1))

  # Get the FY_Date - just the date, no year
  FY_Date = get_date(now, "%b-%d")

  # Get the day number for the FY_Date
  FY_Day = (strftime("%j", (FY_Time), UTC) + 0)

  # Feb 28 has day number 59 so if FY_Day <= 60 - (1st day next FY)
  # then the current FY would include leap day if (FY - 1) was a leap year
  FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)
}


## Functions start here
# Initialize read/write of state files
function initialize_state(    x) {
  # Get which variables to write out
  ((SUBSEP in Array_Names)?((1)):((0)))
  ((SUBSEP in Scalar_Names)?((1)):((0)))

  # Current Version
  MPX_Version = Current_Version = "Version " string_hash(("Account_Closed Account_Term Accounting_Cost Capital_Losses Carry_Offsets Cost_Basis Dividend_Date Foreign_Offset_Limit Held_From Held_Until Income_Tax Leaf Lifetime Long_Gains Long_Losses Long_Name Maturity_Date Method_Name No_Carry_Offsets Number_Parcels Parcel_Proceeds Parcel_Tag Parent_Name Price Qualified_Units Refundable_Offsets Short_Gains Short_Losses Tax_Adjustments Tax_Bands Tax_Credits Tax_Losses Taxable_Income Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount Franking_Deficit_Offsets GST_Rate LIC_Allowance LIC_Deduction Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Pension_Liability Reserve_Rate ") ("MPX_Version MPX_Arrays MPX_Scalars Document_Protocol Document_Root Enforce_Names Enforce_Qualification EOFY_Window FY_Day FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window Start_Record ALLOCATED Dividend_Qualification_Function Get_Taxable_Gains_Function Gross_Up_Gains_Function Imputation_Report_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function "))
  if ("" != Write_Variables) {
    # This time we just use the requested variables
    split(Write_Variables, Array_Names, ",")
    for (x in Array_Names)
      # Ensure the requested variable name is allowable - it could be an array or a scalar
      if (!index(("Account_Closed Account_Term Accounting_Cost Capital_Losses Carry_Offsets Cost_Basis Dividend_Date Foreign_Offset_Limit Held_From Held_Until Income_Tax Leaf Lifetime Long_Gains Long_Losses Long_Name Maturity_Date Method_Name No_Carry_Offsets Number_Parcels Parcel_Proceeds Parcel_Tag Parent_Name Price Qualified_Units Refundable_Offsets Short_Gains Short_Losses Tax_Adjustments Tax_Bands Tax_Credits Tax_Losses Taxable_Income Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount Franking_Deficit_Offsets GST_Rate LIC_Allowance LIC_Deduction Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Pension_Liability Reserve_Rate "), Array_Names[x])) {
        assert(index(("MPX_Version MPX_Arrays MPX_Scalars Document_Protocol Document_Root Enforce_Names Enforce_Qualification EOFY_Window FY_Day FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window Start_Record ALLOCATED Dividend_Qualification_Function Get_Taxable_Gains_Function Gross_Up_Gains_Function Imputation_Report_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function "), Array_Names[x]), "Unknown Variable <" Array_Names[x] ">")

        # This is a scalar
        Scalar_Names[x] = Array_Names[x]
        delete Array_Names[x]
      }
  } else {
    # Use default read and write list
    Write_Variables = (0)
    MPX_Arrays = ("Account_Closed Account_Term Accounting_Cost Capital_Losses Carry_Offsets Cost_Basis Dividend_Date Foreign_Offset_Limit Held_From Held_Until Income_Tax Leaf Lifetime Long_Gains Long_Losses Long_Name Maturity_Date Method_Name No_Carry_Offsets Number_Parcels Parcel_Proceeds Parcel_Tag Parent_Name Price Qualified_Units Refundable_Offsets Short_Gains Short_Losses Tax_Adjustments Tax_Bands Tax_Credits Tax_Losses Taxable_Income Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount Franking_Deficit_Offsets GST_Rate LIC_Allowance LIC_Deduction Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Pension_Liability Reserve_Rate ")
    MPX_Scalars = ("MPX_Version MPX_Arrays MPX_Scalars Document_Protocol Document_Root Enforce_Names Enforce_Qualification EOFY_Window FY_Day FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window Start_Record ALLOCATED Dividend_Qualification_Function Get_Taxable_Gains_Function Gross_Up_Gains_Function Imputation_Report_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function ")

    split(MPX_Arrays, Array_Names, " ")
    split(MPX_Scalars, Scalar_Names, " ")
  }
}


# A function to set banded arrays (like tax bands)
function set_array_bands(now, bands, nf,     i, k) {

  # Negative threshold is required to force correct ordering
  for (i = 4; i < nf; i += 2) {
    k = strtonum($(i+1))
    bands[now][-k] = strtonum($i)

  }

  # Need to deal with case
  # %%,DATE,<ACTION>,15
  # i.e $(i+1) does not exist
  if (4 == nf) {
    bands[now][0] = strtonum($nf)

  }
}

function read_input_record(   t, n, a, threshold) {
  # Must have started journal
  assert(Start_Journal, "No START_JOURNAL record found")

  # Optional values
  new_line()

  # Interpret the date
  t = read_date($1)

  # t should be positive
  assert(t > (-1), Read_Date_Error)

  # If this is a new time check if it is a new FY
  if (t > Last_Record) {
    # Are we done?
    if (t > Stop_Time + EOFY_Window)
      # this record will not be processed so
      # don't update Last_Record
      exit

    # Update the Last_Record
    Last_Record = t

    # EOFY actions
    while (FY_Time + EOFY_Window < t) {
      # Get each EOFY in turn
      eofy_actions(FY_Time)

      # The next financial year - unless we are stopping
      FY_Year ++

      # Update FY length
      FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)
      Last_FY = FY_Time
      FY_Time = ((FY_Time) + one_year(FY_Time,  1))
    }
  } else # Check for semi-monotonicity
    assert(t == Last_Record, sprintf("Current entry %s is earlier than the previously recorded transaction %s", $0, get_date(Last_Record)))

  # Parse the line
  # First filter out any control actions (ADJUST|CHANGE|CHECK|MERGE|SET|SHOW|SPLIT)
  if (control_action(t))
    return

  # Returns number of accounts
  n = parse_line(t)

  # There are n accounts in each transaction
  # Currently flag a single entry transaction as an error
  assert(2 == n || 0 == n, sprintf("<%s> - syntax error %d accounts found in transaction", $0, n))

  # If the transaction is already parsed simply print it out
  if (t < ((Last_State) + 1)) {
    if (2 == n)
      # Truncated line
      print_transaction(t, Comments " <**STATE**>", Account[1], Account[2], amount)
  } else if (2 == n) {
    parse_transaction(t, Account[1], Account[2], amount)
  } else # A zero entry line - a null transaction or a comment in the journal
    print_transaction(t, Comments)

  # Were totals changed by this transaction?
  @Check_Balance_Function(t)

  # Clean up the Account array
  delete Account
}

## Apply a account control action
function control_action(now,    i, is_check, x) {
  #
  # Trim the fields
  for (i = 2; i <= NF; i ++)
    # Need to remove white space from the fields
    $i = ($i)

  # Is this a control action?
  # (ADJUST|CHANGE|CHECK|MERGE|SET|SHOW|SPLIT)
  x = $2

  # First deal with the case when X needs adjustment
  is_check = 2
  switch (x) {
    case "CHANGE"  :
      $5 = 1
      break
    case "MERGE"  : # Recirpocal of split
      $5 = (($5)?( 1.0 / $5):( 1))
      break
    case "SPLIT"  :
      $5 = (($5)?( $5):( 1))
      break


    # Checkset actions
    case "CHECK" :
      is_check = 1
      break
    case "SHOW" :
      is_check = -1
      break
    case "SET" :
    case "ADJUST" :
      is_check = 0
      break
    default  :
      return (0)
  }

  # Syntax for CONTROL is
  # DATE CONTROL A B [X] [# Comment]
  # We need to rebuild the line into something more standard
  # DATE A B X [# Comment]
  if (NF < 5) {
    $5 = 0
    NF = 5
  }
  for (i = 2; i < NF; i ++)
    $i = $(i + 1)
  NF --

  # We can parse this line now
  i = parse_line(now)
  assert(2 == i, $0 " : Unknown syntax for CONTROL action <" x ">")

  # Is this a CHANGE|MERGE|SPLIT action?
  if (2 == is_check)
    split_account(now, Account[1], Account[2], amount)
  else
    checkset(now, Account[1], Account[2], Real_Value[(0)], amount, is_check)

  # Get the next record
  return (1)
}

# Break out transaction parsing into a separate module
function parse_transaction(now, a, b, amount,
                           units,
                           underlying_asset, credit_account,
                           swop, g,
                           n, account_array,
                           bought_parcel,
                           current_brokerage, gst,
                           correct_order, tax_credits,
                           other_currency,
                           fields, number_fields) {

  # No swop
  swop = ""

  # Start at zero
  number_fields = 0

  # a list of output fields is needed
  ((SUBSEP in fields)?((1)):((0)))

  # Just process the amount at this point - later consider other values (especially brokerage!)
  if (Transaction_Currency)
    amount *= Translation_Rate

  # Special franking provisions
  if (a == TAX) {
    # Reduce franking
    adjust_cost(FRANKING, - amount, now)

    # Note this as a reduction in the balance
    adjust_cost(FRANKING_STAMPED, amount, now)

    print_transaction(now, "Reduce Franking Balance", FRANKING, FRANKING_STAMPED, amount)
   } else if (b != GST && ((b) ~ /^(ASSET\.CURRENT|LIABILITY)\.TAX[.:]/)) {
    # Increase franking
    adjust_cost(FRANKING, amount, now)

    # Note this as an increase in the balance
    adjust_cost(FRANKING_STAMPED, -amount, now)

    print_transaction(now, "Increase Franking Balance", FRANKING_STAMPED, FRANKING, amount)
  }

  # For a SMSF process member benefits
  if ((Journal_Type ~ /^SMSF$/)) {
    ((account_array[1] =  a)?( account_array[2] =  b):( account_array[2] =  b))
    amount = @Process_Member_Benefits(now, account_array, amount)
    a = account_array[1]; b = account_array[2]
    delete account_array
  }

  # Assets or Liabilities with a fixed term need a maturity date
  # Set the maturity date and account term
  # Assume that a single transaction can't establish two term limited accounts
  # since in general the terms will be different
  if (((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/)) {
    assert(!((b) ~ /^(ASSET|LIABILITY)\.TERM[.:]/),
           sprintf("Transactions between two fixed term accounts %s & %s are not supported due to ambiguity of timestamps",
                   a, b))
    # Set the term
    set_account_term(a, now)
  } else if (((b) ~ /^(ASSET|LIABILITY)\.TERM[.:]/))
    set_account_term(b, now)

  # Initially no optional fields
  number_fields = 0

  # Is this a franked transaction?
  if (((a) ~ ("^" ( "INCOME") "[.:]"))) {
    # Income accounts are caught to check for franking credits
    # Income must always be account a
    tax_credits = Translation_Rate * Real_Value[(1)]
    Real_Value[(1)] = 0

    # Get the underlying asset (if there is one)
    if (a in Underlying_Asset)
      underlying_asset = Underlying_Asset[a]
    else
      underlying_asset = (0)

    # Foreign or franking credits
    if (((((tax_credits) - ( Epsilon)) > 0) || (((tax_credits) - ( -Epsilon)) < 0))) {
      # Keep an account of tax credits
      # We need the underlying asset to
      assert(underlying_asset, sprintf("Income account %s must have an underlying asset to receive tax credits", Leaf[a]))
      if (a in Tax_Credits)
        credit_account = Tax_Credits[a]
      else {
        # Create tax credits account - just in time
        # Type of credits account depends on the underlying asset
        # INCOME.DIVIDEND     => SPECIAL.FRANKING.OFFSET
        # INCOME.DISTRIBUTION => SPECIAL.FRANKING.OFFSET
        # INCOME.FOREIGN      => SPECIAL.FOREIGN.OFFSET
        #
        if (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION") "[.:]")))
          credit_account = Tax_Credits[a] = initialize_account("SPECIAL.FRANKING.OFFSET:I_TAX." Leaf[underlying_asset])
        else if (((a) ~ ("^" ( "INCOME.FOREIGN") "[.:]")))
          credit_account = Tax_Credits[a] = initialize_account("SPECIAL.FOREIGN.OFFSET:C_TAX." Leaf[underlying_asset])
        else
          assert((0), sprintf("Can't link a tax credit account to income account %s", a))
      }

      # Adjust credit account
      adjust_cost(credit_account, - tax_credits, now)

      # Adjust franking account when necessary
      if (((a) ~ ("^" ( "INCOME.FOREIGN") "[.:]")))
        # Foreign Credits - adjust top level
        adjust_cost("*SPECIAL", tax_credits, now)
      else
        # Franking Credits
        adjust_cost(FRANKING, tax_credits, now)

      # Record the tax credits to the listing
      fields[++ number_fields] = tax_credits

    } else
      tax_credits = 0

    # Now LIC deduction if any
    if (((((Real_Value[(2)]) - ( Epsilon)) > 0) || (((Real_Value[(2)]) - ( -Epsilon)) < 0))) {
      # Record LIC Deduction to the listing
      sum_entry(LIC_Deduction, - Translation_Rate * Real_Value[(2)], now)
      fields[++ number_fields] = Translation_Rate * Real_Value[(2)]
    }

    # Now check for a timestamp - this is the ex-dividend date if present
    if (underlying_asset) {
      if (Extra_Timestamp > Epoch) {
        # Save this for reporting
        fields[++ number_fields] = get_date(Extra_Timestamp)

        # Assert that the ex-dividend date must not be later than the payment date
        assert(Extra_Timestamp <= now, "The ex-dividend date <" fields[1] "> must be before the payment date <" get_date(now) ">")

        # Save the ex-dividend date
        # But note that it must relate to an underlying asset
        #
        (Dividend_Date[underlying_asset][ Extra_Timestamp] = ( now))

      } else if (Qualification_Window && (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION.CLOSE") "[.:]")))) {
        Extra_Timestamp = get_exdividend_date(underlying_asset, now)

        # A date error is an error
        assert((-1) != Extra_Timestamp, "<" Leaf[a] "> Cannot find ex-dividend date for payment date <" get_date(now) ">. Use option -q to override.")

        if (!Extra_Timestamp)
          printf "Warning: No exdividend information for %s\n", Leaf[a] > "/dev/stderr"
        else
          fields[++ number_fields] = get_date(Extra_Timestamp)
      }

      # Clear the timestamp
      Extra_Timestamp = (-1)
    }

    # Now check for GST
    if (((((GST_Claimable) - ( Epsilon)) > 0) || (((GST_Claimable) - ( -Epsilon)) < 0))) {
      # This is GST collected
      # The transaction itself will be posted later a => b
      # Need to adjust amount transacted
      amount -= (g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount)
      print_transaction(now, ("# GST " Leaf[b]), GST, b, g)

      # GST claimed
      GST_Claimable = 0
    }

    # A SMSF member contribution
    @Process_Member_Contributions(now, amount, Member_Liability, a)
  } else if (((b) ~ ("^" ( "EXPENSE.NON-DEDUCTIBLE.DIVIDEND") "[.:]"))) {
    # A franking entity (eg company) can distribute franking credits
    tax_credits = Real_Value[(1)]
    Real_Value[(1)] = 0

    # Simplified version of above
    if (((((tax_credits) - ( Epsilon)) > 0) || (((tax_credits) - ( -Epsilon)) < 0))) {
      # The credits are adjusted in the FRANKING balance
      adjust_cost(FRANKING,    - tax_credits, now)
      adjust_cost(FRANKING_PAID,  tax_credits, now)

      print_transaction(now, ("# " Leaf[a] " Franking Credits Distributed"), FRANKING, FRANKING_PAID, tax_credits)
      fields[++ number_fields] = tax_credits

    } else
      tax_credits = 0
  }

  # Obtain units
  units = Real_Value[(0)]
  Real_Value[(0)] = 0

  # A sale transaction
  if (units < 0) {
    # The asset being sold must be "a" but if equity must be "b"
    correct_order = (((( a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/) && is_open( a, now)) || ((( b) ~ /^EQUITY[.:]/) && is_open( b, now)))
    assert(correct_order, sprintf("%s => can't sell either %s or %s\n", $0, (Leaf[a]), (Leaf[b])))

    # If this is not an asset sale swop the accounts
    if (!((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/) || (!is_open((a), ( now)))) {
      swop = a; a = b; b = swop
      amount = - amount
    }

    # Get brokerage (if any)
    current_brokerage = Translation_Rate * Real_Value[(1)]
    Real_Value[(1)] = 0

    # Amount should be the consideration as recorded by the broker
    #
    # Impact of GST
    if (((((GST_Claimable) - ( Epsilon)) > 0) || (((GST_Claimable) - ( -Epsilon)) < 0))) {
      # Two cases
      #   Not Present => Adjust Whole Amount
      if (((((current_brokerage) - ( Epsilon)) <= 0) && (((current_brokerage) - ( -Epsilon)) >= 0))) {
        # No Brokerage
        # A sale
        # A, B, -U,  (1 - g) * x
        # G, B,  0,        g * x
        g = - GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount

        # This must be recorded
        # This reduces GST liability
        print_transaction(now, ("# GST " Leaf[a]), GST, b, -g)
        assert((0), "GST Was levied on whole SELL transaction <" $0 ">")
      } else {
        # Brokerage Present => Adjust Brokerage
        # We Have A, B, -U, x - b, g
        # Produce A, B, -U, x - (1 - g) * x, # Note sign change with other case
        #         B, G,  0,           g * x, # Sign change engenders sense change in accounts
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * current_brokerage

        # This must be recorded
        print_transaction(now, ("# GST " Leaf[a]), b, GST, g)
      }

      # Non-zero GST to be paid
      adjust_cost(GST, g, now)
      GST_Claimable = 0
    } else
      g = 0

    # Sell units -> cash is flowing out of a
    # Ensure positive values for units and amount are passed
    # Brokerage is treated as a cost so it can be applied to the units actually sold
    sell_units(now, a, -units, amount + g, Parcel_Name, Extra_Timestamp)

    # And simply adjust settlement account b by the
    if (Real_Value[(-1)] > 0) {
      # This is a forex account - extra arguments fo not apply to complementary account
      buy_units(now, b, Real_Value[(-1)], amount)

      # Update parent entries for <b>
      update_cost(b, amount, now)
      Real_Value[(-1)] = 0
    } else # Normal account
      adjust_cost(b, amount, now)

    # Buy units in asset (b) - this ignores impact of brokerage
    bought_parcel = buy_units(now, b, units, amount - current_brokerage, Parcel_Name, Extra_Timestamp)

    # Adjust the cost of this **parcel** for the impact of brokerage and GST
    adjust_parcel_cost(b, bought_parcel, now, current_brokerage - g,  II, (0))

    # Did we swop? If so swop back
    if (b == swop) {
      b = a; a = swop
      amount = - amount
    }

    # Record the transaction
    if (((((current_brokerage) - ( Epsilon)) > 0) || (((current_brokerage) - ( -Epsilon)) < 0)))
      fields[++ number_fields] = sprintf("%.*f", (2), current_brokerage - g) # Always use 1st field

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if ((-1) != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Normally next field is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # The number of fields is not fixed
    # A, B, -U, x + bg, (optional_fields), comment
    print_transaction(now, Comments, a, b, amount + g, sprintf("%10.3f", units), fields, number_fields)
  } else if (units > 0) {
    # # For a purchase the asset must be account "b"
    # This must be a purchase
    correct_order = ((( b) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/) || ((a) ~ /^EQUITY[.:]/))
    assert(correct_order, sprintf("%s => can't buy asset %s\n", $0, (Leaf[b])))

    # If this is not an asset purchase swop the accounts
    if (!((b) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|\.CURRENCY:/)) {
      swop = a; a = b; b = swop
      amount = - amount
    }

    # Normally fields[1] is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if ((-1) != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Is this a new depreciating asset?
    if (((b) ~ /^ASSET\.FIXED[.:]/) && !(b in Method_Name)) {
      # This is  the asset Lifetime
      fields[++ number_fields] = Lifetime[b]  = Real_Value[(1)]; Real_Value[(1)] = 0
      assert(((((Lifetime[b]) - ( Epsilon)) > 0) || (((Lifetime[b]) - ( -Epsilon)) < 0)), sprintf("%s => Can't have a fixed asset %s with zero life", $0, (Leaf[b])))

      # We need the method name
      # Currently a choice of POOL, DV, or PC
      fields[++ number_fields] = Method_Name[b] = Depreciation_Type
    }

    # Allow for brokerage if required - note can't have brokerage with depreciation
    if (((((Real_Value[(1)]) - ( Epsilon)) > 0) || (((Real_Value[(1)]) - ( -Epsilon)) < 0))) {
      current_brokerage = Translation_Rate * Real_Value[(1)]
      Real_Value[(1)] = 0
    }

    # And simply adjust settlement account a by the
    if ((((Real_Value[(-2)]) - ( -Epsilon)) < 0)) {
      # This is a forex account - extra arguments fo not apply to complementary account
      sell_units(now, a, - Real_Value[(-2)], amount)
      Real_Value[(-2)] = 0
    } else # Simply adjust cost of <a> by the whole amount
      adjust_cost(a, -amount, now)

    # Impact of GST
    if (((((GST_Claimable) - ( Epsilon)) > 0) || (((GST_Claimable) - ( -Epsilon)) < 0))) {
      # Two cases
      #   No Brokerage Present => Adjust Whole Amount
      if (((((current_brokerage) - ( Epsilon)) <= 0) && (((current_brokerage) - ( -Epsilon)) >= 0)))
        # A  purchase
        # A, B, U, (1 - g) * x
        # A, G, 0,      g * x
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount
      else
        # Brokerage Present => Adjust Brokerage
        # Produce A, B, U, x + (1 - g) * b
        #         A, G,  0,         g * b
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * current_brokerage

      # Non-zero GST to be paid
      adjust_cost(GST, g, now)

      # This must be recorded
      Cost_Element = II
      print_transaction(now, ("# GST " Leaf[b]), a, GST, g)
      if (((((current_brokerage) - ( Epsilon)) <= 0) && (((current_brokerage) - ( -Epsilon)) >= 0)))
        assert((0), "GST Was levied on whole BUY transaction <" $0 ">")
      GST_Claimable = 0
    } else
      g = 0

    # Buy units in asset (b) - this ignores impact of brokerage
    bought_parcel = buy_units(now, b, units, amount - current_brokerage, Parcel_Name, Extra_Timestamp)

    # Adjust the cost of this **parcel** for the impact of brokerage and GST
    adjust_parcel_cost(b, bought_parcel, now, current_brokerage - g,  II, (0))

    # Update parent entries for <b>
    update_cost(b, amount - g, now)

    # Did we swop? If so swop back
    if (b == swop) {
      b = a; a = swop
      amount = - amount
    }

    # Record the transaction
    # Normal transactions
    # A, B, U, x, b - b*g, <optional-fields>, Comments
    # Record the adjustment due to brokerage and gst
    if (((((current_brokerage) - ( Epsilon)) > 0) || (((current_brokerage) - ( -Epsilon)) < 0)))
      fields[++ number_fields] = sprintf("%.*f", (2), current_brokerage - g)
    print_transaction(now, Comments, a, b, amount - g, sprintf("%10.3f", units), fields, number_fields)
  } else if (Automatic_Depreciation) {
    # This is automatic depreciation
    # Only need the assertion
    assert(((a) ~ /^ASSET\.FIXED[.:]/), sprintf("%s => Can't depreciate account %s", $0, a))

    # Carry out depreciation
    amount = depreciate_now(a, now)

    # Update parent costs
    update_cost(a, -amount, now)

    # The corresonding transaction
    adjust_cost(b, amount, now)

    # Record the transaction
    print_transaction(now, Comments, a, b, amount, "(I)")
  } else if (Extra_Timestamp > Epoch) {
    # The timestamp must be associated with a parcel
    fields[++ number_fields] = get_date(Extra_Timestamp)

    # A "parcel_timestamp" can indicate a maturity date
    # for a cash like term asset (eg a loan or a term deposit)
    # but only when an the asset is acquired and the timestamp is in the future

    # One account must be unitized or term limited
    # This logic is opaque...
    if (((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/) || ((b) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/)) {
      assert(!((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/) ||  !((b) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/),
               sprintf("%s Both %s and %s cannot be unitized when parcel timestamp [%s] is set",
               $0, (Leaf[a]), (Leaf[b]), get_date(Extra_Timestamp)))
      # Instalment purchase
      adjust_cost(b,  amount, Extra_Timestamp)
      adjust_cost(a, -amount, now)

      # This will not balance when re-read from the state file unless balancing entries made
      adjust_cost(ADJUSTMENTS, -amount, Extra_Timestamp)
      adjust_cost(ADJUSTMENTS,  amount, now)

    } else if (((b) ~ /^(ASSET|LIABILITY)\.TERM[.:]/) || ((b) ~ /^(ASSET|LIABILITY)\.CURRENT[.:]/)) {
      # This is a term deposit or similar (eg a mortgage or loan issued by the fund)
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, now)
    } else
      assert((0),
             sprintf("<%s> Either %s or %s must be a capital asset or a term asset when timestamp [%s] is set",
             $0, (Leaf[a]), (Leaf[b]), (Leaf[b]), get_date(Extra_Timestamp)))

     # Record the transaction
     print_transaction(now, Comments, a, b, amount, Cost_Element, fields, number_fields)
  } else {
    # All Other Transactions
    # This must be an expense if GST is involved
    if (((((GST_Claimable) - ( Epsilon)) > 0) || (((GST_Claimable) - ( -Epsilon)) < 0))) {
      # An expense
      # A, B, 0, (1 - g) * x
      # A, G, 0,      g * x
      g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount

      # Non-zero GST to be paid - transfer from EXPENSE account
      adjust_cost(GST,  g, now)
      adjust_cost(b,   -g, now)

      # Record GST
      print_transaction(now, ("# GST " Leaf[a]), b, GST, g)
      GST_Claimable = 0
    }

    # Adjust costs
    adjust_cost(a, -amount, now)
    adjust_cost(b,  amount, now)

    # Record the transaction
    print_transaction(now, Comments, a, b, amount, ((Tax_Adjustment)?( "(" Cost_Element ")"):( Cost_Element)), fields, number_fields)
  }

  # Tidy up
  delete fields
}

# Set an account term for term limited assets and liabilities
#
function set_account_term(a, now) {
  # It is possible for a current account to have no term set

  # If the term is set make a note of it
  if (Real_Value[(1)] > 0)
   # It can change as long as it doesn't make a CURRENT asset non current
   (Account_Term[a][ now] = ( Real_Value[(1)]))

  # At this stage term is assumed to be set in months
  if ((((Extra_Timestamp) - ( now)) > 0)) { # Use the time stamp if it exists
    (Maturity_Date[a][ now] = ( Extra_Timestamp))

    # Don't use real value again
    Real_Value[(1)] = 0
  } else if (Real_Value[(1)] > 0) {
    # Need to set the first maturity date - real value is the same as account term
    Extra_Timestamp = add_months(now, Real_Value[(1)])
    (Maturity_Date[a][ now] = ( Extra_Timestamp))

    # Don't use real value again
    Real_Value[(1)] = 0
  } else if ((a in Maturity_Date) && (((now) - ( ((__MPX_KEY__ = find_key(Maturity_Date[a],  ((now) - 1)))?( Maturity_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Maturity_Date[a][0]):( 0)))))) > 0)) {
    # Compute the maturity date
    Extra_Timestamp = add_months(now, ((__MPX_KEY__ = find_key(Account_Term[a],  now))?( Account_Term[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Account_Term[a][0]):( 0)))))
    (Maturity_Date[a][ now] = ( Extra_Timestamp))
  }
}

# Some no-ops covering for SMSF related functions
function update_profits(now) {
  # A No-op
  return
}

function update_member_liability(now, delta_profits, array, a) {
  # A no-op
  return
}

function process_member_benefits(t, array, x) {
  # A no-op
  return x
}

function process_member_contributions(t, x, array, a) {
  # A no-op
  return
}

# checking and setting
# Syntax
# date, COST, ACCOUNT, ....
function checkset(now, a, account, units, amount, is_check,
                       action, quantity) {
  # Save short name for action
  action = (Leaf[a])

  # First lets check
  if (is_check) {
    # Check the action just after now
    switch(action) {
      case "VALUE" :
        quantity = get_value(account, now); break
      case "PRICE" :
        assert(((account) ~ /^ASSET\.(CAPITAL|FIXED)[.:]|^EQUITY[.:]|\.CURRENCY:/), sprintf("CHECK/SHOW: Only assets or equities have a PRICE: not %s\n", (Leaf[account])))
        quantity = ((__MPX_KEY__ = find_key(Price[account],  now))?( Price[account][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[account][0]):( 0)))); break

      case "BALANCE" :
      case "COST" : quantity = get_cost(account, now); break

      case "UNITS" : quantity = ((account in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[account],   now))?( Total_Units[account][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[account][0]):( 0))))):( 0)); break
      default : assert((0), sprintf("CHECK/SHOW: %s => Unkown action %s\n",
                                      (Leaf[account]), action))
    }

    # Is this a checkpoint?
    assert(((((amount - quantity) - ( Epsilon)) <= 0) && (((amount - quantity) - ( -Epsilon)) >= 0)), sprintf("%s fails checkpoint %s [%s] => %.4f != %.4f\n",
                                                 (Leaf[account]), action, get_date(now, LONG_FORMAT), quantity, amount))
    # Show this
    printf "## %s %s %s => %s\n", get_date(now), account, action,  format_value(quantity)
  } else {
    # is a setter
    switch(action) {
      case "VALUE" :
        # Valuations are  per  unit price
        # Was the number of units given?
        if ((((units) - ( Epsilon)) > 0))
          amount /= units
        else # Just use the current cost if zero or negative units specified
          # If you want to set a zero value use PRICE instead
          amount = get_cost(account, now) / ((account in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[account],   now))?( Total_Units[account][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[account][0]):( 0))))):( 0))

      case "PRICE" :
        # This is a single unit
        # Set the price per unit
        (Price[account][ now] = ( amount))
      break

      case "BALANCE" :
      case "COST" :
        adjust_cost(account, amount, now)

        # top level class
        adjust_cost(((((get_name_component(account, 1)) ~ ("*")))?( (get_name_component(account, 1))):( (("*") (get_name_component(account, 1))))), - amount, now)
        break

      default : assert((0), sprintf("ADJUST/SET: %s => Unkown action %s\n",
                                      (Leaf[account]), action))
    }

    # Show this
    printf "## %s %s %s => %s\n", get_date(now), account, action,  format_value(get_cost(account, now))
  }
}

# Final processing
END {
  if (_assert_exit == 1)
    exit 1

  # Check version consistency
  assert(MPX_Version == Current_Version,     "Inconsistent snapshot file:\n\tExpected Version => " Current_Version " Found Version => " MPX_Version)

  # Delete empty accounts
  # Filter out data entries that were added by import CSV records
  # that do not overlap with the the holding period
  filter_data(Last_Record, Filter_Data, (0))

  # Make sure any eofy transactions are recorded
  # This loop will happen at least once
  if (Last_Record > Stop_Time)
    eofy_actions(Stop_Time)
  else if (!Write_State) {
    # We need to produce statements
    # The reason for not producing a final statement
    # when a state file is being written is that
    # it would lead to duplication of data processing
    # if the last record is not already beyond the Stop_Time
    do {
      # Get each EOFY in turn
      eofy_actions(FY_Time)

      # The next financial year
      FY_Year ++

      # Update FY length
      FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)
      Last_FY = FY_Time
      FY_Time = ((FY_Time) + one_year(FY_Time,  1))
    } while (FY_Time + EOFY_Window < Last_Record)

    # Fix FY_Time so that the snapshot is accurate
    FY_Year --
    FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)

    FY_Time = ((FY_Time) - one_year(FY_Time, -1))
    Last_FY = ((FY_Time) - one_year(FY_Time, -1))
  }

  # Write out code state - it sould be ok to do this after all processing now
  if (Write_State) {
    # Record the last state
    if (Last_Record > Last_State)
      Last_State = Last_Record
    write_state(Array_Names, Scalar_Names)

    # The last line is (oddly enough) when the journal starts -
    # this allows initialization to occur when the file is read back in
    if (!Write_Variables)
      printf "%s%sSTART_JOURNAL\n", get_date(Start_Record, ("%F")       ), OFS > Write_State
  }

  # Log data about selected variables
  if (Write_Variables)
    filter_data(Last_Record, Write_Variables, (1))
} #// END
#

# Sell qualified units
# This is trickier than ordinary units
# because the qualification is provisional until the
# qualification window is expired
function sell_qualified_units(a, u, now, half_window,      du, dq, key, next_key) {


  # Get the latest key not beyond the window
  key = find_key(Qualified_Units[a], now + half_window)

  # While keys exist that are in the future
  # adjust them on a last-in-first-out basis
  du = u
  while ((((key) - ( now)) > 0)) {
    # We will need the next key
    next_key = find_key(Qualified_Units[a], ((key) - 1))



    # How many provisionally qualified units are at the key entry?
    dq = ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[a],   key))?( Qualified_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[a][0]):( 0))))):( ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],    key))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0)))) - ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[a],   next_key))?( Qualified_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[a][0]):( 0))))):( ((a in Total_Units)?( ((__MPX_KEY__ = find_key(Total_Units[a],    next_key))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))):( 0))))

    # Assert all parcels must be positive
    assert((((dq) - ( Epsilon)) > 0), sprintf("Found a non-positive provisional parcel of units %s[%s] => %.3f",
            (Leaf[a]), get_date(key), dq))

    # Reduce this parcel first
    if (du < dq) {
      # Reduce this parcel
      sum_entry(Qualified_Units[a], -du, key)


      du = 0
    } else {


      # This parcel is removed fully
      # Safe to skip sum_entry because this is a last-in-first-out stack
      # so an empty entry must be the last entry
      delete Qualified_Units[a][key]
      du -= dq
    }

    # Any units left to apply?
    if (((((du) - ( Epsilon)) <= 0) && (((du) - ( -Epsilon)) >= 0)))
      break

    # Get the next key
    key = next_key
  } # End of while each key in the window

  # Do not adjust fully qualified units...
  # Instead add a negative parcel?
  # Do not adjust qualified parcels since these are needed for historical comparisons?
  if ((((du) - ( Epsilon)) > 0)) {
    sum_entry(Qualified_Units[a], - du, now) # Ok to make a non-provisional negative parcel

  }


}

# A special function to return the held time
function get_held_time(now, from,     held_time) {
  held_time = now - from

  # Fix the held time to allow for the case of leap years
  if (31622400 == held_time && 31622400 == one_year(now)) # A leap year
     held_time -= (86400) # Fake the held time to get the correct tax treatment
  return held_time
}

# Buy a parcel of u units at the cost of x
function buy_units(now, a, u, x, parcel_tag, parcel_timestamp,
                                             last_parcel, p) {


  # Some units are bought
  assert(((((u) - ( Epsilon)) > 0) || (((u) - ( -Epsilon)) < 0)), sprintf("buy_units[%s] : can't buy zero units", $0))

  # Override timestamp
  if (parcel_timestamp >= Epoch)
    now = parcel_timestamp

  # Make a new parcel
  last_parcel = new_parcel(a, u, x, now, parcel_tag)

  # Update units
  ((a in Total_Units)?( sum_entry(Total_Units[a],  u,  now)):( 0))

  # Also update qualified units - buying is easy
  if (((a) ~ /^ASSET\.CAPITAL[.:]/) && Qualification_Window) {
    sum_entry(Qualified_Units[a], u, now + 0.5 * Qualification_Window)

  }

  # Debugging


  # Buy u units for x
  u = Units_Held[a][last_parcel]
  x = (((__MPX_KEY__ = find_key(Accounting_Cost[a][ last_parcel][ I],  ( now)))?( Accounting_Cost[a][ last_parcel][ I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][ last_parcel][ I][0]):( 0))))) # Element I is all important

  # Passive revaluation
  p = x / u


  # Set the new price - not when Translation_Rate is set or this is circular
  if (!Translation_Currency)
    (Price[a][ now] = ( p))

  # Return the parcel id
  return last_parcel
}

# Create or modify a parcel
function new_parcel(ac, u, x, now, parcel_tag,        last_parcel, key) {
  # Purchases made at the same time can be averaged
  # So only increment the parcel counter if a new parcel name specified
  last_parcel = Number_Parcels[ac] - 1
  if ((now > Held_From[ac][last_parcel]) || "" != parcel_tag) {
    Number_Parcels[ac] ++
    last_parcel ++

    # This adds a new parcel - always cost element I
    # We have to initialize  this parcels cost elements
    Parcel_Proceeds[ac][last_parcel] = 0
    for (key in Elements)
      Accounting_Cost[ac][last_parcel][key][Epoch] = 0

    # The tax adjustments
    Tax_Adjustments[ac][last_parcel][Epoch] = 0

    # A new purchase is always cost element I
    Accounting_Cost[ac][last_parcel][I][now] = x

    # The other entries
    Units_Held[ac][last_parcel]   = u
    Held_From[ac][last_parcel]  = now # When this parcel was purchased
    Held_Until[ac][last_parcel] = Future # Simplify logic

    # Is parcel name set?
    if (parcel_tag)
      Parcel_Tag[ac][last_parcel] = parcel_tag
  } else {
    # The parcel exists but update the units and cash
    Units_Held[ac][last_parcel] += u
    sum_entry(Accounting_Cost[ac][last_parcel][I], x, now)
  }

  return last_parcel # This is the id of the updated parcel
}

# Sell units
#  Sale receipts go to cash_out and happen at Held_Until
#  At this point only the adjusted cost is made use of
function sell_units(now, ac, u, x, parcel_tag, parcel_timestamp,        du, p, did_split, new_price, proportional_cost, catch_up_depreciation, t) {
  # Set a default parcel_timestamp
  parcel_timestamp = (("" == parcel_timestamp)?( (-1)):( parcel_timestamp))



  # Try adjusting units now...
  ((ac in Total_Units)?( sum_entry(Total_Units[ac],  -u,  now)):( 0))

  # For a depreciating asset with multiple parcels (apart from a pooled asset) then
  # the proportion of the asset being sold depends on the
  # original cost of each unit; not on the number of units
  proportional_cost = (0)
  if (((ac) ~ /^ASSET\.FIXED[.:]/)) {
    # Is this asset's depreciation upto date?
    # Depreciate

    if (now > FY_Time) {
      t = ((FY_Time) - 1)
      catch_up_depreciation = depreciate_now(ac, t)
      if (((((catch_up_depreciation) - ( Epsilon)) > 0) || (((catch_up_depreciation) - ( -Epsilon)) < 0))) {
        update_cost(ac, - catch_up_depreciation, t)

        # Balance accounts
        adjust_cost(DEPRECIATION, catch_up_depreciation, t)

        # Print the transaction
        print_transaction(t, "# Closing Depreciation", ac, DEPRECIATION, catch_up_depreciation, "(D)")

      }
    }

    # More complications - a fully depreciated asset
    # or a Pooled asset
    if ("POOL" == Method_Name[ac]) {
      # For a pooled asset we must see a unique parcel
      # Since only one parcel proportional_cost is FALSE
      assert(1 == Number_Parcels[ac] || "" != parcel_tag || (-1) != parcel_timestamp,
             "Must sell a specified parcel for pooled asset <" ac ">")

      # Need to set new price  - based on units just like non-depreciating assets
      new_price = x / u
    } else {
      # Not pooled
      # Care is needed when the cost is zero
      p = get_cost(ac, now)
      if (((((p) - ( Epsilon)) > 0) || (((p) - ( -Epsilon)) < 0)))
        proportional_cost = x / p
      else
        new_price = 0
    }
  } else {
    # Not depreciating
    # Also update qualified units - selling is harder
    if (Qualification_Window && (((u) - ( Epsilon)) > 0))
      sell_qualified_units(ac, u, now, 0.5 * Qualification_Window)

    # Sell u units for x
    # Both x & u are positive by design
    new_price = x / u

    # Can set the price of a normal asset - not when Transaction_Currency is set
    if (Transaction_Currency)
      (Price[ac][ now] = ( new_price))
  }

  # Default assumption is first-in-first-out (FIFO)
  # But can be overriden if specific parcel given
  p = 0
  while (u > 0 && p < Number_Parcels[ac]) {
    # Skip sold parcels - including those sold today
    if ((Held_Until[ac][ p] <= ( now))) {
      p ++
      continue
    }

    # Match the parcel id
    if (match_parcel(ac, p, parcel_tag, parcel_timestamp)) {
      # The units in each parcel are fixed for the life of the parcel
      du = Units_Held[ac][p]

      # If there are fewer units in this parcel than
      # are being sold then the whole parcel is sold off
      if (du < u) # Parcel entirely sold off
        u -= du
      else {
        du = u # Last parcel
        u = 0
      }

      # Set the share of each unit's cost applicable to this parcel
      # it differs for depreciating and non-depreciating assets
      if (proportional_cost)
        # Share per parcel is (du * x * parcel_cost / total_cost)
        new_price = get_parcel_cost(ac, p, now) * proportional_cost



      # Sell the parcel
      did_split = sell_parcel(ac, p, du, du * new_price, now)


    } # End of match parcel

    # The next parcel
    p ++
  } # End of while statement



  # Were all the requested units actually sold?
  assert(((((u) - ( Epsilon)) <= 0) && (((u) - ( -Epsilon)) >= 0)), sprintf("sell_units: Failed to sell the requested %d units of %s", u, (Leaf[ac])))

  # Update parent sums
  update_cost(ac, -x, now)
}

#
# sell part or all a parcel at time now
# account  a
# parcel   p
# sell     du units
# proceeds amount_paid
# time     now

function sell_parcel(a, p, du, amount_paid, now,      gains, i, is_split) {
  # The sale date
  Held_Until[a][p] = now

  # No parcel split yet
  is_split = (0)

  # Amount paid


  # Check for an empty parcel - allow for rounding error
  if (((((Units_Held[a][p] - du) - ( Epsilon)) <= 0) && (((Units_Held[a][p] - du) - ( -Epsilon)) >= 0)))
    # Parcel is sold off
    Units_Held[a][p] = du
  else { # Units remain - parcel not completely sold off


    # Shuffle parcels up by one
    for (i = Number_Parcels[a]; i > p + 1; i --)
      # Copy the parcels
      copy_parcel(a, i - 1, a, i)

    # At this point we need to split parcels p & p + 1
    split_parcel(a, p, du)
    is_split = (1)

    # One extra parcel
    Number_Parcels[a] += 1
  } # End of if splitting a parcel

  # Save realized gains
  if (((a) ~ /^ASSET\.FIXED[.:]/))
    gains = save_parcel_income(a, p, now, - amount_paid,  SOLD_APPRECIATION, SOLD_DEPRECIATION)
  else if (((a) ~ /^ASSET\.CURRENT\.CURRENCY[.:]/)) {
    # This should be treated as income rather than as capital gains and losses
    # gains = save_parcel_gain(a, p, now, - amount_paid)
    gains = save_parcel_income(a, p, now, - amount_paid,  FOREX_INCOME, FOREX_EXPENSE)

  } else
    gains = save_parcel_gain(a, p, now, - amount_paid)

  # Don't need to set the accounting cost to zero because get_cost will pick that up automatically
  # But the capital gain or loss needs to be balanced in the asset sums
  update_cost(a, -gains, now)

  # The sale price
  # This must be recorded as cash flowing out of the account
  # A parcel is only ever sold once so we can simply set the cost
  (Parcel_Proceeds[a][ p] = ( -amount_paid))



  # Was a parcel split
  return is_split
} # End of if non-zero Parcel

# #
# # When a parcel of a fixed asset is sold
# # it changes the depreciation amounts
# function sell_fixed_parcel(a, p, now, gains,    cost) {
#   # A depreciating asset will neither have capital gains nor losses
#   # Suppose current value => c  (c >= 0)
#   # Proceeds received     => p  (p <= 0)
#   #
#   # After sale value      => 0
#   # Depreciation          => c + p <= 0
#   # Appreciation             c + p >  0
#
#   gains += sum_cost_elements(Accounting_Cost[a][p], now)
# @ifeq LOG sell_units
#   if (above_zero(gains)) # This was a DEPRECIATION expense
#     printf "\tDepreciation => %s\n", print_cash(gains) > STDERR
#   else if (below_zero(gains)) # This was an APPRECIATION income
#     printf "\tAppreciation => %s\n", print_cash(-gains) > STDERR
#   else
#     printf "\tZero Depreciation\n" > STDERR
# @endif # LOG
#
#   # Any excess income or expenses are recorded
#   if (above_zero(gains)) # This was a DEPRECIATION expense
#     adjust_cost(SOLD_DEPRECIATION, gains, now)
#   else if (below_zero(gains)) # This was APPRECIATION income
#     adjust_cost(SOLD_APPRECIATION, gains, now)
#
#   # return parcel gains
#   return gains
# }
#

# Some parcels when sold are treated as income rather than gains
# Namely fixed (depreciating) assets and foreign exchange
function save_parcel_income(a, p, now, income, income_account, expense_account,   cost) {
  # Suppose current value => c  (c >= 0)
  # Proceeds received     => p  (p <= 0)
  #
  # After sale value      => 0
  # Expenses              => c + p >= 0
  # Income                => c + p <  0

  income += sum_cost_elements(Accounting_Cost[a][p], now)


  # Any excess income or expenses are recorded
  if ((((income) - ( Epsilon)) > 0)) # This was an expense
    adjust_cost(expense_account, income, now)
  else if ((((income) - ( -Epsilon)) < 0)) # This was income
    adjust_cost(income_account, income, now)

  # return parcel income
  return income
}

# Save a capital gain
# The price paid for an asset will probably not match its
# current cost; if the price is greater a capital gain will result
# if less a capital loss; these losses and gains will be retained
# in the cost basis...
function save_parcel_gain(a, p, now, gains,   tax_gain, held_time) {
  # Get the held time
  held_time = get_held_time(now, Held_From[a][p])

  # Accounting gains or Losses - based on reduced cost
  # Also taxable losses are based on the reduced cost...
  gains += sum_cost_elements(Accounting_Cost[a][p], now)
  if ((((gains) - ( Epsilon)) > 0)) {
    adjust_cost(REALIZED_LOSSES, gains, now)

    # Taxable losses are based on the reduced cost
    if (held_time >= 31622400) {
      if (!(a in Long_Losses))
        Long_Losses[a] = initialize_account(("SPECIAL.TAXABLE.LOSSES.LONG") ":LL." Leaf[a])
      adjust_cost(Long_Losses[a], gains, now)
    } else {
      if (!(a in Short_Losses))
        Short_Losses[a] = initialize_account(("SPECIAL.TAXABLE.LOSSES.SHORT") ":SL." Leaf[a])
      adjust_cost(Short_Losses[a], gains, now)
    }

    # Balance taxable losses
    adjust_cost("*SPECIAL", -gains, now)
  } else if ((((gains) - ( -Epsilon)) < 0))
    adjust_cost(REALIZED_GAINS, gains, now)

  # Taxable gains
  # after application of tax adjustments
  # This works if tax adjustments are negative
  tax_gains = gains - ((__MPX_KEY__ = find_key(Tax_Adjustments[a][p],  now))?( Tax_Adjustments[a][p][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][p][0]):( 0))))

  # Taxable Gains are based on the adjusted cost
  if ((((tax_gains) - ( -Epsilon)) < 0)) {
    # Taxable losses are based on the reduced cost
    if (held_time >= 31622400) {
      if (!(a in Long_Gains))
        Long_Gains[a] = initialize_account(("SPECIAL.TAXABLE.GAINS.LONG") ":LG." Leaf[a])
      adjust_cost(Long_Gains[a], tax_gains, now)
    } else {
      if (!(a in Short_Gains))
        Short_Gains[a] = initialize_account(("SPECIAL.TAXABLE.GAINS.SHORT") ":SG." Leaf[a])
      adjust_cost(Short_Gains[a], tax_gains, now)
    }

    # Balance taxable gains
    adjust_cost("*SPECIAL", -tax_gains, now)
  }

  # Return gains
  return gains
}

# Copy and split parcels
function copy_parcel(a, p, b, q,     e, key) {
  # Copy parcel a:p => b:q
  Units_Held[b][q]  = Units_Held[a][p]
  Held_From[b][q] = Held_From[a][p]
  Held_Until[b][q] = Held_Until[a][p]
  Parcel_Proceeds[b][q] = Parcel_Proceeds[a][p]
  if ((( a in Parcel_Tag) && ( p in Parcel_Tag[ a]))) {
    Parcel_Tag[b][q] = Parcel_Tag[a][p]
    delete Parcel_Tag[a][p]
  }

  # Copy all entries
  # Note keys will not match so need to delete old entries from parcel q
  delete Accounting_Cost[b][q] # Delete old entries
  delete Tax_Adjustments[b][q]
  for (e in Accounting_Cost[a][p])
    for (key in Accounting_Cost[a][p][e])
        Accounting_Cost[b][q][e][key] = Accounting_Cost[a][p][e][key]
  for (key in Tax_Adjustments[a][p])
    Tax_Adjustments[b][q][key]  = Tax_Adjustments[a][p][key]
}

function split_parcel(ac, p, du,   fraction_kept, e, key) {
  # Split partly sold parcel p into parcel p (all sold)
  # and unsold parcel p + 1

  # Fraction Kept
  fraction_kept = (Units_Held[ac][p] - du) / Units_Held[ac][p]

  # Parcel p + 1 units - careful with the order of these statements
  Units_Held[ac][p + 1] = Units_Held[ac][p] - du
  Units_Held[ac][p]     = du # Units are conserved

  # The date
  Held_From[ac][p + 1]  = Held_From[ac][p]
  Held_Until[ac][p + 1] = Future

  # Parcel Proceeds
  Parcel_Proceeds[ac][p + 1] = 0

  # Parcel tag
  if ((( ac in Parcel_Tag) && ( p in Parcel_Tag[ ac])))
    Parcel_Tag[ac][p + 1] = Parcel_Tag[ac][p]

  # The New Adjustments
  # Need to delete old arrays because
  # they will exist with different keys
  delete Accounting_Cost[ac][p + 1] # Delete old entries
  delete Tax_Adjustments[ac][p + 1]

  # The very first key
  for (e in Accounting_Cost[ac][p])
    for (key in Accounting_Cost[ac][p][e])
        Accounting_Cost[ac][p + 1][e][key] = fraction_kept * Accounting_Cost[ac][p][e][key]
  for (key in Tax_Adjustments[ac][p])
      Tax_Adjustments[ac][p + 1][key]  = fraction_kept * Tax_Adjustments[ac][p][key]

  # The balance
  for (e in Accounting_Cost[ac][p])
    for (key in Accounting_Cost[ac][p][e])
        Accounting_Cost[ac][p][e][key] -= Accounting_Cost[ac][p + 1][e][key]
  for (key in Tax_Adjustments[ac][p])
    Tax_Adjustments[ac][p][key]  -= Tax_Adjustments[ac][p + 1][key]
}

# Find a parcel that can be sold
function match_parcel(a, p, parcel_tag, parcel_timestamp,
                      matched_date) {
  # Does the parcel timestamp match?
  matched_date = ((-1) == parcel_timestamp || Held_From[a][p] == parcel_timestamp)

  # Determine if this parcel matches either the parcel tag or timestamp if either set
  # Does the parcel tag match?
  if ("" != parcel_tag)
    # There is a tag
    # This will either find a unique tag or a possibly non unique tag on the matched date
    return matched_date && ((( p in Parcel_Tag[a])?( Parcel_Tag[a][ p]):( "")) == parcel_tag)

  # Now check  the timestamp
  return matched_date
}


# This checks all is ok
function check_balance(now,        sum_assets, sum_liabilities, sum_equities, sum_expenses, sum_income, sum_adjustments, balance, show_balance, output_stream) {
  # The following should always be true
  # Assets - Liabilities = Income + Expenses
  # This compares the cost paid - so it ignores the impact of revaluations
  sum_assets =  get_cost("*ASSET", now)

  # Work out the total assets etc
  sum_liabilities = - get_cost("*LIABILITY", now)
  sum_equities    = - get_cost("*EQUITY", now)
  sum_expenses    = - get_cost("*EXPENSE", now)
  sum_income      = - get_cost("*INCOME", now)
  sum_adjustments =   get_cost("*BALANCING", now)

  # The balance should be zero
  balance = sum_assets - (sum_liabilities + sum_equities + sum_income + sum_expenses + sum_adjustments)


  # No default printing
  show_balance = (0)
  output_stream = "/dev/stderr"


  # Is there an error?
  if (((((balance) - ( Epsilon)) > 0) || (((balance) - ( -Epsilon)) < 0))) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > output_stream
    show_balance = (1)
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > output_stream
    printf "\tAssets      => %20.2f\n", sum_assets > output_stream
    printf "\tIncome      => %20.2f\n", sum_income > output_stream
    printf "\tExpenses    => %20.2f\n", sum_expenses > output_stream
    printf "\tLiabilities => %20.2f\n", sum_liabilities > output_stream
    printf "\tEquities    => %20.2f\n", sum_equities > output_stream
    printf "\tAdjustments => %20.2f\n", sum_adjustments > output_stream
    printf "\tBalance     => %20.2f\n", balance > output_stream
    assert(((((balance) - ( Epsilon)) <= 0) && (((balance) - ( -Epsilon)) >= 0)), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
