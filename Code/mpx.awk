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



# // Control Logging




# // Control Export format




# //




# // Some constants









# // Output Date Formats



# // Default Asset Prefix for Price Lists



# // The Epoch and minimum time difference









# // Day Number For Feb 29



# // Reserved Classes


# // Useful inline functions - this may be overdoing it






#
# // Useful shorthands for various kinds of accounts






# // Fixed asset


#//@define is_depreciating(a) ((a) ~ /^ASSET\.FIXED[.:]/)




# //
# // The last component of a name is the suffix


# // Reserved Tax Offset Classes








# // Is a leaf name in a linked account format i.e. first component is
# // (DIV|DIST|FOR).LEAF => LEAF



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















# // Get a single tranaction from the account




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





#




# // Capital Loss Window
# // Unlimited goes all the way to the Epoch
# // No need to compute the number of years exactly
# // The carry forward and write back limits



# // Multi-Line Macro




# // These two are not very readable
# // @define get_cash_in(a, i, now) (ternary((now >= Held_From[(a)][(i)]), find_entry(Accounting_Cost[(a)][(i)][I], Held_From[(a)][(i)]), 0))
# // @define get_cash_out(a, i, now) (ternary(is_sold(a, i, (now) + 1), find_entry(Accounting_Cost[(a)][(i)][0], (now)),0))


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



# This function actually reads the CBA formatted record
# It is very minimal
function read_price(  a, p, t, x, symbol, date_string) {
  # Check syntax
  assert(7 == NF, "Illegal price record syntax <" $0 ">")

  # closing price
  p = $6
  if ((((p) <= Epsilon) && ((p) >= -Epsilon)))
    next # Nothing to do

  # The symbol has to be expanded
  # the symbol needs a prefix and suffix , usually ASSET.CAPITAL.SHARES and ASX
  # but an entry like these can override them, eg
  # <<, Asset_Prefix, ASSET.CAPITAL.LONDON,>>
  # <<, Asset_Suffix, FTSE,>>
  #
  symbol = Asset_Prefix ":" $1 "." Asset_Suffix

  # Ok
  a = initialize_account(symbol)
  t = read_date((substr(($2), 1, 2) "-" substr(($2), 3, 2) "-" substr(($2), 5, 2)), (16)) # Set the CLOSING price

  # A legal date?
  assert((-1) != t, Read_Date_Error)

  # Logging


  # Set the price
  (Price[a][( t)] = ( p))

  # Done
}

# This function actually reads the CBA formatted record
# It is very minimal
# Syntax of record is
# $1 => Qualifying_Date, $2 => Dividend amount in cents (ignored), $3 => Record Date (ignored), $4 => Payment_Date (used), $5 => Dividend_Type (ignored), $6... (ignored)
# 07/03/2019,77.3232,08/03/2019,26/03/2019,I,-,
function read_qualifying_dates(  a, q_date, p_date) {
  # Get the account
  a = initialize_account(Asset_Prefix ":" Symbol)

  # Ok
  q_date = read_date($1) # Default hour - qualifying date

  # A legal date?
  if (q_date < Epoch)
    return

  # Now the payment date
  p_date = read_date($4) # Default hour - payment date
  assert(p_date > q_date, "Qualifying date <" $1 "> must always precede payment date <" $4 ">")
  # Logging


  # Set the ex dividend date
  (Payment_Date[a][( q_date)] = ( p_date))

  # Done
}

# get the most relevant ex-dividend date
function get_exdividend_date(a, now,   value, key, exdividend_key, discrepancy) {

  # We start at the time "now" in the accounts
  # Which should be equal to or shortly after the
  # payment date - now since  the
  # payment date must be after the qualifying date
  # search back to find the earlier entries
  # since Payment_Date[ex_dividend_date] => now-ish
  if (a in Payment_Date) {
    # Get the most recent key - this is an ex-dividend date
    exdividend_key = find_key(Payment_Date[a], now)

    # The payment date that corresponds to this key
    value = Payment_Date[a][exdividend_key]
    discrepancy = now - value

    # The value cannot be later than the current time "now"
    if (value > now) {
      Read_Date_Error = "Payment date is later than current date"
      return (-1)
    }
    else if ((((discrepancy) <= Epsilon) && ((discrepancy) >= -Epsilon)))
      return exdividend_key

    # Some times dividends are paid out of order, for example
    # a special or buyback dividend might have an extra
    # long qualification period - so look ahead more dividends
    # until the discrepancy increases
    #
    key = exdividend_key
    while (key) {
      key = find_key(Payment_Date[a], ((key) - 1))
      value = Payment_Date[a][key]
      if ((now - value) > discrepancy)
        # A worse match
        break

      # A better match
      discrepancy = now - value
      if ((((discrepancy) <= Epsilon) && ((discrepancy) >= -Epsilon)))
        return key

      # Save  this match
      exdividend_key = key
    }

    # Best match was exdividend_key
    if (discrepancy > 604800) {
      Read_Date_Error = "Failed to find a payment date within one week of current date"
      return (-1)
    }

    return exdividend_key
  }

  # Failed to find a qualification date
  Read_Date_Error = "Failed to find any payment date"
  return (-1)
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


  # Is this an array?
  if (nf == 1) { # No
    SYMTAB[Variable_Name] = value

  } else {
    # The rest of the keys
    for (i = 1; i < nf; i ++)
      # The code can minimize output file by
      # retaining the old key if the "ditto"
      # symbol is encountered;
      if (($i != ("^")) || !(i in Variable_Keys))
        Variable_Keys[i] = $i



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


  # Delete temporary key
  if (flag) {
    delete array[SUBSEP]   # delete scalar element
    flag = FALSE
  }

  # Set the array recursively
  if (first_key == last_key)
    # Set the value
    (array[( keys[first_key])] = ( value))
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
    ((SUBSEP in Key_Index)?(TRUE):(FALSE))
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
        printf "%s,", Key_Index[i] > stream
        if (("^"))
          Key_Index[i] = ("^")
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
    if ((((v) <= Epsilon) && ((v) >= -Epsilon)))
      v = 0
    else # force floating point format
      v = sprintf("%.*f", (6), v)
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

  for (j in array) {
    v = array[j]
    k = find_key(array, j - 1)
    if ("" == k) # Last key
      return
    w = array[k]

    # Look for duplicates
    if ((((v - w) <= Epsilon) && ((v - w) >= -Epsilon)))
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

  # Finished
  array[now] = x + array[key]
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
    max = (((max)>( array[key]))?(max):( array[key]))

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

# Is a number between two others? (allow boundary cases to be inside)
function is_between(x, low, high) {
  return  (x - low) * (x - high) <= 0
}

# Clear global values ready to read a new input record
function new_line() {
  Extra_Timestamp = (-1)
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
  precision = (("" == precision)?( (2)):( precision))

  if ("" == x)
    return "  -  "
  if ((((x) <= Epsilon) && ((x) >= -Epsilon)))
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
        Comments = (("" == Comments)?(  x):( (("" ==  x)?( Comments):( (Comments ", "  x)))))
    }

    # Increment i
    i ++
  }

  # Comments should be signified with an octothorpe
  if (Comments !~ /^#/)
    Comments = (("" == "# ")?(  Comments):( (("" ==  Comments)?( "# "):( ("# " ", "  Comments)))))

  # A document name is added as a final comment
  Comments = (("" == Comments)?(  Documents):( (("" ==  Documents)?( Comments):( (Comments ", "  Documents)))))

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
  if ((-1) == Extra_Timestamp) {
    Extra_Timestamp = read_date(field)

    # The timestamp interpretation takes precedence
    if ((-1) != Extra_Timestamp)
      # If a time-stamp is set this will return ""
      return ""
  }

  # Next - is it a numerical value?
  # FIXME can do better than this
  value = (((strtonum(field)) < 0)?( -(strtonum(field))):(strtonum(field)))

  # This is a value
  if ((((value) <= Epsilon) && ((value) >= -Epsilon)))
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
    Documents = (("" == Documents)?(  Document_Name[string]):( (("" ==  Document_Name[string])?( Documents):( (Documents ", "  Document_Name[string])))))
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
    ((SUBSEP in optional_fields[field_rank])?(TRUE):(FALSE))
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
function one_year(now, sense,     n, year, day, sum) {
  # Sense == n is forward n years
  # Sense == -n is back n years

  # Get the day number
  day = (strftime("%j", (now), UTC) + 0)

  # Which Calendar year is this?
  year = (strftime("%Y", (now), UTC) + 0)
  if (sense > 0) {
    n = sense
    year += sense
  } else
    n = - sense

  # Go back n years
  sum = 0
  while (n -- > 0) {
    sum += (((( day) <= (60))?( ((((year) - 1) % 4 == 0 && ((year) - 1) % 100 != 0) || ((year) - 1) % 400 == 0)):( (((year) % 4 == 0 && (year) % 100 != 0) || (year) % 400 == 0))) + 365)
    year --
  }

  # Get the length in seconds
  return sense * (86400) * sum
}

# Useful account filters
function is_open(a, now,     p) {
  # An asset is open if there are unsold parcels at time 'now'
  for (p = 0; p < Number_Parcels[a]; p ++) {
    if (Held_From[a][p] > now)
      break
    if ((Held_Until[(a)][( p)] > ( now)))
      return TRUE
  }
  return FALSE
}

# Is an account a an ancestor of another account b?
function is_ancestor(a, b,    p) {
  if (!((a) ~ /^*/))
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
    if ((Held_Until[(a)][( p)] <= ( now)))
      latest_sale = (((latest_sale)>( Held_Until[ac][p]))?(latest_sale):( Held_Until[ac][p]))
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

  # On the first call the long name might not have been set
  #if (show_name == get_key(Leaf, a))
  #  return a

  # If the class name is another account it does not match
  if (show_name in Leaf)
    return FALSE

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
  if (((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)) {

    # What proportion of the sum is allocated to each unit at time now?

    # Either divide adjustment between all open parcels OR
    # concentrate with a parcel with the same timestamp
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if (Held_From[a][i] == now) {
        # The adjustment is pooled explicitly with this parcel
        adjust_parcel_cost(a, i, now, x, Cost_Element, tax_adjustment)


        # Also record the parents cost
        # If this is a tax adjustment then only negative costs are significant
        if (!tax_adjustment || x < 0)
          update_cost(a, x, now)

        return # Only one parcel is adjusted - it must be unsold if only just purchased
      }
    }

    # The cost adjustment per unit except for depreciating assets
    if (flag = ((a) ~ /^ASSET\.FIXED[.:]/))
      adjustment = x / get_cost(a, now)
    else
      adjustment = x / ((__MPX_H_TEMP__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[a][0]):( 0))))

    # Debugging


    # Scan back down the parcels held and unsold at time now
    while (i -- > 0) {
      if ((Held_Until[(a)][( i)] > ( now))) # This is an unsold parcel at time (now)
        # The parcel adjustment is proportional to the parcel size unless it is a depreciating asset
        if (flag)
          adjust_parcel_cost(a, i, now, get_parcel_cost(a, i, now) * adjustment, Cost_Element, tax_adjustment)
        else
          adjust_parcel_cost(a, i, now, Units_Held[a][i] * adjustment, Cost_Element, tax_adjustment)
    } # End of each parcel

    # Debugging


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

  update_cost(p, x, now)
}

function adjust_parcel_cost(a, p, now, parcel_adjustment, element, adjust_tax,        cost_base) {


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
  if (!((a) ~ /^EQUITY[.:]/)) {
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

} # End of adjust_parcel_cost

# # The idea of the "cost" of the account
# # This is the same as the reduced cost
# function get_cost(a, now,      i, sum_cost) {
#   # Initial cost
#   sum_cost = 0
#
#   # Adjustments for units bought
#   if (is_unitized(a)) {
#     for (i = 0; i < Number_Parcels[a]; i ++) {
#       if (Held_From[a][i] > now) # All further transactions occured after (now)
#         break # All done
#       if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
#         sum_cost += sum_cost_elements(Accounting_Cost[a][i], now)
#     }
#   } else if (a in Cost_Basis) # Cash-like
#     sum_cost = find_entry(Cost_Basis[a], now)
#
#   return sum_cost
# }

# The idea of the "cost" of the account
# This is the same as the reduced cost
function get_cost(a, now,     i, sum_cost) {
  # Adjustments for units bought
  if (((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)) {
    # Initial cost
    sum_cost = 0

    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if ((Held_Until[(a)][( i)] > ( now))) # This is an unsold parcel at time (now)
        sum_cost += sum_cost_elements(Accounting_Cost[a][i], now)
    }
    return sum_cost
  } else if (a in Cost_Basis) # Cash-like
    return ((__MPX_H_TEMP__ = find_key(Cost_Basis[a],  now))?( Cost_Basis[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Cost_Basis[a][0]):( 0))))

  return 0
}

# The tax adjustments at time (now)
# Note that depreciation is always a tax adjustment
function get_cost_adjustment(a, now,   i, sum_adjustments) {
  # Initial adjustments
  sum_adjustments = 0

  # Adjustments for units bought
  # Do not apply to equities
  if (((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]/)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if ((Held_Until[(a)][( i)] > ( now))) # This is an unsold parcel at time (now)
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
    if (((a) ~ /^ASSET\.CAPITAL[.:]/) && is_open(a, now))
      # The asset must be active
      sum += get_cost(a, now) - ((__MPX_H_TEMP__ = find_key(Price[a],  now))?( Price[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Price[a][0]):( 0)))) * ((__MPX_H_TEMP__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[a][0]):( 0))))

  # All done - negative values are gains
  return sum
}

# Sum  the cost elements
function sum_cost_elements(array, now,     sum_elements, e) {


  sum_elements = 0
  for (e in array) # Should this include [0] or not?
    sum_elements += ((__MPX_H_TEMP__ = find_key(array[e],  now))?( array[e][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( array[e][0]):( 0))))
  return sum_elements
}

# Get the specified cost element
function get_cost_element(a, element, now,      i, sum_cost) {
  # Initial cost
  sum_cost = 0

  # Only assets have cost elements - equity is just for simplicity
  if (((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)) {
    for (i = 0; i < Number_Parcels[a]; i ++) {
      if (Held_From[a][i] > now) # All further transactions occured after (now)
        break # All done
      if ((Held_Until[(a)][( i)] > ( now))) # This is an unsold parcel at time (now)
        sum_cost += ((__MPX_H_TEMP__ = find_key(Accounting_Cost[a][i][element],  now))?( Accounting_Cost[a][i][element][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Accounting_Cost[a][i][element][0]):( 0))))
    }
  }

  return sum_cost
}

# The parcel cost
function get_parcel_element(a, p, element, now, adjusted) {
  # Adjusted or reduced cost?
  if (adjusted)
    # The adjusted parcel cost
    adjusted = (((__MPX_H_TEMP__ = find_key(Tax_Adjustments[a][ p][ element],  ( now)))?( Tax_Adjustments[a][ p][ element][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Tax_Adjustments[a][ p][ element][0]):( 0)))))
  else
    adjusted = 0

  # This elements costs
  return ((__MPX_H_TEMP__ = find_key(Accounting_Cost[a][p][element],  now))?( Accounting_Cost[a][p][element][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Accounting_Cost[a][p][element][0]):( 0)))) - adjusted
}

# The initial cost
function get_cash_in(a, i, now) {

  # Is the account open?
  if (now >= Held_From[a][i])
    # Yes - always element I
    return ((__MPX_H_TEMP__ = find_key(Accounting_Cost[a][i][I],  Held_From[a][i]))?( Accounting_Cost[a][i][I][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Accounting_Cost[a][i][I][0]):( 0)))) # The Held_From time ensures  that later element I costs do not impact the result

  # No - so no activity
  return 0
}

# The cash paid out of the asset when sold
function get_cash_out(a, i, now) {
  # We are only interested in the sale payment for this parcel - the zeroth element
  if ((Held_Until[(a)][( i)] <= ( now)))
    # Each parcel can only be sold once - so if sold it is the first entry
    return (first_entry(Accounting_Cost[a][ i][0]))

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

# # The tax adjustments at time (now)
# # Note that depreciation is always a tax adjustment
# function get_cost_adjustment(a, now,   i, sum_adjustments) {
#   # Initial adjustments
#   sum_adjustments = 0
#
#   # Adjustments for units bought
#   # Do not apply to equities
#   if (!is_equity(a)) {
#     for (i = 0; i < Number_Parcels[a]; i ++) {
#       if (Held_From[a][i] > now) # All further transactions occured after (now)
#         break # All done
#       if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
#         sum_adjustments += sum_cost_elements(Tax_Adjustments[a][i], now)
#     }
#   }
#
#   return sum_adjustments
# }

# # Get the tax adjustment for a particular element
# function get_tax_element_adjustment(a, element, now,      i, sum_adjustments) {
#   # Initial adjustments
#   sum_adjustments = 0
#
#   # Adjustments for units bought
#   # Do not apply to equities
#   if (!is_equity(a)) {
#     for (i = 0; i < Number_Parcels[a]; i ++) {
#       if (Held_From[a][i] > now) # All further transactions occured after (now)
#         break # All done
#       if (is_unsold(a, i, now)) # This is an unsold parcel at time (now)
#         sum_adjustments += get_parcel_tax_adjustment(a, i, element, now)
#     }
#   }
#
#   return sum_adjustments
# }

# Print out transactions
# Generalize for the case of a single entry transaction
function print_transaction(now, comments, a, b, u, amount, fields, n_field,     matched) {
  if (!Show_All && (now < Start_Time || now > Stop_Time))
    return

  # Are we matching particular accounts?
  ((matched= Show_Account)?(((matched=match_account( a,  Show_Account))?( matched):( matched=match_account( b,  Show_Account)))):(matched=""))
  if (!Show_Account || matched)
    # Print the transaction out
    printf "%s\n", transaction_string(now, comments, a, b, u, amount, fields, n_field, matched)
}

# Describe the transaction as a string


# Normal style
# Describe the transaction as a string
function transaction_string(now, comments, a, b, u, amount, fields, n_fields, matched,     i, string) {
  # Print statement
  # This could be a zero, single or double entry transaction
  #
  # # floating point precision
  # float_precision = ternary("" == float_precision, PRECISION, float_precision)

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
      string = string sprintf(", %11.2f", get_cost(matched, now))
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
    return Long_Name[overload_name(account_name)]

  # Special cases exist which could mean this is not an uninitialized long name
  # Is this a new long name or an optional string?
  account_name = parse_optional_string(account_name)
  if ("" == account_name) # An optional string
    return "" # Not an account name

  # Still need to check the account name is in a recognized class
  class_name = get_name_component(account_name, (1))
  assert(class_name ~ /ASSET|EQUITY|EXPENSE|INCOME|LIABILITY|SPECIAL/, "<" account_name "> is not a member of a recognized class")

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
  # Actually let's allow this - but only in very restricted cases
  # the leaves will be distinguished and
  # a reference to an ambiguous leaf will be taken to refer to the
  # most recent definition
  #   This allows dynamic reassigment of leaf pointers
  #   But the accounts' must have consistent classes
  #
  leaf_name = array[2]

  if ((leaf_name in Long_Name)) {
    if (Leaf[Long_Name[leaf_name]] == leaf_name) {
      # If the existing account is new (unused) it can be deleted
      if (("" == first_key(Cost_Basis[Long_Name[leaf_name]])))
        delete Long_Name[leaf_name]
      else {
        #
        # Are the accounts compatible?
        # Parent names must match OR convert X.TERM => X.CURRENT
        # Mapping leaf_name => new leaf_name
        p = Long_Name[leaf_name]
        assert(((p) ~ /^(ASSET|LIABILITY)\.TERM[.:]/) && ((account_name) ~ /^(ASSET|LIABILITY)\.CURRENT[.:]/),
          sprintf("Can't overload %s => %s - can only map TERM => CURRENT", account_name, p))

        # Check to see if  the Show_Account is being overloaded - which is why check was not needed earlier
        if (Show_Account == p)
          Show_Account = account_name

        # Overload the leaf name
        leaf_name = overload_name(leaf_name, 1)
      }
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
  if (((account_name) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)) { # This could include EQUITY too
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
    (( account_name in Number_Parcels)?( 0):(Number_Parcels[ account_name] = ( 0)))

    # End of if ASSET
  } else if (((account_name) ~ ("^" ( "INCOME") "[.:]"))) {
    # Set an Underlying_Asset if the leaf name
    # is of the appropriate format
    #
    # (DIV|DIST|FOR).LEAF => LEAF
    #
    if (((leaf_name) ~ /^(DIV|DIST|FOR)\./)) {
      # Probably a better way to do this using a regex
      linked_name = get_name_component(leaf_name, 2, -1)

      # Call initialize account to ensure this account is initialized
      Underlying_Asset[account_name] = initialize_account(linked_name)


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
  assert(is_open(a, now), sprintf("depreciate_now: Can't depreciate closed asset %s", (Leaf[(a)])))

  # Depreciating assets only use cost elements I or II


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
    if ((Held_Until[(a)][( p)] > ( now))) {
      # First see if depreciation already was computed for time (now)
      if (now in Accounting_Cost[a][p][I]) {
        # Already depreciated

        continue # Get next parcel
      }

      # Now we need the opening value for this parcel - always cost element I
      open_key = find_key(Accounting_Cost[a][p][I], ((now) - 1))
      assert(open_key - Epoch >= 0, sprintf("%s: No earlier depreciation record than %s", (Leaf[(a)]), get_date(now)))

      # The opening value - cost element I
      open_value = get_parcel_element(a, p, I, open_key)



      # Refine factor at parcel level
      if (first_year_factor) {
        # First year sometimes has modified depreciation
        if (((((((__MPX_H_TEMP__ = find_key(Tax_Adjustments[a][ p][ I],  ( now)))?( Tax_Adjustments[a][ p][ I][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Tax_Adjustments[a][ p][ I][0]):( 0)))))) <= Epsilon) && (((((__MPX_H_TEMP__ = find_key(Tax_Adjustments[a][ p][ I],  ( now)))?( Tax_Adjustments[a][ p][ I][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Tax_Adjustments[a][ p][ I][0]):( 0)))))) >= -Epsilon)))
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
      if (!((open_value - delta) >  Epsilon))
        delta = open_value

      # Adjust the parcel cost - element I
      adjust_parcel_cost(a, p, now, -delta, I, TRUE)
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
    if (!(((band_width) <= Epsilon) && ((band_width) >= -Epsilon)))
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
  if ((((tax_left) <= Epsilon) && ((tax_left) >= -Epsilon)))
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
    if ((((tax_left) <= Epsilon) && ((tax_left) >= -Epsilon)))
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

# Get a long account name from a  possibly overloaded leaf name
#
# Overloading
#   This is useful when an account class changes
#   IN practice a very rare eventuality
#
function overload_name(leaf_name, overload) {
  # Overload a name when reqested
  overload = ((overload)?( 1):( 0))

  # If this leaf name is overloaded get the most recent version
  if (leaf_name in Leaf_Count)
    overload += Leaf_Count[leaf_name]

  # Is the name overloaded?
  if (overload) {
    Leaf_Count[leaf_name] = overload
    leaf_name = leaf_name "_" overload
  }

  # Return leaf name
  return leaf_name
}

# urlencode a string
function url_encode(string,     c, chars, url, i) {
  # Get an array holding the characters
  split(string, chars, "")

  # the encoded string
  url = ""
  for (i in chars) {
    c = chars[i]

    # Just append plain vanilla characters
	  if (c ~ /[0-9A-Za-z]/)
	    url = url c
	  else # Get the hex code
	    url = url "%" sprintf("%02X", ((c in URL_Lookup)?( URL_Lookup[c]):( (0))))
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
  hour = (("" == hour)?( (12)):( hour))

  # default is YYYY-MM-DD

  if ("" == date_string) {
    Read_Date_Error = "Empty string"
    return (-1)
  }


  # Split the input date
  if (3 == split(trim(date_string), date_fields, "[-/ ]")) {
    # The fields are YYYY MM DD
    # or             YYYY Mon DD where Mon is a three char month abbreviation or a month name in English
    # or             Mon DD YYYY
    # or             DD MM YYYY if DD & YYYY are inconsistent with dates
    # year-month-day, monthname/day/year, monthname-day-year
    if (month = (((date_fields[1]) in Lookup_Month)?( Lookup_Month[date_fields[1]]):( 0))) {
      day   = date_fields[2] + 0
      year  = date_fields[3] + 0
    } else if (month = (((date_fields[2]) in Lookup_Month)?( Lookup_Month[date_fields[2]]):( 0))) {
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
      return (-1)
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

  delete month_name
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
  # EOFY actions
  # Turn on reporting?
  if (now > Start_Time)
    EOFY = Reports

  # past is referred to now
  past = ((now) + one_year(now, -1))



  # Depreciate everything - at EOFY
  depreciate_all(now)

  # Set EOFY accounts
  # Very careful ordering of get/set actions is required
  # the tax statements adjust costs and so this must be reproducible
  # Therefore affected accounts need to be reset to the input value
  set_cost(ADJUSTMENTS, get_cost(ADJUSTMENTS, ((now) - 1)), now)

  # For the case of ALLOCATED being a separate account
  if (ALLOCATED != ADJUSTMENTS) {
    allocated_profits = get_cost(ALLOCATED, ((now) - 1))
    set_cost(ALLOCATED, allocated_profits, now)
  }
  set_cost(MARKET_CHANGES, sum_market_gains(((now) - 1)), now)

  # Do we need to check for dividend qualification
  if (Qualification_Window)
    print_dividend_qualification(now, past, 1)

  # Realized gains report
  get_capital_gains(now, past, Show_Extra)

  # And deferred gains
  get_deferred_gains(now, past, Show_Extra)

  # Print Market Gains
  print_market_gains(now, past, Show_Extra)

  # Print the depreciation schedule
  print_depreciating_holdings(((now) + 1), past, Show_Extra)

  # We need to compute EOFY statements
  # First the operating statement (income & expenses)
  benefits = print_operating_statement(now, past, 1)

  # Compute the tax due
  @Income_Tax_Function(now, past, benefits)

  # A Super fund must allocate assets to members - this requires account balancing
  @Balance_Profits_Function(now, past, allocated_profits)

  # Print the balance sheet
  print_balance_sheet(now, past, 1)

  # Allocate second element costs associated with fixed assets - at SOFY
  allocate_second_element_costs(now)
}

# Default balance journal is a no-op
function balance_journal(now, past, initial_allocation) {
  return
}


# Gains Reconciliation
# Both Realized & Unrealized Gains
#
function print_gains(now, past, is_detailed, gains_type, gains_stream, sold_time,

                                                            is_realized_flag,
                                                            gains_event, current_price, p, a,
                                                            key,
                                                            description,
                                                            parcel_gains, adjusted_gains,
                                                            held_time,
                                                            label, no_header_printed,

                                                            long_gains, short_gains,
                                                            long_losses, short_losses,
                                                            gains,

                                                            units, units_sold,
                                                            cost, reduced_cost, adjusted_cost,
                                                            parcel_cost, parcel_proceeds,
                                                            proceeds,
                                                            accounting_gains,

                                                            sum_long_gains, sum_long_losses,
                                                            sum_short_gains, sum_short_losses) {

  # Print the gains report
  print Journal_Title > gains_stream
  printf "%s Report for Period Ending %s\n", gains_type, get_date(yesterday(now))  > gains_stream

  # Are we printing out a detailed schedule?
  is_detailed = ((is_detailed)?( is_detailed):( FALSE))

  # A flag to discriminate realized and unrealized gains
  is_realized_flag = ("Realized Gains" == gains_type)

  # A default sold time = the Future
  sold_time = ((sold_time)?( sold_time):( Future))

  # No header printed
  no_header_printed = TRUE

  # Record accounting gains
  accounting_gains = 0
  sum_long_gains = sum_short_gains = sum_long_losses = sum_short_losses = 0 # Tax gains/losses summed here

  # For each asset sold in the current period
  for (a in Leaf)
    if (((a) ~ /^ASSET\.CAPITAL[.:]/) && (is_realized_flag || is_open(a, now))) {
      gains_event = FALSE
      proceeds = cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
      long_gains = short_gains = long_losses = short_losses = 0
      units_sold = 0

      # The price
      if (!is_realized_flag)
        current_price = ((__MPX_H_TEMP__ = find_key(Price[a],  now))?( Price[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Price[a][0]):( 0))))

      # Need to select parcels by sold date
      for (p = 0; p < Number_Parcels[a]; p++ ) {
        if (Held_From[a][p] > now) # All further transactions occured after (now) - parcels are sorted in order bought
          break # All done

        # Check if sold in the (past, now) window (capital gains)
        # or if it is unsold (deferred gains)
        if (((Held_Until[(a)][( p)] <= ( now)) == is_realized_flag) && (Held_Until[(a)][( p)] > ( past))) {
          if (!gains_event) {
            # Two types of header
            if (is_detailed)
              printf "\n%12s %10s %9s %11s %10s %16s %15s %14s %14s %15s %9s %20s %15s\n",
                      "Asset", "Parcel", "Units", "From", "To", "Cost", "Proceeds",
                      "Reduced", "Adjusted", "Accounting", "Type", "Taxable", "Per Unit" > gains_stream
            else if (no_header_printed)
              printf "%12s %12s %12s %15s %14s %14s %15s %9s %20s\n",
                     "Asset", "Units", "Cost",
                     "Proceeds", "Reduced", "Adjusted", "Accounting", "Type", "Taxable" > gains_stream

            # print Name
            label = (Leaf[(a)])
            gains_event = TRUE
            no_header_printed = FALSE
          }

          # Keep track
          units = Units_Held[a][p]
          units_sold += units
          if (is_realized_flag)
            held_time = get_held_time(Held_Until[a][p], Held_From[a][p])
          else
            held_time = get_held_time(sold_time, Held_From[a][p])

          reduced_cost  += get_parcel_cost(a, p, now)
          adjusted_cost += get_parcel_cost(a, p, now, TRUE)

          # cash in and out
          parcel_cost     =   get_cash_in(a, p, now)
          if (is_realized_flag)
            parcel_proceeds = - get_cash_out(a, p, now)
          else
            parcel_proceeds = current_price * Units_Held[a][p]

          cost           += parcel_cost
          proceeds       += parcel_proceeds

          # Total gains (accounting gains)
          gains = sum_cost_elements(Accounting_Cost[a][p], now)
          if (!is_realized_flag) # This is not sold yet
            gains -=  parcel_proceeds

          # Keep track of accounting gains
          accounting_gains += gains

          # We want taxable gains
          # Gains are relative to adjusted cost
          # Losses are relative to reduced cost (so equal accounting losses)
          if (((gains) >  Epsilon)) {
            # These are losses
            parcel_gains = gains
            if (held_time >= 31622400) {
              description = "Long Losses "
              long_losses += gains
              sum_long_losses += gains
            } else {
              description = "Short Losses"
              short_losses += gains
              sum_short_losses += gains
            }
          } else {
            # Assume zero losses or gains
            parcel_gains = 0

            # Taxable gains
            description = "Zero Gains  "
          }

          # after application of tax adjustments
          # If there were losses then parcel_gains will be above zero
          adjusted_gains = gains - sum_cost_elements(Tax_Adjustments[a][p], now)
          if (((adjusted_gains) < -Epsilon)) {
            # Adjustments are negative and reduce taxable gains
            parcel_gains = adjusted_gains
            if (held_time >= 31622400) {
              description = "Long Gains  "
              long_gains += parcel_gains
              sum_long_gains += parcel_gains

            } else {
              description = "Short Gains "
              short_gains += parcel_gains
              sum_short_gains += parcel_gains
            }
          }

          # Print out the parcel information
          if (is_detailed) {
            # If printing out in detail
            printf "%13s %7d %12.3f [%11s, %11s] %14s %14s %14s %14s %14s %14s %15s %15s\n",
              label, p, units, get_date(Held_From[a][p]), get_date(Held_From[a][p] + held_time),
                 print_cash(parcel_cost),
                 print_cash(parcel_proceeds),
                 print_cash(get_parcel_cost(a, p, now)),
                 print_cash(get_parcel_cost(a, p, now, TRUE)),
                 print_cash(- gains),
                 description,
                 print_cash(- parcel_gains),
                 print_cash(- parcel_gains / units, 4) > gains_stream

            # Clear label
            label = ""
          }
        }
      } # End of each parcel p

      # Show any gains event
      if (gains_event) {
        if (is_detailed) {
          # Detailed format
          print_underline(157, 0, gains_stream)
          printf "%13s %20.3f %41s ",
            label, units_sold,
            print_cash(cost) > gains_stream
        } else
          printf "%13s %12.3f %14s ",
            label, units_sold,
            print_cash(cost) > gains_stream

        # Stack the gains & losses
        if ((((long_gains) > Epsilon) || ((long_gains) < -Epsilon)))
          Gains_Stack[Long_Gains_Key]   = long_gains
        if ((((long_losses) > Epsilon) || ((long_losses) < -Epsilon)))
          Gains_Stack[Long_Losses_Key]  = long_losses
        if ((((short_gains) > Epsilon) || ((short_gains) < -Epsilon)))
          Gains_Stack[Short_Gains_Key]  = short_gains
        if ((((short_losses) > Epsilon) || ((short_losses) < -Epsilon)))
          Gains_Stack[Short_Losses_Key] = short_losses

        # Common entries
        for (key in Gains_Stack)
          break
        if (key) {
          printf "%14s %14s %14s %14s %14s %15s",
            print_cash(proceeds),
            print_cash(reduced_cost), print_cash(adjusted_cost),
            print_cash(proceeds - reduced_cost),
            key, print_cash(- Gains_Stack[key]) > gains_stream
          delete Gains_Stack[key]
        } else
          printf "%14s %14s %14s %15s",
            print_cash(proceeds),
            print_cash(reduced_cost), print_cash(adjusted_cost),
            print_cash(proceeds - reduced_cost) > gains_stream

        # Extra entries
        for (key in Gains_Stack)
          printf "\n%*s %14s", 116 + 35 * is_detailed, key, print_cash(- Gains_Stack[key]) > gains_stream

        printf "\n" > gains_stream
        delete Gains_Stack[key]
      } # End of gains event
    } # End of each asset

  # Stack the gains & losses
  Gains_Stack[Long_Gains_Key]   = sum_long_gains
  Gains_Stack[Long_Losses_Key]  = sum_long_losses
  Gains_Stack[Short_Gains_Key]  = sum_short_gains
  Gains_Stack[Short_Losses_Key] = sum_short_losses

  return accounting_gains
} # End of print gains


# Compute capital gains and losses
function get_capital_gains(now, past, is_detailed,

                                gains_stream,
                                accounting_gains,
                                cgt_total_gains,
                                cgt_short_gains, cgt_long_gains,
                                cgt_losses,
                                cgt_short_losses, cgt_long_losses,
                                cgt_total_losses,
                                tax_refund) {


    # The gains_stream is the pipe to write the schedule out to
    gains_stream = ("" == EOFY) ? "/dev/null" : EOFY

    # First print the gains out in detail when required
    if ("/dev/null" != gains_stream) {
      print_gains(now, past, is_detailed, "Realized Gains", gains_stream)
      delete Gains_Stack
    }

    # Print the capital gains schedule
    print Journal_Title > gains_stream
    printf "Capital Gains Report for Period Ending %s\n\n", get_date(yesterday(now))  > gains_stream

    # Get total capital gains
    # Exploit existing sums
    # taxable capital gains are messy
    print_underline(43, 0, gains_stream)

    # First the cgt gains & losses
    #
    # Total Gains
    cgt_total_gains = get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past)

    # The realized capital losses
    # The Australian system doesn't discriminate between LONG & SHORT losses
    cgt_total_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)

    # Would need
    # Now compute the accounting gains
    accounting_gains = cgt_total_gains + cgt_total_losses

    # Print Capital Gains
    printf "\t%27s => %14s\n", "Accounting Capital Gains", print_cash(- accounting_gains) > gains_stream
    printf "\t%27s => %14s\n", "Total Capital Gains", print_cash(- cgt_total_gains) > gains_stream

    # The taxable long & short gains
    # The long gains first
    cgt_long_gains  = get_cost(LONG_GAINS, now) - get_cost(LONG_GAINS, past)
    cgt_long_losses = get_cost(LONG_LOSSES, now) - get_cost(LONG_LOSSES, past)

    # short gains & losses
    cgt_short_gains   = get_cost(SHORT_GAINS, now) - get_cost(SHORT_GAINS, past)
    cgt_short_losses  = get_cost(SHORT_LOSSES, now) - get_cost(SHORT_LOSSES, past)

    # If there are other income gains (eg from distributions etc)
    # then the taxable gains will need adjustment
    cgt_long_gains += get_cost(INCOME_LONG, now) - get_cost(INCOME_LONG, past)
    cgt_short_gains += get_cost(INCOME_SHORT, now) - get_cost(INCOME_SHORT, past)

    # The taxable gains and losses
    printf "\t%27s => %14s\n", "Long Capital Gains", print_cash(- cgt_long_gains) > gains_stream
    printf "\t%27s => %14s\n\n", "Short Capital Gains", print_cash(- cgt_short_gains) > gains_stream

    # Now consider the losses
    # Need to consider a maximum loss window beyond which losses will not be carried
    # Also are carried losses treated as LONG or SHORT? FIXME
    # FIXME
    # FIXME
    cgt_losses = get_cost(CAPITAL_LOSSES, ((now) - 1)) - get_cost(CAPITAL_LOSSES, (((""))?( ((now) - (""))):( Epoch)))
    if ((""))
      printf "\t%27s => %14s\n", "Losses Carried Forward Since", get_date((((""))?( ((now) - (""))):( Epoch))) > gains_stream
    if (!(((cgt_losses) <= Epsilon) && ((cgt_losses) >= -Epsilon)))
      printf "\t%27s => %14s\n", "Carried Capital Losses", print_cash(cgt_losses) > gains_stream

    # Finally the losses
    printf "\t%27s => %14s\n", "New Capital Losses", print_cash(cgt_total_losses) > gains_stream
    printf "\t%27s => %14s\n", "Total Capital Losses", print_cash(cgt_losses + cgt_short_losses + cgt_long_losses) > gains_stream
    printf "\t%27s => %14s\n", "Long Capital Losses", print_cash(cgt_long_losses) > gains_stream
    printf "\t%27s => %14s\n\n", "Short Capital Losses", print_cash(cgt_short_losses) > gains_stream

    # Get the taxable gains
    cgt_losses = get_taxable_gains(now, gains_stream,
                                   cgt_long_gains, cgt_long_losses, TAXABLE_LONG,
                                   cgt_short_gains, cgt_short_losses, TAXABLE_SHORT,
                                   cgt_losses)

    # Losses might sometimes be written back against earlier gains
    if (("") && !(((cgt_losses) <= Epsilon) && ((cgt_losses) >= -Epsilon))) {
      # Try writing back losses
      printf "\n\t%27s => %14s\n", "Write Back Losses Available", print_cash(cgt_losses) > gains_stream

      # Rewrite refundable offsets to just before now so they can be zeroed later at a distinct timestamp
      cgt_losses = write_back_losses(((now) - 1), ((now) + one_year(now, -1)), (((""))?( ((now) - (""))):( Epoch)), cgt_losses, gains_stream)
    }

    # All done
    print_underline(43, 0, gains_stream)
    print "\n" > gains_stream

    # Save losses and taxable gains
    set_cost(CAPITAL_LOSSES, cgt_losses, now)
}



# Compute the deferred gains
# And print out a schedule
#
function get_deferred_gains(now, past, is_detailed,       accounting_gains, gains_stream,
                                                          gains, losses) {

 # The gains_stream is the pipe to write the schedule out to
 gains_stream = ("" == EOFY) ? "/dev/null" : EOFY

 # First print the gains out in detail
 accounting_gains = print_gains(now, past, is_detailed, "Deferred Gains", gains_stream)
 losses = Gains_Stack[Long_Losses_Key]
 gains  = Gains_Stack[Long_Gains_Key]
 delete Gains_Stack

 # Print the deferred gains report
 print Journal_Title > gains_stream
 printf "Deferred Gains Report for Period Ending %s\n\n", get_date(yesterday(now))  > gains_stream

 # Print Capital Gains & Losses
 print_underline(43, 0, gains_stream)

 printf "\t%27s => %14s\n", "Accounting Deferred Gains", print_cash(- accounting_gains) > gains_stream
 printf "\t%27s => %14s\n", "Taxable Deferred Gains",
                            print_cash(- gains) > gains_stream
 printf "\t%27s => %14s\n", "Taxable Deferred Losses",
                            print_cash(losses) > gains_stream

 printf "\nAfter Application of Any Losses\n" > gains_stream

 # Get the deferred taxable gains
 get_taxable_gains(now, gains_stream, gains, losses, DEFERRED_GAINS)
} # End of deferred gains



# Print out operating statement
function print_operating_statement(now, past, is_detailed,     benefits, losses,
                                                               gains, market_gains,
                                                               more_past, label, x) {

  # Set arguments
  more_past = ((past) + one_year(past, -1))
  is_detailed = ("" == is_detailed) ? 1 : 2

  print Journal_Title > EOFY
  if (is_detailed)
    printf "Detailed Operating Statement\n" > EOFY
  else
    printf "Operating Statement\n" > EOFY
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > EOFY

  print_underline(80, 1, EOFY)
  printf "\n\n" > EOFY

  # We start with the investment income
  label = sprintf("Income\nInvestment Income %33s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC))

  # Exclude contributions
  label = print_account_class(label, "block_class", "INCOME", "INCOME.CONTRIBUTION", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Obtain the income per year
  benefits[now]  = - (get_cost("*INCOME", now) - (x = get_cost("*INCOME", past)))
  benefits[past] = - (x - get_cost("*INCOME", more_past))

  # Now the Contributions
  label = sprintf("\nContributions\n")
  print_account_class(label, "select_class", "INCOME.CONTRIBUTION", "", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Print a running total
  print_line(past, EOFY)

  # Print grand total income
  printf "\t%22s %23s", "Total Income", print_cash(benefits[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(benefits[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }

  # the unrealized gains
  label = sprintf("\n\nInvestment Gains\nUnrealized Gains in Market Value\n")
  print_account_class(label, "select_class", "ASSET.CAPITAL", "", "get_unrealized_gains", now, past, past, more_past, is_detailed, -1) # Block Depreciating Assets

  # Obtain the market gains per year
  market_gains[now]  = - (get_cost(MARKET_CHANGES, now) - (x = get_cost(MARKET_CHANGES, past)))
  market_gains[past] = - (x - get_cost(MARKET_CHANGES, more_past))

  # Print the total unrealized gains
  print_line(past, EOFY)

  # Print any unrealized gains
  if (((market_gains[now]) >  Epsilon) || ((market_gains[past]) >  Epsilon)) {
    printf "\t%22s %23s", "Total Market Gains", print_cash((((market_gains[now]) >   Epsilon)?( (market_gains[now])):(  ""))) > EOFY
    if (past)
      printf " %26s\n", print_cash((((market_gains[past]) >   Epsilon)?( (market_gains[past])):(  ""))) > EOFY
    else
      printf "\n" > EOFY
    benefits[now]  += (((market_gains[now]) >   Epsilon)?( (market_gains[now])):(  0))
    benefits[past] += (((market_gains[past]) >   Epsilon)?( (market_gains[past])):(  0))
  }

  # Print a grand total
  print_line(past, EOFY)

  # Print grand total income
  printf "\t%22s %23s", "Total of All Income", print_cash(benefits[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(benefits[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n" > EOFY

  # Now print expenses... (exclude tax payments)
  label = sprintf("Expenses\nGeneral Expenses\n")
  label = print_account_class(label, "block_class", "EXPENSE", "EXPENSE.UNREALIZED", "get_cost", now, past, past, more_past, is_detailed) > EOFY

  # Need to correct for market gains captured as expenses
  losses[now]  = market_gains[now]  + get_cost("*EXPENSE", now) - (x = get_cost("*EXPENSE", past))
  losses[past] = market_gains[past] + x - get_cost("*EXPENSE", more_past)

  # Print a total
  print_line(past, EOFY)

  # Print grand total income
  printf "\t%22s %23s", "Total Expenses", print_cash(losses[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(losses[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n\n" > EOFY

  # Print any unrealized losses
  if (((market_gains[now]) < -Epsilon) || ((market_gains[past]) < -Epsilon)) {
    printf "\t%22s %23s", "Total Market Losses", print_cash((((market_gains[now]) < - Epsilon)?( (market_gains[now])):(  ""))) > EOFY
    if (past)
      printf " %26s\n", print_cash((((market_gains[past]) < - Epsilon)?( (market_gains[past])):(  ""))) > EOFY
    else
      printf "\n" > EOFY
    losses[now]  -= (((market_gains[now]) < - Epsilon)?( (market_gains[now])):(  0))
    losses[past] -= (((market_gains[past]) < - Epsilon)?( (market_gains[past])):(  0))
  }
  # Print a total
  print_line(past, EOFY)

  # Print grand total expenses
  printf "\t%22s %23s", "Total of All Expenses", print_cash(losses[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(losses[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n\n" > EOFY

  # Print Before Tax benefits
  benefits[now]  -= losses[now]
  benefits[past] -= losses[past]
  printf "\t%27s %18s", "Benefits Accrued Before Tax", print_cash(benefits[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(benefits[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n\n" > EOFY

  # If detailed print tax credits
  label = sprintf("Appendix\n\nTax Offsets\n")
  label = print_account_class(label, "select_class", "SPECIAL.OFFSET", "", "get_cost", now, past, past, more_past, is_detailed, -1) > EOFY

  # Print a nice line
  if (!label) {
    print_underline(72, 0, EOFY)
    x = get_cost("*SPECIAL.OFFSET", past)
    printf "\t%24s%22s %26s\n\n", "Total Tax Offsets",
              print_cash(x - get_cost("*SPECIAL.OFFSET", now)),
              print_cash(get_cost("*SPECIAL.OFFSET", more_past) - x) > EOFY
  }

  printf "\n\n\n" > EOFY

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
  reports_stream = ("" == EOFY) ? "/dev/null" : EOFY

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
  print_underline(80, 1, reports_stream)
  printf "%53s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC) > reports_stream
  printf "%53s %26s\n", "$", "$" > reports_stream

  # We start with the current assets (cash)
  label = sprintf("Current Assets\n")
  label = print_account_class(label, "select_class", "ASSET.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Term assets are current if they mature within one year
  current_assets[now]  = get_cost("*ASSET.CURRENT", now)
  current_assets[past] = get_cost("*ASSET.CURRENT", past)

  # Print a nice line
  print_underline(72, 0, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Current Assets",
          print_cash(current_assets[now]), print_cash(current_assets[past]) > reports_stream

  # Now the non-current assets
  label = sprintf("Non-Current Assets\n")
  label = print_account_class(label, "block_class", "ASSET", "ASSET.CURRENT", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Here we need to adjust for accounting gains & losses
  assets[now]  =  get_cost("*ASSET", now)  - get_cost("*INCOME.GAINS.REALIZED", now)  - get_cost("*EXPENSE.LOSSES.REALIZED", now)  - get_cost(MARKET_CHANGES, now)
  assets[past] =  get_cost("*ASSET", past) - get_cost("*INCOME.GAINS.REALIZED", past) - get_cost("*EXPENSE.LOSSES.REALIZED", past) - get_cost(MARKET_CHANGES, past)

  # Print a nice line
  print_underline(72, 0, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Non–Current Assets", print_cash(assets[now] - current_assets[now]), print_cash(assets[past] - current_assets[past]) > reports_stream

  # Print Total Assets
  print_underline(72, 0, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Assets", print_cash(assets[now]), print_cash(assets[past]) > reports_stream

  # Treat tax payments/refunds as liabilities
  label = sprintf("Tax Liabilities\n")
  label = print_account_class(label, "select_class", "LIABILITY.TAX", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # We start with the current liabilities
  label = sprintf("Current Liabilities\n")
  label = print_account_class(label, "select_class", "LIABILITY.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)
  current_liabilities[now]   = -(get_cost("*LIABILITY.CURRENT", now ) + get_cost("*LIABILITY.TAX", now))
  current_liabilities[past]  = -(get_cost("*LIABILITY.CURRENT", past) + get_cost("*LIABILITY.TAX", past))

  # Print a nice line
  if (!label) {
    print_underline(72, 0, reports_stream)
    printf "\t%24s%21s %26s\n\n", "Total Current Liabilities",
              print_cash(current_liabilities[now]), print_cash(current_liabilities[past]) > reports_stream
  }

  # Need non-current Liabilities
  label = sprintf("Non-Current Liabilities\n")

  # Now the remaining non current liabilities
  class_list["LIABILITY.CURRENT"] = TRUE
  class_list["LIABILITY.MEMBER"] = TRUE
  class_list["LIABILITY.TAX"] = TRUE
  label = print_account_class(label, "block_class_list", "LIABILITY", class_list, "get_cost", now, Epoch, past, Epoch, is_detailed, -1, 2)
  liabilities[now]  = - get_cost("*LIABILITY", now)
  liabilities[past] = - get_cost("*LIABILITY", past)
  delete class_list

  # Print Member Liabilities
  label = print_account_class(label, "select_class", "LIABILITY.MEMBER", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # Print a nice line
  if (!label) {
    print_underline(72, 0, reports_stream)
    printf "\t%27s %18s %26s\n", "Total Long Term Liabilities",
      print_cash(liabilities[now] - current_liabilities[now]), print_cash(liabilities[past] - current_liabilities[past]) > reports_stream
  }

  print_underline(72, 0, reports_stream)
  print_underline(72, 0, reports_stream)
  printf "\t%27s %18s %26s\n\n", "Total Liabilities",
    print_cash(liabilities[now]), print_cash(liabilities[past]) > reports_stream

  # Now find total Equity
  label = sprintf("Share Equity\n")
  label = print_account_class(label, "select_class", "EQUITY", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # the equity
  equity[now]  = - get_cost("*EQUITY", now)
  equity[past] = - get_cost("*EQUITY", past)

  # Print a nice line
  if (!label) {
    print_underline(72, 0, reports_stream)
    printf "\t%24s %21s %26s\n\n", "Total Equity",
                print_cash(equity[now]), print_cash(equity[past]) > reports_stream
  }

  # Print Accumulated Profits (INCOME - EXPENSES) == (ASSETS - LIABILITY - EQUITY)
  print_underline(72, 0, reports_stream)
  printf "\t%24s %21s %26s\n", "Accumulated Profits",
    print_cash(assets[now] - liabilities[now] - equity[now]), print_cash(assets[past] - liabilities[past] - equity[past]) > reports_stream
  print_underline(72, 0, reports_stream)

  # Tidy up
  delete assets
  delete liabilities
  delete equity
}

# Print the holdings at time now
function print_market_gains(now, past, is_detailed,    gains_stream) {
  # Show current gains/losses
   # The gains_stream is the pipe to write the schedule out to
   gains_stream = ("" == EOFY) ? "/dev/null" : EOFY

   # First print the gains out in detail
   if ("/dev/null" != gains_stream) {
     print_gains(now, past, is_detailed, "Market Gains", gains_stream, now)
     delete Gains_Stack
  }
}

# Compute annual depreciation
function depreciate_all(now,      a, current_depreciation, comments) {
  # Depreciation is Cost Element I
  comments = "Automatic EOFY Depreciation"
  Cost_Element = I

  # Depreciate all open fixed assets
  for (a in Leaf)
    if (((a) ~ /^ASSET\.FIXED[.:]/) && is_open(a, now)) {
      # Depreciate
      current_depreciation = depreciate_now(a, now)
      update_cost(a, - current_depreciation, now)

      # Balance accounts
      adjust_cost(DEPRECIATION, current_depreciation, now)

      # Print the transaction
      print_transaction(now, comments, a, DEPRECIATION, "(D)", current_depreciation)
    }

  # Restore defaults
  Cost_Element = COST_ELEMENT
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
        if ((Held_Until[(a)][( p)] > ( now))) {
          # Debugging


          # Get the second element of the cost
          second_element = get_parcel_element(a, p, II, now)
          if (!(((second_element) <= Epsilon) && ((second_element) >= -Epsilon))) {
            # The Second Element Cost is applied to the First Element
            adjust_parcel_cost(a, p, now,   second_element,  I, FALSE)
            adjust_parcel_cost(a, p, now, - second_element, II, FALSE)
          }


        } # End of if unsold parcel
      } # End of each parcel


    } # End of each fixed asset a
}


# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      reports_stream, a, p, open_key, close_key, parcel_depreciation, account_depreciation, open_cost, total_depreciaiton, sum_open,
                                                                  sale_depreciation, sale_appreciation, sum_adjusted) {

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = ("" == EOFY) ? "/dev/null" : EOFY

  # Return if nothing to do
  if ("/dev/null" == reports_stream)
    return

  is_detailed = ("" == is_detailed) ? FALSE : is_detailed
  total_depreciation = ""

  # Print out the assets in alphabetical order
  for (a in Leaf)
    if (((a) ~ /^ASSET\.FIXED[.:]/) && (is_open(a, now) || is_open(a, past))) {

      if ("" == total_depreciation) {
        printf "\n" > reports_stream
        print Journal_Title > reports_stream
        printf "Depreciation Schedule for the Period [%11s, %11s]\n", get_date(past), get_date(now) > reports_stream
        total_depreciation = 0 # Total value summed here
      }

      # The opening value of an asset with multiple parcels cannot be tied to a single time
      account_depreciation = sum_open = 0

      # Get each parcel
      printf "%10s %15s ", Depreciation_Method[Method_Name[a]], (Leaf[(a)]) > reports_stream
      for (p = 0; p < Number_Parcels[a]; p ++) {
        # When was this parcel  opened?
        open_key = Held_From[a][p] # First parcel opened here
        if (open_key < past)
          open_key = past # This must be earlier than now for this asset to be open and considered

        # Is there is a problem if item is sold exactly at same time as depreciation occurs? (no if done carefully)
        if ((Held_Until[(a)][( p)] <= ( now))) {
          close_key = ((Held_Until[a][p]) - 1)
        } else
          close_key = ((now) - 1)

        # parcel open cost
        open_cost = get_parcel_cost(a, p, open_key)
        sum_open += open_cost

        # Always get the parcel depreciation
        parcel_depreciation = (((__MPX_H_TEMP__ = find_key(Tax_Adjustments[a][ p][ I],  ( open_key)))?( Tax_Adjustments[a][ p][ I][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Tax_Adjustments[a][ p][ I][0]):( 0))))) - (((__MPX_H_TEMP__ = find_key(Tax_Adjustments[a][ p][ I],  ( close_key)))?( Tax_Adjustments[a][ p][ I][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Tax_Adjustments[a][ p][ I][0]):( 0)))))

        # Record detailed statement
        # Is this a named parcel?
        if (is_detailed) {
          if ((( a in Parcel_Tag) && ( p in Parcel_Tag[ a])))
            printf "\n%20s %5d ", Parcel_Tag[a][p], p > reports_stream
          else
            printf "\n%26d ", p > reports_stream

          # Depreciation is the sum of the I tax adjustments
          printf "[%11s, %11s] Opening => %14s Closing => %14s Second Element => %14s Adjusted => %14s Depreciation => %14s",
                    get_date(open_key), get_date(close_key), print_cash(open_cost),
                    print_cash(open_cost - delta),
                    print_cash(get_parcel_element(a, p, II, close_key)),
                    print_cash(get_parcel_cost(a, p, close_key)),
                    print_cash(parcel_depreciation) > reports_stream
        } # End of is_detailed

        #  Just track the total depreciation
        account_depreciation   += parcel_depreciation
      } # End of each parcel

      # Clean up output
      if (is_detailed) {
        printf "\n" > reports_stream
        print_underline(197, 0, reports_stream)
        printf "%26s ", (Leaf[(a)]) > reports_stream
      }

      # Depreciation is the sum of the tax adjustments
      # When was this asset opened?
      open_key = Held_From[a][0] # First parcel opened here
      if (open_key < past)
        open_key = past # This must be less than now for this asset to be open and considered
      if ((!is_open((a), ( now))))
        close_key = ((held_to(a, now)) - 1)
      else
        close_key = ((now) - 1)

      # For depreciating assets depreciation corresponds to the tax adjustments
      # Period depreciation is the difference in the tax adjustments
      printf "[%11s, %11s] Opening => %14s Closing => %14s Second Element => %14s Adjusted => %14s Depreciation => %14s\n",
        get_date(open_key), get_date(close_key), print_cash(sum_open),
        print_cash(sum_open - delta),
        print_cash(get_cost_element(a, II, close_key)),
        print_cash(get_cost(a, close_key)),
        print_cash(account_depreciation) > reports_stream

      # Track total depreciation too
      total_depreciation += account_depreciation
    } # End of a depreciating asset

  # Is there any depreciation/appreciation due to the sale of depreciating assets?
  sale_appreciation = get_cost(SOLD_APPRECIATION, now) - get_cost(SOLD_APPRECIATION, past)
  sale_depreciation = get_cost(SOLD_DEPRECIATION, now) - get_cost(SOLD_DEPRECIATION, past)
  if (!(((sale_depreciation) <= Epsilon) && ((sale_depreciation) >= -Epsilon)))
    printf "\n\tDepreciation from Sales => %14s\n", print_cash(sale_depreciation) > reports_stream
  if (!(((sale_appreciation) <= Epsilon) && ((sale_appreciation) >= -Epsilon)))
    printf "\n\tAppreciation from Sales => %14s\n", print_cash(-sale_appreciation) > reports_stream
  total_depreciation += sale_depreciation + sale_appreciation

  # Print a nice line
  if (!(((total_depreciation) <= Epsilon) && ((total_depreciation) >= -Epsilon))) {
    print_underline(197, 0, reports_stream)
    printf "\tPeriod Depreciation     => %14s\n", print_cash(total_depreciation) > reports_stream
  }
} # End of print depreciating holdings

#
#
## Dividend Qualification Function
##
## Compute whether dividends are qualified or not
function print_dividend_qualification(now, past, is_detailed,

                                         a, underlying_asset, credit_account,
                                         qualifying_date,
                                         qualified_units, total_units, qualified_fraction, q,
                                         key, next_key, payment,
                                         print_header) {

  ## Output Stream => Dividend_Report

  # For each dividend in the previous financial
  print Journal_Title > EOFY
  if (is_detailed)
    printf "Detailed Dividend Qualification Report\n" > EOFY
  else
    printf "Dividend Qualification Report\n" > EOFY
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > EOFY

  # A header
  print_header = TRUE

  # Get each dividend/distribution
  # Start with dividends - this could be abstracted out later to include distributions
  # First key
  for (a in Leaf)
    if (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION.CLOSELY_HELD") "[.:]"))) { # Only closely held distributions are considered

      # Get each payment in the target period
      key = (find_key(Cost_Basis[a],  now))

      while (key > past) {
        # A heading
        if (print_header) {
          printf "\n\n%22s\n", "Dividends" > EOFY
          print_header = FALSE
        }

        # The current asset
        assert(a in Underlying_Asset, "Can't find underlying asset for %s" Leaf[a]) > EOFY
        underlying_asset = Underlying_Asset[a]

        # We will need the next key
        next_key = (find_key(Cost_Basis[a], (( key) - 1)))

        # Short cut directly to the value of the dividend payment
        payment = - (get_cost(a,  key) - get_cost(a, (( key) - 1)))

        # The qualifying date is one day before the ex-dividend date
        qualifying_date = ((yesterday(get_exdividend_date(underlying_asset, key), (12))) + 1)

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > 0, sprintf("%s: %s <%s>",  Leaf[a], Read_Date_Error, get_date(key)))

        # These are the units that were qualified on the qualifying date
        qualified_units = ((Qualification_Window)?(  ((__MPX_H_TEMP__ = find_key(Qualified_Units[underlying_asset],   qualifying_date))?( Qualified_Units[underlying_asset][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Qualified_Units[underlying_asset][0]):( 0))))):( ((__MPX_H_TEMP__ = find_key(Total_Units[underlying_asset],    qualifying_date))?( Total_Units[underlying_asset][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[underlying_asset][0]):( 0))))))

        # Now get the total units
        total_units = ((__MPX_H_TEMP__ = find_key(Total_Units[underlying_asset],   qualifying_date))?( Total_Units[underlying_asset][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[underlying_asset][0]):( 0))))

        # If not all units are qualified need to check the second half of the Qualification Window
        if (!(((total_units - qualified_units) <= Epsilon) && ((total_units - qualified_units) >= -Epsilon))) {
          q = maximum_entry(Qualified_Units[underlying_asset], qualifying_date, qualifying_date + 0.5 * Qualification_Window)
          qualified_units = (((q)>( qualified_units))?(q):( qualified_units))
          qualified_fraction = qualified_units / total_units

          # Should never be greater than unity
          assert(!((qualified_fraction - 1.0) >  Epsilon), sprintf("Qualified Units[%s] => %.3f > Units held on qualification date <%s>",
            underlying_asset, qualified_units, total_units))
        } else
          qualified_fraction = 1.0

        # The output - show ex-dividend date not qualifying date
        printf "\t%22s %11s %11s %14s %7.5f\n", Leaf[underlying_asset], get_date(key), get_date(qualifying_date + (86400)), print_cash(payment), qualified_fraction > EOFY

        # Make the appropriate changes for the current tax jurisdiction
        @Dividend_Qualification_Function(a, underlying_asset, key, 1.0 - qualified_fraction)

        # Get the next key
        key = next_key
      } # End of while each key in the window
    } # End of if a dividend
} # End of function print_dividend_qualification

# Module for printing out parcel capital gains
# FIXME almost redundant
function print_parcel_gain(a, p, now, current_price, cgt_schedule,
                             paid, description, units, tax_gains, gains, held_time, parcel_adjustments) {
  # Default values
  cgt_schedule = ("" == cgt_schedule) ? "/dev/stdout" : cgt_schedule
  current_price = ("" == current_price) ? ((__MPX_H_TEMP__ = find_key(Price[a],  now))?( Price[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Price[a][0]):( 0)))) : current_price

  # Keep track
  gains = sum_cost_elements(Accounting_Cost[a][p], now)

  # Held
  held_time = get_held_time(now, Held_From[a][p])
  units = Units_Held[a][p]
  paid  = current_price * units
  gains -= paid
  parcel_adjustments = sum_cost_elements(Tax_Adjustments[a][p], now)

  # We want taxable gains
  if (gains < parcel_adjustments - Epsilon) { # This is a capital gain cash_in < cash_out
    tax_gains = gains - parcel_adjustments
    if (held_time  >= 31622400)
      description = "Long Gain    "
    else
      description = "Short Gain   "
  } else {
    tax_gains = 0
    if (gains > Epsilon)
      description = "Taxable Loss "
    else
      description = "Zero Gain    "
  }

  # Complicated logic for layout
  # Top line has accounting losses or gains if they differ from taxable losses or gains
  printf "\t%6d Units => %10.3f Held => [%11s, %11s] Cost => %14s Value => %14s Reduced => %14s",
    p, units, get_date(Held_From[a][p]), get_date(Held_From[a][p] + held_time), print_cash(get_cash_in(a, p, now)),
       print_cash(paid), print_cash(get_parcel_cost(a, p, now)) > cgt_schedule
  if ((((parcel_adjustments) <= Epsilon) && ((parcel_adjustments) >= -Epsilon))) {
    printf " %15s => %14s Per Unit => %14s", description, print_cash(tax_gains < 0 ? - tax_gains : gains), print_cash(get_parcel_cost(a, p, now) / units, 4) > cgt_schedule
    if ((( a in Parcel_Tag) && ( p in Parcel_Tag[ a])))
      printf "%20s\n", Parcel_Tag[a][p], p > EOFY
    else
      printf "\n" > EOFY
  } else {
    # Next line has adjusted cost and tax gains or losses
    if ((((tax_gains) <= Epsilon) && ((tax_gains) >= -Epsilon)) && gains < 0) {
      # Zero tax gains
      description = "Zero Gain    "
      gains = 0
    }

    # The accounting gain/loss is simple
    printf " %15s => %14s\n", "Accounting Gain", print_cash(- gains) > cgt_schedule
    printf "\t%117s => %13s %15s => %14s\n", "Adjusted", print_cash(get_parcel_cost(a, p, now, TRUE)), description, print_cash(tax_gains < 0 ? -tax_gains : gains) > cgt_schedule
  }
}

## Helper functions


# This is the newer more compact form of this function
# But it is still carrying a lot of remnant complications
# Replace class_name, blocked_class with a
# selector function
function print_account_class(heading, selector, class_name, blocked_class, income_function, now, now_past, past, past_past, print_all, sign,
  subclass, last_subclass,
  x, account_income, account_sum, did_print) {

  # Bail out when nothing to print
  if ("/dev/null" == EOFY)
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
    if (@selector(x, class_name, blocked_class)) {

      # The required name component is the last in the parent - watch out for
      # the leading "*" if only a single component
      subclass = get_name_component(Parent_Name[x], 0)

      # Initialize sums
      if (last_subclass != subclass) {
        # If at least one entry found print a summary
        if (did_print) {
          if (print_all > 1) {
            if (past)
              print_underline(72, 0, EOFY)
            else
              print_underline(46, 0, EOFY)
          }
          if (print_all)
            print_subclass_sum(last_subclass, account_sum[now],
                                              account_sum[past])
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
      if (past) {
        if ("get_unrealized_gains" == income_function && (!is_open((x), ( past))))
          account_income[past] = 0
        else {
          account_income[past] = sign * (@income_function(x, past) - @income_function(x, past_past))
          account_sum[past] += account_income[past]
        }
      }

      if (!(((account_income[now]) <= Epsilon) && ((account_income[now]) >= -Epsilon)) || !(((account_income[past]) <= Epsilon) && ((account_income[past]) >= -Epsilon))) {
        # Only print the heading if there was a non-zero entry
        if (0 == did_print) {
          if ("" != heading) {
            printf heading > EOFY

            # Only print out once
            heading = ""
          }
          if (print_all > 1)
            printf "%22s\n", subclass > EOFY

          # The heading has been printed out
          did_print = 1
        }

        if (print_all > 1) {
          printf "\t%24s %21s", (Leaf[(x)]), print_cash(account_income[now]) > EOFY
          if (past)
            printf " %26s", print_cash(account_income[past]) > EOFY
          printf "\n"> EOFY
        }
      }
    }
  } # End of each Leaf

  # Print a nice line (careful here when no subclasses found!)
  if (did_print) {
    if (print_all > 1) {
      if (past)
        print_underline(72, 0, EOFY)
      else
        print_underline(46, 0, EOFY)
    }
    if (print_all)
      print_subclass_sum(subclass, account_sum[now], account_sum[past])
  }

  # return heading
  return heading
}

function print_subclass_sum(name, sum_now, sum_past) {
  printf "\t%24s %21s", substr(name, 1, 1) tolower(substr(name, 2)), print_cash(sum_now) > EOFY
  if (sum_past)
    printf " %26s\n", print_cash(sum_past) > EOFY
  else
    printf "\n" > EOFY
}

# The selector functions are filters for controlling
# what is printed out
# The simplest
function select_class(a, class_name, blocked_class) {
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Include class blocking
function block_class(a, class_name, blocked_class) {
  if (((a) ~ ("^" ( blocked_class) "[.:]")))
    return FALSE # Blocked!

  # Just the simple case
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Block multiple classes...
function block_class_list(a, class_name, blocked_class_list,      x) {
  # blocked class might actually be an array of blocked classes
  for (x in blocked_class_list)
    if (((a) ~ ("^" ( x) "[.:]"))) # Blocked!
      return FALSE

  # Just the simple case
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# A not so simple "print_line"
# FIXME
#
function print_line(long, stream) {
  if (long)
    print_underline(72, 0, stream)
  else
    print_underline(46, 0, stream)
}

function print_underline(l, notab, fd,    i) {
  notab = !notab ? 0 : 1
  fd = ("" == fd) ? "/dev/stdout" : fd
  if (!notab)
    printf  "\t" > fd
  for (i = 0; i <= l; i++)
    printf "_" > fd
  printf "\n" > fd
}

# Code sharing between capital gains and deferred gains
# FIXME almost redundant
function print_gains_summary(units_sold, sum_cost, sum_paid, adjusted_cost, reduced_cost, width, disc_gains, other_gains, tax_losses, output_stream,
                             label, gain_type) {
  # If units sold is negative these are deferred gains...
  if (units_sold < 0) {
    label = "Held"
    gain_type = "Deferred"
    units_sold = - units_sold
  } else {
    gain_type = "Taxable"
    label = "Sold"
  }

  # Shared code for printing gains summaries
  printf "\t%6s Units => %10.3f %*s => %14s Value => %14s Reduced  => %13s Accounting Gain => %14s\n", label,
    units_sold, 4 + width, "Cost", print_cash(sum_cost), print_cash(sum_paid), print_cash(reduced_cost), print_cash(sum_paid - reduced_cost) > output_stream

  # Check the adjusted cost
  if (adjusted_cost != reduced_cost)
    printf "\t%*s => %13s\n", 82 + width, "Adjusted", print_cash(adjusted_cost) > output_stream

  printf "\t%*s => %14s\n", 115 + width, (gain_type " Gain"), print_cash(- disc_gains) > output_stream
  if (!(((other_gains) <= Epsilon) && ((other_gains) >= -Epsilon)))
    printf "\t%*s => %14s\n", 115 + width, "Other Gain",      print_cash(- other_gains) > output_stream
  printf "\t%*s => %14s\n", 115 + width, (gain_type " Loss"),    print_cash(tax_losses) > output_stream
}


function get_taxable_gains(now, gains_stream,
                           long_gains, long_losses, tax_long,
                           short_gains, short_losses, tax_short,
                           losses) {

  # This function computes the taxable gains
  # It works for partioned long & short gains
  # And also for deferred gains when all such gains are
  losses = ((losses)?( losses):( 0))

  # Summarize starting point
  print_underline(43, 0, gains_stream)
  printf "\nAfter Application of Any Losses\n" > gains_stream

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  # Australian scheme & US Scheme are same
  # once short & long losses are disregarded
  long_gains  += long_losses # Net long term gains / losses
  short_gains += short_losses # Net short term losses / gains
  if (!((losses + short_gains + long_gains) < -Epsilon)) {
    # More carried losses generated
    losses += short_gains + long_gains

    # Record the details of short term & long term losses
    short_losses = ((((short_gains) >  Epsilon))?( short_gains):( 0))
    long_losses  = ((((long_gains) >  Epsilon))?(  long_gains):( 0))

    # Zero negligible losses
    if ((((losses) <= Epsilon) && ((losses) >= -Epsilon)))
      losses = 0

    printf "\n\tOverall Capital Loss\n" > gains_stream
    if (((losses) >  Epsilon))
      printf "\t%27s => %14s\n", "Capital Losses", print_cash(losses) > gains_stream
    if (((short_gains) < -Epsilon))
      printf "\t%27s => %14s\n", "Short Gains", print_cash(- short_gains) > gains_stream
    else if (((short_losses) >  Epsilon))
      printf "\t%27s => %14s\n", "Short Losses", print_cash(short_losses) > gains_stream

    if (((long_gains) < -Epsilon))
      printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > gains_stream
    else if (((long_losses) >  Epsilon))
      printf "\t%27s => %14s\n", "Long Losses", print_cash(long_losses) > gains_stream

    # Zero the gains
    short_gains = long_gains = 0
  } else if (!((losses + short_gains) < -Epsilon)) {
    # No overall losses, only long gains left
    losses += short_gains
    long_gains += losses

    # There could be a short term loss in this scenario
    short_losses = ((((short_gains) >  Epsilon))?( short_gains):( 0))

    # But not a long term loss
    losses = short_gains = long_losses = 0

    printf "\n\tOnly Long Gains\n" > gains_stream
    printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > gains_stream
    if (!(((short_losses) <= Epsilon) && ((short_losses) >= -Epsilon)))
      printf "\t%27s => %14s\n", "Short Losses", print_cash(short_losses) > gains_stream
  } else {
    # Long and Short Gains
    short_gains += losses

    # No long term or short term losses
    losses = short_losses = long_losses = 0

    printf "\n\tBoth Short & Long Gains\n" > gains_stream
    printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > gains_stream
    printf "\t%27s => %14s\n", "Short Gains", print_cash(- short_gains) > gains_stream
  }

  # Save taxable short & long gains
  if (tax_long)
    set_cost(tax_long, long_gains, now)
  if (tax_short)
    set_cost(tax_short, short_gains, now)

  # Return capital losses
  return losses
}

# A write back function
function write_back_losses(future_time, now, limit, available_losses, write_stream,

                           income_tax, taxable_income, tax_refund,
                           taxable_gains, gains_written_off) {
  #
  # Oldest losses are dealt with first
  # a recursive routine is easy - this could also be flattened into a loop
  #
  # This rewrites existing values but should be ok for repeated calls since
  # after first call one or both values are zero and unamendable
  #
  if (now > limit) {
    # Keep going
    available_losses = write_back_losses(future_time, ((now) + one_year(now, -1)), limit, available_losses, write_stream)

    # Any losses left?
    if ((((available_losses) <= Epsilon) && ((available_losses) >= -Epsilon)))
      return 0

    # Record the process
    taxable_gains  = get_cost(TAXABLE_GAINS, now)
    printf "\t%27s => %13s\n", "Write Back", get_date(now) > write_stream
    printf "\t%27s => %14s\n", "Gains", print_cash(- taxable_gains) > write_stream

    # Get the gains
    if (((taxable_gains) < -Epsilon)) {
      # There are gains which can be offset
      # We only get to here when there are available losses
      if (available_losses + taxable_gains > 0) {
        # More losses than gains
        available_losses += taxable_gains

        # Record gains written off
        gains_written_off = taxable_gains

        # Reset taxable gains
        taxable_gains = 0
      } else {
        # More gains than losses
        taxable_gains += available_losses

        # Record gains written off
        gains_written_off = - available_losses

        # Reset available losses
        available_losses = 0
      }

      # This generates a change in the total income tax - the tax refund
      tax_refund = get_tax(now, Tax_Bands, get_cost(TAXABLE_INCOME, now) + gains_written_off) - get_cost(INCOME_TAX, now)

      # Overwrite taxable gains
      set_cost(TAXABLE_GAINS, taxable_gains, now)

      # The refund is a simple refundable offset at the future time
      if (((tax_refund) < -Epsilon))
        adjust_cost(REFUNDABLE_OFFSETS, tax_refund, future_time)

      # Record This
      printf "\t%27s => %14s\n", "Rewritten Gains", print_cash(- taxable_gains) > write_stream
      printf "\t%27s => %14s\n", "New Available Losses", print_cash(available_losses) > write_stream
      printf "\t%27s => %14s\n", "Tax Refund", print_cash(- tax_refund) > write_stream
      printf "\t%27s => %14s\n", "Total Refundable Offset", print_cash(get_cost(REFUNDABLE_OFFSETS, future_time)) > write_stream
    }
  }

  # Finish Up
  printf "\n\n" > write_stream

  # Finished
  return available_losses
}


# Currency specific modules

#!/usr/local/bin/gawk -f
# p.aud_modules.awk
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



# It needs its own begin section...
BEGIN {

  # // Extras for AUD
  ((SUBSEP in ATO_Levy)?(TRUE):(FALSE))
  ((SUBSEP in CGT_Discount)?(TRUE):(FALSE))
  ((SUBSEP in GST_Rate)?(TRUE):(FALSE))
  ((SUBSEP in LIC_Allowance)?(TRUE):(FALSE))
  ((SUBSEP in Low_Income_Offset)?(TRUE):(FALSE))
  ((SUBSEP in Medicare_Levy)?(TRUE):(FALSE))
  ((SUBSEP in Member_Liability)?(TRUE):(FALSE))
  ((SUBSEP in Reserve_Rate)?(TRUE):(FALSE))

  # // Can set constants here
  if ("" == Qualification_Window)
    EOFY_Window = Qualification_Window = 0
  else {
    Qualification_Window = 91 * (86400) # seconds
    EOFY_Window = 0.5 * (Qualification_Window - (86400))
  }

  # Start of FY
  if ("" == FY_Date)
    FY_Date = "Jul 01"

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

  # The default tax band
  Tax_Bands[Epoch][0] = 0.15

  #  The Default Medicare Levy
  Medicare_Levy[Epoch][0] = 0.00

  # The default low income offset
  Low_Income_Offset[Epoch][0] = 0.00

  # Other special accounts
  FRANKING_TAX = initialize_account("LIABILITY.TAX:FRANKING.TAX")

  # Kept apart to allow correct allocation of member benfits in an SMSF
  CONTRIBUTION_TAX = initialize_account("LIABILITY.TAX:CONTRIBUTION.TAX")
  #
  # # Franking Credits
  # FRANKING_PAID   = initialize_account("SPECIAL.FRANKING:FRANKING.PAID")
  # FRANKING        = initialize_account("SPECIAL.FRANKING:FRANKING") # The Franking account balance
  # # Other tax credits, offsets & deductions
  # LIC_CREDITS     = initialize_account("SPECIAL.TAX:LIC.CREDITS")

  # Franking deficit
  FRANKING_DEFICIT   = initialize_account("SPECIAL.OFFSET.FRANKING_DEFICIT:FRANKING.OFFSETS")

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

    # Special accounts for SMSFs
    RESERVE   = initialize_account("LIABILITY.RESERVE:INVESTMENT.RESERVE")
    ALLOCATED = initialize_account("SPECIAL.ACCOUNT:ALLOCATED")

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

                                        taxable_gains,
                                        market_changes,
                                        accounting_gains, accounting_losses,
                                        foreign_income, exempt_income,
                                        foreign_expenses, extra_tax,
                                        contributions, income_due, other_expenses,
                                        lic_deductions,
                                        other_income, deferred_tax, deferred_gains,
                                        capital_losses, old_losses, tax_losses,
                                        tax_owed, tax_paid, tax_due, tax_with, tax_cont, income_tax,
                                        franking_offsets, foreign_offsets, franking_balance,
                                        no_carry_offsets, carry_offsets, refundable_offsets, no_refund_offsets,
                                        taxable_income,
                                        medicare_levy, extra_levy, x) {

  # Get market changes
  market_changes = get_cost(MARKET_CHANGES, now) - get_cost(MARKET_CHANGES, past)

  # Let's go
  printf "%s\n", Journal_Title > EOFY
  printf "Statement of Taxable Income\n" > EOFY

  printf "For the year ending %s\n", get_date(yesterday(now)) > EOFY
  print_underline(80, 1, EOFY)
  printf "%80s\n", strftime("%Y", now, UTC) > EOFY
  printf "%80s\n", "$" > EOFY

  # First entry
  printf "%22s %38s\n", "Benefits Accrued as a Result of Operations", print_cash(benefits) > EOFY

  # Additions
  printf "ADD\n" > EOFY

  # Start with market losses
  other_income = (((market_changes) >   Epsilon)?( (market_changes)):(  0))
  if (!(((other_income) <= Epsilon) && ((other_income) >= -Epsilon)))
    printf "\t%40s %32s\n", "Unrealized Losses", print_cash(other_income) > EOFY

  # Accounting losses are added - as are taxable gains
  accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)
  if (!(((accounting_losses) <= Epsilon) && ((accounting_losses) >= -Epsilon))) {
    printf "\t%40s %32s\n", "Capital Losses", print_cash(accounting_losses) > EOFY
    other_income += accounting_losses
  }

  # Non deductible EXPENSES
  # This should be handled by having EXPENSE.NON-DEDUCTIBLE.BENEFIT etc
  #    EXPENSE.NON-DEDUCTIBLE.DEEMED
  #    EXPENSE.NON-DEDUCTIBLE.BENEFIT (SMSF)
  #    EXPENSE.NON-DEDUCTIBLE.DIVIDEND (PTY)
  # Deductible EXPENSE
  #    EXPENSE.DISTRIBUTION (TRUST)
  other_expenses = get_cost("*EXPENSE.NON-DEDUCTIBLE", now) - get_cost("*EXPENSE.NON-DEDUCTIBLE", past)
  if (!(((other_expenses) <= Epsilon) && ((other_expenses) >= -Epsilon))) {
    printf "\t%40s %32s\n", "Other Non Deductible Expenses", print_cash(other_expenses) > EOFY
    other_income += other_expenses
  }

  # taxable capital gains
  #
  taxable_gains = get_cost(TAXABLE_SHORT, now) + (1.0 - ((CGT_Discount[2])?( (CGT_Discount[1]/CGT_Discount[2])):( assert(FALSE, "Division by zero in rational fraction" CGT_Discount[1] "/" CGT_Discount[2])))) * get_cost(TAXABLE_LONG, now)
  if ((((taxable_gains) <= Epsilon) && ((taxable_gains) >= -Epsilon)))
    taxable_gains = 0
  else {
    # Gains are a negative number
    other_income -= taxable_gains
    printf "\t%40s %32s\n", "Taxable Capital Gains", print_cash(-taxable_gains) > EOFY
  }

  # Save the taxable gains
  set_cost(TAXABLE_GAINS, taxable_gains, now)

  # Imputation Tax Offsets
  #

  # Tax credits received during this FY
  franking_offsets = - (get_cost("*SPECIAL.OFFSET.FRANKING", now) - get_cost("*SPECIAL.OFFSET.FRANKING", past))
  if (!(((franking_offsets) <= Epsilon) && ((franking_offsets) >= -Epsilon))) {
    other_income += franking_offsets
    printf "\t%40s %32s\n", "Franking Offsets", print_cash(franking_offsets) > EOFY
  }

  if (!(((other_income) <= Epsilon) && ((other_income) >= -Epsilon))){
    print_underline(80, 1, EOFY)
    printf "\t%40s %32s\n\n", "Other Income", print_cash(other_income) > EOFY
  }

  # Reductions
  printf "LESS\n" > EOFY

  # Expenses
  exempt_income = -(get_cost("*INCOME.EXEMPT", now) - get_cost("*INCOME.EXEMPT", past))
  if (exempt_income > Epsilon)
    printf "\t%40s %32s\n", "Exempt Income", print_cash(exempt_income) > EOFY

  # Market and Accounting Capital Gains
  other_expenses = - (((market_changes) < - Epsilon)?( (market_changes)):(  0))
  if (other_expenses > Epsilon)
    printf "\t%40s %32s\n", "Unrealized Gains", print_cash(other_expenses) > EOFY

  # Tax exempt income
  other_expenses += exempt_income

  # Accounting losses are added - as are taxable gains
  accounting_gains = -(get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past))
  if (!(((accounting_gains) <= Epsilon) && ((accounting_gains) >= -Epsilon))) {
    printf "\t%40s %32s\n", "Capital Gains", print_cash(accounting_gains) > EOFY
    other_expenses += accounting_gains
  }

  # And the non-concessional contributions
  # Should look at CONTRIBUTION minus the one taxed subclass because maybe more than one tax-free subclass?
  contributions = -(get_cost("*INCOME.CONTRIBUTION.TAX-FREE", now) - get_cost("*INCOME.CONTRIBUTION.TAX-FREE", past))
  if (!(((contributions) <= Epsilon) && ((contributions) >= -Epsilon))) {
    printf "\t%40s %32s\n", "Non Taxable Contributions", print_cash(contributions) > EOFY
    other_expenses += contributions
  }

  # Finally LIC Deductions (if eligible)
  # LIC credits 1/3 for SMSF
  #             1/2 for individual
  #             0/3 for company
  lic_deductions = - ((LIC_Allowance[2])?( (LIC_Allowance[1]/LIC_Allowance[2])):( assert(FALSE, "Division by zero in rational fraction" LIC_Allowance[1] "/" LIC_Allowance[2]))) * (get_cost(LIC_CREDITS, now) - get_cost(LIC_CREDITS, past))

  # Always apply allowance at this point to catch explicit allocations to LIC
  if (!(((lic_deductions) <= Epsilon) && ((lic_deductions) >= -Epsilon))) {
    printf "\t%40s %32s\n", "LIC Deduction", print_cash(lic_deductions) > EOFY
    other_expenses += lic_deductions
  }

  # Summarize other expenses
  if (!(((other_expenses) <= Epsilon) && ((other_expenses) >= -Epsilon))) {
    print_underline(80, 1, EOFY)
    printf "\t%40s %32s\n\n", "Other Expenses", print_cash(other_expenses) > EOFY
  }

  taxable_income = benefits + other_income - other_expenses
  print_underline(80, 1, EOFY)
  printf "%48s %32s\n\n", "TAXABLE INCOME OR LOSS", print_cash(taxable_income) > EOFY

  # Record this quantity
  set_cost(TAXABLE_INCOME, taxable_income, now)

  # Keep the income tax on the taxable income - the actual amount owed may change due to tax offsets etc
  income_tax = tax_owed = get_tax(now, Tax_Bands, taxable_income) # Just need total tax
  printf "%48s %32s\n", "Income Tax on Taxable Income or Loss ", print_cash(tax_owed) > EOFY

  # Record this quantity
  set_cost(INCOME_TAX, income_tax, now)

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
  printf "Less\n" > EOFY

  ## Franking deficit needs to be checked here
  franking_balance = 0
  if ((Journal_Type ~ /^(PTY|CORP|LTD)$/)) {

    # Franking
    # Check the franking balance (not the same as offsets, which were accumulated this FY)
    franking_balance = get_cost(FRANKING, now)

    # The franking deficit offsets
    franking_deficit_offsets = - get_cost(FRANKING_DEFICIT, now)


    # Need to check for franking deficit tax here
    if (((franking_balance) < -Epsilon)) {
      # This is a condition for franking deficit tax - that the franking balance
      # is zero; in fact it is not a sufficient condition; since a refund
      # within three months of the EOFY will also trigger it
      printf "\t%40s %32s\n", "Franking Balance is Overdrawn", print_cash(franking_balance) > EOFY

      # Compute the franking deficit tax due
      printf "\t%40s %32s\n", "Franking Deficit Tax Due", print_cash(- franking_balance) > EOFY

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

      if (((x - franking_balance) >  Epsilon)) {
        franking_deficit_offsets -= Franking_Deficit_Reduction * franking_balance

      } else
        franking_deficit_offsets -= franking_balance



      # Don't adjust tax due - this is a separate liability
      x = 0
      set_cost(FRANKING_TAX, franking_balance, now)
    } else
      set_cost(FRANKING_TAX, 0, now)
  }

  # Report the Imputation and Foreign Offsets
  if (!(((franking_offsets) <= Epsilon) && ((franking_offsets) >= -Epsilon)))
    printf "\t%40s %32s\n", "Franking Offsets", print_cash(franking_offsets) > EOFY

  # Foreign offsets
  # Are no-refund-no-carry
  foreign_offsets = - (get_cost("*SPECIAL.OFFSET.FOREIGN", now) - get_cost("*SPECIAL.OFFSET.FOREIGN", past))
  if (!(((foreign_offsets) <= Epsilon) && ((foreign_offsets) >= -Epsilon))) {
    # Foreign offsets have complex rules too :( sigh ):
    #
    # If they are not greater than the Foreign_Offset_Limit it is ok to just use  them
    if (foreign_offsets > ((__MPX_H_TEMP__ = find_key(Foreign_Offset_Limit,  now))?( Foreign_Offset_Limit[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Foreign_Offset_Limit[0]):( 0))))) {
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
        foreign_offsets = max(((__MPX_H_TEMP__ = find_key(Foreign_Offset_Limit,  now))?( Foreign_Offset_Limit[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Foreign_Offset_Limit[0]):( 0)))), extra_tax)

      printf "\t%40s\n", "Foreign Offset Limit Applied" > EOFY
    } else
      extra_tax = 0

    # The offsets
    printf "\t%40s %32s\n\n", "Foreign Offsets", print_cash(foreign_offsets) > EOFY

  } else
    foreign_offsets = 0

  # No Carry Offsets (Class C)
  # The low income tax offset depends on income
  if ((Journal_Type ~ /^IND$/)) {
    x = get_tax(now, Low_Income_Offset, taxable_income)

    # This is an Australian no-carry offset computed from the taxable income
    if (!(((x) <= Epsilon) && ((x) >= -Epsilon)))
      printf "\t%40s %32s\n", "Low Income Tax Offset", print_cash(x) > EOFY

    # Get the other no_carry offsets
    no_carry_offsets = -(get_cost(NO_CARRY_OFFSETS, now) - get_cost(NO_CARRY_OFFSETS, past))
    if (!(((no_carry_offsets) <= Epsilon) && ((no_carry_offsets) >= -Epsilon)))
      printf "\t%40s %32s\n", "Other No-Carry Offsets", print_cash(no_carry_offsets) > EOFY

    # No need to adjust cost - since it will not be retained
    no_carry_offsets += x
  } else
    # Just get the total change in the offset
    no_carry_offsets = -(get_cost(NO_CARRY_OFFSETS, now) - get_cost(NO_CARRY_OFFSETS, past))

  # Foreign offsets are no-carry offsets
  no_carry_offsets += foreign_offsets

  # The no-carry offset
  if (!(((no_carry_offsets) <= Epsilon) && ((no_carry_offsets) >= -Epsilon)))
    printf "\t%40s %32s\n", "Total No-Carry Offsets", print_cash(no_carry_offsets) > EOFY

  # Other offsets
  # The carry offset (Class D)
  carry_offsets = - get_cost(CARRY_OFFSETS, now)
  if (!(((carry_offsets) <= Epsilon) && ((carry_offsets) >= -Epsilon)))
    printf "\t%40s %32s\n", "Total Carry Offsets", print_cash(carry_offsets) > EOFY
  printf "\n" > EOFY

  # The refundable offset (Class E)
  refundable_offsets = - get_cost(REFUNDABLE_OFFSETS, now)
  if (!(((refundable_offsets) <= Epsilon) && ((refundable_offsets) >= -Epsilon)))
    printf "\t%40s %32s\n", "Total Refundable Offsets", print_cash(refundable_offsets) > EOFY
  printf "\n" > EOFY

  # Franking offsets are (currently) refundable for SMSF and individuals
  if ((Journal_Type ~ /^SMSF$/) || (Journal_Type ~ /^IND$/)) {
    refundable_offsets += franking_offsets
    franking_offsets = 0
  } else
    no_carry_offsets += franking_offsets

  # At this stage no-carry and carry offsets behave the same
  no_refund_offsets = no_carry_offsets + carry_offsets

  # Apply the no_refund offsets (if any)
  if (((tax_owed) >  Epsilon) && ((no_refund_offsets) >  Epsilon)) {
    # Since franking offsets can generate a loss add them to
    # both sides of the balance
    tax_owed += franking_offsets

    if (tax_owed < no_refund_offsets) {
      # How many carry offsets were used?
      if (tax_owed > no_carry_offsets) # Some were used
        carry_offsets -= (tax_owed - no_carry_offsets)

      # information
      printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(tax_owed - franking_offsets) > EOFY
      tax_owed = 0
    } else { # All the no_refund offsets were used
      tax_owed -= no_refund_offsets
      carry_offsets = 0
      if (((no_refund_offsets - franking_offsets) >  Epsilon))
        printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(no_refund_offsets - franking_offsets) > EOFY
    }

    # OK now if the tax_owed is less than the amount of franking offsets
    # then the difference is transferred to tax losses
    if (tax_owed < franking_offsets) {
      franking_offsets -= tax_owed

      printf "\t%40s %32s>\n", "<Franking Offsets Used", print_cash(tax_owed) > EOFY
      # Report remaining  franking offsets
      if (((franking_offsets) >  Epsilon))
        printf "\t%40s %32s>\n", "<Franking Offsets Remaining", print_cash(franking_offsets) > EOFY

      tax_owed = 0
    } else {
      tax_owed -= franking_offsets
      if (((franking_offsets) >  Epsilon))
        printf "\t%40s %32s>\n", "<All Franking Offsets Used", print_cash(franking_offsets) > EOFY
      franking_offsets = 0
    }

    # Report tax owed

  } # End of if any attempt to apply non-refundable assets

  # Now apply refundable offsets - but note if used these will not generate a tax loss
  if (((refundable_offsets) >  Epsilon)) {
    tax_owed -= refundable_offsets
    printf "\t%40s %32s>\n", "<Refundable Offsets Used", print_cash(refundable_offsets) > EOFY

  }

  # Finally franking deficit tax offsets can be applied
  if (((tax_owed) >  Epsilon) && ((franking_deficit_offsets) >  Epsilon)) {
    if (tax_owed < franking_deficit_offsets) {
      # How many franking deficit tax offsets were used?
      if (tax_owed > franking_deficit_offsets) # Some were used
        franking_deficit_offsets -= tax_owed

      # information
      printf "\t%40s %32s>\n", "<Franking Deficit Tax Offsets Used", print_cash(tax_owed - franking_deficit_offsets) > EOFY
      tax_owed = 0
    } else { # All the franking deficit offsets were used
      tax_owed -= franking_deficit_offsets
      printf "\t%40s %32s>\n", "<Franking Deficit Tax Offsets Used", print_cash(franking_deficit_offsets) > EOFY
      franking_deficit_offsets = 0
    }
  }

  #
  # Tax Losses
  #
  # The carried tax losses
  tax_losses = old_losses = get_cost(TAX_LOSSES, past)


  #
  # We can reduce tax_owed to zero, but not increase or generate a loss
  if (((tax_owed) >  Epsilon)) {
    # If tax is owed franking offsets must be all used
    assert((((franking_offsets) <= Epsilon) && ((franking_offsets) >= -Epsilon)), "Can't have remaining franking offsets if tax is still owed")

    if (((tax_losses) >  Epsilon)) {
      # Tax losses available for use - compute marginal tax change
      # x is the tax that would be paid on the tax_losses
      x = get_tax(now, Tax_Bands, tax_losses + taxable_income) - income_tax


    } else # No losses available
      x = 0

    # Is the tax owed less than the losses available?
    # Remember we have tax_owed > 0
    if (tax_owed < x) {
      # Yes so some losses will be extinguished
      # Which will reduce tax_owed to zero;
      tax_losses = get_taxable_income(now, x - tax_owed)

      tax_owed = 0
    } else {
      # All losses extinguished
      tax_owed -= x

      # So this reduces tax losses to zero

      tax_losses = 0
    }

  # Tax owed is negative - so losses are increased but allow for refundable offsets which were returned
#  } else if (above_zero(franking_offsets)) { # Increase losses
  } else if (((tax_owed + refundable_offsets) < -Epsilon)) { # Increase losses

    tax_losses = get_taxable_income(now, franking_offsets - refundable_offsets - tax_owed)

  }

  # The carried tax losses


  # Print the tax owed
  print_underline(80, 1, EOFY)
  printf "%48s %32s\n\n", "CURRENT TAX OR REFUND", print_cash(tax_owed) > EOFY

  #
  # Tax Residuals
  #
  # These occur due to mismatches in these accounts and one's actually used
  # Either due to errors or rounding in the accounts
  #
  # Take care that amounts are reset correctly
  # The residual tax liability is tax computed to be due but not actually paid or refunded
  # Careful when adjusting cost - a second run will continue to increase it
  # Either explicitly set the cost or reset it first
  set_cost(RESIDUAL, get_cost(RESIDUAL, ((now) - 1)) + get_cost(TAX, ((now) - 1)), now)

  # Adjust Levys

  # Compute tax due
  tax_paid = get_cost(PAYG, ((now) - 1)) - get_cost(PAYG, past)

  # And tax witheld
  tax_with = get_cost(WITHOLDING, ((now) - 1)) - get_cost(WITHOLDING, past)

  # If this is SMSF the levy is required
  if ((Journal_Type ~ /^SMSF$/))
    printf "\t%40s %32s\n", "Supervisory Levy", print_cash(((__MPX_H_TEMP__ = find_key(ATO_Levy,  now))?( ATO_Levy[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( ATO_Levy[0]):( 0))))) > EOFY

  # Medicare levy (if any)
  if (!(((medicare_levy) <= Epsilon) && ((medicare_levy) >= -Epsilon))) {
    printf "\t%40s %32s\n", "Medicare Levy", print_cash(medicare_levy) > EOFY
    tax_owed += medicare_levy
  }

  if (!(((tax_paid) <= Epsilon) && ((tax_paid) >= -Epsilon)))
    printf "\t%40s %32s\n", "Income Tax Distributions Paid", print_cash(tax_paid) > EOFY
  if (!(((tax_with) <= Epsilon) && ((tax_with) >= -Epsilon)))
    printf "\t%40s %32s\n", "Income Tax Withheld", print_cash(tax_with) > EOFY

  # Compute income tax due
  tax_due = tax_owed - (tax_paid + tax_with)
  set_cost(TAX, - tax_due, now)
  print_underline(80, 1, EOFY)
  printf "%48s %32s\n\n\n", "AMOUNT DUE OR REFUNDABLE", print_cash(((__MPX_H_TEMP__ = find_key(ATO_Levy,  now))?( ATO_Levy[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( ATO_Levy[0]):( 0)))) + tax_due) > EOFY

  # Clean up balance sheet - watch out for unbalanced transactions
  # Save contribution tax accounted for
  tax_cont = get_cost(CONTRIBUTION_TAX, ((now) - 1))

  # If this is an SMSF this disturbs the member liabilities
  # Adjust cost is OK because ALLOCATED/ADJUSTMENTS were reset at comencement of eofy_actions
  # For a none SMSF this is a synonym for ADJUSTMENTS
  adjust_cost(ALLOCATED, -(tax_cont + tax_owed - get_cost(FRANKING_TAX, now)), now)

  # Print out the tax and capital losses carried forward
  # These really are for time now - already computed
  capital_losses = get_cost(CAPITAL_LOSSES, now)
  if (!(((capital_losses) <= Epsilon) && ((capital_losses) >= -Epsilon)))
    printf "\t%40s %32s\n", "Capital Losses Carried Forward", print_cash(capital_losses) > EOFY

  # The change in tax losses
  if (!(((tax_losses - old_losses) <= Epsilon) && ((tax_losses - old_losses) >= -Epsilon))) {
    if (tax_losses > old_losses)
      printf "\t%40s %32s\n", "Tax Losses Generated", print_cash(tax_losses - old_losses) > EOFY
    else
      printf "\t%40s %32s\n", "Tax Losses Extinguished", print_cash(old_losses - tax_losses) > EOFY
  }

  # The carried tax losses
  if (!(((tax_losses) <= Epsilon) && ((tax_losses) >= -Epsilon)))
    printf "\t%40s %32s\n", "Tax Losses Carried Forward", print_cash(tax_losses) > EOFY
  else
    tax_losses = 0

  # Save the carried losses
  set_cost(TAX_LOSSES, tax_losses, now)

  # Franking
  if (!(((franking_balance) <= Epsilon) && ((franking_balance) >= -Epsilon)))
    printf "\t%40s %32s\n", "Franking Balance Carried Forward", print_cash(franking_balance) > EOFY

  # Franking Deficit
  # Save the franking deficit offsets
  if (!(((franking_deficit_offsets) <= Epsilon) && ((franking_deficit_offsets) >= -Epsilon)))
    printf "%48s %32s\n\n", "Franking Deficit Offsets Carried Forward", print_cash(franking_deficit_offsets) > EOFY
  else
    franking_deficit_offsets = 0
  set_cost(FRANKING_DEFICIT, -franking_deficit_offsets, now)

  # Update carry forward offsets
  if (!(((carry_offsets) <= Epsilon) && ((carry_offsets) >= -Epsilon)))
    printf "\t%40s %32s\n", "Non-Refundable Offsets Carried Forwards", print_cash(carry_offsets) > EOFY
  else
    carry_offsets = 0
  set_cost(CARRY_OFFSETS, -carry_offsets, now)

  # Refundable offsets were (well) refunded so reset them too
  set_cost(REFUNDABLE_OFFSETS, 0, now)

  # Now we need Deferred Tax - the hypothetical liability that would be due if all
  # assets were liquidated today
  deferred_gains = get_cost(DEFERRED_GAINS, now)

  # Schedule is finished
  print_underline(43, 0, EOFY)
  print "\n" > EOFY

  # Gains are negative - losses are positive
  # Catch negligible gains
  if (!(((deferred_gains) <= Epsilon) && ((deferred_gains) >= -Epsilon))) {
    # Deferred tax losses can reduce future tax liability so are a deferred tax asset
    deferred_tax = get_tax(now, Tax_Bands, taxable_income - deferred_gains) - income_tax
    set_cost(DEFERRED, - deferred_tax, now)

    if (((deferred_tax) >  Epsilon))
      printf "\t%40s %32s\n", "Deferred Tax Liability", print_cash(deferred_tax) > EOFY
    else if (((deferred_tax) < -Epsilon))
      printf "\t%40s %32s\n", "Deferred Tax Asset    ", print_cash(deferred_tax) > EOFY
    else {
      deferred_tax = 0
      printf "\t%40s %32s\n", "Zero Deferred Tax", print_cash(deferred_tax) > EOFY
    }

    # Get the change this FY
    # If x < 0 EXPENSE
    # if x > 0 INCOME
    x = - deferred_tax - get_cost(DEFERRED, past)
    if (!(((x) <= Epsilon) && ((x) >= -Epsilon))) {
      # Adjust cost/receipts for deferred expense/income
      # For a none SMSF this is a synonym for ADJUSTMENTS
      adjust_cost(ALLOCATED, x, now)
    }
  }

  # Set tax values to zero - is this needed?
  set_cost(PAYG, 0, now)
  set_cost(WITHOLDING, 0, now)
  set_cost(CONTRIBUTION_TAX, 0, now)
}

#
#
## Dividend Qualification Function
##
function dividend_qualification_aud(a, underlying_asset, now, unqualified,

                                       unqualified_account, imputation_credits) {

  # For Australia we need to adjust tax credits associated with an account
  #
  if ((((unqualified) <= Epsilon) && ((unqualified) >= -Epsilon)))
    # The payment was fully qualified
    return

  # Were there any tax credits anyway?
  if (underlying_asset in Tax_Credits) {
    # Get the Imputation credits associated with this transaction - and only this transaction
    imputation_credits = (get_cost(Tax_Credits[underlying_asset],  now) - get_cost(Tax_Credits[underlying_asset], (( now) - 1)))
    if (!(((imputation_credits) <= Epsilon) && ((imputation_credits) >= -Epsilon))) {
      # Create an unqualified account
      unqualified_account = initialize_account("SPECIAL.OFFSET.FRANKING.UNQUALIFIED:U_TAX." Leaf[underlying_asset])

      # The adjustment
      unqualified *= imputation_credits


      # Now sum the unqualified credits in this account
      # This would occur when state files are used
      set_cost(unqualified_account, get_cost(unqualified_account, now) - unqualified, ((now) + 1))

      # Adjust the franking account too... (opposite sign - this is asset like)
      set_cost(FRANKING, get_cost(FRANKING, now) + unqualified, ((now) + 1))


    } # No credits at time now
  } # No tax credits for this account
} # All done

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
  # Note that this is only needed to be done once
  # Reset the liabilities to just before now so that they are correct even if balance journal is re-run
  for (x in Member_Liability)
    set_cost(x, get_cost(x, ((now) - 1)), now)
  set_cost(RESERVE, get_cost(RESERVE, ((now) - 1)), now)

  # Adjust member liability
  delta_profits = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - initial_allocation



  # Update the allocation - a get is before the set
  if (!(((delta_profits) <= Epsilon) && ((delta_profits) >= -Epsilon)))
    # Update the Allocated Profits - this adds to changes made in print_tax_statement
    adjust_cost(ALLOCATED, delta_profits, now)

  # Also make adjustments to the reserve - use the updated Allocation
  x = get_cost(ALLOCATED, now) - get_cost(ALLOCATED, past)


  # Apply actual profits to the reserve
  if (((x) >  Epsilon)) {
    # Only distribute actual delta_profits to the reserve
    # Compute the net allocated profits in the current period
    x *= ((__MPX_H_TEMP__ = find_key(Reserve_Rate,  now))?( Reserve_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Reserve_Rate[0]):( 0))))

    # The only reserve set in eofy actions so use now
    adjust_cost(RESERVE, -x, now)

  } else
    x = 0

  # By this point there are several adjustments required to
  # both redistribute liabilities and allocated profits
  delta_profits = get_cost(ALLOCATED, now) - initial_allocation - x
  if (!(((delta_profits) <= Epsilon) && ((delta_profits) >= -Epsilon)))
    update_member_liability(now, delta_profits)

  # Unallocated expenses/income
  adjust_cost(ALLOCATED, (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now), now)

}

# This checks all is ok
function check_balance_smsf(now,        sum_assets, sum_liabilities, sum_adjustments, balance, show_balance) {
  # The following should always be true (Equity is treated a special case of liability)
  # Assets - Liabilities = 0 (SMSFs have asimplified equation)
  # This compares the cost paid - so it ignores the impact of revaluations and realized gains & losses
  sum_assets =  get_cost("*ASSET", now) - get_cost("*INCOME.GAINS.REALIZED", now) - get_cost("*EXPENSE.LOSSES.REALIZED", now) - get_cost("*EXPENSE.UNREALIZED", now)

  # Work out the total assets etc
  sum_liabilities = - get_cost("*LIABILITY", now)
  sum_adjustments =   get_cost("*SPECIAL.BALANCING", now)

  # The balance should be zero
  # A super fund has only assets and liabilities since the income and expenses are attributed to members
  sum_adjustments = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now)
  balance = sum_assets - (sum_liabilities + sum_adjustments)


  # No default printing
  show_balance = FALSE


  # Is there an error?
  if (!(((balance) <= Epsilon) && ((balance) >= -Epsilon))) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0
    show_balance = TRUE
  }

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now)
    printf "\tAssets      => %20.2f\n", sum_assets
    printf "\tLiabilities => %20.2f\n", sum_liabilities
    printf "\tAdjustments => %20.2f\n", sum_adjustments
    printf "\tBalance     => %20.2f\n", balance
    assert((((balance) <= Epsilon) && ((balance) >= -Epsilon)), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}



# We are nearly at Version 0.9 (Most Basic Functions)
#   This processes an mpx format file
#
#    Original style
#   2008 Apr 09, 604000, 776003,    200.000,    8382.00, BHP in specie transfer (200 units)
#   became over time
#   2008 Apr 09, CASH, BHP.ASX, 200, 8382.00, # (in specie transfer in fact)
#
# To Do =>
#
#   Fix up wiki files
#   More flexible ordering of optional fields?
#   Selectively print individual reports
#   other tax_statement calculations (eg UK, US, NZ etc...)
#
#   Tax Adjustments / could be simplified?
#   Allow non-rectangular arrays, i.e. only have [a][p][e] for active elements
#   Allow other currencies or commodities (eg USD, AU, AG, etc)
#   Read single entry transactions
#   special notes for -l version
#   describe gpp properly
#   single entry style
#   Share splits etc
#   Short day-by-day performance summary (cost-value-etc.. estimated for EOFY)
#
#   Integrate with graphing/analysis package (eris)
#   Produce output for import into Ledger?
#
BEGIN {
  # An array to hold real values
  ((SUBSEP in Real_Value)?(TRUE):(FALSE))

  # And a gains stack
  ((SUBSEP in Gains_Stack)?(TRUE):(FALSE))
  Long_Gains_Key   = "Long Gains  "
  Long_Losses_Key  = "Long Losses "
  Short_Gains_Key  = "Short Gains "
  Short_Losses_Key = "Short Losses"

  # Sort arrays this way...
  Array_Sort = sort_arrays_on("@ind_num_desc")
  Variable_Name = ""
  Variable_Keys[0] = ""

  # Output Fields
  OFS = ","

  # MONTH_FORMAT = "%Y %b %d"
  # ISO_FORMAT = "%F"
  if ("" == DATE_FORMAT)
    DATE_FORMAT = ("%Y %b %d") 
  LONG_FORMAT = (DATE_FORMAT " %H::%M::%S")

  # Logic conventions
  TRUE  = 1
  FALSE = 0

  # Price Record is Off
  Price_Record = FALSE
  Q_Record = FALSE
  Filter_Data = ""

  # Suppress rounding errors
  Epsilon = 0.0001

  # The Time Zone
  Time_Zone = 10
  UTC = 1

  # The Epoch
  # A more practical Epoch
  Epoch = mktime((2000) " 01 01 00 00 00", UTC)

  # A distant Future
  Future = mktime((2999) " 12 31 00 00 00", UTC)

  # Default FY date
  FY_Date = ("Jul-01")

  # The allowed cost elements
  I   = "I"
  II  = "II"
  III = "III"
  IV  = "IV"
  V   = "V"
  Elements[I]   = I
  Elements[II]  = II
  Elements[III] = III
  Elements[IV]  = IV
  Elements[V]   = V
  COST_ELEMENT = II # Default value

  # url encoding lookup table
  url_init()

  # Initialize arrays
  ((SUBSEP in Account_Term)?(TRUE):(FALSE))
  ((SUBSEP in Accounting_Cost)?(TRUE):(FALSE))
  ((SUBSEP in Cost_Basis)?(TRUE):(FALSE))
  ((SUBSEP in Foreign_Offset_Limit)?(TRUE):(FALSE))
  ((SUBSEP in Held_From)?(TRUE):(FALSE))
  ((SUBSEP in Held_Until)?(TRUE):(FALSE))
  ((SUBSEP in Leaf)?(TRUE):(FALSE))
  ((SUBSEP in Leaf_Count)?(TRUE):(FALSE))
  ((SUBSEP in Lifetime)?(TRUE):(FALSE))
  ((SUBSEP in Long_Name)?(TRUE):(FALSE))
  ((SUBSEP in Maturity_Date)?(TRUE):(FALSE))
  ((SUBSEP in Method_Name)?(TRUE):(FALSE))
  ((SUBSEP in Number_Parcels)?(TRUE):(FALSE))
  ((SUBSEP in Parcel_Tag)?(TRUE):(FALSE))
  ((SUBSEP in Parent_Name)?(TRUE):(FALSE))
  ((SUBSEP in Price)?(TRUE):(FALSE))
  ((SUBSEP in Payment_Date)?(TRUE):(FALSE))
  ((SUBSEP in Qualified_Units)?(TRUE):(FALSE))
  ((SUBSEP in Tax_Adjustments)?(TRUE):(FALSE))
  ((SUBSEP in Tax_Bands)?(TRUE):(FALSE))
  ((SUBSEP in Tax_Credits)?(TRUE):(FALSE))
  ((SUBSEP in Threshold_Dates)?(TRUE):(FALSE))
  ((SUBSEP in Total_Units)?(TRUE):(FALSE))
  ((SUBSEP in Underlying_Asset)?(TRUE):(FALSE))
  ((SUBSEP in Units_Held)?(TRUE):(FALSE))

  # This is a CSV file
  read_csv_records()

  # Transaction line defaults
  new_line()
  # #
  # # Initialize state file information
  # initialize_state()

  # Default Portfolio Name and document URI
  Journal_Title = "NEMO"
  Journal_Type = "IND"
  Document_Root = "https:example.com/NEMO"

  # Default currency
  Journal_Currency = "AUD"

  # Default Price Record Class
  Asset_Prefix = ("ASSET.CAPITAL.SHARES")
  Asset_Suffix = ("ASX")

  # A Symbol
  Symbol = ""

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
  if ("" == Show_Extra)
    Show_Extra = 0

  # Show all transactions
  if ("" == Show_All)
    Show_All = 0

  # Report file
  if (!Reports)
    Reports = "/dev/null"

  # EOFY statements are printed to a specific stream
  EOFY = "/dev/null"

  # Which account to track
  if ("" == Show_Account)
    Show_Account = FALSE
  else
    Show_All = FALSE

  # Initially set the last time to be -1
  Last_Time = - 1
  FY_Time = -1
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

# Program state can be given using a state pattern range
# #
/^<</,/>>$/ {
  # We need to establish if this is an array or a scalar
  if ($0 ~ /^<</) {
    Variable_Name = trim($2)
    assert(Variable_Name in SYMTAB, "<" Variable_Name "> is not declared")

    # Would be nice to have scalar syntax
    # <<,Variable_Name,Value>>
    # Any more fields on this line?
    if ($NF ~ />>/ && NF == 4) {
      # This is a scalar - value in field 3
      SYMTAB[Variable_Name] = read_value(trim($3))

      # Logging

    }
  } else if ($0 !~ /^>>/)
    # Could be array OR scalar
    read_state(NF)

  # Get the next record
  next
}

# Special price records can be imported in CBA style metastock format
##
/^PP/ {
  # Format is
  # PP
  # BST,171124,.895,.895,.895,.895,0
  # BST,171127,0,0,0,0,0
  # BST,171128,.9,.9,.9,.9,35000
  # PP
  #
  # which is
  # <SYMBOL>, DATE, OPEN, HIGH, LOW, CLOSE, VOLUME
  #
  Price_Record = !Price_Record

  # If this is the end of the price record reset the Asset_Prefix to the default value
  if (!Price_Record) {
    Asset_Prefix = ("ASSET.CAPITAL.SHARES")
    Asset_Suffix = ("ASX")
  } else # If Price_Record is ever set make sure to filter out of range entries
    if (Filter_Data !~ /Price/)
      Filter_Data = Filter_Data " Price "
  next
}

1 == Price_Record {
  read_price()
  next
}

# Another special case (temporary?) is to import ex-dividend dates
##
/^QQ/ {
  # Format is
  # <<,Code,BHP.ASX,>>
  # QQ
  # 07/03/2019,77.3232,08/03/2019,26/03/2019,I,-,
  # 10/01/2019,141.2742,11/01/2019,30/01/2019,S,-,
  # 06/09/2018,88.5453,07/09/2018,25/09/2018,F,-,
  # 08/03/2018,70.5852,09/03/2018,27/03/2018,I,-,
  # QQ
  #
  # which is
  # EX-DATE, IGNORE...
  #
  Q_Record = !Q_Record

  # If Q_Record is ever set make sure to filter out of range entries
  if (Q_Record) {
    if (Filter_Data !~ /Payment_Date/)
      Filter_Data = Filter_Data " Payment_Date "

  }

  next
}

1 == Q_Record {
  read_qualifying_dates()
  next
}


/START_JOURNAL/ {
  if (NF > 1) {
    # Check not called before
    assert(-1 == Last_Time, "Can't START_JOURNAL twice")

    # Is the currency consistent
    assert("AUD" == Journal_Currency, "Incompatible journal currency <" Journal_Currency "> in journal file - expected <" "AUD" "> instead")

    # Set the initial time
    Last_Time = read_date($2)

    # Set default functions
    Income_Tax_Function     = "income_tax_" tolower(Journal_Currency)
    Initialize_Tax_Function = "initialize_tax_" tolower(Journal_Currency)
    Dividend_Qualification_Function = "dividend_qualification_" tolower(Journal_Currency)

    # These functions are not
    Balance_Profits_Function  = "balance_journal"
    Check_Balance_Function  = "check_balance"
  }

  # Can only call this once and check a legal timestamp
  assert(Last_Time != (-1), Read_Date_Error)

  # Need to initialize FY information
  if (FY)
    initialize_fy(FY "-" FY_Date)
  else { # Turn off EOFY reporting
    Reports = "/dev/null"

    # Default Start_Time and Stop_Time
    if (!Start_Time)
      Start_Time = Last_Time
    else {
      Start_Time = read_date(Start_Time)
      assert((-1) != Start_Time, "Start_Time " Read_Date_Error)
    }

    if (!Stop_Time)
      Stop_Time = Future
    else {
      Stop_Time = read_date(Stop_Time)
      assert((-1) != Stop_Time, "Stop_Time " Read_Date_Error)
    }
  }

  # Initialize local tax variables
  @Initialize_Tax_Function()

  #
  # Initialize state file information
  initialize_state()

  # Which Calendar year is this?
  FY_Year = (strftime("%Y", (Last_Time), UTC) + 0)

  # The timestamp at the end of the year
  # This assumes FY_Date is the date of the
  # first day of a financial year
  FY_Time = read_date(FY_Year "-" FY_Date, 0)

  # Get the day number for the FY_Date
  FY_Day = (strftime("%j", (FY_Time), UTC) + 0)

  # Feb 28 has day number 59 so if FY_Day <= 60 - (1st day next FY)
  # then the current FY would include leap day if (FY - 1) was a leap year
  FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)

  # All done
  next
}

# Standard control function syntax is
# FUNCTION, [ARGUMENT_1, ARGUMENT_2]
# Control functions are:
#  SET_ENTRY
#  SET_BANDS
#  SET
#  CHECK
#
$1 ~ /(CHECK|SET|SET_BANDS|SET_ENTRY)/ {
 # Use a function so we can control scope of variables
 read_control_record()
 next
}

# Default record
{
 # Use a function so we can control scope of variables
 read_input_record()
 next
}



## Functions start here


# Initialize the financial year
function initialize_fy(fy_date) {
  # Month name format
  Stop_Time = read_date(fy_date, 0)
  Start_Time = ((Stop_Time) + one_year(Stop_Time, -1))

  # Override
  if (EOFY == Reports)
    Reports = "/dev/stdout"
}


# Initialize read/write of state files
function initialize_state(    x) {
  # Get which variables to write out
  ((SUBSEP in Array_Names)?(TRUE):(FALSE))
  ((SUBSEP in Scalar_Names)?(TRUE):(FALSE))

  # Current Version
  MPX_Version = Current_Version = "Version " string_hash(("Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Leaf_Count Lifetime Long_Name Maturity_Date Method_Name Number_Parcels Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Medicare_Levy Member_Liability Reserve_Rate ") ("MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_Time Qualification_Window ALLOCATED Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function "))
  if ("" != Write_Variables) {
    # This time we just use the requested variables
    split(Write_Variables, Array_Names, ",")
    for (x in Array_Names)
      # Ensure the requested variable name is allowable - it could be an array or a scalar
      if (!index(("Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Leaf_Count Lifetime Long_Name Maturity_Date Method_Name Number_Parcels Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Medicare_Levy Member_Liability Reserve_Rate "), Array_Names[x])) {
        assert(index(("MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_Time Qualification_Window ALLOCATED Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function "), Array_Names[x]), "Unknown Variable <" Array_Names[x] ">")

        # This is a scalar
        Scalar_Names[x] = Array_Names[x]
        delete Array_Names[x]
      }
  } else {
    # Use default read and write list
    Write_Variables = FALSE
    MPX_Arrays = ("Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Leaf_Count Lifetime Long_Name Maturity_Date Method_Name Number_Parcels Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Medicare_Levy Member_Liability Reserve_Rate ")
    MPX_Scalars = ("MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_Time Qualification_Window ALLOCATED Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function ")

    split(MPX_Arrays, Array_Names, " ")
    split(MPX_Scalars, Scalar_Names, " ")
  }
}

# Initialize special accounts
function set_special_accounts() {
  # Built in accounts are required for EOFY statements
  #
  # Check & Set use special accounts to trigger actions
  #
  initialize_account("SPECIAL.CONTROL:COST")
  initialize_account("SPECIAL.CONTROL:UNITS")
  initialize_account("SPECIAL.CONTROL:VALUE")
  initialize_account("SPECIAL.CONTROL:PRICE")

  # A NULL account
  NULL = initialize_account("SPECIAL.ACCOUNT:NULL")

  # The DEPRECIATION account
  DEPRECIATION = initialize_account("EXPENSE.DEPRECIATION:DEPRECIATION")

  # When a depreciating asset is sold any profit or loss is booked as income/expense to these accounts
  SOLD_APPRECIATION = initialize_account("INCOME.APPRECIATION:APPRECIATION.SOLD")
  SOLD_DEPRECIATION = initialize_account("EXPENSE.DEPRECIATION:DEPRECIATION.SOLD")

  # Balancing - to simplify processing of transactions at EOFY
  # These are income/expense items not needed in the operating statement
  ADJUSTMENTS      = initialize_account("SPECIAL.BALANCING:ADJUSTMENTS")

  # Keeping a record of taxable income, gains, losses
  CAPITAL_LOSSES   = initialize_account("SPECIAL.TAX:CAPITAL.LOSSES")
  TAX_LOSSES       = initialize_account("SPECIAL.TAX:TAX.LOSSES")
  TAXABLE_GAINS    = initialize_account("SPECIAL.TAX:TAXABLE.GAINS")
  TAXABLE_INCOME   = initialize_account("SPECIAL.TAX:TAXABLE.INCOME")
  INCOME_TAX       = initialize_account("SPECIAL.TAX:INCOME.TAX")
  TAXABLE_LONG     = initialize_account("SPECIAL.TAX:TAXABLE.LONG")
  TAXABLE_SHORT    = initialize_account("SPECIAL.TAX:TAXABLE.SHORT")

  # Built in TAX accounts - debtor like
  WITHOLDING   = initialize_account("ASSET.CURRENT.TAX:TAX.WITHOLDING")
  PAYG         = initialize_account("ASSET.CURRENT.TAX:TAX.PAYG")

  # Built in TAX accounts - creditor like
  DEFERRED     = initialize_account("LIABILITY.TAX:DEFERRED.TAX")
  TAX          = initialize_account("LIABILITY.TAX:TAX")
  RESIDUAL     = initialize_account("LIABILITY.TAX:RESIDUAL")
  GST          = initialize_account("LIABILITY.TAX:TAX.GST")

  # Offsets
  NO_CARRY_OFFSETS   = initialize_account("SPECIAL.OFFSET.NO_CARRY:NO_CARRY.OFFSETS")
  CARRY_OFFSETS      = initialize_account("SPECIAL.OFFSET.CARRY:CARRY.OFFSETS")
  REFUNDABLE_OFFSETS = initialize_account("SPECIAL.OFFSET.REFUNDABLE:REFUNDABLE.OFFSETS")

  # Franking Credits - strictly speaking should be AUD accounts
  FRANKING_PAID   = initialize_account("SPECIAL.FRANKING:FRANKING.PAID")
  FRANKING        = initialize_account("SPECIAL.FRANKING:FRANKING") # The Franking account balance
  # Other tax credits, offsets & deductions
  LIC_CREDITS     = initialize_account("SPECIAL.TAX:LIC.CREDITS")

  # Accounting capital gains accounts
  REALIZED_GAINS  = initialize_account("INCOME.GAINS.REALIZED:GAINS")
  REALIZED_LOSSES = initialize_account("EXPENSE.LOSSES.REALIZED:LOSSES")
  MARKET_CHANGES  = initialize_account("EXPENSE.UNREALIZED:MARKET.CHANGES")

  # Extra capital gains accounts which can be manipulated independently of asset revaluations
  INCOME_LONG        = initialize_account("INCOME.GAINS:INCOME.LONG")
  INCOME_SHORT       = initialize_account("INCOME.GAINS:INCOME.SHORT")

  # Taxable capital gains are in special accounts
  # Tax Adjustments have potentially been applied to these quantities
  LONG_GAINS    = initialize_account("SPECIAL.GAINS:LONG.GAINS")
  LONG_LOSSES   = initialize_account("SPECIAL.LOSSES:LONG.LOSSES")
  SHORT_GAINS   = initialize_account("SPECIAL.GAINS:SHORT.GAINS")
  SHORT_LOSSES  = initialize_account("SPECIAL.LOSSES:SHORT.LOSSES")
  #
  # Deferred Gains & Losses too (all long...)
  DEFERRED_GAINS  = initialize_account("SPECIAL.GAINS:DEFERRED.GAINS")
  DEFERRED_LOSSES = initialize_account("SPECIAL.LOSSES:DEFERRED.LOSSES")
}

#
function read_control_record(       now, i, x, p, is_check){
  # Clear initial values
  new_line()

  # Use the last recorded date
  now = ((-1 != Last_Time)?( Last_Time):( Epoch))

  # The control records - must be exact match
  is_check = FALSE
  x = trim($1)
  switch (x) {
    case "CHECK" :
      is_check = TRUE
    case "SET" :
      # Syntax for check and set is
      # CHECKSET, ACCOUNT, WHAT, X, # Comment
      # We need to rebuild the line into something more standard
      # DATE, ACCOUNT, WHAT, 0, X, # Comment
      #
      # Some special accounts are needed

      # Put DATE in 1st Field
      $1 = get_date(now)

      # Shuffle up fields
      for (i = NF; i > 3; i --)
        $(i+1) = $i

      # Set cost element field
      $4 = 0

      # Add one field
      NF ++

      # Check amount is set
      if (NF < 5) {
        $5 = 0
        NF = 5
      }

      # We can parse this line now
      i = parse_line(now)

      # Now either check or set the quantity
      assert(2 == i, $0 " : Unknown syntax for CHECK/SET actions")
      checkset(now, Account[1], Account[2], units, amount, is_check)
      break

    case "SET_BANDS" :
      # Set banded thresholds
      i = trim($2)
      assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")

      set_array_bands(now, SYMTAB[i], NF)
      break

    case "SET_ENTRY" :
      # Set a time-dependent array entry
      i = trim($2)
      assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
      p = strtonum($3)

      (SYMTAB[i][( now)] = ( p))
      break

    default: # This should not happen
      assert(FALSE, "Unknown Control Record <" i ">")
      break
  }

  # Get the next record
  return
}

# A function to set banded arrays (like tax bands)
function set_array_bands(now, bands, nf,     i, k) {

  # Negative threshold is required to force correct ordering
  for (i = 3; i < nf; i += 2) {
    k = strtonum($(i+1))
    bands[now][-k] = strtonum($i)

  }

  # Need to deal with case
  # %%,DATE,<ACTION>,15
  # i.e $(i+1) does not exist
  if (3 == nf) {
    bands[now][0] = strtonum($nf)

  }
}

function read_input_record(   t, n, a, threshold) {
  # Skip empty lines
  if ("" == $0)
    next

  # Optional values
  new_line()

  # Interpret the date
  t = read_date($1)

  # t should be positive
  assert(t != (-1), Read_Date_Error)

  # If this is a new time check if it is a new FY
  if (t > Last_Time) {
    # Are we done?
    if (t > Stop_Time + EOFY_Window)
      # this record will not be processed so
      # don't update Last_Time
      exit

    # We need to check for accounts changing from TERM=>CURRENT
    # Find the most recent threshold
    threshold = find_key(Threshold_Dates, t)

    #
    # Does if occur before now?
    # Comment this will not work if no transactions in that year
    # so need to ensure check at EOFY
    while (threshold > Last_Time) {
      # Which accounts does this key correpond to?
      for (a in Threshold_Dates[threshold]) {
        if (Threshold_Dates[threshold][a] > t) {
          # It is updated
          update_fixed_account(a, t, Threshold_Dates[threshold][a])

          # Make sure this won't be picked up again
          Threshold_Dates[threshold][a] = t
        }
      }

      # Get the next earlier maturity date
      threshold = find_key(Threshold_Dates, ((threshold) - 1))
    }

    # Update the Last_Time
    Last_Time = t

    while (FY_Time + EOFY_Window < t) {
      # Get each EOFY in turn
      eofy_actions(FY_Time)

      # The next financial year - unless we are stopping
      FY_Year ++

      # Update FY length
      FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)
      FY_Time = ((FY_Time) + one_year(FY_Time,  1))
    }
  } else # Check for semi-monotonicity
    assert(t == Last_Time, sprintf("Current entry %s is earlier than the previously recorded transaction %s", $0, get_date(Last_Time)))

  # Modular form - parse the line
  # returns number of accounts
  n = parse_line(t)

  # There are n accounts in each transaction
  if (n == 2)
    parse_transaction(t, Account[1], Account[2], units, amount)
  else if (n == 1) {
    # So far all this can do is initialize an account
    if (t >= Start_Time)
      printf "## Single Entry Transaction\n"
    print_transaction(t, Comments, Account[1], NULL, Write_Units, amount)
  } else {
    # A zero entry line - a null transaction or a comment in the ledger
    if (t >= Start_Time)
      print_transaction(t, Comments)
  }

  # Clean up the Account array
  delete Account

  # Were totals changed by this transaction?
  @Check_Balance_Function(t)
}

# Break out transaction parsing into a separate module
function parse_transaction(now, a, b, units, amount,

                           underlying_asset, credit_account,
                           swop, g,
                           array, n,
                           bought_parcel,
                           amount_taxed, use_name, taxable_account,
                           current_brokerage, gst,
                           member_name,
                           correct_order, tax_credits,
                           fields, number_fields) {
  # No member name set
  member_name = ""

  # No swop
  swop = ""

  # Start at zero
  number_fields = 0

  # a list of output fields is needed
  ((SUBSEP in fields)?(TRUE):(FALSE))

  # Special franking provisions
  if (a == TAX) {
    # Reduce franking
    adjust_cost(FRANKING, - amount, now)

    print_transaction(now, "Reduce Franking Balance", FRANKING, NULL, 0, amount)
   } else if (((b) ~ /^(ASSET\.CURRENT|LIABILITY)\.TAX[.:]/)) {
    # Increase franking
    adjust_cost(FRANKING, amount, now)

    print_transaction(now, "Increase Franking Balance", NULL, FRANKING, 0, amount)
  }

  # A SMSF member benefit
  if (((b) ~ ("^" ( "EXPENSE.NON-DEDUCTIBLE.BENEFIT") "[.:]"))) {
    # But there is another complication - this needs to consider
    # unrealized gains too => so important assets are priced accurately
    #
    set_cost(MARKET_CHANGES, sum_market_gains(((now) - 1)), ((now) - 1))

    # This will change proportions so update the profits first
    update_profits(now)

    # Expense must be account b
    amount_taxed = amount * update_member_liability(now, -amount, b)
    if (!((b) ~ ("[.:]" ( "TAXABLE") "(_|$)")) && !((b) ~ ("[.:]" ( "TAX-FREE") "(_|$)"))) {
      # Naming convention
      #
      # *:NAME.SUFFIX => *.NAME:NAME.SUFFIX.TAXABLE & *.NAME:NAME.SUFFIX.TAX-FREE
      #
      # Initialize accounts as needed
      use_name = sprintf("%s.%s:%s", substr(Parent_Name[b], 2), get_name_component(Leaf[b], 1), Leaf[b])
      taxable_account = initialize_account(sprintf("%s.TAXABLE", use_name))

      # Adjust costs for taxable account
      #
      adjust_cost(a, -amount_taxed, now)
      adjust_cost(taxable_account, amount_taxed, now)

      # Record this sub-transaction
      print_transaction(now, Comments, a, taxable_account, Write_Units, amount_taxed)

      # Replace account b with tax-free account
      b = initialize_account(sprintf("%s.TAX-FREE", use_name))

      # Adjust the amount for later processing
      amount -= amount_taxed
    }
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
    a = set_account_term(a, now)
  } else if (((b) ~ /^(ASSET|LIABILITY)\.TERM[.:]/))
    b = set_account_term(b, now)

  # Initially no optional fields
  number_fields = 0

  # Is this a franked transaction?
  if (((a) ~ ("^" ( "INCOME") "[.:]"))) {
    # Income accounts are caught to check for franking credits
    # Income must always be account a
    tax_credits = Real_Value[1]
    Real_Value[1] = 0

    # Get the underlying asset (if there is one)
    if (a in Underlying_Asset)
      underlying_asset = Underlying_Asset[a]
    else
      underlying_asset = FALSE

    # Foreign or franking credits - could be made more general?
    if (!(((tax_credits) <= Epsilon) && ((tax_credits) >= -Epsilon))) {
      # Keep an account of tax credits
      # We need the underlying asset to
      assert(underlying_asset, sprintf("Income account %s must have an underlying asset to receive tax credits", Leaf[a]))
      if (underlying_asset in Tax_Credits)
        credit_account = Tax_Credits[underlying_asset]
      else {
        # Create tax credits account - just in time
        # Type of credits account depends on the underlying asset
        # INCOME.DIVIDEND     => SPECIAL.OFFSET.FRANKING
        # INCOME.DISTRIBUTION => SPECIAL.OFFSET.FRANKING
        # INCOME.FOREIGN      => SPECIAL.OFFSET.FOREIGN
        #
        if (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION") "[.:]")))
          credit_account = Tax_Credits[underlying_asset] = initialize_account("SPECIAL.OFFSET.FRANKING:I_TAX." Leaf[underlying_asset])
        else if (((a) ~ ("^" ( "INCOME.FOREIGN") "[.:]")))
          credit_account = Tax_Credits[underlying_asset] = initialize_account("SPECIAL.OFFSET.FOREIGN:C_TAX." Leaf[underlying_asset])
        else
          assert(FALSE, sprintf("Can't link a tax credit account to income account %s", a))
      }

      # Adjust franking account and credit account
      adjust_cost(FRANKING, tax_credits, now)
      adjust_cost(credit_account, - tax_credits, now)
      print_transaction(now, ("# " Leaf[underlying_asset] " Tax Credits"), credit_account, FRANKING, 0, tax_credits)
    } else
      tax_credits = 0

    # Now LIC credits if any
    if (!(((Real_Value[2]) <= Epsilon) && ((Real_Value[2]) >= -Epsilon))) {
      # Always treated as positive
      adjust_cost(LIC_CREDITS, - Real_Value[2], now)
      print_transaction(now, ("# " Leaf[a] " LIC Deduction"), LIC_CREDITS, NULL, 0, Real_Value[2])
    }

    # Now check for a timestamp - this is the ex-dividend date if present
    if (underlying_asset) {
      if (Extra_Timestamp > Epoch) {
        # Save this for reporting
        fields[number_fields = 1] = get_date(Extra_Timestamp)

        # Assert that the ex-dividend date must not be later than the payment date
        assert(Extra_Timestamp <= now, "The ex-dividend date <" fields[1] "> must be before the payment date <" get_date(now) ">")

        # Save the ex-dividend date
        # But note that it must relate to an underlying asset
        #
        (Payment_Date[underlying_asset][( Extra_Timestamp)] = ( now))

      } else if (Qualification_Window && (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION.CLOSELY_HELD") "[.:]")))) {
        Extra_Timestamp = get_exdividend_date(underlying_asset, now)

        # This must exist
        assert((-1) != Extra_Timestamp, "Cannot find the ex-dividend date for <" Leaf[a] "> relating to the payment date <" get_date(now) ">")
        fields[number_fields = 1] = get_date(Extra_Timestamp)
      }

      # Clear the timestamp
      Extra_Timestamp = (-1)
    }

    # Now check for GST
    if (!(((GST_Claimable) <= Epsilon) && ((GST_Claimable) >= -Epsilon))) {
      # This is GST collected
      # The transaction itself will be posted later a => b
      # Need to adjust amount transacted
      amount -= (g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_H_TEMP__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount)
      print_transaction(now, ("# GST " Leaf[b]), GST, b, II, g)

      # GST claimed
      GST_Claimable = 0
    }

    # A SMSF member contribution
    if (((a) ~ ("^" ( "INCOME.CONTRIBUTION") "[.:]"))) {
      # This will change proportions so update the profits first
      update_profits(now)

      # Drop the INCOME prefix
      update_member_liability(now, amount, a)
    }
  } else if (((b) ~ ("^" ( "EXPENSE.NON-DEDUCTIBLE.DIVIDEND") "[.:]"))) {
    # A franking entity (eg company) can distribute franking credits
    tax_credits = Real_Value[1]
    Real_Value[1] = 0

    # Simplified version of above
    if (!(((tax_credits) <= Epsilon) && ((tax_credits) >= -Epsilon))) {
      # The credits are adjusted in the FRANKING balance
      adjust_cost(FRANKING,    - tax_credits, now)
      adjust_cost(FRANKING_PAID,  tax_credits, now)

      print_transaction(now, ("# " Leaf[a] " Franking Credits Distributed"), FRANKING, FRANKING_PAID, 0, tax_credits)
    } else
      tax_credits = 0
  }

  # A sale transaction
  if (units < 0) {
    # The asset being sold must be "a" but if an equity must be "b"
    #
    correct_order = ((a) ~ /^ASSET\.(CAPITAL|FIXED)[.:]/) && is_open(a, now)
    if (!correct_order) {
      correct_order = ((b) ~ /^EQUITY[.:]/) && is_open(b, now)
      if (correct_order) {
        swop = a; a = b; b = swop
        amount = - amount
      }
    }
    assert(correct_order, sprintf("%s => can't sell either %s or %s\n", $0, (Leaf[(a)]), (Leaf[(b)])))

    # Get the number of units to be sold in the special case of sell all
    if ("SELL" == Write_Units) {
      units = - ((__MPX_H_TEMP__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[a][0]):( 0))))
      Write_Units = sprintf("%10.3f", units)
    }

    # Get brokerage (if any)
    current_brokerage = Real_Value[1]
    Real_Value[1] = 0

    # Amount should be the consideration as recorded by the broker
    #
    # Impact of GST
    if (!(((GST_Claimable) <= Epsilon) && ((GST_Claimable) >= -Epsilon))) {
      # Two cases
      #   Not Present => Adjust Whole Amount
      if ((((current_brokerage) <= Epsilon) && ((current_brokerage) >= -Epsilon))) {
        # No Brokerage
        # A sale
        # A, B, -U,  (1 - g) * x
        # G, B,  0,        g * x
        g = - GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_H_TEMP__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount

        # This must be recorded
        # This reduces GST liability
        print_transaction(now, ("# GST " Leaf[a]), GST, b, II, -g)
        assert(FALSE, "GST Was levied on whole SELL transaction <" $0 ">")
      } else {
        # Brokerage Present => Adjust Brokerage
        # We Have A, B, -U, x - b, g
        # Produce A, B, -U, x - (1 - g) * x, # Note sign change with other case
        #         B, G,  0,           g * x, # Sign change engenders sense change in accounts
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_H_TEMP__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * current_brokerage

        # This must be recorded
        print_transaction(now, ("# GST " Leaf[a]), b, GST, II, g)
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
    adjust_cost(b, amount, now)

    # Did we swop? If so swop back
    if ("" != swop) {
      swop = a; a = b; b = swop
      amount = - amount
      swop = ""
    }


    # Record the transaction
    if (!(((current_brokerage) <= Epsilon) && ((current_brokerage) >= -Epsilon)))
      fields[++ number_fields] = sprintf("%.*f", (2), current_brokerage - g) # Always use 1st field

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if ((-1) != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Normally next field is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # Normal format is much the same - but number of fields is not fixed
    # A, B, -U, x + bg, (optional_fields), comment
    print_transaction(now, Comments, a, b, Write_Units, amount + g, fields, number_fields)


  } else if (units > 0) {
    # For a purchase the asset must be account "b"
    correct_order = ((b) ~ /^ASSET\.(CAPITAL|FIXED)[.:]/)
    if (!correct_order) {
      correct_order = ((a) ~ /^EQUITY[.:]/)
      if (correct_order) {
        swop = a; a = b; b = swop
        amount = - amount
      }
    }
    assert(correct_order, sprintf("%s => can't buy asset %s\n", $0, (Leaf[(b)])))

    # Normally fields[1] is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if ((-1) != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Is this a new depreciating asset?
    if (((b) ~ /^ASSET\.FIXED[.:]/) && !(b in Method_Name)) {
      # This is  the asset Lifetime
      fields[++ number_fields] = Lifetime[b]  = Real_Value[1]; Real_Value[1] = 0
      assert(!(((Lifetime[b]) <= Epsilon) && ((Lifetime[b]) >= -Epsilon)), sprintf("%s => Can't have a fixed asset %s with zero life", $0, (Leaf[(b)])))

      # We need the method name
      # Currently a choice of POOL, DV, or PC
      fields[++ number_fields] = Method_Name[b] = Depreciation_Type
    }

    # Allow for brokerage if required - note can't have brokerage with depreciation
    if (!(((Real_Value[1]) <= Epsilon) && ((Real_Value[1]) >= -Epsilon))) {
      current_brokerage = Real_Value[1]
      Real_Value[1] = 0
    }

    # Simply adjust cost of <a> by the whole amount
    adjust_cost(a, -amount, now)

    # Impact of GST
    if (!(((GST_Claimable) <= Epsilon) && ((GST_Claimable) >= -Epsilon))) {
      # Two cases
      #   No Brokerage Present => Adjust Whole Amount
      if ((((current_brokerage) <= Epsilon) && ((current_brokerage) >= -Epsilon)))
        # A  purchase
        # A, B, U, (1 - g) * x
        # A, G, 0,      g * x
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_H_TEMP__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount
      else
        # Brokerage Present => Adjust Brokerage
        # Produce A, B, U, x + (1 - g) * b
        #         A, G,  0,         g * b
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_H_TEMP__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * current_brokerage

      # Non-zero GST to be paid
      adjust_cost(GST, g, now)

      # This must be recorded
      print_transaction(now, ("# GST " Leaf[b]), a, GST, II, g)
      if ((((current_brokerage) <= Epsilon) && ((current_brokerage) >= -Epsilon)))
        assert(FALSE, "GST Was levied on whole BUY transaction <" $0 ">")
      GST_Claimable = 0
    } else
      g = 0

    # Buy units in asset (b) - this ignores impact of brokerage
    bought_parcel = buy_units(now, b, units, amount - current_brokerage, Parcel_Name, Extra_Timestamp)

    # Adjust the cost of this **parcel** for the impact of brokerage and GST
    adjust_parcel_cost(b, bought_parcel, now, current_brokerage - g,  II, FALSE)

    # Update parent entries for <b>
    update_cost(b, amount - g, now)

    # Did we swop? If so swop back
    if ("" != swop) {
      swop = a; a = b; b = swop
      amount = - amount
      swop = ""
    }

    # Record the transaction

    # Normal transactions
    # A, B, U, x, b - b*g, <optional-fields>, Comments
    # Record the adjustment due to brokerage and gst
    if (!(((current_brokerage) <= Epsilon) && ((current_brokerage) >= -Epsilon)))
      fields[++ number_fields] = sprintf("%.*f", (2), current_brokerage - g)
    print_transaction(now, Comments, a, b, Write_Units, amount - g, fields, number_fields)

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
    print_transaction(now, Comments, a, b, "(I)", amount)
  } else if (Extra_Timestamp > Epoch) {
    # The timestamp must be associated with a parcel
    fields[++ number_fields] = get_date(Extra_Timestamp)

    # A "parcel_timestamp" can indicate a maturity date
    # for a cash like term asset (eg a loan or a term deposit)
    # but only when an the asset is acquired and the timestamp is in the future

    # One account must be unitized or term limited
    if (((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)) {
      assert(!((b) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/),
               sprintf("%s Both %s and %s cannot be unitized when parcel timestamp [%s] is set",
               $0, (Leaf[(a)]), (Leaf[(b)]), get_date(Extra_Timestamp)))
      adjust_cost(a, -amount, Extra_Timestamp)
      adjust_cost(b,  amount, now)
    } else if (((b) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/)) {
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, Extra_Timestamp)
    } else if (((b) ~ /^(ASSET|LIABILITY)\.TERM[.:]/) || ((b) ~ /^(ASSET|LIABILITY)\.CURRENT[.:]/)) {
      # This is a term deposit or similar (eg a mortgage or loan issued by the fund)
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, now)
    } else
      assert(FALSE,
             sprintf("<%s> Either %s or %s must be a capital asset or a term asset when timestamp [%s] is set",
             $0, (Leaf[(a)]), (Leaf[(b)]), (Leaf[(b)]), get_date(Extra_Timestamp)))

     # Record the transaction
     print_transaction(now, Comments, a, b, Write_Units, amount, fields, number_fields)
  } else {
    # All Other Transactions
    # This must be an expense if GST is involved
    if (!(((GST_Claimable) <= Epsilon) && ((GST_Claimable) >= -Epsilon))) {
      # An expense
      # A, B, 0, (1 - g) * x
      # A, G, 0,      g * x
      g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_H_TEMP__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount

      # Non-zero GST to be paid - transfer from EXPENSE account
      adjust_cost(GST,  g, now)
      adjust_cost(b,   -g, now)

      # Record GST
      print_transaction(now, ("# GST " Leaf[a]), b, GST, II, g)
      GST_Claimable = 0
    }

    # Adjust costs
    adjust_cost(a, -amount, now)
    adjust_cost(b,  amount, now)

    # Catch manual depreciation
    #if (is_fixed(a))
    #  allocate_costs(a, now)

    # Record the transaction
    print_transaction(now, Comments, a, b, Write_Units, amount, fields, number_fields)
  }

  # Tidy up
  delete fields
}

# Set an account term for term limited assets and liabilities
function set_account_term(a, now) {
  # It is possible for a current account to have no term set

  # If the term is set make a note of it
  if (Real_Value[1] > 0)
   # It can change as long as it doesn't make a CURRENT asset non current
   (Account_Term[a][( now)] = ( Real_Value[1]))

  # At this stage term is assumed to be set in months
  if (Extra_Timestamp > now) { # Use the time stamp if it exists
    (Maturity_Date[a][( now)] = ( Extra_Timestamp))

    # Don't use real value again
    Real_Value[1] = 0
  } else if (Real_Value[1] > 0) {
    # Need to set the first maturity date - real value is the same as account term
    Extra_Timestamp = add_months(now, Real_Value[1])
    (Maturity_Date[a][( now)] = ( Extra_Timestamp))

    # Don't use real value again
    Real_Value[1] = 0
  } else if ((a in Maturity_Date) && now > ((__MPX_H_TEMP__ = find_key(Maturity_Date[a],  ((now) - 1)))?( Maturity_Date[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Maturity_Date[a][0]):( 0))))) {
    # Compute the maturity date
    Extra_Timestamp = add_months(now, ((__MPX_H_TEMP__ = find_key(Account_Term[a],  now))?( Account_Term[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Account_Term[a][0]):( 0)))))
    (Maturity_Date[a][( now)] = ( Extra_Timestamp))
  } else
    # No term was set
    return a

  # Ensure the name  of this account is correct
  #   X.TERM => non-current
  #   X.CURRENT => current
  return update_fixed_account(a, now, Extra_Timestamp)
}

#
# # Make sure current assets and liabilities are correctly identified
# Ensure the name  of this account is correct
#   X.TERM => non-current
#   X.CURRENT => current
function update_fixed_account(a, now, maturity,       active_account, x, threshold) {


  # Is this a current or non-current account?
  active_account = a
  if (maturity > ((now) + one_year(now,  1))) {
    # Never switch a current account to a non-current account
    assert(((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/), sprintf("Cannot convert %s to a non-current account with maturity %s",
                                a, get_date(maturity)))

    # Store the timestamp  - the first entry will be the last timestamp
    # Actually this is not needed...
    # Make sure the array exists and  the  entry is unique
    threshold = ((maturity) + one_year(maturity, -1)) # Not  the same as now!
    Threshold_Dates[threshold][SUBSEP] = 0
    delete Threshold_Dates[threshold][SUBSEP]

    # The time "now" is recorded since  the entry can be modified later
    (Threshold_Dates[threshold][( active_account)] = ( maturity))

  } else if (((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/)) {
    # Need to rename account
    # TERM => CURRENT
    active_account = gensub(/(\.TERM)([.:])/, ".CURRENT\\2", 1, a)

    # Create the new account is necessary
    active_account = initialize_account(active_account)

    # Now create a synthetic transaction
    # DATE, A, ACTIVE_ACCOUNT, 0, COST(A), # ....
    set_cost(active_account, get_cost(a, ((now) - 1)), now)
    set_cost(a, 0, now)


  }

  # Return the active account
  return active_account
}

# A wrapper function updates allocated profits when required ()
function update_profits(now,     delta_profits) {
  # Compute the profits that need to be allocated to members
  # These are the profits accumulated since the last time they were distributed to members
  delta_profits = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now)
  if (!(((delta_profits) <= Epsilon) && ((delta_profits) >= -Epsilon))) {
    # Update the Allocated Profits
    adjust_cost(ALLOCATED, delta_profits, now, FALSE)

    # Update the liabilities
    update_member_liability(now, delta_profits)
  }
}

# checking and setting
# Syntax
# date, COST, ACCOUNT, ....
function checkset(now, a, account, units, amount, is_check,
                       action, quantity) {
  # Save short name for action
  action = (Leaf[(a)])

  # First lets check
  if (is_check) {
    switch(action) {
      case "VALUE" :
        assert(((account) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/), sprintf("CHECK: Only assets or equities have a VALUE or PRICE: not %s\n", (Leaf[(account)])))
        quantity = get_value(account, now); break
      case "PRICE" :
        assert(((account) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/), sprintf("CHECK: Only assets or equities have a VALUE or PRICE: not %s\n", (Leaf[(account)])))
        quantity = ((__MPX_H_TEMP__ = find_key(Price[account],  now))?( Price[account][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Price[account][0]):( 0)))); break

      case "COST" : quantity = get_cost(account, now); break

      case "UNITS" : quantity = ((__MPX_H_TEMP__ = find_key(Total_Units[account],   now))?( Total_Units[account][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[account][0]):( 0)))); break
      default : assert(FALSE, sprintf("%s => I don't know how to check %s\n",
                                      (Leaf[(account)]), action))
    }

    # Is this a checkpoint?
    assert((((amount - quantity) <= Epsilon) && ((amount - quantity) >= -Epsilon)), sprintf("%s fails checkpoint %s[%s] => %.4f != %.4f\n",
                                                 action, (Leaf[(account)]), get_date(now), quantity, amount))

  } else {
    # is a setter
    switch(action) {
      case "VALUE" :
        # Valuations are  per  unit price
        # Was the number of units given?
        if (amount > Epsilon)
          amount /= units
        else # Just use the current cost if zero or negative units specified
          # If you want to set a zero value use PRICE instead
          amount = get_cost(account, now) / ((__MPX_H_TEMP__ = find_key(Total_Units[account],   now))?( Total_Units[account][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[account][0]):( 0))))

      case "PRICE" :
        # This is a single unit
        # Set the price per unit
        (Price[account][( now)] = ( amount))
      break

      case "COST" :
      case "SIZE" : # Just an alias for COST - looks neater for non cash special accounts
        # Override the account cost -> this can cause the accounts not to balance!
        set_cost(account, amount, now)
      break

      default : assert(FALSE, sprintf("SET: I don't know how to set <%s> for account %s\n",
                                      action, (Leaf[(account)])))
    }

    # All Done
    return
  }

}

# Update a member liability
# This can be (i)   a contribution - specified member, taxable or tax-free
#          or (ii)  a benefit - specified member
#          or (iii) allocation amongst members - no specificiation
#          or (iv)  allocation to or from the reserve - no specification
# This function keeps the member liability up to date for a SMSF
#
function update_member_liability(now, amount, a,
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

  # Note if a taxable share is driven negative the value should be transferred from the tax-free share

  # Get the appropriate member account
  if ("" == a)
    member_id = ""
  else # This will be an account - but when not a CONTRIBUTION it will be a parent account
    member_id = get_member_name(a, now, amount)



  # Allocation to the liability accounts
  # Either no id is given - distribute amongst all accounts
  # Or a parent account - distribute amongst its offspring
  # Or a specific account - distribute solely to that account
  taxable_share = sum_total = sum_share = 0

  # Normalize amounts
  if (member_id in Member_Liability) { # Exact match - a contribution
    # Adjust the liability
    adjust_cost(member_id, - amount, now)
    if (member_id ~ /TAXABLE/)
      taxable_share = 1.0


  } else { # Get totals
    # We still get the share from each account
    # Don't use the accumulated totals because (rarely) a negative account balance will break the proportioning
    # Also since  the order of transactions on a particular day is not defined use just_before() to compute proportions
    for (member_account in Member_Liability)
      if (!member_id || is_ancestor(member_id, member_account)) {
        share[member_account] = x = get_cost(member_account, ((now) - 1))
        sum_total += x

        # Compute what fraction of the allocation was taxable
        if (member_account ~ /TAXABLE/)
          taxable_share += x
      }

    # Normalize taxable share
    taxable_share /= sum_total

    # There are two possibilities here -
    #   No member id => profit/loss everything goes to/from TAXABLE accounts
    #   A parent id  => proportioning rule applies
    # Update the liabilities - but only the target accounts
    for (member_account in share) {
      x = share[member_account] / sum_total

      # Target account
      if (!member_id)
        target_account = Member_Liability[member_account]
      else
        target_account = member_account

      # Adjust the liability
      adjust_cost(target_account, - x * amount, now)

    } # End of exact share

    # Tidy up
    delete share
  } # End of allocation



  # return proportion that was taxable
  return taxable_share
}

# Obtain the member account
function get_member_name(a, now, x,   member_name, member_account, target_account, subclass, contribution_tax) {
  # This obtains the liability account that needs to be modified
  # In more detail INCOME.CONTRIBUTION.SUBCLASS:NAME.SUFFIX => LIABILITY.MEMBER.NAME:NAME.SUBCLASS
  # And            EXPENSE.NON-DEDUCTIBLE.BENEFIT:NAME.SUFFIX => *LIABILITY.MEMBER.NAME
  # In fact        X.Y:NAME.SUFFIX => *LIABILITY.MEMBER.NAME

  # Get the member name
  member_name = get_name_component(Leaf[a], 1) # first component

  # A member liability account can only be created by a contribution
  if (((a) ~ ("^" ( "INCOME.CONTRIBUTION") "[.:]"))) {
    # Identify the "subclass" - use Parent_Name because it is always available
    subclass = get_name_component(Parent_Name[a], 0) # last component

    # If a link is made in a "MEMBER" array to each members liabilities
    # then there is no need to identify this as a member liability in the
    # account name
    member_account = initialize_account(sprintf("LIABILITY.MEMBER.%s:%s.%s", member_name, member_name, subclass))

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
  } else {
    # Return the parent account
    member_account = "*LIABILITY.MEMBER." member_name
    assert(member_account in Parent_Name, "<" $0 "> Unknown account <" member_account ">")
  }

  # Return the account
  return member_account
}

# Final processing
END {
  if (_assert_exit == 1)
    exit 1

  # Check version consistency
  assert(MPX_Version == Current_Version,     "Inconsistent snapshot file:\n\tExpected Version => " Current_Version " Found Version => " MPX_Version)

  # Delete empty accounts
  # Filter out data entries that were added by PP or QQ records
  # that do not overlap with the the holding period
  filter_data(Last_Time)

  # Make sure any eofy transactions are recorded
  # This loop will happen at least once
  if (Last_Time > Stop_Time)
    eofy_actions(Stop_Time)
  else if (Stop_Time < Future) {
    # We need to produce statements
    do {
      # Get each EOFY in turn
      eofy_actions(FY_Time)

      # The next financial year
      FY_Year ++

      # Update FY length
      FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)
      FY_Time = ((FY_Time) + one_year(FY_Time,  1))
    } while (FY_Time + EOFY_Window < Last_Time)

    # Fix FY_Time so that the snapshot is accurate
    FY_Year --
    FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)

    FY_Time = ((FY_Time) + one_year(FY_Time, -1))
  }

  # Write out code state - it sould be ok to do this after all processing now
  if (Write_State) {
    write_state(Array_Names, Scalar_Names)

    # The last line is (oddly enough) when the journal starts - this allows initialization to occur when file read back in
    if (!Write_Variables)
      printf "START_JOURNAL\n" > Write_State
  }
} #// END
#

#
# Filter Data
#
# Filter out irrelevant data - data that is out-of-range
# can arise from reading p&q records
#
function filter_data(now,      array_names, name) {
  # Which data arrays are to be filtered
  if (!split(Filter_Data, array_names, " "))
    return # Nothing to filter

  # Filter the data arrays
  for (name in array_names)
    filter_array(now, SYMTAB[array_names[name]], array_names[name])
}

# Handle each array in turn
function filter_array(now, data_array, name,
                           a, p, start_block, end_block, block_id,
                           stack, key) {


  # list holding "blocks" - ie non-overlapping holding periods
  # Each block is preceeded and/or followed by "gaps"
  for (a in Leaf)
    if ((a in data_array) && ((a) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/))
      if ((Held_From[(a)][0] > Epoch)) {
        # Get each parcel in turn and list the contiguous blocks of time held
        start_block = Held_From[a][0]
        end_block = Held_Until[a][0]
        block_id = 0
        for (p = 1; p < Number_Parcels[a]; p ++) {
          # This starts a new holding block if the purchase date is after the current end date
          if (((Held_From[a][p] -  end_block) > 0)) {
            # Filter the old block


            # # Check the data against each block
            for (key in  data_array[a]) {  if (key -  end_block > 0)    continue;  if (key -  start_block >= 0)    stack[key] =  data_array[a][key];  else    break;}

            # Remove anything kept to speed up processing
            for (key in stack)
              delete data_array[a][key]

            # A new block
            block_id ++
            start_block = Held_From[a][p]
            end_block = Held_Until[a][p]
          } else if (((Held_Until[a][p] -  end_block) > 0)) # extend the old block
            end_block = Held_Until[a][p]

          # If this parcel is open we have completed all possible blocks
          if ((Held_Until[(a)][( p)] > ( now)))
            break
        } # End of each parcel p

        # The last holding block

          # Check the data against each block
          for (key in  data_array[a]) {  if (key -  end_block > 0)    continue;  if (key -  start_block >= 0)    stack[key] =  data_array[a][key];  else    break;}


        # Copy the kept items back
        for (key in stack)
          data_array[a][key] = stack[key]
        delete stack

      } else # Never held!
        unlink_account(a)

    # End of each asset a
}

# The current value of an asset
function get_value(a, now) {
  # Depreciating assets are different
  if (((a) ~ /^ASSET\.CAPITAL[.:]/))
    return (((__MPX_H_TEMP__ = find_key(Price[a],  now))?( Price[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Price[a][0]):( 0)))) * ((__MPX_H_TEMP__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[a][0]):( 0)))))

  # Just the cost
  return get_cost(a, now)
}

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
  while (((key -  now) > 0)) {
    # We will need the next key
    next_key = find_key(Qualified_Units[a], ((key) - 1))



    # How many provisionally qualified units are at the key entry?
    dq = ((Qualification_Window)?(  ((__MPX_H_TEMP__ = find_key(Qualified_Units[a],   key))?( Qualified_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Qualified_Units[a][0]):( 0))))):( ((__MPX_H_TEMP__ = find_key(Total_Units[a],    key))?( Total_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[a][0]):( 0)))))) - ((Qualification_Window)?(  ((__MPX_H_TEMP__ = find_key(Qualified_Units[a],   next_key))?( Qualified_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Qualified_Units[a][0]):( 0))))):( ((__MPX_H_TEMP__ = find_key(Total_Units[a],    next_key))?( Total_Units[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Total_Units[a][0]):( 0))))))

    # Assert all parcels must be positive
    assert(((dq) >  Epsilon), sprintf("Found a non-positive provisional parcel of units %s[%s] => %.3f",
            (Leaf[(a)]), get_date(key), dq))

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
    if ((((du) <= Epsilon) && ((du) >= -Epsilon)))
      break

    # Get the next key
    key = next_key
  } # End of while each key in the window

  # Do not adjust fully qualified units...
  # Instead add a negative parcel?
  # Do not adjust qualified parcels since these are needed for historical comparisons?
  if (((du) >  Epsilon)) {
    sum_entry(Qualified_Units[a], - du, now) # Ok to make a non-proviosional negative parcel

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

# This is the hard way to do it
# another way is return (Cost_Basis_change - market_value_change)
# this way is being used ATM for consistency checking reasons
function get_unrealized_gains(a, now,
                              current_price, p, gains, x) {
  if ((!is_open((a), ( now))))
    return 0 # NO unrealized gains

  # Sum the total value of the asset
  gains = 0
  current_price = ((__MPX_H_TEMP__ = find_key(Price[a],  now))?( Price[a][__MPX_H_TEMP__]):( ((0 == __MPX_H_TEMP__)?( Price[a][0]):( 0))))



  # Unrealized gains held at time t are those in unsold parcels
  for (p = 0; p < Number_Parcels[a]; p++) {
    if (((Held_From[a][p] -  now) > 0)) # All further transactions occured after (now)
      break # All done
    if ((Held_Until[(a)][( p)] > ( now))) # This is an unsold parcel at time (now)
      # If value > cash_in this is an unrealized gain
      gains += sum_cost_elements(Accounting_Cost[a][p], now) - Units_Held[a][p] * current_price
  }



  # The result
  return gains
}

# Buy a parcel of u units at the cost of x
function buy_units(now, a, u, x, parcel_tag, parcel_timestamp,
                                             last_parcel, p) {


  # Some units are bought
  assert(!(((u) <= Epsilon) && ((u) >= -Epsilon)), sprintf("buy_units[%s] : can't buy zero units", $0))

  # Override timestamp
  if (parcel_timestamp >= Epoch)
    now = parcel_timestamp

  # Make a new parcel
  last_parcel = new_parcel(a, u, x, now, parcel_tag)

  # Update units
  sum_entry(Total_Units[a],  u,  now)

  # Also update qualified units - buying is easy
  if (((a) ~ /^ASSET\.CAPITAL[.:]/) && Qualification_Window) {
    sum_entry(Qualified_Units[a], u, now + 0.5 * Qualification_Window)

  }

  # Debugging


  # Buy u units for x
  u = Units_Held[a][last_parcel]
  x = sum_cost_elements(Accounting_Cost[a][last_parcel], now) # This must be just the cash paid

  # Passive revaluation
  p = x / u


  # Set the new price
  (Price[a][( now)] = ( p))

  # Return the parcel id
  return last_parcel
}

# Create or modify a parcel
function new_parcel(ac, u, x, now, parcel_tag,        last_parcel, i) {
  # Purchases made at the same time can be averaged
  # So only increment the parcel counter if a new parcel name specified
  last_parcel = Number_Parcels[ac] - 1
  if ((now > Held_From[ac][last_parcel]) || "" != parcel_tag) {
    Number_Parcels[ac] ++
    last_parcel ++

    # This adds a new parcel - always cost element I
    # We have to initialize  this parcels cost elements
    Accounting_Cost[ac][last_parcel][0][Epoch] = 0
    zero_costs(Accounting_Cost[ac][last_parcel], Epoch)

    # Ditto for tax adjustments
    Tax_Adjustments[ac][last_parcel][0][Epoch] = 0
    zero_costs(Tax_Adjustments[ac][last_parcel], Epoch)

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


  # Try adjusting units now...
  sum_entry(Total_Units[ac],  -u,  now)

  # For a depreciating asset with multiple parcels (apart from a pooled asset) then
  # the proportion of the asset being sold depends on the
  # original cost of each unit; not on the number of units
  proportional_cost = FALSE
  if (((ac) ~ /^ASSET\.FIXED[.:]/)) {
    # Is this asset's depreciation upto date?
    # Depreciate

    if (now > FY_Time) {
      #t = yesterday(FY_Time, HOUR)
      t = ((FY_Time) - 1)
      catch_up_depreciation = depreciate_now(ac, t)
      if (!(((catch_up_depreciation) <= Epsilon) && ((catch_up_depreciation) >= -Epsilon))) {
        update_cost(ac, - catch_up_depreciation, t)

        # Balance accounts
        adjust_cost(DEPRECIATION, catch_up_depreciation, t)

        # Print the transaction
        print_transaction(t, "Catch-Up Depreciation", ac, DEPRECIATION, "(D)", catch_up_depreciation)

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
      if (!(((p) <= Epsilon) && ((p) >= -Epsilon)))
        proportional_cost = x / p
      else
        new_price = 0
    }
  } else {
    # Not depreciating
    # Also update qualified units - selling is harder
    if (Qualification_Window && ((u) >  Epsilon))
      sell_qualified_units(ac, u, now, 0.5 * Qualification_Window)

    # Sell u units for x
    # Both x & u are positive by design
    new_price = x / u

    # Can set the price of a normal asset
    (Price[ac][( now)] = ( new_price))
  }

  # Default assumption is first-in-first-out (FIFO)
  # But can be overriden if specific parcel given
  p = 0
  while (u > 0 && p < Number_Parcels[ac]) {
    # Skip sold parcels - including those sold today
    if ((Held_Until[(ac)][( p)] <= ( now))) {
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
  assert((((u) <= Epsilon) && ((u) >= -Epsilon)), sprintf("sell_units: Failed to sell the requested %d units of %s", u, (Leaf[(ac)])))

  # Update parent sums
  update_cost(ac, -x, now)
}

function sell_parcel(a, p, du, amount_paid, now,      i, is_split) {
  # The sale date
  Held_Until[a][p] = now

  # No parcel split yet
  is_split = FALSE

  # Amount paid


  # Check for an empty parcel - allow for rounding error
  if ((((Units_Held[a][p] - du) <= Epsilon) && ((Units_Held[a][p] - du) >= -Epsilon)))
    # Parcel is sold off
    Units_Held[a][p] = du
  else { # Units remain - parcel not completely sold off


    # Shuffle parcels up by one
    for (i = Number_Parcels[a]; i > p + 1; i --)
      # Copy the parcels
      copy_parcel(a, i - 1, i)

    # At this point we need to split parcels p & p + 1
    split_parcel(a, p, du)
    is_split = TRUE

    # One extra parcel
    Number_Parcels[a] += 1
  } # End of if splitting a parcel

  # The sale price
  # This is always recorded as cost_element 0 since it is not actually a true cost
  # This must be recorded as cash flowing out of the account
  # A parcel is only ever sold once so we can simply set the cost
  (Accounting_Cost[a][p][0][( now)] = ( -amount_paid))



  # Save realized gains
  if (!((a) ~ /^ASSET\.FIXED[.:]/))
    save_parcel_gain(a, p, now)
  else
    sell_fixed_parcel(a, p, now)

  # Was a parcel split
  return is_split
} # End of if non-zero Parcel

#
# When a parcel of a fixed asset is sold
# it changes the depreciation amounts
# these are effected at time now
function sell_fixed_parcel(a, p, now,     x) {
  # A depreciating asset will neither have capital gains nor losses
  # so it will have accounting cost zero

  # Only need to save depreciation or appreciation
  x = sum_cost_elements(Accounting_Cost[a][p], now) # The cost of a depreciating asset

  # Set it to zero (use element 0)
  sum_entry(Accounting_Cost[a][p][0], -x, now)

  # We need to adjust the asset sums by this too
  update_cost(a, -x, now)



  # Any excess income or expenses are recorded
  if (((x) >  Epsilon)) # This was a DEPRECIATION expense
    adjust_cost(SOLD_DEPRECIATION, x, now)
  else if (((x) < -Epsilon)) # This was APPRECIATION income
    adjust_cost(SOLD_APPRECIATION, x, now)
}

# Save a capital gain
# The gain was made at time "now" on at asset purchased at "date_purchased"
function save_parcel_gain(a, p, now,    x, held_time) {
  # Get the held time
  held_time = get_held_time(now, Held_From[a][p])

  # Accounting gains or Losses - based on reduced cost
  # Also taxable losses are based on the reduced cost...
  x = sum_cost_elements(Accounting_Cost[a][p], now)
  if (((x) >  Epsilon)) {
    adjust_cost(REALIZED_LOSSES, x, now)

    # Taxable losses are based on the reduced cost
    if (held_time >= 31622400)
      adjust_cost(LONG_LOSSES, x, now)
    else
      adjust_cost(SHORT_LOSSES, x, now)
  } else if (((x) < -Epsilon))
    adjust_cost(REALIZED_GAINS, x, now)

  # Taxable gains
  # after application of tax adjustments
  # This works if tax adjustments are negative
  x -= sum_cost_elements(Tax_Adjustments[a][p], now)

  # Taxable Gains are based on the adjusted cost
  if (((x) < -Epsilon)) {
    # Taxable losses are based on the reduced cost
    if (held_time >= 31622400)
      adjust_cost(LONG_GAINS, x, now)
    else
      adjust_cost(SHORT_GAINS, x, now)
  }
}

# Copy and split parcels
function copy_parcel(ac, p, q,     e, key) {


  # Copy parcel p => q
  Units_Held[ac][q]  = Units_Held[ac][p]
  Held_From[ac][q] = Held_From[ac][p]
  Held_Until[ac][q] = Held_Until[ac][p]

  # Copy all entries
  # Note keys will not match so need to delete old entries from parcel q
  delete Accounting_Cost[ac][q] # Delete old entries
  delete Tax_Adjustments[ac][q]
  for (e in Accounting_Cost[ac][p])
    for (key in Accounting_Cost[ac][p][e])
        Accounting_Cost[ac][q][e][key] = Accounting_Cost[ac][p][e][key]
  for (e in Tax_Adjustments[ac][p])
    for (key in Tax_Adjustments[ac][p][e])
      Tax_Adjustments[ac][q][e][key]  = Tax_Adjustments[ac][p][e][key]
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

  # The New Adjustments
  # Need to delete old arrays because
  # they will exist with different keys
  delete Accounting_Cost[ac][p + 1] # Delete old entries
  delete Tax_Adjustments[ac][p + 1]

  # The very first key
  for (e in Accounting_Cost[ac][p])
    for (key in Accounting_Cost[ac][p][e])
        Accounting_Cost[ac][p + 1][e][key] = fraction_kept * Accounting_Cost[ac][p][e][key]
  for (e in Tax_Adjustments[ac][p])
    for (key in Tax_Adjustments[ac][p][e])
      Tax_Adjustments[ac][p + 1][e][key]  = fraction_kept * Tax_Adjustments[ac][p][e][key]

  for (e in Accounting_Cost[ac][p])
    for (key in Accounting_Cost[ac][p][e])
        Accounting_Cost[ac][p][e][key] -= Accounting_Cost[ac][p + 1][e][key]
  for (e in Tax_Adjustments[ac][p])
    for (key in Tax_Adjustments[ac][p][e])
      Tax_Adjustments[ac][p][e][key]  -= Tax_Adjustments[ac][p + 1][e][key]
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
function check_balance(now,        sum_assets, sum_liabilities, sum_equities, sum_expenses, sum_income, sum_adjustments, balance, show_balance) {
  # The following should always be true
  # Assets - Liabilities = Income + Expenses
  # This compares the cost paid - so it ignores the impact of revaluations and realized gains & losses
  sum_assets =  get_cost("*ASSET", now) - get_cost("*INCOME.GAINS.REALIZED", now) - get_cost("*EXPENSE.LOSSES.REALIZED", now) - get_cost("*EXPENSE.UNREALIZED", now)

  # Work out the total assets etc
  sum_liabilities = - get_cost("*LIABILITY", now)
  sum_equities    = - get_cost("*EQUITY", now)
  sum_expenses    = - get_cost("*EXPENSE", now)
  sum_income      = - get_cost("*INCOME", now)
  sum_adjustments =   get_cost("*SPECIAL.BALANCING", now)

  # The balance should be zero
  balance = sum_assets - (sum_liabilities + sum_equities + sum_income + sum_expenses + sum_adjustments)


  # No default printing
  show_balance = FALSE


  # Is there an error?
  if (!(((balance) <= Epsilon) && ((balance) >= -Epsilon))) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > "/dev/stderr"
    show_balance = TRUE
  }

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > "/dev/stderr"
    printf "\tAssets      => %20.2f\n", sum_assets > "/dev/stderr"
    printf "\tIncome      => %20.2f\n", sum_income > "/dev/stderr"
    printf "\tExpenses    => %20.2f\n", sum_expenses > "/dev/stderr"
    printf "\tLiabilities => %20.2f\n", sum_liabilities > "/dev/stderr"
    printf "\tEquities    => %20.2f\n", sum_equities > "/dev/stderr"
    printf "\tAdjustments => %20.2f\n", sum_adjustments > "/dev/stderr"
    printf "\tBalance     => %20.2f\n", balance > "/dev/stderr"
    assert((((balance) <= Epsilon) && ((balance) >= -Epsilon)), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
