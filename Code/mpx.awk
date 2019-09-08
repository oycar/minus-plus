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

@load "filefuncs"


# // Control Logging




# // Control Export format




# // Logic conventions



# // Output Streams




# //




# // Some constants










# // Default Import Values





# // Output Date Formats





# // Default Reports


# // Default Reports














# // Default Asset Prefix for Price Lists



# // The Epoch and minimum time difference









# // Day Number For Feb 29



# // Reserved Classes


# // Useful inline functions - this may be overdoing it






#
# // Useful shorthands for various kinds of accounts






# // Fixed asset





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
  # since Payment_Date[ex_dividend_date] => now-ish
  if (a in Payment_Date) {

    # Get the most recent payment date
    value = ((__MPX_KEY__ = find_key(Payment_Date[a],  now))?( Payment_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Payment_Date[a][0]):( 0))))
    discrepancy = now - value

    # The value cannot be later than the current time "now"
    if (value > now) {
      Read_Date_Error = "Payment date is later than current date"
      return (-1)
    } else if ((((discrepancy) <= Epsilon) && ((discrepancy) >= -Epsilon)))
      return (__MPX_KEY__)

    # Some times dividends are paid out of order, for example
    # a special or buyback dividend might have an extra
    # long qualification period - so look ahead more dividends
    # until the discrepancy increases
    #
    key = (__MPX_KEY__)
    while (key) {
      value = ((__MPX_KEY__ = find_key(Payment_Date[a],  ((key) - 1)))?( Payment_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Payment_Date[a][0]):( 0))))
      if ((now - value) > discrepancy)
        # A worse match
        break

      # A better match
      discrepancy = now - value
      if ((((discrepancy) <= Epsilon) && ((discrepancy) >= -Epsilon)))
        return (__MPX_KEY__)

      # Save  this match
      key = (__MPX_KEY__)
    }

    # Best match was key
    if (discrepancy > 604800) {
      Read_Date_Error = "Failed to find a payment date within one week of current date"
      return (-1)
    }

    # Return it
    return key
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
    set_array(SYMTAB[Variable_Name], Variable_Keys, 1, nf - 1, value, (0))
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
    flag = (0)
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
      flag = (1)
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
    ((SUBSEP in Key_Index)?((1)):((0)))
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

# Clear global values ready to read a new input record
function new_line() {
  Extra_Timestamp = (-1)
  Parcel_Name = ""
  Real_Value[1] = Real_Value[2] = 0
  Tax_Adjustment = (0)
  Cost_Element = COST_ELEMENT # The default value
  Automatic_Depreciation = (0)
  GST_Claimable = 0
  Depreciation_Type = ""
  Comments = ""

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
function parse_line(now,    i, j, x, number_accounts) {
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
  #    units == 0 => Cost Base element II (the default)
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
      x = parse_optional_string($i, (1))

      # Treat as a comment
      if (x)
        Comments = (("" == Comments)?(  x):( (("" ==  x)?( Comments):( (Comments  ", "  x)))))
    }

    # Increment i
    i ++
  }

  # Comments should be signified with an octothorpe
  if (Comments !~ /^#/)
    Comments = (("" == "# ")?(  Comments):( (("" ==  Comments)?( "# "):( ("# "  ", "  Comments)))))

  # Documents can be added as comments
  # Some special document names are supported
  # So for example [<Buy>] expands to ABC Buy YYYY Mon
  # and            [<Chess.x>] expands to ABC Chess YYYY Mon DD
  for (x in Documents) {
    delete Documents[x]

    # Parse this document name
    i = parse_document_name(x, now)

    # Add the parsed name to the comments
    Comments = (("" == Comments)?(  i):( (("" ==  i)?( Comments):( (Comments  ", "  i)))))
  }

  # All done - return record type
  return number_accounts
}


# A document name may contain a filetype suffix
function parse_document_name(name, now,    prefix, suffix, account_name, array, suffix_set) {

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
  #  If the colon is needed in a string literal a different Document_Shortcut code can be set in the Journal file
  #
  # <<, Document_Shortcut, =,>>
  # 2008 Jun 30, INCOME.FOREIGN:FOR.PXUPA.ASX,          CASH,          0,      726.63, [PX:UPA Distribution=], # PX:UPA distribution
  # <<, Document_Shortcut, :,>>

    # Split the code name
    # Use name component because we want the capture all the components apart from the first
    if (split(name, array, Document_Shortcut) > 1) {
      suffix = get_name_component(name, 2, -1, array)
      suffix_set = (1)
    } else
      suffix_set = (0)

    prefix = get_name_component(name, 1, 1, array)

    #
    switch (prefix) {
      case "B" :
      case "S":
      case "H":

        # Is this a buy, sell or holding statement?
        if (units < 0 || "SELL" == Write_Units) {
          prefix = (("H" == prefix)?( "Holding Statement"):( "Sell"))
          account_name = get_name_component(Leaf[Account[1]], 1)
        } else {
          prefix = (("H" == prefix)?( "Holding Statement"):( "Buy"))
          account_name = get_name_component(Leaf[Account[2]], 1)
        }

        # Add the date
        prefix = prefix " " get_date(now, ("%Y %b")    )
      break;;

      case "I":
      case "D": # Income
        if (((Leaf[Account[1]]) ~ /^(DIV|DIST|FOR)\./))
          account_name = get_name_component(Leaf[Account[1]], 2)
        else
          account_name = get_name_component(Leaf[Account[1]], 1)

        # The second component of the account name (unless this is accrued income)
        if (((Account[1]) ~ ("^" ( "ASSET.CURRENT.ACCRUED") "[.:]")))
          prefix = "Distribution " get_date(now, ("%Y %b")    )
        else
          prefix = tolower(get_name_component(Account[1], 2)) " " get_date(now, ("%Y %b")    )
        break;;

      case "C":
      case "E": # Expense or Cost
        account_name = get_name_component(Leaf[Account[2]], 1)

        # The second component of the account name
        prefix = tolower(get_name_component(Account[2], 2)) " " get_date(now, ("%Y %b")    )
        break;;

      case "T": # Annual Tax Statement
        account_name = get_name_component(Leaf[Account[2]], 1)
        prefix = "Annual Tax Statement " (strftime("%Y", (now), UTC) + 0)
        break;;

      default: # no match - assume this is a literal string
        # When a distinct suffix is present add the date
        if (suffix_set)
          prefix = (("" == prefix)?(  get_date(now, ("%Y %b")    )):( (("" ==  get_date(now, ("%Y %b")    ))?( prefix):( (prefix  " "  get_date(now, ("%Y %b")    ))))))

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



# Is units a numerical value?
function parse_units(u, units,      len) {
  units = strtonum(u)
  if (0 == units) {
    # Need to examine the original string more closely
    len = length(u)

    # Brackets?
    if (u ~ /^()/ && u ~ /)$/) {
      # This is probably a tax adjustment
      Tax_Adjustment = (1)

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
        Tax_Adjustment = (0)
        break
      case "D" : # Depreciation
        Cost_Element = I # First cost element
        Tax_Adjustment = Automatic_Depreciation = (1)
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
        Tax_Adjustment = (0)
    }

    # Ensure units are still zero
    units = 0
  } else { # BUY or SELL transaction
    # Units should be numerical
    if (units !~ /^[0-9\.\-]+$/)
      # If we get here this assertion will fail
      assert((0), "<" $0 "> Unexpected cost element field syntax <" units ">")

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
    ((SUBSEP in optional_fields[field_rank])?((1)):((0)))
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
  for (p = 0; p < Number_Parcels[a]; p ++) {
    if (Held_From[a][p] > now)
      break
    if ((Held_Until[(a)][( p)] > ( now)))
      return (1)
  }
  return (0)
}

# Is an account a an ancestor of another account b?
function is_ancestor(a, b,    p) {
  if (!((a) ~ /^*/))
    return (0)

  # Check
  p = Parent_Name[b]
  while ("" != p) {
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
      adjustment = x / ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))

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
    return ((__MPX_KEY__ = find_key(Cost_Basis[a],  now))?( Cost_Basis[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Cost_Basis[a][0]):( 0))))

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
  adjust_cost(a, new_cost - initial_cost, now, (0))
}

# Unrealized or market gains
function sum_market_gains(now,     sum, a) {
  sum = 0

  # Cash-like assets can be ignored
  for (a in Leaf)
    if (((a) ~ /^ASSET\.CAPITAL[.:]/) && is_open(a, now))
      # The asset must be active
      sum += get_cost(a, now) - ((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0)))) * ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))

  # All done - negative values are gains
  return sum
}

# Sum  the cost elements
function sum_cost_elements(array, now,     sum_elements, e) {


  sum_elements = 0
  for (e in array) # Should this include [0] or not?
    sum_elements += ((__MPX_KEY__ = find_key(array[e],  now))?( array[e][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( array[e][0]):( 0))))
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
        sum_cost += ((__MPX_KEY__ = find_key(Accounting_Cost[a][i][element],  now))?( Accounting_Cost[a][i][element][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][i][element][0]):( 0))))
    }
  }

  return sum_cost
}

# The parcel cost
function get_parcel_element(a, p, element, now, adjusted) {
  # Adjusted or reduced cost?
  if (adjusted)
    # The adjusted parcel cost
    adjusted = (((__MPX_KEY__ = find_key(Tax_Adjustments[a][ p][ element],  ( now)))?( Tax_Adjustments[a][ p][ element][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][ p][ element][0]):( 0)))))
  else
    adjusted = 0

  # This elements costs
  return ((__MPX_KEY__ = find_key(Accounting_Cost[a][p][element],  now))?( Accounting_Cost[a][p][element][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][p][element][0]):( 0)))) - adjusted
}

# The initial cost
function get_cash_in(a, i, now) {

  # Is the account open?
  if (now >= Held_From[a][i])
    # Yes - always element I
    return ((__MPX_KEY__ = find_key(Accounting_Cost[a][i][I],  Held_From[a][i]))?( Accounting_Cost[a][i][I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Accounting_Cost[a][i][I][0]):( 0)))) # The Held_From time ensures  that later element I costs do not impact the result

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

# Print out transactions
# Generalize for the case of a single entry transaction
function print_transaction(now, comments, a, b, u, amount, fields, n_field,     matched) {
  if (now > Stop_Time)
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
  # Actually let's allow this - but only in very restricted casee of the account never having been used
  # This might be possible some day...
  leaf_name = array[2]
  if ((leaf_name in Long_Name)) {
    if (Leaf[Long_Name[leaf_name]] == leaf_name) {
      # If the existing account is new (unused) it can be deleted
      assert(("" == first_key(Cost_Basis[Long_Name[leaf_name]])), sprintf("Account name %s: Leaf name[%s] => %s is already taken", account_name, leaf_name, Long_Name[leaf_name]))

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
    if (((Leaf[account_name]) ~ /^(DIV|DIST|FOR)\./)) {
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
        if (((((((__MPX_KEY__ = find_key(Tax_Adjustments[a][ p][ I],  ( now)))?( Tax_Adjustments[a][ p][ I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][ p][ I][0]):( 0)))))) <= Epsilon) && (((((__MPX_KEY__ = find_key(Tax_Adjustments[a][ p][ I],  ( now)))?( Tax_Adjustments[a][ p][ I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][ p][ I][0]):( 0)))))) >= -Epsilon)))
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
  # Now get the tax due on the whole sum
  current_key = find_key(Tax_Bands, now)
  last_threshold = 0

  # When the tax left is zero or negative it must be the first band
  if (!((tax_left) >  Epsilon))
    return tax_left / Tax_Bands[current_key][last_threshold]

  # Now get the tax due on the whole sum
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
  ##if (now > Start_Time)
  ##  EOFY = STDERR

  # past is referred to now
  past = ((now) - one_year(now, -1))



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

  # Print Imputation report
  @Imputation_Report_Function(now, past, Show_Extra)

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
  allocate_second_element_costs(((now) + 1))
}

# Default balance journal is a no-op
function balance_journal(now, past, initial_allocation) {
  return
}


# Gains Reconciliation
# Both Realized & Unrealized Gains
#
function print_gains(now, past, is_detailed, gains_type, reports_stream, sold_time,

                                                            is_realized_flag,
                                                            gains_event, current_price, p, a,
                                                            key,
                                                            description,
                                                            parcel_gains, adjusted_gains,
                                                            held_time, price_key,
                                                            label, no_header_printed,
                                                            to_label, proceeds_label,

                                                            asset_width,

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
  print Journal_Title > reports_stream
  printf "%s Report for Period Ending %s\n\n", gains_type, get_date(yesterday(now))  > reports_stream

  # Are we printing out a detailed schedule?
  is_detailed = ((is_detailed)?( is_detailed):( (0)))

  # A flag to discriminate realized and unrealized gains
  is_realized_flag = ("Realized Gains" == gains_type)

  # A default sold time = the Future
  sold_time = ((sold_time)?( sold_time):( Future))

  # The proceeds label
  proceeds_label = ((is_realized_flag)?( "Proceeds"):( "  Value "))
  to_label       = ((is_realized_flag)?( "  To  "):( "Latest"))

  # No header printed
  no_header_printed = (1)

  # Record accounting gains
  accounting_gains = 0
  sum_long_gains = sum_short_gains = sum_long_losses = sum_short_losses = 0 # Tax gains/losses summed here

  # formatting
  asset_width = 15

  # For each asset sold in the current period
  for (a in Leaf)
    if (((a) ~ /^ASSET\.CAPITAL[.:]/) && (is_realized_flag || is_open(a, now))) {
      gains_event = (0)
      proceeds = cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
      long_gains = short_gains = long_losses = short_losses = 0
      units_sold = 0

      # The price
      if (!is_realized_flag) {
        current_price = ((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0))))
        last_key = (__MPX_KEY__)
      }

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
              printf "%*s %*s %*s %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s\n",
                      asset_width, "Asset", 10, "Parcel",
                      7, "Units", 14, "Cost",
                      11, "From", 12, to_label,
                      11, "Price", 16, proceeds_label,
                      13, "Reduced", 14, "Adjusted",
                      15, "Accounting", 9, "Type",
                      18, "Taxable", 15, "Per Unit" > reports_stream
            else if (no_header_printed) {
              printf "%*s %*s %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s\n",
                     asset_width, "Asset",
                     10, "Units", 14, "Cost", 11, "From", 12, to_label, 11, "Price", 16, proceeds_label,
                     13, "Reduced", 14, "Adjusted", 15, "Accounting", 9, "Type", 18, "Taxable" > reports_stream
              underline(151 + asset_width, 6, reports_stream)
            }

            # print Name
            label = (Leaf[(a)])
            gains_event = (1)
            no_header_printed = (0)
          }

          # Keep track
          units = Units_Held[a][p]
          units_sold += units
          if (is_realized_flag) {
            held_time = get_held_time(Held_Until[a][p], Held_From[a][p])
            last_key = Held_Until[a][p]
          } else
            held_time = get_held_time(sold_time, Held_From[a][p])

          reduced_cost  += get_parcel_cost(a, p, now)
          adjusted_cost += get_parcel_cost(a, p, now, (1))

          # cash in and out
          parcel_cost     =   get_cash_in(a, p, now)
          if (is_realized_flag) {
            parcel_proceeds = - get_cash_out(a, p, now)
            current_price = parcel_proceeds / units
          } else
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
            printf "%*s %*d %*.3f %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s\n",
                 asset_width + 1, label,
                 7, p,
                 11, units,
                 15, print_cash(parcel_cost),
                 11, get_date(Held_From[a][p]),
                 10, get_date(last_key),
                 12, print_cash(current_price),
                 14, print_cash(parcel_proceeds),
                 14, print_cash(get_parcel_cost(a, p, now)),
                 14, print_cash(get_parcel_cost(a, p, now, (1))),
                 14, print_cash(- gains),
                 14, description,
                 14, print_cash(- parcel_gains),
                 14, print_cash(- parcel_gains / units, 4) > reports_stream

            # Clear label
            label = ""
          }
        }
      } # End of each parcel p

      # Show any gains event
      if (gains_event) {
        if (is_detailed)
          # Detailed format
          underline(160 + asset_width, 6, reports_stream)

        # The output starts here
        printf "%*s %*.3f %*s %*s   %*s %*s %*s %*s %*s ",
               asset_width + 1, label,
               (11 + 8 * is_detailed), units_sold,
               15, print_cash(cost),
               7, get_date(Held_From[a][0]),
               10, get_date(last_key),
               12, print_cash(current_price),
               14, print_cash(proceeds),
               14, print_cash(reduced_cost),
               14, print_cash(adjusted_cost) > reports_stream

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
          printf "%*s %*s %*s",
            14, print_cash(proceeds - reduced_cost),
            14, key,
            14, print_cash(- Gains_Stack[key]) > reports_stream
          delete Gains_Stack[key]
        } else
          printf "%14s", print_cash(proceeds - reduced_cost) > reports_stream

        # Extra entries {
        for (key in Gains_Stack) {
          printf "\n%*s %14s", 143 + asset_width + 8 * is_detailed, key, print_cash(- Gains_Stack[key]) > reports_stream
          delete Gains_Stack[key]
        }

        printf "\n" > reports_stream
        if (is_detailed)
         printf "\n" > reports_stream

      } # End of gains event
    } # End of each asset

  # Final line
  if (!is_detailed)
    underline(166, 6, reports_stream)
  printf "\n" > reports_stream

  # Stack the gains & losses
  Gains_Stack[Long_Gains_Key]   = sum_long_gains
  Gains_Stack[Long_Losses_Key]  = sum_long_losses
  Gains_Stack[Short_Gains_Key]  = sum_short_gains
  Gains_Stack[Short_Losses_Key] = sum_short_losses

  return accounting_gains
} # End of print gains


# Compute capital gains and losses
function get_capital_gains(now, past, is_detailed,

                                reports_stream,
                                accounting_gains, accounting_losses,
                                income_long_gains, income_short_gains,
                                expense_long_losses, expense_short_losses,
                                taxable_long_gains, taxable_short_gains,
                                taxable_long_losses, taxable_short_losses,
                                total_losses, carried_losses,
                                carry_limit, cancelled_losses) {


    # The reports_stream is the pipe to write the schedule out to
    reports_stream = (("bcot" ~ /[cC]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

    # First print the gains out in detail when required
    if ("/dev/null" != reports_stream) {
      print_gains(now, past, is_detailed, "Realized Gains", reports_stream)
      delete Gains_Stack
    }

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
    accounting_gains = get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past)

    # The realized capital losses
    accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)

    # Print Accounting Capital Gains
    printf "\t%27s => %14s\n", "Accounting Capital Gains", print_cash(- accounting_gains) > reports_stream
    printf "\t%27s => %14s\n", "Accounting Capital Losses", print_cash(accounting_losses) > reports_stream

    # Now compute the total accounting gains
    accounting_gains += accounting_losses
    underline(44, 8, reports_stream)
    printf "\t%27s => %14s\n", "Net Accounting Gains", print_cash(- accounting_gains) > reports_stream

    # The taxable long & short gains
    # If there are other income gains (eg from distributions etc)
    # then the taxable gains will need adjustment
    income_long_gains  = get_cost(INCOME_LONG, now) - get_cost(INCOME_LONG, past)
    income_short_gains = get_cost(INCOME_SHORT, now) - get_cost(INCOME_SHORT, past)

    # Take care that the adjustments are not applied more than once
    if ((((income_long_gains) > Epsilon) || ((income_long_gains) < -Epsilon)))
      printf "\t%27s => %14s\n", "Long Income Gains", print_cash(- income_long_gains) > reports_stream
    if ((((income_short_gains) > Epsilon) || ((income_short_gains) < -Epsilon)))
      printf "\t%27s => %14s\n", "Short Income Gains", print_cash(- income_short_gains) > reports_stream

    # Simililarly for expenses
    expense_long_losses  = get_cost(EXPENSE_LONG, now) - get_cost(EXPENSE_LONG, past)
    expense_short_losses = get_cost(EXPENSE_SHORT, now) - get_cost(EXPENSE_SHORT, past)

    # Take care that the adjustments are not applied more than once
    if ((((expense_long_losses) > Epsilon) || ((expense_long_losses) < -Epsilon)))
      printf "\t%27s => %14s\n", "Long Expense Losses", print_cash(expense_long_losses) > reports_stream
    if ((((expense_short_losses) > Epsilon) || ((expense_short_losses) < -Epsilon)))
      printf "\t%27s => %14s\n", "Short Expense Losses", print_cash(expense_short_losses) > reports_stream

    # The long gains and losses first
    taxable_long_gains  = income_long_gains + get_cost(LONG_GAINS, ((now) - 1)) - get_cost(LONG_GAINS, past)
    taxable_long_losses = expense_long_losses + get_cost(LONG_LOSSES, ((now) - 1)) - get_cost(LONG_LOSSES, past)

    # short gains & losses
    taxable_short_gains   = income_short_gains + get_cost(SHORT_GAINS, ((now) - 1)) - get_cost(SHORT_GAINS, past)
    taxable_short_losses  = expense_short_losses + get_cost(SHORT_LOSSES, ((now) - 1)) - get_cost(SHORT_LOSSES, past)

    # The taxable gains and losses
    printf "\t%27s => %14s\n",   "Long Taxable Gains", print_cash(- taxable_long_gains) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Taxable Gains", print_cash(- taxable_short_gains) > reports_stream

    ## If the taxable gains/losses are non zero at the EOFY  they must be carried losses
    carried_losses = get_carried_losses(past, (0))
    if (((carried_losses) >  Epsilon))
      printf "\t%27s => %14s\n", "Carried Capital Losses", print_cash(carried_losses) > reports_stream
    else {
      assert(!((carried_losses) < -Epsilon), sprintf("Cannot carry taxable capital gains forward [%s] Gains => %14s", get_date(past), print_cash(- carried_losses)))
      carried_losses = 0
      printf "\t%27s\n", "No Carried Capital Losses" > reports_stream
    }

    # Finally the losses
    printf "\t%27s => %14s\n", "Long Capital Losses", print_cash(taxable_long_losses) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Capital Losses", print_cash(taxable_short_losses) > reports_stream
    underline(44, 8, reports_stream)
    printf "\t%27s => %14s\n", "Total Capital Losses", print_cash(taxable_short_losses + taxable_long_losses + carried_losses) > reports_stream

    # Apply long & short losses separately
    # This is not strictly necessary in all cases but useful
    apply_losses(now, reports_stream, "Long",  taxable_long_gains,  taxable_long_losses,  LONG_GAINS,  LONG_LOSSES)
    apply_losses(now, reports_stream, "Short", taxable_short_gains, taxable_short_losses, SHORT_GAINS, SHORT_LOSSES)

    # All done
    underline(44, 8, reports_stream)
    print "\n" > reports_stream
}

# Shared code for applying losses to taxable gains
function apply_losses(now, reports_stream, label,
                           gains, losses, save_gains, save_losses) {
  # It works for partioned long & short gains

  # Summarize starting point
  underline(44, 8, reports_stream)
  printf "\nAfter Application of %s Losses\n", label > reports_stream

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  gains  += losses # Net gains / losses

  # Carried losses generated
  if (!((gains) < -Epsilon)) {
    # No overall gains
    # There could be a loss in this scenario
    losses = (((gains) >   Epsilon)?( (gains)):(  0))

    # But not a gain
    gains = 0
  } else {
    # No overall losses
    losses = 0
  }

  # Save  gains
  # these could be deferred gains or taxable gains
  # this could be rewritten with fewer flags/options
  if (save_gains) {
    set_cost(save_gains, gains, now)
    printf "\t%27s => %14s\n", (label " Gains"), print_cash(- get_cost(save_gains, now)) > reports_stream
  }

  # Remaining options could only be for taxable gains
  if (save_losses) {
    set_cost(save_losses, losses, now)
    printf "\t%27s => %14s\n", (label " Losses"), print_cash(get_cost(save_losses, now)) > reports_stream
  }
}

# Get the carried losses - limit how many years a loss can be carried forward
function get_carried_losses(past, limit,
                            carried_losses, losses, y, limit_time) {

  # When there is no limit simply return the losses
  # carried forward from last year
  if (!limit)
    return get_cost(CARRIED_LOSSES, past)

  # There is a limit - do not use any
  # losses from more  than "limit" years ago -
  # First note that this routine already starts one year back
  if (limit > 1)
    # When is that?
    limit_time = past - one_year(past, 1 - limit)
  else
    limit_time = past


  # No losses prior to the limit time are considered
  # Lets start summing the losses
  carried_losses = 0

  # The first year considered
  y = limit_time
  do {
    # Get the annualized combined losses & gains
    losses = get_taxable_gains(y)
    carried_losses += losses

    # Carried losses cannot be negative
    carried_losses = (((carried_losses) >   Epsilon)?( (carried_losses)):(  0))



    # Next year
    y += one_year(y, 1)
  } while (y < past + 604800)

  # record if carried losses were extinguished
  losses = get_cost(CARRIED_LOSSES, past)
  if ((((losses - carried_losses) > Epsilon) || ((losses - carried_losses) < -Epsilon))) {
    printf "\tPast  => %14s\n", get_date(past) > "/dev/stderr"
    printf "\tOld Carried Losses => %14s\n", losses > "/dev/stderr"
    printf "\tNew Carried Losses => %14s\n", carried_losses > "/dev/stderr"
  }

  # This determines the carried losses
  set_cost(CARRIED_LOSSES, carried_losses, past)
  return carried_losses
  #return get_cost(CARRIED_LOSSES, past)
}

# Compute the deferred gains
# And print out a schedule
#
function get_deferred_gains(now, past, is_detailed,       accounting_gains, reports_stream,
                                                          gains, losses) {

 # The reports_stream is the pipe to write the schedule out to
 reports_stream = (("bcot" ~ /[dD]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

 # First print the gains out in detail
 accounting_gains = print_gains(now, past, is_detailed, "Deferred Gains", reports_stream)
 losses = Gains_Stack[Long_Losses_Key]
 gains  = Gains_Stack[Long_Gains_Key]
 delete Gains_Stack

 # Print the deferred gains report
 print Journal_Title > reports_stream
 printf "Deferred Gains Report for Period Ending %s\n", get_date(yesterday(now))  > reports_stream

 # Print Capital Gains & Losses
 underline(44, 8, reports_stream)

 printf "\t%27s => %14s\n", "Accounting Deferred Gains", print_cash(- accounting_gains) > reports_stream
 printf "\t%27s => %14s\n", "Taxable Deferred Gains",
                            print_cash(- gains) > reports_stream
 printf "\t%27s => %14s\n", "Taxable Deferred Losses",
                            print_cash(losses) > reports_stream

 printf "\nAfter Application of Any Losses\n" > reports_stream

 # Get the deferred taxable gains
 apply_losses(now, reports_stream, "Deferred", gains, losses, DEFERRED_GAINS)

  # All done
  underline(43, 8, reports_stream)
  print "\n" > reports_stream

} # End of deferred gains



# Print out operating statement
function print_operating_statement(now, past, is_detailed,     reports_stream,
                                                               benefits, losses,
                                                               gains, market_gains,
                                                               more_past, label, x) {

  # Set arguments
  more_past = ((past) - one_year(past, -1))
  is_detailed = ("" == is_detailed) ? 1 : 2

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = (("bcot" ~ /[oO]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

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
  market_gains[now]  = - (get_cost(MARKET_CHANGES, now) - (x = get_cost(MARKET_CHANGES, past)))
  market_gains[past] = - (x - get_cost(MARKET_CHANGES, more_past))

  # Print the total unrealized gains
  ((past)?( underline(73, 8,  reports_stream)):( underline(47, 8,  reports_stream)))

  # Print any unrealized gains
  if (((market_gains[now]) >  Epsilon) || ((market_gains[past]) >  Epsilon)) {
    printf "\t%22s %23s", "Total Market Gains", print_cash((((market_gains[now]) >   Epsilon)?( (market_gains[now])):(  ""))) > reports_stream
    if (past)
      printf " %26s\n", print_cash((((market_gains[past]) >   Epsilon)?( (market_gains[past])):(  ""))) > reports_stream
    else
      printf "\n" > reports_stream
    benefits[now]  += (((market_gains[now]) >   Epsilon)?( (market_gains[now])):(  0))
    benefits[past] += (((market_gains[past]) >   Epsilon)?( (market_gains[past])):(  0))
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
  if (((market_gains[now]) < -Epsilon) || ((market_gains[past]) < -Epsilon)) {
    printf "\t%22s %23s", "Total Market Losses", print_cash((((market_gains[now]) < - Epsilon)?( (market_gains[now])):(  ""))) > reports_stream
    if (past)
      printf " %26s\n", print_cash((((market_gains[past]) < - Epsilon)?( (market_gains[past])):(  ""))) > reports_stream
    else
      printf "\n" > reports_stream
    losses[now]  -= (((market_gains[now]) < - Epsilon)?( (market_gains[now])):(  0))
    losses[past] -= (((market_gains[past]) < - Epsilon)?( (market_gains[past])):(  0))
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
  reports_stream = (("bcot" ~ /[bB]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

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
  #label = print_account_class(reports_stream, label, "block_class", "ASSET", "ASSET.CURRENT", "get_cost", now, Epoch, past, Epoch, is_detailed)
  #label = print_account_class(reports_stream, label, "not_current_class", "ASSET", "", "get_cost", now, Epoch, past, Epoch, is_detailed)
  class_list["ASSET.TERM"] = (1)
  class_list["ASSET.CURRENT"] = (1)
  label = print_account_class(reports_stream, label, "block_class_list", "ASSET", class_list, "get_cost", now, Epoch, past, Epoch, is_detailed)
  label = print_account_class(reports_stream, label, "not_current_class", "ASSET.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed)
  delete class_list

  # Here we need to adjust for accounting gains & losses
  assets[now]  =  get_cost("*ASSET", now)  - get_cost("*INCOME.GAINS.REALIZED", now)  - get_cost("*EXPENSE.LOSSES.REALIZED", now)  - get_cost(MARKET_CHANGES, now)
  assets[past] =  get_cost("*ASSET", past) - get_cost("*INCOME.GAINS.REALIZED", past) - get_cost("*EXPENSE.LOSSES.REALIZED", past) - get_cost(MARKET_CHANGES, past)

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
function print_market_gains(now, past, is_detailed,    reports_stream) {
  # Show current gains/losses
   # The reports_stream is the pipe to write the schedule out to
   reports_stream = (("bcot" ~ /[mM]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

   # First print the gains out in detail
   if ("/dev/null" != reports_stream) {
     print_gains(now, past, is_detailed, "Market Gains", reports_stream, now)
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
            adjust_parcel_cost(a, p, now,   second_element,  I, (0))
            adjust_parcel_cost(a, p, now, - second_element, II, (0))
          }


        } # End of if unsold parcel
      } # End of each parcel


    } # End of each fixed asset a
}


# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      reports_stream, a, p, open_key, close_key, parcel_depreciation, account_depreciation, open_cost, total_depreciaiton, sum_open,
                                                                  sale_depreciation, sale_appreciation, sum_adjusted) {

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = (("bcot" ~ /[fF]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))
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
      account_depreciation = sum_open = 0

      # Get each parcel
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
        parcel_depreciation = (((__MPX_KEY__ = find_key(Tax_Adjustments[a][ p][ I],  ( open_key)))?( Tax_Adjustments[a][ p][ I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][ p][ I][0]):( 0))))) - (((__MPX_KEY__ = find_key(Tax_Adjustments[a][ p][ I],  ( close_key)))?( Tax_Adjustments[a][ p][ I][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Tax_Adjustments[a][ p][ I][0]):( 0)))))

        #  Just track the total depreciation
        account_depreciation   += parcel_depreciation

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
                    print_cash(get_parcel_element(a, p, II, ((close_key) - 1))),
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
      printf "%16s %11s ", (Leaf[(a)]), Depreciation_Method[Method_Name[a]] > reports_stream
      if (is_detailed)
        printf "%8s", " " > reports_stream

      # The rest of the footer
      printf "[%11s, %11s] %14s %14s %14s %14s %14s\n",
        get_date(open_key), get_date(close_key), print_cash(sum_open),
        print_cash(sum_open - account_depreciation),
        print_cash(get_cost_element(a, II, ((close_key) - 1))),
        print_cash(get_cost(a, close_key)),
        print_cash(account_depreciation) > reports_stream

      # Track total depreciation too
      total_depreciation += account_depreciation
    } # End of a depreciating asset

  # Is there any depreciation/appreciation due to the sale of depreciating assets?
  sale_appreciation = get_cost(SOLD_APPRECIATION, now) - get_cost(SOLD_APPRECIATION, past)
  sale_depreciation = get_cost(SOLD_DEPRECIATION, now) - get_cost(SOLD_DEPRECIATION, past)
  if (!(((sale_depreciation) <= Epsilon) && ((sale_depreciation) >= -Epsilon)))
    printf  "\n%24s %115s\n", "Depreciation from Sales", print_cash(sale_depreciation) > reports_stream
  if (!(((sale_appreciation) <= Epsilon) && ((sale_appreciation) >= -Epsilon)))
    printf  "\n%24s %115s\n", "Appreciation from Sales", print_cash(-sale_appreciation) > reports_stream
  total_depreciation += sale_depreciation + sale_appreciation

  # Print a nice line
  if (!(((total_depreciation) <= Epsilon) && ((total_depreciation) >= -Epsilon))) {
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
  reports_stream = (("bcot" ~ /[qQ]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

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
        qualifying_date = ((yesterday(get_exdividend_date(underlying_asset, key), (12))) + 1)

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > (-1), sprintf("%s: %s <%s>",  Leaf[a], Read_Date_Error, get_date(key)))

        # These are the units that were qualified on the qualifying date
        qualified_units = ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[underlying_asset],   qualifying_date))?( Qualified_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[underlying_asset][0]):( 0))))):( ((__MPX_KEY__ = find_key(Total_Units[underlying_asset],    qualifying_date))?( Total_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[underlying_asset][0]):( 0))))))

        # Now get the total units
        total_units = ((__MPX_KEY__ = find_key(Total_Units[underlying_asset],   qualifying_date))?( Total_Units[underlying_asset][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[underlying_asset][0]):( 0))))

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
        printf "%13s %12s %12.3f %16s %14.3f %12.2f %16s\n", Leaf[underlying_asset], get_date(tomorrow(qualifying_date)), total_units,
                get_date(key), qualified_units, 100.0 * qualified_fraction, print_cash(payment) > reports_stream

        # Sum qualified payment
        qualified_payment += qualified_fraction * payment

        # Make the appropriate changes for the current tax jurisdiction
        @Dividend_Qualification_Function(a, underlying_asset, key, 1.0 - qualified_fraction)

        # Get the next key
        key = next_key
      } # End of while each key in the window
    } # End of if a dividend

    # summary
    underline(95, 6, reports_stream)
    payment = get_cost("*INCOME.DIVIDEND", past) + get_cost("*INCOME.DISTRIBUTION.CLOSE", past) -               get_cost("*INCOME.DIVIDEND", now) - get_cost("*INCOME.DISTRIBUTION.CLOSE", now)
    if ((((payment) > Epsilon) || ((payment) < -Epsilon)))
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
    if (@selector(x, class_name, blocked_class)) {

      # The required name component is the last in the parent - watch out for
      # the leading "*" if only a single component
      subclass = get_name_component(Parent_Name[x], 0)
      if (((subclass) ~ /^*/))
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
          printf "\t%24s %21s", (Leaf[(x)]), print_cash(account_income[now]) > stream
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
function select_class(a, class_name, blocked_class) {
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Include class blocking
function block_class(a, class_name, blocked_class) {
  if (((a) ~ ("^" ( blocked_class) "[.:]")))
    return (0) # Blocked!

  # Just the simple case
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Block multiple classes...
function block_class_list(a, class_name, blocked_class_list,      x) {
  # blocked class might actually be an array of blocked classes
  for (x in blocked_class_list)
    if (((a) ~ ("^" ( x) "[.:]"))) # Blocked!
      return (0)

  # Just the simple case
  return ((a) ~ ("^" ( class_name) "[.:]"))
}

# Special purpose filter for current accounts
function current_class(a, class_name, blocked_class) {
  return ((a) ~ ("^" ( class_name) "[.:]")) && !(a in Maturity_Date)
}

# And its pigeon pair
function not_current_class(a, class_name, blocked_class) {
  return ((a) ~ ("^" ( class_name) "[.:]")) && (a in Maturity_Date)
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
    if ((((available_losses) <= Epsilon) && ((available_losses) >= -Epsilon)))
      return 0

    # Record the process
    # that adjusts the gains ONLY in this function
    # So SPECIAL.TAXABLE.LOSSES:WRITTEN.BACK
    taxable_gains  = get_cost("*SPECIAL.TAXABLE", now)
    printf "\t%27s => %13s\n", "Write Back", get_date(now) > reports_stream
    printf "\t%27s => %14s\n", "Gains", print_cash(- taxable_gains) > reports_stream

    # Get the gains
    if (((taxable_gains) < -Epsilon)) {
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
      tax_refund = get_tax(now, Tax_Bands, get_cost(TAXABLE_INCOME, now) + gains_written_back) - get_cost(INCOME_TAX, now)

      # Update taxable gains
      set_cost(WRITTEN_BACK, - gains_written_back, now)

      # The refund is a simple refundable offset at the future time
      if (((tax_refund) < -Epsilon))
        adjust_cost(REFUNDABLE_OFFSETS, tax_refund, future_time)

      # Record This
      printf "\t%27s => %14s\n", "Rewritten Gains", print_cash(- taxable_gains) > reports_stream
      printf "\t%27s => %14s\n", "New Available Losses", print_cash(available_losses) > reports_stream
      printf "\t%27s => %14s\n", "Tax Refund", print_cash(- tax_refund) > reports_stream
      printf "\t%27s => %14s\n", "Total Refundable Offset", print_cash(get_cost(REFUNDABLE_OFFSETS, future_time)) > reports_stream
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
  ((SUBSEP in ATO_Levy)?((1)):((0)))
  ((SUBSEP in CGT_Discount)?((1)):((0)))
  ((SUBSEP in GST_Rate)?((1)):((0)))
  ((SUBSEP in LIC_Allowance)?((1)):((0)))
  ((SUBSEP in Low_Income_Offset)?((1)):((0)))
  ((SUBSEP in Middle_Income_Offset)?((1)):((0)))
  ((SUBSEP in Medicare_Levy)?((1)):((0)))
  ((SUBSEP in Member_Liability)?((1)):((0)))
  ((SUBSEP in Reserve_Rate)?((1)):((0)))

  # The Epoch
  if ("" == Epoch)
    set_epoch()

  # // Can set constants here
  if (!Qualification_Window)
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

  # The default low and middle income offsets
  Low_Income_Offset[Epoch][0] = 0.00
  Middle_Income_Offset[Epoch][0] = 0.00

  # # Other special accounts
  # FRANKING_TAX = initialize_account("LIABILITY.TAX:FRANKING.TAX")

  # Kept apart to allow correct allocation of member benfits in an SMSF
  CONTRIBUTION_TAX = initialize_account("LIABILITY.TAX:CONTRIBUTION.TAX")
  #
  #
  # # Franking deficit
  # FRANKING_DEFICIT   = initialize_account("SPECIAL.FRANKING.OFFSET:FRANKING.DEFICIT")

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
    Update_Member_Function   = "update_member_liability_smsf"
    Update_Profits_Function  = "update_profits_smsf"

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

                                        write_stream,
                                        taxable_gains, carried_losses,
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
                                        low_income_offset, middle_income_offset,
                                        taxable_income,
                                        medicare_levy, extra_levy, tax_levy, x, header) {

  # Print this out?
  write_stream = (("bcot" ~ /[tT]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

  # Get market changes
  market_changes = get_cost(MARKET_CHANGES, now) - get_cost(MARKET_CHANGES, past)

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
  other_income = (((market_changes) >   Epsilon)?( (market_changes)):(  0))
  if (!(((other_income) <= Epsilon) && ((other_income) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Unrealized Losses", print_cash(other_income) > write_stream
    header = ""
  }

  # Accounting losses are added - as are taxable gains
  accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)
  if (!(((accounting_losses) <= Epsilon) && ((accounting_losses) >= -Epsilon))) {
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
  if (!(((other_expenses) <= Epsilon) && ((other_expenses) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Other Non Deductible Expenses", print_cash(other_expenses) > write_stream
    other_income += other_expenses
    header = ""
  }

  # taxable capital gains
  #
  #
  # Australia ignores the distinction between long & short term losses
  taxable_gains = get_taxable_gains(now, get_cost(CARRIED_LOSSES, past))
  if (((taxable_gains) < -Epsilon)) {
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
  if ((0) && (((carried_losses) > Epsilon) || ((carried_losses) < -Epsilon))) {
    # Try writing back losses
    printf "\n\t%27s => %14s\n", "Write Back Losses Available", print_cash(carried_losses) > write_stream

    # Rewrite refundable offsets to just before now so they can be zeroed later at a distinct timestamp
    carried_losses = write_back_losses(((now) - 1), ((now) - one_year(now, -1)), (((0))?( (now - one_year(now, (0)))):( Epoch)), carried_losses, write_stream)
  }

  # Save the loss
  set_cost(CARRIED_LOSSES, carried_losses, now)

  # Imputation Tax Offsets
  #

  # Tax credits received during this FY
  franking_offsets = - (get_cost("*SPECIAL.FRANKING.OFFSET", now) - get_cost("*SPECIAL.FRANKING.OFFSET", past))
  if (!(((franking_offsets) <= Epsilon) && ((franking_offsets) >= -Epsilon))) {
    other_income += franking_offsets
    printf "%s\t%40s %32s\n", header, "Franking Offsets", print_cash(franking_offsets) > write_stream
    header = ""
  }

  if (!(((other_income) <= Epsilon) && ((other_income) >= -Epsilon))){
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
  other_expenses = - (((market_changes) < - Epsilon)?( (market_changes)):(  0))
  if (other_expenses > Epsilon) {
    printf "%s\t%40s %32s\n", header, "Unrealized Gains", print_cash(other_expenses) > write_stream
    header = ""
  }

  # Tax exempt income
  other_expenses += exempt_income

  # Accounting losses are added - as are taxable gains
  accounting_gains = -(get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past))
  if (!(((accounting_gains) <= Epsilon) && ((accounting_gains) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Capital Gains", print_cash(accounting_gains) > write_stream
    other_expenses += accounting_gains
    header = ""
  }

  # And the non-concessional contributions
  # Should look at CONTRIBUTION minus the one taxed subclass because maybe more than one tax-free subclass?
  contributions = -(get_cost("*INCOME.CONTRIBUTION.TAX-FREE", now) - get_cost("*INCOME.CONTRIBUTION.TAX-FREE", past))
  if (!(((contributions) <= Epsilon) && ((contributions) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Non Taxable Contributions", print_cash(contributions) > write_stream
    other_expenses += contributions
    header = ""
  }

  # Finally LIC Deductions (if eligible)
  # LIC deductions 1/3 for SMSF
  #                1/2 for individual
  #                0/3 for company
  lic_deductions = - ((LIC_Allowance[2])?( (LIC_Allowance[1]/LIC_Allowance[2])):( assert((0), "Division by zero in rational fraction" LIC_Allowance[1] "/" LIC_Allowance[2]))) * (get_cost(LIC_CREDITS, now) - get_cost(LIC_CREDITS, past))

  # Always apply allowance at this point to catch explicit allocations to LIC
  if (!(((lic_deductions) <= Epsilon) && ((lic_deductions) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header,"LIC Deduction", print_cash(lic_deductions) > write_stream
    other_expenses += lic_deductions
    header = ""
  }

  # Summarize other expenses
  if (!(((other_expenses) <= Epsilon) && ((other_expenses) >= -Epsilon))) {
    underline(81, 0, write_stream)
    printf "%s\t%40s %32s\n\n", header, "Other Expenses", print_cash(other_expenses) > write_stream
    header = ""
  }

  taxable_income = benefits + other_income - other_expenses
  underline(81, 0, write_stream)
  header = ""
  printf "%48s %32s\n\n", "TAXABLE INCOME OR LOSS", print_cash(taxable_income) > write_stream

  # Record this quantity
  set_cost(TAXABLE_INCOME, taxable_income, now)

  # Keep the income tax on the taxable income - the actual amount owed may change due to tax offsets etc
  income_tax = tax_owed = get_tax(now, Tax_Bands, taxable_income) # Just need total tax
  printf "%48s %32s\n", "Income Tax on Taxable Income or Loss ", print_cash(tax_owed) > write_stream

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
  header = "LESS\n"

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
      # within three months of the write_stream will also trigger it
      printf "\t%40s %32s\n", "Franking Balance is Overdrawn", print_cash(franking_balance) > write_stream

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
  if (!(((franking_offsets) <= Epsilon) && ((franking_offsets) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Franking Offsets", print_cash(franking_offsets) > write_stream
    header = ""
  }

  # Foreign offsets
  # Are no-refund-no-carry
  foreign_offsets = - (get_cost("*SPECIAL.OFFSET.FOREIGN", now) - get_cost("*SPECIAL.OFFSET.FOREIGN", past))
  if (!(((foreign_offsets) <= Epsilon) && ((foreign_offsets) >= -Epsilon))) {
    # Foreign offsets have complex rules too :( sigh ):
    #
    # If they are not greater than the Foreign_Offset_Limit it is ok to just use  them
    if (foreign_offsets > ((__MPX_KEY__ = find_key(Foreign_Offset_Limit,  now))?( Foreign_Offset_Limit[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Foreign_Offset_Limit[0]):( 0))))) {
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
    no_carry_offsets -= (get_cost(NO_CARRY_OFFSETS, now) - get_cost(NO_CARRY_OFFSETS, past))
  } else
    # Just get the total change in the offset
    no_carry_offsets = -(get_cost(NO_CARRY_OFFSETS, now) - get_cost(NO_CARRY_OFFSETS, past))

  # Foreign offsets are no-carry offsets
  no_carry_offsets += foreign_offsets

  # The no-carry offset
  if ((((no_carry_offsets) > Epsilon) || ((no_carry_offsets) < -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Total No-Carry Offsets", print_cash(no_carry_offsets) > write_stream
    header = ""
  }

  # Other offsets
  # The carry offset (Class D)
  carry_offsets = -(get_cost(CARRY_OFFSETS, now) - get_cost(CARRY_OFFSETS, past))
  #carry_offsets = - get_cost(CARRY_OFFSETS, now)
  if (!(((carry_offsets) <= Epsilon) && ((carry_offsets) >= -Epsilon))) {
    printf "%s\t%40s %32s\n", header, "Total Carry Offsets", print_cash(carry_offsets) > write_stream
    header = ""
  }

  # The refundable offset (Class E)
  refundable_offsets = - (get_cost(REFUNDABLE_OFFSETS, now) - get_cost(REFUNDABLE_OFFSETS, past))
  if (!(((refundable_offsets) <= Epsilon) && ((refundable_offsets) >= -Epsilon))) {
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
  if (((tax_owed) >  Epsilon) && ((no_refund_offsets) >  Epsilon)) {
    # Since franking offsets can generate a loss add them to
    # both sides of the balance
    tax_owed += franking_offsets

    if (tax_owed < no_refund_offsets) {
      # How many carry offsets were used?
      if (tax_owed > no_carry_offsets) # Some were used
        carry_offsets -= (tax_owed - no_carry_offsets)

      # information
      printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(tax_owed - franking_offsets) > write_stream
      tax_owed = 0
    } else { # All the no_refund offsets were used
      tax_owed -= no_refund_offsets
      carry_offsets = 0
      if (((no_refund_offsets - franking_offsets) >  Epsilon))
        printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(no_refund_offsets - franking_offsets) > write_stream
    }

    # OK now if the tax_owed is less than the amount of franking offsets
    # then the difference is transferred to tax losses
    if (tax_owed < franking_offsets) {
      franking_offsets -= tax_owed

      printf "\t%40s %32s>\n", "<Franking Offsets Used", print_cash(tax_owed) > write_stream
      # Report remaining  franking offsets
      if (((franking_offsets) >  Epsilon))
        printf "\t%40s %32s>\n", "<Franking Offsets Remaining", print_cash(franking_offsets) > write_stream

      tax_owed = 0
    } else {
      tax_owed -= franking_offsets
      if (((franking_offsets) >  Epsilon))
        printf "\t%40s %32s>\n", "<All Franking Offsets Used", print_cash(franking_offsets) > write_stream
      franking_offsets = 0
    }

    # Report tax owed

  } # End of if any attempt to apply non-refundable assets

  # What happens when the tax owed is negative??



  # Now apply refundable offsets - but note these will not generate a tax loss - since they are refunded :)
  if (((refundable_offsets) >  Epsilon)) {
    tax_owed -= refundable_offsets
    printf "\t%40s %32s>\n", "<Refundable Offsets Used", print_cash(refundable_offsets) > write_stream

  }

  # Finally franking deficit tax offsets can be applied
  if (((tax_owed) >  Epsilon) && ((franking_deficit_offsets) >  Epsilon)) {
    if (tax_owed < franking_deficit_offsets) {
      # How many franking deficit tax offsets were used?
      if (tax_owed > franking_deficit_offsets) # Some were used
        franking_deficit_offsets -= tax_owed

      # information
      printf "\t%40s %32s>\n", "<Franking Deficit Tax Offsets Used", print_cash(tax_owed - franking_deficit_offsets) > write_stream
      tax_owed = 0
    } else { # All the franking deficit offsets were used
      tax_owed -= franking_deficit_offsets
      printf "\t%40s %32s>\n", "<Franking Deficit Tax Offsets Used", print_cash(franking_deficit_offsets) > write_stream
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
  } else if (!((tax_owed + refundable_offsets) >  Epsilon)) { # Increase losses
    # This is a bit tricky
    # (unused) franking offsets may still be present here
    # plus the actual tax owed is modifiable by any refundable offsets (which will be refunded)
    # so adjust the tax losses accordingly
    # -- so does this work for an individual or smsf?
    tax_losses -= get_taxable_income(now, tax_owed + refundable_offsets - franking_offsets)


  }

  # The carried tax losses


  # Print the tax owed
  if (!header) {
    underline(81, 0, write_stream)
    printf "%48s %32s\n\n", "CURRENT TAX OR REFUND", print_cash(tax_owed) > write_stream
  }

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
    printf "\t%40s %32s\n", "Supervisory Levy", print_cash(((__MPX_KEY__ = find_key(ATO_Levy,  now))?( ATO_Levy[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( ATO_Levy[0]):( 0))))) > write_stream

  # Medicare levy (if any)
  if (!(((medicare_levy) <= Epsilon) && ((medicare_levy) >= -Epsilon))) {
    printf "\t%40s %32s\n", "Medicare Levy", print_cash(medicare_levy) > write_stream
    tax_owed += medicare_levy
  }

  # Any other levys
  tax_levy = - get_cost("*LIABILITY.CURRENT.LEVY", ((now) - 1))
  if ((((tax_levy) > Epsilon) || ((tax_levy) < -Epsilon))) {
    printf "\t%40s %32s\n", "Tax Levies", print_cash(tax_levy) > write_stream
    tax_owed += tax_levy
  }

  if (!(((tax_paid) <= Epsilon) && ((tax_paid) >= -Epsilon)))
    printf "\t%40s %32s\n", "Income Tax Distributions Paid", print_cash(tax_paid) > write_stream
  if (!(((tax_with) <= Epsilon) && ((tax_with) >= -Epsilon)))
    printf "\t%40s %32s\n", "Income Tax Withheld", print_cash(tax_with) > write_stream

  # Compute income tax due
  tax_due = tax_owed - (tax_paid + tax_with)
  set_cost(TAX, - tax_due, now)
  underline(81, 0, write_stream)
  printf "%48s %32s\n\n\n", "AMOUNT DUE OR REFUNDABLE", print_cash(((__MPX_KEY__ = find_key(ATO_Levy,  now))?( ATO_Levy[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( ATO_Levy[0]):( 0)))) + tax_due) > write_stream

  # Clean up balance sheet - watch out for unbalanced transactions
  # Save contribution tax accounted for
  tax_cont = get_cost(CONTRIBUTION_TAX, ((now) - 1))

  # If this is an SMSF this disturbs the member liabilities
  # Adjust cost is OK because ALLOCATED/ADJUSTMENTS were reset at comencement of eofy_actions
  # For a none SMSF this is a synonym for ADJUSTMENTS
  adjust_cost(ALLOCATED, -(tax_cont + tax_owed - get_cost(FRANKING_TAX, now)), now)

  # Print out the tax and capital losses carried forward
  # These really are for time now - already computed
  capital_losses = get_cost(CARRIED_LOSSES, now)
  if (!(((capital_losses) <= Epsilon) && ((capital_losses) >= -Epsilon)))
    printf "\t%40s %32s\n", "Capital Losses Carried Forward", print_cash(capital_losses) > write_stream

  # The change in tax losses
  if (!(((tax_losses - old_losses) <= Epsilon) && ((tax_losses - old_losses) >= -Epsilon))) {
    if (tax_losses > old_losses)
      printf "\t%40s %32s\n", "Tax Losses Generated", print_cash(tax_losses - old_losses) > write_stream
    else
      printf "\t%40s %32s\n", "Tax Losses Extinguished", print_cash(old_losses - tax_losses) > write_stream
  }

  # The carried tax losses
  if (!(((tax_losses) <= Epsilon) && ((tax_losses) >= -Epsilon)))
    printf "\t%40s %32s\n", "Tax Losses Carried Forward", print_cash(tax_losses) > write_stream
  else
    tax_losses = 0

  # Save the carried losses
  set_cost(TAX_LOSSES, tax_losses, now)

  # Franking
  if (!(((franking_balance) <= Epsilon) && ((franking_balance) >= -Epsilon)))
    printf "\t%40s %32s\n", "Franking Balance Carried Forward", print_cash(franking_balance) > write_stream

  # Franking Deficit
  # Save the franking deficit offsets
  if (!(((franking_deficit_offsets) <= Epsilon) && ((franking_deficit_offsets) >= -Epsilon)))
    printf "%48s %32s\n\n", "Franking Deficit Offsets Carried Forward", print_cash(franking_deficit_offsets) > write_stream
  else
    franking_deficit_offsets = 0
  set_cost(FRANKING_DEFICIT, -franking_deficit_offsets, now)

  # Update carry forward offsets
  if (!(((carry_offsets) <= Epsilon) && ((carry_offsets) >= -Epsilon)))
    printf "\t%40s %32s\n", "Non-Refundable Offsets Carried Forwards", print_cash(carry_offsets) > write_stream
  else
    carry_offsets = 0
  set_cost(CARRY_OFFSETS, -carry_offsets, now)

  # Now we need Deferred Tax - the hypothetical liability that would be due if all
  # assets were liquidated today
  deferred_gains = get_cost(DEFERRED_GAINS, now)

  # Gains are negative - losses are positive
  # Catch negligible gains
  if (!(((deferred_gains) <= Epsilon) && ((deferred_gains) >= -Epsilon))) {
    # Deferred tax losses can reduce future tax liability so are a deferred tax asset
    deferred_tax = get_tax(now, Tax_Bands, taxable_income - deferred_gains) - income_tax
    set_cost(DEFERRED, - deferred_tax, now)



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


## This should become jurisdiction specific
## There are complications with the discounting
function get_taxable_gains(now, losses,

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
  losses     += get_cost(LONG_LOSSES, now) + get_cost(SHORT_LOSSES, now)
  long_gains  = get_cost(LONG_GAINS, now)
  short_gains = get_cost(SHORT_GAINS, now)

  # Suppress negligible losses
  losses      = (((losses) >   Epsilon)?( (losses)):(  0))

  # Summarize starting point

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  # Australian scheme & US Scheme are same
  # once short & long losses are disregarded
  if (!((losses + short_gains + long_gains) < -Epsilon)) {
    # More carried losses generated
    losses += short_gains + long_gains

    # Zero negligible losses
    if ((((losses) <= Epsilon) && ((losses) >= -Epsilon)))
      losses = 0

    # Zero the gains
    short_gains = long_gains = 0
  } else if (!((losses + short_gains) < -Epsilon)) {
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
  if (((losses) >  Epsilon))
    return losses
  else # Taxable gains (may be zero)
    return short_gains + (1.0 - discount) * long_gains
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
      unqualified_account = initialize_account("SPECIAL.FRANKING.OFFSET.UNQUALIFIED:U_TAX." Leaf[underlying_asset])

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
  reports_stream = (("bcot" ~ /[iI]|[aA]/ && "bcot" !~ /[zZ]/)?( EOFY):( "/dev/null"))

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
    x *= ((__MPX_KEY__ = find_key(Reserve_Rate,  now))?( Reserve_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Reserve_Rate[0]):( 0))))

    # The only reserve set in eofy actions so use now
    adjust_cost(RESERVE, -x, now)

  } else
    x = 0

  # By this point there are several adjustments required to
  # both redistribute liabilities and allocated profits
  delta_profits = get_cost(ALLOCATED, now) - initial_allocation - x
  if (!(((delta_profits) <= Epsilon) && ((delta_profits) >= -Epsilon)))
    update_member_liability_smsf(now, delta_profits)

  # Unallocated expenses/income
  adjust_cost(ALLOCATED, (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now), now)

}

# This checks all is ok
function check_balance_smsf(now,        sum_assets, sum_liabilities, sum_adjustments, sum_future, balance, show_balance) {
  # The following should always be true (Equity is treated a special case of liability)
  # Assets - Liabilities = 0 (SMSFs have a simplified equation)
  # A complication exists if back payments are included so we have innstead
  # Assets - Liabilities = Future_Payments
  # This compares the cost paid - so it ignores the impact of revaluations and realized gains & losses
  sum_assets =  get_cost("*ASSET", now) - get_cost("*INCOME.GAINS.REALIZED", now) - get_cost("*EXPENSE.LOSSES.REALIZED", now) - get_cost("*EXPENSE.UNREALIZED", now)

  # Work out the total assets etc
  sum_liabilities = - get_cost("*LIABILITY", now)
  sum_future      = - get_cost(FUTURE_PAYMENT, now)

  # The balance should be zero
  # A super fund has only assets and liabilities since the income and expenses are attributed to members
  sum_adjustments = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now)
  balance = sum_assets - (sum_liabilities + sum_adjustments + sum_future)


  # No default printing
  show_balance = (0)


  # Is there an error?
  if (!(((balance) <= Epsilon) && ((balance) >= -Epsilon))) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0
    show_balance = (1)
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now)
    printf "\tAssets      => %20.2f\n", sum_assets
    printf "\tLiabilities => %20.2f\n", sum_liabilities
    if ((((sum_adjustments) > Epsilon) || ((sum_adjustments) < -Epsilon)))
      printf "\tAdjustments => %20.2f\n", sum_adjustments
    if ((((sum_future) > Epsilon) || ((sum_future) < -Epsilon)))
      printf "\tFuture      => %20.2f\n", sum_future
    printf "\tBalance     => %20.2f\n", balance
    assert((((balance) <= Epsilon) && ((balance) >= -Epsilon)), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}

# A wrapper function updates allocated profits when required ()
function update_profits_smsf(now,     delta_profits) {
  # Compute the profits that need to be allocated to members
  # These are the profits accumulated since the last time they were distributed to members
  delta_profits = (get_cost("*INCOME.CONTRIBUTION",now) + get_cost("*EXPENSE.NON-DEDUCTIBLE.BENEFIT",now) - get_cost("*INCOME",now) - get_cost("*EXPENSE",now)) - get_cost(ALLOCATED, now)
  if (!(((delta_profits) <= Epsilon) && ((delta_profits) >= -Epsilon))) {
    # Update the Allocated Profits
    adjust_cost(ALLOCATED, delta_profits, now, (0))

    # Update the liabilities
    update_member_liability_smsf(now, delta_profits)
  }
}

# Update a member liability
# This can be (i)   a contribution - specified member, taxable or tax-free
#          or (ii)  a benefit - specified member
#          or (iii) allocation amongst members - no specificiation
#          or (iv)  allocation to or from the reserve - no specification
# This function keeps the member liability up to date for a SMSF
#
function update_member_liability_smsf(now, amount, a,
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
#   other tax_statement calculations (eg UK, US, NZ etc...)
#
#   Tax Adjustments / could be simplified?
#   Allow non-rectangular arrays, i.e. only have [a][p][e] for active elements
#   Allow other currencies or commodities (eg USD, AU, AG, etc)
#   Read single entry transactions
#   special notes for -l version
#   describe gpp properly
#   Share splits etc
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

  # A Document shortcut code
  Document_Shortcut = ":"

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
  OFS = ","

  # MONTH_FORMAT = "%Y %b %d"
  # ISO_FORMAT = "%F"
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
  ((SUBSEP in Account_Term)?((1)):((0)))
  ((SUBSEP in Accounting_Cost)?((1)):((0)))
  ((SUBSEP in Cost_Basis)?((1)):((0)))
  ((SUBSEP in Foreign_Offset_Limit)?((1)):((0)))
  ((SUBSEP in Held_From)?((1)):((0)))
  ((SUBSEP in Held_Until)?((1)):((0)))
  ((SUBSEP in Leaf)?((1)):((0)))
  ((SUBSEP in Lifetime)?((1)):((0)))
  ((SUBSEP in Long_Name)?((1)):((0)))
  ((SUBSEP in Maturity_Date)?((1)):((0)))
  ((SUBSEP in Method_Name)?((1)):((0)))
  ((SUBSEP in Number_Parcels)?((1)):((0)))
  ((SUBSEP in Parcel_Tag)?((1)):((0)))
  ((SUBSEP in Parent_Name)?((1)):((0)))
  ((SUBSEP in Price)?((1)):((0)))
  ((SUBSEP in Payment_Date)?((1)):((0)))
  ((SUBSEP in Qualified_Units)?((1)):((0)))
  ((SUBSEP in Tax_Adjustments)?((1)):((0)))
  ((SUBSEP in Tax_Bands)?((1)):((0)))
  ((SUBSEP in Tax_Credits)?((1)):((0)))
  ((SUBSEP in Threshold_Dates)?((1)):((0)))
  ((SUBSEP in Total_Units)?((1)):((0)))
  ((SUBSEP in Underlying_Asset)?((1)):((0)))
  ((SUBSEP in Units_Held)?((1)):((0)))

  # This is a CSV file
  read_csv_records()

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
  Key_Field    = (1)
  Value_Field  = (2)
  Key_Date     = 1
  Value_Date   = 0
  Import_Zero  = (0)
  Import_Time  = (12)

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

  # stop time - can be overriden by FY option
  # Need to initialize FY information
  if (FY)
    # Initialize the financial year
    Stop_Time = read_date(FY "-" FY_Date, 0)
  else {
    if (!Stop_Time)
      Stop_Time = Future
    else {
      Stop_Time = read_date(Stop_Time)
      assert((-1) < Stop_Time, "Stop_Time " Read_Date_Error)
    }
  }

  # EOFY statements are not printed until requested
  EOFY = "/dev/stderr"

  # Which account to track
  if ("" == Show_Account)
    Show_Account = (0)

  # Last time is the most recent earlier timestamp
  # Initially set the last time to be -1
  Last_Record = - 1

  # The last recorded timestamp in a state file
  # is Last_State - also initialized to -1
  Last_State = - 1
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

    # Scalar syntax
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

# Import data
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
      Filter_Data = Filter_Data " " Import_Array_Name



    # Prices are given a special import time
    if ("Price" == Import_Array_Name) {
      Import_Time = (16)
      Import_Zero = (0)
    } else
      Import_Time = (12)
  } else
    # End of block
    # Reset asset default prefix
    Asset_Prefix = ("ASSET.CAPITAL.SHARES")

  # End of if importing
  next
}

1 == Import_Record {
  import_csv_data(SYMTAB[Import_Array_Name], Asset_Prefix ":" Asset_Symbol, Import_Array_Name)
  next
}

##
## Imports CSV format
## <<,Account_Code, XYZ.ABC,>>
## <<,Key_Field, X,>>
## <<,Value_Field, Y,>>
## <<,Key_Date, 1,>>
## <<,Import_Array_Name, XXX,>>
##
## So for CBA price Data
## <<,Key_Field,1,>>
## <<,Value_Field,5>>
##
## Yields
## XXX[read_date($1)] => $5
##
## %%
## field-1, field-2, ..., field-NF
## ...
## %%

##
##
# This reads an array from csv data
function import_csv_data(array, symbol, name,
                         a, key, value) {


  # Check syntax
  assert(Key_Field <= NF && Value_Field <= NF, "Illegal import record syntax <" $0 ">")

  # Ok
  a = initialize_account(symbol)

  # Get the key
  if (Key_Date) {
    key = read_date(trim($Key_Field)) # Default Hour overruled sometimes
    assert((-1) != key, Read_Date_Error)

    # Skip dates before the epoch
    if ((-2) == key)
      return
  } else
    key = trim($Key_Field)

  # Get the value
  if (Value_Date) {
    value = read_date(trim($Value_Field))
    assert((-1) != value, Read_Date_Error)

    # Skip dates before the epoch
    if ((-2) == value)
      return
  } else {
    value = trim($Value_Field)
    if (!Import_Zero && (((value) <= Epsilon) && ((value) >= -Epsilon)))
      return # Don't import zero values
  }

  # Logging


  # Set the price
  (array[a][( key)] = ( value))

  # Done
}

/START_JOURNAL/ {
  # Allow multiple calls
  if (Start_Journal)
    next

  # Ensure agreement of Last_Record with Last_State
  if (Last_Record < Last_State)
    Last_Record = Last_State

  # Flag this
  Start_Journal = (1)

  # Is the currency consistent
  assert("AUD" == Journal_Currency, "Incompatible journal currency <" Journal_Currency "> in journal file - expected <" "AUD" "> instead")

  # Set default functions
  Income_Tax_Function     = "income_tax_" tolower(Journal_Currency)
  Initialize_Tax_Function = "initialize_tax_" tolower(Journal_Currency)
  Dividend_Qualification_Function = "dividend_qualification_" tolower(Journal_Currency)
  Imputation_Report_Function      = "imputation_report_" tolower(Journal_Currency)

  # These functions are not dependent on currency
  Balance_Profits_Function  = "balance_journal"
  Check_Balance_Function  = "check_balance"
  Update_Profits_Function = "update_profits"
  Update_Member_Function  = "update_member_liability"

  # Set the URI document prefix
  Document_URI = Document_Protocol url_encode(Document_Root)

  # Initialize local tax variables
  @Initialize_Tax_Function()

  #
  # Initialize state file information
  initialize_state()

  # All done
  next
}

function set_financial_year(now,   new_fy) {
  # Which Calendar year is this?
  FY_Year = (strftime("%Y", (now), UTC) + 0)

  # The timestamp at the end of the year
  # This assumes FY_Date is the date of the
  # first day of a financial year
  new_fy = read_date(FY_Year "-" FY_Date, 0)
  assert(new_fy > ((FY_Time) - 1), "Cannot regress financial year: Current FY => " get_date(FY_Time) " New FY => " get_date(new_fy))
  FY_Time = new_fy

  # Get the day number for the FY_Date
  FY_Day = (strftime("%j", (FY_Time), UTC) + 0)

  # Feb 28 has day number 59 so if FY_Day <= 60 - (1st day next FY)
  # then the current FY would include leap day if (FY - 1) was a leap year
  FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)
}

# Standard control function syntax is
# FUNCTION, [ARGUMENT_1, ARGUMENT_2]
# Control functions are:
#  SET_ENTRY
#  SET_BANDS
#  SET
#  CHECK
#
$1 ~ /^(CHECK|SET)/ { #|SET_BANDS|SET_ENTRY)/ {
 # Use a function so we can control scope of variables
 read_control_record()
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



## Functions start here
# Initialize read/write of state files
function initialize_state(    x) {
  # Get which variables to write out
  ((SUBSEP in Array_Names)?((1)):((0)))
  ((SUBSEP in Scalar_Names)?((1)):((0)))

  # Current Version
  MPX_Version = Current_Version = "Version " string_hash(("Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Lifetime Long_Name Maturity_Date Method_Name Number_Parcels Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Reserve_Rate ") ("MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window ALLOCATED Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function "))
  if ("" != Write_Variables) {
    # This time we just use the requested variables
    split(Write_Variables, Array_Names, ",")
    for (x in Array_Names)
      # Ensure the requested variable name is allowable - it could be an array or a scalar
      if (!index(("Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Lifetime Long_Name Maturity_Date Method_Name Number_Parcels Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Reserve_Rate "), Array_Names[x])) {
        assert(index(("MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window ALLOCATED Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function "), Array_Names[x]), "Unknown Variable <" Array_Names[x] ">")

        # This is a scalar
        Scalar_Names[x] = Array_Names[x]
        delete Array_Names[x]
      }
  } else {
    # Use default read and write list
    Write_Variables = (0)
    MPX_Arrays = ("Account_Term Accounting_Cost Cost_Basis Foreign_Offset_Limit Held_From Held_Until Leaf Lifetime Long_Name Maturity_Date Method_Name Number_Parcels Parcel_Tag Parent_Name Payment_Date Price Qualified_Units Tax_Adjustments Tax_Bands Tax_Credits Threshold_Dates Total_Units Underlying_Asset Units_Held " " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Reserve_Rate ")
    MPX_Scalars = ("MPX_Version MPX_Arrays MPX_Scalars Document_Root EOFY_Window FY_Day FY_Date FY_Length FY_Time Journal_Currency Journal_Title Journal_Type Last_State Qualification_Window ALLOCATED Dividend_Qualification_Function Income_Tax_Function Initialize_Tax_Function " " Balance_Profits_Function Check_Balance_Function ")

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
  FUTURE_PAYMENT   = initialize_account("SPECIAL.BALANCING:FUTURE.PAYMENT")

  # Keeping a record of taxable income, gains, losses
  TAXABLE_INCOME   = initialize_account("SPECIAL.TAX:TAXABLE.INCOME")
  INCOME_TAX       = initialize_account("SPECIAL.TAX:INCOME.TAX")

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
  ##FRANKING        = initialize_account("SPECIAL.FRANKING:FRANKING") # The Franking account balance
  ##FRANKING_PAID   = initialize_account("SPECIAL.FRANKING:FRANKING.PAID")

  ## Franking Credits
  #
  FRANKING          = initialize_account("SPECIAL.FRANKING:FRANKING") # The Franking account balance
  FRANKING_PAID     = initialize_account("SPECIAL.FRANKING:FRANKING.PAID") # Disbursed
  FRANKING_STAMPED  = initialize_account("SPECIAL.FRANKING:FRANKING.STAMPED") # Received through net tax paid

  # Franking deficit offset
  # Other offsets stored in unique accounts with same branch name
  FRANKING_DEFICIT   = initialize_account("SPECIAL.FRANKING.OFFSET:FRANKING.DEFICIT")

  # Franking tax account - a creditor like account
  FRANKING_TAX = initialize_account("LIABILITY.TAX:FRANKING.TAX")

  # Other tax credits, offsets & deductions
  LIC_CREDITS     = initialize_account("SPECIAL.TAX:LIC.CREDITS")

  # Accounting capital gains accounts
  REALIZED_GAINS  = initialize_account("INCOME.GAINS.REALIZED:GAINS")
  REALIZED_LOSSES = initialize_account("EXPENSE.LOSSES.REALIZED:LOSSES")
  MARKET_CHANGES  = initialize_account("EXPENSE.UNREALIZED:MARKET.CHANGES")

  # Extra capital gains accounts which can be manipulated independently of asset revaluations
  INCOME_LONG        = initialize_account("INCOME.GAINS:INCOME.LONG")
  INCOME_SHORT       = initialize_account("INCOME.GAINS:INCOME.SHORT")
  EXPENSE_LONG       = initialize_account("EXPENSE.LOSSES:EXPENSE.LONG")
  EXPENSE_SHORT      = initialize_account("EXPENSE.LOSSES:EXPENSE.SHORT")

  # Taxable capital gains are in special accounts
  # Tax Adjustments have potentially been applied to these quantities
  LONG_GAINS    = initialize_account("SPECIAL.TAXABLE.GAINS.LONG:LONG.GAINS")
  LONG_LOSSES   = initialize_account("SPECIAL.TAXABLE.LOSSES.LONG:LONG.LOSSES")
  SHORT_GAINS    = initialize_account("SPECIAL.TAXABLE.GAINS.SHORT:SHORT.GAINS")
  SHORT_LOSSES   = initialize_account("SPECIAL.TAXABLE.LOSSES.SHORT:SHORT.LOSSES")
  WRITTEN_BACK   = initialize_account("SPECIAL.TAXABLE.LOSSES:WRITTEN.BACK")
  CARRIED_LOSSES = initialize_account("SPECIAL.CARRIED:CARRIED.LOSSES")

  # Taxable carried losses
  TAX_LOSSES       = initialize_account("SPECIAL.CARRIED:TAX.LOSSES")

  #
  # Deferred Gains & Losses too (all long...)
  DEFERRED_GAINS  = initialize_account("SPECIAL.DEFERRED:DEFERRED.GAINS")
  DEFERRED_LOSSES = initialize_account("SPECIAL.DEFERRED:DEFERRED.LOSSES")
}

#
function read_control_record(       now, i, x, p, is_check){
  # Clear initial values
  new_line()

  # Use the last recorded date
  now = ((-1 != Last_Record)?( Last_Record):( Epoch))

  # The control records - must be exact match
  is_check = (0)
  x = trim($1)
  switch (x) {
    case "CHECK" :
      is_check = (1)
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

    ## Several dates can be set
    case "SET_FINANCIAL_YEAR" :
      # This would set the first day of the next FY so go back one day
      now = yesterday(read_date($2 "-" ("Jul-01")))
      assert(now > (-1), Read_Date_Error)
      if (now > Last_State)
        set_financial_year(now)
      break

    default: # This should not happen
      assert((0), "Unknown Control Record <" i ">")
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

    # We need to check for accounts changing from TERM=>CURRENT
    # Find the most recent threshold
    threshold = find_key(Threshold_Dates, t)

    #
    # Does if occur before now?
    # Comment this will not work if no transactions in that year
    # so need to ensure check at EOFY
    while (threshold > Last_Record) {
      # Which accounts does this key correpond to?
      for (a in Threshold_Dates[threshold]) {
        if (Threshold_Dates[threshold][a] > t) {
          # It is updated
          convert_term_account(a, t, Threshold_Dates[threshold][a])

          # Make sure this won't be picked up again
          Threshold_Dates[threshold][a] = t
        }
      }

      # Get the next earlier maturity date
      threshold = find_key(Threshold_Dates, ((threshold) - 1))
    }

    # Update the Last_Record
    Last_Record = t

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
    assert(t == Last_Record, sprintf("Current entry %s is earlier than the previously recorded transaction %s", $0, get_date(Last_Record)))

  # Modular form - parse the line
  # returns number of accounts
  n = parse_line(t)

  # There are n accounts in each transaction
  # Currently flag a single entry transaction as an error
  assert(2 == n || 0 == n, sprintf("<%s> - syntax error %d accounts found in transaction", $0, n))

  # If the transaction is already parsed simply print it out
  if (t < ((Last_State) + 1)) {
    if (2 == n)
      print_transaction(t, Comments " <**STATE**>", Account[1], Account[2], units, amount)
  } else if (2 == n) {
    parse_transaction(t, Account[1], Account[2], units, amount)
  } else # A zero entry line - a null transaction or a comment in the journal
    print_transaction(t, Comments)

  # Were totals changed by this transaction?
  @Check_Balance_Function(t)

  # Clean up the Account array
  delete Account
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
  ((SUBSEP in fields)?((1)):((0)))

  # Special franking provisions
  if (a == TAX) {
    # Reduce franking
    adjust_cost(FRANKING, - amount, now)

    # Note this as a reduction in the balance
    adjust_cost(FRANKING_STAMPED, amount, now)

    print_transaction(now, "Reduce Franking Balance", FRANKING, FRANKING_STAMPED, 0, amount)
   } else if (b != GST && ((b) ~ /^(ASSET\.CURRENT|LIABILITY)\.TAX[.:]/)) {
    # Increase franking
    adjust_cost(FRANKING, amount, now)

    # Note this as an increase in the balance
    adjust_cost(FRANKING_STAMPED, -amount, now)

    print_transaction(now, "Increase Franking Balance", FRANKING_STAMPED, FRANKING, 0, amount)
  }

  # A SMSF member benefit
  if (((b) ~ ("^" ( "EXPENSE.NON-DEDUCTIBLE.BENEFIT") "[.:]"))) {
    # But there is another complication - this needs to consider
    # unrealized gains too => so important assets are priced accurately
    #
    set_cost(MARKET_CHANGES, sum_market_gains(((now) - 1)), ((now) - 1))

    # This will change proportions so update the profits first
    @Update_Profits_Function(now)

    # Expense must be account b
    amount_taxed = amount * @Update_Member_Function(now, -amount, b)
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
      underlying_asset = (0)

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
        # INCOME.DIVIDEND     => SPECIAL.FRANKING.OFFSET
        # INCOME.DISTRIBUTION => SPECIAL.FRANKING.OFFSET
        # INCOME.FOREIGN      => SPECIAL.FOREIGN.OFFSET
        #
        if (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION") "[.:]")))
          credit_account = Tax_Credits[underlying_asset] = initialize_account("SPECIAL.FRANKING.OFFSET:I_TAX." Leaf[underlying_asset])
        else if (((a) ~ ("^" ( "INCOME.FOREIGN") "[.:]")))
          credit_account = Tax_Credits[underlying_asset] = initialize_account("SPECIAL.FOREIGN.OFFSET:C_TAX." Leaf[underlying_asset])
        else
          assert((0), sprintf("Can't link a tax credit account to income account %s", a))
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

      } else if (Qualification_Window && (((a) ~ ("^" ( "INCOME.DIVIDEND") "[.:]")) || ((a) ~ ("^" ( "INCOME.DISTRIBUTION.CLOSE") "[.:]")))) {
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
      amount -= (g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount)
      print_transaction(now, ("# GST " Leaf[b]), GST, b, II, g)

      # GST claimed
      GST_Claimable = 0
    }

    # A SMSF member contribution
    if (((a) ~ ("^" ( "INCOME.CONTRIBUTION") "[.:]"))) {
      # This will change proportions so update the profits first
      @Update_Profits_Function(now)

      # Drop the INCOME prefix
      @Update_Member_Function(now, amount, a)
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
    # The asset being sold must be "a" but if equity must be "b"
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
      units = - ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))
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
        g = - GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount

        # This must be recorded
        # This reduces GST liability
        print_transaction(now, ("# GST " Leaf[a]), GST, b, II, -g)
        assert((0), "GST Was levied on whole SELL transaction <" $0 ">")
      } else {
        # Brokerage Present => Adjust Brokerage
        # We Have A, B, -U, x - b, g
        # Produce A, B, -U, x - (1 - g) * x, # Note sign change with other case
        #         B, G,  0,           g * x, # Sign change engenders sense change in accounts
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * current_brokerage

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
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount
      else
        # Brokerage Present => Adjust Brokerage
        # Produce A, B, U, x + (1 - g) * b
        #         A, G,  0,         g * b
        g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * current_brokerage

      # Non-zero GST to be paid
      adjust_cost(GST, g, now)

      # This must be recorded
      print_transaction(now, ("# GST " Leaf[b]), a, GST, II, g)
      if ((((current_brokerage) <= Epsilon) && ((current_brokerage) >= -Epsilon)))
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
      # Instalment purchase
      adjust_cost(b,  amount, Extra_Timestamp)
      adjust_cost(a, -amount, now)

      # This will not balance when re-read from the state file unless balancing entries made
      adjust_cost(FUTURE_PAYMENT, -amount, Extra_Timestamp)
      adjust_cost(FUTURE_PAYMENT,  amount, now)

    } else if (((b) ~ /^(ASSET|LIABILITY)\.TERM[.:]/) || ((b) ~ /^(ASSET|LIABILITY)\.CURRENT[.:]/)) {
      # This is a term deposit or similar (eg a mortgage or loan issued by the fund)
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, now)
    } else
      assert((0),
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
      g = GST_Claimable * ((__MPX_H_TEMP__ = ((__MPX_KEY__ = find_key(GST_Rate, now))?( GST_Rate[__MPX_KEY__]):( ((0 == __MPX_KEY__)?( GST_Rate[0]):( 0)))))?( __MPX_H_TEMP__ / (1.0 + __MPX_H_TEMP__)):( 0)) * amount

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
  } else if ((a in Maturity_Date) && now > ((__MPX_KEY__ = find_key(Maturity_Date[a],  ((now) - 1)))?( Maturity_Date[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Maturity_Date[a][0]):( 0))))) {
    # Compute the maturity date
    Extra_Timestamp = add_months(now, ((__MPX_KEY__ = find_key(Account_Term[a],  now))?( Account_Term[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Account_Term[a][0]):( 0)))))
    (Maturity_Date[a][( now)] = ( Extra_Timestamp))
  } else
    # No term was set
    return a

  # Ensure the name  of this account is correct
  #   X.TERM => non-current
  #   X.CURRENT => current
  return convert_term_account(a, now, Extra_Timestamp)
}

#
# # Make sure current assets and liabilities are correctly identified
# Ensure the name  of this account is correct
#   X.TERM => non-current
#   X.CURRENT => current
# This is deprecated
# Use a filter function in print_account_class
# Use the absence of
function convert_term_account(a, now, maturity,       active_account, x, threshold) {


  # Is this a current or non-current account?
  active_account = a
  if (maturity > ((now) + one_year(now,  1))) {
    # Never switch a current account to a non-current account
    assert(((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/), sprintf("Cannot convert %s to a non-current account with maturity %s",
                                a, get_date(maturity)))

    # Store the timestamp  - the first entry will be the last timestamp
    # Actually this is not needed...
    # Make sure the array exists and  the  entry is unique
    threshold = ((maturity) - one_year(maturity, -1)) # Not  the same as now!
    Threshold_Dates[threshold][SUBSEP] = 0
    delete Threshold_Dates[threshold][SUBSEP]

    # The time "now" is recorded since  the entry can be modified later
    (Threshold_Dates[threshold][( active_account)] = ( maturity))

  } else if (((a) ~ /^(ASSET|LIABILITY)\.TERM[.:]/)) {
    # Need to identify this as a current account
    if (a in Maturity_Date)
      delete Maturity_Date[a]

  }

  # Return the active account
  return active_account
}

#
function update_profits(now) {
  # A No-op
  return
}

function update_member_liability(now, delta_profits, a) {
  # A no-op
  return
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
    # Check the action just after now
    switch(action) {
      case "VALUE" :
        assert(((account) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/), sprintf("CHECK: Only assets or equities have a VALUE or PRICE: not %s\n", (Leaf[(account)])))
        quantity = get_value(account, now); break
      case "PRICE" :
        assert(((account) ~ /^(ASSET\.(CAPITAL|FIXED)|EQUITY)[.:]/), sprintf("CHECK: Only assets or equities have a VALUE or PRICE: not %s\n", (Leaf[(account)])))
        quantity = ((__MPX_KEY__ = find_key(Price[account],  now))?( Price[account][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[account][0]):( 0)))); break

      case "COST" : quantity = get_cost(account, now); break

      case "UNITS" : quantity = ((__MPX_KEY__ = find_key(Total_Units[account],   now))?( Total_Units[account][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[account][0]):( 0)))); break
      default : assert((0), sprintf("%s => I don't know how to check %s\n",
                                      (Leaf[(account)]), action))
    }

    # Is this a checkpoint?
    assert((((amount - quantity) <= Epsilon) && ((amount - quantity) >= -Epsilon)), sprintf("%s fails checkpoint %s [%s] => %.4f != %.4f\n",
                                                 (Leaf[(account)]), action, get_date(now, LONG_FORMAT), quantity, amount))

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
          amount = get_cost(account, now) / ((__MPX_KEY__ = find_key(Total_Units[account],   now))?( Total_Units[account][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[account][0]):( 0))))

      case "PRICE" :
        # This is a single unit
        # Set the price per unit
        (Price[account][( now)] = ( amount))
      break

      case "COST" :
        # Override the account cost -> this can cause the accounts not to balance!
        set_cost(account, amount, now)

      break

      default : assert((0), sprintf("SET: I don't know how to set <%s> for account %s\n",
                                      action, (Leaf[(account)])))
    }

    # All Done
    return
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
  filter_data(Last_Record)

  # Make sure any eofy transactions are recorded
  # This loop will happen at least once
  if (Last_Record > Stop_Time)
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
    } while (FY_Time + EOFY_Window < Last_Record)

    # Fix FY_Time so that the snapshot is accurate
    FY_Year --
    FY_Length = (((( FY_Day) <= (60))?( ((((FY_Year) - 1) % 4 == 0 && ((FY_Year) - 1) % 100 != 0) || ((FY_Year) - 1) % 400 == 0)):( (((FY_Year) % 4 == 0 && (FY_Year) % 100 != 0) || (FY_Year) % 400 == 0))) + 365)

    FY_Time = ((FY_Time) - one_year(FY_Time, -1))
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
      printf "START_JOURNAL\n" > Write_State
  }
} #// END
#

#
# Filter Data
#
# Filter out irrelevant data - data that is out-of-range
# can arise from importing records
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

      } else {
        # Never held!

        unlink_account(a)
      }


    # End of each asset a
}

# The current value of an asset
function get_value(a, now) {
  # Depreciating assets are different
  if (((a) ~ /^ASSET\.CAPITAL[.:]/))
    return (((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0)))) * ((__MPX_KEY__ = find_key(Total_Units[a],   now))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0)))))

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
    dq = ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[a],   key))?( Qualified_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[a][0]):( 0))))):( ((__MPX_KEY__ = find_key(Total_Units[a],    key))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0)))))) - ((Qualification_Window)?(  ((__MPX_KEY__ = find_key(Qualified_Units[a],   next_key))?( Qualified_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Qualified_Units[a][0]):( 0))))):( ((__MPX_KEY__ = find_key(Total_Units[a],    next_key))?( Total_Units[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Total_Units[a][0]):( 0))))))

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

# This is the hard way to do it
# another way is return (Cost_Basis_change - market_value_change)
# this way is being used ATM for consistency checking reasons
function get_unrealized_gains(a, now,
                              current_price, p, gains, x) {
  if ((!is_open((a), ( now))))
    return 0 # NO unrealized gains

  # Sum the total value of the asset
  gains = 0
  current_price = ((__MPX_KEY__ = find_key(Price[a],  now))?( Price[a][__MPX_KEY__]):( ((0 == __MPX_KEY__)?( Price[a][0]):( 0))))



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
  proportional_cost = (0)
  if (((ac) ~ /^ASSET\.FIXED[.:]/)) {
    # Is this asset's depreciation upto date?
    # Depreciate

    if (now > FY_Time) {
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
  is_split = (0)

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
    is_split = (1)

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
  show_balance = (0)


  # Is there an error?
  if (!(((balance) <= Epsilon) && ((balance) >= -Epsilon))) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > "/dev/stderr"
    show_balance = (1)
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > "/dev/stderr"
    printf "\tAssets      => %20.2f\n", sum_assets > "/dev/stderr"
    printf "\tIncome      => %20.2f\n", sum_income > "/dev/stderr"
    printf "\tExpenses    => %20.2f\n", sum_expenses > "/dev/stderr"
    printf "\tLiabilities => %20.2f\n", sum_liabilities > "/dev/stderr"
    printf "\tEquities    => %20.2f\n", sum_equities > "/dev/stderr"
    if ((((sum_adjustments) > Epsilon) || ((sum_adjustments) < -Epsilon)))
      printf "\tAdjustments => %20.2f\n", sum_adjustments > "/dev/stderr"
    printf "\tBalance     => %20.2f\n", balance > "/dev/stderr"
    assert((((balance) <= Epsilon) && ((balance) >= -Epsilon)), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
