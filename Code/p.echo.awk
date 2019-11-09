#!/usr/local/bin/gawk -f
@include "mpx.h"
@include "p.shared.awk"

## Modify a journal file....
##
BEGIN {
  # Read a CSV file
  read_csv_records()
  set_special_accounts()
  set_months()

  # Required column widths
  required = "11 36 36 14"
  split(required, Width, " ")
}

# Comment record
/^([[:space:]])*#/  {
  print
  next
}

# State record
/^<</,/>>$/ {
  print
  next
}

# Control record
$1 ~  /^([[:space:]])*(CHECK|SET)/  {
  print
  next
}

# Start command
/START_JOURNAL/ {
  print
  next
}

# Everything else
{
  # Skip empty lines
  if ("" == $0)
    next

 # Use a function so we can control scope of variables
 switch_units()
 next
}

# # read csv records
# function read_csv_records() {
#   # This is a CSV file
#   FPAT = "([^,]*)|(\"[^\"]+\")"
# }


# Possible record styles
# Journal styles (date format is somewhat flexible)
#      2017 Aug 24, AMH.DIV, AMH.ASX, 3072, 2703.96, [1025.64,] [1655.49], # DRP & LIC
function parse_old_line(now,    i, j, x, number_accounts) {
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
    $4 = Write_Units
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
        Comments = add_field(Comments, x, ", ")
    }

    # Increment i
    i ++
  }

  # Comments should be signified with an octothorpe
  if (Comments !~ /^#/)
    Comments = add_field("# ", Comments, ", ")

  # Documents can be added as comments
  # Some special document names are supported
  # So for example [<Buy>] expands to ABC Buy YYYY Mon
  # and            [<Chess.x>] expands to ABC Chess YYYY Mon DD
  for (x in Documents) {
    delete Documents[x]

    # Parse this document name
    i = parse_document_name(x, now)

    # Add the parsed name to the comments
    Comments = add_field(Comments, i, ", ")
  }

  # All done - return record type
  return number_accounts
}

function switch_units(    t, n, i) {
  # Have to process enough to determine number of accounts
  new_line()
  t = read_date($1)
  # t should be positive
  assert(t > DATE_ERROR, Read_Date_Error)

  # Modular form - parse the line
  # returns number of accounts
  # This parses the old version of the journal file
  # with a fixed Unit column in field 4
  n = parse_old_line(t)

  # pretty print each column
  if ((2 == n) && (NF > 4)) {
    # Units are column 4
    # If they are zero just elide them
    i = $4
    if (0 != i) {
      # Swop
      $4 = $5
      $5 = i
    } else {
      # Shuffle down fields
      for (i = 4; i < NF; i ++)
        $i = $(i+1)
      NF --
    }
  }

  for (i = 1; i < NF; i++)
    if (i in Width)
      printf "%*s, ", Width[i], $i
    else
      printf "%14s, ", $i
  if (i in Width)
    printf "%*s\n", Width[i], $i
  else
    printf "%s\n", $i
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
  if (0 != units)
    Write_Units  = sprintf("%10.3f", units)
  else if (Tax_Adjustment)
    Write_Units =  "(" Cost_Element ")"
  else if (COST_ELEMENT == Cost_Element)
    Write_Units = 0 # Simpler to read and most common case
  else
    Write_Units = Cost_Element

  return units
}
