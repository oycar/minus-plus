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
@include "mpx.h"
@include "p.shared.awk"
@include "p.eofy.awk"

# Currency specific modules
@ifeq JOURNAL_CURRENCY "AUD"
@include "p.aud_modules.awk"
@include "p.smsf_modules.awk"
@elif JOURNAL_CURRENCY "USD"
@include "p.usd_modules.awk"
@endif

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
  make_array(Real_Value)

  # And a gains stack
  make_array(Gains_Stack)
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
    DATE_FORMAT = MONTH_FORMAT
  LONG_FORMAT = (DATE_FORMAT " %H::%M::%S")

  # Import Record is Off
  Import_Record = FALSE
  Filter_Data = ""

  # Suppress rounding errors
  Epsilon = 0.0001

  # The Time Zone
  Time_Zone = 10
  UTC = 1

  # The Epoch
  # A more practical Epoch
  Epoch = mktime(EPOCH_START " 01 01 00 00 00", UTC)

  # A distant Future
  Future = mktime(EPOCH_END " 12 31 00 00 00", UTC)

  # Default FY date
  FY_Date = FY_DATE

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
  make_array(Account_Term)
  make_array(Accounting_Cost)
  make_array(Cost_Basis)
  make_array(Foreign_Offset_Limit)
  make_array(Held_From)
  make_array(Held_Until)
  make_array(Leaf)
  make_array(Leaf_Count)
  make_array(Lifetime)
  make_array(Long_Name)
  make_array(Maturity_Date)
  make_array(Method_Name)
  make_array(Number_Parcels)
  make_array(Parcel_Tag)
  make_array(Parent_Name)
  make_array(Price)
  make_array(Payment_Date)
  make_array(Qualified_Units)
  make_array(Tax_Adjustments)
  make_array(Tax_Bands)
  make_array(Tax_Credits)
  make_array(Threshold_Dates)
  make_array(Total_Units)
  make_array(Underlying_Asset)
  make_array(Units_Held)

  # This is a CSV file
  read_csv_records()

  # Transaction line defaults
  new_line()

  # Default Portfolio Name and document URI
  Journal_Title = "NEMO"
  Journal_Type = "IND"
  Document_Root = "https:example.com/NEMO"

  # Default currency
  Journal_Currency = JOURNAL_CURRENCY

  # Importing CSV files
  # Default Price Record Class
  Asset_Prefix = ASSET_PREFIX

  # A short name
  Asset_Symbol = ""

  # Default import fields
  Key_Field    = KEY_FIELD
  Value_Field  = VALUE_FIELD
  Key_Date     = KEY_DATE
  Value_Date   = VALUE_DATE
  Import_Zero  = FALSE
  Import_Time  = HOUR

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

  # EOFY statements are not printed until requested
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
@ifeq LOG read_state
      printf "%s => %s\n", Variable_Name, SYMTAB[Variable_Name] > STDERR
@endif
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
/^QQ/  {
  #
  Import_Record = !Import_Record
  if (Import_Record) {
    # Filter data
    # Currently importing Import_Array
    if (!index(Filter_Data, Import_Array_Name))
      # Make sure this array will be filtered
      Filter_Data = Filter_Data " " Import_Array_Name

@ifeq LOG import_record
    printf "Import %s\n",  Import_Array_Name > STDERR
    printf "Filter %s\n",  Filter_Data > STDERR
    if (Asset_Symbol)
      printf "\t%s\n", Asset_Symbol > STDERR
@endif

    # Prices are given a special import time
    if ("Price" == Import_Array_Name) {
      Import_Time = CLOSING
      Import_Zero = FALSE
    } else
      Import_Time = HOUR
  } else
    # End of block
    # Reset asset default prefix
    Asset_Prefix = ASSET_PREFIX

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
## <<,Date_Field, "1:0">>
##
## Yields
## XXX[read_date($1)] => $5
##
## CSV
## field-1, field-2, ..., field-NF
## ...
## CSV

##
##
# This reads an array from csv data
function import_csv_data(array, symbol, name,
                         a, key, value) {


  # Check syntax
  assert(Key_Field <= NF && Date_Field <= NF, "Illegal import record syntax <" $0 ">")

  # Ok
  a = initialize_account(symbol)

  # Get the key
  if (Key_Date) {
    key = read_date(trim($Key_Field)) # Default Hour overruled sometimes
    assert(DATE_ERROR != key, Read_Date_Error)

    # Skip dates before the epoch
    if (BEFORE_EPOCH == key)
      return
  } else
    key = trim($Key_Field)

  # Get the value
  if (Value_Date) {
    value = read_date(trim($Value_Field))
    assert(DATE_ERROR != value, Read_Date_Error)

    # Skip dates before the epoch
    if (BEFORE_EPOCH == value)
      return
  } else {
    value = trim($Value_Field)
    if (!Import_Zero && near_zero(value))
      return # Don't import zero values
  }

  # Logging
@ifeq LOG import_record
  printf "%s %16s[%11s] => %14s\n", name, Leaf[a], ternary(Key_Date, get_date(key), key), ternary(Value_Date, get_date(value), print_cash(value, 4)) > STDERR
@endif

  # Set the price
  set_entry(array[a], value, key)

  # Done
}


# # This function actually reads the CBA formatted record
# # It is very minimal
# # Syntax of record is
# # $1 => Qualifying_Date, $2 => Dividend amount in cents (ignored), $3 => Record Date (ignored), $4 => Payment_Date (used), $5 => Dividend_Type (ignored), $6... (ignored)
# # 07/03/2019,77.3232,08/03/2019,26/03/2019,I,-,
# function read_qualifying_dates(  a, q_date, p_date) {
#   # Get the account
#   a = initialize_account(Asset_Prefix ":" Symbol)
#
#   # Ok
#   q_date = read_date($1) # Default hour - qualifying date
#
#   # A legal date?
#   if (q_date < Epoch)
#     return
#
#   # Now the payment date
#   p_date = read_date($4) # Default hour - payment date
#   assert(p_date > q_date, "Qualifying date <" $1 "> must always precede payment date <" $4 ">")
#   # Logging
# @ifeq LOG read_qualifying_dates
#   printf "\t%s, %s\n", get_date(q_date), get_date(p_date) > STDERR
# @endif
#
#   # Set the ex dividend date
#   set_entry(Payment_Date[a], p_date, q_date)
#
#   # Done
# }

/START_JOURNAL/ {
  if (NF > 1) {
    # Check not called before
    assert(-1 == Last_Time, "Can't START_JOURNAL twice")

    # Is the currency consistent
    assert(JOURNAL_CURRENCY == Journal_Currency, "Incompatible journal currency <" Journal_Currency "> in journal file - expected <" JOURNAL_CURRENCY "> instead")

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
  assert(Last_Time != DATE_ERROR, Read_Date_Error)

  # Need to initialize FY information
  if (FY) {
    # Initialize the financial year
    Stop_Time = read_date(FY "-" FY_Date, 0)
    Start_Time = last_year(Stop_Time)
  } else {
    # Default Start_Time and Stop_Time
    if (!Start_Time)
      Start_Time = Last_Time
    else {
      Start_Time = read_date(Start_Time)
      assert(DATE_ERROR != Start_Time, "Start_Time " Read_Date_Error)
    }

    # Is a specific stop time set
    if (!Stop_Time)
      Stop_Time = Future
    else {
      Stop_Time = read_date(Stop_Time)
      assert(DATE_ERROR != Stop_Time, "Stop_Time " Read_Date_Error)
    }
  }

  # Initialize local tax variables
  @Initialize_Tax_Function()

  #
  # Initialize state file information
  initialize_state()

  # Which Calendar year is this?
  FY_Year = get_year_number(Last_Time)

  # The timestamp at the end of the year
  # This assumes FY_Date is the date of the
  # first day of a financial year
  FY_Time = read_date(FY_Year "-" FY_Date, 0)

  # Get the day number for the FY_Date
  FY_Day = get_day_number(FY_Time)

  # Feb 28 has day number 59 so if FY_Day <= 60 - (1st day next FY)
  # then the current FY would include leap day if (FY - 1) was a leap year
  FY_Length = get_year_length(FY_Year, FY_Day)

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
# Initialize read/write of state files
function initialize_state(    x) {
  # Get which variables to write out
  make_array(Array_Names)
  make_array(Scalar_Names)

  # Current Version
  MPX_Version = Current_Version = "Version " string_hash(MPX_ARRAYS MPX_SCALARS)
  if ("" != Write_Variables) {
    # This time we just use the requested variables
    split(Write_Variables, Array_Names, ",")
    for (x in Array_Names)
      # Ensure the requested variable name is allowable - it could be an array or a scalar
      if (!index(MPX_ARRAYS, Array_Names[x])) {
        assert(index(MPX_SCALARS, Array_Names[x]), "Unknown Variable <" Array_Names[x] ">")

        # This is a scalar
        Scalar_Names[x] = Array_Names[x]
        delete Array_Names[x]
      }
  } else {
    # Use default read and write list
    Write_Variables = FALSE
    MPX_Arrays = MPX_ARRAYS
    MPX_Scalars = MPX_SCALARS

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
  #CAPITAL_LOSSES   = initialize_account("SPECIAL.TAX:CAPITAL.LOSSES")
  #TAX_LOSSES       = initialize_account("SPECIAL.TAX:TAX.LOSSES")
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

  # Taxable carried losses
  TAX_LOSSES       = initialize_account("SPECIAL.LOSSES.CARRIED:TAX.LOSSES")
  CAPITAL_LOSSES   = initialize_account("SPECIAL.LOSSES.CAPITAL:CAPITAL.LOSSES")

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
  now = ternary(-1 != Last_Time, Last_Time, Epoch)

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
@ifeq LOG checkset
      printf "%%%%, %s[%s]", i, get_date(now) > STDERR
@endif
      set_array_bands(now, SYMTAB[i], NF)
      break

    case "SET_ENTRY" :
      # Set a time-dependent array entry
      i = trim($2)
      assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
      p = strtonum($3)
@ifeq LOG checkset
      printf "%%%%, %s[%s] => %11.2f\n", i, get_date(now), p > STDERR
@endif
      set_entry(SYMTAB[i], p, now)
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
@ifeq LOG checkset
    printf "[%s] => %11.2f\n\t", k, bands[now][-k] > STDERR
@endif
  }

  # Need to deal with case
  # %%,DATE,<ACTION>,15
  # i.e $(i+1) does not exist
  if (3 == nf) {
    bands[now][0] = strtonum($nf)
@ifeq LOG checkset
    printf " => %11.2f\n", bands[now][0] > STDERR
@endif
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
  assert(t != DATE_ERROR, Read_Date_Error)

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
      threshold = find_key(Threshold_Dates, just_before(threshold))
    }

    # Update the Last_Time
    Last_Time = t

    while (FY_Time + EOFY_Window < t) {
      # Get each EOFY in turn
      eofy_actions(FY_Time)

      # The next financial year - unless we are stopping
      FY_Year ++

      # Update FY length
      FY_Length = get_year_length(FY_Year, FY_Day)
      FY_Time = next_year(FY_Time)
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
      printf "## Single Entry Transaction\n" > STDERR
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
  make_array(fields)

  # Special franking provisions
  if (a == TAX) {
    # Reduce franking
    adjust_cost(FRANKING, - amount, now)

    print_transaction(now, "Reduce Franking Balance", FRANKING, NULL, 0, amount)
   } else if (is_tax(b)) {
    # Increase franking
    adjust_cost(FRANKING, amount, now)

    print_transaction(now, "Increase Franking Balance", NULL, FRANKING, 0, amount)
  }

  # A SMSF member benefit
  if (is_class(b, "EXPENSE.NON-DEDUCTIBLE.BENEFIT")) {
    # But there is another complication - this needs to consider
    # unrealized gains too => so important assets are priced accurately
    #
    set_cost(MARKET_CHANGES, sum_market_gains(just_before(now)), just_before(now))

    # This will change proportions so update the profits first
    update_profits(now)

    # Expense must be account b
    amount_taxed = amount * update_member_liability(now, -amount, b)
    if (!is_suffix(b, "TAXABLE") && !is_suffix(b, "TAX-FREE")) {
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
  if (is_term(a)) {
    assert(!is_term(b),
           sprintf("Transactions between two fixed term accounts %s & %s are not supported due to ambiguity of timestamps",
                   a, b))
    # Set the term
    a = set_account_term(a, now)
  } else if (is_term(b))
    b = set_account_term(b, now)

  # Initially no optional fields
  number_fields = 0

  # Is this a franked transaction?
  if (is_class(a, "INCOME")) {
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
    if (!near_zero(tax_credits)) {
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
        if (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION"))
          credit_account = Tax_Credits[underlying_asset] = initialize_account("SPECIAL.OFFSET.FRANKING:I_TAX." Leaf[underlying_asset])
        else if (is_class(a, "INCOME.FOREIGN"))
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
    if (!near_zero(Real_Value[2])) {
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
        set_entry(Payment_Date[underlying_asset], now, Extra_Timestamp)

      } else if (Qualification_Window && (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION.CLOSELY_HELD"))) {
        Extra_Timestamp = get_exdividend_date(underlying_asset, now)

        # This must exist
        assert(DATE_ERROR != Extra_Timestamp, "Cannot find the ex-dividend date for <" Leaf[a] "> relating to the payment date <" get_date(now) ">")
        fields[number_fields = 1] = get_date(Extra_Timestamp)
      }

      # Clear the timestamp
      Extra_Timestamp = DATE_ERROR
    }

    # Now check for GST
    if (!near_zero(GST_Claimable)) {
      # This is GST collected
      # The transaction itself will be posted later a => b
      # Need to adjust amount transacted
      amount -= (g = GST_Claimable * gst_proportion(now) * amount)
      print_transaction(now, ("# GST " Leaf[b]), GST, b, II, g)

      # GST claimed
      GST_Claimable = 0
    }

    # A SMSF member contribution
    if (is_class(a, "INCOME.CONTRIBUTION")) {
      # This will change proportions so update the profits first
      update_profits(now)

      # Drop the INCOME prefix
      update_member_liability(now, amount, a)
    }
  } else if (is_class(b, "EXPENSE.NON-DEDUCTIBLE.DIVIDEND")) {
    # A franking entity (eg company) can distribute franking credits
    tax_credits = Real_Value[1]
    Real_Value[1] = 0

    # Simplified version of above
    if (!near_zero(tax_credits)) {
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
    correct_order = is_asset(a) && is_open(a, now)
    if (!correct_order) {
      correct_order = is_equity(b) && is_open(b, now)
      if (correct_order) {
        swop = a; a = b; b = swop
        amount = - amount
      }
    }
    assert(correct_order, sprintf("%s => can't sell either %s or %s\n", $0, get_short_name(a), get_short_name(b)))

    # Get the number of units to be sold in the special case of sell all
    if ("SELL" == Write_Units) {
      units = - get_units(a, now)
      Write_Units = sprintf("%10.3f", units)
    }

    # Get brokerage (if any)
    current_brokerage = Real_Value[1]
    Real_Value[1] = 0

    # Amount should be the consideration as recorded by the broker
    #
    # Impact of GST
    if (!near_zero(GST_Claimable)) {
      # Two cases
      #   Not Present => Adjust Whole Amount
      if (near_zero(current_brokerage)) {
        # No Brokerage
        # A sale
        # A, B, -U,  (1 - g) * x
        # G, B,  0,        g * x
        g = - GST_Claimable * gst_proportion(now) * amount

        # This must be recorded
        # This reduces GST liability
        print_transaction(now, ("# GST " Leaf[a]), GST, b, II, -g)
        assert(FALSE, "GST Was levied on whole SELL transaction <" $0 ">")
      } else {
        # Brokerage Present => Adjust Brokerage
        # We Have A, B, -U, x - b, g
        # Produce A, B, -U, x - (1 - g) * x, # Note sign change with other case
        #         B, G,  0,           g * x, # Sign change engenders sense change in accounts
        g = GST_Claimable * gst_proportion(now) * current_brokerage

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

@ifdef EXPORT_FORMAT
    # Export format
    # Needs fixed number of fields
    # A, B, -U, x, b, b*g, (parcel_timestamp || parcel_name), SELL, comment

    # Record the GST on the brokerage
    fields[1] = sprintf("%.*f", PRECISION, current_brokerage) # Always use 1st field
    fields[2] = sprintf("%.*f", PRECISION, g)
    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if (DATE_ERROR != Extra_Timestamp) # No timestamp
      fields[3] = get_date(Extra_Timestamp)
    else
      # Otherwise save the parcel name
      fields[3] = Parcel_Name

    fields[3] = fields[1]
    fields[4] = "SELL"
    # Exclude brokerage here because the importing package usually expects to find the consideration alone
    print_transaction(now, Comments, a, b, Write_Units, amount + g, fields, 4)
@else
    # Record the transaction
    if (!near_zero(current_brokerage))
      fields[++ number_fields] = sprintf("%.*f", PRECISION, current_brokerage - g) # Always use 1st field

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if (DATE_ERROR != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Normally next field is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # Normal format is much the same - but number of fields is not fixed
    # A, B, -U, x + bg, (optional_fields), comment
    print_transaction(now, Comments, a, b, Write_Units, amount + g, fields, number_fields)
@endif

  } else if (units > 0) {
    # For a purchase the asset must be account "b"
    correct_order = is_asset(b)
    if (!correct_order) {
      correct_order = is_equity(a)
      if (correct_order) {
        swop = a; a = b; b = swop
        amount = - amount
      }
    }
    assert(correct_order, sprintf("%s => can't buy asset %s\n", $0, get_short_name(b)))

    # Normally fields[1] is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if (DATE_ERROR != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Is this a new depreciating asset?
    if (is_fixed(b) && !(b in Method_Name)) {
      # This is  the asset Lifetime
      fields[++ number_fields] = Lifetime[b]  = Real_Value[1]; Real_Value[1] = 0
      assert(!near_zero(Lifetime[b]), sprintf("%s => Can't have a fixed asset %s with zero life", $0, get_short_name(b)))

      # We need the method name
      # Currently a choice of POOL, DV, or PC
      fields[++ number_fields] = Method_Name[b] = Depreciation_Type
    }

    # Allow for brokerage if required - note can't have brokerage with depreciation
    if (!near_zero(Real_Value[1])) {
      current_brokerage = Real_Value[1]
      Real_Value[1] = 0
    }

    # Simply adjust cost of <a> by the whole amount
    adjust_cost(a, -amount, now)

    # Impact of GST
    if (!near_zero(GST_Claimable)) {
      # Two cases
      #   No Brokerage Present => Adjust Whole Amount
      if (near_zero(current_brokerage))
        # A  purchase
        # A, B, U, (1 - g) * x
        # A, G, 0,      g * x
        g = GST_Claimable * gst_proportion(now) * amount
      else
        # Brokerage Present => Adjust Brokerage
        # Produce A, B, U, x + (1 - g) * b
        #         A, G,  0,         g * b
        g = GST_Claimable * gst_proportion(now) * current_brokerage

      # Non-zero GST to be paid
      adjust_cost(GST, g, now)

      # This must be recorded
      print_transaction(now, ("# GST " Leaf[b]), a, GST, II, g)
      if (near_zero(current_brokerage))
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
@ifdef EXPORT_FORMAT
    # Format is
    # Needs fixed number of fields - we can ignore parcel name etc..
    # A, B, U, x + b, b, b*g, <parcel-name>, BUY, Comments
    # Record the GST on the brokerage
    fields[3] = Parcel_Name
    fields[1] = sprintf("%.*f", PRECISION, current_brokerage) # Always use 2nd field
    fields[2] = sprintf("%.*f", PRECISION, g)
    fields[4] = "BUY"

    print_transaction(now, Comments, a, b, Write_Units, amount, fields, 4)
@else
    # Normal transactions
    # A, B, U, x, b - b*g, <optional-fields>, Comments
    # Record the adjustment due to brokerage and gst
    if (!near_zero(current_brokerage))
      fields[++ number_fields] = sprintf("%.*f", PRECISION, current_brokerage - g)
    print_transaction(now, Comments, a, b, Write_Units, amount - g, fields, number_fields)
@endif
  } else if (Automatic_Depreciation) {
    # This is automatic depreciation
    # Only need the assertion
    assert(is_fixed(a), sprintf("%s => Can't depreciate account %s", $0, a))

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
    if (is_unitized(a)) {
      assert(!is_unitized(b),
               sprintf("%s Both %s and %s cannot be unitized when parcel timestamp [%s] is set",
               $0, get_short_name(a), get_short_name(b), get_date(Extra_Timestamp)))
      adjust_cost(a, -amount, Extra_Timestamp)
      adjust_cost(b,  amount, now)
    } else if (is_unitized(b)) {
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, Extra_Timestamp)
    } else if (is_term(b) || is_current(b)) {
      # This is a term deposit or similar (eg a mortgage or loan issued by the fund)
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, now)
    } else
      assert(FALSE,
             sprintf("<%s> Either %s or %s must be a capital asset or a term asset when timestamp [%s] is set",
             $0, get_short_name(a), get_short_name(b), get_short_name(b), get_date(Extra_Timestamp)))

     # Record the transaction
     print_transaction(now, Comments, a, b, Write_Units, amount, fields, number_fields)
  } else {
    # All Other Transactions
    # This must be an expense if GST is involved
    if (!near_zero(GST_Claimable)) {
      # An expense
      # A, B, 0, (1 - g) * x
      # A, G, 0,      g * x
      g = GST_Claimable * gst_proportion(now) * amount

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
   set_entry(Account_Term[a], Real_Value[1], now)

  # At this stage term is assumed to be set in months
  if (Extra_Timestamp > now) { # Use the time stamp if it exists
    set_entry(Maturity_Date[a], Extra_Timestamp, now)

    # Don't use real value again
    Real_Value[1] = 0
  } else if (Real_Value[1] > 0) {
    # Need to set the first maturity date - real value is the same as account term
    Extra_Timestamp = add_months(now, Real_Value[1])
    set_entry(Maturity_Date[a], Extra_Timestamp, now)

    # Don't use real value again
    Real_Value[1] = 0
  } else if ((a in Maturity_Date) && now > find_entry(Maturity_Date[a], just_before(now))) {
    # Compute the maturity date
    Extra_Timestamp = add_months(now, find_entry(Account_Term[a], now))
    set_entry(Maturity_Date[a], Extra_Timestamp, now)
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
@ifeq LOG update_fixed_account
  printf "%s => %s\n", LOG, get_date(now) > STDERR
  printf "\tActive account => %s\n", a > STDERR
  printf "\t\t Cost     => %s\n", print_cash(get_cost(a, now)) > STDERR
  printf "\t\t Maturity => %s\n", get_date(maturity) > STDERR
  printf "\t\t Term     => %d\n", find_entry(Account_Term[a], now) > STDERR
@endif

  # Is this a current or non-current account?
  active_account = a
  if (maturity > next_year(now)) {
    # Never switch a current account to a non-current account
    assert(is_term(a), sprintf("Cannot convert %s to a non-current account with maturity %s",
                                a, get_date(maturity)))

    # Store the timestamp  - the first entry will be the last timestamp
    # Actually this is not needed...
    # Make sure the array exists and  the  entry is unique
    threshold = last_year(maturity) # Not  the same as now!
    Threshold_Dates[threshold][SUBSEP] = 0
    delete Threshold_Dates[threshold][SUBSEP]

    # The time "now" is recorded since  the entry can be modified later
    set_entry(Threshold_Dates[threshold], maturity, active_account)
@ifeq LOG update_fixed_account
    printf "\tThreshold_Dates => \n" > STDERR
    walk_array(Threshold_Dates, 1, STDERR)
    printf "\n" > STDERR
@endif
  } else if (is_term(a)) {
    # Need to rename account
    # TERM => CURRENT
    active_account = gensub(/(\.TERM)([.:])/, ".CURRENT\\2", 1, a)

    # Create the new account is necessary
    active_account = initialize_account(active_account)

    # Now create a synthetic transaction
    # DATE, A, ACTIVE_ACCOUNT, 0, COST(A), # ....
    set_cost(active_account, get_cost(a, just_before(now)), now)
    set_cost(a, 0, now)

@ifeq LOG update_fixed_account
    printf "\tRenamed account => %s\n", active_account > STDERR
    printf "\t\t Cost     => %s\n", print_cash(get_cost(active_account, now)) > STDERR
@endif
  }

  # Return the active account
  return active_account
}

# A wrapper function updates allocated profits when required ()
function update_profits(now,     delta_profits) {
  # Compute the profits that need to be allocated to members
  # These are the profits accumulated since the last time they were distributed to members
  delta_profits = accumulated_profits(now) - get_cost(ALLOCATED, now)
  if (!near_zero(delta_profits)) {
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
  action = get_short_name(a)

  # First lets check
  if (is_check) {
    switch(action) {
      case "VALUE" :
        assert(is_unitized(account), sprintf("CHECK: Only assets or equities have a VALUE or PRICE: not %s\n", get_short_name(account)))
        quantity = get_value(account, now); break
      case "PRICE" :
        assert(is_unitized(account), sprintf("CHECK: Only assets or equities have a VALUE or PRICE: not %s\n", get_short_name(account)))
        quantity = find_entry(Price[account], now); break

      case "COST" : quantity = get_cost(account, now); break

      case "UNITS" : quantity = get_units(account, now); break
      default : assert(FALSE, sprintf("%s => I don't know how to check %s\n",
                                      get_short_name(account), action))
    }

    # Is this a checkpoint?
    assert(near_zero(amount - quantity), sprintf("%s fails checkpoint %s[%s] => %.4f != %.4f\n",
                                                 action, get_short_name(account), get_date(now), quantity, amount))

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
          amount = get_cost(account, now) / get_units(account, now)

      case "PRICE" :
        # This is a single unit
        # Set the price per unit
        set_entry(Price[account], amount, now)
      break

      case "COST" :
      case "SIZE" : # Just an alias for COST - looks neater for non cash special accounts
        # Override the account cost -> this can cause the accounts not to balance!
        set_cost(account, amount, now)
      break

      default : assert(FALSE, sprintf("SET: I don't know how to set <%s> for account %s\n",
                                      action, get_short_name(account)))
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

@ifeq LOG update_member_liability
  printf "Update Liabilities [%s]\n", get_date(now) > STDERR
  if (member_id)
    printf "\t%20s => %s\n", "Member id", member_id > STDERR
  printf "\tMember Shares\n" > STDERR
@endif # LOG

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

@ifeq LOG update_member_liability
    sum_share = 1.0
    printf "\t%20s => %8.6f %16s => %14s\n", Leaf[member_id], sum_share, Leaf[member_id], print_cash(- amount) > STDERR
@endif # LOG
  } else { # Get totals
    # We still get the share from each account
    # Don't use the accumulated totals because (rarely) a negative account balance will break the proportioning
    # Also since  the order of transactions on a particular day is not defined use just_before() to compute proportions
    for (member_account in Member_Liability)
      if (!member_id || is_ancestor(member_id, member_account)) {
        share[member_account] = x = get_cost(member_account, just_before(now))
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
@ifeq LOG update_member_liability
      sum_share += x
      printf "\t%20s => %8.6f %16s => %14s\n", Leaf[member_account], x, Leaf[target_account], print_cash(- x * amount) > STDERR
      if (get_cost(target_account, now) > 0)
        printf "\t\tNegative Balance in target account %16s => %14s\n", Leaf[target_account], print_cash(- get_cost(target_account, now)) > STDERR
@endif # LOG
    } # End of exact share

    # Tidy up
    delete share
  } # End of allocation

@ifeq LOG update_member_liability
  # Just debugging
  printf "\t%20s => %8.6f %16s => %14s\n", "Share", sum_share, "Total", print_cash(- amount) > STDERR
@endif # LOG

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
  if (is_class(a, "INCOME.CONTRIBUTION")) {
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
  assert(MPX_Version == Current_Version, \
    "Inconsistent snapshot file:\n\tExpected Version => " Current_Version " Found Version => " MPX_Version)

  # Delete empty accounts
  # Filter out data entries that were added by import CSV records
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
      FY_Length = get_year_length(FY_Year, FY_Day)
      FY_Time = next_year(FY_Time)
    } while (FY_Time + EOFY_Window < Last_Time)

    # Fix FY_Time so that the snapshot is accurate
    FY_Year --
    FY_Length = get_year_length(FY_Year, FY_Day)

    FY_Time = last_year(FY_Time)
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
@ifeq LOG filter_data
 printf "Filter Data %s\n", name > STDERR
@endif

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
            # Filter the old block
@ifeq LOG filter_data
            # List this block
            printf "%12s, %03d, %s, %s\n", Leaf[a], block_id, get_date(start_block), get_date(end_block) > STDERR
@endif

            # # Check the data against each block
            filter_block(key, data_array[a], start_block, end_block)

            # Remove anything kept to speed up processing
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

        # The last holding block
@ifeq LOG filter_data
          printf "%12s, %03d, %s, %s\n", Leaf[a], block_id, get_date(start_block), get_date(end_block) > STDERR
@endif
          # Check the data against each block
          filter_block(key, data_array[a], start_block, end_block)

@ifeq LOG filter_data
        for (key in stack)
          printf "\tKeep   => %s\n", get_date(key) > STDERR
@endif
        # Copy the kept items back
        for (key in stack)
          data_array[a][key] = stack[key]
        delete stack

      } else {
        # Never held!
@ifeq LOG filter_data
        printf "%12s Never Held!\n", Leaf[a] > STDERR
@endif
        unlink_account(a)
      }


    # End of each asset a
}

# The current value of an asset
function get_value(a, now) {
  # Depreciating assets are different
  if (is_capital(a))
    return (find_entry(Price[a], now) * get_units(a, now))

  # Just the cost
  return get_cost(a, now)
}

# Sell qualified units
# This is trickier than ordinary units
# because the qualification is provisional until the
# qualification window is expired
function sell_qualified_units(a, u, now, half_window,      du, dq, key, next_key) {
@ifeq LOG qualified_units
  printf "Sell Qualified Units [%s]\n", get_short_name(a) > STDERR
  printf "\tDate              => %s\n", get_date(now) > STDERR
  printf "\tWindow End        => %s\n", get_date(now + half_window) > STDERR
  printf "\tQualified Units   => %.3f\n", get_qualified_units(a, now) > STDERR
  printf "\tProvisional Units => %.3f\n", get_qualified_units(a, now + half_window) > STDERR
  printf "\tSell              => %.3f\n", u > STDERR
@endif

  # Get the latest key not beyond the window
  key = find_key(Qualified_Units[a], now + half_window)

  # While keys exist that are in the future
  # adjust them on a last-in-first-out basis
  du = u
  while (greater_than(key, now)) {
    # We will need the next key
    next_key = find_key(Qualified_Units[a], just_before(key))

@ifeq LOG qualified_units
    printf "\tKey               => %s\n", get_date(key) > STDERR
    printf "\tUnits             => %.3f\n", get_qualified_units(a, key) > STDERR
    printf "\tNext Key          => %s\n", get_date(next_key) > STDERR
    printf "\tUnits             => %.3f\n", get_qualified_units(a, next_key) > STDERR
    printf "\tParcel            => %.3f\n", get_qualified_units(a, key) - get_qualified_units(a, next_key) > STDERR
@endif

    # How many provisionally qualified units are at the key entry?
    dq = get_qualified_units(a, key) - get_qualified_units(a, next_key)

    # Assert all parcels must be positive
    assert(above_zero(dq), sprintf("Found a non-positive provisional parcel of units %s[%s] => %.3f",
            get_short_name(a), get_date(key), dq))

    # Reduce this parcel first
    if (du < dq) {
      # Reduce this parcel
      sum_entry(Qualified_Units[a], -du, key)
@ifeq LOG qualified_units
      printf "\tReduced Units     => %.3f\n",  -du > STDERR
@endif

      du = 0
    } else {
@ifeq LOG qualified_units
      printf "\tReduced Units     => %.3f\n",  dq > STDERR
@endif

      # This parcel is removed fully
      # Safe to skip sum_entry because this is a last-in-first-out stack
      # so an empty entry must be the last entry
      delete Qualified_Units[a][key]
      du -= dq
    }

    # Any units left to apply?
    if (near_zero(du))
      break

    # Get the next key
    key = next_key
  } # End of while each key in the window

  # Do not adjust fully qualified units...
  # Instead add a negative parcel?
  # Do not adjust qualified parcels since these are needed for historical comparisons?
  if (above_zero(du)) {
    sum_entry(Qualified_Units[a], - du, now) # Ok to make a non-provisional negative parcel
@ifeq LOG qualified_units
    printf "\tAdjust Units      => %.3f\n", -du > STDERR
@endif
  }

@ifeq LOG qualified_units
  printf "\tFinished Selling [%s]\n", Leaf[a] > STDERR
  printf "\tWindow End        => %s\n", get_date(now + half_window) > STDERR
  printf "\tQualified Units   => %.3f\n", get_qualified_units(a, now) > STDERR
  printf "\tProvisional Units => %.3f\n", get_qualified_units(a, now + half_window) > STDERR
@endif
}

# A special function to return the held time
function get_held_time(now, from,     held_time) {
  held_time = now - from

  # Fix the held time to allow for the case of leap years
  if (CGT_PERIOD == held_time && CGT_PERIOD == one_year(now)) # A leap year
     held_time -= ONE_DAY # Fake the held time to get the correct tax treatment
  return held_time
}

# This is the hard way to do it
# another way is return (Cost_Basis_change - market_value_change)
# this way is being used ATM for consistency checking reasons
function get_unrealized_gains(a, now,
                              current_price, p, gains, x) {
  if (is_closed(a, now))
    return 0 # NO unrealized gains

  # Sum the total value of the asset
  gains = 0
  current_price = find_entry(Price[a], now)

@ifeq LOG get_unrealized_gains
  printf "Get Unrealized Gains (%s)\n", get_short_name(a) > STDERR
  printf "\tDate => %s\n",  get_date(now) > STDERR
  printf "\tPrice => %s\n", print_cash(current_price) > STDERR
  printf "\tAdjustments => %s\n", print_cash(gains) > STDERR
@endif # LOG

  # Unrealized gains held at time t are those in unsold parcels
  for (p = 0; p < Number_Parcels[a]; p++) {
    if (greater_than(Held_From[a][p], now)) # All further transactions occured after (now)
      break # All done
    if (is_unsold(a, p, now)) # This is an unsold parcel at time (now)
      # If value > cash_in this is an unrealized gain
      gains += sum_cost_elements(Accounting_Cost[a][p], now) - Units_Held[a][p] * current_price
  }

@ifeq LOG get_unrealized_gains
  printf "\tGains   => %s\n", print_cash(gains) > STDERR
  printf "\tParcels => %03d\n", p > STDERR
  printf "\tUnits   => %05d\n", get_units(a, now) > STDERR
@endif # LOG

  # The result
  return gains
}

# Buy a parcel of u units at the cost of x
function buy_units(now, a, u, x, parcel_tag, parcel_timestamp,
                                             last_parcel, p) {
@ifeq LOG buy_units
  printf "%s: %s units => %.3f amount => %11.2f\n", "buy_units", get_short_name(a), u, x > STDERR
  printf "\tU => %.3f Cost => %.2f\n", get_units(a, now), get_cost(a, now) > STDERR
  printf "\tTime => %s\n", get_date(now, LONG_FORMAT) > STDERR
  if (parcel_tag)
    printf "\tParcel Name => %s\n", parcel_tag > STDERR
  if (parcel_timestamp >= Epoch)
    printf "\tSet purchase date   => %s\n", get_date(parcel_timestamp) > STDERR
@endif # LOG

  # Some units are bought
  assert(!near_zero(u), sprintf("buy_units[%s] : can't buy zero units", $0))

  # Override timestamp
  if (parcel_timestamp >= Epoch)
    now = parcel_timestamp

  # Make a new parcel
  last_parcel = new_parcel(a, u, x, now, parcel_tag)

  # Update units
  adjust_units(a, u, now)

  # Also update qualified units - buying is easy
  if (is_capital(a) && Qualification_Window) {
    sum_entry(Qualified_Units[a], u, now + 0.5 * Qualification_Window)
@ifeq LOG qualified_units
    printf "Buy Units %s\n\tDate               => %s\n", Leaf[a], get_date(now) > STDERR
    printf "\tQualification Date => %s\n", get_date(now + 0.5 * Qualification_Window) > STDERR
    printf "\tBuy                => %.3f units\n", u > STDERR
    printf "\tQualified Units    => %.3f\n", get_qualified_units(a, now) > STDERR
    printf "\tProvisional Units  => %.3f\n", get_qualified_units(a, now + 0.5 * Qualification_Window) > STDERR
@endif
  }

  # Debugging
@ifeq LOG buy_units
  printf "\t%s\n\t\tUnits => %.3f Cost => %.2f\n", get_short_name(a), get_units(a, now), get_cost(a, now) > STDERR
  printf "\t\tParcel => %05d\n", last_parcel > STDERR
@endif # LOG

  # Buy u units for x
  u = Units_Held[a][last_parcel]
  x = sum_cost_elements(Accounting_Cost[a][last_parcel], now) # This must be just the cash paid

  # Passive revaluation
  p = x / u
@ifeq LOG buy_units
  printf "\tPrice       => %11.2f\n", p > STDERR
  printf "\tParcel      => %05d\n", last_parcel > STDERR
  printf "\tCost        => %11.2f\n", x > STDERR
  if (keys_in(Parcel_Tag, a, last_parcel))
    printf "\tParcel Name => %s\n", Parcel_Tag[a][last_parcel] > STDERR
@endif # LOG

  # Set the new price
  set_entry(Price[a], p, now)

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
@ifeq LOG sell_units
  printf "%s: %s units => %.3f amount => %11.2f\n", "sell_units", get_short_name(ac), u, x > STDERR
  if ("" != parcel_tag)
    printf "\tSpecified parcel   => %s\n", parcel_tag > STDERR
  if (parcel_timestamp >= Epoch)
    printf "\tParcel bought at   => %s\n", get_date(parcel_timestamp) > STDERR
  printf "\tInitial Units      => %.3f\n", get_units(ac, now) > STDERR
  printf "\tInitial Total Cost => %s\n", print_cash(get_cost(ac, now)) > STDERR
@endif # LOG

  # Try adjusting units now...
  adjust_units(ac, -u, now)

  # For a depreciating asset with multiple parcels (apart from a pooled asset) then
  # the proportion of the asset being sold depends on the
  # original cost of each unit; not on the number of units
  proportional_cost = FALSE
  if (is_fixed(ac)) {
    # Is this asset's depreciation upto date?
    # Depreciate
@ifeq LOG sell_units
    printf "\tDate => %s\n",  get_date(now, LONG_FORMAT)> STDERR
    printf "\tEOFY => %s\n",  get_date(FY_Time, LONG_FORMAT)> STDERR
@endif
    if (now > FY_Time) {
      t = just_before(FY_Time)
      catch_up_depreciation = depreciate_now(ac, t)
      if (!near_zero(catch_up_depreciation)) {
        update_cost(ac, - catch_up_depreciation, t)

        # Balance accounts
        adjust_cost(DEPRECIATION, catch_up_depreciation, t)

        # Print the transaction
        print_transaction(t, "Catch-Up Depreciation", ac, DEPRECIATION, "(D)", catch_up_depreciation)
@ifeq LOG sell_units
        printf "\tCatch-Up Depreciation => %s\n", print_cash(catch_up_depreciation) > STDERR
        printf "\tApplied At Time => %s\n", get_date(t, LONG_FORMAT) > STDERR
        printf "\tModified Total Cost => %s\n", print_cash(get_cost(ac, now)) > STDERR
@endif
      }
    }

    # More complications - a fully depreciated asset
    # or a Pooled asset
    if ("POOL" == Method_Name[ac]) {
      # For a pooled asset we must see a unique parcel
      # Since only one parcel proportional_cost is FALSE
      assert(1 == Number_Parcels[ac] || "" != parcel_tag || DATE_ERROR != parcel_timestamp,
             "Must sell a specified parcel for pooled asset <" ac ">")

      # Need to set new price  - based on units just like non-depreciating assets
      new_price = x / u
    } else {
      # Not pooled
      # Care is needed when the cost is zero
      p = get_cost(ac, now)
      if (!near_zero(p))
        proportional_cost = x / p
      else
        new_price = 0
    }
  } else {
    # Not depreciating
    # Also update qualified units - selling is harder
    if (Qualification_Window && above_zero(u))
      sell_qualified_units(ac, u, now, 0.5 * Qualification_Window)

    # Sell u units for x
    # Both x & u are positive by design
    new_price = x / u

    # Can set the price of a normal asset
    set_entry(Price[ac], new_price, now)
  }

  # Default assumption is first-in-first-out (FIFO)
  # But can be overriden if specific parcel given
  p = 0
  while (u > 0 && p < Number_Parcels[ac]) {
    # Skip sold parcels - including those sold today
    if (is_sold(ac, p, now)) {
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

@ifeq LOG sell_units # // LOG
      # Identify which parcel matches
      printf "\tprice => %11.2f\n", new_price > STDERR

      printf "\tSell from parcel => %05d\n", p > STDERR
      if (parcel_tag)
        printf "\t\tParcel Tag       => %s\n", Parcel_Tag[ac][p] > STDERR
      if (parcel_timestamp > Epoch)
        printf "\t\tParcel TimeStamp => %s\n", get_date(Held_From[ac][p]) > STDERR
@endif

      # Sell the parcel
      did_split = sell_parcel(ac, p, du, du * new_price, now)

@ifeq LOG sell_units # // LOG
      if (did_split) {
        # Parcel was split
        if (0 == u) {
          # Was this the last sold parcel
          if (p + 1 < Number_Parcels[ac])
            # Was a parcel kept
            printf "\tkept parcel => %05d on  => %10.3f date => %s\n\t\tHeld => [%s, %s]\n\t\tadjustment => %s\n\t\tparcel cost => %s\n",
              p + 1, Units_Held[ac][p + 1], get_date(now),
              get_date(Held_From[ac][p + 1]), get_date(Held_Until[ac][p + 1]),
              print_cash(get_cost_modifications(ac, p + 1, now)),
              print_cash(get_parcel_cost(ac, p + 1, now)) > STDERR
        } # Was this the last parcel sold
      } # Was a parcel split
@endif # LOG
    } # End of match parcel

    # The next parcel
    p ++
  } # End of while statement

@ifeq LOG sell_units
  printf "\tFinal Units => %.3f\n", get_units(ac, now) > STDERR
  printf "\tFinal Total Cost     => %s\n", print_cash(get_cost(ac, now)) > STDERR
@endif # LOG

  # Were all the requested units actually sold?
  assert(near_zero(u), sprintf("sell_units: Failed to sell the requested %d units of %s", u, get_short_name(ac)))

  # Update parent sums
  update_cost(ac, -x, now)
}

function sell_parcel(a, p, du, amount_paid, now,      i, is_split) {
  # The sale date
  Held_Until[a][p] = now

  # No parcel split yet
  is_split = FALSE

  # Amount paid
@ifeq LOG sell_units
  printf "\tAmount Paid => %s\n", print_cash(amount_paid) > STDERR
@endif # LOG

  # Check for an empty parcel - allow for rounding error
  if (near_zero(Units_Held[a][p] - du))
    # Parcel is sold off
    Units_Held[a][p] = du
  else { # Units remain - parcel not completely sold off
@ifeq LOG sell_units
    printf "\tsplit parcel %3d on => %10.3f off => %10.3f\n\t\tadjustment => %s\n\t\tparcel cost => %s\n",
          p, Units_Held[a][p], du, print_cash(get_cost_modifications(a, p, now)), print_cash(get_parcel_cost(a, p, now)) > STDERR
@endif # LOG

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
  set_entry(Accounting_Cost[a][p][0], -amount_paid, now)

@ifeq LOG sell_units
  printf "\tsold parcel => %05d off => %10.3f date => %s\n\t\tHeld => [%s, %s]\n\t\tadjustment => %s\n\t\tparcel cost => %s\n\t\tparcel paid => %s\n",
    p, du,  get_date(now),
    get_date(Held_From[a][p]), get_date(Held_Until[a][p]),
    print_cash(get_cost_modifications(a, p, now)),
    print_cash(get_parcel_cost(a, p, now)),
    print_cash(get_cash_out(a, p, now)) > STDERR
@endif # LOG

  # Save realized gains
  if (!is_fixed(a))
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

@ifeq LOG sell_units
  if (above_zero(x)) # This was a DEPRECIATION expense
    printf "\tDepreciation => %s\n", print_cash(x) > STDERR
  else if (below_zero(x)) # This was an APPRECIATION income
    printf "\tAppreciation => %s\n", print_cash(-x) > STDERR
  else
    printf "\tZero Depreciation\n" > STDERR
@endif # LOG

  # Any excess income or expenses are recorded
  if (above_zero(x)) # This was a DEPRECIATION expense
    adjust_cost(SOLD_DEPRECIATION, x, now)
  else if (below_zero(x)) # This was APPRECIATION income
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
  if (above_zero(x)) {
    adjust_cost(REALIZED_LOSSES, x, now)

    # Taxable losses are based on the reduced cost
    if (held_time >= CGT_PERIOD)
      adjust_cost(LONG_LOSSES, x, now)
    else
      adjust_cost(SHORT_LOSSES, x, now)
  } else if (below_zero(x))
    adjust_cost(REALIZED_GAINS, x, now)

  # Taxable gains
  # after application of tax adjustments
  # This works if tax adjustments are negative
  x -= sum_cost_elements(Tax_Adjustments[a][p], now)

  # Taxable Gains are based on the adjusted cost
  if (below_zero(x)) {
    # Taxable losses are based on the reduced cost
    if (held_time >= CGT_PERIOD)
      adjust_cost(LONG_GAINS, x, now)
    else
      adjust_cost(SHORT_GAINS, x, now)
  }
}

# Copy and split parcels
function copy_parcel(ac, p, q,     e, key) {
@ifeq LOG sell_units
  printf "\t\t\tCopy parcel %3d => %3d\n", p, q > STDERR
@endif # LOG

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
  matched_date = (DATE_ERROR == parcel_timestamp || Held_From[a][p] == parcel_timestamp)

  # Determine if this parcel matches either the parcel tag or timestamp if either set
  # Does the parcel tag match?
  if ("" != parcel_tag)
    # There is a tag
    # This will either find a unique tag or a possibly non unique tag on the matched date
    return matched_date && (get_key(Parcel_Tag[a], p) == parcel_tag)

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

@ifeq LOG check_balance
  # Verbose balance printing
  show_balance = (now >= Start_Time)
@else
  # No default printing
  show_balance = FALSE
@endif #// LOG

  # Is there an error?
  if (!near_zero(balance)) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > STDERR
    show_balance = TRUE
  }

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > STDERR
    printf "\tAssets      => %20.2f\n", sum_assets > STDERR
    printf "\tIncome      => %20.2f\n", sum_income > STDERR
    printf "\tExpenses    => %20.2f\n", sum_expenses > STDERR
    printf "\tLiabilities => %20.2f\n", sum_liabilities > STDERR
    printf "\tEquities    => %20.2f\n", sum_equities > STDERR
    printf "\tAdjustments => %20.2f\n", sum_adjustments > STDERR
    printf "\tBalance     => %20.2f\n", balance > STDERR
    assert(near_zero(balance), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
