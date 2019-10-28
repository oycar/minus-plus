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
#   SPECIAL.OFFSET ordering varies between FRANKING and others... confusing
#   Need to break down carried tax losses by year
#   Need to enforce CARRY_FORWARD_LIMIT or carried capital losses
#   In a State file the distinction between CURRENT and TERM is lost completely when  the asset is redefined - this is a bug
#   Consider breaking out income/expenses in  the same way as the tax return does?
#   Share splits could be considered using a similar mechanism to currencies - with a weighting formula
#
#
#   Fix up wiki files
#   More flexible ordering of optional fields?
#   other tax_statement calculations (eg UK, US, NZ etc...)
#
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
  Start_Journal = FALSE

  # An array to hold real values
  make_array(Real_Value)
  make_array(account_sum)

  # An array to hold document strings
  make_array(Documents)

  # A Document shortcut code
  Document_Shortcut = "[:+]"

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
  if ("" == Epoch)
    set_epoch()

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
  make_array(Lifetime)
  make_array(Long_Gains)
  make_array(Long_Losses)
  make_array(Long_Name)
  make_array(Maturity_Date)
  make_array(Method_Name)
  make_array(Number_Parcels)
  make_array(Parcel_Proceeds)
  make_array(Parcel_Tag)
  make_array(Parent_Name)
  make_array(Price)
  make_array(Payment_Date)
  make_array(Qualified_Units)
  make_array(Short_Gains)
  make_array(Short_Losses)
  make_array(Tax_Adjustments)
  make_array(Tax_Bands)
  make_array(Tax_Credits)
  make_array(Threshold_Dates)
  make_array(Total_Units)
  make_array(Underlying_Asset)
  make_array(Units_Held)

  # Provisional Carried Loss Arrays
  #make_array(Remaining_Losses)
  Remaining_Losses[0][SUBSEP] = 0; delete Remaining_Losses[0][SUBSEP]

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
      assert(DATE_ERROR < Stop_Time, "Stop_Time " Read_Date_Error)
    }
  }

  # EOFY statements are not printed until requested
  EOFY = STDERR

  # Which account to track
  if ("" == Show_Account)
    Show_Account = FALSE

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

/START_JOURNAL/ {
  # Allow multiple calls
  if (Start_Journal)
    next

  # Ensure agreement of Last_Record with Last_State
  if (Last_Record < Last_State)
    Last_Record = Last_State

  # Flag this
  Start_Journal = TRUE

  # Is the currency consistent
  assert(JOURNAL_CURRENCY == Journal_Currency, "Incompatible journal currency <" Journal_Currency "> in journal file - expected <" JOURNAL_CURRENCY "> instead")

  # Set default functions
  Income_Tax_Function     = "income_tax_" tolower(Journal_Currency)
  Initialize_Tax_Function = "initialize_tax_" tolower(Journal_Currency)
  Dividend_Qualification_Function = "dividend_qualification_" tolower(Journal_Currency)
  Imputation_Report_Function      = "imputation_report_" tolower(Journal_Currency)
  Gross_Up_Gains_Function   = "gross_up_gains_" tolower(Journal_Currency)
  Get_Taxable_Gains_Function   = "get_taxable_gains_" tolower(Journal_Currency)

  # These functions are not dependent on currency
  Balance_Profits_Function  = "balance_journal"
  Check_Balance_Function  = "check_balance"
  Update_Profits_Function = "update_profits"
  Update_Member_Function  = "update_member_liability"

  # Dividend Qualification Window - is recorded
  # with the help of the auxilliary variable
  # which sets an EOFY_Window
  if ("" == EOFY_Window) {
    # Dividend Qualification Window is not yet set
    if ("" == Qualification_Window)
      Qualification_Window = QUALIFICATION_WINDOW # Units in days

    # Qualification_Window is set, but since EOFY_Window is unset the units are days
    Qualification_Window *= ONE_DAY

    # Still need to set EOFY_Window
    if (Qualification_Window > ONE_DAY)
      EOFY_Window = 0.5 * (Qualification_Window - ONE_DAY)
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
  Last_FY = last_year(FY_Time)

  # All done
  next
}

function set_financial_year(now,   new_fy) {
  # Which Calendar year is this?
  FY_Year = get_year_number(now)

  # The timestamp at the end of the year
  # This assumes FY_Date is the date of the
  # first day of a financial year
  new_fy = read_date(FY_Year "-" FY_Date, 0)
  assert(new_fy > just_before(FY_Time), "Cannot regress financial year: Current FY => " get_date(FY_Time) " New FY => " get_date(new_fy))
  FY_Time = new_fy
  Last_FY = last_year(FY_Time)

  # Get the day number for the FY_Date
  FY_Day = get_day_number(FY_Time)

  # Feb 28 has day number 59 so if FY_Day <= 60 - (1st day next FY)
  # then the current FY would include leap day if (FY - 1) was a leap year
  FY_Length = get_year_length(FY_Year, FY_Day)
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
  #FRANKING_DEFICIT   = initialize_account("SPECIAL.FRANKING.OFFSET:FRANKING.DEFICIT")
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
  INCOME_LONG        = initialize_account("INCOME.GAINS.LONG.SUM:INCOME.LONG")
  INCOME_SHORT       = initialize_account("INCOME.GAINS.SHORT:INCOME.SHORT")
  EXPENSE_LONG       = initialize_account("EXPENSE.LOSSES.LONG:EXPENSE.LONG")
  EXPENSE_SHORT      = initialize_account("EXPENSE.LOSSES.SHORT:EXPENSE.SHORT")

  # Taxable capital gains are in special accounts
  # Make sure the parent accounts exist
  initialize_account(LONG_GAINS  ":LONG.GAINS")
  initialize_account(LONG_LOSSES ":LONG.LOSSES")
  initialize_account(SHORT_GAINS ":SHORT.GAINS")
  WRITTEN_BACK   =   initialize_account(SHORT_LOSSES ":SHORT.LOSSES")

  # Taxable carried losses
  TAX_LOSSES       = initialize_account("SPECIAL.LOSSES:TAX.LOSSES")

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
  now = ternary(-1 != Last_Record, Last_Record, Epoch)

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

    ## Several dates can be set
    case "SET_FINANCIAL_YEAR" :
      # This would set the first day of the next FY so go back one day
      now = yesterday(read_date($2 "-" FY_DATE))
      assert(now > DATE_ERROR, Read_Date_Error)
      if (now > Last_State)
        set_financial_year(now)
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
  # Must have started journal
  assert(Start_Journal, "No START_JOURNAL record found")

  # Optional values
  new_line()

  # Interpret the date
  t = read_date($1)

  # t should be positive
  assert(t > DATE_ERROR, Read_Date_Error)

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
      threshold = find_key(Threshold_Dates, just_before(threshold))
    }

    # Update the Last_Record
    Last_Record = t

    while (FY_Time + EOFY_Window < t) {
      # Get each EOFY in turn
      eofy_actions(FY_Time)

      # The next financial year - unless we are stopping
      FY_Year ++

      # Update FY length
      FY_Length = get_year_length(FY_Year, FY_Day)
      Last_FY = FY_Time
      FY_Time = next_year(FY_Time)
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
  if (t < just_after(Last_State)) {
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
  make_array(fields)

  # Special franking provisions
  if (a == TAX) {
    # Reduce franking
    adjust_cost(FRANKING, - amount, now)

    # Note this as a reduction in the balance
    adjust_cost(FRANKING_STAMPED, amount, now)

    print_transaction(now, "Reduce Franking Balance", FRANKING, FRANKING_STAMPED, 0, amount)
   } else if (b != GST && is_tax(b)) {
    # Increase franking
    adjust_cost(FRANKING, amount, now)

    # Note this as an increase in the balance
    adjust_cost(FRANKING_STAMPED, -amount, now)

    print_transaction(now, "Increase Franking Balance", FRANKING_STAMPED, FRANKING, 0, amount)
  }

  # A SMSF member benefit
  if (is_class(b, "EXPENSE.NON-DEDUCTIBLE.BENEFIT")) {
    # But there is another complication - this needs to consider
    # unrealized gains too => so important assets are priced accurately
    #
    set_cost(MARKET_CHANGES, get_asset_gains("get_unrealized_gains", just_before(now)), just_before(now))

    # This will change proportions so update the profits first
    @Update_Profits_Function(now)

    # Expense must be account b
    amount_taxed = amount * @Update_Member_Function(now, -amount, b)
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

    # Foreign or franking credits
    if (!near_zero(tax_credits)) {
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
        if (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION"))
          credit_account = Tax_Credits[a] = initialize_account("SPECIAL.FRANKING.OFFSET:I_TAX." Leaf[underlying_asset])
        else if (is_class(a, "INCOME.FOREIGN"))
          credit_account = Tax_Credits[a] = initialize_account("SPECIAL.OFFSET.FOREIGN:C_TAX." Leaf[underlying_asset])
        else
          assert(FALSE, sprintf("Can't link a tax credit account to income account %s", a))
      }

      # Adjust credit account
      adjust_cost(credit_account, - tax_credits, now)

      # Adjust franking account when necessary
      if (is_class(a, "INCOME.FOREIGN")) {
        # Foreign Credits
        adjust_cost(NULL, tax_credits, now)
        print_transaction(now, ("# " Leaf[underlying_asset] " Foreign Credits"), credit_account, NULL, 0, tax_credits)
      } else {
        # Frannking Credits
        adjust_cost(FRANKING, tax_credits, now)
        print_transaction(now, ("# " Leaf[underlying_asset] " Franking Credits"), credit_account, FRANKING, 0, tax_credits)
      }
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

      } else if (Qualification_Window && (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION.CLOSE"))) {
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
      @Update_Profits_Function(now)

      # Drop the INCOME prefix
      @Update_Member_Function(now, amount, a)
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
    # The asset being sold must be "a" but if equity must be "b"
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

@ifeq EXPORT_FORMAT 1
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
@ifeq EXPORT_FORMAT 1
    # Format is
    # Needs fixed number of fields - we can ignore parcel name etc..
    # A, B, U, x + b, b, b*g, <parcel-name>, BUY, Comments
    # Record the GST on the brokerage
    fields[3] = Parcel_Name
    fields[1] = sprintf("%.*f", PRECISION, current_brokerage) # Always use 2nd field
    fields[2] = sprintf("%.*f", PRECISION, g)
    fields[4] = "BUY"

    print_transaction(now, Comments, a, b, Write_Units, amount - g, fields, 4)
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
      # Instalment purchase
      adjust_cost(b,  amount, Extra_Timestamp)
      adjust_cost(a, -amount, now)

      # This will not balance when re-read from the state file unless balancing entries made
      adjust_cost(FUTURE_PAYMENT, -amount, Extra_Timestamp)
      adjust_cost(FUTURE_PAYMENT,  amount, now)

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
@ifeq LOG convert_term_account
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
@ifeq LOG convert_term_account
    printf "\tThreshold_Dates => \n" > STDERR
    walk_array(Threshold_Dates, 1, STDERR)
    printf "\n" > STDERR
@endif
  } else if (is_term(a)) {
    # Need to identify this as a current account
    if (a in Maturity_Date)
      delete Maturity_Date[a]
@ifeq LOG convert_term_account
    printf "\tRelabelled account => %s\n", a > STDERR
    printf "\tCurrent Account [%s] => %d\n", a, is_current(a) > STDERR
@endif
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
  action = get_short_name(a)

  # First lets check
  if (is_check) {
    # Check the action just after now
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
    assert(near_zero(amount - quantity), sprintf("%s fails checkpoint %s [%s] => %.4f != %.4f\n",
                                                 get_short_name(account), action, get_date(now, LONG_FORMAT), quantity, amount))

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
        # Override the account cost -> this can cause the accounts not to balance!
        set_cost(account, amount, now)
@ifeq LOG checkset
        printf "%% SET %s[%11s] => %14s\n", account, get_date(now), print_cash(get_cost(account, now)) > STDERR
        printf "%% SET %s[%11s] => %14s\n", Parent_Name[account], get_date(now), print_cash(get_cost(Parent_Name[account], now)) > STDERR
@endif
      break

      default : assert(FALSE, sprintf("SET: I don't know how to set <%s> for account %s\n",
                                      action, get_short_name(account)))
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
  assert(MPX_Version == Current_Version, \
    "Inconsistent snapshot file:\n\tExpected Version => " Current_Version " Found Version => " MPX_Version)

  # Delete empty accounts
  # Filter out data entries that were added by import CSV records
  # that do not overlap with the the holding period
  filter_data(Last_Record, Filter_Data, FALSE)

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
      FY_Length = get_year_length(FY_Year, FY_Day)
      Last_FY = FY_Time
      FY_Time = next_year(FY_Time)
    } while (FY_Time + EOFY_Window < Last_Record)

    # Fix FY_Time so that the snapshot is accurate
    FY_Year --
    FY_Length = get_year_length(FY_Year, FY_Day)

    FY_Time = last_year(FY_Time)
    Last_FY = last_year(FY_Time)
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

  # Log data about selected variables
  if (Write_Variables)
    filter_data(Last_Record, Write_Variables, TRUE)

  # Check
} #// END
#

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
  x = get_element_cost(a, last_parcel, I, now) # Element I is all important

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

#
# sell part or all a parcel at time now
# account  a
# parcel   p
# sell     du units
# proceeds amount_paid
# time     now

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
  set_parcel_proceeds(a, p, -amount_paid)

@ifeq LOG sell_units
  printf "\tsold parcel => %05d off => %10.3f date => %s\n\t\tHeld => [%s, %s]\n\t\tadjustment => %s\n\t\tparcel cost => %s\n\t\tparcel paid => %s\n",
    p, du,  get_date(now),
    get_date(Held_From[a][p]), get_date(Held_Until[a][p]),
    print_cash(get_cost_modifications(a, p, now)),
    print_cash(get_parcel_cost(a, p, now)),
    print_cash(get_parcel_proceeds(a, p)) > STDERR
@endif # LOG

  # Save realized gains
  if (!is_fixed(a))
    save_parcel_gain(a, p, now, - amount_paid)
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
  # In fact the cost of sold assets is 0 anyway
  set_parcel_proceeds(a, p, -x)

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
# The gain was made at time "now" on at asset purchased at Held_From[a][p]
# Normally this saves taxable gains/losses in LONG_GAINS etc
# Very occasionally uses the INCOME_LONG
# So to avoid inefficiency uses a global name for these
# accounts which can be flipped if necessary
function save_parcel_gain(a, p, now, x,       held_time) {
  # Get the held time
  held_time = get_held_time(now, Held_From[a][p])

  # Accounting gains or Losses - based on reduced cost
  # Also taxable losses are based on the reduced cost...
  x += sum_cost_elements(Accounting_Cost[a][p], now) # Needs all elements
  if (above_zero(x)) {
    adjust_cost(REALIZED_LOSSES, x, now)

    # Taxable losses are based on the reduced cost
    if (held_time >= CGT_PERIOD) {
      if (!(a in Long_Losses))
        Long_Losses[a] = initialize_account(LONG_LOSSES ":LL." Leaf[a])
      adjust_cost(Long_Losses[a], x, now)
    } else {
      if (!(a in Short_Losses))
        Short_Losses[a] = initialize_account(SHORT_LOSSES ":SL." Leaf[a])
      adjust_cost(Short_Losses[a], x, now)
    }
  } else if (below_zero(x))
    adjust_cost(REALIZED_GAINS, x, now)

  # Taxable gains
  # after application of tax adjustments
  # This works if tax adjustments are negative
  x -= find_entry(Tax_Adjustments[a][p], now)

  # Taxable Gains are based on the adjusted cost
  if (below_zero(x)) {
    # Taxable losses are based on the reduced cost
    if (held_time >= CGT_PERIOD) {
      if (!(a in Long_Gains))
        Long_Gains[a] = initialize_account(LONG_GAINS ":LG." Leaf[a])
      adjust_cost(Long_Gains[a], x, now)
    } else {
      if (!(a in Short_Gains))
        Short_Gains[a] = initialize_account(SHORT_GAINS ":SG." Leaf[a])
      adjust_cost(Short_Gains[a], x, now)
    }
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
  Parcel_Proceeds[ac][q] = Parcel_Proceeds[ac][p]

  # Copy all entries
  # Note keys will not match so need to delete old entries from parcel q
  delete Accounting_Cost[ac][q] # Delete old entries
  delete Tax_Adjustments[ac][q]
  for (e in Accounting_Cost[ac][p])
    for (key in Accounting_Cost[ac][p][e])
        Accounting_Cost[ac][q][e][key] = Accounting_Cost[ac][p][e][key]
  for (key in Tax_Adjustments[ac][p])
    Tax_Adjustments[ac][q][key]  = Tax_Adjustments[ac][p][key]
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
  show_balance = TRUE
@else
  # No default printing
  show_balance = FALSE
@endif #// LOG

  # Is there an error?
  if (!near_zero(balance)) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > STDERR
    show_balance = TRUE
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > STDERR
    printf "\tAssets      => %20.2f\n", sum_assets > STDERR
    printf "\tIncome      => %20.2f\n", sum_income > STDERR
    printf "\tExpenses    => %20.2f\n", sum_expenses > STDERR
    printf "\tLiabilities => %20.2f\n", sum_liabilities > STDERR
    printf "\tEquities    => %20.2f\n", sum_equities > STDERR
    if (not_zero(sum_adjustments))
      printf "\tAdjustments => %20.2f\n", sum_adjustments > STDERR
    printf "\tBalance     => %20.2f\n", balance > STDERR
    assert(near_zero(balance), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
