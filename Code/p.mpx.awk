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
  make_array(Account_Closed)
  make_array(Account_Currency)
  make_array(Account_Term)
  make_array(Accounting_Cost)
  make_array(Carry_Offsets)
  make_array(Cost_Basis)
  make_array(Dividend_Date)
  make_array(Foreign_Offset_Limit)
  make_array(Held_From)
  make_array(Held_Until)
  make_array(Income_Tax)
  make_array(Leaf)
  make_array(Lifetime)
  make_array(Long_Gains)
  make_array(Long_Losses)
  make_array(Long_Name)
  make_array(Maturity_Date)
  make_array(Method_Name)
  make_array(No_Carry_Offsets)
  make_array(Number_Parcels)
  make_array(Parcel_Proceeds)
  make_array(Parcel_Tag)
  make_array(Parent_Name)
  make_array(Price)
  make_array(Qualified_Units)
  make_array(Refundable_Offsets)
  make_array(Short_Gains)
  make_array(Short_Losses)
  make_array(Tax_Adjustments)
  make_array(Tax_Bands)
  make_array(Tax_Credits)
  make_array(Taxable_Income)
  make_array(Total_Units)
  make_array(Underlying_Asset)
  make_array(Units_Held)

  # Carried Loss Arrays
  # Capital Losses
  Capital_Losses[0][SUBSEP] = 0; delete Capital_Losses[0][SUBSEP]
  Tax_Losses[0][SUBSEP] = 0; delete Tax_Losses[0][SUBSEP]

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
  Key_is_Date     = KEY_IS_DATE
  Value_is_Date   = VALUE_IS_DATE
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
  if (!Show_Extra)
    Show_Extra = 0

  # stop time
  if (!Stop_Time)
    Stop_Time = Future
  else {
    Stop_Time = read_date(Stop_Time)
    assert(DATE_ERROR < Stop_Time, "Stop_Time " Read_Date_Error)
  }

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

# State Record
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


# Control Record
# Standard control function syntax is
# FUNCTION, [ARGUMENT_1, ARGUMENT_2]
# Control functions are:
#  SET_ENTRY
#  ADJUST_ENTRY

#  SET_BANDS
#  SET
#  CHECK
#  SPLIT
#  MERGE
#  CHANGE
#
/^!!/  {
  #
  ##Control_Record = !Control_Record

## $1 ~  /^([[:space:]])*(ADJUST|CHECK|SET|SHOW|SPLIT|MERGE|CHANGE)/  {
 # Use a function so we can control scope of variables
 read_control_record()
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


# Start Record
# Syntax is START_JOURNAL, Date
/START_JOURNAL/ {

  # Allow multiple calls
  if (Start_Journal)
    next
  else if (NF > 1)
    # Interpret the date at midnight
    Start_Record = read_date($2, 0)
  else
    assert(FALSE, "START_JOURNAL, Date: Date is required on first call of start journal")

  # Check Start_Record
  assert(Start_Record > DATE_ERROR, Read_Date_Error)

  # Set FY information if required
  if (-1 == Last_State)
    set_financial_year(Start_Record)

  # Ensure agreement of Last_Record with Last_State
  if (Last_Record < Last_State)
    Last_Record = Last_State

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
  Start_Journal = TRUE

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

##
## Imports CSV format
## <<,Account_Code, XYZ.ABC,>>
## <<,Key_Field, X,>>
## <<,Value_Field, Y,>>
## <<,Key_is_Date, 1,>>
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
  if (Key_is_Date) {
    key = read_date(trim($Key_Field)) # Default Hour overruled sometimes
    assert(DATE_ERROR != key, Read_Date_Error)

    # Skip dates before the epoch
    if (BEFORE_EPOCH == key)
      return
  } else
    key = trim($Key_Field)

  # Get the value
  if (Value_is_Date) {
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
  printf "%s %16s[%11s] => %14s\n", name, Leaf[a], ternary(Key_is_Date, get_date(key), key), ternary(Value_is_Date, get_date(value), print_cash(value, 4)) > STDERR
@endif

  # Set the price
  set_entry(array[a], value, key)

  # Done
}







function set_financial_year(now,   new_fy) {
  # Which Calendar year is this?
  FY_Year = get_year_number(now)

  # The financial year
  Last_FY = now
  assert(now > just_before(FY_Time), "Cannot regress financial year: Current FY => " get_date(FY_Time) " New FY => " get_date(now))
  FY_Time = next_year(now)

  # Get the FY_Date - just the date, no year
  FY_Date = get_date(now, "%b-%d")

  # Get the day number for the FY_Date
  FY_Day = get_day_number(FY_Time)

  # Feb 28 has day number 59 so if FY_Day <= 60 - (1st day next FY)
  # then the current FY would include leap day if (FY - 1) was a leap year
  FY_Length = get_year_length(FY_Year, FY_Day)
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


#
function read_control_record(       now, i, x, p, is_check){
  # Clear initial values
  new_line()

  # Use the last recorded date
  now = ternary(-1 != Last_Record, Last_Record, Epoch)

  # The control records - must be exact match
  is_check = 0
  x = trim($2)
  switch (x) {
    # case "CHECK" :
    #   is_check = 1
    # case "SHOW" :
    #   if (!is_check)
    #     is_check = -1
    # case "SET" :
    # case "ADJUST" :
    #   # Syntax for check and set is
    #   # CHECKSET, ACCOUNT, WHAT, X, # Comment
    #   # We need to rebuild the line into something more standard
    #   # DATE, ACCOUNT, WHAT, X, # Comment
    #   #
    #   # Some special accounts are needed
    #
    #   # Put DATE in 1st Field
    #   $1 = get_date(now)
    #
    #   # Check amount is set
    #   if (NF < 4) {
    #     $4 = 0
    #     NF = 4
    #   }
    #
    #   # We can parse this line now
    #   i = parse_line(now)
    #
    #   # Now either check or set the quantity
    #   assert(2 == i, $0 " : Unknown syntax for CHECK/SET actions")
    #   checkset(now, Account[1], Account[2], Real_Value[UNITS_KEY], amount, is_check)
    #   break
    #
    # case "CHANGE"  :
    # case "MERGE" :
    # case "SPLIT" :
    #   # SPLIT, ACCOUNT:SOURCE, ACCOUNT:TARGET, FACTOR, # Comment
    #   # Put DATE in 1st Field
    #   $1 = get_date(now)
    #
    #   # Copy?
    #   if ("CHANGE" == x)
    #     $4 = 1
    #
    #   # Parse the line
    #   i = parse_line(now)
    #
    #   # Merge?
    #   if ("MERGE" == x)
    #     # The reciprocal of split
    #     amount = ternary(amount, 1.0 / amount, 1)
    #
    #   # Split Account[1] => Account[2] by factor amount - zero means copy
    #   split_account(now, Account[1], Account[2], amount)
    #   break

    case "SET_BANDS" :
      # Set banded thresholds
      i = trim($2)
      assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
      set_array_bands(now, SYMTAB[i], NF)
      break

    case "ADJUST_ENTRY" :
      # Set a time-dependent array entry
      i = trim($2)
      assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
      p = strtonum($3)
      sum_entry(SYMTAB[i], p, now)
      break

    default: # This should not happen
      assert(FALSE, "Unknown Control Record <" x ">")
      break
  }

  # Get the next record
  return
}

# #
# function read_control_record(       now, i, x, p, is_check){
#   # Clear initial values
#   new_line()
#
#   # Use the last recorded date
#   now = ternary(-1 != Last_Record, Last_Record, Epoch)
#
#   # The control records - must be exact match
#   is_check = 0
#   x = trim($2)
#   switch (x) {
#     case "CHECK" :
#       is_check = 1
#     case "SHOW" :
#       if (!is_check)
#         is_check = -1
#     case "SET" :
#     case "ADJUST" :
#       # Syntax for check and set is
#       # CHECKSET, ACCOUNT, WHAT, X, # Comment
#       # We need to rebuild the line into something more standard
#       # DATE, ACCOUNT, WHAT, X, # Comment
#       #
#       # Some special accounts are needed
#
#       # Put DATE in 1st Field
#       $1 = get_date(now)
#
#       # Check amount is set
#       if (NF < 4) {
#         $4 = 0
#         NF = 4
#       }
#
#       # We can parse this line now
#       i = parse_line(now)
#
#       # Now either check or set the quantity
#       assert(2 == i, $0 " : Unknown syntax for CHECK/SET actions")
#       checkset(now, Account[1], Account[2], Real_Value[UNITS_KEY], amount, is_check)
#       break
#
#     case "CHANGE"  :
#     case "MERGE" :
#     case "SPLIT" :
#       # SPLIT, ACCOUNT:SOURCE, ACCOUNT:TARGET, FACTOR, # Comment
#       # Put DATE in 1st Field
#       $1 = get_date(now)
#
#       # Copy?
#       if ("CHANGE" == x)
#         $4 = 1
#
#       # Parse the line
#       i = parse_line(now)
#
#       # Merge?
#       if ("MERGE" == x)
#         # The reciprocal of split
#         amount = ternary(amount, 1.0 / amount, 1)
#
#       # Split Account[1] => Account[2] by factor amount - zero means copy
#       split_account(now, Account[1], Account[2], amount)
#       break
#
#     case "SET_BANDS" :
#       # Set banded thresholds
#       i = trim($2)
#       assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
#       set_array_bands(now, SYMTAB[i], NF)
#       break
#
#     case "ADJUST_ENTRY" :
#       # Set a time-dependent array entry
#       i = trim($2)
#       assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
#       p = strtonum($3)
#       sum_entry(SYMTAB[i], p, now)
#       break
#
#     default: # This should not happen
#       assert(FALSE, "Unknown Control Record <" x ">")
#       break
#   }
#
#   # Get the next record
#   return
# }

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

    # Update the Last_Record
    Last_Record = t

    # EOFY actions
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
  if (t < just_after(Last_State)) {
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
    $i = trim($i)

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
      $5 = ternary($5, 1.0 / $5, 1)
      break
    case "SPLIT"  :
      $5 = ternary($5, $5, 1)
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
      return FALSE
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
    checkset(now, Account[1], Account[2], Real_Value[UNITS_KEY], amount, is_check)

  # Get the next record
  return TRUE
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
                           fields, number_fields) {

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

    print_transaction(now, "Reduce Franking Balance", FRANKING, FRANKING_STAMPED, amount)
   } else if (b != GST && is_tax(b)) {
    # Increase franking
    adjust_cost(FRANKING, amount, now)

    # Note this as an increase in the balance
    adjust_cost(FRANKING_STAMPED, -amount, now)

    print_transaction(now, "Increase Franking Balance", FRANKING_STAMPED, FRANKING, amount)
  }

  # For a SMSF process member benefits
  if (is_smsf) {
    ordered_pair(account_array, a, b)
    amount = @Process_Member_Benefits(now, account_array, amount)
    a = account_array[1]; b = account_array[2]
    delete account_array
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
    set_account_term(a, now)
  } else if (is_term(b))
    set_account_term(b, now)

  # Initially no optional fields
  number_fields = 0

  # Is this a franked transaction?
  if (is_class(a, "INCOME")) {
    # Income accounts are caught to check for franking credits
    # Income must always be account a
    tax_credits = Real_Value[TAX_CREDITS_KEY]
    Real_Value[TAX_CREDITS_KEY] = 0

    # Get the underlying asset (if there is one)
    if (a in Underlying_Asset)
      underlying_asset = Underlying_Asset[a]
    else
      underlying_asset = FALSE

    # Foreign or franking credits
    if (not_zero(tax_credits)) {
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
          credit_account = Tax_Credits[a] = initialize_account("SPECIAL.FOREIGN.OFFSET:C_TAX." Leaf[underlying_asset])
        else
          assert(FALSE, sprintf("Can't link a tax credit account to income account %s", a))
      }

      # Adjust credit account
      adjust_cost(credit_account, - tax_credits, now)

      # Adjust franking account when necessary
      if (is_class(a, "INCOME.FOREIGN"))
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
    if (not_zero(Real_Value[LIC_DEDUCTION_KEY])) {
      # Record LIC Deduction to the listing
      sum_entry(LIC_Deduction, - Real_Value[LIC_DEDUCTION_KEY], now)
      fields[++ number_fields] = Real_Value[LIC_DEDUCTION_KEY]
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
        set_entry(Dividend_Date[underlying_asset], now, Extra_Timestamp)

      } else if (Qualification_Window && (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION.CLOSE"))) {
        Extra_Timestamp = get_exdividend_date(underlying_asset, now)

        # A date error is an error
        assert(DATE_ERROR != Extra_Timestamp, "<" Leaf[a] "> Cannot find ex-dividend date for payment date <" get_date(now) ">. Use option -q to override.")

        if (!Extra_Timestamp)
          printf "Warning: No exdividend information for %s\n", Leaf[a] > STDERR
        else
          fields[++ number_fields] = get_date(Extra_Timestamp)
      }

      # Clear the timestamp
      Extra_Timestamp = DATE_ERROR
    }

    # Now check for GST
    if (not_zero(GST_Claimable)) {
      # This is GST collected
      # The transaction itself will be posted later a => b
      # Need to adjust amount transacted
      amount -= (g = GST_Claimable * gst_proportion(now) * amount)
      print_transaction(now, ("# GST " Leaf[b]), GST, b, g)

      # GST claimed
      GST_Claimable = 0
    }

    # A SMSF member contribution
    @Process_Member_Contributions(now, amount, Member_Liability, a)
  } else if (is_class(b, "EXPENSE.NON-DEDUCTIBLE.DIVIDEND")) {
    # A franking entity (eg company) can distribute franking credits
    tax_credits = Real_Value[TAX_CREDITS_KEY]
    Real_Value[TAX_CREDITS_KEY] = 0

    # Simplified version of above
    if (not_zero(tax_credits)) {
      # The credits are adjusted in the FRANKING balance
      adjust_cost(FRANKING,    - tax_credits, now)
      adjust_cost(FRANKING_PAID,  tax_credits, now)

      print_transaction(now, ("# " Leaf[a] " Franking Credits Distributed"), FRANKING, FRANKING_PAID, tax_credits)
      fields[++ number_fields] = tax_credits

    } else
      tax_credits = 0
  }

  # Obtain units
  units = Real_Value[UNITS_KEY]
  Real_Value[UNITS_KEY] = 0

  # A sale transaction
  if (units < 0) {
    # The asset being sold must be "a" but if equity must be "b"
    correct_order = is_sale(now, a, b)
    assert(correct_order, sprintf("%s => can't sell either %s or %s\n", $0, get_short_name(a), get_short_name(b)))

    # If this is not an asset sale swop the accounts
    if (!is_asset(a) || is_closed(a, now)) {
      swop = a; a = b; b = swop
      amount = - amount
    }

    # Get brokerage (if any)
    current_brokerage = Real_Value[BROKERAGE_KEY]
    Real_Value[BROKERAGE_KEY] = 0

    # Amount should be the consideration as recorded by the broker
    #
    # Impact of GST
    if (not_zero(GST_Claimable)) {
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
        print_transaction(now, ("# GST " Leaf[a]), GST, b, -g)
        assert(FALSE, "GST Was levied on whole SELL transaction <" $0 ">")
      } else {
        # Brokerage Present => Adjust Brokerage
        # We Have A, B, -U, x - b, g
        # Produce A, B, -U, x - (1 - g) * x, # Note sign change with other case
        #         B, G,  0,           g * x, # Sign change engenders sense change in accounts
        g = GST_Claimable * gst_proportion(now) * current_brokerage

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
    adjust_cost(b, amount, now)

    # Did we swop? If so swop back
    if (b == swop) {
      b = a; a = swop
      amount = - amount
    }

    # Record the transaction
    if (not_zero(current_brokerage))
      fields[++ number_fields] = sprintf("%.*f", PRECISION, current_brokerage - g) # Always use 1st field

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if (DATE_ERROR != Extra_Timestamp) # No timestamp
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
    correct_order = is_purchase(a, b)
    assert(correct_order, sprintf("%s => can't buy asset %s\n", $0, get_short_name(b)))

    # If this is not an asset purchase swop the accounts
    if (!is_asset(b)) {
      swop = a; a = b; b = swop
      amount = - amount
    }

    # Normally fields[1] is  the parcel name
    if ("" != Parcel_Name)
      fields[++ number_fields] = Parcel_Name

    # Need to save the parcel_name, or alternatively parcel_timestamp, if present
    if (DATE_ERROR != Extra_Timestamp) # No timestamp
      fields[++ number_fields] = get_date(Extra_Timestamp)

    # Is this a new depreciating asset?
    if (is_fixed(b) && !(b in Method_Name)) {
      # This is  the asset Lifetime
      fields[++ number_fields] = Lifetime[b]  = Real_Value[LIFETIME_KEY]; Real_Value[LIFETIME_KEY] = 0
      assert(not_zero(Lifetime[b]), sprintf("%s => Can't have a fixed asset %s with zero life", $0, get_short_name(b)))

      # We need the method name
      # Currently a choice of POOL, DV, or PC
      fields[++ number_fields] = Method_Name[b] = Depreciation_Type
    }

    # Allow for brokerage if required - note can't have brokerage with depreciation
    if (not_zero(Real_Value[BROKERAGE_KEY])) {
      current_brokerage = Real_Value[BROKERAGE_KEY]
      Real_Value[BROKERAGE_KEY] = 0
    }

    # Simply adjust cost of <a> by the whole amount
    adjust_cost(a, -amount, now)

    # Impact of GST
    if (not_zero(GST_Claimable)) {
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
      Cost_Element = II
      print_transaction(now, ("# GST " Leaf[b]), a, GST, g)
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
    if (b == swop) {
      b = a; a = swop
      amount = - amount
    }

    # Record the transaction
    # Normal transactions
    # A, B, U, x, b - b*g, <optional-fields>, Comments
    # Record the adjustment due to brokerage and gst
    if (not_zero(current_brokerage))
      fields[++ number_fields] = sprintf("%.*f", PRECISION, current_brokerage - g)
    print_transaction(now, Comments, a, b, amount - g, sprintf("%10.3f", units), fields, number_fields)
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
    print_transaction(now, Comments, a, b, amount, "(I)")
  } else if (Extra_Timestamp > Epoch) {
    # The timestamp must be associated with a parcel
    fields[++ number_fields] = get_date(Extra_Timestamp)

    # A "parcel_timestamp" can indicate a maturity date
    # for a cash like term asset (eg a loan or a term deposit)
    # but only when an the asset is acquired and the timestamp is in the future

    # One account must be unitized or term limited
    # This logic is opaque...
    if (is_unitized(a) || is_unitized(b)) {
      assert(!is_unitized(a) ||  !is_unitized(b),
               sprintf("%s Both %s and %s cannot be unitized when parcel timestamp [%s] is set",
               $0, get_short_name(a), get_short_name(b), get_date(Extra_Timestamp)))
      # Instalment purchase
      adjust_cost(b,  amount, Extra_Timestamp)
      adjust_cost(a, -amount, now)

      # This will not balance when re-read from the state file unless balancing entries made
      adjust_cost(ADJUSTMENTS, -amount, Extra_Timestamp)
      adjust_cost(ADJUSTMENTS,  amount, now)

    } else if (is_term(b) || is_current(b)) {
      # This is a term deposit or similar (eg a mortgage or loan issued by the fund)
      adjust_cost(a, -amount, now)
      adjust_cost(b,  amount, now)
    } else
      assert(FALSE,
             sprintf("<%s> Either %s or %s must be a capital asset or a term asset when timestamp [%s] is set",
             $0, get_short_name(a), get_short_name(b), get_short_name(b), get_date(Extra_Timestamp)))

     # Record the transaction
     print_transaction(now, Comments, a, b, amount, Cost_Element, fields, number_fields)
  } else {
    # All Other Transactions
    # This must be an expense if GST is involved
    if (not_zero(GST_Claimable)) {
      # An expense
      # A, B, 0, (1 - g) * x
      # A, G, 0,      g * x
      g = GST_Claimable * gst_proportion(now) * amount

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
    print_transaction(now, Comments, a, b, amount, ternary(Tax_Adjustment, "(" Cost_Element ")", Cost_Element), fields, number_fields)
  }

  # Tidy up
  delete fields
}

# Set an account term for term limited assets and liabilities
#
function set_account_term(a, now) {
  # It is possible for a current account to have no term set

  # If the term is set make a note of it
  if (Real_Value[MATURITY_DATE_KEY] > 0)
   # It can change as long as it doesn't make a CURRENT asset non current
   set_entry(Account_Term[a], Real_Value[MATURITY_DATE_KEY], now)

  # At this stage term is assumed to be set in months
  if (greater_than(Extra_Timestamp, now)) { # Use the time stamp if it exists
    set_entry(Maturity_Date[a], Extra_Timestamp, now)

    # Don't use real value again
    Real_Value[MATURITY_DATE_KEY] = 0
  } else if (Real_Value[MATURITY_DATE_KEY] > 0) {
    # Need to set the first maturity date - real value is the same as account term
    Extra_Timestamp = add_months(now, Real_Value[MATURITY_DATE_KEY])
    set_entry(Maturity_Date[a], Extra_Timestamp, now)

    # Don't use real value again
    Real_Value[MATURITY_DATE_KEY] = 0
  } else if ((a in Maturity_Date) && greater_than(now, find_entry(Maturity_Date[a], just_before(now)))) {
    # Compute the maturity date
    Extra_Timestamp = add_months(now, find_entry(Account_Term[a], now))
    set_entry(Maturity_Date[a], Extra_Timestamp, now)
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
  action = get_short_name(a)

  # First lets check
  if (is_check) {
    # Check the action just after now
    switch(action) {
      case "VALUE" :
        quantity = get_value(account, now); break
      case "PRICE" :
        assert(is_unitized(account), sprintf("CHECK/SHOW: Only assets or equities have a PRICE: not %s\n", get_short_name(account)))
        quantity = find_entry(Price[account], now); break

      case "BALANCE" :
      case "COST" : quantity = get_cost(account, now); break

      case "UNITS" : quantity = get_units(account, now); break
      default : assert(FALSE, sprintf("CHECK/SHOW: %s => Unkown action %s\n",
                                      get_short_name(account), action))
    }

    # Is this a checkpoint?
    assert(near_zero(amount - quantity), sprintf("%s fails checkpoint %s [%s] => %.4f != %.4f\n",
                                                 get_short_name(account), action, get_date(now, LONG_FORMAT), quantity, amount))
    # Show this
    printf "## %s %s %s => %s\n", get_date(now), account, action,  format_value(quantity)
  } else {
    # is a setter
    switch(action) {
      case "VALUE" :
        # Valuations are  per  unit price
        # Was the number of units given?
        if (above_zero(units))
          amount /= units
        else # Just use the current cost if zero or negative units specified
          # If you want to set a zero value use PRICE instead
          amount = get_cost(account, now) / get_units(account, now)

      case "PRICE" :
        # This is a single unit
        # Set the price per unit
        set_entry(Price[account], amount, now)
      break

      case "BALANCE" :
      case "COST" :
        adjust_cost(account, amount, now)

        # top level class
        adjust_cost(star(get_name_component(account, 1)), - amount, now)
        break

      default : assert(FALSE, sprintf("ADJUST/SET: %s => Unkown action %s\n",
                                      get_short_name(account), action))
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
      printf "START_JOURNAL, %s\n", get_date(Start_Record) > Write_State
  }

  # Log data about selected variables
  if (Write_Variables)
    filter_data(Last_Record, Write_Variables, TRUE)
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
  printf "\tTime => %s\n", get_date(now) > STDERR
  if (parcel_tag)
    printf "\tParcel Name => %s\n", parcel_tag > STDERR
  if (parcel_timestamp >= Epoch)
    printf "\tSet purchase date   => %s\n", get_date(parcel_timestamp) > STDERR
@endif # LOG

  # Some units are bought
  assert(not_zero(u), sprintf("buy_units[%s] : can't buy zero units", $0))

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
      if (not_zero(catch_up_depreciation)) {
        update_cost(ac, - catch_up_depreciation, t)

        # Balance accounts
        adjust_cost(DEPRECIATION, catch_up_depreciation, t)

        # Print the transaction
        print_transaction(t, "# Closing Depreciation", ac, DEPRECIATION, catch_up_depreciation, "(D)")
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
      if (not_zero(p))
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

function sell_parcel(a, p, du, amount_paid, now,      gains, i, is_split) {
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
      copy_parcel(a, i - 1, a, i)

    # At this point we need to split parcels p & p + 1
    split_parcel(a, p, du)
    is_split = TRUE

    # One extra parcel
    Number_Parcels[a] += 1
  } # End of if splitting a parcel

  # Save realized gains
  if (is_fixed(a))
    gains = sell_fixed_parcel(a, p, now, - amount_paid)
  else
    gains = save_parcel_gain(a, p, now, - amount_paid)

  # Don't need to set the accounting cost to zero because get_cost will pick that up automatically
  # But the capital gain or loss needs to be balanced in the asset sums
  update_cost(a, -gains, now)

  # The sale price
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

  # Was a parcel split
  return is_split
} # End of if non-zero Parcel

#
# When a parcel of a fixed asset is sold
# it changes the depreciation amounts
function sell_fixed_parcel(a, p, now, gains,    cost) {
  # A depreciating asset will neither have capital gains nor losses
  # Suppose current value => c  (c >= 0)
  # Proceeds received     => p  (p <= 0)
  #
  # After sale value      => 0
  # Depreciation          => c + p <= 0
  # Appreciation             c + p >  0

  gains += sum_cost_elements(Accounting_Cost[a][p], now)
@ifeq LOG sell_units
  if (above_zero(gains)) # This was a DEPRECIATION expense
    printf "\tDepreciation => %s\n", print_cash(gains) > STDERR
  else if (below_zero(gains)) # This was an APPRECIATION income
    printf "\tAppreciation => %s\n", print_cash(-gains) > STDERR
  else
    printf "\tZero Depreciation\n" > STDERR
@endif # LOG

  # Any excess income or expenses are recorded
  if (above_zero(gains)) # This was a DEPRECIATION expense
    adjust_cost(SOLD_DEPRECIATION, gains, now)
  else if (below_zero(gains)) # This was APPRECIATION income
    adjust_cost(SOLD_APPRECIATION, gains, now)

  # return parcel gains
  return gains
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
  if (above_zero(gains)) {
    adjust_cost(REALIZED_LOSSES, gains, now)

    # Taxable losses are based on the reduced cost
    if (held_time >= CGT_PERIOD) {
      if (!(a in Long_Losses))
        Long_Losses[a] = initialize_account(LONG_LOSSES ":LL." Leaf[a])
      adjust_cost(Long_Losses[a], gains, now)
    } else {
      if (!(a in Short_Losses))
        Short_Losses[a] = initialize_account(SHORT_LOSSES ":SL." Leaf[a])
      adjust_cost(Short_Losses[a], gains, now)
    }

    # Balance taxable losses
    adjust_cost("*SPECIAL", -gains, now)
  } else if (below_zero(gains))
    adjust_cost(REALIZED_GAINS, gains, now)

  # Taxable gains
  # after application of tax adjustments
  # This works if tax adjustments are negative
  tax_gains = gains - find_entry(Tax_Adjustments[a][p], now)

  # Taxable Gains are based on the adjusted cost
  if (below_zero(tax_gains)) {
    # Taxable losses are based on the reduced cost
    if (held_time >= CGT_PERIOD) {
      if (!(a in Long_Gains))
        Long_Gains[a] = initialize_account(LONG_GAINS ":LG." Leaf[a])
      adjust_cost(Long_Gains[a], tax_gains, now)
    } else {
      if (!(a in Short_Gains))
        Short_Gains[a] = initialize_account(SHORT_GAINS ":SG." Leaf[a])
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
  if (keys_in(Parcel_Tag, ac, p))
    Parcel_Tag[b][q] = Parcel_Tag[a][p]

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
  if (keys_in(Parcel_Tag, ac, p))
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

@ifeq LOG check_balance
  # Verbose balance printing
  show_balance = TRUE
  output_stream = STDOUT
@else
  # No default printing
  show_balance = FALSE
  output_stream = STDERR
@endif #// LOG

  # Is there an error?
  if (not_zero(balance)) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > output_stream
    show_balance = TRUE
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
    assert(near_zero(balance), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
