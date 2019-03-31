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
  # #
  # # Initialize state file information
  # initialize_state()

  # Default Portfolio Name and document URI
  Journal_Title = "NEMO"
  Journal_Type = "IND"
  Document_Root = "https:example.com/NEMO"

  # Default currency
  Journal_Currency = JOURNAL_CURRENCY

  # Default Price Record Class
  Asset_Prefix = ASSET_PREFIX
  Asset_Suffix = ASSET_SUFFIX

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
@ifeq LOG read_state
      printf "%s => %s\n", Variable_Name, SYMTAB[Variable_Name] > "/dev/stderr"
@endif
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
    Asset_Prefix = ASSET_PREFIX
    Asset_Suffix = ASSET_SUFFIX
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
@ifeq LOG read_qualifying_dates
    printf "Get Ex-Dividend Dates\n" > "/dev/stderr"
    if (Symbol)
      printf "\t%s\n", Symbol > "/dev/stderr"
@endif
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
  if (FY)
    initialize_fy(FY "-" FY_Date)
  else { # Turn off EOFY reporting
    Reports = "/dev/null"

    # Default Start_Time and Stop_Time
    if (!Start_Time)
      Start_Time = Last_Time
    else {
      Start_Time = read_date(Start_Time)
      assert(DATE_ERROR != Start_Time, "Start_Time " Read_Date_Error)
    }

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


# Initialize the financial year
function initialize_fy(fy_date) {
  # Month name format
  Stop_Time = read_date(fy_date, 0)
  Start_Time = last_year(Stop_Time)

  # Override
  if (EOFY == Reports)
    Reports = "/dev/stdout"
}


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
  # These names need to be improved?
  LONG_GAINS    = initialize_account("SPECIAL.GAINS:LONG.GAINS")
  LONG_LOSSES   = initialize_account("SPECIAL.LOSSES:LONG.LOSSES")
  SHORT_GAINS   = initialize_account("SPECIAL.GAINS:SHORT.GAINS")
  SHORT_LOSSES  = initialize_account("SPECIAL.LOSSES:SHORT.LOSSES")
  #
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
      printf "%%%%, %s[%s]", i, get_date(now) > "/dev/stderr"
@endif
      set_array_bands(now, SYMTAB[i], NF)
      break

    case "SET_ENTRY" :
      # Set a time-dependent array entry
      i = trim($2)
      assert(i in SYMTAB && isarray(SYMTAB[i]), "Variable <" i "> is not an array")
      p = strtonum($3)
@ifeq LOG checkset
      printf "%%%%, %s[%s] => %11.2f\n", i, get_date(now), p > "/dev/stderr"
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
    printf "[%s] => %11.2f\n\t", k, bands[now][-k] > "/dev/stderr"
@endif
  }

  # Need to deal with case
  # %%,DATE,<ACTION>,15
  # i.e $(i+1) does not exist
  if (3 == nf) {
    bands[now][0] = strtonum($nf)
@ifeq LOG checkset
    printf " => %11.2f\n", bands[now][0] > "/dev/stderr"
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
  make_array(fields)

  # Special franking provisions
  if (a == TAX) {
    # Reduce franking
    adjust_cost(FRANKING,     - amount, now)

    print_transaction(now, "Reduce Franking Balance", FRANKING, NULL, 0, amount)
   } else if (is_tax(b)) {
    # Increase franking
    adjust_cost(FRANKING,       amount, now)

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

      # Need to establish if the franking account is needed
      if (is_franking(credit_account)) {
        adjust_cost(FRANKING, tax_credits, now)
        use_name = FRANKING
      } else
        use_name = NULL

      adjust_cost(credit_account, - tax_credits, now)
      print_transaction(now, ("# Tax Credits"), credit_account, NULL, 0, tax_credits)
    } else
      tax_credits = 0

    # Now LIC credits if any
    if (!near_zero(Real_Value[2])) {
      # Always treated as positive
      adjust_cost(LIC_CREDITS, - Real_Value[2], now)
      print_transaction(now, ("# " Leaf[a] " LIC Credits"), LIC_CREDITS, NULL, 0, Real_Value[2])
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
    if (is_fixed(a))
      allocate_costs(a, now)

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
  printf "%s => %s\n", LOG, get_date(now) > "/dev/stderr"
  printf "\tActive account => %s\n", a > "/dev/stderr"
  printf "\t\t Cost     => %s\n", print_cash(get_cost(a, now)) > "/dev/stderr"
  printf "\t\t Maturity => %s\n", get_date(maturity) > "/dev/stderr"
  printf "\t\t Term     => %d\n", find_entry(Account_Term[a], now) > "/dev/stderr"
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
    printf "\tThreshold_Dates => \n" > "/dev/stderr"
    walk_array(Threshold_Dates, 1, "/dev/stderr")
    printf "\n" > "/dev/stderr"
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
    printf "\tRenamed account => %s\n", active_account > "/dev/stderr"
    printf "\t\t Cost     => %s\n", print_cash(get_cost(active_account, now)) > "/dev/stderr"
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
  printf "Update Liabilities [%s]\n", get_date(now) > "/dev/stderr"
  if (member_id)
    printf "\t%20s => %s\n", "Member id", member_id > "/dev/stderr"
  printf "\tMember Shares\n" > "/dev/stderr"
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
    printf "\t%20s => %8.6f %16s => %14s\n", Leaf[member_id], sum_share, Leaf[member_id], print_cash(- amount) > "/dev/stderr"
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
      printf "\t%20s => %8.6f %16s => %14s\n", Leaf[member_account], x, Leaf[target_account], print_cash(- x * amount) > "/dev/stderr"
      if (get_cost(target_account, now) > 0)
        printf "\t\tNegative Balance in target account %16s => %14s\n", Leaf[target_account], print_cash(- get_cost(target_account, now)) > "/dev/stderr"
@endif # LOG
    } # End of exact share

    # Tidy up
    delete share
  } # End of allocation

@ifeq LOG update_member_liability
  # Just debugging
  printf "\t%20s => %8.6f %16s => %14s\n", "Share", sum_share, "Total", print_cash(- amount) > "/dev/stderr"
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
  # Filter out data entries that were added by PP or QQ records
  # do not overlap with the the holding period
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

   # Transactions
   if (Show_Transactions)
     list_transactions()
} #// END

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
  for (name in array_names) {
@ifeq LOG filter_data
    printf "Filter %s\n", array_names[name] > "/dev/stderr"
@endif
    filter_array(now, SYMTAB[array_names[name]], array_names[name])
  }
}

# Handle each array in turn
function filter_array(now, data_array, name,         t, a, p, start_block, end_block, block_id) {

  # list holding "blocks" - ie non-overlapping holding periods
  # Each block is preceeded and/or followed by "gaps"
  for (a in Leaf)
    if ((a in data_array) && is_unitized(a))
      if (ever_held(a)) {
        # Get each parcel in turn and list the contiguous blocks of time held
        start_block = end_block = just_before(Epoch)
        block_id = -1
        for (p = 0; p < Number_Parcels[a]; p ++) {
          # This starts a new holding block if the purchase date is after the current end date
          if (Held_From[a][p] > end_block) {
            if (end_block > start_block) {
@ifeq LOG filter_data
              # List this block
              printf "%36s, %16s, %03d, %11s, %11s\n", a, get_short_name(a), block_id + 1, get_date(start_block), get_date(end_block) > "/dev/stderr"
@endif

            } else
              # Any entries before this block can be filtered out
              filter_block(a, data_array, name, just_after(end_block), just_before(Held_From[a][p]))

            # A new block
            block_id ++
            start_block = end_block = Held_From[a][p]
          }

          # Each parcel is held until when?
          t = min_value(Held_Until[a][p], now)

          # Does this extend the holding period?
          if (t > end_block)
            end_block = t

          # If this parcel is open we have completed all possible blocks
          if (t == now)
            break
        } # End of each parcel p

        # The last holding block
@ifeq LOG filter_data
        # List this block
        printf "%36s, %16s, %03d, %11s, %11s\n", a, get_short_name(a), block_id + 1, get_date(start_block), get_date(end_block) > "/dev/stderr"
@endif
        if (end_block < now)
          filter_block(a, data_array, name, just_after(end_block), now)

        # delete duplicates
        #delete_duplicate_entries(Price[a])
      } else # Never held!
        unlink_account(a)

    # End of each asset a
}

# This simply removes out-of-range data ("name") from a time delimited block
function filter_block(a, data, name, start, end,      key) {

  # Get each price key and check it lies within the current block
  for (key in data[a]) {
    if (is_between(key, start, end)) {
@ifeq LOG filter_data
      printf "\t%s => Delete %s[%s]\n", Leaf[a], name, get_date(key) > "/dev/stderr"
@endif
      delete data[a][key]
    }
    } # End of each key
} # End of filter block

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
  printf "Sell Qualified Units [%s]\n", get_short_name(a)
  printf "\tDate              => %s\n", get_date(now)
  printf "\tWindow End        => %s\n", get_date(now + half_window)
  printf "\tQualified Units   => %.3f\n", get_qualified_units(a, now)
  printf "\tProvisional Units => %.3f\n", get_qualified_units(a, now + half_window)
  printf "\tSell              => %.3f\n", u
@endif

  # Get the latest key not beyond the window
  key = find_key(Qualified_Units[a], now + half_window)

  # While keys exist that are in the future
  # adjust them on a last-in-first-out basis
  du = u
  while (key > now) {
    # We will need the next key
    next_key = find_key(Qualified_Units[a], just_before(key))

@ifeq LOG qualified_units
  printf "\tKey               => %s\n", get_date(key)
  printf "\tUnits             => %.3f\n", get_qualified_units(a, key)
  printf "\tNext Key          => %s\n", get_date(next_key)
  printf "\tUnits             => %.3f\n", get_qualified_units(a, next_key)
  printf "\tParcel            => %.3f\n", get_qualified_units(a, key) - get_qualified_units(a, next_key)
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
      printf "\tReduced Units     => %.3f\n",  -du
@endif

      du = 0
    } else {
@ifeq LOG qualified_units
      printf "\tReduced Units     => %.3f\n",  dq
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
    sum_entry(Qualified_Units[a], - du, now) # Ok to make a non-proviosional negative parcel
@ifeq LOG qualified_units
    printf "\tAdjust Units      => %.3f\n", -du
@endif
  }

@ifeq LOG qualified_units
  printf "\tFinished Selling [%s]\n", Leaf[a]
  printf "\tWindow End        => %s\n", get_date(now + half_window)
  printf "\tQualified Units   => %.3f\n", get_qualified_units(a, now)
  printf "\tProvisional Units => %.3f\n", get_qualified_units(a, now + half_window)
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
  printf "Get Unrealized Gains (%s)\n", get_short_name(a)
  printf "\tDate => %s\n",  get_date(now)
  printf "\tPrice => %s\n", print_cash(current_price)
  printf "\tAdjustments => %s\n", print_cash(gains)
@endif # LOG

  # Unrealized gains held at time t are those in unsold parcels
  for (p = 0; p < Number_Parcels[a]; p++) {
    if (Held_From[a][p] > now) # All further transactions occured after (now)
      break # All done
    if (is_unsold(a, p, now)) # This is an unsold parcel at time (now)
      # If value > cash_in this is an unrealized gain
      gains += sum_cost_elements(Accounting_Cost[a][p], now) - Units_Held[a][p] * current_price
  }

@ifeq LOG get_unrealized_gains
  printf "\tGains   => %s\n", print_cash(gains)
  printf "\tParcels => %03d\n", p
  printf "\tUnits   => %05d\n", get_units(a, now)
@endif # LOG

  # The result
  return gains
}

# Buy a parcel of u units at the cost of x
function buy_units(now, a, u, x, parcel_tag, parcel_timestamp,
                                             last_parcel, p) {
@ifeq LOG buy_units
  printf "%s: %s units => %.3f amount => %11.2f\n", "buy_units", get_short_name(a), u, x
  printf "\tU => %.3f Cost => %.2f\n", get_units(a, now), get_cost(a, now)
  printf "\tTime => %s\n", get_date(now, LONG_FORMAT)
  if (parcel_tag)
    printf "\tParcel Name => %s\n", parcel_tag
  if (parcel_timestamp >= Epoch)
    printf "\tSet purchase date   => %s\n", get_date(parcel_timestamp)
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
    printf "Buy Units %s\n\tDate               => %s\n", Leaf[a], get_date(now)
    printf "\tQualification Date => %s\n", get_date(now + 0.5 * Qualification_Window)
    printf "\tBuy                => %.3f units\n", u
    printf "\tQualified Units    => %.3f\n", get_qualified_units(a, now)
    printf "\tProvisional Units  => %.3f\n", get_qualified_units(a, now + 0.5 * Qualification_Window)
@endif
  }

  # Debugging
@ifeq LOG buy_units
  printf "\t%s\n\t\tUnits => %.3f Cost => %.2f\n", get_short_name(a), get_units(a, now), get_cost(a, now)
  printf "\t\tParcel => %05d\n", last_parcel
@endif # LOG

  # Buy u units for x
  u = Units_Held[a][last_parcel]
  x = sum_cost_elements(Accounting_Cost[a][last_parcel], now) # This must be just the cash paid

  # Passive revaluation
  p = x / u
@ifeq LOG buy_units
  printf "\tPrice       => %11.2f\n", p
  printf "\tParcel      => %05d\n", last_parcel
  printf "\tCost        => %11.2f\n", x
  if (keys_in(Parcel_Tag, a, last_parcel))
    printf "\tParcel Name => %s\n", Parcel_Tag[a][last_parcel]
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
function sell_units(now, ac, u, x, parcel_tag, parcel_timestamp,        du, p, did_split, new_price, proportional_cost) {
@ifeq LOG sell_units
  printf "%s: %s units => %.3f amount => %11.2f\n", "sell_units", get_short_name(ac), u, x
  if ("" != parcel_tag)
    printf "\tSpecified parcel   => %s\n", parcel_tag
  if (parcel_timestamp >= Epoch)
    printf "\tParcel bought at   => %s\n", get_date(parcel_timestamp)
  printf "\tInitial Units      => %.3f\n", get_units(ac, now)
  printf "\tInitial Total Cost => %s\n", print_cash(get_cost(ac, now))
@endif # LOG

  # Try adjusting units now...
  adjust_units(ac, -u, now)


  # For a depreciating asset with multiple parcels (apart from a pooled asset) then
  # the proportion of the asset being sold depends on the
  # original cost of each unit; not on the number of units
  proportional_cost = FALSE
  if (is_fixed(ac)) {
    # Two complications - a fully depreciated asset
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
      printf "\tprice => %11.2f\n", new_price

      printf "\tSell from parcel => %05d\n", p
      if (parcel_tag)
        printf "\t\tParcel Tag       => %s\n", Parcel_Tag[ac][p]
      if (parcel_timestamp > Epoch)
        printf "\t\tParcel TimeStamp => %s\n", get_date(Held_From[ac][p])
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
              print_cash(get_parcel_cost(ac, p + 1, now))
        } # Was this the last parcel sold
      } # Was a parcel split
@endif # LOG
    } # End of match parcel

    # The next parcel
    p ++
  } # End of while statement

@ifeq LOG sell_units
  printf "\tFinal Units => %.3f\n", get_units(ac, now)
  printf "\tFinal Total Cost     => %s\n", print_cash(get_cost(ac, now))
@endif # LOG

  # Were all the requested units actually sold?
  assert(near_zero(u), sprintf("sell_units: Failed to sell the requested %d units of %s", u, get_short_name(ac)))

  # Update parent sums
  update_cost(ac, -x, now)
}

function sell_parcel(ac, p, du, amount_paid, now,      i, is_split) {
  # The sale date
  Held_Until[ac][p] = now

  # No parcel split yet
  is_split = FALSE

  # Amount paid
@ifeq LOG sell_units
  printf "\tAmount Paid => %s\n", print_cash(amount_paid)
@endif # LOG

  # Check for an empty parcel - allow for rounding error
  if (near_zero(Units_Held[ac][p] - du))
    # Parcel is sold off
    Units_Held[ac][p] = du
  else { # Units remain - parcel not completely sold off
@ifeq LOG sell_units
    printf "\tsplit parcel %3d on => %10.3f off => %10.3f\n\t\tadjustment => %s\n\t\tparcel cost => %s\n",
          p, Units_Held[ac][p], du, print_cash(get_cost_modifications(ac, p, now)), print_cash(get_parcel_cost(ac, p, now))
@endif # LOG

    # Shuffle parcels up by one
    for (i = Number_Parcels[ac]; i > p + 1; i --)
      # Copy the parcels
      copy_parcel(ac, i - 1, i)

    # At this point we need to split parcels p & p + 1
    split_parcel(ac, p, du)
    is_split = TRUE

    # One extra parcel
    Number_Parcels[ac] += 1
  } # End of if splitting a parcel

  # The sale price
  # This is always recorded as cost_element 0 since it is not actually a true cost
  # This must be recorded as cash flowing out of the account
  # A parcel is only ever sold once so we can simply set the cost
  set_entry(Accounting_Cost[ac][p][0], -amount_paid, now)

@ifeq LOG sell_units
  printf "\tsold parcel => %05d off => %10.3f date => %s\n\t\tHeld => [%s, %s]\n\t\tadjustment => %s\n\t\tparcel cost => %s\n\t\tparcel paid => %s\n",
    p, du,  get_date(now),
    get_date(Held_From[ac][p]), get_date(Held_Until[ac][p]),
    print_cash(get_cost_modifications(ac, p, now)),
    print_cash(get_parcel_cost(ac, p, now)),
    print_cash(get_cash_out(ac, p, now))
@endif # LOG

  # Save realized gains
  if (!is_fixed(ac))
    save_parcel_gain(ac, p, now)
  else {
    # A depreciating asset will neither have capital gains nor losses
    # so it will have accounting cost zero

    # Only need to save depreciation or appreciation
    i = sum_cost_elements(Accounting_Cost[ac][p], now) # The cost of a depreciating asset

    # Set it to zero (use element 0)
    sum_entry(Accounting_Cost[ac][p][0], -i, now)

    # We need to adjust the asset sums by this too
    update_cost(ac, -i, now)

@ifeq LOG sell_units
    if (near_zero(i))
      printf "\tZero Depreciation\n"
    else if (i > 0) # This was a DEPRECIATION expense
      printf "\tDepreciation => %s\n", print_cash(i)
    else if (i < 0) # This was APPRECIATION income
      printf "\tAppreciation => %s\n", print_cash(-i)
@endif # LOG

    # Any excess income or expenses are recorded in the following special accounts
    if (above_zero(i)) # This was a DEPRECIATION expense
      adjust_cost(SOLD_DEPRECIATION, i, now)
    else if (below_zero(i)) # This was APPRECIATION income
      adjust_cost(SOLD_APPRECIATION, i, now)
  }

  # Any tax adjustments are zeroed out here too - why?
  #
  # Do this at the end because they can influence capital gains
  #
  # This can create a spurious zero adjustment at now...
  #zero_costs(Tax_Adjustments[ac][p], now)

  # Was a parcel split
  return is_split
} # End of if non-zero Parcel

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

  return
}

# Copy and split parcels
function copy_parcel(ac, p, q,     e, key) {
@ifeq LOG sell_units
  printf "\t\t\tCopy parcel %3d => %3d\n", p, q
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
  # The following should always be true (Equity is treated a special case of liability)
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
    printf "Problem - Accounts Unbalanced <%s>\n", $0
    show_balance = TRUE
  }

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now)
    printf "\tAssets      => %20.2f\n", sum_assets
    printf "\tIncome      => %20.2f\n", sum_income
    printf "\tExpenses    => %20.2f\n", sum_expenses
    printf "\tLiabilities => %20.2f\n", sum_liabilities
    printf "\tEquities    => %20.2f\n", sum_equities
    printf "\tAdjustments => %20.2f\n", sum_adjustments
    printf "\tBalance     => %20.2f\n", balance
    assert(near_zero(balance), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}

# A control function which acts a useful shorthand for computing annual depreciation
function depreciate_all(now,       a, current_depreciation, comments) {
  # Depreciation is Cost Element I
  comments = "Automatic EOFY Depreciation"
  Automatic_Depreciation = TRUE
  Cost_Element = I

  # Depreciate everything
  for (a in Leaf)
    if (is_fixed(a) && is_open(a, now)) {
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
  Automatic_Depreciation = FALSE
}

# Allocate second element costs to the first element
# This always happens explicitly in the accounts
# Is this correct?
function allocate_costs(a, now,       p, second_element) {

  # Depreciating assets only use cost elements I or II
@ifeq LOG allocate_costs
  printf "Allocate Cost Element II\n%16s\n\tDate => %s\n", get_short_name(a), get_date(now)
@endif # LOG

  # Get each parcel
  for (p = 0; p < Number_Parcels[a]; p ++) {
    # Is this parcel purchased yet?
    if (Held_From[a][p] > now)
      break # All done
    if (is_unsold(a, p, now)) {
      # Debugging
@ifeq LOG allocate_costs
      printf "\tAdjusted Cost[%s] => %s\n", I, print_cash(get_parcel_element(a, p, I, now))
      printf "\tAdjusted Cost[%s] => %s\n", II, print_cash(get_parcel_element(a, p, II, now))
@endif # LOG

      # Get the second element of the cost
      second_element = get_parcel_element(a, p, II, now)
      if (!near_zero(second_element)) {
        # The Second Element Cost is applied to the First Element
        adjust_parcel_cost(a, p, now,   second_element,  I, FALSE)
        adjust_parcel_cost(a, p, now, - second_element, II, FALSE)
      }

@ifeq LOG allocate_costs
      printf "\t\tApply 2nd Element Cost => %s\n", second_element
      printf "\t\tAfter Application\n"
      printf "\t\tParcel            => %d\n", p
      printf "\t\tAdjusted Cost[%s] => %s\n", I, print_cash(get_parcel_element(a, p, I, now))
      printf "\t\tAdjusted Cost[%s] => %s\n", II, print_cash(get_parcel_element(a, p, II, now))
      printf "\t\tParcel Cost       => %11.2f\n", get_parcel_cost(a, p, now)
@endif # LOG
    } # End of if unsold parcel
  } # End of each parcel

@ifeq LOG allocate_costs
  printf "%s: %s New Reduced Cost[%s] => %11.2f\n", "allocate_costs", get_short_name(a), get_date(now), get_reduced_cost(a, now)
@endif # LOG
}


## Echo transactions
function list_transactions(     a, p, e, transaction_list, key,
                                old_s) {
  Start_Time = 0
  Show_All = 1
  make_array(transaction_list)

  ## Get each account
  ## This is clunky in the extreme
  ## It suggests that the parcel structure should be reworked to match the
  ## logic in the rest of the code
  ## Stop having cost[a][p][e][t] + a list of parcels
  ## and have instead cost[a][t][e] and units[a][t][p]??

  for (a in Leaf)
    if (is_unitized(a)) {
      # Assets and Equities are more complex
      # Organized by parcels
      # Each parcel has several components
      #   Bought => Held_From[a][p]
      #   Sold   => Held_Until[a][p]
      #   Accounting_Cost => Sums in and out [a][p][e][t]
      #   Tax_Adjustments => Adjustments to handle tax matters (eg depreciation) [a][p][e][t]
      #   Units_Held      => Units held [a][p][t]
      #   Parcel_Tag      => Named parcel  [a][p]
      #
      #   Other elements may include
      #   Lifetime [a][p]
      #   Method_Name [a][p]

      # get each parcel
      for (p in Accounting_Cost[a]) {
        for (e in Accounting_Cost[a][p])
          get_element_transactions(Accounting_Cost[a][p][e], a, p, e, transaction_list)
      }

      # Repeat for tax adjustments
      for (p in Tax_Adjustments[a])
        for (e in Tax_Adjustments[a][p])
          get_element_transactions(Tax_Adjustments[a][p][e], a, p, ("(" e ")"), transaction_list)
    } else {
      # The default cost element
      get_element_transactions(Cost_Basis[a], a, -1, COST_ELEMENT, transaction_list)
    } # Finished account a s

  # Sort in ascending order
  old_s = sort_arrays_on("@ind_num_asc")

  # Print out all the transactions
  for (key in transaction_list)
    printf "%s\n", transaction_list[key]

  # Tidy up
  delete transaction_list
  sort_arrays_off(old_s)
}

# Get the transactions for each parcel
function get_element_transactions(cost_array, a, parcel_id, element, transaction_list,
                                       parcel_units, parcel_tag,
                                       field_6, field_7, matched,
                                       k, dk, key, next_key, sum, next_sum,
                                       label) {

  # First key
  key = first_key(cost_array)

  # First cost
  sum = cost_array[key]

  # Is this the last key?
  while ((key - Epoch) > 0) {
    # No
    next_key = find_key(cost_array, key - 1)

    # Next sum
    next_sum = cost_array[next_key]

    # Start by clearing optional fields
    parcel_tag = field_6 = field_7 = ""

    # Not matched yet
    matched = ""

    # Allow a transaction label
    label = ""

    # Is this a parcel based asset?
    # The complications below are due to a few modifying optional fields on BUY, SELL and some CASH transactions
    if (-1 != parcel_id) {
      # Get the dates this parcel was
      #  bought, sold and the number of units that were sold
      parcel_units = Units_Held[a][parcel_id]
      if (parcel_units) {
        # Is this a BUY transaction?
        if ((I == element) && (Held_From[a][parcel_id] == key)) {
          # BUY
          label = "BUY, "

          # Check for depreciating assets
          if (a in Method_Name) {
            # Is this is not the low value POOL use the Lifetime
            if (Method_Name[a] !~ /POOL/)
              field_6 = Lifetime[a]
            else if (keys_in(Parcel_Tag, a, parcel_id))
              # Use the parcel tag - unless it is
              field_6 = Parcel_Tag[a][parcel_id]

            # Another field is present for depreciating assets
            field_7 = Method_Name[a]
          } else if (keys_in(Parcel_Tag, a, parcel_id))
            # Use the parcel tag
            field_6 = Parcel_Tag[a][parcel_id]

          # Cost element is units bought
          element = sprintf("%10.3f", parcel_units)
        } else if ((0 == element) && (Held_Until[a][parcel_id] == key)) {
          # SELL
          label = "SELL,"

          # Use parcel tag if one is available
          if (keys_in(Parcel_Tag, a, parcel_id))
            field_6 = Parcel_Tag[a][parcel_id]
          else # Use the purchase date
            field_6 = get_date(Held_From[a][parcel_id])

          # Cost element is units sold
          element = sprintf("%10.3f", - parcel_units)
        } else if (EXPORT_FORMAT)
          element = ""
      }
    } else if (a in Account_Term) {
      # Modifers for term based accounts - can ignore this if export format set
      # Is  this the initial transaction?
      if (find_key(cost_array, just_before(key)) == Epoch)
        field_6 = find_entry(Account_Term[a], just_after(key)) # just_after might not be needed here
      else
        field_6 = get_date(find_entry(Maturity_Date[a], just_after(key)))
    }

    # Are we matching particular accounts?
    # mmmm - this seems ramshackle
    match_accounts(matched, Show_Account, a, "")
    if (!Show_Account || matched) {
      # Prevent key conflicts
      # If the closest keys are DELTA_T apart
      # and we multiply by MAX_TRANSACTIONS then if k < DELTA_T * MAX_TRANSACTIONS this should work
      k = MAX_TRANSACTIONS * (key - Epoch)
      for (dk = 0; dk < DELTA_T * MAX_TRANSACTIONS; dk ++) {
        if (!(k in transaction_list))
          # An empty slot
          break

        # Increment k too
        k ++
      }

      # Is this ok?
      assert(dk < DELTA_T * MAX_TRANSACTIONS, "Too many transactions too create a unique list - increase DELTA_T * MAX_TRANSACTIONS <" dk ">")

      # Save the transaction
      transaction_list[k] = transaction_string(key, label, a, "", element, sum - next_sum, field_6, field_7, matched)
    }

    # The next key
    key = next_key
    sum = next_sum
  } # Finished transactions
} # End of get_element_transactions