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
@include "mpx.h"

# It needs its own begin section...
BEGIN {

  # // Extras for AUD
  make_array(ATO_Levy)
  make_array(CGT_Discount)
  make_array(Franking_Deficit_Offsets)
  make_array(GST_Rate)
  make_array(LIC_Allowance)
  make_array(LIC_Deduction)
  make_array(Member_Liability)
  make_array(Pension_Liability)
  make_array(Reserve_Rate)

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
    set_key(Reserve_Rate, Epoch, 0.0) # Default reserve allocation
  } else
    # Normally ALLOCATED is a synonym for ADJUSTMENTS - only needed by SMSF Journals
    ALLOCATED = ADJUSTMENTS


  # Precision is an issue - use a rational number for some allowances
  if (is_smsf) {
    ordered_pair(CGT_Discount, 1, 3) # Capital Gains Discount
    ordered_pair(LIC_Allowance, 1, 3) # Listed Investment Company Allowance
  } else if (is_individual) {
    ordered_pair(CGT_Discount, 1, 2) # Capital Gains Discount
    ordered_pair(LIC_Allowance, 2, 3) # Listed Investment Company Allowance
  } else {
    ordered_pair(CGT_Discount, 0, 1) # Capital Gains Discount
    ordered_pair(LIC_Allowance, 0, 1) # Listed Investment Company Allowance
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
                                        low_income_offset, middle_income_offset, offset_name,
                                        taxable_income,
                                        tax_levy, x, header) {

  # Print this out?
  write_stream = report_tax(eofy_stream(now))

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
  other_income = yield_positive(market_changes, 0)
  if (!near_zero(other_income)) {
    printf "%s\t%40s %32s\n", header, "Unrealized Losses", print_cash(other_income) > write_stream
    header = ""
  }

  # Accounting losses are added - as are taxable gains
  accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)
  if (!near_zero(accounting_losses)) {
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
  if (!near_zero(other_expenses)) {
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
  taxable_gains = @Get_Taxable_Gains_Function(now, carry_losses(Capital_Losses, past, CARRY_FORWARD_TAX_LIMIT))
  if (below_zero(taxable_gains)) {
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
  if (WRITE_BACK_LIMIT && not_zero(carried_losses)) {
    # Try writing back losses
    printf "\n\t%27s => %14s\n", "Write Back Losses Available", print_cash(carried_losses) > write_stream

    # Rewrite refundable offsets to just before now so they can be zeroed later at a distinct timestamp
    carried_losses = write_back_losses(just_before(now), last_year(now), write_back_limit(now), carried_losses, write_stream)
  }

  # Imputation Tax Offsets
  #

  # Tax credits received during this FY
  franking_offsets = - (get_cost("*SPECIAL.FRANKING.OFFSET", now) - get_cost("*SPECIAL.FRANKING.OFFSET", past))
  if (!near_zero(franking_offsets)) {
    other_income += franking_offsets
    printf "%s\t%40s %32s\n", header, "Franking Offsets", print_cash(franking_offsets) > write_stream
    header = ""
  }

  if (!near_zero(other_income)){
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
  other_expenses = - yield_negative(market_changes, 0)
  if (other_expenses > Epsilon) {
    printf "%s\t%40s %32s\n", header, "Unrealized Gains", print_cash(other_expenses) > write_stream
    header = ""
  }

  # Tax exempt income
  other_expenses += exempt_income

  # Accounting losses are added - as are taxable gains
  accounting_gains = -(get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past))
  if (!near_zero(accounting_gains)) {
    printf "%s\t%40s %32s\n", header, "Capital Gains", print_cash(accounting_gains) > write_stream
    other_expenses += accounting_gains
    header = ""
  }

  # And the non-concessional contributions
  # Should look at CONTRIBUTION minus the one taxed subclass because maybe more than one tax-free subclass?
  contributions = -(get_cost("*INCOME.CONTRIBUTION.TAX-FREE", now) - get_cost("*INCOME.CONTRIBUTION.TAX-FREE", past))
  if (!near_zero(contributions)) {
    printf "%s\t%40s %32s\n", header, "Non Taxable Contributions", print_cash(contributions) > write_stream
    other_expenses += contributions
    header = ""
  }

  # Finally LIC Deductions (if eligible)
  # LIC deductions 1/3 for SMSF
  #                1/2 for individual
  #                0/3 for company
  lic_deduction = - rational_value(LIC_Allowance) * (find_entry(LIC_Deduction, now) - find_entry(LIC_Deduction, past))

  # Always apply allowance at this point to catch explicit allocations to LIC
  if (!near_zero(lic_deduction)) {
    printf "%s\t%40s %32s\n", header,"LIC Deduction", print_cash(lic_deduction) > write_stream
    other_expenses += lic_deduction
    header = ""
  }

  # Summarize other expenses
  if (!near_zero(other_expenses)) {
    underline(81, 0, write_stream)
    printf "%s\t%40s %32s\n\n", header, "Other Expenses", print_cash(other_expenses) > write_stream
    header = ""
  }

  taxable_income = benefits + other_income - other_expenses
  underline(81, 0, write_stream)
  header = ""
  printf "%48s %32s\n\n", "TAXABLE INCOME OR LOSS", print_cash(taxable_income) > write_stream

  # Record this quantity
  set_entry(Taxable_Income, taxable_income, now)

  # Keep the income tax on the taxable income - the actual amount owed may change due to tax offsets etc
  income_tax = tax_owed = get_band_tax(now, "Tax", taxable_income) # Just need total tax
  printf "%48s %32s\n", "Income Tax on Taxable Income or Loss", print_cash(tax_owed) > write_stream
  underline(81, 0, write_stream)

  # Record this quantity
  set_entry(Income_Tax, income_tax, now)

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
  if (is_company) {

    # Franking
    # Check the franking balance (not the same as offsets, which were accumulated this FY)
    franking_balance = get_cost(FRANKING, just_before(now))

    # The franking deficit offsets
    franking_deficit_offsets = find_entry(Franking_Deficit_Offsets, now)
@ifeq LOG income_tax
    if (!near_zero(franking_deficit_offsets))
      printf "%48s %32s\n\n", "Franking Deficit Offsets", print_cash(franking_deficit_offsets) > write_stream
@endif

    # Need to check for franking deficit tax here
    if (below_zero(franking_balance)) {
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
      if (greater_than(- franking_balance, x)) {
        franking_deficit_offsets -= Franking_Deficit_Reduction * franking_balance
        printf "%48s\n", "Franking Deficit Offset Reduction Applied" > write_stream
      } else
        franking_deficit_offsets -= franking_balance

      if (!near_zero(franking_deficit_offsets))
        printf "%48s %32s\n\n", "New Franking Deficit Offsets", print_cash(franking_deficit_offsets) > write_stream

      # Do adjust tax owed
      tax_owed -= franking_balance

      printf "%48s %32s\n\n", "Tax Owed After Using Non-Refundable Offsets", print_cash(tax_owed) > write_stream

      # And reset franking balance
      # This is effectively a payment of stamped franking credits into the account
      # if (Start_Journal) {
      if (process_records(now)) {
        adjust_cost(FRANKING, - franking_balance, now)
        adjust_cost(FRANKING_STAMPED, franking_balance, now)
      }
    }
  }

  # Report the Imputation and Foreign Offsets
  if (!near_zero(franking_offsets)) {
    printf "%s\t%40s %32s\n", header, "Franking Offsets", print_cash(franking_offsets) > write_stream
    header = ""
  }

  # Foreign offsets
  # Are no-refund-no-carry
  foreign_offsets = - (get_cost("*SPECIAL.FOREIGN.OFFSET", now) - get_cost("*SPECIAL.FOREIGN.OFFSET", past))
  if (!near_zero(foreign_offsets)) {
    # Foreign offsets have complex rules too :( sigh ):
    #
    # If they are not greater than the Foreign_Offset_Limit it is ok to just use  them
    if (greater_than(foreign_offsets, find_entry(Foreign_Offset_Limit, now))) {
      # But they are greater  ....
      # we have taxable_income
      # and income_tax
      # (which are before any offsets)

      # compute the income tax that would be due if no foreign income or expenses were present
      foreign_income   = - (get_cost("*INCOME.FOREIGN", now) - get_cost("*INCOME.FOREIGN", past))
      foreign_expenses = - (get_cost("*EXPENSE.FOREIGN", now) - get_cost("*EXPENSE.FOREIGN", past))

      # Compute extra tax that would be due if foreign income and expenses were local income and expenses
      extra_tax = income_tax - get_band_tax(now, "Tax", taxable_income - foreign_income + foreign_expenses) # Just need total tax

      # Add levies (if any)
      extra_tax += get_band_tax(now, "Levy", taxable_income) # Is this correct - FixMe

      if (extra_tax < foreign_offsets)
        foreign_offsets = max(find_entry(Foreign_Offset_Limit, now), extra_tax)

      printf "\t%40s\n", "Foreign Offset Limit Applied" > write_stream
    } else
      extra_tax = 0

    # The offsets
    printf "%s\t%40s %32s\n\n", header, "Foreign Offsets", print_cash(foreign_offsets) > write_stream
    header = ""
    if (Show_Extra) {
      printf "\t%40s %32s\n\n", "Foreign Offset Limit", print_cash(find_entry(Foreign_Offset_Limit, now)) > write_stream
      if (above_zero(extra_tax))
        printf "\t%40s %32s\n\n", "Extra Tax Paid on Foreign Earnings", print_cash(extra_tax) > write_stream
    }
  } else
    foreign_offsets = 0

  # The following blocks should be made modular
  # No Carry Offsets (Class C)
  # Get defined no carry offsets
  # Foreign offsets are no-carry offsets
  no_carry_offsets = foreign_offsets - (find_entry(No_Carry_Offsets, now) - find_entry(No_Carry_Offsets, past))

  # Add any extra defined offsets
  no_carry_offsets += get_band_tax(now, "No_Carry", taxable_income, write_stream)

  # The no-carry offset
  if (not_zero(no_carry_offsets)) {
    printf "%s\t%40s %32s\n", header, "Total No-Carry Offsets", print_cash(no_carry_offsets) > write_stream
    header = ""
  }

  # Other offsets
  # The carry offset (Class D)
  carry_offsets = -(find_entry(Carry_Offsets, now) - find_entry(Carry_Offsets, past))
  carry_offsets += get_band_tax(now, "Carry", taxable_income, write_stream)
  if (!near_zero(carry_offsets)) {
    printf "%s\t%40s %32s\n", header, "Total Carry Offsets", print_cash(carry_offsets) > write_stream
    header = ""
  }

  # The refundable offset (Class E)
  refundable_offsets = - (find_entry(Refundable_Offsets, now) - find_entry(Refundable_Offsets, past))
  refundable_offsets += get_band_tax(now, "Refundable", taxable_income, write_stream)
  if (!near_zero(refundable_offsets)) {
    printf "%s\t%40s %32s\n", header, "Total Refundable Offsets", print_cash(refundable_offsets) > write_stream
    header = ""
  }
  printf "\n" > write_stream

  # Franking offsets are (currently) refundable for SMSF and individuals
  if (is_smsf || is_individual) {
    refundable_offsets += franking_offsets
    franking_offsets = 0
  } else
    no_carry_offsets += franking_offsets

  # At this stage no-carry and carry offsets behave the same
  no_refund_offsets = no_carry_offsets + carry_offsets

  # Apply the no_refund offsets (if any)
  if (above_zero(tax_owed) && above_zero(no_refund_offsets)) {
    # Since franking offsets can generate a loss add them to
    # both sides of the balance
    tax_owed += franking_offsets

    if (less_than(tax_owed, no_refund_offsets)) {
      # How many carry offsets were used?
      if (greater_than(tax_owed, no_carry_offsets)) # Some were used
        carry_offsets -= (tax_owed - no_carry_offsets)

      # information
      printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(tax_owed - franking_offsets) > write_stream
      tax_owed = 0
    } else { # All the no_refund offsets were used
      tax_owed -= no_refund_offsets
      carry_offsets = 0
      if (greater_than(no_refund_offsets, franking_offsets))
        printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(no_refund_offsets - franking_offsets) > write_stream
    }

    # OK now if the tax_owed is less than the amount of franking offsets
    # then the difference is transferred to tax losses
    if (less_than(tax_owed, franking_offsets)) {
      franking_offsets -= tax_owed

      printf "\t%40s %32s>\n", "<Franking Offsets Used", print_cash(tax_owed) > write_stream
      # Report remaining  franking offsets
      if (above_zero(franking_offsets))
        printf "\t%40s %32s>\n", "<Franking Offsets Remaining", print_cash(franking_offsets) > write_stream

      tax_owed = 0
    } else {
      tax_owed -= franking_offsets
      if (above_zero(franking_offsets))
        printf "\t%40s %32s>\n", "<All Franking Offsets Used", print_cash(franking_offsets) > write_stream
      franking_offsets = 0
    }

    # Report tax owed
    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Non-Refundable Offsets", print_cash(tax_owed) > write_stream
  } # End of if any attempt to apply non-refundable assets

  # Now apply refundable offsets - but note these will not generate a tax loss - since they are refunded :)
  if (above_zero(refundable_offsets)) {
    tax_owed -= refundable_offsets
    printf "\t%40s %32s>\n", "<Refundable Offsets Used", print_cash(refundable_offsets) > write_stream
    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Refundable Offsets", print_cash(tax_owed) > write_stream
  }

  # Franking deficit
  # Finally franking deficit tax offsets can be applied
  if (above_zero(tax_owed) && above_zero(franking_deficit_offsets)) {
    if (less_than(tax_owed, franking_deficit_offsets)) {
      # How many franking deficit tax offsets were used?
      if (greater_than(tax_owed, franking_deficit_offsets)) # Some were used
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
    if (above_zero(franking_deficit_offsets))
      printf "\t%40s %32s>\n", "<Franking Deficit Offsets Remaining", print_cash(find_entry(Franking_Deficit_Offsets, now)) > write_stream
  }

  #
  # Tax Losses
  #
  # The carried tax losses should be computed using the carried losses function
  #
  # First check if the taxable income was actually a loss this year
  tax_losses = get_carried_losses(now, Tax_Losses, - yield_negative(taxable_income, 0), CARRY_FORWARD_TAX_LIMIT, write_stream)

  # Losses can either be extinguished or (if there are new losses) carried forward
  # We can reduce tax_owed to zero, but not increase or generate a loss
  # Notice tax losses are stored as the income that generates the loss but
  # tax_owed is actually the tax
  if (above_zero(tax_owed)) {
    # If tax is owed franking offsets must be all used
    assert(near_zero(franking_offsets), "Can't have remaining franking offsets if tax is still owed")

    # Tax losses available for use - and tax is owed - compute marginal tax change
    if (above_zero(tax_losses)) {
      # x is the tax that would be paid on the tax_losses
      x = get_band_tax(now, "Tax", tax_losses + taxable_income) - income_tax # Just need total tax

    } else # No tax owed
      x = 0

    # We have tax owed for this year (tax_owed)
    # And we have the tax that would be extinguished by the carried tax losses (x)
    # Now we need to compute the change in the tax losses this would equate to
    if (less_than(tax_owed, x)) {
      # Yes so some losses will be extinguished
      # Which will reduce tax_owed to zero - so the effective reduction
      # in tax losses is the income that would produce tax equal to tax_owed
      if ("Tax" in Tax_Bands)
        x = - get_taxable_income(now, Tax_Bands["Tax"]["Income_Tax"], tax_owed) # This is effectively a gain - so make it negative
      else
        x = 0
      tax_owed = 0
    } else if (not_zero(x)) {
      # All losses extinguished
      tax_owed -= x
      if ("Tax" in Tax_Bands)
        x = - get_taxable_income(now, Tax_Bands["Tax"]["Income_Tax"], x) # This is effectively a gain - so make it negative
    }

    if (Show_Extra)
      printf "%48s %32s>\n\n", "<Tax Owed After Using Carried Tax Losses", print_cash(tax_owed) > write_stream
  } else {
    # No tax is owed
    # Was this caused by refundable offsets?
    if (!above_zero(tax_owed + refundable_offsets)) {
      # No - there are increased tax losses
      # This is a bit tricky
      # (unused) franking offsets may still be present here
      # plus the actual tax owed is modifiable by any refundable offsets (which will be refunded)
      if ("Tax" in Tax_Bands)
        x = - get_taxable_income(now, Tax_Bands["Tax"]["Income_Tax"], tax_owed + refundable_offsets - franking_offsets)
      else
        x = 0
    } else
      # Yes so zero new losses
      x = 0
  }

  # Now we can update the carried tax losses at last
  tax_losses = get_carried_losses(now, Tax_Losses, x, CARRY_FORWARD_TAX_LIMIT, write_stream)

  # Print the tax owed
  if (!header) {
    underline(81, 0, write_stream)
    printf "%48s %32s\n\n", "CURRENT TAX OR REFUND", print_cash(tax_owed) > write_stream
  }

  #
  # Tax Due
  #
  # Compute tax paid
  tax_paid = get_cost(PAYG, just_before(now))

  # And tax witheld
  tax_with = get_cost(WITHOLDING, just_before(now))

  # Now compute any tax levies
  #
  # If this is SMSF the levy is required
  if (is_smsf)
    printf "\t%40s %32s\n", "Supervisory Levy", print_cash(find_entry(ATO_Levy, now)) > write_stream

  # Add levies (if any)
  tax_levy  = - get_cost("*LIABILITY.CURRENT.LEVY", just_before(now))
  tax_levy += get_band_tax(now, "Levy", taxable_income, write_stream)

  # Summarize levies
  if (not_zero(tax_levy)) {
    printf "\t%40s %32s\n", "Tax Levies", print_cash(tax_levy) > write_stream
    tax_owed += tax_levy
  }

  if (!near_zero(tax_paid))
    printf "\t%40s %32s\n", "Income Tax Distributions Paid", print_cash(tax_paid) > write_stream
  if (!near_zero(tax_with))
    printf "\t%40s %32s\n", "Income Tax Withheld", print_cash(tax_with) > write_stream

  # Compute income tax due
  tax_due = tax_owed - (tax_paid + tax_with)
  underline(81, 0, write_stream)
  printf "%48s %32s\n\n\n", "AMOUNT DUE OR REFUNDABLE", print_cash(find_entry(ATO_Levy, now) + tax_due) > write_stream

  # Clean up balance sheet - watch out for unbalanced transactions
  # Save contribution tax accounted for
  tax_cont = get_cost(CONTRIBUTION_TAX, just_before(now))

  # Now save quantities -
  if ( process_records(now)) {
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
  capital_losses = carry_losses(Capital_Losses, now)
  if (Show_Extra) {
    # Report on the losses
    report_losses(now, Capital_Losses, "Capital Losses", write_stream)
    x = carry_losses(Capital_Losses, past)
    if (greater_than(capital_losses, x))
      printf "\t%40s %32s\n", "Capital Losses Generated", print_cash(capital_losses - x) > write_stream
    else if (less_than(capital_losses, x))
      printf "\t%40s %32s\n", "Capital Losses Extinguished", print_cash(capital_losses - x) > write_stream
  }
  if (!near_zero(capital_losses))
    printf "\t%40s %32s\n", "Capital Losses Carried Forward", print_cash(capital_losses) > write_stream

  tax_losses = carry_losses(Tax_Losses, now)

  if (Show_Extra) {
    report_losses(now, Tax_Losses, "Tax Losses", write_stream)
    x = carry_losses(Tax_Losses, past)
    if (greater_than(tax_losses, x))
      printf "\t%40s %32s\n", "Tax Losses Generated", print_cash(tax_losses - x) > write_stream
    else if (less_than(tax_losses, x))
      printf "\t%40s %32s\n", "Tax Losses Extinguished", print_cash(tax_losses - x) > write_stream
  }
  if (!near_zero(tax_losses))
    printf "\t%40s %32s\n", "Tax Losses Carried Forward", print_cash(tax_losses) > write_stream

  # Franking
  if (is_company && !near_zero(get_cost(FRANKING, now)))
    printf "\t%40s %32s\n", "Franking Balance Carried Forward", print_cash(get_cost(FRANKING, now)) > write_stream

  # Franking Deficit
  # Save the franking deficit offsets
  if (!near_zero(franking_deficit_offsets))
    printf "%48s %32s\n\n", "Franking Deficit Offsets Carried Forward", print_cash(franking_deficit_offsets) > write_stream
  else
    franking_deficit_offsets = 0
  if ( process_records(now))
    set_entry(Franking_Deficit_Offsets, -franking_deficit_offsets, now)

  # Update carry forward offsets
  if (!near_zero(carry_offsets))
    printf "\t%40s %32s\n", "Non-Refundable Offsets Carried Forwards", print_cash(carry_offsets) > write_stream
  else
    carry_offsets = 0
  if (process_records(now))
    set_entry(Carry_Offsets, -carry_offsets, now)

  # End report
  printf "\n" > write_stream

  if (process_records(now)) {
    # Now we need Deferred Tax - the hypothetical liability that would be due if all
    # assets were liquidated today
    deferred_gains = get_cost(UNREALIZED, now)

    # Gains are negative - losses are positive
    if (below_zero(deferred_gains))
      # Deferred tax losses can reduce future tax liability so are a deferred tax asset
      deferred_tax = - get_band_tax(now, "Tax", taxable_income - deferred_gains) - income_tax
    else
      deferred_tax = 0

    # Compute change in deferred tax
    x = deferred_tax - get_cost(DEFERRED, past)
    adjust_cost(DEFERRED, x, now)

    # Get the change this FY
    # If x < 0 EXPENSE
    # if x > 0 INCOME
    if (!near_zero(x)) {
      # Adjust cost/receipts for deferred expense/income
      # For a none SMSF this is a synonym for ADJUSTMENTS
      # There is a sign error here...
      adjust_cost(ALLOCATED, x, now)
    }
  }
}

#
# Get a banded tax
function get_band_tax(now, type, taxable_income, write_stream,    total, x, name) {
  # Get any offsets
  total = 0
  if (type in Tax_Bands)
    for (name in Tax_Bands[type]) {
      total += (x = get_tax(now, Tax_Bands[type][name], taxable_income))
      if (write_Stream && Show_Extra && not_zero(x))
        printf "\t<%39s %32s>\n", name, print_cash(x) > write_stream
    }

  # Return the offset
  return total
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
    discount = rational_value(CGT_Discount)

  # This function computes the taxable gains
  # It works for partioned long & short gains
  # And also for deferred gains when all such gains are long
  losses     += get_cost(star(LONG_LOSSES), now) + get_cost(star(SHORT_LOSSES), now)
  long_gains  = get_cost(star(LONG_GAINS), now)
  short_gains = get_cost(star(SHORT_GAINS), now)

  # Suppress negligible losses
  losses      = yield_positive(losses, 0)

  # Summarize starting point
@ifeq LOG get_gains
  printf "\nTaxable Gains Application of Combined Losses\n" > STDERR
  printf "\tDate        => %14s\n", get_date(now) > STDERR
  printf "\tLong  Gains => %14s\n", print_cash(-long_gains) > STDERR
  printf "\tShort Gains => %14s\n", print_cash(-short_gains) > STDERR
  printf "\tLosses      => %14s\n", print_cash(losses) > STDERR

@endif # LOG
  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  # Australian scheme & US Scheme are same
  # once short & long losses are disregarded
  if (!below_zero(losses + short_gains + long_gains)) {
    # More carried losses generated
    losses += short_gains + long_gains

    # Zero negligible losses
    if (near_zero(losses))
      losses = 0
@ifeq LOG get_gains
    else {
      printf "\n\tOverall Taxable Loss\n" > STDERR
      printf "\t%27s => %14s\n", "Taxable Losses", print_cash(losses) > STDERR
    }
@endif
    # Zero the gains
    short_gains = long_gains = 0
  } else if (!below_zero(losses + short_gains)) {
    # This can happen if when the losses are insufficient to
    # remove all the long gains
    losses += short_gains # reduce losses
    long_gains += losses  # apply them against long gains

    # But not a long term loss
    losses = short_gains = 0
@ifeq LOG get_gains
    printf "\n\tOnly Long Gains\n" > STDERR
    printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > STDERR
@endif
  } else {
    # Long and Short Gains
    short_gains += losses # Reduce short gains
    losses = 0
@ifeq LOG get_gains
    printf "\n\tBoth Short & Long Gains\n" > STDERR
    printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > STDERR
    printf "\t%27s => %14s\n", "Short Gains", print_cash(- short_gains) > STDERR
@endif
  }

  # Return either taxable gains or carried losses
  # if there are losses then the taxable gains are zero & vice-versa
  if (above_zero(losses))
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
  short_gains = ternary(short_gains, short_gains, 0)

  # Ensure there are gains
  if (!below_zero(total_gains))
    return 0

  # Neglect the component due to short gains
  fraction = long_gains / (long_gains + short_gains)

  # Compute the difference between the grossed up and net income long gains - short gains are disregarded
  extra_gains = rational_value(CGT_Discount) * fraction * total_gains / (1.0 - rational_value(CGT_Discount))

  # Track total share of extra gains remaining
  if (process_records(now)) {
    total_share = 1
    for (a in Leaf)
      if (select_class(a, "INCOME.GAINS.NET")) {
        # These are the income gains classes
        # Each account needs the income gains increased in proportion to its share of the total gains
        gains     = get_cost(a, now) - get_cost(a, past)

        # Skip negligible gains
        if (!below_zero(gains))
          continue

        # What share of the gains is this
        fraction = gains / long_gains

        # set new costs
        extra_share = fraction * extra_gains

        # Adjusting totals will allow swifter exit
        total_share -= fraction

        # Get underlying account and adjust its cost base
        assert(a in Underlying_Asset, "No underlying asset account to balance extra capital gains <" a ">")

@ifeq LOG get_gains
        printf "\t\t%27s\n", Leaf[Underlying_Asset[a]] > STDERR
        printf "\t\t%27s\n", Leaf[a] > STDERR
        printf "\t\t\tGains => %s\n", print_cash(- gains) > STDERR
        printf "\t\t\tExtra => %s\n", print_cash(- extra_share) > STDERR
        printf "\t\t\tTotal => %s\n", print_cash(- (gains + extra_share)) > STDERR
@endif

        # Because this is a tax adjustment it will not impact the market gains
        adjust_cost(a,                       extra_share, now) # This is the extra taxable gain
        adjust_cost(Underlying_Asset[a],   - extra_share, now, TRUE) # This is a tax adjustment because this is tax paid

        # Are we done?
        if (!above_zero(total_share))
          break
      }
  }

  # Compute the difference between the grossed up and net income long gains
  long_gains += extra_gains

@ifeq LOG get_gains
  printf "\t%27s => %14s\n", "Gross Short Income Gains", print_cash(- short_gains) > STDERR
  printf "\t%27s => %14s\n", "Gross Long Income Gains",  print_cash(- long_gains) > STDERR
@endif

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
  if (near_zero(unqualified))
    # The payment was fully qualified
    return

  # Were there any tax credits anyway?
  if (a in Tax_Credits) {
    underlying_asset = Underlying_Asset[a]

    # Get the Imputation credits associated with this transaction - and only this transaction
    imputation_credits = get_delta_cost(Tax_Credits[a], now)
    if (!near_zero(imputation_credits)) {
      # Create an unqualified account
      unqualified_account = initialize_account("SPECIAL.FRANKING.OFFSET.UNQUALIFIED:U_TAX." Leaf[underlying_asset])

      # The adjustment
      unqualified *= imputation_credits
@ifeq LOG dividend_qualification
      printf "Underlying Asset %s\n", Leaf[underlying_asset] > STDERR
      printf "\tTax Credits %s[%s]      => %s\n", Leaf[Tax_Credits[a]], get_date(now), print_cash(- imputation_credits) > STDERR
      printf "\tUnqualified Tax Credits => %s\n", print_cash(- unqualified) > STDERR
      printf "\tTotal Tax Credits       => %s\n", print_cash(- get_cost(Tax_Credits[a], now)) > STDERR
      printf "\tFranking Balance        => %s\n", print_cash(get_cost(FRANKING, now)) > STDERR
      printf "\tTotal Unqualified       => %s\n", print_cash(- get_cost(unqualified_account, now)) > STDERR
@endif

      # Now sum the unqualified credits in this account
      # This would occur when state files are used
      adjust_cost(unqualified_account, - unqualified, just_after(now))

      # Adjust the franking account too... (opposite sign - this is asset like)
      adjust_cost(FRANKING, unqualified, just_after(now))

@ifeq LOG dividend_qualification
      printf "\tNew Unqualified       => %s\n", print_cash(- get_cost(unqualified_account, just_after(now))) > STDERR
      printf "\tnew Franking Balance  => %s\n", print_cash(get_cost(FRANKING, just_after(now))) > STDERR
@endif
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
  more_past = last_year(past)
  is_detailed = ("" == is_detailed) ? 1 : 2

  # Show imputation report
  # The reports_stream is the pipe to write the schedule out to
  reports_stream = report_imputation(eofy_stream(now))

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
    print_line(past, reports_stream)
    x = get_cost("*" offset_class, past)
    printf "\t%24s%22s %26s\n\n", "Total Tax Offsets",
              print_cash(x - get_cost("*" offset_class, now)),
              print_cash(get_cost("*" offset_class, more_past) - x) > reports_stream
  }

  # Show the franking credits earned through tax payments
  print_line(past, reports_stream)
  x = get_cost(FRANKING_STAMPED, past)
  printf "\t%24s%22s %26s\n\n", "Net Franked Tax Payments",
    print_cash(x - get_cost(FRANKING_STAMPED, now)),
    print_cash(get_cost(FRANKING_STAMPED, more_past) - x) > reports_stream

  # Franking Credits Disbursed
  print_line(past, reports_stream)
  x = get_cost(FRANKING_PAID, past)
  printf "\t%24s%22s %26s\n\n", "Franking Credits Paid",
    print_cash(x - get_cost(FRANKING_PAID, now)),
    print_cash(get_cost(FRANKING_PAID, more_past) - x) > reports_stream

  # The balance
  print_line(past, reports_stream)
  x = get_cost(FRANKING, past)
  printf "\t%24s%22s %26s\n\n", "Closing Balance",
    print_cash(get_cost(FRANKING, now)),
    print_cash(get_cost(FRANKING, past)) > reports_stream

  printf "\n\n\n" > reports_stream
}

# A function to compute the gains associated with a parcel
function get_parcel_gains_aud(a, p, now,            held_time, gains, parcel_gains, units) {

  # Number of units
  units = Units_Held[a][p]

  # The held time
  held_time = get_held_time(now, Held_From[a][p])

  # Total gains (accounting gains)
  gains = get_parcel_proceeds(a, p) + sum_cost_elements(Accounting_Cost[a][p], now) # All elements

  # We want taxable gains
  # Gains are relative to adjusted cost
  # Losses are relative to reduced cost (so equal accounting losses)
  if (above_zero(gains))
    # These are losses
    parcel_gains = gains
  else
    # Assume zero losses or gains
    parcel_gains = 0

  # after application of tax adjustments
  # If there were losses then parcel_gains will be above zero
  gains -= find_entry(Tax_Adjustments[a][p], now)
  if (below_zero(gains)) {
    # Adjustments are negative and reduce taxable gains
    if (held_time >= CGT_PERIOD)
      parcel_gains = (1.0 - rational_value(CGT_Discount)) * gains
    else
      parcel_gains = gains
  }

  # return the gains
  return parcel_gains / units
}
