#!/usr/local/bin/gawk -f
# p.usd_modules.awk
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


# It needs its own begin section...
BEGIN {

  make_array(Capital_Gains_Tax_Bands)


  # // Can set constants here
  if ("" == Qualification_Window)
    EOFY_Window = Qualification_Window = 0
  else {
    Qualification_Window = 121 * ONE_DAY # seconds
    EOFY_Window = 0.5 * (Qualification_Window - ONE_DAY)
  }

  # Start of FY - arbitrary date
  if ("" == FY_Date)
    FY_Date = "Jan 01"

  # Default capital gains discount and LIC Allowance and Reserve rate
  # Default capital gains

  # Depreciation
  First_Year_Factor           = 1.0
  Depreciation_Method["PC"]   = "Prime Cost"
  Depreciation_Method["DV"]   = "Dim. Value"
  Depreciation_Method["POOL"] = "Pool Value"

  # Default tax rates
  if (!(Epoch in GST_Rate))
    GST_Rate[Epoch] = 0.0 # Default rate is zero

  # Foreign Offset Limit is $0 for US?
  if (!(Epoch in Foreign_Offset_Limit))
    Foreign_Offset_Limit[Epoch] = 0.00

  # The default tax band
  Tax_Bands[Epoch][0] = 0.15
}


# The tax modules for mpx
# These are the ones localized for the US (this is a proof of concept only)
# By far the most accurate
#
# Tax computations differ according to the entity type and the state
#
#
function initialize_tax_usd() {
  # Certain global variables are needed by the tax modules -
  # they are jurisdiction dependent and initialized here

  # Normally ALLOCATED is a synonym for ADJUSTMENTS
  ALLOCATED = ADJUSTMENTS
}

## Income Tax
# Print the statement of taxable income
# Tax Jurisdiction usd
# This is only the basis of the calculation
function income_tax_usd(now, past, benefits,

                                        reports_stream,
                                        taxable_gains,
                                        market_changes,
                                        gains_tax,
                                        accounting_gains, accounting_losses,
                                        foreign_income, exempt_income,
                                        foreign_expenses, extra_tax,
                                        income_due, other_expenses,
                                        other_income, deferred_tax, deferred_gains,
                                        capital_losses, old_losses, tax_losses,
                                        tax_owed, tax_paid, tax_due, tax_with, tax_cont, income_tax,
                                        foreign_offsets,
                                        no_carry_offsets, carry_offsets, refundable_offsets, no_refund_offsets,
                                        taxable_income,
                                        x) {

  # Print this out?
  reports_stream = report_tax(EOFY)

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
  other_income = yield_positive(market_changes, 0)
  if (!near_zero(other_income))
    printf "\t%40s %32s\n", "Unrealized Losses", print_cash(other_income) > EOFY

  # Accounting losses are added - as are taxable gains
  accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)
  if (!near_zero(accounting_losses)) {
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
  if (!near_zero(other_expenses)) {
    printf "\t%40s %32s\n", "Other Non Deductible Expenses", print_cash(other_expenses) > EOFY
    other_income += other_expenses
  }

  # taxable capital gains
  #
  # For individuals taxed preferentially
  if (is_individual) {
    taxable_gains = get_cost(TAXABLE_SHORT, now) # Short gains are added to taxable income
    gains_tax = get_tax(now, Capital_Gains_Tax_Bands, - get_cost(TAXABLE_LONG, now))
  } else {
    taxable_gains = get_cost(TAXABLE_SHORT, now) + get_cost(TAXABLE_LONG, now)
    gains_tax = 0
  }

  if (near_zero(taxable_gains))
    taxable_gains = 0
  else {
    # Gains are a negative number
    other_income -= taxable_gains
    printf "\t%40s %32s\n", "Taxable Capital Gains", print_cash(-taxable_gains) > EOFY
  }

  # Save the taxable gains
  set_cost(TAXABLE_GAINS, taxable_gains, now)
  if (!near_zero(other_income)){
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
  other_expenses = - yield_negative(market_changes, 0)
  if (other_expenses > Epsilon)
    printf "\t%40s %32s\n", "Unrealized Gains", print_cash(other_expenses) > EOFY

  # Tax exempt income
  other_expenses += exempt_income

  # Accounting losses are added - as are taxable gains
  accounting_gains = -(get_cost("*INCOME.GAINS", now) - get_cost("*INCOME.GAINS", past))
  if (!near_zero(accounting_gains)) {
    printf "\t%40s %32s\n", "Capital Gains", print_cash(accounting_gains) > EOFY
    other_expenses += accounting_gains
  }

  # Summarize other expenses
  if (!near_zero(other_expenses)) {
    print_underline(80, 1, EOFY)
    printf "\t%40s %32s\n\n", "Other Expenses", print_cash(other_expenses) > EOFY
  }

  taxable_income = benefits + other_income - other_expenses
  print_underline(80, 1, EOFY)
  printf "%48s %32s\n\n", "TAXABLE INCOME OR LOSS", print_cash(taxable_income) > EOFY

  # Record this quantity
  set_cost(TAXABLE_INCOME, taxable_income, now)

  # Keep the income tax on the taxable income - the actual amount owed may change due to tax offsets etc
  income_tax = get_tax(now, Tax_Bands, taxable_income)  # The straight forward computation
  tax_owed = income_tax + gains_tax # Plus capital gains tax at the preferential rate
  printf "%48s %32s\n", "Income Tax on Taxable Income or Loss ", print_cash(tax_owed) > EOFY

  # Record this quantity
  set_cost(INCOME_TAX, tax_owed, now)

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

  # Foreign offsets
  # Are no-refund-no-carry
  foreign_offsets = - (get_cost("*SPECIAL.OFFSET.FOREIGN", now) - get_cost("*SPECIAL.OFFSET.FOREIGN", past))
  if (!near_zero(foreign_offsets)) {
    # Foreign offsets have complex rules too :( sigh ):
    #
    # If they are not greater than the Foreign_Offset_Limit it is ok to just use  them
    # For the US system there is no limit? not sure
    if (foreign_offsets > find_entry(Foreign_Offset_Limit, now)) {
      # But they are greater  ....
      # we have taxable_income
      # and income_tax
      # (which are before any offsets)

      # compute the income tax that would be due if no foreign income or expenses were present
      foreign_income   = - (get_cost("*INCOME.FOREIGN", now) - get_cost("*INCOME.FOREIGN", past))
      foreign_expenses = - (get_cost("*EXPENSE.FOREIGN", now) - get_cost("*EXPENSE.FOREIGN", past))

      extra_tax = income_tax - get_tax(now, Tax_Bands, taxable_income - foreign_income + foreign_expenses)
      if (extra_tax < foreign_offsets)
        foreign_offsets = max(find_entry(Foreign_Offset_Limit, now), extra_tax)

      printf "\t%40s\n", "Foreign Offset Limit Applied" > EOFY
    } else
      extra_tax = 0

    # The offsets
    printf "\t%40s %32s\n\n", "Foreign Offsets", print_cash(foreign_offsets) > EOFY
@ifeq LOG income_tax
    printf "\t%40s %32s\n\n", "Foreign Offset Limit", print_cash(find_entry(Foreign_Offset_Limit, now)) > EOFY
    if (extra_tax > 0)
      printf "\t%40s %32s\n\n", "Extra Tax Paid on Foreign Earnings", print_cash(extra_tax) > EOFY
@endif
  } else
    foreign_offsets = 0

  # No Carry Offsets (Class C)
  # Just get the total change in the offset noting that
  # Foreign offsets are no-carry offsets
  no_carry_offsets = foreign_offsets - (get_cost(NO_CARRY_OFFSETS, now) - get_cost(NO_CARRY_OFFSETS, past))

  # The no-carry offset
  if (!near_zero(no_carry_offsets))
    printf "\t%40s %32s\n", "Total No-Carry Offsets", print_cash(no_carry_offsets) > EOFY

  # Other offsets
  # The carry offset (Class D)
  carry_offsets = - get_cost(CARRY_OFFSETS, now)
  if (!near_zero(carry_offsets))
    printf "\t%40s %32s\n", "Total Carry Offsets", print_cash(carry_offsets) > EOFY
  printf "\n" > EOFY

  # The refundable offset (Class E)
  refundable_offsets = - get_cost(REFUNDABLE_OFFSETS, now)
  if (!near_zero(refundable_offsets))
    printf "\t%40s %32s\n", "Total Refundable Offsets", print_cash(refundable_offsets) > EOFY
  printf "\n" > EOFY

  # At this stage no-carry and carry offsets behave the same
  no_refund_offsets = no_carry_offsets + carry_offsets

  # Apply the no_refund offsets (if any)
  if (above_zero(tax_owed) && above_zero(no_refund_offsets)) {
    if (tax_owed < no_refund_offsets) {
      # How many carry offsets were used?
      if (tax_owed > no_carry_offsets) # Some were used
        carry_offsets -= (tax_owed - no_carry_offsets)

      # information
      printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(tax_owed) > EOFY
      tax_owed = 0
    } else { # All the no_refund offsets were used
      tax_owed -= no_refund_offsets
      carry_offsets = 0
      if (above_zero(no_refund_offsets))
        printf "\t%40s %32s>\n", "<Non-Refundable Offsets Used", print_cash(no_refund_offsets) > EOFY
    }

    # Report tax owed
@ifeq LOG income_tax
    printf "%48s %32s\n\n", "Income Tax After applying Non-Refundable Offsets", print_cash(tax_owed) > EOFY
@endif
  } # End of if any attempt to apply non-refundable assets

  # Now apply refundable offsets
  if (above_zero(refundable_offsets)) {
    tax_owed -= refundable_offsets
    printf "\t%40s %32s>\n", "<Refundable Offsets Used", print_cash(refundable_offsets) > EOFY
@ifeq LOG income_tax
    printf "%48s %32s\n\n", "Income Tax After applying Refundable Offsets", print_cash(tax_owed) > EOFY
@endif
  }

  #
  # Tax Losses
  #
  # The carried tax losses
  tax_losses = old_losses = get_cost(TAX_LOSSES, past)
@ifeq LOG income_tax
  if (above_zero(tax_losses))
    printf "\t%40s %32s\n", "Carried Tax Losses", print_cash(tax_losses) > EOFY
@endif

  #
  # We can reduce tax_owed to zero, but not increase or generate a loss
  if (above_zero(tax_owed)) {
    if (above_zero(tax_losses)) {
      # Tax losses available for use - compute marginal tax change
      # x is the tax that would be paid on the tax_losses
      x = get_tax(now, Tax_Bands, tax_losses + taxable_income) - income_tax

@ifeq LOG income_tax
      printf "\t%40s %32s\n\n", "Income Tax on Carried Tax Losses", print_cash(x) > EOFY
@endif
    } else # No losses available
      x = 0

    # Is the tax owed less than the losses available?
    # Remember we have tax_owed > 0
    if (tax_owed < x) {
      # Yes so some losses will be extinguished
      # Which will reduce tax_owed to zero;
      tax_losses = get_taxable_income(now, x - tax_owed)
@ifeq LOG income_tax
      printf "\t%40s %32s\n", "Tax Losses Extinguished", print_cash(old_losses - tax_losses) > EOFY
@endif
      tax_owed = 0
    } else {
      # All losses extinguished
      tax_owed -= x

      # So this reduces tax losses to zero
@ifeq LOG income_tax
      printf "\t%40s %32s\n", "All Tax Losses Extinguished", print_cash(tax_losses) > EOFY
@endif
      tax_losses = 0
    }
  } else { # Increase losses - franking offsets may not be zero
    tax_losses = get_taxable_income(now, franking_offsets - tax_owed)
@ifeq LOG income_tax
    printf "\t%40s %32s\n", "Tax Losses Generated", print_cash(tax_losses - old_losses) > EOFY
@endif
  }

  # The carried tax losses
@ifeq LOG income_tax
  if (above_zero(tax_losses))
    printf "\t%40s %32s\n", "Tax Losses Carried Forward", print_cash(tax_losses) > EOFY
@endif

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
  set_cost(RESIDUAL, get_cost(RESIDUAL, just_before(now)) + get_cost(TAX, just_before(now)), now)

  # Compute tax due
  tax_paid = get_cost(PAYG, just_before(now)) - get_cost(PAYG, past)

  # And tax witheld
  tax_with = get_cost(WITHOLDING, just_before(now)) - get_cost(WITHOLDING, past)

  if (!near_zero(tax_paid))
    printf "\t%40s %32s\n", "Income Tax Distributions Paid", print_cash(tax_paid) > EOFY
  if (!near_zero(tax_with))
    printf "\t%40s %32s\n", "Income Tax Withheld", print_cash(tax_with) > EOFY

  # Compute income tax due
  tax_due = tax_owed - (tax_paid + tax_with)
  set_cost(TAX, - tax_due, now)
  print_underline(80, 1, EOFY)
  printf "%48s %32s\n\n\n", "AMOUNT DUE OR REFUNDABLE", print_cash(tax_due) > EOFY

  # Adjust cost is OK because ALLOCATED/ADJUSTMENTS were reset at comencement of eofy_actions
  adjust_cost(ALLOCATED, - tax_owed, now)

  # Print out the tax and capital losses carried forward
  # These really are for time now - already computed
  capital_losses = get_cost(CAPITAL_LOSSES, now)
  if (!near_zero(capital_losses))
    printf "\t%40s %32s\n", "Capital Losses Carried Forward", print_cash(capital_losses) > EOFY

  # The change in tax losses
  if (!near_zero(tax_losses - old_losses)) {
    if (tax_losses > old_losses)
      printf "\t%40s %32s\n", "Tax Losses Generated", print_cash(tax_losses - old_losses) > EOFY
    else
      printf "\t%40s %32s\n", "Tax Losses Extinguished", print_cash(old_losses - tax_losses) > EOFY
  }

  # The carried tax losses
  if (!near_zero(tax_losses))
    printf "\t%40s %32s\n", "Tax Losses Carried Forward", print_cash(tax_losses) > EOFY
  else
    tax_losses = 0

  # Save the carried losses
  set_cost(TAX_LOSSES, tax_losses, now)

  # Update carry forward offsets
  if (!near_zero(carry_offsets))
    printf "\t%40s %32s\n", "Non-Refundable Offsets Carried Forwards", print_cash(carry_offsets) > EOFY
  else
    carry_offsets = 0
  set_cost(CARRY_OFFSETS, -carry_offsets, now)

  # Refundable offsets were (well) refunded so reset them too
  set_cost(REFUNDABLE_OFFSETS, 0, now)

  # Now we need Deferred Tax - the hypothetical liability that would be due if all
  # assets were liquidated today
  deferred_gains = get_cost(DEFERRED_GAINS, now)

  # If not actually losses these are all taxed as long gains
  if (below_zero(deferred_gains)) {
    # Print final taxable deferred gains
    printf "\t%27s => %14s\n", "Taxable Deferred Gains", print_cash(- deferred_gains) > EOFY
  }

  # Schedule is finished
  print_underline(43, 0, EOFY)
  print "\n" > EOFY


  # Gains are negative - losses are positive
  # Catch negligible gains
  if (!near_zero(deferred_gains)) {
    # Deferred tax losses can reduce future tax liability so are a deferred tax asset
    if (is_individual)
      # preferential rate
      deferred_tax = get_tax(now, Capital_Gains_Tax_Bands, - (get_cost(LONG_GAINS, now) + deferred_gains)) - gains_tax
    else
      # ordinary rate
      deferred_tax = get_tax(now, Tax_Bands, taxable_income - deferred_gains) - income_tax

    set_cost(DEFERRED, - deferred_tax, now)
    if (above_zero(deferred_tax))
      printf "\t%40s %32s\n", "Deferred Tax Liability", print_cash(deferred_tax) > EOFY
    else if (below_zero(deferred_tax))
      printf "\t%40s %32s\n", "Deferred Tax Asset    ", print_cash(deferred_tax) > EOFY
    else {
      deferred_tax = 0
      printf "\t%40s %32s\n", "Zero Deferred Tax", print_cash(deferred_tax) > EOFY
    }

    # Get the change this FY
    # If x < 0 EXPENSE
    # if x > 0 INCOME
    x = - deferred_tax - get_cost(DEFERRED, past)
    if (!near_zero(x)) {
      # Adjust cost/receipts for deferred expense/income
      # For a none SMSF this is a synonym for ADJUSTMENTS
      adjust_cost(ALLOCATED, x, now)
    }
  }

  # Set tax values to zero - is this needed?
  set_cost(PAYG, 0, now)
  set_cost(WITHOLDING, 0, now)
}


#
#
## Dividend Qualification Function
##
function dividend_qualification_usd(a, underlying_asset, now, unqualified,

                                       unqualified_account, x) {

  # The US version does nothing when fully unqualified


  # For US individuals long gains are created as capital gains
  #if (!is_individual || near_zero(1.0 - unqualified))
  if (near_zero(1.0 - unqualified))
    return # Fully unqualified

  # Treat payment as a long gain
  x = (1.0 - unqualified) * get_delta_cost(a, now)

  # The qualified fraction is not treated as a dividend but as a long gain
  adjust_cost(a, -x, now)
  adjust_cost(INCOME_LONG, x, now)

@ifeq LOG dividend_qualification
  printf "Underlying Asset %s\n", Leaf[underlying_asset] > "/dev/stderr"
  printf "Total Qualified Dividend %s => %s\n", get_date(now), print_cash(- x) > "/dev/stderr"
@endif

  return
}
