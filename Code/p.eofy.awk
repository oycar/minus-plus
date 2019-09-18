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
@include "mpx.h"


# Handle EOFY processing for mpx
function eofy_actions(now,      past, allocated_profits,
                                benefits) {
  # EOFY actions
  # Turn on reporting?
  ##if (now > Start_Time)
  ##  EOFY = STDERR

  # past is referred to now
  past = last_year(now)

@ifeq LOG eofy_actions
  # EOFY actions
  printf "EOFY Actions\n\tDate => %s\n", get_date(now) > STDERR
@endif

  # Depreciate everything - at EOFY
  depreciate_all(now)

  # Set EOFY accounts
  # Very careful ordering of get/set actions is required
  # the tax statements adjust costs and so this must be reproducible
  # Therefore affected accounts need to be reset to the input value
  set_cost(ADJUSTMENTS, get_cost(ADJUSTMENTS, just_before(now)), now)

  # For the case of ALLOCATED being a separate account
  if (ALLOCATED != ADJUSTMENTS) {
    allocated_profits = get_cost(ALLOCATED, just_before(now))
    set_cost(ALLOCATED, allocated_profits, now)
  }
  set_cost(MARKET_CHANGES, sum_market_gains(just_before(now)), now)

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
  print_depreciating_holdings(just_after(now), past, Show_Extra)

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
  allocate_second_element_costs(just_after(now))
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
  is_detailed = ternary(is_detailed, is_detailed, FALSE)

  # A flag to discriminate realized and unrealized gains
  is_realized_flag = ("Realized Gains" == gains_type)

  # A default sold time = the Future
  sold_time = ternary(sold_time, sold_time, Future)

  # The proceeds label
  proceeds_label = ternary(is_realized_flag, "Proceeds", "  Value ")
  to_label       = ternary(is_realized_flag, "  To  ", "Latest")

  # No header printed
  no_header_printed = TRUE

  # Record accounting gains
  accounting_gains = 0
  sum_long_gains = sum_short_gains = sum_long_losses = sum_short_losses = 0 # Tax gains/losses summed here

  # formatting
  asset_width = 15

  # For each asset sold in the current period
  for (a in Leaf)
    if (is_capital(a) && (is_realized_flag || is_open(a, now))) {
      gains_event = FALSE
      proceeds = cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
      long_gains = short_gains = long_losses = short_losses = 0
      units_sold = 0

      # The price
      if (!is_realized_flag) {
        current_price = find_entry(Price[a], now)
        last_key = found_key
      }

      # Need to select parcels by sold date
      for (p = 0; p < Number_Parcels[a]; p++ ) {
        if (Held_From[a][p] > now) # All further transactions occured after (now) - parcels are sorted in order bought
          break # All done

        # Check if sold in the (past, now) window (capital gains)
        # or if it is unsold (deferred gains)
        if ((is_sold(a, p, now) == is_realized_flag) && is_unsold(a, p, past)) {
          if (!gains_event) {
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
            label = get_short_name(a)
            gains_event = TRUE
            no_header_printed = FALSE
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
          adjusted_cost += get_parcel_cost(a, p, now, TRUE)

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
          if (above_zero(gains)) {
            # These are losses
            parcel_gains = gains
            if (held_time >= CGT_PERIOD) {
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
          if (below_zero(adjusted_gains)) {
            # Adjustments are negative and reduce taxable gains
            parcel_gains = adjusted_gains
            if (held_time >= CGT_PERIOD) {
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
            printf "%*s %*d %*.3f %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s %*s\n",
                 asset_width + 1, label,
                 7, p,
                 11, units,
                 15, print_cash(parcel_cost),
                 11, get_date(Held_From[a][p]),
                 10, get_date(last_key),
                 12, print_cash(current_price),
                 14, print_cash(parcel_proceeds),
                 14, print_cash(get_parcel_cost(a, p, now)),
                 14, print_cash(get_parcel_cost(a, p, now, TRUE)),
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
          underline(175 + asset_width, 6, reports_stream)

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
        if (not_zero(long_gains))
          Gains_Stack[Long_Gains_Key]   = long_gains
        if (not_zero(long_losses))
          Gains_Stack[Long_Losses_Key]  = long_losses
        if (not_zero(short_gains))
          Gains_Stack[Short_Gains_Key]  = short_gains
        if (not_zero(short_losses))
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
    reports_stream = report_capital(EOFY)

    # First print the gains out in detail when required
    if (DEVNULL != reports_stream) {
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
    if (not_zero(income_long_gains))
      printf "\t%27s => %14s\n", "Long Income Gains", print_cash(- income_long_gains) > reports_stream
    if (not_zero(income_short_gains))
      printf "\t%27s => %14s\n", "Short Income Gains", print_cash(- income_short_gains) > reports_stream

    # Simililarly for expenses
    expense_long_losses  = get_cost(EXPENSE_LONG, now) - get_cost(EXPENSE_LONG, past)
    expense_short_losses = get_cost(EXPENSE_SHORT, now) - get_cost(EXPENSE_SHORT, past)

    # Take care that the adjustments are not applied more than once
    if (not_zero(expense_long_losses))
      printf "\t%27s => %14s\n", "Long Expense Losses", print_cash(expense_long_losses) > reports_stream
    if (not_zero(expense_short_losses))
      printf "\t%27s => %14s\n", "Short Expense Losses", print_cash(expense_short_losses) > reports_stream

    # The long gains and losses first
    taxable_long_gains  = income_long_gains + get_cost(LONG_GAINS, just_before(now)) - get_cost(LONG_GAINS, past)
    taxable_long_losses = expense_long_losses + get_cost(LONG_LOSSES, just_before(now)) - get_cost(LONG_LOSSES, past)

    # short gains & losses
    taxable_short_gains   = income_short_gains + get_cost(SHORT_GAINS, just_before(now)) - get_cost(SHORT_GAINS, past)
    taxable_short_losses  = expense_short_losses + get_cost(SHORT_LOSSES, just_before(now)) - get_cost(SHORT_LOSSES, past)

    # The taxable gains and losses
    printf "\t%27s => %14s\n",   "Long Taxable Gains", print_cash(- taxable_long_gains) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Taxable Gains", print_cash(- taxable_short_gains) > reports_stream

    ## If the taxable gains/losses are non zero at the EOFY  they must be carried losses
    carried_losses = get_carried_losses(past, CARRY_FORWARD_LIMIT)
    if (above_zero(carried_losses))
      printf "\t%27s => %14s\n", "Carried Capital Losses", print_cash(carried_losses) > reports_stream
    else {
      assert(!below_zero(carried_losses), sprintf("Cannot carry taxable capital gains forward [%s] Gains => %14s", get_date(past), print_cash(- carried_losses)))
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
  if (!below_zero(gains)) {
    # No overall gains
    # There could be a loss in this scenario
    losses = yield_positive(gains, 0)

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
@ifeq LOG get_gains
  printf "get_carried_losses: %3d\n", limit > STDERR
  printf "\tPast  => %14s\n", get_date(past) > STDERR
  printf "\tLimit => %14s\n", get_date(limit_time) > STDERR
@endif

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
    carried_losses = yield_positive(carried_losses, 0)

@ifeq LOG get_gains
  printf "\t\ty      => %14s\n", get_date(y) > STDERR
  printf "\t\tLosses => %14s\n", print_cash(losses) > STDERR
  printf "\t\tSummed => %14s\n", print_cash(carried_losses) > STDERR
@endif

    # Next year
    y += one_year(y, 1)
  } while (y < past + ONE_WEEK)

  # record if carried losses were extinguished
  losses = get_cost(CARRIED_LOSSES, past)
  if (not_zero(losses - carried_losses)) {
    printf "\tPast  => %14s\n", get_date(past) > STDERR
    printf "\tOld Carried Losses => %14s\n", losses > STDERR
    printf "\tNew Carried Losses => %14s\n", carried_losses > STDERR
  }

  # This determines the carried losses
  set_cost(CARRIED_LOSSES, carried_losses, past)
  return carried_losses
}

# Compute the deferred gains
# And print out a schedule
#
function get_deferred_gains(now, past, is_detailed,       accounting_gains, reports_stream,
                                                          gains, losses) {

 # The reports_stream is the pipe to write the schedule out to
 reports_stream = report_deferred(EOFY)

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
  more_past = last_year(past)
  is_detailed = ("" == is_detailed) ? 1 : 2

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = report_operating(EOFY)

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
  print_line(past, reports_stream)

  # Print grand total income
  printf "\t%22s %23s", "Total Income", print_cash(benefits[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > reports_stream
  else
    printf "\n" > reports_stream
  print_line(past, reports_stream)

  # the unrealized gains
  label = sprintf("\n\nInvestment Gains\nUnrealized Gains in Market Value\n")
  print_account_class(reports_stream, label, "select_class", "ASSET.CAPITAL", "", "get_unrealized_gains", now, past, past, more_past, is_detailed, -1) # Block Depreciating Assets

  # Obtain the market gains per year
  market_gains[now]  = - (get_cost(MARKET_CHANGES, now) - (x = get_cost(MARKET_CHANGES, past)))
  market_gains[past] = - (x - get_cost(MARKET_CHANGES, more_past))

  # Print the total unrealized gains
  print_line(past, reports_stream)

  # Print any unrealized gains
  if (above_zero(market_gains[now]) || above_zero(market_gains[past])) {
    printf "\t%22s %23s", "Total Market Gains", print_cash(yield_positive(market_gains[now], "")) > reports_stream
    if (past)
      printf " %26s\n", print_cash(yield_positive(market_gains[past], "")) > reports_stream
    else
      printf "\n" > reports_stream
    benefits[now]  += yield_positive(market_gains[now], 0)
    benefits[past] += yield_positive(market_gains[past], 0)
  }

  # Print a grand total
  print_line(past, reports_stream)

  # Print grand total income
  printf "\t%22s %23s", "Total of All Income", print_cash(benefits[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > reports_stream
  else
    printf "\n" > reports_stream
  print_line(past, reports_stream)
  printf "\n" > reports_stream

  # Now print expenses... (exclude tax payments)
  label = sprintf("Expenses\nGeneral Expenses\n")
  label = print_account_class(reports_stream, label, "block_class", "EXPENSE", "EXPENSE.UNREALIZED", "get_cost", now, past, past, more_past, is_detailed)

  # Need to correct for market gains captured as expenses
  losses[now]  = market_gains[now]  + get_cost("*EXPENSE", now) - (x = get_cost("*EXPENSE", past))
  losses[past] = market_gains[past] + x - get_cost("*EXPENSE", more_past)

  # Print a total
  print_line(past, reports_stream)

  # Print grand total income
  printf "\t%22s %23s", "Total Expenses", print_cash(losses[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(losses[past]) > reports_stream
  else
    printf "\n" > reports_stream
  print_line(past, reports_stream)
  printf "\n\n" > reports_stream

  # Print any unrealized losses
  if (below_zero(market_gains[now]) || below_zero(market_gains[past])) {
    printf "\t%22s %23s", "Total Market Losses", print_cash(yield_negative(market_gains[now], "")) > reports_stream
    if (past)
      printf " %26s\n", print_cash(yield_negative(market_gains[past], "")) > reports_stream
    else
      printf "\n" > reports_stream
    losses[now]  -= yield_negative(market_gains[now], 0)
    losses[past] -= yield_negative(market_gains[past], 0)
  }

  # Print a total
  print_line(past, reports_stream)

  # Print grand total expenses
  printf "\t%22s %23s", "Total of All Expenses", print_cash(losses[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(losses[past]) > reports_stream
  else
    printf "\n" > reports_stream
  print_line(past, reports_stream)
  printf "\n\n" > reports_stream

  # Print Before Tax benefits
  benefits[now]  -= losses[now]
  benefits[past] -= losses[past]
  printf "\t%27s %18s", "Benefits Accrued Before Tax", print_cash(benefits[now]) > reports_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > reports_stream
  else
    printf "\n" > reports_stream
  print_line(past, reports_stream)
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
  reports_stream = report_balance(EOFY)

  # Return if nothing to do
  if (DEVNULL == reports_stream)
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
  class_list["ASSET.TERM"] = TRUE
  class_list["ASSET.CURRENT"] = TRUE
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
  class_list["LIABILITY.TERM"] = TRUE
  class_list["LIABILITY.CURRENT"] = TRUE
  class_list["LIABILITY.MEMBER"] = TRUE
  class_list["LIABILITY.TAX"] = TRUE
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
   reports_stream = report_market(EOFY)

   # First print the gains out in detail
   if (DEVNULL != reports_stream) {
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
}

# Allocate second element costs
function allocate_second_element_costs(now,       a, p, second_element) {
  # Allocate everything
  # Cost II => Cost I
  for (a in Leaf)
    if (is_fixed(a) && is_open(a, now)) {
      # Depreciating assets only use cost elements I or II
@ifeq LOG allocate_second_element_costs
      printf "Allocate Cost Element II\n%16s\n\tDate => %s\n", get_short_name(a), get_date(now) > STDERR
@endif # LOG

      # Get each parcel
      for (p = 0; p < Number_Parcels[a]; p ++) {
        # Is this parcel purchased yet?
        if (Held_From[a][p] > now)
          break # All done
        if (is_unsold(a, p, now)) {
          # Debugging
@ifeq LOG allocate_second_element_costs
          printf "\tAdjusted Cost[%s] => %s\n", I, print_cash(get_parcel_element(a, p, I, now)) > STDERR
          printf "\tAdjusted Cost[%s] => %s\n", II, print_cash(get_parcel_element(a, p, II, now)) > STDERR
@endif # LOG

          # Get the second element of the cost
          second_element = get_parcel_element(a, p, II, now)
          if (!near_zero(second_element)) {
            # The Second Element Cost is applied to the First Element
            adjust_parcel_cost(a, p, now,   second_element,  I, FALSE)
            adjust_parcel_cost(a, p, now, - second_element, II, FALSE)
          }

@ifeq LOG allocate_second_element_costs
          printf "\t\tApply 2nd Element Cost => %s\n", second_element > STDERR
          printf "\t\tAfter Application\n" > STDERR
          printf "\t\tParcel            => %d\n", p > STDERR
          printf "\t\tAdjusted Cost[%s] => %s\n", I, print_cash(get_parcel_element(a, p, I, now)) > STDERR
          printf "\t\tParcel Cost       => %11.2f\n", get_parcel_cost(a, p, now) > STDERR
@endif # LOG
        } # End of if unsold parcel
      } # End of each parcel

@ifeq LOG allocate_second_element_costs
      printf "%s: %s New Reduced Cost[%s] => %11.2f\n", "allocate_second_element_costs", get_short_name(a), get_date(now), get_reduced_cost(a, now) > STDERR
@endif # LOG
    } # End of each fixed asset a
}


# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      reports_stream, a, p, open_key, close_key, parcel_depreciation, account_depreciation, open_cost, total_depreciaiton, sum_open,
                                                                  sale_depreciation, sale_appreciation, sum_adjusted) {

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = report_fixed(EOFY)
  if (DEVNULL == reports_stream)
    return

  is_detailed = ("" == is_detailed) ? FALSE : is_detailed
  total_depreciation = ""

  # Print out the assets in alphabetical order
  for (a in Leaf)
    if (is_fixed(a) && (is_open(a, now) || is_open(a, past))) {

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
        if (is_sold(a, p, now)) {
          close_key = just_before(Held_Until[a][p])
        } else
          close_key = just_before(now)

        # parcel open cost
        open_cost = get_parcel_cost(a, p, open_key)
        sum_open += open_cost

        # Always get the parcel depreciation
        parcel_depreciation = get_parcel_tax_adjustment(a, p, I, open_key) - get_parcel_tax_adjustment(a, p, I, close_key)

        #  Just track the total depreciation
        account_depreciation   += parcel_depreciation

        # Record detailed statement
        # Is this a named parcel?
        if (is_detailed && Number_Parcels[a] > 1) {
          if (keys_in(Parcel_Tag, a, p))
            printf "%28s %6d ", Parcel_Tag[a][p], p > reports_stream
          else
            printf "%35d ", p > reports_stream

          # Depreciation is the sum of the I tax adjustments
          printf " [%11s, %11s] %14s %14s %14s %14s %14s\n",
                    get_date(open_key), get_date(close_key), print_cash(open_cost),
                    print_cash(open_cost - parcel_depreciation),
                    print_cash(get_parcel_element(a, p, II, just_before(close_key))),
                    print_cash(get_parcel_cost(a, p, close_key)),
                    print_cash(parcel_depreciation) > reports_stream
        } # End of is_detailed
      } # End of each parcel

      # Depreciation is the sum of the tax adjustments
      # When was this asset opened?
      open_key = Held_From[a][0] # First parcel opened here
      if (open_key < past)
        open_key = past # This must be less than now for this asset to be open and considered
      if (is_closed(a, now))
        close_key = just_before(held_to(a, now))
      else
        close_key = just_before(now)

      # Two types of footer
      printf "%16s %11s ", get_short_name(a), Depreciation_Method[Method_Name[a]] > reports_stream
      if (is_detailed)
        printf "%8s", " " > reports_stream

      # The rest of the footer
      printf "[%11s, %11s] %14s %14s %14s %14s %14s\n",
        get_date(open_key), get_date(close_key), print_cash(sum_open),
        print_cash(sum_open - account_depreciation),
        print_cash(get_cost_element(a, II, just_before(close_key))),
        print_cash(get_cost(a, close_key)),
        print_cash(account_depreciation) > reports_stream

      # Track total depreciation too
      total_depreciation += account_depreciation
    } # End of a depreciating asset

  # Is there any depreciation/appreciation due to the sale of depreciating assets?
  sale_appreciation = get_cost(SOLD_APPRECIATION, now) - get_cost(SOLD_APPRECIATION, past)
  sale_depreciation = get_cost(SOLD_DEPRECIATION, now) - get_cost(SOLD_DEPRECIATION, past)
  if (!near_zero(sale_depreciation))
    printf  "\n%24s %115s\n", "Depreciation from Sales", print_cash(sale_depreciation) > reports_stream
  if (!near_zero(sale_appreciation))
    printf  "\n%24s %115s\n", "Appreciation from Sales", print_cash(-sale_appreciation) > reports_stream
  total_depreciation += sale_depreciation + sale_appreciation

  # Print a nice line
  if (!near_zero(total_depreciation)) {
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
  reports_stream = report_dividend(EOFY)

  # For each dividend in the previous accounting period
  print Journal_Title > reports_stream
  if (is_detailed)
    printf "Detailed Dividend Qualification Report\n" > reports_stream
  else
    printf "Dividend Qualification Report\n" > reports_stream
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > reports_stream

  # A header
  print_header = TRUE

  # Sum  the qualified payments
  qualified_payment = 0

  # Get each dividend/distribution
  # Start with dividends - this could be abstracted out later to include distributions
  # First key
  for (a in Leaf)
    if (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION.CLOSE")) { # Only closely held distributions are considered

      # Get each payment in the target period
      key = get_latest_transaction(a, now)

      while (key > past) {
        # A heading
        if (print_header) {
          printf "\n%12s\n", "Dividends" > reports_stream
          printf "%12s %13s %14s %14s %14s %14s %13s\n",
                  "Asset", "Ex-Div Date", "Eligible Units", "Payment Date", "Qualified", "% Qualified", "Payment"  > reports_stream
          underline(95, 6, reports_stream)
          print_header = FALSE
        }

        # The current asset
        assert(a in Underlying_Asset, "Can't find underlying asset for %s" Leaf[a]) > reports_stream
        underlying_asset = Underlying_Asset[a]

        # We will need the next key
        next_key = get_previous_transaction(a, key)

        # Short cut directly to the value of the dividend payment
        payment = - get_delta_cost(a, key)

        # The qualifying date is one day before the ex-dividend date
        qualifying_date = just_after(yesterday(get_exdividend_date(underlying_asset, key), HOUR))

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > DATE_ERROR, sprintf("%s: %s <%s>",  Leaf[a], Read_Date_Error, get_date(key)))

        # These are the units that were qualified on the qualifying date
        qualified_units = get_qualified_units(underlying_asset, qualifying_date)

        # Now get the total units
        total_units = get_units(underlying_asset, qualifying_date)

        # If not all units are qualified need to check the second half of the Qualification Window
        if (!near_zero(total_units - qualified_units)) {
          q = maximum_entry(Qualified_Units[underlying_asset], qualifying_date, qualifying_date + 0.5 * Qualification_Window)
          qualified_units = max_value(q, qualified_units)
          qualified_fraction = qualified_units / total_units

          # Should never be greater than unity
          assert(!above_zero(qualified_fraction - 1.0), sprintf("Qualified Units[%s] => %.3f > Units held on qualification date <%s>",
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
    payment = get_cost("*INCOME.DIVIDEND", past) + get_cost("*INCOME.DISTRIBUTION.CLOSE", past) - \
              get_cost("*INCOME.DIVIDEND", now) - get_cost("*INCOME.DISTRIBUTION.CLOSE", now)
    if (not_zero(payment))
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
  if (DEVNULL == stream)
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
      if (is_star(subclass))
        subclass = substr(subclass, 2)

      # Initialize sums
      if (last_subclass != subclass) {
        # If at least one entry found print a summary
        if (did_print) {
          if (print_all > 1)
            print_line(past, stream)
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
      if ("get_unrealized_gains" == income_function && is_closed(x, now))
        account_income[now] = 0
      else {
        account_income[now] = sign * (@income_function(x, now) - @income_function(x, now_past))
        account_sum[now] += account_income[now]
      }
      if (past) {
        if ("get_unrealized_gains" == income_function && is_closed(x, past))
          account_income[past] = 0
        else {
          account_income[past] = sign * (@income_function(x, past) - @income_function(x, past_past))
          account_sum[past] += account_income[past]
        }
      }

      if (!near_zero(account_income[now]) || !near_zero(account_income[past])) {
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
          printf "\t%24s %21s", get_short_name(x), print_cash(account_income[now]) > stream
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
      print_line(past, stream)
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
  return is_class(a, class_name)
}

# Include class blocking
function block_class(a, class_name, blocked_class) {
  if (is_class(a, blocked_class))
    return FALSE # Blocked!

  # Just the simple case
  return is_class(a, class_name)
}

# Block multiple classes...
function block_class_list(a, class_name, blocked_class_list,      x) {
  # blocked class might actually be an array of blocked classes
  for (x in blocked_class_list)
    if (is_class(a, x)) # Blocked!
      return FALSE

  # Just the simple case
  return is_class(a, class_name)
}

# Special purpose filter for current accounts
function current_class(a, class_name, blocked_class) {
  return is_class(a, class_name) && !(a in Maturity_Date)
}

# And its pigeon pair
function not_current_class(a, class_name, blocked_class) {
  return is_class(a, class_name) && (a in Maturity_Date)
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
    available_losses = write_back_losses(future_time, last_year(now), limit, available_losses, reports_stream)

    # Any losses left?
    if (near_zero(available_losses))
      return 0

    # Record the process
    # that adjusts the gains ONLY in this function
    # So SPECIAL.TAXABLE.LOSSES:WRITTEN.BACK
    taxable_gains  = get_cost("*SPECIAL.TAXABLE", now)
    printf "\t%27s => %13s\n", "Write Back", get_date(now) > reports_stream
    printf "\t%27s => %14s\n", "Gains", print_cash(- taxable_gains) > reports_stream

    # Get the gains
    if (below_zero(taxable_gains)) {
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
      if (below_zero(tax_refund))
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
