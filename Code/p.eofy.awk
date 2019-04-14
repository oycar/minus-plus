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
  if (now > Start_Time)
    EOFY = "/dev/stderr"

  # past is referred to now
  past = last_year(now)

@ifeq LOG eofy_actions
  # EOFY actions
  printf "EOFY Actions\n\tDate => %s\n", get_date(now) > "/dev/stderr"
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
                                                            label, no_header_printed, proceeds_label,

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
  printf "%s Report for Period Ending %s\n\n", gains_type, get_date(yesterday(now))  > gains_stream

  # Are we printing out a detailed schedule?
  is_detailed = ternary(is_detailed, is_detailed, FALSE)

  # A flag to discriminate realized and unrealized gains
  is_realized_flag = ("Realized Gains" == gains_type)

  # A default sold time = the Future
  sold_time = ternary(sold_time, sold_time, Future)

  # The proceeds label
  proceeds_label = ternary(is_realized_flag, "Proceeds", "  Value ")

  # No header printed
  no_header_printed = TRUE

  # Record accounting gains
  accounting_gains = 0
  sum_long_gains = sum_short_gains = sum_long_losses = sum_short_losses = 0 # Tax gains/losses summed here

  # For each asset sold in the current period
  for (a in Leaf)
    if (is_capital(a) && (is_realized_flag || is_open(a, now))) {
      gains_event = FALSE
      proceeds = cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
      long_gains = short_gains = long_losses = short_losses = 0
      units_sold = 0

      # The price
      if (!is_realized_flag)
        current_price = find_entry(Price[a], now)

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
              printf "\n%12s %10s %9s %11s %10s %16s %15s %14s %14s %15s %9s %20s %15s\n",
                      "Asset", "Parcel", "Units", "From", "To", "Cost", proceeds_label,
                      "Reduced", "Adjusted", "Accounting", "Type", "Taxable", "Per Unit" > gains_stream
            else if (no_header_printed) {
              printf "%12s %12s %12s %15s %14s %14s %15s %9s %20s\n",
                     "Asset", "Units", "Cost",
                     proceeds_label, "Reduced", "Adjusted", "Accounting", "Type", "Taxable" > gains_stream
              underline(125, 6, gains_stream)
            }

            # print Name
            label = get_short_name(a)
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
          underline(158, 8, gains_stream)
          printf "%13s %20.3f %41s ",
            label, units_sold,
            print_cash(cost) > gains_stream
        } else
          printf "%13s %12.3f %14s ",
            label, units_sold,
            print_cash(cost) > gains_stream

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
          printf "\n%*s %15s", 116 + 35 * is_detailed, key, print_cash(- Gains_Stack[key]) > gains_stream

        printf "\n\n" > gains_stream
        delete Gains_Stack[key]
      } # End of gains event
    } # End of each asset

  # Final line
  if (!is_detailed)
    underline(125, 6, gains_stream)

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
    if (report_capital)
      gains_stream = ("" == EOFY) ? "/dev/null" : EOFY
    else
      gains_stream = "/dev/null"

    # First print the gains out in detail when required
    if ("/dev/null" != gains_stream) {
      print_gains(now, past, is_detailed, "Realized Gains", gains_stream)
      delete Gains_Stack
    }

    # Print the capital gains schedule
    print Journal_Title > gains_stream
    printf "Capital Gains Report for Period Ending %s\n", get_date(yesterday(now))  > gains_stream

    # Get total capital gains
    # Exploit existing sums
    # taxable capital gains are messy
    underline(44, 8, gains_stream)

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
    cgt_losses = get_cost(CAPITAL_LOSSES, just_before(now)) - get_cost(CAPITAL_LOSSES, carry_forward_limit(now))
    if (CARRY_FORWARD_LIMIT)
      printf "\t%27s => %14s\n", "Losses Carried Forward Since", get_date(carry_forward_limit(now)) > gains_stream
    if (!near_zero(cgt_losses))
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
    if (WRITE_BACK_LIMIT && !near_zero(cgt_losses)) {
      # Try writing back losses
      printf "\n\t%27s => %14s\n", "Write Back Losses Available", print_cash(cgt_losses) > gains_stream

      # Rewrite refundable offsets to just before now so they can be zeroed later at a distinct timestamp
      cgt_losses = write_back_losses(just_before(now), last_year(now), write_back_limit(now), cgt_losses, gains_stream)
    }

    # All done
    underline(43, 8, gains_stream)
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
 if (report_deferred)
   gains_stream = ("" == EOFY) ? "/dev/null" : EOFY
 else
   gains_stream = "/dev/null"

 # First print the gains out in detail
 accounting_gains = print_gains(now, past, is_detailed, "Deferred Gains", gains_stream)
 losses = Gains_Stack[Long_Losses_Key]
 gains  = Gains_Stack[Long_Gains_Key]
 delete Gains_Stack

 # Print the deferred gains report
 print Journal_Title > gains_stream
 printf "Deferred Gains Report for Period Ending %s\n", get_date(yesterday(now))  > gains_stream

 # Print Capital Gains & Losses
 underline(44, 8, gains_stream)

 printf "\t%27s => %14s\n", "Accounting Deferred Gains", print_cash(- accounting_gains) > gains_stream
 printf "\t%27s => %14s\n", "Taxable Deferred Gains",
                            print_cash(- gains) > gains_stream
 printf "\t%27s => %14s\n", "Taxable Deferred Losses",
                            print_cash(losses) > gains_stream

 printf "\nAfter Application of Any Losses\n" > gains_stream

 # Get the deferred taxable gains
 get_taxable_gains(now, gains_stream, gains, losses, DEFERRED_GAINS)

  # All done
  underline(43, 8, gains_stream)
  print "\n" > gains_stream

} # End of deferred gains



# Print out operating statement
function print_operating_statement(now, past, is_detailed,     write_stream,
                                                               benefits, losses,
                                                               gains, market_gains,
                                                               more_past, label, x) {

  # Set arguments
  more_past = last_year(past)
  is_detailed = ("" == is_detailed) ? 1 : 2

  # The gains_stream is the pipe to write the schedule out to
  if (report_operating)
    write_stream = EOFY
  else
    write_stream = "/dev/null"

  printf "\n%s\n", Journal_Title > write_stream
  if (is_detailed)
    printf "Detailed Operating Statement\n" > write_stream
  else
    printf "Operating Statement\n" > write_stream
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > write_stream
  underline(81, 0, write_stream)
  printf "%53s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC) > write_stream
  printf "%53s %26s\n", "$", "$" > write_stream

  # We start with the investment income
  label = sprintf("Income\nInvestment Income\n")

  # Exclude contributions
  label = print_account_class(write_stream, label, "block_class", "INCOME", "INCOME.CONTRIBUTION", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Obtain the income per year
  benefits[now]  = - (get_cost("*INCOME", now) - (x = get_cost("*INCOME", past)))
  benefits[past] = - (x - get_cost("*INCOME", more_past))

  # Now the Contributions
  label = sprintf("\nContributions\n")
  print_account_class(write_stream, label, "select_class", "INCOME.CONTRIBUTION", "", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Print a running total
  print_line(past, write_stream)

  # Print grand total income
  printf "\t%22s %23s", "Total Income", print_cash(benefits[now]) > write_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > write_stream
  else
    printf "\n" > write_stream
  print_line(past, write_stream)

  # the unrealized gains
  label = sprintf("\n\nInvestment Gains\nUnrealized Gains in Market Value\n")
  print_account_class(write_stream, label, "select_class", "ASSET.CAPITAL", "", "get_unrealized_gains", now, past, past, more_past, is_detailed, -1) # Block Depreciating Assets

  # Obtain the market gains per year
  market_gains[now]  = - (get_cost(MARKET_CHANGES, now) - (x = get_cost(MARKET_CHANGES, past)))
  market_gains[past] = - (x - get_cost(MARKET_CHANGES, more_past))

  # Print the total unrealized gains
  print_line(past, write_stream)

  # Print any unrealized gains
  if (above_zero(market_gains[now]) || above_zero(market_gains[past])) {
    printf "\t%22s %23s", "Total Market Gains", print_cash(yield_positive(market_gains[now], "")) > write_stream
    if (past)
      printf " %26s\n", print_cash(yield_positive(market_gains[past], "")) > write_stream
    else
      printf "\n" > write_stream
    benefits[now]  += yield_positive(market_gains[now], 0)
    benefits[past] += yield_positive(market_gains[past], 0)
  }

  # Print a grand total
  print_line(past, write_stream)

  # Print grand total income
  printf "\t%22s %23s", "Total of All Income", print_cash(benefits[now]) > write_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > write_stream
  else
    printf "\n" > write_stream
  print_line(past, write_stream)
  printf "\n" > write_stream

  # Now print expenses... (exclude tax payments)
  label = sprintf("Expenses\nGeneral Expenses\n")
  label = print_account_class(write_stream, label, "block_class", "EXPENSE", "EXPENSE.UNREALIZED", "get_cost", now, past, past, more_past, is_detailed)

  # Need to correct for market gains captured as expenses
  losses[now]  = market_gains[now]  + get_cost("*EXPENSE", now) - (x = get_cost("*EXPENSE", past))
  losses[past] = market_gains[past] + x - get_cost("*EXPENSE", more_past)

  # Print a total
  print_line(past, write_stream)

  # Print grand total income
  printf "\t%22s %23s", "Total Expenses", print_cash(losses[now]) > write_stream
  if (past)
    printf " %26s\n", print_cash(losses[past]) > write_stream
  else
    printf "\n" > write_stream
  print_line(past, write_stream)
  printf "\n\n" > write_stream

  # Print any unrealized losses
  if (below_zero(market_gains[now]) || below_zero(market_gains[past])) {
    printf "\t%22s %23s", "Total Market Losses", print_cash(yield_negative(market_gains[now], "")) > write_stream
    if (past)
      printf " %26s\n", print_cash(yield_negative(market_gains[past], "")) > write_stream
    else
      printf "\n" > write_stream
    losses[now]  -= yield_negative(market_gains[now], 0)
    losses[past] -= yield_negative(market_gains[past], 0)
  }

  # Print a total
  print_line(past, write_stream)

  # Print grand total expenses
  printf "\t%22s %23s", "Total of All Expenses", print_cash(losses[now]) > write_stream
  if (past)
    printf " %26s\n", print_cash(losses[past]) > write_stream
  else
    printf "\n" > write_stream
  print_line(past, write_stream)
  printf "\n\n" > write_stream

  # Print Before Tax benefits
  benefits[now]  -= losses[now]
  benefits[past] -= losses[past]
  printf "\t%27s %18s", "Benefits Accrued Before Tax", print_cash(benefits[now]) > write_stream
  if (past)
    printf " %26s\n", print_cash(benefits[past]) > write_stream
  else
    printf "\n" > write_stream
  print_line(past, write_stream)
  printf "\n\n" > write_stream

  # If detailed print tax credits
  label = sprintf("Appendix\n\nTax Offsets\n")
  label = print_account_class(write_stream, label, "select_class", "SPECIAL.OFFSET", "", "get_cost", now, past, past, more_past, is_detailed, -1) > write_stream

  # Print a nice line
  if (!label) {
    print_line(past, write_stream)
    x = get_cost("*SPECIAL.OFFSET", past)
    printf "\t%24s%22s %26s\n\n", "Total Tax Offsets",
              print_cash(x - get_cost("*SPECIAL.OFFSET", now)),
              print_cash(get_cost("*SPECIAL.OFFSET", more_past) - x) > write_stream
  }

  printf "\n\n\n" > write_stream

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
  if (report_balance)
    reports_stream = ("" == EOFY) ? "/dev/null" : EOFY
  else
    return

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

  # Term assets are current if they mature within one year
  current_assets[now]  = get_cost("*ASSET.CURRENT", now)
  current_assets[past] = get_cost("*ASSET.CURRENT", past)

  # Print a nice line
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Current Assets",
          print_cash(current_assets[now]), print_cash(current_assets[past]) > reports_stream

  # Now the non-current assets
  label = sprintf("Non-Current Assets\n")
  label = print_account_class(reports_stream, label, "block_class", "ASSET", "ASSET.CURRENT", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Here we need to adjust for accounting gains & losses
  assets[now]  =  get_cost("*ASSET", now)  - get_cost("*INCOME.GAINS.REALIZED", now)  - get_cost("*EXPENSE.LOSSES.REALIZED", now)  - get_cost(MARKET_CHANGES, now)
  assets[past] =  get_cost("*ASSET", past) - get_cost("*INCOME.GAINS.REALIZED", past) - get_cost("*EXPENSE.LOSSES.REALIZED", past) - get_cost(MARKET_CHANGES, past)

  # Print a nice line
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Non–Current Assets", print_cash(assets[now] - current_assets[now]), print_cash(assets[past] - current_assets[past]) > reports_stream

  # Print Total Assets
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Assets", print_cash(assets[now]), print_cash(assets[past]) > reports_stream

  # Treat tax payments/refunds as liabilities
  label = sprintf("Tax Liabilities\n")
  label = print_account_class(reports_stream, label, "select_class", "LIABILITY.TAX", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # We start with the current liabilities
  label = sprintf("Current Liabilities\n")
  label = print_account_class(reports_stream, label, "select_class", "LIABILITY.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)
  current_liabilities[now]   = -(get_cost("*LIABILITY.CURRENT", now ) + get_cost("*LIABILITY.TAX", now))
  current_liabilities[past]  = -(get_cost("*LIABILITY.CURRENT", past) + get_cost("*LIABILITY.TAX", past))

  # Print a nice line
  if (!label) {
    underline(73, 8, reports_stream)
    printf "\t%24s%21s %26s\n\n", "Total Current Liabilities",
              print_cash(current_liabilities[now]), print_cash(current_liabilities[past]) > reports_stream
  }

  # Need non-current Liabilities
  label = sprintf("Non-Current Liabilities\n")

  # Now the remaining non current liabilities
  class_list["LIABILITY.CURRENT"] = TRUE
  class_list["LIABILITY.MEMBER"] = TRUE
  class_list["LIABILITY.TAX"] = TRUE
  label = print_account_class(reports_stream, label, "block_class_list", "LIABILITY", class_list, "get_cost", now, Epoch, past, Epoch, is_detailed, -1, 2)
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
function print_market_gains(now, past, is_detailed,    gains_stream) {
  # Show current gains/losses
   # The gains_stream is the pipe to write the schedule out to
   if (report_market)
     gains_stream = ("" == EOFY) ? "/dev/null" : EOFY
   else
     return

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
      printf "Allocate Cost Element II\n%16s\n\tDate => %s\n", get_short_name(a), get_date(now) > "/dev/stderr"
@endif # LOG

      # Get each parcel
      for (p = 0; p < Number_Parcels[a]; p ++) {
        # Is this parcel purchased yet?
        if (Held_From[a][p] > now)
          break # All done
        if (is_unsold(a, p, now)) {
          # Debugging
@ifeq LOG allocate_second_element_costs
          printf "\tAdjusted Cost[%s] => %s\n", I, print_cash(get_parcel_element(a, p, I, now)) > "/dev/stderr"
          printf "\tAdjusted Cost[%s] => %s\n", II, print_cash(get_parcel_element(a, p, II, now)) > "/dev/stderr"
@endif # LOG

          # Get the second element of the cost
          second_element = get_parcel_element(a, p, II, now)
          if (!near_zero(second_element)) {
            # The Second Element Cost is applied to the First Element
            adjust_parcel_cost(a, p, now,   second_element,  I, FALSE)
            adjust_parcel_cost(a, p, now, - second_element, II, FALSE)
          }

@ifeq LOG allocate_second_element_costs
          printf "\t\tApply 2nd Element Cost => %s\n", second_element > "/dev/stderr"
          printf "\t\tAfter Application\n" > "/dev/stderr"
          printf "\t\tParcel            => %d\n", p > "/dev/stderr"
          printf "\t\tAdjusted Cost[%s] => %s\n", I, print_cash(get_parcel_element(a, p, I, now)) > "/dev/stderr"
          printf "\t\tParcel Cost       => %11.2f\n", get_parcel_cost(a, p, now) > "/dev/stderr"
@endif # LOG
        } # End of if unsold parcel
      } # End of each parcel

@ifeq LOG allocate_second_element_costs
      printf "%s: %s New Reduced Cost[%s] => %11.2f\n", "allocate_second_element_costs", get_short_name(a), get_date(now), get_reduced_cost(a, now) > "/dev/stderr"
@endif # LOG
    } # End of each fixed asset a
}


# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      reports_stream, a, p, open_key, close_key, parcel_depreciation, account_depreciation, open_cost, total_depreciaiton, sum_open,
                                                                  sale_depreciation, sale_appreciation, sum_adjusted) {

  # The reports_stream is the pipe to write the schedule out to
  if (report_fixed)
    reports_stream = ("" == EOFY) ? "/dev/null" : EOFY
  else
    return

  # Return if nothing to do
  if ("/dev/null" == reports_stream)
    return

  is_detailed = ("" == is_detailed) ? FALSE : is_detailed
  total_depreciation = ""

  # Print out the assets in alphabetical order
  for (a in Leaf)
    if (is_fixed(a) && (is_open(a, now) || is_open(a, past))) {

      if ("" == total_depreciation) {
        printf "\n" > reports_stream
        print Journal_Title > reports_stream
        printf "Depreciation Schedule for the Period [%11s, %11s]\n", get_date(past), get_date(now) > reports_stream
        total_depreciation = 0 # Total value summed here
      }

      # The opening value of an asset with multiple parcels cannot be tied to a single time
      account_depreciation = sum_open = 0

      # Get each parcel
      printf "%10s %15s ", Depreciation_Method[Method_Name[a]], get_short_name(a) > reports_stream
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

        # Record detailed statement
        # Is this a named parcel?
        if (is_detailed) {
          if (keys_in(Parcel_Tag, a, p))
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
        underline(198, 8, reports_stream)
        printf "%26s ", get_short_name(a) > reports_stream
      }

      # Depreciation is the sum of the tax adjustments
      # When was this asset opened?
      open_key = Held_From[a][0] # First parcel opened here
      if (open_key < past)
        open_key = past # This must be less than now for this asset to be open and considered
      if (is_closed(a, now))
        close_key = just_before(held_to(a, now))
      else
        close_key = just_before(now)

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
  if (!near_zero(sale_depreciation))
    printf "\n\tDepreciation from Sales => %14s\n", print_cash(sale_depreciation) > reports_stream
  if (!near_zero(sale_appreciation))
    printf "\n\tAppreciation from Sales => %14s\n", print_cash(-sale_appreciation) > reports_stream
  total_depreciation += sale_depreciation + sale_appreciation

  # Print a nice line
  if (!near_zero(total_depreciation)) {
    underline(198, 8, reports_stream)
    printf "\tPeriod Depreciation     => %14s\n", print_cash(total_depreciation) > reports_stream
  }
} # End of print depreciating holdings

#
#
## Dividend Qualification Function
##
## Compute whether dividends are qualified or not
function print_dividend_qualification(now, past, is_detailed,

                                         dividend_stream,
                                         a, underlying_asset, credit_account,
                                         qualifying_date,
                                         qualified_units, total_units, qualified_fraction, q,
                                         qualified_payment,
                                         key, next_key, payment,
                                         print_header) {

  ## Output Stream => Dividend_Report
  if (report_dividend)
    dividend_stream = ("" == EOFY) ? "/dev/null" : EOFY
  else
    dividend_stream = "/dev/null"

  # For each dividend in the previous financial
  print Journal_Title > dividend_stream
  if (is_detailed)
    printf "Detailed Dividend Qualification Report\n" > dividend_stream
  else
    printf "Dividend Qualification Report\n" > dividend_stream
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > dividend_stream

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
          printf "\n%12s\n", "Dividends" > dividend_stream
          printf "%12s %13s %14s %14s %14s %14s %13s\n",
                  "Asset", "Ex-Div Date", "Eligible Units", "Payment Date", "Qualified", "% Qualified", "Payment"  > dividend_stream
          underline(95, 6, dividend_stream)
          print_header = FALSE
        }

        # The current asset
        assert(a in Underlying_Asset, "Can't find underlying asset for %s" Leaf[a]) > dividend_stream
        underlying_asset = Underlying_Asset[a]

        # We will need the next key
        next_key = get_previous_transaction(a, key)

        # Short cut directly to the value of the dividend payment
        payment = - get_delta_cost(a, key)

        # The qualifying date is one day before the ex-dividend date
        qualifying_date = just_after(yesterday(get_exdividend_date(underlying_asset, key), HOUR))

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > 0, sprintf("%s: %s <%s>",  Leaf[a], Read_Date_Error, get_date(key)))

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
                get_date(key), qualified_units, 100.0 * qualified_fraction, print_cash(payment) > dividend_stream

        # Sum qualified payment
        qualified_payment += qualified_fraction * payment

        # Make the appropriate changes for the current tax jurisdiction
        @Dividend_Qualification_Function(a, underlying_asset, key, 1.0 - qualified_fraction)

        # Get the next key
        key = next_key
      } # End of while each key in the window
    } # End of if a dividend

    # summary
    underline(95, 6, dividend_stream)
    payment = get_cost("*INCOME.DIVIDEND", past) + get_cost("*INCOME.DISTRIBUTION.CLOSE", past) - \
              get_cost("*INCOME.DIVIDEND", now) - get_cost("*INCOME.DISTRIBUTION.CLOSE", now)
    printf "%*s%*s %14s %*.2f %*s\n\n", 6, "", 16, "Qualified Dividends", print_cash(qualified_payment),
      43, 100.0 * (qualified_payment / payment), 16, print_cash(payment) > dividend_stream

} # End of function print_dividend_qualification

## Helper functions


# This is the newer more compact form of this function
# But it is still carrying a lot of remnant complications
# Replace class_name, blocked_class with a
# selector function
function print_account_class(stream, heading, selector, class_name, blocked_class, income_function, now, now_past, past, past_past, print_all, sign,
  subclass, last_subclass,
  x, account_income, account_sum, did_print) {

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
    if (print_all > 1) {
      if (past)
        underline(73, 8, stream)
      else
        underline(47, 8, stream)
    }
    if (print_all)
      print_subclass_sum(subclass, account_sum[now], account_sum[past], stream)
  }

  # return heading
  return heading
}

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

# Shared code for applying losses to taxable gains
function get_taxable_gains(now, gains_stream,
                           long_gains, long_losses, tax_long,
                           short_gains, short_losses, tax_short,
                           losses) {

  # This function computes the taxable gains
  # It works for partioned long & short gains
  # And also for deferred gains when all such gains are long
  losses = ternary(losses, losses, 0)

  # Summarize starting point
  underline(44, 8, gains_stream)
  printf "\nAfter Application of Any Losses\n" > gains_stream

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  # Australian scheme & US Scheme are same
  # once short & long losses are disregarded
  long_gains  += long_losses # Net long term gains / losses
  short_gains += short_losses # Net short term losses / gains
  if (!below_zero(losses + short_gains + long_gains)) {
    # More carried losses generated
    losses += short_gains + long_gains

    # Record the details of short term & long term losses
    short_losses = ternary(above_zero(short_gains), short_gains, 0)
    long_losses  = ternary(above_zero(long_gains),  long_gains, 0)

    # Zero negligible losses
    if (near_zero(losses))
      losses = 0

    printf "\n\tOverall Capital Loss\n" > gains_stream
    if (above_zero(losses))
      printf "\t%27s => %14s\n", "Capital Losses", print_cash(losses) > gains_stream
    if (below_zero(short_gains))
      printf "\t%27s => %14s\n", "Short Gains", print_cash(- short_gains) > gains_stream
    else if (above_zero(short_losses))
      printf "\t%27s => %14s\n", "Short Losses", print_cash(short_losses) > gains_stream

    if (below_zero(long_gains))
      printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > gains_stream
    else if (above_zero(long_losses))
      printf "\t%27s => %14s\n", "Long Losses", print_cash(long_losses) > gains_stream

    # Zero the gains
    short_gains = long_gains = 0
  } else if (!below_zero(losses + short_gains)) {
    # No overall losses, only long gains left
    losses += short_gains
    long_gains += losses

    # There could be a short term loss in this scenario
    short_losses = ternary(above_zero(short_gains), short_gains, 0)

    # But not a long term loss
    losses = short_gains = long_losses = 0

    printf "\n\tOnly Long Gains\n" > gains_stream
    printf "\t%27s => %14s\n", "Long Gains", print_cash(- long_gains) > gains_stream
    if (!near_zero(short_losses))
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
    available_losses = write_back_losses(future_time, last_year(now), limit, available_losses, write_stream)

    # Any losses left?
    if (near_zero(available_losses))
      return 0

    # Record the process
    taxable_gains  = get_cost(TAXABLE_GAINS, now)
    printf "\t%27s => %13s\n", "Write Back", get_date(now) > write_stream
    printf "\t%27s => %14s\n", "Gains", print_cash(- taxable_gains) > write_stream

    # Get the gains
    if (below_zero(taxable_gains)) {
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
      if (below_zero(tax_refund))
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
