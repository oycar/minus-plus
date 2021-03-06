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
@ifeq LOG eofy_actions
  # EOFY actions
  printf "EOFY Actions\n\tDate => %s\n", get_date(now) > STDERR
@endif

  # past is one year earlier
  past = last_year(now)

  # Do we need to check for dividend qualification
  if (Qualification_Window)
    print_dividend_qualification(now, past, 1)

  # Print Imputation report
  @Imputation_Report_Function(now, past, Show_Extra)

  # Realized gains report
  get_capital_gains(now, past, Show_Extra)

  # The following actions are only needed once;
  # Market (Unrealized) gains and losses should
  # be calulated after capital gains because
  # distributed gains can change the market gains
  # Also unrealized gains are *tax adjusted*
  # becaause otherwise they would effect the deferred tax calculation
  if (process_records(now)) {
    # No this is the first time through
    # Depreciate everything - at EOFY
    depreciate_all(now)

    # Allocated can change in the tax computations
    if (ALLOCATED != ADJUSTMENTS)
      allocated_profits = get_cost(ALLOCATED, just_before(now))
  }

  # Print Market Gains
  get_market_gains(now, past, Show_Extra)

  # Print the depreciation schedule
  print_depreciating_holdings(just_after(now), past, Show_Extra)

  # We need to compute EOFY statements
  # First the operating statement (income & expenses)
  benefits = print_operating_statement(now, past, 1)

  # Compute the tax due
  @Income_Tax_Function(now, past, benefits)

  # A Super fund must allocate assets to members - this requires account balancing
  if (process_records(now))
    @Balance_Profits_Function(now, past, allocated_profits)

  # Print the balance sheet
  print_balance_sheet(now, past, 1)

  # Allocate second element costs associated with fixed assets - at SOFY
  if (process_records(now))
    allocate_second_element_costs(just_after(now))
}

# Default balance journal is a no-op
function balance_journal(now, past, initial_allocation) {
  return
}


# Gains Reconciliation
# Both Realized & Unrealized Gains
#
function print_gains(now, past, is_detailed, gains_type, reports_stream, sold_time, show_capital,

                                                            is_realized_flag, found_gains,
                                                            gains_event, current_price, p, a,
                                                            key,
                                                            description,
                                                            parcel_gains, adjusted_gains,
                                                            held_time,
                                                            label, no_header_printed,
                                                            to_label, proceeds_label,

                                                            asset_width,

                                                            long_gains, short_gains,
                                                            long_losses, short_losses,
                                                            gains,

                                                            units, units_sold,
                                                            cost, reduced_cost, adjusted_cost,
                                                            sum_cost, sum_reduced, sum_adjusted,
                                                            parcel_cost, parcel_proceeds,
                                                            proceeds,
                                                            sum_proceeds,
                                                            accounting_gains,

                                                            sum_long_gains, sum_long_losses,
                                                            sum_short_gains, sum_short_losses) {

  # # Print the gains report
  # Are we printing out a detailed schedule?
  is_detailed = ternary(is_detailed, is_detailed, FALSE)

  # A flag to discriminate realized and unrealized gains
  is_realized_flag = (gains_type ~ "Realized")

  # A default sold time = the Future
  sold_time = ternary(sold_time, sold_time, Future)

  # The proceeds label
  proceeds_label = ternary(is_realized_flag, "Proceeds", "  Value ")
  to_label       = ternary(is_realized_flag, "  To  ", "Latest")

  # No header printed
  no_header_printed = TRUE

  # Capital or forex gains?
  show_capital = ternary("" == show_capital, TRUE, show_capital)

  # Record accounting gains
  accounting_gains = 0
  sum_long_gains = sum_short_gains = sum_long_losses = sum_short_losses = 0 # Tax gains/losses summed here
  sum_cost = sum_reduced = sum_proceeds = sum_adjusted = 0

  # formatting
  asset_width = 15

  # For each asset sold in the current period
  for (a in Leaf) {
    # Can choose between capital or forex gains
    if ((show_capital && is_capital(a)) || (!show_capital && is_currency(a))) {
      # Are we looking for realized gains?
      if (is_realized_flag) {
        # If any of these are non-zero there was a gains event
        # These just apply for the whole account - not parcels
        if (a in Long_Gains)
          long_gains   = get_cost(Long_Gains[a], now)   - get_cost(Long_Gains[a], past)
        else
          long_gains = 0

        if (a in Long_Losses)
          long_losses  = get_cost(Long_Losses[a], now)  - get_cost(Long_Losses[a], past)
        else
          long_losses = 0

        if (a in Short_Gains)
          short_gains  = get_cost(Short_Gains[a], now)  - get_cost(Short_Gains[a], past)
        else
          short_gains = 0

        if (a in Short_Losses)
          short_losses = get_cost(Short_Losses[a], now) - get_cost(Short_Losses[a], past)
        else
          short_losses = 0
        key     = FALSE
      } else {
        long_gains = short_gains = long_losses = short_losses = 0

        # The price
        current_price = find_entry(Price[a], now)
      }

      # The last key found
      last_key = Epoch

      # Check each parcel
      if (is_realized_flag || is_open(a, now)) {
        # Where there any found gains - this can occur even if the asset is open
        # (when a capital return makes the cost base negative)
        found_gains = (not_zero(long_gains) || not_zero(long_losses) || not_zero(short_gains) || not_zero(short_losses))

        # We will examine each parcel
        gains_event = FALSE
        proceeds = cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
        units_sold = 0

        # Get each parcel
        for (p = 0; p < Number_Parcels[a]; p++ ) {
          if (Held_From[a][p] > now) # All further transactions occurred after (now) - parcels are sorted in order bought
            break # All done

          # Check if sold in the (past, now) window (capital gains)
          # or if it is unsold (deferred gains)
          if (found_gains || ((is_sold(a, p, now) == is_realized_flag) && is_unsold(a, p, past))) {

            if (!gains_event) {
              # Print the gains report
              if (no_header_printed) {
                print Journal_Title > reports_stream
                printf "%s Report for Period Ending %s\n", gains_type, get_date(yesterday(now))  > reports_stream
              }

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

            # Number of units
            units = Units_Held[a][p]

            # Parcel sale times are tricky
            if (is_sold(a, p, now) && is_unsold(a, p, past))
              key = Held_Until[a][p]
            else if (!is_realized_flag && !found_gains)
              key = sold_time
            else
              key = FALSE

            # The held time (will be wrong when key is FALSE)
            held_time = get_held_time(key, Held_From[a][p])

            # Total gains (accounting gains)
            if (is_sold(a, p, now)) {
              parcel_proceeds = get_parcel_proceeds(a, p)
              current_price = - parcel_proceeds / units
            } else
              parcel_proceeds = - current_price * Units_Held[a][p]
            gains = parcel_proceeds + sum_cost_elements(Accounting_Cost[a][p], now) # All elements

            # cash in and out
            parcel_cost     =   get_cash_in(a, p, now)
            cost           += parcel_cost
            if (key) {
              # Sold parcels
              proceeds      += parcel_proceeds
              reduced_cost  += get_parcel_cost(a, p, now)
              adjusted_cost += get_parcel_cost(a, p, now, TRUE)
              units_sold    += units
            }

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
                if (!found_gains)
                  long_losses += gains
              } else {
                description = "Short Losses"
                if (!found_gains)
                  short_losses += gains
              }
            } else {
              # Assume zero losses or gains
              parcel_gains = 0

              # Taxable gains
              description = "Zero Gains  "
            }

            # after application of tax adjustments
            # If there were losses then parcel_gains will be above zero
            adjusted_gains = gains - find_entry(Tax_Adjustments[a][p], now)
            if (below_zero(adjusted_gains)) {
              # Adjustments are negative and reduce taxable gains
              parcel_gains = adjusted_gains
              if (held_time >= CGT_PERIOD) {
                description = "Long Gains  "
                if (!found_gains)
                  long_gains += parcel_gains

              } else {
                description = "Short Gains "
                if (!found_gains)
                  short_gains += parcel_gains
              }
            }

            # Print out the parcel information
            if (is_detailed && key) {
              # If printing out in detail
              if (keys_in(Parcel_Tag, a, p)) {
                if (label) # Deal with label first
                  printf "%*s\n", asset_width + 1, label > reports_stream
                label = Parcel_Tag[a][p]
              }

              # Print line
              printf "%*s %*d %*.3f %*s %*s   %*s %*s %*s %*s %*s %*s %*s %*s %*s\n",
                   asset_width + 1, label,
                   7, p,
                   11, units,
                   15, print_cash(parcel_cost),
                   11, get_date(Held_From[a][p]),
                   10, get_date(key),
                   12, print_cash(current_price),
                   14, print_cash(- parcel_proceeds),
                   14, print_cash(get_parcel_cost(a, p, now)),
                   14, print_cash(get_parcel_cost(a, p, now, TRUE)),
                   14, print_cash(- gains),
                   14, description,
                   14, print_cash(- parcel_gains),
                   14, print_cash(parcel_cost / units, 4) > reports_stream

              # Clear label
              label = ""
            }

            # Save last key printed out
            if (key)
              last_key = max_value(last_key, key)
          }

          # Reset key
          key = FALSE
        } # End of each parcel p

        # Show any gains event
        if (gains_event) {
          if (greater_than(last_key, Epoch)) {
            if (is_detailed)
              # Detailed format
              underline(175 + asset_width, 6, reports_stream)
          } else
            last_key = Future

         # This is account level information
         printf "%*s %*.3f %*s %*s   %*s %*s %*s %*s %*s ",
                asset_width + 1, label,
                (11 + 8 * is_detailed), units_sold,
                15, print_cash(cost),
                7, get_date(Held_From[a][0]),
                10, get_date(last_key),
                12, print_cash(current_price),
                14, print_cash(- proceeds),
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
          key = ""
          for (key in Gains_Stack)
            break
          if (key) {
            printf "%*s %*s %*s",
              14, print_cash(- proceeds - reduced_cost),
              14, key,
              14, print_cash(- Gains_Stack[key]) > reports_stream
            delete Gains_Stack[key]
          } else
            printf "%14s", print_cash(- proceeds - reduced_cost) > reports_stream

          # Extra entries {
          for (key in Gains_Stack) {
            printf "\n%*s %14s", 143 + asset_width + 8 * is_detailed, key, print_cash(- Gains_Stack[key]) > reports_stream
            delete Gains_Stack[key]
          }

          # Grand totals
          sum_cost     += cost
          sum_proceeds += proceeds
          sum_reduced  += reduced_cost
          sum_adjusted += adjusted_cost
          sum_long_gains += long_gains
          sum_short_gains += short_gains
          sum_long_losses += long_losses
          sum_short_losses += short_losses

          printf "\n" > reports_stream
          if (is_detailed)
           printf "\n" > reports_stream

        } # End of gains event
      }
    } # End of if is capital
  } # End of each asset

  # Final lines
  if (!no_header_printed) {
    underline(166 + is_detailed * (9 + asset_width), 6, reports_stream)
    if (is_detailed)
      underline(166 + is_detailed * (9 + asset_width), 6, reports_stream)

    # Stack the gains & losses
    if (not_zero(sum_long_gains))
      Gains_Stack[Long_Gains_Key]   = sum_long_gains
    if (not_zero(sum_long_losses))
      Gains_Stack[Long_Losses_Key]  = sum_long_losses
    if (not_zero(sum_short_gains))
      Gains_Stack[Short_Gains_Key]  = sum_short_gains
    if (not_zero(sum_short_losses))
      Gains_Stack[Short_Losses_Key] = sum_short_losses

    # Print out a summary
    printf "%*s %*s %*s %*s %*s ",
          asset_width + 1, "Totals",
          (27 + 8 * is_detailed), print_cash(sum_cost),
          53, print_cash(- sum_proceeds),
          14, print_cash(sum_reduced),
          14, print_cash(sum_adjusted)  > reports_stream

    # Common entries
    for (key in Gains_Stack)
      break
    if (key) {
      printf "%*s %*s %*s",
        14, print_cash(- sum_proceeds - sum_reduced),
        14, key,
        14, print_cash(- Gains_Stack[key]) > reports_stream
      delete Gains_Stack[key]
    } else
      printf "%14s", print_cash(sum_proceeds - sum_reduced) > reports_stream

    # Extra entries {
    for (key in Gains_Stack) {
      printf "\n%*s %14s", 143 + asset_width + 8 * is_detailed, key, print_cash(- Gains_Stack[key]) > reports_stream
      delete Gains_Stack[key]
    }

    # If these are not realized gains
    if (!is_realized_flag) {
      printf "\n" > reports_stream
      underline(44, 8, reports_stream)
      printf "\t%27s => %14s\n", "Market Gains",
                                 print_cash(- sum_long_gains - sum_short_gains) > reports_stream
      printf "\t%27s => %14s\n", "Market Losses",
                                 print_cash(sum_long_losses + sum_short_losses) > reports_stream

      # Get the deferred taxable gains
      cost = apply_losses(now, reports_stream, "Deferred", sum_long_gains + sum_short_gains, sum_long_losses + sum_short_losses, "*ASSET", UNREALIZED)

      # Need to balance the market gains
      #cost = get_cost(UNREALIZED, now)
      #adjust_cost("*ASSET", get_cost(UNREALIZED, just_before(now)) - cost, now)

      # Only deferred gains count
      if (below_zero(cost))
         printf "\t%27s => %14s\n", "Deferred Gains", print_cash(- cost) > reports_stream

       # All done
       underline(44, 8, reports_stream)
    }
  }

  printf "\n\n" > reports_stream
  return accounting_gains
} # End of print gains

#
# Report on income gains - distributed directly or indirectly from managed funds
#
function print_income_gains(now, past, is_detailed, reports_stream,
                            a, gains, sum_gains,
                            underlying_asset,
                            no_header_printed,
                            asset_width,
                            class_list,
                            sign, label) {

  # A class list
  class_list["INCOME.GAINS.NET"] = TRUE
  class_list["INCOME.GAINS.LONG"] = TRUE
  class_list["INCOME.GAINS.SHORT"] = TRUE
  class_list["EXPENSE.LOSSES.LONG"] = TRUE
  class_list["EXPENSE.LOSSES.SHORT"] = TRUE

  # Are we printing out a detailed schedule?
  is_detailed = ternary(is_detailed, is_detailed, FALSE)
  asset_width = 15

  # Report on income gains
  # This should examine LONG & SHORT & LOSSES too
  sum_gains = 0
  no_header_printed = TRUE
  for (a in Leaf)
    for (c in class_list)
      if (select_class(a, c)) {
        # These are the income gains classes
        # Each account needs the income gains increased in proportion to its share of the total gains
        gains     = get_cost(a, now) - get_cost(a, past)

        # Skip negligible gains
        if (near_zero(gains))
          continue

        # Print a header when required
        if (no_header_printed) {
          # Print the gains report
          printf "\n%s\n", Journal_Title > reports_stream
          printf "Distributed Capital Gains Report for Period Ending %s\n\n", get_date(yesterday(now))  > reports_stream

          # The header
          if (is_detailed) {
            printf "%*s %*s\n",
                   asset_width, "Asset",
                   14, "Distributed" > reports_stream
            printf "%*s\n", 11 + asset_width, "Gains" > reports_stream
            underline(9 + asset_width, 6, reports_stream)
          } else {
            printf "%*s\n",
                 asset_width + 15, "Distributed" > reports_stream
            printf "%*s\n", 11 + asset_width, "Gains" > reports_stream
          }

          # print Name
          no_header_printed = FALSE
        }

        # Get underlying account
        if (a in Underlying_Asset)
          underlying_asset = Underlying_Asset[a]
        else
          underlying_asset = a

        # Print out the detailed information
        if (is_detailed) {
          # If printing out in detail
          printf "%*s %*s\n",
               asset_width + 1, Leaf[underlying_asset],
               11, print_cash(-gains) > reports_stream
        }

        # Sum the gains
        sum_gains += gains
      }

  # The summary
  if (!no_header_printed) {
    if (is_detailed)
      underline(9 + asset_width, 6, reports_stream)

    printf "%*s %*s\n",
       asset_width + 1, "Total",
       11, print_cash(-sum_gains) > reports_stream
    underline(9 + asset_width, 6, reports_stream)
    printf "\n" > reports_stream
  }

  # Clean up
  delete class_list

  # All done
  return
}

# Compute capital gains and losses
function get_capital_gains(now, past, is_detailed,

                                reports_stream,
                                accounting_gains, accounting_losses,
                                adjusted_gains,
                                distributed_gains,
                                income_long_gains, income_short_gains, income_net_gains,
                                expense_long_losses, expense_short_losses,
                                capital_long_gains, capital_short_gains,
                                capital_long_losses, capital_short_losses,
                                taxable_gains,
                                carried_losses,
                                losses, next_losses, key) {


    # The reports_stream is the pipe to write the schedule out to
    reports_stream = report_capital(eofy_stream(now))

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
    accounting_gains = get_cost("*INCOME.GAINS", just_before(now)) - get_cost("*INCOME.GAINS", past)

    # The realized capital losses
    accounting_losses = get_cost("*EXPENSE.LOSSES", now) - get_cost("*EXPENSE.LOSSES", past)

    # Print Accounting Capital Gains
    printf "\t%27s => %14s\n", "Accounting Capital Gains", print_cash(- accounting_gains) > reports_stream
    printf "\t%27s => %14s\n", "Accounting Capital Losses", print_cash(accounting_losses) > reports_stream

    # Now compute the total accounting gains
    accounting_losses += accounting_gains
    underline(44, 8, reports_stream)
    if (below_zero(accounting_losses))
      printf "\t%27s => %14s\n", "Net Accounting Gains ", print_cash(- accounting_losses) > reports_stream
    else if (above_zero(accounting_losses))
      printf "\t%27s => %14s\n", "Net Accounting Losses", print_cash(accounting_losses) > reports_stream
    else
      printf "\t%27s\n", "Zero Accounting Gains" > reports_stream

    # Carried capital losses from previous years (if any)
    carried_losses = carry_losses(Capital_Losses, past)

    # The taxable long & short gains
    # If there are other income gains (eg from distributions etc)
    # then the taxable gains will need adjustment
    income_net_gains   = get_cost("*INCOME.GAINS.NET", now)  - get_cost("*INCOME.GAINS.NET", past)
    income_long_gains  = get_cost("*INCOME.GAINS.LONG", now)  - get_cost("*INCOME.GAINS.LONG", past)
    income_short_gains = get_cost("*INCOME.GAINS.SHORT", now) - get_cost("*INCOME.GAINS.SHORT", past)

    # Simililarly for expenses
    expense_long_losses  = get_cost("*EXPENSE.LOSSES.LONG", now)  - get_cost("*EXPENSE.LOSSES.LONG", past)
    expense_short_losses = get_cost("*EXPENSE.LOSSES.SHORT", now) - get_cost("*EXPENSE.LOSSES.SHORT", past)

    # The long gains and losses first
    capital_long_gains  = get_cost(star(LONG_GAINS), just_before(now)) - get_cost(star(LONG_GAINS), past)
    capital_long_losses = get_cost(star(LONG_LOSSES), just_before(now)) - get_cost(star(LONG_LOSSES), past)

    # short gains & losses
    capital_short_gains   = get_cost(star(SHORT_GAINS), just_before(now)) - get_cost(star(SHORT_GAINS), past)
    capital_short_losses  = get_cost(star(SHORT_LOSSES), just_before(now)) - get_cost(star(SHORT_LOSSES), past)

    # The adjusted gains and losses
    printf "\n\tAdjusted Gains\n" > reports_stream
    printf "\n\t%27s => %14s\n",  "Long Adjusted Gains", print_cash(- capital_long_gains) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Adjusted Gains", print_cash(- capital_short_gains) > reports_stream

    # Finally the adjusted losses
    # Which are the same as the accounting losses
    printf "\t%27s => %14s\n", "Long Adjusted Losses", print_cash(capital_long_losses) > reports_stream
    printf "\t%27s => %14s\n\n", "Short Adjusted Losses", print_cash(capital_short_losses) > reports_stream

    # Report all distributed gains and losses
    # Take care that the adjustments are not applied more than once
    distributed_gains = FALSE
    if (not_zero(expense_short_losses)) {
      distributed_gains = TRUE
      printf "\t%27s => %14s\n", "Short Expense Losses", print_cash(expense_short_losses) > reports_stream
    }
    if (not_zero(expense_long_losses)) {
      distributed_gains = TRUE
      printf "\t%27s => %14s\n", "Long Expense Losses", print_cash(expense_long_losses) > reports_stream
    }
    if (not_zero(income_short_gains)) {
      distributed_gains = TRUE
      # These should not occur for Australian returns
      printf "\t%27s => %14s\n", "Short Income Gains", print_cash(- income_short_gains) > reports_stream
    }
    if (not_zero(income_long_gains)) {
      distributed_gains = TRUE
      printf "\t%27s => %14s\n", "Long Income Gains", print_cash(- income_long_gains) > reports_stream
    }
    if (not_zero(income_net_gains)) {
      distributed_gains = TRUE
      printf "\t%27s => %14s\n", "Long Income Net Gains", print_cash(- income_net_gains) > reports_stream
    }

    # Need to handle distributed income & expenses in a jurisdiction agnostic way
    if (distributed_gains) {
      # Net up long gains and losses
      income_long_gains += expense_long_losses

      # Net up short gains and losses
      income_short_gains += expense_short_losses

      # Need the overall adjusted gains/losses (including available carried losses)
      adjusted_gains = capital_long_gains  + capital_short_gains  +\
                       capital_long_losses + capital_short_losses +\
                       income_long_gains   + income_short_gains   +\
                       income_net_gains    + carried_losses


      # Compute extra taxable gains (if any)
      # Need the less (in magnitude) of the overall adjusted gain or net gain
      if (below_zero(adjusted_gains)) {
        printf "\t%27s => %14s\n", "Available Gains", print_cash(- adjusted_gains) > reports_stream
        # There are gains
        if (less_than(adjusted_gains, income_net_gains))
          # The gains are greater than the income net gains, so gross up the net gains
          taxable_gains = @Gross_Up_Gains_Function(now, past, income_net_gains, income_net_gains)
        else
          # The gains are less than the income gains, so gross up the adjusted gains
          taxable_gains = @Gross_Up_Gains_Function(now, past, adjusted_gains, income_net_gains)

        # Assume grossed up taxable gains are treated as long gains
        income_long_gains += taxable_gains
      }

      if (not_zero(taxable_gains))
        printf "\t%27s => %14s\n", "Grossed Up Long Gains", print_cash(- income_long_gains) > reports_stream
    }

    # Show total capital gains
    printf "\n\tCapital Gains Before Application of Losses or Discounts\n" > reports_stream
    printf "\n\t%27s => %14s\n",  "Total Capital Gains", print_cash(-(capital_long_gains + income_long_gains + capital_short_gains + income_short_gains)) > reports_stream

    # Apply long & short losses separately
    # This is not strictly necessary in all cases but useful
    # Save net gains or losses
    # What happens when you manipulate a parent account?
    adjusted_gains  = apply_losses(now, reports_stream, "Long",  capital_long_gains + income_long_gains,  capital_long_losses,  "*SPECIAL", star(LONG_GAINS),  star(LONG_LOSSES))
    adjusted_gains += apply_losses(now, reports_stream, "Short", capital_short_gains + income_short_gains, capital_short_losses, "*SPECIAL", star(SHORT_GAINS), star(SHORT_LOSSES))

    # Overall gains, losses and taxable gains
    underline(44, 8, reports_stream)
    if (below_zero(adjusted_gains))
      printf "\t%27s => %14s\n", "Net Adjusted Gains ", print_cash(- adjusted_gains) > reports_stream
    else if (above_zero(adjusted_gains))
      printf "\t%27s => %14s\n", "Net Adjusted Losses", print_cash(adjusted_gains) > reports_stream
    else
      printf "\t%27s\n", "Zero Adjusted Gains" > reports_stream
    if (above_zero(carried_losses)) {
      printf "\t%27s => %14s\n", "Losses Brought Forward", print_cash(carried_losses) > reports_stream

      accounting_gains = adjusted_gains + carried_losses
      if (below_zero(accounting_gains))
        printf "\t%27s => %14s\n", "Total Adjusted Gains ", print_cash(- accounting_gains) > reports_stream
      else if (above_zero(accounting_gains))
        printf "\t%27s => %14s\n", "Total Adjusted Losses", print_cash(accounting_gains) > reports_stream
      else
        printf "\t%27s\n", "Zero Adjusted Gains" > reports_stream
    }

    # Compute taxable gains
    taxable_gains = @Get_Taxable_Gains_Function(now, carried_losses) # FIXME need carry forward limit
    if (below_zero(taxable_gains))
      printf "\t%27s => %14s\n",   "Taxable Gains",  print_cash(- taxable_gains) > reports_stream

    # All done
    underline(44, 8, reports_stream)
    print "\n" > reports_stream

    # If the total capital losses are non zero at the EOFY they must be carried losses
    carried_losses = get_carried_losses(now, Capital_Losses, adjusted_gains, CARRY_FORWARD_CAPITAL_LIMIT, reports_stream)
    if (above_zero(carried_losses)) {
      printf "\t%27s => %14s\n", "Losses Carried Forward", print_cash(carried_losses) > reports_stream
    } else {
      assert(near_zero(carried_losses), sprintf("Cannot carry taxable capital gains forward [%s] Gains => %14s", get_date(past), print_cash(- carried_losses, 6)))
      carried_losses = 0
    }

    # Now the gains reports
    if (DEVNULL != reports_stream) {
      # Now the income gains report
      print_income_gains(now, past, is_detailed, reports_stream)

      # The Realized Capital Gains
      print_gains(now, past, is_detailed, "Realized Capital Gains", reports_stream)
      delete Gains_Stack

      # The Realized Foreign Exchange Gains/Losses
      print_gains(now, past, is_detailed, "Realized Foreign Exchange Gains", reports_stream, Future, FALSE)
      delete Gains_Stack
    }
}

# Report on the losses
function report_losses(now, losses_array, label, write_stream,
                       losses, next_losses, key) {

  # Get the losses
  losses = carry_losses(losses_array, now)
  if (above_zero(losses)) {
    printf "\n%s Report\n", label > write_stream
    printf "\t%14s  %14s\n", "Year", label  > write_stream
    underline(36, 8, write_stream)

    next_losses = losses
    for (key in losses_array[now]) {
      # The next losses
      next_losses = find_entry(losses_array[now], just_before(key))
      printf "\t%14s %14s\n", get_date(key, YEAR_FORMAT), print_cash(losses - next_losses)  > write_stream
      losses = next_losses
    }
    underline(36, 8, write_stream)

    # Inefficient
    losses = carry_losses(losses_array, now)
    printf "\t%14s %14s\n\n", "Total", print_cash(losses) > write_stream
  }

  # Return overall losses
  return losses
}

# Default gross up gains
function gross_up_gains_def(now, past, total_gains, long_gains, short_gains) {
  # Nothing to do by default
  # Gains are alreay computed
  return 0
}

# Shared code for applying losses to taxable gains
function apply_losses(now, reports_stream, label,
                           gains, losses, balancing_account, save_gains, save_losses, x) {
  # It works for partioned long & short gains

  # Summarize starting point
  underline(44, 8, reports_stream)
  printf "\nAfter Application of %s Losses\n", label > reports_stream

  # Apply the losses - most favourable order is to apply them to other gains first
  # A loss > 0
  # A gain < 0
  gains += losses # Net gains / losses

  # Carried losses generated
  if (!below_zero(gains)) {
    # No overall gains
    # There could be a loss in this scenario
    losses = yield_positive(gains, 0)

    # But not a gain
    gains = 0
  } else
    # No overall losses
    losses = 0

  # Save  gains
  # these could be deferred gains or taxable gains
  # Only needed on first pass
  if (save_gains) {
    if (process_records(now)) {
      x = get_cost(save_gains, now)
      adjust_cost(save_gains,          gains - x,  now)
      adjust_cost(balancing_account, -(gains - x), now)
    }
    printf "\t%27s => %14s\n", (label " Gains"), print_cash(- get_cost(save_gains, now)) > reports_stream
  }

  # Remaining options could only be for taxable gains
  if (save_losses) {
    if (process_records(now)) {
      x = get_cost(save_losses, now)
      adjust_cost(save_losses,         losses - x,  now)
      adjust_cost(balancing_account, -(losses - x), now)
    }
    printf "\t%27s => %14s\n", (label " Losses"), print_cash(get_cost(save_losses, now)) > reports_stream
  }

  # Cannot have both a net gain and a net loss simultaneously
  # So return whichever is appropriate
  if (near_zero(losses))
    # A (possibly zero) net gain
    return gains

  # Must be a net loss
  return losses
}

# Get the carried losses - limit how many years a loss can be carried forward
function get_carried_losses(now, losses_array, losses, limit, reports_stream,
                            past,
                            key) {

  # Is this already computed?
  if (process_records(now)) {

    #
    # losses_array[Now] => Total losses (and maybe gains) in year
    # They have a non-standard double dependence on time
    #
    reports_stream = ternary(reports_stream, reports_stream, DEVNULL)

    # Don't use losses prior to (now - limit) if limit is set
    if (limit)
      # Set the limiting time
      # The passed limit is in units of years
      limit = now - one_year(now, - limit)

    # The previous years losses
    past = find_key(losses_array, just_before(now))

    # If there are already earlier losses copy them
    if (past in losses_array) {
      for (key in losses_array[past])
        # Copy the most recent set of losses
        losses_array[now][key] = losses_array[past][key]

      # If limit is set remove any keys older than limit in latest version
      if (limit && now in losses_array) {
        key = remove_keys(losses_array[now], limit)

        # Record this
        if (key && not_zero(key)) {
          printf "\t%27s => %14s\n", "Losses Prior To",  get_date(limit) > reports_stream
          printf "\t%27s => %14s\n", "Losses Cancelled",  print_cash(key) > reports_stream
        }
      }
    }

    # If there are gains cancel the earliest losses
    if (below_zero(losses)) {
      # These are actually gains
      # The oldest losses are cancelled first
      # Remember gains are negative
      # There may be no losses available
      if (now in losses_array)
        remove_entries(losses_array[now], -losses)
    } else if (above_zero(losses)) {
      # OK no old losses will be extinguished
      # A new loss is added
      if (now in losses_array)
        sum_entry(losses_array[now], losses, now)
      else
        losses_array[now][now] = losses
    }
  }

  # Return the carried losses
  return carry_losses(losses_array, now)
}


# Print out operating statement
function print_operating_statement(now, past, is_detailed,     reports_stream,
                                                               benefits, losses,
                                                               gains, market_gains,
                                                               more_past, label, x) {

  # Set arguments
  more_past = last_year(past)
  is_detailed = ("" == is_detailed) ? 1 : 2

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = report_operating(eofy_stream(now))

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
  market_gains[now]  = - (get_cost(UNREALIZED, now) - (x = get_cost(UNREALIZED, past)))
  market_gains[past] = - (x - get_cost(UNREALIZED, more_past))

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
  reports_stream = report_balance(eofy_stream(now))

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
  label = print_account_class(reports_stream, label, "select_class", "ASSET.CURRENCY", "", "get_cost", now, Epoch, past, Epoch, is_detailed)
  label = print_account_class(reports_stream, label, "current_class", "ASSET.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Term assets are current if they mature within one year
  current_assets[now]  = get_cost("*ASSET.CURRENT", now) + get_cost("*ASSET.CURRENCY", now) + account_sum[now]
  current_assets[past] = get_cost("*ASSET.CURRENT", past) + get_cost("*ASSET.CURRENCY", past) + account_sum[past]

  # Print a nice line
  underline(73, 8, reports_stream)
  printf "\t%24s %21s %26s\n\n", "Total Current Assets",
          print_cash(current_assets[now]), print_cash(current_assets[past]) > reports_stream

  # Now the non-current assets
  label = sprintf("Non Current Assets\n")
  class_list["ASSET.TERM"] = TRUE
  class_list["ASSET.CURRENCY"] = TRUE
  class_list["ASSET.CURRENT"] = TRUE

  label = print_account_class(reports_stream, label, "block_class_list", "ASSET", class_list, "get_value", now, Epoch, past, Epoch, is_detailed)
  label = print_account_class(reports_stream, label, "not_current_class", "ASSET.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed)
  delete class_list

  # Here we need to adjust for accounting gains & losses
  assets[now]  =  get_cost("*ASSET", now)
  assets[past] =  get_cost("*ASSET", past)

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
  label = print_account_class(reports_stream, label, "select_class", "LIABILITY.CURRENCY", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)
  label = print_account_class(reports_stream, label, "current_class", "LIABILITY.TERM", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  current_liabilities[now]   = -(get_cost("*LIABILITY.CURRENT", now ) + get_cost("*LIABILITY.CURRENCY", now ) + get_cost("*LIABILITY.TAX", now) + account_sum[now])
  current_liabilities[past]  = -(get_cost("*LIABILITY.CURRENT", past) + get_cost("*LIABILITY.CURRENCY", past ) + get_cost("*LIABILITY.TAX", past) + account_sum[past])

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
  class_list["LIABILITY.CURRENCY"] = TRUE
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
function get_market_gains(now, past, is_detailed,    reports_stream) {
  # Show current gains/losses
   # The reports_stream is the pipe to write the schedule out to
   reports_stream = report_market(eofy_stream(now))

   # First print the gains out in detail
   print_gains(now, past, is_detailed, "Market Gains", reports_stream, now)
   delete Gains_Stack
}

# Compute annual depreciation
function depreciate_all(now,      a, current_depreciation) {
  # Depreciation is Cost Element I
  Cost_Element = I

  # Depreciate all open fixed assets
  for (a in Leaf)
    if (is_fixed(a) && is_open(a, now)) {
      # Depreciate
      current_depreciation = depreciate_now(a, now)
      update_cost(a, - current_depreciation, now)

      # Balance accounts
      adjust_cost(DEPRECIATION, current_depreciation, now)
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
          printf "\tAdjusted Cost[%s] => %s\n", I, print_cash(get_element_cost(a, p, I, now)) > STDERR
          printf "\tAdjusted Cost[%s] => %s\n", II, print_cash(get_element_cost(a, p, II, now)) > STDERR
@endif # LOG

          # Get the second element of the cost
          second_element = get_element_cost(a, p, II, now)
          if (!near_zero(second_element)) {
            # The Second Element Cost is applied to the First Element
            adjust_parcel_cost(a, p, now,   second_element,  I, FALSE)
            adjust_parcel_cost(a, p, now, - second_element, II, FALSE)
          }

@ifeq LOG allocate_second_element_costs
          printf "\t\tApply 2nd Element Cost => %s\n", second_element > STDERR
          printf "\t\tAfter Application\n" > STDERR
          printf "\t\tParcel            => %d\n", p > STDERR
          printf "\t\tElement Cost[%s]  => %s\n", I, print_cash(get_element_cost(a, p, I, now)) > STDERR
          printf "\t\tParcel Cost       => %11.2f\n", get_parcel_cost(a, p, now) > STDERR
@endif # LOG
        } # End of if unsold parcel
      } # End of each parcel

@ifeq LOG allocate_second_element_costs
      printf "%s: %s New Reduced Cost[%s] => %11.2f\n", "allocate_second_element_costs", get_short_name(a), get_date(now), get_cost(a, now) > STDERR
@endif # LOG
    } # End of each fixed asset a
}


# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      reports_stream, a, p, open_key, close_key, parcel_depreciation, account_depreciation,
                                                                  open_cost, total_depreciaiton, sum_open,
                                                                  second_element, sum_second_element,
                                                                  sale_depreciation, sale_appreciation) {

  # The reports_stream is the pipe to write the schedule out to
  reports_stream = report_depreciation(eofy_stream(just_before(now)))
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
      account_depreciation = sum_open = sum_second_element = 0

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
        parcel_depreciation = find_entry(Tax_Adjustments[a][p], open_key) - find_entry(Tax_Adjustments[a][p], close_key)

        #  Just track the total depreciation
        account_depreciation   += parcel_depreciation

        # Save second element cost
        sum_second_element += (second_element = get_element_cost(a, p, II, just_before(close_key)))

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
                    print_cash(second_element),
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
        print_cash(sum_second_element),
        print_cash(get_cost(a, close_key)),
        print_cash(account_depreciation) > reports_stream

      # Track total depreciation too
      total_depreciation += account_depreciation
    } # End of a depreciating asset

  # Is there any depreciation/appreciation due to the sale of depreciating assets?
  sale_appreciation = get_cost(SOLD_APPRECIATION, now) - get_cost(SOLD_APPRECIATION, past)
  sale_depreciation = get_cost(SOLD_DEPRECIATION, now) - get_cost(SOLD_DEPRECIATION, past)
  if (!near_zero(sale_depreciation))
    printf  "\n%24s %*s\n", "Depreciation from Sales", 105 + 8 * is_detailed, print_cash(sale_depreciation) > reports_stream
  if (!near_zero(sale_appreciation))
    printf  "\n%24s %*s\n", "Appreciation from Sales", 105 + 8 * is_detailed, print_cash(-sale_appreciation) > reports_stream
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
  reports_stream = report_dividend(eofy_stream(now))

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
        if (qualifying_date = get_exdividend_date(underlying_asset, key))
          qualifying_date = just_after(yesterday(qualifying_date, HOUR))

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > DATE_ERROR, sprintf("%s: %s <%s>",  Leaf[a], Read_Date_Error, get_date(key)))

        # Catch  the case that no qualification date was recorded
        if (qualifying_date) {
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
        } else {
          # No qualification date
          qualified_units    = total_units = get_units(underlying_asset, now)
          qualified_fraction = 1.0

          printf "%13s %12s %12.3f %16s %14.3f %12.2f %16s\n", Leaf[underlying_asset], "No Date", total_units,
                  get_date(key), total_units, 100.0, print_cash(payment) > reports_stream
        }

        # Sum qualified payment
        qualified_payment += qualified_fraction * payment

        # Make the appropriate changes for the current tax jurisdiction
        if (process_records(now))
          @Dividend_Qualification_Function(a, key, 1.0 - qualified_fraction)

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
    if (@selector(x, class_name, blocked_class, now)) {

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

      # This is more complicated still because
      # an account can be closed due to a code change;
      # if this has occurred before "now" then do not record these gains/losses
      if (past) {
        if ("get_unrealized_gains" == income_function && (is_closed(x, past) || account_closed(x, now)))
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
function select_class(a, class_name, blocked_class, now) {
  return is_class(a, class_name)
}


# Select multiple classes...
function select_class_list(a, class_list, blocked_class, now,     x) {
  #  class list is actually an array of allowed classes
  for (x in class_list)
    if (is_class(a, x)) # selected!
      return TRUE

  # Not selected
  return FALSE
}

# Include class blocking
function block_class(a, class_name, blocked_class, now) {
  if (is_class(a, blocked_class))
    return FALSE # Blocked!

  # Just the simple case
  return is_class(a, class_name)
}

# Block multiple classes...
function block_class_list(a, class_name, blocked_class_list, now,     x) {
  # blocked class might actually be an array of blocked classes
  for (x in blocked_class_list)
    if (is_class(a, x)) # Blocked!
      return FALSE

  # Just the simple case
  return is_class(a, class_name)
}

# Special purpose filter for current accounts
function current_class(a, class_name, blocked_class, now,    maturity) {
  # Is this the right class
  if (is_class(a, class_name)) {
    # Get current maturity date
    if (a in Maturity_Date) {
      maturity = find_entry(Maturity_Date[a], now)
      if (maturity > next_year(now))
        return FALSE
    }

    return TRUE
  }

  return FALSE
  #return is_class(a, class_name) && !(a in Maturity_Date)
}

# And its pigeon pair
function not_current_class(a, class_name, blocked_class, now,    maturity) {
  # Is this the right class
  if (is_class(a, class_name)) {
    # Get current maturity date
    if (a in Maturity_Date) {
      maturity = find_entry(Maturity_Date[a], now)
      if (maturity > next_year(now))
        return TRUE
    }
  }

  return FALSE
  #return is_class(a, class_name) && (a in Maturity_Date)
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
    taxable_gains  = get_cost(star(TAXABLE_GAINS), now)
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
      tax_refund = get_band_tax(now, "Tax", find_entry(Taxable_Income, now) + gains_written_back) - find_entry(Income_Tax, now)

      # Update taxable gains
      adjust_cost(WRITTEN_BACK, - gains_written_back, now)
      adjust_cost("*SPECIAL",   + gains_written_back, now)


      # The refund is a simple refundable offset at the future time
      if (below_zero(tax_refund))
        sum_entry(Refundable_Offsets, tax_refund, future_time)

      # Record This
      printf "\t%27s => %14s\n", "Rewritten Gains", print_cash(- taxable_gains) > reports_stream
      printf "\t%27s => %14s\n", "New Available Losses", print_cash(available_losses) > reports_stream
      printf "\t%27s => %14s\n", "Tax Refund", print_cash(- tax_refund) > reports_stream
      printf "\t%27s => %14s\n", "Total Refundable Offset", print_cash(find_entry(Refundable_Offsets, future_time)) > reports_stream
    }
  }

  # Finish Up
  printf "\n\n" > reports_stream

  # Finished
  return available_losses
}
