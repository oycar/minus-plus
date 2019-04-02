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
    EOFY = Reports

  # past is referred to now
  past = last_year(now)

@ifeq LOG eofy_actions
  # EOFY actions
  printf "EOFY Actions\n\tDate => %s\n", get_date(now, LONG_FORMAT) > "/dev/stderr"
@endif

  # Depreciate everything - at EOFY
  depreciate_all(yesterday(now, HOUR))

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
  if ("/dev/null" != EOFY)
    print_realized_gains(now, past, Show_Extra)

  # Next print out a Capital Gains schedule
  get_capital_gains(now, past)

  # We need to compute EOFY statements
  # First the operating statement (income & expenses)
  benefits = print_operating_statement(now, past, 1)

  # Compute the tax due
  @Income_Tax_Function(now, past, benefits)

  # A Super fund must allocate assets to members - this requires account balancing
  @Balance_Profits_Function(now, past, allocated_profits)

  # The balance sheet and holdings statement set no global values
  if ("/dev/null" != EOFY) {
    # Print the balance sheet
    print_balance_sheet(now, past, 1)

    # Print Holdings
    print_holdings(now)

    # Print the depreciation schedule
    #  Reset the time to July 01 at the standard hour
    print_depreciating_holdings(today(now, HOUR), today(past, HOUR), Show_Extra)
  }
}

# Default balance journal is a no-op
function balance_journal(now, past, initial_allocation) {
  return
}

# Realized Gains Reconciliation
# Only needed when printing out information
#
function print_realized_gains(now, past, is_detailed,       cgt_schedule, gains_event, current_price, p, a,
                                                            description,
                                                            parcel_gains, parcel_adjustments, held_time,
                                                            disc_gains, short_gains, tax_losses, gains,
                                                            units, units_sold,
                                                            reduced_cost, adjusted_cost,
                                                            parcel_cost, parcel_proceeds,
                                                            sum_cost, sum_proceeds) {
  # Are we printing out a detailed schedule?
  is_detailed = ("" == is_detailed) ? FALSE : is_detailed

  # The cgt_schedule is the pipe to write the schedule out to
  cgt_schedule = ("" == EOFY) ? "/dev/null" : EOFY

  # Print the capital gains report
  print Journal_Title > cgt_schedule
  printf "Realized Gains Report for Period Ending %s\n", get_date(yesterday(now))  > cgt_schedule

  # For each asset
  for (a in Leaf)
    if (is_capital(a)) {
      gains_event = FALSE
      sum_proceeds = sum_cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
      disc_gains = short_gains = tax_losses = 0 # Tax gains/losses summed here

      units_sold = 0
      current_price = find_entry(Price[a], now)

      # Need to select parcels by sold date
      for (p = 0; p < Number_Parcels[a]; p++ ) {
        if (Held_From[a][p] > now) # All further transactions occured after (now) - parcels are sorted in order bought
          break # All done

        # Check if sold in the (past, now) window
        if (is_between(Held_Until[a][p], past, now)) {
          if (!gains_event) {
            gains_event = TRUE
            printf "%s\n", get_short_name(a) > cgt_schedule
            if (is_detailed)
              printf "\tSold Parcels\n" > cgt_schedule
          }

          # Keep track
          units = Units_Held[a][p]
          units_sold += units
          held_time = get_held_time(Held_Until[a][p], Held_From[a][p])
          reduced_cost += get_parcel_cost(a, p, now)
          adjusted_cost += get_parcel_cost(a, p, now, TRUE)

          # cash in and out
          parcel_cost     =   get_cash_in(a, p, now)
          parcel_proceeds = - get_cash_out(a, p, now)
          sum_cost += parcel_cost
          sum_proceeds += parcel_proceeds

          # Total gains (accounting gains)
          gains = sum_cost_elements(Accounting_Cost[a][p], now)

          # We want taxable gains
          parcel_adjustments = sum_cost_elements(Tax_Adjustments[a][p], now)
          if (gains < parcel_adjustments - Epsilon) {
            parcel_gains = gains - parcel_adjustments
            # Sold - capital gain
            if (held_time >= ONE_YEAR) {
              description = "Long Gain    "
              disc_gains += parcel_gains
            } else {
              description = "Short Gain   "
              short_gains += parcel_gains
            }
          } else {
            parcel_gains = 0
            tax_losses += gains
            description = "Taxable Loss "
          }

          # Print out the parcel GAINS
          if (is_detailed) {
            # Complicated logic for layout
            # Top line has accounting losses or gains if they differ from taxable losses or gains
            printf "\t%6d Units => %10.3f Held => [%11s, %11s] Cost => %14s Paid  => %14s Reduced => %14s",
              p, units, get_date(Held_From[a][p]), get_date(Held_From[a][p] + held_time), print_cash(parcel_cost),
                 print_cash(parcel_proceeds), print_cash(get_parcel_cost(a, p, now)) > cgt_schedule
            if (near_zero(parcel_adjustments))
              printf " %15s => %14s Per Unit => %14s\n", description, print_cash(parcel_gains < 0 ? - parcel_gains : gains), print_cash(get_parcel_cost(a, p, now) / units, 4) > cgt_schedule
            else {
              # The accounting gain/loss is simple
              printf " %15s => %14s\n", "Accounting Gain", print_cash(- gains) > cgt_schedule

              # Next line has adjusted cost and tax gains or losses
              if (near_zero(parcel_gains) && gains < 0) {
                  # Zero tax gains
                  description = "Zero Gain    "
                  gains = 0
              }
              printf "\t%117s => %13s %15s => %14s\n", "Adjusted", print_cash(get_parcel_cost(a, p, now, TRUE)), description,
                                                                   print_cash(parcel_gains < 0 ? - parcel_gains : gains) > cgt_schedule
            }
          } # If printing out in detail
        }
      } # End of each parcel p

      # Show any gains event
      if (gains_event) {
        if (is_detailed)
          print_underline(167, 0, cgt_schedule)
        print_gains_summary(units_sold, sum_cost, sum_proceeds, adjusted_cost, reduced_cost, 35 * is_detailed, disc_gains, short_gains, tax_losses, cgt_schedule)
      }
    } # End of print current holdings
} # End of print realized gains

# Print out operating statement
function print_operating_statement(now, past, is_detailed,     benefits, losses,
                                                               gains, market_gains,
                                                               more_past, label, x) {
  # Set arguments
  more_past = last_year(past)
  is_detailed = ("" == is_detailed) ? 1 : 2

  print Journal_Title > EOFY
  if (is_detailed)
    printf "Detailed Operating Statement\n" > EOFY
  else
    printf "Operating Statement\n" > EOFY
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > EOFY

  print_underline(80, 1, EOFY)
  printf "\n\n" > EOFY

  # We start with the investment income
  label = sprintf("Income\nInvestment Income %33s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC))

  # Exclude contributions
  label = print_account_class(label, "block_class", "INCOME", "INCOME.CONTRIBUTION", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Obtain the income per year
  benefits[now]  = - (get_cost("*INCOME", now) - (x = get_cost("*INCOME", past)))
  benefits[past] = - (x - get_cost("*INCOME", more_past))

  # Now the Contributions
  label = sprintf("\nContributions\n")
  print_account_class(label, "select_class", "INCOME.CONTRIBUTION", "", "get_cost", now, past, past, more_past, is_detailed, -1)

  # Print a running total
  print_line(past, EOFY)

  # Print grand total income
  printf "\t%22s %23s", "Total Income", print_cash(benefits[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(benefits[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }

  # the unrealized gains
  label = sprintf("\n\nInvestment Gains\nUnrealized Gains in Market Value\n")
  print_account_class(label, "select_class", "ASSET.CAPITAL", "", "get_unrealized_gains", now, past, past, more_past, is_detailed, -1) # Block Depreciating Assets

  # Obtain the market gains per year
  market_gains[now]  = - (get_cost(MARKET_CHANGES, now) - (x = get_cost(MARKET_CHANGES, past)))
  market_gains[past] = - (x - get_cost(MARKET_CHANGES, more_past))

  # Print the total unrealized gains
  print_line(past, EOFY)

  # Print any unrealized gains
  if (above_zero(market_gains[now]) || above_zero(market_gains[past])) {
    printf "\t%22s %23s", "Total Market Gains", print_cash(yield_positive(market_gains[now], "")) > EOFY
    if (past)
      printf " %26s\n", print_cash(yield_positive(market_gains[past], "")) > EOFY
    else
      printf "\n" > EOFY
    benefits[now]  += yield_positive(market_gains[now], 0)
    benefits[past] += yield_positive(market_gains[past], 0)
  }

  # Print a grand total
  print_line(past, EOFY)

  # Print grand total income
  printf "\t%22s %23s", "Total of All Income", print_cash(benefits[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(benefits[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n" > EOFY

  # Now print expenses... (exclude tax payments)
  label = sprintf("Expenses\nGeneral Expenses\n")
  label = print_account_class(label, "block_class", "EXPENSE", "EXPENSE.UNREALIZED", "get_cost", now, past, past, more_past, is_detailed) > EOFY

  # Need to correct for market gains captured as expenses
  losses[now]  = market_gains[now]  + get_cost("*EXPENSE", now) - (x = get_cost("*EXPENSE", past))
  losses[past] = market_gains[past] + x - get_cost("*EXPENSE", more_past)

  # Print a total
  print_line(past, EOFY)

  # Print grand total income
  printf "\t%22s %23s", "Total Expenses", print_cash(losses[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(losses[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n\n" > EOFY

  # Print any unrealized losses
  if (below_zero(market_gains[now]) || below_zero(market_gains[past])) {
    printf "\t%22s %23s", "Total Market Losses", print_cash(yield_negative(market_gains[now], "")) > EOFY
    if (past)
      printf " %26s\n", print_cash(yield_negative(market_gains[past], "")) > EOFY
    else
      printf "\n" > EOFY
    losses[now]  -= yield_negative(market_gains[now], 0)
    losses[past] -= yield_negative(market_gains[past], 0)
  }
  # Print a total
  print_line(past, EOFY)

  # Print grand total expenses
  printf "\t%22s %23s", "Total of All Expenses", print_cash(losses[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(losses[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n\n" > EOFY

  # Print Before Tax benefits
  benefits[now]  -= losses[now]
  benefits[past] -= losses[past]
  printf "\t%27s %18s", "Benefits Accrued Before Tax", print_cash(benefits[now]) > EOFY
  if (past) {
    printf " %26s\n", print_cash(benefits[past]) > EOFY
    print_underline(72, 0, EOFY)
  } else {
    printf "\n" > EOFY
    print_underline(46, 0, EOFY)
  }
  printf "\n\n" > EOFY

  # If detailed print tax credits
  label = sprintf("Appendix\n\nTax Offsets\n")
  label = print_account_class(label, "select_class", "SPECIAL.OFFSET", "", "get_cost", now, past, past, more_past, is_detailed, -1) > EOFY

  # Print a nice line
  if (!label) {
    print_underline(72, 0, EOFY)
    x = get_cost("*SPECIAL.OFFSET", past)
    printf "\t%24s%22s %26s\n\n", "Total Tax Offsets",
              print_cash(x - get_cost("*SPECIAL.OFFSET", now)),
              print_cash(get_cost("*SPECIAL.OFFSET", more_past) - x) > EOFY
  }

  printf "\n\n\n" > EOFY

  # Only need current benefits
  x = benefits[now]
  delete losses
  delete market_gains
  delete benefits

  # Return value
  return x
}

# Print balance sheet
function print_balance_sheet(now, past, is_detailed,
                             current_assets, assets, current_liabilities, liabilities, equity, label, class_list) {
  # By default not detailed
  is_detailed = ("" == is_detailed) ? 1 : 2

  # This is an extended version of check balance but also
  # draws on information from the operating statement and tax statement
  # Let's go
  printf "\n%s\n", Journal_Title > EOFY
  printf "Statement of Financial Position\n" > EOFY
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > EOFY
  print_underline(80, 1, EOFY)
  printf "%53s %26s\n", strftime("%Y", now, UTC), strftime("%Y", past, UTC) > EOFY
  printf "%53s %26s\n", "$", "$" > EOFY

  # We start with the current assets (cash)
  label = sprintf("Current Assets\n")
  label = print_account_class(label, "select_class", "ASSET.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Term assets are current if they mature within one year
  current_assets[now]  = get_cost("*ASSET.CURRENT", now)
  current_assets[past] = get_cost("*ASSET.CURRENT", past)

  # Print a nice line
  print_underline(72, 0, EOFY)
  printf "\t%24s %21s %26s\n\n", "Total Current Assets",
          print_cash(current_assets[now]), print_cash(current_assets[past]) > EOFY

  # Now the non-current assets
  label = sprintf("Non-Current Assets\n")
  label = print_account_class(label, "block_class", "ASSET", "ASSET.CURRENT", "get_cost", now, Epoch, past, Epoch, is_detailed)

  # Here we need to adjust for accounting gains & losses
  assets[now]  =  get_cost("*ASSET", now)  - get_cost("*INCOME.GAINS.REALIZED", now)  - get_cost("*EXPENSE.LOSSES.REALIZED", now)  - get_cost(MARKET_CHANGES, now)
  assets[past] =  get_cost("*ASSET", past) - get_cost("*INCOME.GAINS.REALIZED", past) - get_cost("*EXPENSE.LOSSES.REALIZED", past) - get_cost(MARKET_CHANGES, past)
  #assets[now]  =  get_cost("*ASSET", now)  #- get_cost("*INCOME.GAINS.REALIZED", now)  - get_cost("*EXPENSE.GAINS.REALIZED", now)  - get_cost(MARKET_CHANGES, now)
  #assets[past] =  get_cost("*ASSET", past) #- get_cost("*INCOME.GAINS.REALIZED", past) - get_cost("*EXPENSE.LOSSES.REALIZED", past) - get_cost(MARKET_CHANGES, past)

  # Print a nice line
  print_underline(72, 0, EOFY)
  printf "\t%24s %21s %26s\n\n", "Total Non–Current Assets", print_cash(assets[now] - current_assets[now]), print_cash(assets[past] - current_assets[past]) > EOFY

  # Print Total Assets
  print_underline(72, 0, EOFY)
  printf "\t%24s %21s %26s\n\n", "Total Assets", print_cash(assets[now]), print_cash(assets[past]) > EOFY

  # Treat tax payments/refunds as liabilities
  label = sprintf("Tax Liabilities\n")
  label = print_account_class(label, "select_class", "LIABILITY.TAX", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # We start with the current liabilities
  label = sprintf("Current Liabilities\n")
  label = print_account_class(label, "select_class", "LIABILITY.CURRENT", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)
  current_liabilities[now]   = -(get_cost("*LIABILITY.CURRENT", now ) + get_cost("*LIABILITY.TAX", now))
  current_liabilities[past]  = -(get_cost("*LIABILITY.CURRENT", past) + get_cost("*LIABILITY.TAX", past))

  # Print a nice line
  if (!label) {
    print_underline(72, 0, EOFY)
    printf "\t%24s%21s %26s\n\n", "Total Current Liabilities",
              print_cash(current_liabilities[now]), print_cash(current_liabilities[past]) > EOFY
  }

  # Need non-current Liabilities
  label = sprintf("Non-Current Liabilities\n")

  # Now the remaining non current liabilities
  class_list["LIABILITY.CURRENT"] = TRUE
  class_list["LIABILITY.MEMBER"] = TRUE
  class_list["LIABILITY.TAX"] = TRUE
  label = print_account_class(label, "block_class_list", "LIABILITY", class_list, "get_cost", now, Epoch, past, Epoch, is_detailed, -1, 2)
  liabilities[now]  = - get_cost("*LIABILITY", now)
  liabilities[past] = - get_cost("*LIABILITY", past)
  delete class_list

  # Print Member Liabilities
  label = print_account_class(label, "select_class", "LIABILITY.MEMBER", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # Print a nice line
  if (!label) {
    print_underline(72, 0, EOFY)
    printf "\t%27s %18s %26s\n", "Total Long Term Liabilities",
      print_cash(liabilities[now] - current_liabilities[now]), print_cash(liabilities[past] - current_liabilities[past]) > EOFY
  }

  print_underline(72, 0, EOFY)
  print_underline(72, 0, EOFY)
  printf "\t%27s %18s %26s\n\n", "Total Liabilities",
    print_cash(liabilities[now]), print_cash(liabilities[past]) > EOFY

  # Now find total Equity
  label = sprintf("Share Equity\n")
  label = print_account_class(label, "select_class", "EQUITY", "", "get_cost", now, Epoch, past, Epoch, is_detailed, -1)

  # the equity
  equity[now]  = - get_cost("*EQUITY", now)
  equity[past] = - get_cost("*EQUITY", past)

  # Print a nice line
  if (!label) {
    print_underline(72, 0, EOFY)
    printf "\t%24s %21s %26s\n\n", "Total Equity",
                print_cash(equity[now]), print_cash(equity[past]) > EOFY
  }

  # Print Accumulated Profits (INCOME - EXPENSES) == (ASSETS - LIABILITY - EQUITY)
  print_underline(72, 0, EOFY)
  printf "\t%24s %21s %26s\n", "Accumulated Profits",
    print_cash(assets[now] - liabilities[now] - equity[now]), print_cash(assets[past] - liabilities[past] - equity[past]) > EOFY
  print_underline(72, 0, EOFY)

  # Tidy up
  delete assets
  delete liabilities
  delete equity
}

# Print the holdings at time now
function print_holdings(now,         p, a, c, sum_value, reduced_cost, adjustments, current_price, tagged, prec) {
  printf "\n%s\n", Journal_Title > EOFY
  printf "Statement of Holdings at Date => %s\n", get_date(now) > EOFY

  # Total disposals are added to closed holdings
  reduced_cost = 0
  printf "Current Holdings\n" > EOFY
  for (a in Leaf)
    if (is_cash(a)){
      c = get_cost(a, now)
      if (!near_zero(c)) {
        printf "\tCash      %16s => %14s\n", get_short_name(a), print_cash(c) > EOFY
        reduced_cost += c
      }
    }

  # Print a nice line
  print_underline(42, 0, EOFY)
  printf "\tCurrent Assets Value => %14s\n\n", print_cash(reduced_cost) > EOFY

  # Print the current holdings
  reduced_cost = "" # Total cost summed here
  for (a in Leaf)
    if (is_capital(a) && is_open(a, now)) {
      if ("" == reduced_cost) {
        printf "Other Holdings\n" > EOFY
        sum_value = reduced_cost = 0 # Total cost summed here
      }
      current_price = find_entry(Price[a], now)
      reduced_cost  += get_cost(a, now) # The reduced cost or true cost
      sum_value += get_value(a, now)

      # HOLDING NAME
      printf "%15s", get_short_name(a) > EOFY

      # Need to select parcels by sold date
      if (Show_Extra) {
        # Some parcels are still held
        tagged = FALSE
        for (p = 0; p < Number_Parcels[a]; p++)
          if (Held_From[a][p] <= now) {
            if (is_unsold(a, p, now)) {
              if (!tagged) {
                printf "\n\tHeld Parcels\n" > EOFY
                tagged = TRUE
              }
              print_parcel_gain(a, p, now, current_price, EOFY)
            }
          } else
            break
      } # end of if showing detailed

      # Summary
      if (Show_Extra) {
        print_underline(168, 0, EOFY)
        printf "\t" > EOFY
      }

      # get precision for price
      if (current_price >= 1.0e04)
        prec = 2
      else
        prec = 4

      # The rest of the line
      printf "      Units => %10.3f Price => %12s [%11s] Cost => %14s Value => %14s Reduced => %14s Unrealized Gain => %14s\n",
                    get_units(a, now), print_cash(current_price, prec), get_date(find_key(Price[a], now)), print_cash(get_cost_element(a, I, now)),
                    print_cash(get_value(a, now)),
                    print_cash(get_cost(a, now)), print_cash(get_value(a, now) - get_cost(a, now))  > EOFY

      # Check the adjusted cost
      adjustments = get_Tax_Adjustments(a, now)
      if (!near_zero(adjustments))
        printf "\t%123s => %14s\n", "Adjusted", print_cash(get_cost(a, now) - adjustments) > EOFY

      if (Show_Extra)
        printf "\n" > EOFY
    } # End of print current holdings

  # Print a nice line
  if ("" != reduced_cost) {
    print_underline(174, 0, EOFY)
    printf "\t%97s => %14s Reduced => %14s Unrealized Gain => %14s\n", "Total Value",
      print_cash(sum_value), print_cash(reduced_cost), print_cash(sum_value - reduced_cost) > EOFY
  }

  printf "\n" > EOFY
}

# This function is is for slightly different times than the other EOFY actions
function print_depreciating_holdings(now, past, is_detailed,      a, p, open_key, close_key, delta, open_cost, sum_dep, sum_open,
                                                                  sale_depreciation, sale_appreciation, sum_adjusted, sum_proceeds) {
  is_detailed = ("" == is_detailed) ? FALSE : is_detailed
  sum_dep = ""

  # Print out the assets in alphabetical order
  for (a in Leaf)
    if (is_fixed(a) && (is_open(a, now) || is_open(a, past))) {
      if ("" == sum_dep) {
        printf "\n" > EOFY
        print Journal_Title > EOFY
        printf "Depreciation Schedule for the Period [%11s, %11s]\n", get_date(past), get_date(now) > EOFY
        sum_dep = 0 # Total value summed here
      }

      # The opening value of an asset with multiple parcels cannot be tied to a single time
      sum_open = 0

      # Were any parcels sold in the last period?
      sum_proceeds = 0

      # Get each parcel
      printf "%10s %15s ", Depreciation_Method[Method_Name[a]], get_short_name(a) > EOFY
      for (p = 0; p < Number_Parcels[a]; p ++) {
        # When was this parcel  opened?
        open_key = Held_From[a][p] # First parcel opened here
        if (open_key < past)
          open_key = past # This must be earlier than now for this asset to be open and considered

        # Is there is a problem if item is sold exactly at same time as depreciation occurs...
        if (is_sold(a, p, now)) {
          close_key = Held_Until[a][p]

          # Was it sold during the period being considered?
          if (close_key > past)
            # Short cut macro
            sum_proceeds += get_parcel_proceeds(a, p)
        } else
          close_key = just_before(now)

        # parcel open cost
        open_cost = get_parcel_cost(a, p, open_key)
        sum_open += open_cost

        # Record detailed statement
        # Is this a named parcel?
        if (is_detailed) {
          if (keys_in(Parcel_Tag, a, p))
            printf "\n%20s %5d ", Parcel_Tag[a][p], p > EOFY
          else
            printf "\n%26d ", p > EOFY

          # Depreciation is the sum of the I tax adjustments
          delta = get_parcel_tax_adjustment(a, p, I, open_key) - get_parcel_tax_adjustment(a, p, I, close_key)
          printf "[%11s, %11s] Opening => %14s Closing => %14s Second Element => %14s Adjusted => %14s Depreciation => %14s",
                    get_date(open_key), get_date(close_key), print_cash(open_cost),
                    print_cash(open_cost - delta),
                    print_cash(get_parcel_cost(a, p, close_key) + delta - open_cost),
                    print_cash(get_parcel_cost(a, p, close_key)),
                    print_cash(delta) > EOFY
        } # End of is_detailed
      } # End of each parcel

      # Clean up output
      if (is_detailed) {
        printf "\n" > EOFY
        print_underline(186, 0, EOFY)
        printf "%26s ", get_short_name(a) > EOFY
      }

      # Depreciation is the sum of the tax adjustments
      # When was this asset opened?
      open_key = Held_From[a][0] # First parcel opened here
      if (open_key < past)
        open_key = past # This must be less than now for this asset to be open and considered
      if (is_closed(a, now))
        close_key = held_to(a, now)
      else
        close_key = just_before(now)

      # If close_key was set so was open_key
      # Total depreciation in this period
      delta = get_tax_element_adjustment(a, I, open_key) - get_tax_element_adjustment(a, I, close_key)
      sum_dep   += delta

      # For depreciating assets depreciation corresponds to the tax adjustments
      # Period depreciation is the difference in the tax adjustments
      printf "[%11s, %11s] Opening => %14s Closing => %14s Second Element => %14s Adjusted => %14s Depreciation => %14s\n",
        get_date(open_key), get_date(close_key), print_cash(sum_open),
        print_cash(sum_open - delta),
        print_cash(get_cost(a, close_key) + delta - sum_open - sum_proceeds),
        print_cash(get_cost(a, close_key)),
        print_cash(delta) > EOFY

    } # End of a depreciating asset

  # Is there any depreciation/appreciation due to the sale of depreciating assets?
  sale_appreciation = get_cost(SOLD_APPRECIATION, now) - get_cost(SOLD_APPRECIATION, past)
  sale_depreciation = get_cost(SOLD_DEPRECIATION, now) - get_cost(SOLD_DEPRECIATION, past)
  if (!near_zero(sale_depreciation))
    printf "\n\tDepreciation from Sales => %14s\n", print_cash(sale_depreciation) > EOFY
  if (!near_zero(sale_appreciation))
    printf "\n\tAppreciation from Sales => %14s\n", print_cash(-sale_appreciation) > EOFY
  sum_dep += sale_depreciation + sale_appreciation

  # Print a nice line
  if (!near_zero(sum_dep)) {
    print_underline(186, 0, EOFY)
    printf "\tPeriod Depreciation     => %14s\n", print_cash(sum_dep) > EOFY
    printf "\tOpening Cost            => %14s\n", print_cash(get_cost("*ASSET.FIXED", close_key) + sum_dep) > EOFY
    printf "\tClosing Adjusted Cost   => %14s\n\n", print_cash(get_cost("*ASSET.FIXED", close_key)) > EOFY
  }
} # End of print depreciating holdings

#
#
## Dividend Qualification Function
##
## Compute whether dividends are qualified or not
function print_dividend_qualification(now, past, is_detailed,

                                         a, underlying_asset, credit_account,
                                         qualifying_date,
                                         qualified_units, total_units, qualified_fraction, q,
                                         key, next_key, payment,
                                         print_header) {


  # For each dividend in the previous financial
  print Journal_Title > EOFY
  if (is_detailed)
    printf "Detailed Dividend Qualification Report\n" > EOFY
  else
    printf " Dividend Qualification Report\n" > EOFY
  printf "For the period starting %s and ending %s\n", get_date(past), get_date(yesterday(now)) > EOFY

  # A header
  print_header = TRUE

  # Get each dividend/distribution
  # Start with dividends - this could be abstracted out later to include distributions
  # First key
  for (a in Leaf)
    if (is_class(a, "INCOME.DIVIDEND") || is_class(a, "INCOME.DISTRIBUTION.CLOSELY_HELD")) { # Only closely held distributions are considered

      # Get each payment in the target period
      key = get_latest_transaction(a, now)

      while (key > past) {
        # A heading
        if (print_header) {
          printf "\n\n%22s\n", "Dividends" > EOFY
          print_header = FALSE
        }

        # The current asset
        assert(a in Underlying_Asset, "Can't find underlying asset for %s" Leaf[a]) > EOFY
        underlying_asset = Underlying_Asset[a]

        # We will need the next key
        next_key = get_previous_transaction(a, key)

        # Short cut directly to the value of the dividend payment
        payment = - get_delta_cost(a, key)

        # The qualifying date is one day before the ex-dividend date
        qualifying_date = get_exdividend_date(underlying_asset, key) - ONE_DAY

        # If this date is valid now compute the proportion of the dividend is qualified
        assert(qualifying_date > 0, sprintf("Can't compute qualified dividends without an ex-dividend date for the <%s> payment on <%s>",  Leaf[a], get_date(key)))

        # These are the units that were qualified on the qualifying date
        qualified_units = get_qualified_units(underlying_asset, qualifying_date)

        # Now get the total units
        total_units = get_units(underlying_asset, qualifying_date)

        # If not all units are qualified need to check the second half of the Qualification Window
        if (!near_zero(total_units - qualified_units)) {
          q = maximum_entry(Qualified_Units[underlying_asset], qualifying_date, qualifying_date + 0.5 * Qualification_Window)
          qualified_units = max_value(q, qualified_units)
          qualified_fraction = qualified_units / get_units(underlying_asset, qualifying_date)

          # Should never be greater than unity
          assert(!above_zero(qualified_fraction - 1.0), sprintf("Qualified Units[%s] => %.3f > Units held on qualification date <%s>",
            underlying_asset, qualified_units, get_units(underlying_asset, qualifying_date)))
        } else
          qualified_fraction = 1.0

        # The output - show ex-dividend date not qualifying date
        printf "\t%22s %11s %11s %14s %7.5f\n", Leaf[underlying_asset], get_date(key), get_date(qualifying_date + ONE_DAY), print_cash(payment), qualified_fraction > EOFY

        # Make the appropriate changes for the current tax jurisdiction
        @Dividend_Qualification_Function(a, underlying_asset, key, 1.0 - qualified_fraction)

        # Get the next key
        key = next_key
      } # End of while each key in the window
    } # End of if a dividend
} # End of function print_dividend_qualification

# Module for printing out parcel capital gains
function print_parcel_gain(a, p, now, current_price, cgt_schedule,
                             paid, description, units, tax_gains, gains, held_time, parcel_adjustments) {
  # Default values
  cgt_schedule = ("" == cgt_schedule) ? "/dev/stdout" : cgt_schedule
  current_price = ("" == current_price) ? find_entry(Price[a], now) : current_price

  # Keep track
  gains = sum_cost_elements(Accounting_Cost[a][p], now)

  # Held
  held_time = get_held_time(now, Held_From[a][p])
  units = Units_Held[a][p]
  paid  = current_price * units
  gains -= paid
  parcel_adjustments = sum_cost_elements(Tax_Adjustments[a][p], now)

  # We want taxable gains
  if (gains < parcel_adjustments - Epsilon) { # This is a capital gain cash_in < cash_out
    tax_gains = gains - parcel_adjustments
    if (held_time  >= CGT_PERIOD)
      description = "Long Gain    "
    else
      description = "Short Gain   "
  } else {
    tax_gains = 0
    if (gains > Epsilon)
      description = "Taxable Loss "
    else
      description = "Zero Gain    "
  }

  # Complicated logic for layout
  # Top line has accounting losses or gains if they differ from taxable losses or gains
  printf "\t%6d Units => %10.3f Held => [%11s, %11s] Cost => %14s Value => %14s Reduced => %14s",
    p, units, get_date(Held_From[a][p]), get_date(Held_From[a][p] + held_time), print_cash(get_cash_in(a, p, now)),
       print_cash(paid), print_cash(get_parcel_cost(a, p, now)) > cgt_schedule
  if (near_zero(parcel_adjustments)) {
    printf " %15s => %14s Per Unit => %14s", description, print_cash(tax_gains < 0 ? -tax_gains : gains), print_cash(get_parcel_cost(a, p, now) / units, 4) > cgt_schedule
    if (keys_in(Parcel_Tag, a, p))
      printf "%20s\n", Parcel_Tag[a][p], p > EOFY
    else
      printf "\n" > EOFY
  } else {
    # Next line has adjusted cost and tax gains or losses
    if (near_zero(tax_gains) && gains < 0) {
      # Zero tax gains
      description = "Zero Gain    "
      gains = 0
    }

    # The accounting gain/loss is simple
    printf " %15s => %14s\n", "Accounting Gain", print_cash(- gains) > cgt_schedule
    printf "\t%117s => %13s %15s => %14s\n", "Adjusted", print_cash(get_parcel_cost(a, p, now, TRUE)), description, print_cash(tax_gains < 0 ? -tax_gains : gains) > cgt_schedule
  }
}

## Helper functions


# This is the newer more compact form of this function
# But it is still carrying a lot of remnant complications
# Replace class_name, blocked_class with a
# selector function
function print_account_class(heading, selector, class_name, blocked_class, income_function, now, now_past, past, past_past, print_all, sign,
  subclass, last_subclass,
  x, account_income, account_sum, did_print) {

  # Bail out when nothing to print
  if ("/dev/null" == EOFY)
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
          if (print_all > 1) {
            if (past)
              print_underline(72, 0, EOFY)
            else
              print_underline(46, 0, EOFY)
          }
          if (print_all)
            print_subclass_sum(last_subclass, account_sum[now],
                                              account_sum[past])
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
            printf heading > EOFY

            # Only print out once
            heading = ""
          }
          if (print_all > 1)
            printf "%22s\n", subclass > EOFY

          # The heading has been printed out
          did_print = 1
        }

        if (print_all > 1) {
          printf "\t%24s %21s", get_short_name(x), print_cash(account_income[now]) > EOFY
          if (past)
            printf " %26s", print_cash(account_income[past]) > EOFY
          printf "\n"> EOFY
        }
      }
    }
  } # End of each Leaf

  # Print a nice line (careful here when no subclasses found!)
  if (did_print) {
    if (print_all > 1) {
      if (past)
        print_underline(72, 0, EOFY)
      else
        print_underline(46, 0, EOFY)
    }
    if (print_all)
      print_subclass_sum(subclass, account_sum[now], account_sum[past])
  }

  # return heading
  return heading
}

function print_subclass_sum(name, sum_now, sum_past) {
  printf "\t%24s %21s", substr(name, 1, 1) tolower(substr(name, 2)), print_cash(sum_now) > EOFY
  if (sum_past)
    printf " %26s\n", print_cash(sum_past) > EOFY
  else
    printf "\n" > EOFY
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

# A not so simple "print_line"
# FIXME
#
function print_line(long, stream) {
  if (long)
    print_underline(72, 0, stream)
  else
    print_underline(46, 0, stream)
}

function print_underline(l, notab, fd,    i) {
  notab = !notab ? 0 : 1
  fd = ("" == fd) ? "/dev/stdout" : fd
  if (!notab)
    printf  "\t" > fd
  for (i = 0; i <= l; i++)
    printf "_" > fd
  printf "\n" > fd
}

# Code sharing between capital gains and deferred gains
function print_gains_summary(units_sold, sum_cost, sum_paid, adjusted_cost, reduced_cost, width, disc_gains, other_gains, tax_losses, output_stream,
                             label, gain_type) {
  # If units sold is negative these are deferred gains...
  if (units_sold < 0) {
    label = "Held"
    gain_type = "Deferred"
    units_sold = - units_sold
  } else {
    gain_type = "Taxable"
    label = "Sold"
  }

  # Shared code for printing gains summaries
  printf "\t%6s Units => %10.3f %*s => %14s Value => %14s Reduced  => %13s Accounting Gain => %14s\n", label,
    units_sold, 4 + width, "Cost", print_cash(sum_cost), print_cash(sum_paid), print_cash(reduced_cost), print_cash(sum_paid - reduced_cost) > output_stream

  # Check the adjusted cost
  if (adjusted_cost != reduced_cost)
    printf "\t%*s => %13s\n", 82 + width, "Adjusted", print_cash(adjusted_cost) > output_stream

  printf "\t%*s => %14s\n", 115 + width, (gain_type " Gain"), print_cash(- disc_gains) > output_stream
  if (!near_zero(other_gains))
    printf "\t%*s => %14s\n", 115 + width, "Other Gain",      print_cash(- other_gains) > output_stream
  printf "\t%*s => %14s\n", 115 + width, (gain_type " Loss"),    print_cash(tax_losses) > output_stream
}

# Capital Gains

# Compute capital gains and losses
function get_capital_gains(now, past,       cgt_schedule,
                                        accounting_gains,
                                        cgt_total_gains,
                                        cgt_short_gains, cgt_long_gains,
                                        cgt_losses,
                                        cgt_short_losses, cgt_long_losses,
                                        cgt_total_losses,
                                        tax_refund) {

    # The cgt_schedule is the pipe to write the schedule out to
    cgt_schedule = ("" == EOFY) ? "/dev/null" : EOFY

    # Print the capital gains schedule
    print Journal_Title > cgt_schedule
    printf "Capital Gains Schedule for Period Ending %s\n\n", get_date(yesterday(now))  > cgt_schedule

    # Get total capital gains
    # Exploit existing sums
    # taxable capital gains are messy
    print_underline(43, 0, cgt_schedule)

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
    printf "\t%27s => %14s\n", "Accounting Capital Gains", print_cash(- accounting_gains) > cgt_schedule
    printf "\t%27s => %14s\n", "Total Capital Gains", print_cash(- cgt_total_gains) > cgt_schedule

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
    printf "\t%27s => %14s\n", "Long Capital Gains", print_cash(- cgt_long_gains) > cgt_schedule
    printf "\t%27s => %14s\n\n", "Short Capital Gains", print_cash(- cgt_short_gains) > cgt_schedule

    # Now consider the losses
    # Need to consider a maximum loss window beyond which losses will not be carried
    cgt_losses = get_cost(CAPITAL_LOSSES, just_before(now)) - get_cost(CAPITAL_LOSSES, carry_forward_limit(now))
    if (CARRY_FORWARD_LIMIT)
      printf "\t%27s => %14s\n", "Losses Carried Forward Since", get_date(carry_forward_limit(now)) > cgt_schedule
    if (!near_zero(cgt_losses))
      printf "\t%27s => %14s\n", "Carried Capital Losses", print_cash(cgt_losses) > cgt_schedule

    # Finally the losses
    printf "\t%27s => %14s\n", "New Capital Losses", print_cash(cgt_total_losses) > cgt_schedule
    printf "\t%27s => %14s\n", "Total Capital Losses", print_cash(cgt_losses + cgt_short_losses + cgt_long_losses) > cgt_schedule
    printf "\t%27s => %14s\n", "Long Capital Losses", print_cash(cgt_long_losses) > cgt_schedule
    printf "\t%27s => %14s\n\n", "Short Capital Losses", print_cash(cgt_short_losses) > cgt_schedule

    print_underline(43, 0, cgt_schedule)
    printf "\nAfter Application of Any Losses\n" > cgt_schedule

    # Apply the losses - most favourable order is to apply them to other gains first
    # A loss > 0
    # A gain < 0
    # Australian scheme & US Scheme are same
    # once short & long losses are disregarded
    cgt_long_gains  += cgt_long_losses # Net long term gains / losses
    cgt_short_gains += cgt_short_losses # Net short term losses / gains
    if (cgt_losses + cgt_short_gains + cgt_long_gains > 0) {
      # More carried losses generated
      cgt_losses += cgt_short_gains + cgt_long_gains

      # Record the details of short term & long term losses
      cgt_short_losses = ternary(above_zero(cgt_short_gains), cgt_short_gains, 0)
      cgt_long_losses  = ternary(above_zero(cgt_long_gains),  cgt_long_gains, 0)

      # Zero negligible losses
      if (near_zero(cgt_losses))
        cgt_losses = 0

      printf "\n\tOverall Capital Loss\n" > cgt_schedule
      printf "\t%27s => %14s\n", "Capital Losses", print_cash(cgt_losses) > cgt_schedule
      if (below_zero(cgt_short_gains))
        printf "\t%27s => %14s\n", "Short Gains", print_cash(- cgt_short_gains) > cgt_schedule
      else if (above_zero(cgt_short_losses))
        printf "\n\t%27s => %14s\n", "Short Losses", print_cash(cgt_short_losses) > cgt_schedule

      if (below_zero(cgt_long_gains))
        printf "\t%27s => %14s\n", "Long Gains", print_cash(- cgt_long_gains) > cgt_schedule
      else if (above_zero(cgt_long_losses))
        printf "\n\t%27s => %14s\n", "Long Losses", print_cash(cgt_long_losses) > cgt_schedule

      # Zero the gains
      cgt_short_gains = cgt_long_gains = 0
    } else if (cgt_losses + cgt_short_gains > 0) {
      # No overall losses, only long gains left
      cgt_losses += cgt_short_gains
      cgt_long_gains += cgt_losses

      # There could be a short term loss in this scenario
      cgt_short_losses = ternary(above_zero(cgt_short_gains), cgt_short_gains, 0)

      # But not a long term loss
      cgt_losses = cgt_short_gains = cgt_long_losses = 0

      printf "\n\tOnly Long Capital Gains\n" > cgt_schedule
      printf "\t%27s => %14s\n", "Long Gains", print_cash(- cgt_long_gains) > cgt_schedule
      if (!near_zero(cgt_short_losses))
        printf "\n\t%27s => %14s\n", "Short Losses", print_cash(cgt_short_losses) > cgt_schedule
    } else {
      # Long and Short Gains
      cgt_short_gains += cgt_losses

      # No long term or short term losses
      cgt_losses = cgt_short_losses = cgt_long_losses = 0

      printf "\n\tBoth Short & Long Capital Gains\n" > cgt_schedule
      printf "\t%27s => %14s\n", "Long Gains", print_cash(- cgt_long_gains) > cgt_schedule
      printf "\t%27s => %14s\n", "Short Gains", print_cash(- cgt_short_gains) > cgt_schedule
    }

    # Losses might sometimes be written back against earlier gains
    if (WRITE_BACK_LIMIT && !near_zero(cgt_losses)) {
      # Try writing back losses
      printf "\n\t%27s => %14s\n", "Write Back Losses Available", print_cash(cgt_losses) > cgt_schedule

      # Rewrite refundable offsets to just before now so they can be zeroed later at a distinct timestamp
      cgt_losses = write_back_losses(just_before(now), last_year(now), write_back_limit(now), cgt_losses, cgt_schedule)
    }

    # All done
    print_underline(43, 0, cgt_schedule)
    print "\n" > cgt_schedule

    # Save losses and taxable gains
    set_cost(CAPITAL_LOSSES, cgt_losses, now)
    #set_cost(TAXABLE_GAINS, cgt_taxable_gains, now)

    # Also save taxable short & long gains
    set_cost(TAXABLE_LONG, cgt_long_gains, now)
    set_cost(TAXABLE_SHORT, cgt_short_gains, now)
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

# Compute the deferred gains
# And print out a schedule
#
function get_deferred_gains(now, carried_losses, is_detailed,       def_schedule,
                                                                   gains_event, current_price, p, a, units_sold,
                                                                   reduced_cost, adjusted_cost, sum_cost,
                                                                   adjust, paid, sum_paid,
                                                                   parcel_gains, parcel_tax_gains, parcel_tax_losses,
                                                                   gains, tax_gains, tax_losses,
                                                                   sum_gains, sum_tax_gains, sum_tax_losses,
                                                                   description, past) {

 # variables
 # by parcel
 #   parcel_gains        past_parcel_gains
 #   parcel_tax_gains    past_tax_gains
 #   parcel_tax_losses   past_tax_losses
 #
 # by asset
 #   gains
 #   tax_gains
 #   tax_losses
 #
 # sums
 #   sum_gains
 #   sum_tax_gains
 #   sum_tax_losses

 # The pipe to write  the schedule to
 def_schedule = ("" == EOFY) ? "/dev/null" : EOFY

 # Are we printing out a detailed schedule?
 is_detailed = ("" == is_detailed) ? FALSE : is_detailed

 # Print the capital gains schedule
 printf "\n\n%s\n", Journal_Title > def_schedule
 printf "Deferred Tax Schedule for Period Ending %s\n", get_date(yesterday(now))  > def_schedule

 # The hypothetical gains arising from selling the assets in the future based on their values now
 sum_gains = sum_tax_gains = 0
 sum_tax_losses = carried_losses

 # Previous accouting period
 past = last_year(now)

 # Deferred tax
 #   Due to unrealized capital revaluations
 #   And to deferred tax distributions made to capital assets in Cost Element I
 #   Thus total deferred gain is  Adjusted_Cost - Value +  Tax_Adjustments[I]

 # For each open asset
 for (a in Leaf)
   if (is_capital(a) && is_open(a, now)) {
     gains_event = FALSE
     sum_paid = sum_cost = reduced_cost = adjusted_cost = 0 # Total cost summed here
     units_sold = 0
     tax_gains = tax_losses = gains = 0 # Totals summed here

     # The price
     current_price = find_entry(Price[a], now)

     # Need to select parcels by sold date
     for (p = 0; p < Number_Parcels[a]; p++ ) {
       if (Held_From[a][p] > now) # All further transactions occured after (now) - parcels are sorted in order bought
         break # All done

       # This is a hypothetical calculation based on unsold parcels
       if (is_unsold(a, p, now)) {
         if (!gains_event) {
           gains_event = TRUE
           printf "%s\n", get_short_name(a) > def_schedule
           if (is_detailed)
             printf "\tUnsold Parcels\n" > def_schedule
         }

         # Keep track
         units_sold    += Units_Held[a][p]
         reduced_cost  += get_parcel_cost(a, p, now)
         adjusted_cost += get_parcel_cost(a, p, now, TRUE)

         # "paid" is actually the parcel value
         paid           = current_price * Units_Held[a][p]

         # cash in and out
         sum_cost += get_cash_in(a, p, now)
         sum_paid += paid

         # Accounting Gains
         parcel_gains = sum_cost_elements(Accounting_Cost[a][p], now) - paid

         # Adjustment
         adjust = sum_cost_elements(Tax_Adjustments[a][p], now)

         # All gains are discounted plus check for tax adjustments
         #
         #
         # > 0 is a LOSS
         # < 0 is a GAIN
         parcel_tax_gains = parcel_gains - adjust
         parcel_tax_losses = parcel_gains

         # Accounting Gains
         if (parcel_tax_losses > Epsilon) {
           description = "Deferred Loss"
           tax_losses += parcel_tax_losses
           parcel_tax_gains = 0
         } else if (parcel_tax_gains < - Epsilon) {
           description = "Deferred Gain"
           tax_gains  += parcel_tax_gains
           parcel_tax_losses = 0
         } else
           parcel_tax_losses = parcel_tax_gains = 0

         # Sum gains
         gains += parcel_gains

         # Printing
         if (is_detailed) {
           # A nice label
           printf "\t%6d Units => %10.3f Held => [%11s, %11s] Cost => %14s Value => %14s Reduced  => %13s",
             p, Units_Held[a][p], get_date(Held_From[a][p]), get_date(now), print_cash(get_cash_in(a, p, now)),
                print_cash(paid), print_cash(get_parcel_cost(a, p, now)) > def_schedule
           if (near_zero(adjust))
             printf " %15s => %14s\n", description, print_cash(parcel_tax_gains < 0 ? -parcel_tax_gains : parcel_tax_losses) > def_schedule
           else {
             # The accounting gain/loss is simple
             printf " %15s => %14s\n", "Accounting Gain", print_cash(- parcel_gains) > def_schedule

             # If a parcel has value between the reduced and adjusted cost
             # it can have zero taxable gains (or losses)
             # In this case the accounting gains < 0 but parcel_tax_gains would be zero
             if (near_zero(parcel_tax_gains) && parcel_gains < 0) {
               # Zero tax gains
               description = "      Zero Gain"
               parcel_gains = 0
             }

             # Next line has adjusted cost and tax gains or losses
             printf "\t%117s => %13s %15s => %14s\n", "Adjusted", print_cash(get_parcel_cost(a, p, now, TRUE)), description,
               print_cash(parcel_tax_gains < 0 ? -parcel_tax_gains : parcel_tax_losses) > def_schedule
           }
         } # End of is detailed
       } # End of unsold parcel
     } # End of each parcel p

     # Show any parcel_gains event
     if (is_detailed)
       print_underline(167, 0, def_schedule)
     print_gains_summary(-units_sold, sum_cost, sum_paid, adjusted_cost, reduced_cost, 35 * is_detailed, tax_gains, 0, tax_losses, def_schedule)

     # Sum the deferred gains
     sum_gains      += gains
     sum_tax_gains  += tax_gains
     sum_tax_losses += tax_losses

     # Debugging
@ifeq LOG get_deferred_gains
     printf "\t%27s => %14s\n", "Accounting Deferred Gains", print_cash(- sum_gains) > "/dev/stderr"
     printf "\t%27s => %14s\n", "Taxable Deferred Gains", print_cash(- sum_tax_gains) > "/dev/stderr"
     printf "\t%27s => %14s\n", "Deferred Losses", print_cash(sum_tax_losses) > "/dev/stderr"
@endif

   } # End of each active asset
 # End of each asset a

 # Print Capital Gains & Losses
 print_underline(43, 0, def_schedule)

 printf "\t%27s => %14s\n", "Accounting Deferred Gains", print_cash(- sum_gains) > def_schedule
 printf "\t%27s => %14s\n", "Taxable Deferred Gains", print_cash(- sum_tax_gains) > def_schedule
 printf "\t%27s => %14s\n", "Deferred Losses", print_cash(sum_tax_losses) > def_schedule
 printf "\nAfter Application of Any Losses\n" > def_schedule

 # Apply the losses - most favourable order is to apply them to other gains first
 # A loss > 0
 # A gain < 0
 if (sum_tax_losses + sum_tax_gains > 0) {
   # More carried losses generated
   gains = sum_tax_losses += sum_tax_gains
   sum_tax_gains = 0

   printf "\n\tOverall Deferred Loss\n" > def_schedule
   printf "\t%27s => %14s\n", "Deferred Losses", print_cash(gains) > def_schedule
 } else {
   # Taxable gains
   sum_tax_gains += sum_tax_losses
   sum_tax_losses = 0
   printf "\n\tOverall Deferred Gain\n" > def_schedule
   printf "\t%27s => %14s\n", "Deferred Gains", print_cash(- sum_tax_gains) > def_schedule

   # Return these gains
   gains = sum_tax_gains
 }

 # Return results
 return gains
} # End of deferred gains
