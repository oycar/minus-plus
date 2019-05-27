#!/usr/local/bin/gawk -f
# p.smsf_modules.awk
# Copyright (C) 2019  Robert Whitehurst
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


# Extra functions
# For Super Funds (Australian - SMSF - Journal Type "SMSF")
# This adjusts member balances to allow for profit/loss including
# deferred profit/loss calculated in the annual tax return
function balance_profits_smsf(now, past, initial_allocation,     delta_profits, x) {
  # Balance the books - including the reserve
  # Note that this is only needed to be done once
  # Reset the liabilities to just before now so that they are correct even if balance journal is re-run
  for (x in Member_Liability)
    set_cost(x, get_cost(x, just_before(now)), now)
  set_cost(RESERVE, get_cost(RESERVE, just_before(now)), now)

  # Adjust member liability
  delta_profits = accumulated_profits(now) - initial_allocation

@ifeq LOG balance_journal
  # Track reserve and accumulated profits
  printf "EOFY Balance Journal\n\tDate => %s\n", get_date(now, LONG_FORMAT) > "/dev/stderr"
  printf "\tAccumulated Profits => %s\n", print_cash(delta_profits + get_cost(ALLOCATED, now)) > "/dev/stderr"
  printf "\tAllocated Profits   => %s\n", print_cash(get_cost(ALLOCATED, now)) > "/dev/stderr"
  printf "\tInitial Allocation  => %s\n", print_cash(initial_allocation) > "/dev/stderr"
  printf "\tDelta Profits       => %s\n", print_cash(delta_profits) > "/dev/stderr"
@endif

  # Update the allocation - a get is before the set
  if (!near_zero(delta_profits))
    # Update the Allocated Profits - this adds to changes made in print_tax_statement
    adjust_cost(ALLOCATED, delta_profits, now)

  # Also make adjustments to the reserve - use the updated Allocation
  x = get_cost(ALLOCATED, now) - get_cost(ALLOCATED, past)
@ifeq LOG balance_journal
  # Track reserve
  printf "\tAllocated Profits [%s]    => %s\n", get_date(now, LONG_FORMAT), print_cash(get_cost(ALLOCATED, now)) > "/dev/stderr"
  printf "\tAllocated Profits [%s]    => %s\n", get_date(just_before(now), LONG_FORMAT), print_cash(initial_allocation) > "/dev/stderr"
  printf "\tAllocated but not Applied => %s\n", print_cash(get_cost(ALLOCATED, now) - initial_allocation + \
                                                           get_cost(MARKET_CHANGES, now) - get_cost(MARKET_CHANGES, past)) > "/dev/stderr"
  printf "\tAllocated Profits [%s]    => %s\n", get_date(past, LONG_FORMAT), print_cash(get_cost(ALLOCATED, past)) > "/dev/stderr"
  printf "\tChange in Profits         => %s\n", print_cash(x) > "/dev/stderr"
@endif

  # Apply actual profits to the reserve
  if (above_zero(x)) {
    # Only distribute actual delta_profits to the reserve
    # Compute the net allocated profits in the current period
    x *= find_entry(Reserve_Rate, now)

    # The only reserve set in eofy actions so use now
    adjust_cost(RESERVE, -x, now)
@ifeq LOG balance_journal
    # Track reserve
    printf "\tApplied to Reserve      => %s\n", print_cash(-x) > "/dev/stderr"
    printf "\tApplied to Members      => %s\n", print_cash(-delta_profits) > "/dev/stderr"
@endif
  } else
    x = 0

  # By this point there are several adjustments required to
  # both redistribute liabilities and allocated profits
  delta_profits = get_cost(ALLOCATED, now) - initial_allocation - x
  if (!near_zero(delta_profits))
    update_member_liability(now, delta_profits)

  # Unallocated expenses/income
  adjust_cost(ALLOCATED, accumulated_profits(now) - get_cost(ALLOCATED, now), now)
@ifeq LOG balance_journal
  # Track reserve
  printf "\tDelta Profits                => %s\n", print_cash(delta_profits) > "/dev/stderr"
  printf "\tAccumulated - Allocated      => %s\n", print_cash(accumulated_profits(now) - get_cost(ALLOCATED, now)) > "/dev/stderr"
@endif
}

# This checks all is ok
function check_balance_smsf(now,        sum_assets, sum_liabilities, sum_adjustments, sum_future, balance, show_balance) {
  # The following should always be true (Equity is treated a special case of liability)
  # Assets - Liabilities = 0 (SMSFs have a simplified equation)
  # A complication exists if back payments are included so we have innstead
  # Assets - Liabilities = Future_Payments
  # This compares the cost paid - so it ignores the impact of revaluations and realized gains & losses
  sum_assets =  get_cost("*ASSET", now) - get_cost("*INCOME.GAINS.REALIZED", now) - get_cost("*EXPENSE.LOSSES.REALIZED", now) - get_cost("*EXPENSE.UNREALIZED", now)

  # Work out the total assets etc
  sum_liabilities = - get_cost("*LIABILITY", now)
  sum_future      = - get_cost(FUTURE_PAYMENT, now)

  # The balance should be zero
  # A super fund has only assets and liabilities since the income and expenses are attributed to members
  sum_adjustments = accumulated_profits(now) - get_cost(ALLOCATED, now)
  balance = sum_assets - (sum_liabilities + sum_adjustments + sum_future)

@ifeq LOG check_balance
  # Verbose balance printing
  show_balance = TRUE
@else
  # No default printing
  show_balance = FALSE
@endif #// LOG

  # Is there an error?
  if (!near_zero(balance)) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0
    show_balance = TRUE
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now)
    printf "\tAssets      => %20.2f\n", sum_assets
    printf "\tLiabilities => %20.2f\n", sum_liabilities
    if (not_zero(sum_adjustments))
      printf "\tAdjustments => %20.2f\n", sum_adjustments
    if (not_zero(sum_future))
      printf "\tFuture      => %20.2f\n", sum_future
    printf "\tBalance     => %20.2f\n", balance
    assert(near_zero(balance), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}
