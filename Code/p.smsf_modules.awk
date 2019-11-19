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
  # Adjust member liability
  delta_profits = accumulated_profits(now) - initial_allocation

@ifeq LOG balance_journal
  # Track reserve and accumulated profits
  printf "EOFY Balance Journal\n\tDate => %s\n", get_date(now) > STDERR
  printf "\tDeferred Tax           => %14s\n", print_cash(get_cost(DEFERRED, now)) > STDERR
  printf "\tInitial Allocation     => %14s\n", print_cash(initial_allocation) > STDERR
  printf "\tAccumulated Profits    => %14s\n", print_cash(accumulated_profits(now)) > STDERR
  printf "\tDelta Profits          => %14s\n", print_cash(delta_profits) > STDERR
  printf "\tAllocated Profits      => %14s\n\n", print_cash(get_cost(ALLOCATED, now)) > STDERR
@endif

  # Update the allocation
  if (!near_zero(delta_profits))
    # Update the Allocated Profits - this adds to changes made in print_tax_statement
    adjust_cost(ALLOCATED, delta_profits, now)

  # Also make adjustments to the reserve - use the updated Allocation
  x = get_cost(ALLOCATED, now) - get_cost(ALLOCATED, past)
@ifeq LOG balance_journal
  # Track reserve
  printf "\tNew Allocated Profits      => %14s\n", print_cash(get_cost(ALLOCATED, now)) > STDERR
  printf "\tChange in Profits to Apply => %14s\n", print_cash(x) > STDERR
@endif

  # Apply actual profits to the reserve
  x *= find_entry(Reserve_Rate, now)
  if (above_zero(x)) {
    # Initialize reserve if needed
    if (!RESERVE)
      RESERVE   = initialize_account("LIABILITY.RESERVE:INVESTMENT.RESERVE")

    # The only reserve set in eofy actions so use now
    adjust_cost(RESERVE, -x, now)
@ifeq LOG balance_journal
    # Track reserve
    printf "\tApplied to Reserve         => %14s\n", print_cash(-x) > STDERR
@endif
  } else
    x = 0

  # By this point there are several adjustments required to
  # both redistribute liabilities and allocated profits
  delta_profits = get_cost(ALLOCATED, now) - initial_allocation - x
  if (!near_zero(delta_profits))
    update_member_liability_smsf(now, delta_profits, Member_Liability)

  # Unallocated expenses/income
  adjust_cost(ALLOCATED, accumulated_profits(now) - get_cost(ALLOCATED, now), now)
@ifeq LOG balance_journal
  # Track reserve
  printf "\tApplied to Members         => %14s\n", print_cash(delta_profits) > STDERR
  printf "\tUnallocated Profits        => %14s\n", print_cash(accumulated_profits(now) - get_cost(ALLOCATED, now)) > STDERR
  printf "\tFinal Allocated            => %14s\n", print_cash(get_cost(ALLOCATED, now)) > STDERR
@endif
}

# This checks all is ok
function check_balance_smsf(now,        sum_assets, sum_liabilities, sum_adjustments, sum_future, balance, show_balance, output_stream) {
  # The following should always be true (Equity is treated a special case of liability)
  # Assets - Liabilities = 0 (SMSFs have a simplified equation)
  # A complication exists if back payments are included so we have innstead
  # Assets - Liabilities = Future_Payments
  # This compares the cost paid - so it ignores the impact of revaluations and realized gains & losses
  sum_assets =  get_cost("*ASSET", now)

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
  output_stream = STDOUT
@else
  # No default printing
  show_balance = FALSE
  output_stream = STDERR
@endif #// LOG

  # Is there an error?
  if (!near_zero(balance)) {
    printf "Problem - Accounts Unbalanced <%s>\n", $0 > output_stream
    show_balance = TRUE
  } else
    balance = 0

  # // Print the balance if necessary
  if (show_balance) {
    printf "\tDate => %s\n", get_date(now) > output_stream
    printf "\tAssets      => %20.2f\n", sum_assets > output_stream
    printf "\tLiabilities => %20.2f\n", sum_liabilities > output_stream
    if (not_zero(sum_adjustments)) {
      printf "\tAdjustments => %20.2f\n", sum_adjustments > output_stream
      printf "\tIncome      => %20.2f\n",  get_cost("*INCOME", now) > output_stream
      #printf "\t**<Realized => %20.2f>\n", get_cost("*INCOME.GAINS", now) > output_stream
      #printf "\t**<Contribution => %20.2f>\n", get_cost("*INCOME.CONTRIBUTION", now) > output_stream
      printf "\tExpenses    => %20.2f\n", get_cost("*EXPENSE", now) > output_stream
      #printf "\t**<Benefits => %20.2f>\n", get_cost("*EXPENSE.BENEFIT", now) > output_stream
      #printf "\t**<Realized => %20.2f>\n", get_cost("*EXPENSE.LOSSES", now) > output_stream
      #printf "\t**<Market   => %20.2f>\n", get_cost("*EXPENSE.UNREALIZED", now) > output_stream
      printf "\t**<Allocated=> %20.2f>\n", get_cost(ALLOCATED, now) > output_stream
      printf "\tSpecial    => %20.2f\n", - get_cost("*SPECIAL", now) > output_stream
      printf "\tBalancing  => %20.2f\n", - get_cost("*SPECIAL.BALANCING", now) > output_stream

    }

    if (not_zero(sum_future))
      printf "\tFuture      => %20.2f\n", sum_future > output_stream
    printf "\tBalance     => %20.2f\n", balance > output_stream
    assert(near_zero(balance), sprintf("check_balance(%s): Ledger not in balance => %10.2f", get_date(now), balance))
  }
}

# A wrapper function updates allocated profits when required ()
function update_profits_smsf(now,     delta_profits) {
  # Compute the profits that need to be allocated to members
  # These are the profits accumulated since the last time they were distributed to members
  delta_profits = accumulated_profits(now) - get_cost(ALLOCATED, now)
  if (!near_zero(delta_profits)) {

    # Update the Allocated Profits
    adjust_cost(ALLOCATED, delta_profits, now, FALSE)

    # Update the liabilities
    update_member_liability_smsf(now, delta_profits, Member_Liability)
  }
}

# Update a member liability
#
# How does this interact with liabilities - surely the value of pension liabilities
# should be taken into account when pro-rating; so need to revisit this
#
#  Stream/Pension Taxable/Tax-Free ratios are locked
#  Contributions other benefits go to and from the accumulation accounts
#
# This can be (i)   a contribution - specified member, taxable or tax-free
#          or (ii)  a benefit - specified member
#          or (iii) allocation amongst members - no specificiation
#          or (iv)  allocation to or from the reserve - no specification
# This function keeps the member liability up to date for a SMSF
#
function update_member_liability_smsf(now, amount, liability_array, a,

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

  # Note if a taxable share is driven negative the value should be transferred
  # from the tax-free share - where else

  # Get the appropriate member account
  member_id = ternary(a, get_member_name(a, now, amount), "")

@ifeq LOG update_member_liability
  printf "Update Liabilities [%s]\n", get_date(now) > STDERR
  if (member_id)
    printf "\t%20s => %s\n", "Member id", member_id > STDERR
  printf "\tMember Shares\n" > STDERR
@endif # LOG

  # Allocation to the liability accounts
  # Either no id is given - distribute amongst all accounts
  # Or a parent account - distribute amongst its offspring
  # Or a specific account - distribute solely to that account
  taxable_share = sum_total = sum_share = 0

  # Normalize amounts
  if (member_id in liability_array) { # Exact match - a contribution
    # Adjust the liability
    adjust_cost(member_id, - amount, now)
    if (member_id ~ /TAXABLE/)
      taxable_share = 1.0

@ifeq LOG update_member_liability
    sum_share = 1.0
    printf "\t%20s => %10.6f %20s => %14s\n", Leaf[member_id], sum_share, Leaf[member_id], print_cash(amount) > STDERR
@endif # LOG
  } else { # Get totals
    # We still get the share from each account
    # Don't use the accumulated totals because (rarely) a negative account balance will break the proportioning
    for (member_account in liability_array)
      if (!member_id || is_ancestor(member_id, member_account)) {
        share[member_account] = x = get_cost(member_account, now)
        sum_total += x

        # Compute what fraction of the allocation was taxable
        if (member_account ~ /TAXABLE/)
          taxable_share += x
      }

    # Normalize taxable share
    assert(not_zero(sum_total), "update_member_liability: No liabilities to share")
    taxable_share /= sum_total

    # Update the liabilities - but only if account a is not a liability already
    if (!is_liability(a)) {
      # There are two possibilities here -
      #   No member id => profit/loss everything goes to/from TAXABLE accounts
      #   A parent id  => proportioning rule applies
      for (member_account in share) {
        x = share[member_account] / sum_total

        # Target account
        if (!member_id)
          target_account = liability_array[member_account]
        else
          target_account = member_account

        # Adjust the liability
        adjust_cost(target_account, - x * amount, now)
@ifeq LOG update_member_liability
        sum_share += x
        printf "\t%20s => %10.6f %20s => %14s\n", Leaf[member_account], x, Leaf[target_account], print_cash(x * amount) > STDERR
@endif # LOG
      } # End of exact share
    }
  } # End of allocation

  # Tidy up
  delete share

@ifeq LOG update_member_liability
  # Just debugging
  printf "\t%20s => %10.6f %20s => %14s\n", "Share", sum_share, "Total", print_cash(amount) > STDERR
@endif # LOG

  # return proportion that was taxable
  return taxable_share
}

# Obtain the member account
function get_member_name(a, now, x,   member_name, member_account, target_account, account_type, contribution_tax) {
  # This obtains the liability account that needs to be modified
  # In more detail INCOME.CONTRIBUTION.TYPE:NAME.X => LIABILITY.MEMBER.NAME:NAME.TYPE
  # And            EXPENSE.NON-DEDUCTIBLE.BENEFIT:NAME.TYPE => *LIABILITY.MEMBER.NAME (pro-rated if TYPE not specified)
  # And            LIABILITY.MEMBER.(STREAM|PENSION) => *LIABILITY.MEMBER.(STREAM|PENSION).NAME:SOME.NAME.TYPE (pro-rated if TYPE not specified)
  # In fact        X.Y:NAME.TYPE => *LIABILITY.MEMBER.NAME

  # Get the member name
  member_name = get_name_component(Leaf[a], 1) # first component

  # A member liability account can only be created by a contribution
  if (is_class(a, "INCOME.CONTRIBUTION")) {
    # Identify the "account_type" (eg TAXABLE or TAX-FREE) - use Parent_Name because it is always available
    account_type = get_name_component(Parent_Name[a], 0) # last component

    # If a link is made in a "MEMBER" array to each members liabilities
    # then there is no need to identify this as a member liability in the
    # account name
    member_account = initialize_account(sprintf("LIABILITY.MEMBER.%s:%s.%s", member_name, member_name, account_type))

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
  } else if (is_pension(a))
    member_account = "*LIABILITY.MEMBER.PENSION." member_name
  else if (is_stream(a))
    member_account = "*LIABILITY.MEMBER.STREAM." member_name
  else {
    # Return the ancestral account
    member_account = "*LIABILITY.MEMBER." member_name
    assert(member_account in Parent_Name, "<" $0 "> Unknown account <" member_account ">")
  }

  # Return the account
  return member_account
}

# Process member Benefits
# Can use shortcut function names
function process_member_contributions_smsf(now, x, liability_array, a) {
  if (is_contribution(a)) {
    # This will change proportions so update the profits first
    update_profits_smsf(now)

    # Fix up member liabilities
    update_member_liability_smsf(now, x, Member_Liability, a)
  }
}

# Pay out Member benefits into pension or other accounts
function process_member_benefits_smsf(now, array, amount,
           a, b,
           taxable_account, use_name,
           target_account, member_name,
           unrealized_gains,
           amount_taxed) {

  # Local accounts
  a = array[1]; b = array[2]

  # A complication for SMSF are transfers into a pension sub-account
  taxable_account = ""
  if (is_stream(a)) {
    if (!is_suffix(a, "TAXABLE") && !is_suffix(a, "TAX-FREE")) {
      # Naming convention
      #
      # *:NAME.SUFFIX => *.NAME:NAME.SUFFIX.TAXABLE & *.NAME:NAME.SUFFIX.TAX-FREE
      #
      # Initialize accounts as needed
      member_name = get_name_component(Leaf[a], 1)
      use_name = sprintf("%s.%s:%s", substr(Parent_Name[a], 2), member_name, Leaf[a])
      taxable_account = initialize_account(sprintf("%s.TAXABLE", use_name))
      if (!(taxable_account in Pension_Liability)) {
        # Need to ensure target account is recorded too
        target_account = initialize_account(sprintf("LIABILITY.MEMBER.%s:%s.TAXABLE", member_name, member_name))
        Member_Liability[taxable_account] = Pension_Liability[taxable_account] = target_account
      } else
        target_account = Member_Liability[taxable_account]

      # Replace account a with tax-free account
      a = initialize_account(sprintf("%s.TAX-FREE", use_name))
      if (!(a in Pension_Liability))
        Member_Liability[a] = Pension_Liability[a] = target_account

      # These are Pension Liability Accounts
    } else if (is_suffix(a, "TAXABLE"))
      taxable_account = a
  }

  # A SMSF member benefit or pension pament
  if (is_benefit(b) || is_stream(b)) {

    # But there is another complication - this needs to consider
    # unrealized gains too => so important assets are priced accurately
    #
    # Save unrealized gains; notice that the asset class must be updated too for balancing
    unrealized_gains = get_asset_gains("get_unrealized_gains", now)

    # Get the change since previous transaction
    unrealized_gains -= get_cost(UNREALIZED, get_previous_transaction(UNREALIZED, just_before(now)))

    # Adjust the market gains and the asset values
    adjust_cost("*ASSET", - unrealized_gains, now)
    adjust_cost(UNREALIZED, unrealized_gains, now)

    # This will change proportions so update the profits first
    update_profits_smsf(now)

    # Expense must be account b
    if (is_stream(b))
      amount_taxed = amount * @Update_Member_Function(now, -amount, Pension_Liability, b)
    else
      amount_taxed = amount * @Update_Member_Function(now, -amount, Member_Liability, b)

    if (!is_suffix(b, "TAXABLE") && !is_suffix(b, "TAX-FREE")) {
      # Naming convention
      #
      # *:NAME.SUFFIX => *.NAME:NAME.SUFFIX.TAXABLE & *.NAME:NAME.SUFFIX.TAX-FREE
      #
      # Initialize accounts as needed
      use_name = sprintf("%s.%s:%s", substr(Parent_Name[b], 2), get_name_component(Leaf[b], 1), Leaf[b])

      # Adjust costs for taxable account
      #
      if (taxable_account)
        adjust_cost(taxable_account, -amount_taxed, now)
      else
        adjust_cost(a, -amount_taxed, now)

      # Finished with the credit taxable account
      b = initialize_account(sprintf("%s.TAXABLE", use_name))
      adjust_cost(b, amount_taxed, now)

      # Record this sub-transaction
      if (taxable_account)
        print_transaction(now, Comments, taxable_account, b, amount_taxed, Cost_Element)
      else
        print_transaction(now, Comments, a, b, amount_taxed, Cost_Element)

      # Replace account b with tax-free account
      b = initialize_account(sprintf("%s.TAX-FREE", use_name))

      # Adjust the amount for later processing
      amount -= amount_taxed
    }
  }

  ordered_pair(array, a, b)
  return amount
}
