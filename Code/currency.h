# // Include currency.h
# // Currency specification

# This is a C Style Header file
# It contains macros and definitions for the awk scripts
# MPX.h
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



# // Default Currency Symbol
@ifndef JOURNAL_CURRENCY
@define JOURNAL_CURRENCY ("AUD")


@ifeq JOURNAL_CURRENCY "AUD"
#// Extra definitions for AUD
#// Add localized State Variables & Scalars
# // Add localized State Variables & Scalars
@define MPX_ARRAYS (SHARED_ARRAYS " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Medicare_Levy Member_Liability Reserve_Rate ")
@define MPX_SCALARS (SHARED_SCALARS " Balance_Profits_Function Check_Balance_Function ")

@defeval CARRY_FORWARD_LIMIT  ("")
@defeval WRITE_BACK_LIMIT     ("")

@elif JOURNAL_CURRENCY "USD"
# // Add localized State Variables & Scalars
@define MPX_ARRAYS (SHARED_ARRAYS " Capital_Gains_Tax_Bands ")
@define MPX_SCALARS (SHARED_SCALARS "")
#
@defeval CARRY_FORWARD_LIMIT  @eval (5 * 366 * ONE_DAY)
@defeval WRITE_BACK_LIMIT     @eval (3 * 366 * ONE_DAY)

@endif # // USD
@endif # // JOURNAL_CURRENCY
