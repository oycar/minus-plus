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
@endif # // JOURNAL_CURRENCY

@ifeq JOURNAL_CURRENCY "AUD"
#// Extra definitions for AUD
#// Add localized State Variables & Scalars
# // Add localized State Variables & Scalars
@define MPX_ARRAYS (SHARED_ARRAYS " ATO_Levy CGT_Discount GST_Rate LIC_Allowance Low_Income_Offset Middle_Income_Offset Medicare_Levy Member_Liability Reserve_Rate ")
@define MPX_SCALARS (SHARED_SCALARS " Balance_Profits_Function Check_Balance_Function ")
@define QUALIFICATION_WINDOW (91)

# // Carry Forward & Write Back Limits in Years
# // @defeval CARRY_FORWARD_LIMIT  (0)
@defeval CARRY_FORWARD_LIMIT  (0)
@defeval CARRY_FORWARD_TAX_LIMIT @eval (CARRY_FORWARD_LIMIT)
@defeval CARRY_FORWARD_CAPITAL_LIMIT @eval (CARRY_FORWARD_LIMIT)
@defeval WRITE_BACK_LIMIT     (0)

@elif JOURNAL_CURRENCY "USD"
# // US Dollars
# // Add localized State Variables & Scalars
@define MPX_ARRAYS (SHARED_ARRAYS " Capital_Gains_Tax_Bands ")
@define MPX_SCALARS (SHARED_SCALARS "")
#
@defeval CARRY_FORWARD_LIMIT  (5)
@defeval WRITE_BACK_LIMIT     (3)
@define QUALIFICATION_WINDOW (121)

@endif # // JOURNAL_CURRENCY
