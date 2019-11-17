## Minus-Plus Usage ##

Here are a number of examples of that extend the examples in the _example.jrn_
file to demonstrate
how different financial transactions can be accounted for using _Minus Plus_.

# Handling Rent and Property Expenses #
This is simple - just create appropriate **INCOME** and **EXPENSE** accounts
These can be as fine grained as required; e.g. a single **EXPENSE** account or alternatively various accounts broken up into agency expenses, repairs, rates could be used.
In either case financial statements will be produced using the final component
of the class name as a sub-header; so so if multiple properties were held
then these could be collected up in one heading with accounts like **INCOME.FIRST.PROPERTY:FIRST.RENT**,
**INCOME.PROPERTY.SECOND:SECOND.RENT**, and so on; alternatively account names like
**INCOME.PROPERTY.FIRST_PROPERTY:FIRST.RENT** would create different sub-headings for each property. (The _PROPERTY_ name component is not required.)

```
2008 Aug 08,  CASH, EXPENSE.PROPERTY:AGENT.EXPENSE,    52.58, # Rent - Agent's Expenses
2008 Aug 08,  INCOME.PROPERTY:FARM.RENT,      CASH,   640.00, #
```

# Processing GST #
If  GST (or for that matter VAT) need to be accounted for
a GST field can be appended to the earlier fields. In the case of a share or managed fund
transaction GST is only applied to the brokerage and so a purchase of 52 units
in the _CBAPB_ hybrid trust  might look like
```
2008 Aug 14,  LIABILITY.CURRENT.CREDITOR,  ASSET.CAPITAL.TRUSTS:CBAPB.ASX,   9884.70,   52,  32.95,  GST,  [B], [H], # CBAPB - Perls IV -  52
```
which would apply GST at the rate specified by the variable _GST_Rate_, which is 10% by default.
For Australian super funds a reduced rate of GST often applies, which is signified by the _PART_GST_ field

```
2008 Sep 25,    CSR.ASX,   ANZ,   1207.05, -500,  32.95,  PART_GST,  [S], [H], # Disposal of 500 units in CSR.ASX
```

A GST refund would look like this, with _TAX.GST_ being a predefined account **LIABILITY.TAX:TAX.GST**
```
2008 Oct 17,   TAX.GST,    CASH,   167.00, # ATO GST Refund
```

An example of GST being collected (and being levied on the whole amount) is:
```
2008 Dec 22,   INCOME.PROPERTY:LAND.RENT,   CASH,   693.50, GST, # Quarterly Rental for Farmland
```

At the moment the easiest way to check the state of the _TAX.GST_ account is to run
Minus Plus in account mode; i.e.
```
mpx -aTAX.GST Journals/journal_file.jrn 2> /dev/null
```
which would yield a running total of the _TAX.GST_ account.

# Income Tax #

Paying income tax involves other predefined accounts **LIABILITY.TAX:TAX**, **LIABILITY.TAX:TAX.PAYG** and the
administrative levy EXPENSE.LEVY:ATO.LEVY_

```
2009 Jan 06,     ESAVER,         TAX,       10152.50,  [Tax Return:], # Income Tax 2008 due
2009 Jan 06,        TAX,    ATO.LEVY,         150.00,                 # Supervisory Levy
2009 Jan 06,     ESAVER,    TAX.PAYG,        2700.00,        [PAYG:], # Income Tax Instalment (2008-2009)
2009 Jan 06,     ESAVER,     TAX.GST,          69.00,                 # GST Paid
```

# Managed Fund Distributions #

Distributions from managed funds are taxed on an accrual rather than a cash basis. To deal with this
record distributions as being paid from a current **ASSET** account **ASSET.CURRENT.ACCRUED** (in fact
the final name component _ACCRUED_ is not required, but if some distinctive final name is not used the end of financial year accounts will not present these entries under a separate sub–heading.)

```
2009 Jan 07, ASSET.CURRENT.ACCRUED:UBS_AUS.DUE,    CASH,  707.55,             [D], # UBS Australian Bonds Distribution
2009 Jan 14,     ASSET.CURRENT.ACCRUED:AYT.DUE,    CASH,  825.00,             [D], # Asset Backed Yield Trust (AYT) Distribution
```

The at the end of the financial year the distributions paid can be recorded once the annual tax statements
are received; the distribution may include franking credits too.

```
2009 Jun 30,     INCOME.DISTRIBUTION:AYF.NPPI,  AYF.DUE,   934.59,            [T], # NPPI
2009 Jun 30, INCOME.DISTRIBUTION:DIST.AYF.ASX,  AYF.DUE,   500.46,   214.48,  [T], # AYF Annual Distribution
```

Allowed fund expenses can be attributed to an expense account
```
2009 Jun 30,     AYF.DUE,   EXPENSE.DISTRIBUTION:EXP.AYF,  185.06,                 # AYF Annual Tax Statement
```

Deferred income (which decreases the cost basis)
can be paid as capital return from the asset capital account itself;
```
2009 Jun 30,                          AYF.ASX,    AYF.DUE,   580.51,           [T], # AYF Annual Distribution capital return
2009 Jun 30, INCOME.DISTRIBUTION:DIST.AYT.ASX,    AYT.DUE,  1815.93,           [T], # Annual Dist
```

Other possible transactions related to end of financial year statements include net
capital gains recorded in an annual tax statement from account **ASSET.CAPITAL.TRUSTS:LEAF.NAME**
are recorded to **INCOME.GAINS.NET:GAINS.LEAF.NAME**
The _GAINS.LEAF.NAME_ is a linked account; and sums processed here will be handled
to produce the appropriate taxable gains in the end of financial year statement; the gains reported
should be the actual net gains recorded as being paid; not the grossed up gains. The accounts
will take care of any discounting or offsetting against available capital losses automatically.

```
2009 Jun 30,  INCOME.GAINS.NET:GAINS.CPA.ASX,   CPA.DUE,      28.97,           [T], # CPA Net capital gain paid out in cash
```

Sometimes the reported taxable distribution is greater than the cash paid; here the distribution
is recorded as being paid to the capital asset, increasing the cost basis
```
2009 Jun 30,  DIST.HHY.ASX,                     HHY.ASX,     594.00,           [T], # HHY Annual Statement
```

Foreign sourced income can be handled with special class **INCOME.FOREIGN**. An optional field here after
the amount would indicate foreign tax offsets rather than franking credits
```
# DATE          CREDIT ACCOUNT            DEBIT ACCOUNT      AMOUNT  FOREIGN OFFSET DOCUMENTS   COMMENTS
#____________________________________________________________________________________________________________________________________________
2009 Jun 30, INCOME.FOREIGN:FOR.UBS_SHARE, UBS_SHARE.DUE,    204.56,      48.33,       [T],     # Annual Statement
```

Yet another possibility is for exempt income which is not subject to taxes at all;
this can be classified as being paid to an account in class **INCOME.EXEMPT**,
the leaf name is not linked (so any unique leaf name could be used)
```
2009 Jun 30,   INCOME.EXEMPT:EXEMPT.LLC.ASX,      LLC.DUE,          53.97,     [T], # LLC Non Assessable, no impact on cost base
```


# Dividend Reinvestment #

Dividend reinvestment is easily handled by including the UNITS after the amount but before
any franking credits etc

```
# DATE        CREDIT ACCOUNT  DEBIT ACCOUNT   AMOUNT   UNITS   FRANKING CREDIT EX-DIVIDEND  DOCUMENTS   COMMENTS
#____________________________________________________________________________________________________________________________________________
2010 May 31,   DIV.WHF.ASX,     WHF.ASX,      59.50,     20,        25.50,     2010 May 11  [D], [H],   # WHF DRP 20
```

For individuals and super funds listed investment companies can distribute a Listed Investment Company
tax deduction ("LIC Deduction"). These can be recorded as yet another optional field
after the franking credit. In this case if there is no franking credit a zero
value should be given rather than simply omitting it, (otherwise the LIC deduction
will be mistaken for a franking credit) - this is getting almost as complex as it gets

```
# DATE       CREDIT ACCOUNT DEBIT ACCOUNT AMOUNT   UNITS  FRANKING CREDIT LIC DEDUCTION  DOCUMENTS  COMMENTS
#____________________________________________________________________________________________________________________________________________
2013 Aug 27,  DIV.AMH.ASX,     AMH.ASX,  400.00,   459.000,    171.43,       357.14      [D],  [H], # AMH 2013 Dividends
```

# Cost Elements #

_Minus Plus_ uses the concept of a  [cost element](https://www.ato.gov.au/General/Capital-gains-tax/Working-out-your-capital-gain-or-loss/Cost-base/Elements-of-the-cost-base-and-reduced-cost-base/) in
keeping track of an asset's account balance. Whenever a unitized asset is bought the cost is recorded as being in the _first cost element I_. Any other cost adjustment, (eg brokerage, a
capital return), would by default be applied to the _second cost element II_. However this behaviour can be modified by specifying the cost element explicitly;

```
## DATE                     CREDIT ACCOUNT                DEBIT ACCOUNT      AMOUNT    COST ELEMENT      EFFECTIVE DATE    COMMENTS
##___________________________________________________________________________________________________________________________________________________________________________________________________
2008 Jun 26,                     CASH,                      FARM.990,      436401.85,       I,            2008 May 05,     # Farm Final Installment
2008 Jun 26,                     CASH,                      FARM.990,           5.00,       I,            2008 May 05,     # Bank Charges for Bank Cheque
```
in this case the adjustments are made to cost element _I_, the purchase cost, directly. Altogether there are five cost elements which make up the
[cost base](https://www.ato.gov.au/forms/guide-to-capital-gains-tax-2017/?anchor=What_is_cost_base#What_is_cost_base) of a capital gains tax liable asset, **ASSET.CAPITAL**. These are:

<ol type="upper-roman">
  <li>Money paid or property given for the CGT asset</li>
  <li>Incidental costs of acquiring the CGT asset or that relate to the CGT event</li>
  <li>Costs of owning the CGT asset</li>
  <li>Capital costs to increase or preserve the value of your asset or to install or move it</li>
  <li>Capital costs of preserving or defending your title or rights to your CGT asset</li>
</ol>   

_Minus Plus_ uses the Roman Numerals _I_, _II_, _III_, _IV_ or _V_ to indentify these.

Thus

```
2018 May 31, CASH, ASSET.CAPITAL.SHARES:ABC, 100.0, 100, # Pay $100 for 100 units of ABC, uses the first cost element I
2018 May 31, CASH,                      ABC,  10.0, II, # Pay $100 into the second cost element II - eg for brokerage, no units

## More arbitrary examples
2018 Jun 01, CASH, ASSET.CAPITAL:SOMETHING, 5000.0,  1, # Pay $5000 to the first cost element I - one unit bought
2018 Jun 02, CASH,               SOMETHING,  100.0, IV, # Pay $100 into the fourth cost element IV - no units bought or sold
2018 Jun 03, CASH,               SOMETHING,  100.0,  V, # Pay $100 into the fifth cost element V - no units bought or sold
```
So purchase costs would be cost element _I_, brokerage costs cost element _II_, fees paid to obtain a valuation cost element _III_, and so on.

# Cost Bases #

Cost elements are not the only complication, since each asset also has both a [cost base](https://www.ato.gov.au/forms/guide-to-capital-gains-tax-2017/?anchor=What_is_cost_base#What_is_cost_base),
generally the cost of purchasing an asset, and a  [reduced cost base](https://www.ato.gov.au/general/capital-gains-tax/working-out-your-capital-gain-or-loss/cost-base/elements-of-the-cost-base-and-reduced-cost-base/). These cost bases are typically identical, but they can diverge in certain circumstances. An example is the payment of _tax-free_ distributions as a [trust non-assessable payment](https://www.ato.gov.au/general/capital-gains-tax/shares,-units-and-similar-investments/non-assessable-payments-in-relation-to-shares-and-units/trust-non-assessable-payments-(cgt-event-e4)/);

A tax-free payment reduces only the _reduced cost base_, this is accomplished by bracketing the cost element symbol,
here the second cost element _II_, which notes that the _$1000_ cost base reduction is a tax adjustment.
```
## Non-assessable payments
2013 Jun 30,  ASSET.CAPITAL.TRUST:SOME.TRUST, SOME.DUE,  1000.00,       # A tax-deferred payment from SOME.TRUST, reduces both cost bases
2013 Jun 30, INCOME.EXEMPT:EXEMPT.SOME.TRUST, SOME.DUE,  1000.00,       # A tax-exempt or CGT-concession payment from SOME.TRUST, does not change either cost base
2013 Jun 30,  ASSET.CAPITAL.TRUST:SOME.TRUST, SOME.DUE,  1000.00, (II), # A tax-free payment from SOME.TRUST, changes reduced cost base only

## Assessable payments
2013 Jun 30, INCOME.DISTRIBUTION:DIST.SOME.TRUST, SOME.DUE, 1000.00, 100.00,   # An assessable distribution - with franking credits of $100
2013 Jun 30,       INCOME.FOREIGN:FOR.SOME.TRUST, SOME.DUE, 1000.00,  50.00,   # An assessable foreign sourced distribution - with foreign tax credits of $50
```

# Depreciation #

Another major issue is depreciation. This can be processed by using assets of class **ASSET.FIXED**
which are (like capital assets) unitised but (unlike capital gains) not subject to capital
gains and losses. Instead they can be depreciated over a period of time.

```
#   DATE    CREDIT ACCOUNT      DEBIT ACCOUNT         AMOUNT   UNITS  LIFETIME METHOD COMMENTS
#____________________________________________________________________________________________________________________________________________
2012 Dec 19,    ESAVER,    ASSET.FIXED:FARM.FENCES,   1132.56,    1,    30,     DV,     # Farm fencing
2012 Dec 19,    FARM.FENCES,       TAX.GST,            102.96,    I,                    # GST adjustment to Cost Element I
```

GST cannot be applied directly to this transaction because normally GST is applied as a _cost element II_ adjustment;
in this case of a fixed depreciating asset it needs to be applied to  the purchase cost in _cost element I_ directly;
which it is simple to do by using one extra transaction. (Note that
no parcel date needs to be specified if the adjustment is made on the same day as the original transaction.)


The above asset will be depreciated using the diminishing value (DV) method with an effective life of
thirty years; the cost includes GST which will be allowed for in the process. Depreciation is
automatically carried at the end of the financial year.
Extra expenditure on the asset will be applied to the second element cost, the lifetime and
method will stay the same.

```
#   DATE    CREDIT ACCOUNT      DEBIT ACCOUNT   AMOUNT   COMMENTS
#____________________________________________________________________________________________________________________________________________
2013 Apr 15,      CASH,          FARM.FENCES,   7115.44, # Farm Fencing (Replacement of old fences)
```

It is debatable whether a fencing project should be accounted for as a single initial cost followed by extra costs incurred
later as second element costs or rather as several separate assets purchased on separate dates, if each involved the
construction of a separate stretch of fence; (versus different types of cost coming due at different times, like fence posts
first, followed by labour, followed by more materials etc). In the case of several self-contained periods of fence making then each extra
batch could be treated as a different unit;

```
#   DATE    CREDIT ACCOUNT      DEBIT ACCOUNT   AMOUNT   UNITS  COMMENTS
#____________________________________________________________________________________________________________________________________________
2013 Apr 15,      CASH,          FARM.FENCES,   7115.44,   1,   # Farm Fencing (Replacement of old fences)
```
Since lifetime and the method are already defined they do not need to be included here for this extra unit.



Prime cost, PC, (or straight line) depreciation can also be used
```
#   DATE    CREDIT ACCOUNT      DEBIT ACCOUNT          AMOUNT   UNITS  LIFETIME METHOD  COMMENTS
#____________________________________________________________________________________________________________________________________________
2013 Oct 15,     CASH,    ASSET.FIXED:FARM.ENERGIZER,  1316.12,     1,    2.5,    PC,   # Portable solar powered fence energizer
```

Depreciation can be inserted manually into the accounts (if computed using an external application)
The amount is paid to the predefined debit account **EXPENSE.DEPRECIATION:DEPRECIATION**
```
#   DATE      CREDIT ACCOUNT   DEBIT ACCOUNT     AMOUNT   COST ELEMENT    COMMENTS
#____________________________________________________________________________________________________________________________________________
2013 Jun 30,  FARM.FENCES,    DEPRECIATION,     143.12,      (I),        # Manual Farm fences depreciation for FY2013
```


Depreciation is not a standard expense in that it incurs a tax adjustment;
as in the case of a tax-free distribution from a trust the
reduced cost base of the asset decreases, but the cost base stays the same. In both
cases an immediate tax benefit has occurred; to tax was payable on the tax-free
payment and tax can be reclaimed on the depreciation expense.


Depreciation can thus be entered manually by using the cost element (I).
Alternatively the special symbol (D) can be used which triggers automatic
depreciation at the date requested; e.g.
```
2013 Jan 01,  FARM.FENCES,    DEPRECIATION,     0,      (D),        # Automatic depreciation at an arbitrary date
```

The amount will be ignored and the internally computed value used; therefore
and amount of zero can be used. However it is not necessary to include
depreciation in the journal files at all; if assets are declared as depreciating
assets they will be depreciated automatically at the end of  the financial year
and appropriate entries will be added to the financial statements.




```
Depreciation Schedule for the Period [2012 Jul 01, 2013 Jul 01]

          Asset      Method  Parcel      From          To           Opening        Closing       Second Element  Adjusted    Depreciation
     ____________________________________________________________________________________________________________________________________
                                 0  [2012 Dec 19, 2013 Jul 01]      1,029.60         993.21           0.00         993.21          36.39
                                 1  [2013 Apr 16, 2013 Jul 01]      6,468.58       6,379.38           0.00       6,379.38          89.20
                                 2  [2013 May 12, 2013 Jul 01]        873.76         865.86           0.00         865.86           7.90
                                 3  [2013 Jun 23, 2013 Jul 01]      2,035.72       2,032.93           0.00       2,032.93           2.79
                                 4  [2013 Jun 26, 2013 Jul 01]      8,327.10       8,320.26           0.00       8,320.26           6.84
    FARM.FENCES  Dim. Value         [2012 Dec 19, 2013 Jul 01]     18,734.76      18,591.64           0.00      18,591.64         143.12
     ____________________________________________________________________________________________________________________________________
    Period Depreciation                                                                                                           143.12
```

No second element costs were incurred because each cost corresponded to a new section of fencing rather than modifications or
adjustments to earlier work. This could have equally been accounted for as five different assets with different names, but this
approach is more elegant. If the stages of the project had (for some reason) different lifetimes or were depreciated in a different
way (eg prime cost rather than diminishing value) then it would be necessary to use separate accounts for those that differed.
0

In Australia depreciating items can be pooled if they are of low value; in this case each member of the
pool is a separate parcel (so each represents at least one unit). The method name is POOL and the effective
lifetime is always 5.3333 years in Australia. (A lifetime of _l_ years means that in one year _1/l_ of
the balance of the value can be depreciated, or for 5.3333 years _18.75%_.)

To allow the components of the pool to be identified in the financial statements it is convenient
to give each parcel a label, which would always be the last optional field before
documents and comments, and is signified by being surrounded by double quotes
```
#   DATE    CREDIT ACCOUNT    DEBIT ACCOUNT     AMOUNT   UNITS  LIFETIME METHOD  LABEL            DOCUMENTS         COMMENTS
#__________________________________________________________________________________________________________________________________________
2017 Jul 01,  PURCHASES, ASSET.FIXED:KSH.POOL,   127.20,  1,   5.3333,    POOL, "Iron",                            # Iron for West Row
2017 Jul 01,  PURCHASES,             KSH.POOL,   493.40,  1,                    "Coffee Table",  [Furniture],      # Coffee Table
2017 Jul 01,  PURCHASES,             KSH.POOL,   470.25,  1,                    "Easy Chair",    [Bed plus Chair], # Dixie Chair
2018 Jan 24,  PURCHASES,             KSH.POOL,   160.00,  4,                    "Outdoor Chairs",                  # West Row Outdoor Chairs
```

# Disposing of Assets #

The labels are required to uniquely identify units in fixed assets because each separate purchase always maintains its own identity; in the
above example three separate parcels are purchased on 2017 July 01, so it is impossible to identify them by purchase date should
they be disposed of or undergo other accounting adjustments. For other unitized assets such as shares parcels are aggregated should
there be more than purchase on a single day unless they are given distinguishing labels;

For example
```

# DATE       CREDIT ACCOUNT DEBIT ACCOUNT   AMOUNT     UNITS   DOCUMENTS   COMMENTS
#_____________________________________________________________________________________________________________
2011 Feb 17,     ESAVER,      WAA.ASX,     10700.00,   10000,  [B], [H],   # Buy 10000 WAA
2011 Feb 17,     ESAVER,      WAA.ASX,      2120.00,    2000,  [B], [H],   # Buy  2000 WAA
2011 Feb 17,     ESAVER,      WAA.ASX,       690.12,     648,  [B], [H],   # Buy   648 WAA
```

Would produce a single parcel of 12648 shares with an averaged price per share of $1.06816
But if the parcels were labelled three separate parcels will be saved with average prices of $1.07, $1.06 and $1.065 respectively.
```

# DATE       CREDIT ACCOUNT DEBIT ACCOUNT   AMOUNT     UNITS     LABEL     DOCUMENTS   COMMENTS
#_____________________________________________________________________________________________________________
2011 Feb 17,  ESAVER,         WAA.ASX,     10700.00,   10000,   "First",   [B], [H],  # Buy 10000 WAA
2011 Feb 17,  ESAVER,         WAA.ASX,      2120.00,    2000,   "Second",  [B], [H],  # Buy  2000 WAA
2011 Feb 17,  ESAVER,         WAA.ASX,       690.12,     648,   "Third",   [B], [H],  # Buy   648 WAA
```


Normally assets are disposed off in a first-in-first-out (FIFO) way; but this can be overridden using either the parcel purchase date or (when available) the parcel label;
a label needs only to be unique to each asset.
Thus a later sale could be
```

# DATE       CREDIT ACCOUNT DEBIT ACCOUNT   AMOUNT     UNITS  BROKERAGE  PURCHASE DATE   GST-FLAG  COMMENTS
#_____________________________________________________________________________________________________________
2013 Feb 19,  WAA.ASX,          ANZ,       4277.84,    -3648,   8.56,     2011 Feb 17,     GST,    # Disposal of 3648 units of WAA.ASX purchased on 2011 Feb 17
```

This would dispose of units purchased on 2011 Feb 17 in FIFO order; which would have the effect as
One small change is that the label is always the last option before any document links or comments
so  the label occurs after the GST-FLAG whereas a purchase date occurs before any flags or labels;

```
# DATE       CREDIT ACCOUNT DEBIT ACCOUNT   AMOUNT     UNITS  BROKERAGE  GST-FLAG    LABEL      COMMENTS
#_____________________________________________________________________________________________________________
2013 Feb 19,  WAA.ASX,          ANZ,       4277.84,    -3648,    8.56,     GST,      "First",   # Disposal of 3648 units of WAA.ASX from parcel "First" 2011 Feb 17
```

But a different result to
```
# DATE       CREDIT ACCOUNT DEBIT ACCOUNT   AMOUNT     UNITS  BROKERAGE  GST-FLAG    LABEL     COMMENTS
#_____________________________________________________________________________________________________________
2013 Feb 19,  WAA.ASX,         ANZ,         759.88,     -648,    1.52,     GST,     "Third",   # Disposal of  648 units of WAA.ASX from parcel "Third" 2011 Feb 17
2013 Feb 19,  WAA.ASX,         ANZ,        2345.31,    -2000,    4.69,     GST,     "Second",  # Disposal of 2000 units of WAA.ASX from parcel "Second" 2011 Feb 17
2013 Feb 19,  WAA.ASX,         ANZ,        1172.65,    -1000,    2.35,     GST,     "First",   # Disposal of 1000 units of WAA.ASX purchased on 2011 Feb 17
```

# Company Buy-Backs and Deemed Income #

Company buy-backs are another particular case with the accounting complication of a deemed value, for example the BHP Buyback of 2011 Apr 11 had
A deemed capital value of $49.88 per share with a total payment of $40.85 per share, so a deemed component of $9.03 : this can be dealt with
by employing a non-deductible expense to deduct the deemed component from the total income; eg
According to statement deemed tax value was $9.31 per share so $9.03 per share was not included in payment - deemed payment is a non deductible expense
```

# DATE        CREDIT ACCOUNT             DEBIT ACCOUNT         AMOUNT     UNITS   FRANKING CREDIT EX-DIVIDEND  DOCUMENTS            COMMENTS
#______________________________________________________________________________________________________________________________________________
2011 Apr 18,        BHP.ASX,                     DISPOSALS,   1554.77,  -167.000,                             [H],                  # Disposal of 167 units in BHP - deemed component $9.31
2011 Apr 18,      DISPOSALS, EXPENSE.NON-DEDUCTIBLE:DEEMED,   1508.01,                                                              # Non deductible expense - ($9.03 per share)
2011 Apr 18,      DISPOSALS,                          CASH,     46.76,                                        [BHP Share Buyback:], # BHP Capital payment (BHP Buyback) - $0.28 per share
2011 Apr 18,    DIV.BHP.ASX,                          CASH,   6775.19,                2903.65,   2011 Feb 25, [BHP Share Buyback:], # BHP Buyback Dividend
```

# Company Share Splits, Merges and Code Changes #

Control records can handle share splits and merges; for example
```
2017 Mar 17, [TFC Code Change]
CHANGE, TFC.ASX, ASSET.CAPITAL.SHARES
```
produces the code change
```
##
## Change TFC.ASX => QIN.ASX by factor    1.00
##   Date => 2017 Mar 17
##   TFC.ASX Cost            =>  28,718.09
##   TFC.ASX Units           =>  17000.000
##   TFC.ASX Qualified Units =>  17000.000
##   After Change
##   QIN.ASX Cost            =>  28,718.09
##   QIN.ASX Units           =>  17000.000
##   QIN.ASX Qualified Units =>  17000.000
##
```
whilst a share split
```
## Note the name change with the split; MLT.AX => MLT.ASX
2013 Oct 22, [MLT Split]
SPLIT, MLT.AX, ASSET.CAPITAL.SHARES:MLT.ASX
```
also requires a leaf name change and produces
```
##
## Split  MLT.AX => MLT.ASX by factor    5.00
##   Date => 2013 Oct 22
##   MLT.AX Cost            =>  47,823.15
##   MLT.AX Units           =>   2877.000
##   MLT.AX Qualified Units =>   2093.000
##   After Split
##   MLT.ASX Cost            =>  47,823.15
##   MLT.ASX Units           =>  14385.000
##   MLT.ASX Qualified Units =>  10465.000
##
```
As with any _control record_ the date is that carried from the previous transaction.
At the end of the financial year the old asset name will be shown as disposed
with zero gains; the new asset will be an accurate copy albeit with a possibly
changed number of units.


The *MERGE* and *SPLIT* actions are reciprocals; so
```
MERGE, ABC.AX, ASSET.CAPITAL.SHARES:ABC.ASX, 10.0
```
is identical to
```
SPLIT, ABC.AX, ASSET.CAPITAL.SHARES:ABC.ASX, 0.10
```
Similarly *CHANGE* is the same as
```
## A Code Change
SPLIT, ABC.AX, ASSET.CAPITAL.SHARES:ABC.ASX, 1.0
```

After creating a new asset name any associated dividend names
will also need redefining when used.
