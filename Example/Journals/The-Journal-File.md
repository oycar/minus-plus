## The Journal ##

# Transaction Records #

The main input file is the _journal_ consisting of lines of text where most lines describe transactions; for example a simple transaction is

```
2018 Jun 01,           CBA,     BANK.FEES,          10.00, # CBA Fees
```

Each transaction is comma-separated list of fields; the first field records the date in one of the several supported [formats](https://github.com/oycar/minus-plus/wiki/Date-Format).
Normally the next two fields record a credit account (from which money is paid out) and a debit account; which receives the payment. The next field is the amount transacted
(which in the above example is _$10.00_), with a final optional comment indicated by a _#_ symbol. So the above transaction records that _$10_ was
paid in bank fees from an account with the short name **CBA**  on June 1st, 2018.   

_Minus Plus_ creates accounts on  the fly when first encountered using their full name to decide what type of
account each is. The naming system is hierarchical with a dotted _class name_ followed by a colon and a unique
_leaf name_. On subsequent uses the leaf name alone can be used to identify the account.
There is some latitude in naming accounts, although certain rules need to be followed for accounts to
be drawn up properly.

Example bank account names might be as follows:

```
# CLASS.NAME:LEAF.NAME
#_________________
ASSET.CURRENT:CASH
ASSET.CURRENT.BANK:CBA
ASSET.CURRENT.BANK.CUA:SAVINGS.CUA
```

At the very least a bank account must have the leading components _class name_ **_ASSET.CURRENT_**; this means it is has top
level class **_ASSET_** and is a current asset, i.e. is liquid or able to be liquid within one year.
The leaf name must be unique and is the identifying name in later references to the account. The _class name_
can have more than the minimum **ASSET.CURRENT**
components; when financial statements are drawn up sub-headings will correspond to the
final component of the class name; so a good option for the class
name is **ASSET.CURRENT.BANK**; if there are many accounts
they could be further sub-classified; by bank for example **ASSET.CURRENT.BANK.CBA**.

All accounts are formed from
one of five top level classes;

* **EXPENSE** - An expense account
* **INCOME** - An income account
* **LIABILITY** - A liability account
* **ASSET** - An asset account
* **EQUITY** - An equity account

and these accounts are balanced using the
accounting equation
```
ASSET  = LIABILITY + INCOME + EXPENSE + EQUITY
```

# Comment Records #

As to the journal file itself other information can be provided apart from transactions.
If a line starts with one or more _#_ characters the whole line is a comment;

```
2010 Aug 31,       ESA.INT,        ESAVER,      62.41, # ESaver Interest
##
## September
##
2010 Sep 01,          CASH,  FARM.EXPENSE,     64.90, # Agency fees
```

# State Records #

Transactions and comments are the bulk of a journal file but other types of data are needed to complete a set of
accounts, these include _state records_ which is the internal format _Minus Plus_ uses to write out its internal state
in output _state files_; which can be used to record a set of accounts without the need of processing the
journal files (typically this is quicker). Whilst not designed as a data input format it is useful
to set certain fixed properties; such as a journal title, the location of documents linked to transactions and so on.


```
## KSH Journal
##
## Documents are stored in the following folder - link with [file-name]
<<, Document_Protocol, file://,>>
<<, Document_Root, /Users/me/CloudStation/Projects/Accounts/KSH Documents/,>>
<<, Journal_Title, KSH Pty Ltd,>>
<<, Journal_Currency, AUD,>>
<<, Journal_Type, PTY,>>
##
```

An important variable is the _Journal Type_, because this greatly effects the tax treatment of the accounts.
Possible values are

* **PTY**   Private Company
* **SMSF**  Self Managed Superannuation Fund
* **TRUST** Trust
* **IND**   Individual

Documents are useful to record transactions, and a _Minus Plus_ provides shorthands for standardized document
file names; if these are placed in the folder specified by **Document_Root** a hyperlink using the
protocol specified by **Document_Protocol** will be appended to the comments on the output
listing.

Apart from _state_records_ more fine control of how *Minus Plus* operates can be obtained with _control records_. Unlike
_state_records_ these can either interrogate or modify the accounts or set time varying properties like taxation rates
and levies. They can also indicate share splits, share merges and asset name changes.

For an  individual tax payer tax rates, levies and offsets are quite complex and can vary from year to year.
For example here they are for financial year 2018, (i.e 2017-2018) in Australia.

```
## An individual
#
##
## Tax Rates (2018)
## Defined using control records - which set values which can be time variable
## Tax Bands are defined as (Rate, Threshold) pairs, if a threshold is missing
## it defaults to zero;  
##         Tax Bands            1st Band  1st Threshold 2nd Band  2nd Threshold 3rd Band  3rd Threshold  4th Band  4th Threshold 5th Band  5th Threshold
#_______________________________________________________________________________________________________________________________________________________
SET_BANDS, Tax_Bands,               0,          0,         0.19,      18200,      0.325,    37000,        0.370,       87000,      0.450,      180000
SET_BANDS, Low_Income_Offset,       0,          0,         0.19,      18200,      0,        20542.1054,  -0.015,       37000,      0,           66666.6667
SET_BANDS, Medicare_Levy,           0,          0,         0.10,      21980,      0.02,     27475
```

# Import Records #


It was mentioned that security prices are required to allow market valuations; one
possible use of control records is to set security prices;
```
## Missing Prices
SET, PRICE, AB_RRF, 1.9344
SET, PRICE, AB_ILF, 1.0242
```
but whilst this is occasionaly useful this would soon overwhelm the journal file with thousands of lengthy entries. Instead prices
are best provided using _state record_ format. But this internal format is inconvenient for direct data entry. Therefore
_Minus Plus_ can import _csv_ data files containing time varying data, (such as security prices).


```
## Import Prices
## The fields
## These are the those needed for the Price import from the CBA (2019 format)
<<,  Key_Field, 1, >>
<<, Value_Field, 5, >>

## The key field is a date
<<, Key_is_Date, 1,>>
## The value field is not a date
<<, Value_is_Date ,  0,>>

# These are price data
<<,Import_Array_Name , Price ,>>

## The above information states that
## Price['Field 1'] => 'Field 5'
## And that 'Field 1' is a date field

## This asset is ASSET.CAPITAL.TRUSTS:AYF.ASX
<<,Asset_Prefix, ASSET.CAPITAL.TRUSTS,>>
<<,Asset_Symbol, AYF.ASX,>>

## Provided price data is surrounded by %% symbols
## Any date format readable by Minus Plus is ok
## The above fields
%%
28/10/2019,6.18,6.2,6.18,6.2,0.02,0.32,11409
25/10/2019,6.18,6.18,6.18,6.18,0.08,1.31,3419
%%
```

# State Files #

Whilst these data can be used directly it is useful to consolidate data
into state files. Suppose we have a price file in state file format; _latest_prices.data_
and a journal file _journal.jrn_, and data downloaded from a data source
like a broker with price data _price_history.csv_ and a suitable header;
we can run
```
mpx -w new_prices.data -v Price price_history.csv latest_prices.data journal.jrn
```
which produces a new updated price file _new_prices.data_. Prices which so not
correpond to active assets within the journal are not saved. The option *-v Price*
meant only price data was written to the output state file specified by the *-w* flag.


If instead of just prices all data were required in the state file then simply
omit the *-v* flag
```
mpx -w new_state.data price_history.csv latest_prices.data journal.jrn
```
Now _new_state.data_ contains the entire state file for the journal along with
price data and is self sufficient. However, unlike the journal files it is
not easily readable nor editable if changes are desired.

# Types of Accounts #

We noted basic account structure earlier; in order to produce tax and financial
records accurately some types of account must have certain class names. The important
class names are

* **ASSET.CURRENT** - a current asset, i.e. a liquid asset or one that will be liquid within twelve months
* **ASSET.TERM**  - an asset with such a term depost or loan with a fixed term; note that if the term is within twelve months
_Minus Plus_ will reassign this asset to the _ASSET.CURRENT_ class automatically
* **ASSET.CAPITAL** - an asset subject to capital gains (and by definition not a current asset). These are unitized assets
* **ASSET.FIXED** - a depreciating asset - so not subject to capital gains. These are unitized assets.

* **LIABILITY.CURRENT** - a current liability
* **LIABILITY.TERM** - A liability with a fixed term, such as a loan. Again when within twelve months of maturity this would be reassigned to the _LIABILITY.CURRENT_ class.
* **LIABILITY.MEMBER** - Accounts used to track the members' interests in a superannuation fund
* **LIABILITY.MEMBER.STREAM** - A liability supporting a SMSF member's income steam

* **INCOME.CONTRIBUTION** - payments made by SMSF members to the fund
* **INCOME.EXEMPT** - income exempt of tax
* **INCOME.DIVIDEND** - A dividend payment; subject to dividend qualification rules
* **INCOME.DISTRIBUTION** - identifying income as a trust distribution
* **INCOME.FOREIGN** - foreign sourced income

* **EXPENSE.NON-DEDUCTIBLE** - a non deductible expense
* **EXPENSE.DIVIDEND** - A dividend paid by a company
* **EXPENSE.BENEFIT** - A benefit paid by a SMSF to a member

So for example a share holding in a company _BHP_ must commence with the name **ASSET.CAPITAL** and record units bought and sold.
The rest of the name can be freely chosen; but when financial statements are drawn up the last component of the class name
will be used as a sub-heading; therefore a descriptive class name could be **ASSET.CAPITAL.SHARES**. This class name should
then be used for other share holdings if they are to be displayed together in the financial statements. A full name could be
**ASSET.CAPITAl.SHARES:BHP.ASX** for example.

# Predefined Accounts #

Furthermore some accounts are predefined within _Minus Plus_
These are

* **EXPENSE.DEPRECIATION:DEPRECIATION** The depreciation account
* **INCOME.APPRECIATION:APPRECIATION.SOLD** Appreciation on disposal of a depreciating asset
* **EXPENSE.DEPRECIATION:DEPRECIATION.SOLD** Depreciation on disposal of a depreciating asset
* **LIABILITY.TAX:DEFERRED.TAX** Deferred tax owed.
* **LIABILITY.TAX:TAX** Income tax owed.
* **LIABILITY.TAX:RESIDUAL** An internal account for rounding error accumulation in tax payments.
(Tax office paperwork is often rounded; _Minus Plus_ does not round.)
* **LIABILITY.TAX:TAX.GST** An internal account for recording GST owed
* **LIABILITY.TAX:FRANKING.TAX** Franking tax owed under the imputation system by  company
* **ASSET.CURRENT.TAX:TAX.WITHOLDING** Withholding tac paid.
* **ASSET.CURRENT.TAX:TAX.PAYG** Tax installments paid.
* **INCOME.GAINS.REALIZED:GAINS**       Realized capital gains
* **EXPENSE.LOSSES.REALIZED:LOSSES**    Realized capital losses
* **EXPENSE.UNREALIZED:MARKET.CHANGES** Unrealized market changes


# The Special Class #

For tax offsets (such as franking), taxable gains and to make balancing adjustments with
regard to future liabilities and assets _Minus Plus_ maintains a _SPECIAL_ account class.
Within this class are several predefined accounts;

* **SPECIAL.OFFSET.NO_CARRY:NO_CARRY.OFFSETS** - Tax offsets which cannot be carried forward to later years
* **SPECIAL.OFFSET.CARRY:CARRY.OFFSETS** - Tax offset which can be carried forward to later years
* **SPECIAL.OFFSET.REFUNDABLE:REFUNDABLE.OFFSETS** Tax offsets which are refundable
* **SPECIAL.FRANKING:FRANKING** The Franking account balance
* **SPECIAL.FRANKING:FRANKING.PAID** Franking sub-account showing disbursed credits
* **SPECIAL.FRANKING:FRANKING.STAMPED** Franking sub-account showing net tax paid
* **SPECIAL.FRANKING.OFFSET:FRANKING.DEFICIT** The franking deficit account
* **SPECIAL.TAX:LIC.CREDITS** LIC Deduction account
* **SPECIAL.ACCOUNT:NULL** A null account which does not take part in any balancing

These accounts should typically not be used in a transaction file; although it is necesary
to explicitly set the various tax offsets explicitly occasionally. For example
```
2018 Jun 30, REFUNDABLE.OFFSETS, NULL,  400.00, # Set the value of refundable offsets to $400
```
Offsets are a tax credit; they should be inserted in the credit account field.


# Linked Accounts #

From the above example if a dividend is to be received from a company the account describing
it must begin with class **INCOME.DIVIDEND**. Furthermore it needs to be linked to its underlying
asset so that dividend qualification for can be carried out correctly. So if we have
company account **ASSET.CAPITAL.SHARES:BHP.ASX** then a dividend paid out by the company should
be credited to account **INCOME.DIVIDEND:DIV.BHP.ASX** (although their could be extra name
components at the end of the class name.) Thus

```
## DATE                             CREDIT ACCOUNT               DEBIT ACCOUNT      AMOUNT        FRANKING CREDIT EX-DIVIDEND DATE   STATEMENT   COMMENTS
##___________________________________________________________________________________________________________________________________________________________________________________________________
2008 Mar 18,          INCOME.DIVIDEND:DIV.BHP.ASX,                    CASH,         663.02,         284.15,          2008 Feb 25,       [D],     # BHP 356th Dividend
##
```

Linked accounts to
* **ASSET.CAPITAL:LEAF.NAME**

are

* **INCOME.DISTRIBUTION** _DIST.LEAF.NAME_
* **INCOME.DISTRIBUTION** _DIST.LEAF.NAME_
* **INCOME.FOREIGN**      _FOR.LEAF.NAME_
* **INCOME.GAINS.NET**    _GAINS.LEAF.NAME_

The last _GAINS_ linkage is used explicitly only for distributed net capital gains from a managed fund or trust.

# Putting it Together #

A few more details are needed before we can generate the first set of accounts. _Minus Plus_ will concatenate
any _state_ or _journal_ files to produce the accounts. These files should not overlap, that is try and
process the same transaction twice; and _state_ files cannot occur after _journal_ files because a _state_
file is always a complete record of the accounts up to when it was written. If no _state_ file is present the first
journal file must include, before any transactions are processed, the _control_record_ and an indication of the
initial financial year

```
## Set the financial year to that ending in 2008 and start processing the transactions

SET_FINANCIAL_YEAR, 2008
START_JOURNAL
```

The _example.jrn_ file gives a short example of a journal file; it begins like this:

```
2008 Jan 21, INCOME.CONTRIBUTION.TAXABLE:WILKINS.CON,  ASSET.CURRENT.BANK:CASH,          10.00,               # Contribution
2008 Feb 08,                                    CASH,  EXPENSE.GENERAL:BANK.FEES,        10.00,               # Bank Fees
2008 Feb 20,                             WILKINS.CON,  ASSET.CURRENT.BANK:ANZ,           55.50,               # Contribution
2008 Feb 20,   INCOME.CONTRIBUTION.TAX-FREE:EMMA.NON,  ANZ,                           80320.44,               # Contribution
2008 Feb 20,                                     ANZ,  ASSET.CAPITAL.SHARES:BHP.ASX,  80370.44,  2076, 50.00, # BHP Shares Initial Investment
```

The entries are lengthy because at the beginning almost every account is new. Later things become more concise;
```
2008 May 21,                       CASH,  ESAVER,     190.00,   # Transfer to ESAVER
2008 May 31,                    ESA.INT,  ESAVER,    1557.40,   # Interest (ESaver)
2008 Jun 06,                  EMMA.WORK,   CASH,    14022.36,   # Employer Contribution
2008 Jun 07,                       CASH,  ESAVER,   14023.00,   # Transfer to ESAVER
2008 Jun 12, INCOME.INTEREST:TERM00.INT,  ESAVER,    1314.47,   # Interest (CUA Term Deposit)
2008 Jun 12,                 TERM00.CUA,  ESAVER,   70000.00,   # Close term deposit
```

The _example.jrn_ journal file along with the example header *_example_header.jrn* and the example data file _latest_data.mpxd_
can be used to produce some simple accounts.
With the appropriate entries commented out in the header we obtain we can run _Minus Plus_ using the _mpx_ on  the command line.
The simplest option using all files is
```
mpx Import/latest_data.mpxd Journals/*.jrn
```
which will write the transaction list to the standard output, and financial statements and any errors or logging information
to the standard output. We can therefore catch these streams in output files if we wish
```
mpx Import/latest_data.mpxd Journals/*.jrn 2> reports.txt > listing.txt

```
Financial reports go the _reports.txt_ and the transactions to _listing.txt_

Control can be held over which financial reports are produced; they are made at the end of each financial year and the
flags
* **mpx -A** All Reports
* **mpx -B** Statement of financial position (Balance Sheet)
* **mpx -C** Capital Gains Report  
* **mpx -D** Depreciation Report  
* **mpx -I** Imputation (Franking Credits) Report
* **mpx -M** Market Gains (and Deferred Gains) Report
* **mpx -O** Operating Statement (Profit and Loss)
* **mpx -Q** Dividend Qualification Report
* **mpx -T** Statement of Taxable Position
* **mpx -Z** No Reports

Furthermore extra detail can be obtained in many cases by using the _mpx -x_ flag;
this _mpx -xBMOT_ would yield four reports, with extra detail where possible.


## Some Example Reports

The provided example is quite simple; here are some more complex reports

**A Dividend Qualification Report** showing one non fully qualified payment
```
Dividends
      Asset   Ex-Div Date Eligible Units   Payment Date      Qualified    % Qualified       Payment
     _______________________________________________________________________________________________
     WLE.ASX  2018 Apr 12    31667.000      2018 Apr 27      31667.000       100.00          791.68
     WLE.ASX  2017 Nov 23    31667.000      2017 Dec 06      31667.000       100.00          633.34
   WHFPB.ASX  2018 May 23      191.000      2018 Jun 12        191.000       100.00          668.50
   WHFPB.ASX  2017 Nov 24      191.000      2017 Dec 12        191.000       100.00          668.50
     WHF.ASX  2018 May 23     6710.000      2018 Jun 12       6710.000       100.00          603.90
     WHF.ASX  2017 Nov 24     6710.000      2017 Dec 12       6710.000       100.00          587.12
     WAA.ASX  2018 Apr 12    58500.000      2018 Apr 27      58500.000       100.00        1,667.25
     WAA.ASX  2017 Oct 17    42500.000      2017 Oct 27      42500.000       100.00        1,168.75
     TLS.ASX  2018 Feb 28     8000.000      2018 Mar 29       8000.000       100.00          880.00
     TGG.ASX  2017 Sep 07    40000.000      2017 Sep 22      40000.000       100.00        1,800.00
     MQG.ASX  2017 Nov 07      154.000      2017 Dec 13        154.000       100.00          315.70
     MQG.ASX  2017 May 16      154.000      2017 Jul 03        154.000       100.00          431.20
     MLT.ASX  2018 Feb 14    14121.000      2018 Mar 01      14121.000       100.00        1,242.65
     MLT.ASX  2017 Aug 10    10700.000      2017 Sep 05      10700.000       100.00        1,070.00
     FSI.ASX  2018 Feb 26    28000.000      2018 Mar 13      28000.000       100.00        1,050.00
     FSI.ASX  2017 Aug 17    28000.000      2017 Sep 13      28000.000       100.00        1,050.00
     DUI.ASX  2018 Feb 22    17828.000      2018 Mar 14      17828.000       100.00        1,158.82
     DUI.ASX  2017 Aug 30    14600.000      2017 Sep 22      14600.000       100.00        1,168.00
     CIN.ASX  2018 Feb 28     2000.000      2018 Mar 20       2000.000       100.00        1,020.00
     CIN.ASX  2017 Aug 31     2000.000      2017 Sep 25       2000.000       100.00        1,360.00
     CDM.ASX  2018 Apr 09    85158.000      2018 Apr 23      85158.000       100.00        3,406.32
     CDM.ASX  2017 Sep 08    40244.000      2017 Sep 18      40244.000       100.00        1,609.76
     CBA.ASX  2018 Feb 14      605.000      2018 Mar 28        605.000       100.00        1,210.00
     CAM.ASX  2018 Apr 03    70000.000      2018 Apr 27      70000.000       100.00          875.00
     CAM.ASX  2018 Jan 03    70000.000      2018 Jan 25      70000.000       100.00          875.00
     CAM.ASX  2017 Oct 03    50000.000      2017 Oct 27      50000.000       100.00          625.00
     CAM.ASX  2017 Jul 06    50000.000      2017 Jul 28      50000.000       100.00          625.00
     BST.ASX  2018 Mar 08    57673.000      2018 Mar 23      57673.000       100.00          720.91
     BST.ASX  2017 Sep 05    87673.000      2017 Sep 23      77673.000        88.59        1,315.10
     BHP.ASX  2018 Mar 08     4700.000      2018 Mar 27       4700.000       100.00        3,317.51
     BHP.ASX  2017 Sep 07     4700.000      2017 Sep 26       4700.000       100.00        2,488.61
     AUI.ASX  2018 Feb 22     7844.000      2018 Mar 14       7844.000       100.00        1,255.04
     AUI.ASX  2017 Aug 30     3450.000      2017 Sep 23       3450.000       100.00          638.25
     ARG.ASX  2018 Feb 16     5650.000      2018 Mar 09       5650.000       100.00          875.75
     ARG.ASX  2017 Aug 25     5650.000      2017 Sep 15       5650.000       100.00          904.00
     AMH.ASX  2017 Aug 07    47500.000      2017 Aug 24      47500.000       100.00        1,662.50
     ALI.ASX  2018 Mar 02    22632.000      2018 Mar 23      22632.000       100.00          282.90
     ALI.ASX  2017 Sep 08    22632.000      2017 Sep 29      22632.000       100.00          565.80
     AFI.ASX  2018 Feb 08    11533.000      2018 Feb 23      11533.000       100.00        1,153.30
     AFI.ASX  2017 Aug 08     4400.000      2017 Aug 30       4400.000       100.00          616.00
     _______________________________________________________________________________________________
     Qualified Dividends     44,207.16                                        99.97       44,357.19
```

**A detailed capital gains report** Gains and losses are classified as _LONG_ (made on assets held more
than the _CGT_Threshold_, normally one year) and _SHORT_, (made on assets held less than this period).
Taxable gains and losses are adjusted for capital returns, deferred gains etc.

```
No Name Pty Ltd
Capital Gains Report for Period Ending 2019 Jun 30
        ____________________________________________
	   Accounting Capital Gains =>     20,083.16
	  Accounting Capital Losses =>      8,552.52
        ____________________________________________
	      Net Accounting Gains  =>     11,530.64

	Adjusted Gains

	        Long Adjusted Gains =>     18,970.10
	       Short Adjusted Gains =>      1,113.06

	       Long Adjusted Losses =>      4,867.47
	      Short Adjusted Losses =>      3,685.05


	Capital Gains Before Application of Losses or Discounts

	        Total Capital Gains =>     20,083.16
        ____________________________________________

After Application of Long Losses
	                 Long Gains =>     14,102.63
	                Long Losses =>          0.00
        ____________________________________________

After Application of Short Losses
	                Short Gains =>          0.00
	               Short Losses =>      2,571.99
        ____________________________________________
	        Net Adjusted Gains  =>     11,530.64
	     Losses Brought Forward =>     18,631.09
	      Total Adjusted Losses =>      7,100.45
        ____________________________________________


	     Losses Carried Forward =>      7,100.45

No Name Pty Ltd
Realized Gains Report for Period Ending 2019 Jun 30

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         AYF.ASX       0    2000.000      11,900.00  2018 Sep 05   2019 May 08        5.80      11,600.00      11,737.09      11,737.09        (137.09)   Short Losses       (137.09)        5.9500
                       1    2257.000      13,338.87  2018 Oct 11   2019 May 08        5.80      13,090.60      13,121.23      13,121.23         (30.63)   Short Losses        (30.63)        5.9100
                       2    1743.000      10,301.13  2018 Oct 12   2019 May 08        5.80      10,109.40      10,163.00      10,163.00         (53.60)   Short Losses        (53.60)        5.9100
                       3    2000.000      11,760.00  2018 Oct 17   2019 May 08        5.80      11,600.00      11,597.09      11,597.09           2.91    Short Gains           2.91         5.8800
                       4     410.000       2,398.50  2018 Oct 26   2019 May 08        5.80       2,378.00       2,378.91       2,378.91          (0.91)   Short Losses         (0.91)        5.8500
                       5    2407.000      14,129.09  2018 Nov 05   2019 May 08        5.80      13,960.60      13,906.98      13,906.98          53.62    Short Gains          53.62         5.8700
                       6    1385.000       8,116.10  2018 Nov 12   2019 May 08        5.80       8,033.00       7,982.55       7,982.55          50.45    Short Gains          50.45         5.8600
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           12202.000      71,943.69  2018 Sep 05   2019 May 08        5.80      70,771.60      70,886.86      70,886.86        (115.26)   Short Losses       (222.24)
                                                                                                                                                          Short Gains         106.98

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         WLE.ASX       0   31667.000      34,833.70  2017 Nov 10   2019 May 20        1.13      35,740.77      34,833.70      34,833.70         907.07    Long Gains          907.07         1.1000
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           31667.000      34,833.70  2017 Nov 10   2019 May 20        1.13      35,740.77      34,833.70      34,833.70         907.07    Long Gains          907.07

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
       WHFPB.ASX       0     191.000      21,013.82  2016 Jul 27   2018 Dec 03      100.00      19,100.00      21,036.93      21,036.93      (1,936.93)   Long Losses      (1,936.93)      110.0200
      ______________________________________________________________________________________________________________________________________________________________________________________________
                             191.000      21,013.82  2016 Jul 27   2018 Dec 03      100.00      19,100.00      21,036.93      21,036.93      (1,936.93)   Long Losses      (1,936.93)

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         TLS.ASX       0    8000.000      28,320.00  2017 Nov 02   2018 Oct 24        3.12      24,970.05      28,351.15      28,351.15      (3,381.10)   Short Losses     (3,381.10)        3.5400
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           27825.000      88,423.65  2017 Nov 02   2018 Oct 24        3.12      24,970.05      28,351.15      28,351.15      (3,381.10)   Short Losses     (3,381.10)

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         MQG.ASX       0      98.000       7,575.89  2016 Aug 10   2019 May 08      124.03      12,154.94       7,523.64       7,523.64       4,631.30    Long Gains        4,631.30        77.3050
                       2       2.000         331.10  2018 Dec 18   2019 May 08      124.03         248.06         329.77         329.77         (81.71)   Short Losses        (81.71)      165.5500
      ______________________________________________________________________________________________________________________________________________________________________________________________
                             156.000      12,236.07  2016 Aug 10   2019 May 08      124.03      12,403.00       7,853.42       7,853.42       4,549.58    Short Losses        (81.71)
                                                                                                                                                          Long Gains        4,631.30

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         FSI.ASX       0   14948.000      23,723.10  2016 Sep 29   2019 May 08        1.73      25,785.30      23,749.19      23,749.19       2,036.11    Long Gains        2,036.11         1.5870
                       1      52.000          82.68  2016 Sep 30   2019 May 08        1.73          89.70          82.78          82.78           6.92    Long Gains            6.92         1.5900
                       2    3000.000       4,605.00  2016 Oct 27   2019 May 08        1.73       5,175.00       4,624.95       4,624.95         550.05    Long Gains          550.05         1.5350
                       3   10000.000      16,000.00  2017 Mar 24   2019 May 08        1.73      17,250.00      16,019.95      16,019.95       1,230.05    Long Gains        1,230.05         1.6000
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           28000.000      44,410.78  2016 Sep 29   2019 May 08        1.73      48,300.00      44,476.87      44,476.87       3,823.13    Long Gains        3,823.13

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         CIN.ASX       0     609.000      19,661.33  2017 Aug 10   2018 Oct 25       31.15      18,971.94      19,681.13      19,681.13        (709.19)   Long Losses        (709.19)       32.2846
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            4000.000     125,180.81  2017 Aug 10   2018 Oct 25       31.15      18,971.94      19,681.13      19,681.13        (709.19)   Long Losses        (709.19)

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         CDM.ASX       0    9302.000      11,720.52  2017 Aug 10   2018 Oct 31        1.07       9,979.70      11,733.41      11,733.41      (1,753.71)   Long Losses      (1,753.71)        1.2600
      ______________________________________________________________________________________________________________________________________________________________________________________________
                          100158.000     129,763.95  2017 Aug 10   2018 Oct 31        1.07       9,979.70      11,733.41      11,733.41      (1,753.71)   Long Losses      (1,753.71)

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
        CAMG.ASX       0    8334.000       8,000.64  2017 Dec 14   2018 Oct 25        1.00       8,314.05       8,000.64       8,000.64         313.41    Short Gains         313.41         0.9600
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            8334.000       8,000.64  2017 Dec 14   2018 Oct 25        1.00       8,314.05       8,000.64       8,000.64         313.41    Short Gains         313.41

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         BST.ASX       1    8923.000       8,709.41  2016 Oct 04   2018 Oct 23        0.93       8,253.78       8,721.41       8,721.41        (467.64)   Long Losses        (467.64)        0.9761
                       2    7000.000       6,440.00  2016 Oct 31   2018 Oct 23        0.93       6,475.00       6,459.95       6,459.95          15.05    Long Gains           15.05         0.9200
                       3    6189.000       5,260.65  2017 Mar 14   2018 Oct 23        0.93       5,724.83       5,280.60       5,280.60         444.23    Long Gains          444.23         0.8500
                       4    5561.000       4,726.85  2017 Mar 15   2018 Oct 23        0.93       5,143.93       4,726.85       4,726.85         417.08    Long Gains          417.08         0.8500
                       5   10000.000       8,700.00  2017 May 19   2018 Oct 23        0.93       9,250.00       8,719.95       8,719.95         530.05    Long Gains          530.05         0.8700
                       6   10000.000       8,650.00  2017 Aug 04   2018 Oct 23        0.93       9,250.00       8,669.95       8,669.95         580.05    Long Gains          580.05         0.8650
                       7   10000.000       8,800.00  2017 Sep 01   2018 Oct 23        0.93       9,250.00       8,819.95       8,819.95         430.05    Long Gains          430.05         0.8800
                       8   10869.000       9,999.48  2018 Oct 25   2019 May 08        0.95      10,325.55      10,019.43      10,019.43         306.12    Short Gains         306.12         0.9200
                       9    4775.000       4,345.25  2018 Oct 31   2019 May 08        0.95       4,536.25       4,365.20       4,365.20         171.05    Short Gains         171.05         0.9100
                      10    4310.000       3,879.00  2018 Nov 01   2019 May 08        0.95       4,094.50       3,879.00       3,879.00         215.50    Short Gains         215.50         0.9000
      ______________________________________________________________________________________________________________________________________________________________________________________________
                          107627.000      98,792.53  2016 Oct 04   2019 May 08        0.95      72,303.83      69,662.29      69,662.29       2,641.54    Short Gains         692.67
                                                                                                                                                          Long Losses        (467.64)
                                                                                                                                                          Long Gains        2,416.50

          Asset     Parcel   Units           Cost        From           To         Price         Proceeds       Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         BHP.ASX       2     700.000      15,582.00  2017 Jun 21   2018 Oct 24       32.58      22,804.05      15,611.95      15,611.95       7,192.10    Long Gains        7,192.10        22.2600
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            5425.000     126,842.17  2016 Sep 30   2018 Oct 24       32.58      22,804.05      15,611.95      15,611.95       7,192.10    Long Gains        7,192.10

      ______________________________________________________________________________________________________________________________________________________________________________________________
      ______________________________________________________________________________________________________________________________________________________________________________________________
          Totals                         761,441.81                                            343,658.99     332,128.35     332,128.35      11,530.64    Short Losses     (3,685.05)
                                                                                                                                                          Short Gains       1,113.06
                                                                                                                                                          Long Losses      (4,867.47)
                                                                                                                                                          Long Gains       18,970.10

```                                                                                                                                                          
**A Detailed Market Gains Report** for 2014. This report is useful in planning asset disposals using the manual method to minimize (or maximize for that matter) tax liability, since
parcels can be selected according  to their gain, loss and whether they qualify as _LONG_ or _SHORT_ gains and losses.
```
What The Dickens Superannuation Fund
Market Gains Report for Period Ending 2010 Jun 30

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         HHY.ASX       0    2810.000       2,795.95  2008 Nov 17   2010 Jul 01        1.16       3,264.07       2,882.60       2,882.60         381.46    Long Gains          381.46         0.9950
                       1    1190.000       1,184.05  2008 Nov 18   2010 Jul 01        1.16       1,382.29       1,207.75       1,207.75         174.55    Long Gains          174.55         0.9950
                       2    3980.000       3,920.30  2008 Nov 27   2010 Jul 01        1.16       4,623.13       4,025.13       4,025.13         598.00    Long Gains          598.00         0.9850
                       3    4020.000       3,959.70  2008 Nov 28   2010 Jul 01        1.16       4,669.59       4,039.75       4,039.75         629.84    Long Gains          629.84         0.9850
                       4    6432.000       6,335.52  2009 Jan 28   2010 Jul 01        1.16       7,471.35       6,494.30       6,494.30         977.05    Long Gains          977.05         0.9850
                       5    5860.000       8,031.85  2010 Apr 27   2010 Jul 01        1.16       6,806.92       8,148.54       8,148.54      (1,341.62)   Short Losses     (1,341.62)        1.3706
                       6     539.000         681.98  2010 May 10   2010 Jul 01        1.16         626.10         692.71         692.71         (66.62)   Short Losses        (66.62)        1.2653
                       7    5000.000       5,807.95  2010 May 28   2010 Jul 01        1.16       5,807.95       5,907.51       5,907.51         (99.56)   Short Losses        (99.56)        1.1616
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           29831.000      32,717.30  2008 Nov 17   2010 Jul 01        1.16      34,651.39      33,398.28      33,398.28       1,253.11    Short Losses     (1,507.79)
                                                                                                                                                          Long Gains        2,760.91

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         CPA.ASX       1    6450.000       5,929.20  2009 Oct 30   2010 Jul 01        0.92       5,950.12       5,870.85       5,870.85          79.27    Short Gains          79.27         0.9193
                       2   13186.000      12,164.07  2009 Dec 10   2010 Jul 01        0.92      12,164.07      12,044.78      12,044.78         119.29    Short Gains         119.29         0.9225
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           19636.000      18,093.27  2009 Oct 13   2010 Jul 01        0.92      18,114.19      17,915.62      17,915.62         198.56    Short Gains         198.56

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         AYT.ASX       1      57.000          71.53  2008 Nov 24   2010 Jul 01        1.55          88.18          68.88          68.88          19.30    Long Gains           19.30         1.2550
                       2    5000.000       6,225.00  2008 Dec 15   2010 Jul 01        1.55       7,735.01       6,007.60       6,007.60       1,727.42    Long Gains        1,727.42         1.2450
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            5057.000       6,296.53  2008 Nov 24   2010 Jul 01        1.55       7,823.19       6,076.48       6,076.48       1,746.71    Long Gains        1,746.71

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         AYF.ASX       1     461.000       3,157.85  2008 Nov 17   2010 Jul 01        6.50       2,995.98       3,040.57       3,040.57         (44.59)   Long Losses         (44.59)        6.8500
                       2     470.000       3,149.00  2008 Nov 18   2010 Jul 01        6.50       3,054.47       3,028.83       3,028.83          25.64    Long Gains           25.64         6.7000
                       3    1000.000       6,050.00  2008 Dec 01   2010 Jul 01        6.50       6,498.88       5,761.96       5,761.96         736.92    Long Gains          736.92         6.0500
                       4     720.000       3,960.00  2008 Dec 29   2010 Jul 01        6.50       4,679.19       3,759.58       3,759.58         919.61    Long Gains          919.61         5.5000
                       5     370.000       2,035.00  2008 Dec 30   2010 Jul 01        6.50       2,404.59       1,916.23       1,916.23         488.35    Long Gains          488.35         5.5000
                       6    2542.000      15,000.00  2009 Aug 20   2010 Jul 01        6.50      16,520.15      14,567.32      14,567.32       1,952.83    Short Gains       1,952.83         5.9009
                       7     107.000         695.38  2010 Apr 16   2010 Jul 01        6.50         695.38         677.17         677.17          18.21    Short Gains          18.21         6.4989
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            5670.000      34,047.23  2008 Nov 04   2010 Jul 01        6.50      36,848.64      32,751.67      32,751.67       4,096.97    Short Gains       1,971.04
                                                                                                                                                          Long Losses         (44.59)
                                                                                                                                                          Long Gains        2,170.52

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         WHF.ASX       1     700.000       1,808.93  2009 Jul 09   2010 Jul 01        2.98       2,082.50       1,808.93       1,808.93         273.57    Short Gains         273.57         2.5842
                       2    2000.000       6,172.95  2010 May 28   2010 Jul 01        2.98       5,950.00       6,172.95       6,172.95        (222.95)   Short Losses       (222.95)        3.0865
                       3      20.000          59.50  2010 May 31   2010 Jul 01        2.98          59.50          59.50          59.50           0.00    Zero Gains            0.00         2.9750
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            2720.000       8,041.38  2009 Jul 09   2010 Jul 01        2.98       8,092.00       8,041.38       8,041.38          50.62    Short Losses       (222.95)
                                                                                                                                                          Short Gains         273.57

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         WFL.ASX       0   21700.000       8,061.95  2010 Apr 22   2010 Jul 01        0.37       8,061.95       8,061.95       8,061.95           0.00    Zero Gains            0.00         0.3715
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           21700.000       8,061.95  2010 Apr 22   2010 Jul 01        0.37       8,061.95       8,061.95       8,061.95           0.00

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         WAX.ASX       0    8250.000       5,931.70  2010 Feb 03   2010 Jul 01        0.72       5,931.70       5,931.70       5,931.70           0.00    Zero Gains            0.00         0.7190
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            8250.000       5,931.70  2010 Feb 03   2010 Jul 01        0.72       5,931.70       5,931.70       5,931.70           0.00

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         WAM.ASX       1    6000.000       5,070.00  2009 Feb 16   2010 Jul 01        1.26       7,590.00       5,086.57       5,086.57       2,503.43    Long Gains        2,503.43         0.8450
                       3    2434.000       3,124.13  2010 Apr 27   2010 Jul 01        1.26       3,079.01       3,124.13       3,124.13         (45.12)   Short Losses        (45.12)        1.2835
                       4    3866.000       4,890.49  2010 Apr 29   2010 Jul 01        1.26       4,890.49       4,890.49       4,890.49           0.00    Zero Gains            0.00         1.2650
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           12300.000      13,084.62  2009 Feb 16   2010 Jul 01        1.26      15,559.50      13,101.19      13,101.19       2,458.31    Short Losses        (45.12)
                                                                                                                                                          Long Gains        2,503.43

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         WAA.ASX       0    6015.000       6,012.38  2010 May 28   2010 Jul 01        1.00       6,012.38       6,012.38       6,012.38           0.00    Zero Gains            0.00         0.9996
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            6015.000       6,012.38  2010 May 28   2010 Jul 01        1.00       6,012.38       6,012.38       6,012.38           0.00

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         TLS.ASX       0    1900.000       6,093.95  2009 Oct 13   2010 Jul 01        3.05       5,788.30       6,093.95       6,093.95        (305.65)   Short Losses       (305.65)        3.2073
                       1    1820.000       6,033.45  2009 Oct 29   2010 Jul 01        3.05       5,544.58       6,033.45       6,033.45        (488.87)   Short Losses       (488.87)        3.3151
                       2    1850.000       6,100.95  2010 Feb 16   2010 Jul 01        3.05       5,635.98       6,100.95       6,100.95        (464.97)   Short Losses       (464.97)        3.2978
                       3    2000.000       6,092.95  2010 Mar 11   2010 Jul 01        3.05       6,092.95       6,092.95       6,092.95           0.00    Zero Gains            0.00         3.0465
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            7570.000      24,321.30  2009 Oct 13   2010 Jul 01        3.05      23,061.82      24,321.30      24,321.30      (1,259.48)   Short Losses     (1,259.48)

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         NAB.ASX       1     232.000       6,102.07  2010 May 06   2010 Jul 01       26.30       6,102.07       6,102.07       6,102.07           0.00    Zero Gains            0.00        26.3020
      ______________________________________________________________________________________________________________________________________________________________________________________________
                             232.000       6,102.07  2009 Nov 09   2010 Jul 01       26.30       6,102.07       6,102.07       6,102.07           0.00

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         DUI.ASX       4    1745.000       3,490.00  2009 Apr 30   2010 Jul 01        3.02       5,275.05       3,490.00       3,490.00       1,785.05    Long Gains        1,785.05         2.0000
                       5    1000.000       3,022.95  2010 May 28   2010 Jul 01        3.02       3,022.95       3,022.95       3,022.95           0.00    Zero Gains            0.00         3.0229
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            2745.000       6,512.95  2009 Feb 11   2010 Jul 01        3.02       8,298.00       6,512.95       6,512.95       1,785.05    Long Gains        1,785.05

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         CTX.ASX       0     666.000       6,393.60  2009 Mar 24   2010 Jul 01        8.94       5,954.04       6,424.30       6,424.30        (470.26)   Long Losses        (470.26)        9.6000
                       1     584.000       6,053.99  2009 Nov 17   2010 Jul 01        8.94       5,220.96       6,053.99       6,053.99        (833.03)   Short Losses       (833.03)       10.3664
                       2    1100.000       9,834.00  2010 Feb 16   2010 Jul 01        8.94       9,834.00       9,884.00       9,884.00         (50.00)   Short Losses        (50.00)        8.9400
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            2350.000      22,281.59  2009 Mar 24   2010 Jul 01        8.94      21,009.00      22,362.29      22,362.29      (1,353.29)   Short Losses       (883.03)
                                                                                                                                                          Long Losses        (470.26)

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         BSL.ASX       1     900.000       2,079.00  2009 May 01   2010 Jul 01        2.46       2,210.53       2,089.63       2,089.63         120.90    Long Gains          120.90         2.3100
                       2    2600.000       4,030.00  2009 May 28   2010 Jul 01        2.46       6,385.97       4,030.00       4,030.00       2,355.97    Long Gains        2,355.97         1.5500
                       4     259.000         714.22  2010 Jan 28   2010 Jul 01        2.46         636.14         714.22         714.22         (78.08)   Short Losses        (78.08)        2.7576
                       5    2041.000       5,012.99  2010 May 05   2010 Jul 01        2.46       5,012.99       5,012.99       5,012.99           0.00    Zero Gains            0.00         2.4561
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            5800.000      11,836.21  2009 May 01   2010 Jul 01        2.46      14,245.64      11,846.84      11,846.84       2,398.79    Short Losses        (78.08)
                                                                                                                                                          Long Gains        2,476.88

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         BLD.ASX       1    1034.000       5,813.01  2010 Jan 21   2010 Jul 01        5.48       5,666.27       5,813.01       5,813.01        (146.74)   Short Losses       (146.74)        5.6219
                       2    1100.000       6,027.95  2010 May 28   2010 Jul 01        5.48       6,027.95       6,027.95       6,027.95           0.00    Zero Gains            0.00         5.4800
      ______________________________________________________________________________________________________________________________________________________________________________________________
                            2134.000      11,840.96  2009 Oct 19   2010 Jul 01        5.48      11,694.22      11,840.96      11,840.96        (146.74)   Short Losses       (146.74)

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         BHP.ASX       5     144.000       5,571.36  2008 Feb 20   2010 Jul 01       37.50       5,399.29       5,574.83       5,574.83        (175.54)   Long Losses        (175.54)       38.6900
                       7     256.000       7,621.12  2008 Oct 06   2010 Jul 01       37.50       9,598.74       7,651.82       7,651.82       1,946.92    Long Gains        1,946.92        29.7700
      ______________________________________________________________________________________________________________________________________________________________________________________________
                             400.000      13,192.48  2008 Feb 20   2010 Jul 01       37.50      14,998.03      13,226.65      13,226.65       1,771.39    Long Losses        (175.54)
                                                                                                                                                          Long Gains        1,946.92

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
        FARM.990       0       1.000     484,906.85  2008 May 05   2010 Jul 01  561,000.00     561,000.00     511,650.95     511,650.95      49,349.05    Long Gains       49,349.05   484,906.8500
      ______________________________________________________________________________________________________________________________________________________________________________________________
                               1.000     484,906.85  2008 May 05   2010 Jul 01  561,000.00     561,000.00     511,650.95     511,650.95      49,349.05    Long Gains       49,349.05

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
      UBS_INCOME       0   23513.604      21,000.00  2010 Feb 15   2010 Jul 01        0.90      21,152.84      20,532.49      20,532.49         620.35    Short Gains         620.35         0.8931
                       1    1104.484       1,000.00  2010 Mar 26   2010 Jul 01        0.90         993.59         978.04         978.04          15.55    Short Gains          15.55         0.9054
                       2     414.491         369.27  2010 Apr 01   2010 Jul 01        0.90         372.88         361.03         361.03          11.85    Short Gains          11.85         0.8909
                       3    1111.605       1,000.00  2010 May 06   2010 Jul 01        0.90       1,000.00         977.90         977.90          22.10    Short Gains          22.10         0.8996
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           26144.184      23,369.27  2010 Feb 15   2010 Jul 01        0.90      23,519.31      22,849.46      22,849.46         669.85    Short Gains         669.85

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
       UBS_FIXED       0   25167.785      21,000.00  2010 Feb 15   2010 Jul 01        0.84      21,176.18      20,728.35      20,728.35         447.83    Short Gains         447.83         0.8344
                       1    1187.085       1,000.00  2010 Mar 26   2010 Jul 01        0.84         998.81         987.19         987.19          11.63    Short Gains          11.63         0.8424
                       2     316.349         263.55  2010 Apr 01   2010 Jul 01        0.84         266.18         260.14         260.14           6.04    Short Gains           6.04         0.8331
                       3    1188.495       1,000.00  2010 May 06   2010 Jul 01        0.84       1,000.00         987.17         987.17          12.83    Short Gains          12.83         0.8414
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           27859.714      23,263.55  2010 Feb 15   2010 Jul 01        0.84      23,441.17      22,962.84      22,962.84         478.33    Short Gains         478.33

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
        UBS_CASH       1    3801.647       4,001.99  2009 Nov 26   2010 Jul 01        1.05       3,995.89       4,001.99       4,001.99          (6.10)   Short Losses         (6.10)        1.0527
                       2    2849.544       3,000.00  2010 Jan 21   2010 Jul 01        1.05       2,995.14       3,000.00       3,000.00          (4.86)   Short Losses         (4.86)        1.0528
                       3   13313.047      14,000.00  2010 Feb 04   2010 Jul 01        1.05      13,993.28      14,000.00      14,000.00          (6.72)   Short Losses         (6.72)        1.0516
                       4      58.876          61.89  2010 Mar 01   2010 Jul 01        1.05          61.88          61.89          61.89          (0.01)   Short Losses         (0.01)        1.0512
                       5     948.857       1,000.00  2010 Mar 26   2010 Jul 01        1.05         997.34       1,000.00       1,000.00          (2.66)   Short Losses         (2.66)        1.0539
                       6      59.844          62.92  2010 Apr 01   2010 Jul 01        1.05          62.90          62.92          62.92          (0.02)   Short Losses         (0.02)        1.0514
                       7      70.038          73.61  2010 Apr 28   2010 Jul 01        1.05          73.62          73.61          73.61           0.01    Short Gains           0.01         1.0510
                       8     950.661       1,000.00  2010 May 06   2010 Jul 01        1.05         999.24       1,000.00       1,000.00          (0.76)   Short Losses         (0.76)        1.0519
                       9      83.922          88.21  2010 Jun 01   2010 Jul 01        1.05          88.21          88.21          88.21           0.00    Zero Gains            0.00         1.0511
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           22136.436      23,288.62  2009 Nov 26   2010 Jul 01        1.05      23,267.50      23,288.62      23,288.62         (21.13)   Short Losses        (21.13)
                                                                                                                                                          Short Gains           0.01

          Asset     Parcel   Units           Cost        From         Latest       Price           Value        Reduced       Adjusted      Accounting      Type            Taxable         Per Unit
         UBS_AUS       0   21770.682      24,000.00  2008 Dec 17   2010 Jul 01        1.04      22,750.36      24,000.00      24,000.00      (1,249.64)   Long Losses      (1,249.64)        1.1024
                       1    2223.046       2,400.00  2009 Mar 16   2010 Jul 01        1.04       2,323.08       2,400.00       2,400.00         (76.92)   Long Losses         (76.92)        1.0796
                       2    2299.952       2,400.00  2009 May 25   2010 Jul 01        1.04       2,403.45       2,400.00       2,400.00           3.45    Long Gains            3.45         1.0435
                       3     953.107       1,000.00  2010 Mar 26   2010 Jul 01        1.04         996.00       1,000.00       1,000.00          (4.00)   Short Losses         (4.00)        1.0492
                       4     395.146         408.70  2010 Apr 01   2010 Jul 01        1.04         412.93         408.70         408.70           4.23    Short Gains           4.23         1.0343
                       5     956.938       1,000.00  2010 May 06   2010 Jul 01        1.04       1,000.00       1,000.00       1,000.00           0.00    Zero Gains            0.00         1.0450
      ______________________________________________________________________________________________________________________________________________________________________________________________
                           28598.871      31,208.70  2008 Dec 17   2010 Jul 01        1.04      29,885.81      31,208.70      31,208.70      (1,322.89)   Short Losses         (4.00)
                                                                                                                                                          Short Gains           4.23
                                                                                                                                                          Long Losses      (1,326.56)
                                                                                                                                                          Long Gains            3.45

      ______________________________________________________________________________________________________________________________________________________________________________________________
      ______________________________________________________________________________________________________________________________________________________________________________________________
          Totals                         814,410.92                                            901,617.51     839,464.29     839,464.29      62,153.23    Short Losses     (4,168.33)
                                                                                                                                                          Short Gains       3,595.59
                                                                                                                                                          Long Losses      (2,016.94)
                                                                                                                                                          Long Gains       64,742.92
        ____________________________________________
	               Market Gains =>     68,338.51
	              Market Losses =>      6,185.28
        ____________________________________________

After Application of Deferred Losses
	             Deferred Gains =>     62,153.23
        ____________________________________________


```

**A Depreciation Report** (not detailed)
```
No Name Pty Ltd
Depreciation Schedule for the Period [2017 Jul 01, 2018 Jul 01]

           Asset      Method      From          To           Opening        Closing       Second Element  Adjusted    Depreciation
      ____________________________________________________________________________________________________________________________
      WESTROW.TV  Dim. Value [2017 Jul 01, 2018 Jul 01]      1,637.00       1,310.05           0.00       1,310.05         326.95
   WESTROW.TABLE  Dim. Value [2017 Jul 01, 2018 Jul 01]      2,540.00       2,159.52           0.00       2,159.52         380.48
  WESTROW.STOOLS  Dim. Value [2017 Jul 01, 2018 Jul 01]      1,640.00       1,394.34           0.00       1,394.34         245.66
    WESTROW.SOFA  Dim. Value [2017 Jul 01, 2018 Jul 01]      2,985.35       2,538.16           0.00       2,538.16         447.19
    WESTROW.OVEN  Dim. Value [2017 Jul 01, 2018 Jul 01]      2,216.00       1,847.17           0.00       1,847.17         368.83
   WESTROW.LIFTS  Dim. Value [2017 Jul 01, 2018 Jul 01]     10,394.00       9,702.02           0.00       9,702.02         691.98
     WESTROW.HOB  Dim. Value [2017 Jul 01, 2018 Jul 01]      2,007.00       1,672.96           0.00       1,672.96         334.04
  WESTROW.FRIDGE  Dim. Value [2017 Jul 01, 2018 Jul 01]      1,409.00       1,174.49           0.00       1,174.49         234.51
    WESTROW.FANS  Dim. Value [2017 Jul 01, 2018 Jul 01]      1,082.00         937.93           0.00         937.93         144.07
    WESTROW.DISH  Dim. Value [2017 Jul 01, 2018 Jul 01]      2,100.00       1,680.58           0.00       1,680.58         419.42
  WESTROW.CARPET  Dim. Value [2017 Jul 01, 2018 Jul 01]      3,491.00       2,793.76           0.00       2,793.76         697.24
 WESTROW.CAPITAL  Dim. Value [2017 Jul 01, 2018 Jul 01]    305,204.00     289,964.70           0.00     289,964.70      15,239.30
     WESTROW.BED  Dim. Value [2017 Jul 01, 2018 Jul 01]      1,081.50         919.50           0.00         919.50         162.00
  WESTROW.AIRCON  Dim. Value [2017 Jul 01, 2018 Jul 01]      8,291.00       7,187.05           0.00       7,187.05       1,103.95
        KSH.POOL  Pool Value [2017 Jul 01, 2018 Jul 01]     15,597.77      12,695.84           0.00      12,574.95       2,901.93

 Depreciation from Sales                                                                                                    20.89
      ____________________________________________________________________________________________________________________________
     Period Depreciation                                                                                                23,718.46
```


# The Transaction Listing #

The other useful output from _Minus Plus_ is the transaction listing. This is broadly similar to the input journal
but several transactions have been expanded,
```
2008 Jan 21,   WILKINS.CON,          CASH,       10.00,           , # Contribution
2008 Feb 08,          CASH,     BANK.FEES,       10.00,           , # Bank Fees
2008 Feb 20,   WILKINS.CON,           ANZ,       55.50,           , # Contribution
2008 Feb 20,      EMMA.NON,           ANZ,    80320.44,           , # Contribution
2008 Feb 20,           ANZ,       BHP.ASX,    80370.44,   2076.000, 50.00, # BHP Shares Initial Investment, [[BHP Holding Statement 2008 Feb]], [[BHP Buy 2008 Feb]]
2008 Feb 29,   WILKINS.CON,          CASH,      500.00,           , # Contribution
2008 Mar 03,          CASH,         ADMIN,      374.00,           , # Simple Fund Subscription
2008 Mar 04,          CASH,        ESAVER,      125.00,           , # Transfer to Savings
2008 Mar 04, WILKINS.TAX-FREE,        ESAVER,     1583.44,           , # WILKINS Roll in
2008 Mar 04, WILKINS.TAXABLE,        ESAVER,   146525.18,           , # WILKINS Roll in
2008 Mar 05,           ANZ,          CASH,        5.49,           , # Transfer to Bank
2008 Mar 06,          CASH,        ESAVER,        6.48,           , # Transfer to Savings
2008 Mar 12,        ESAVER,    TERM00.CUA,    70000.00,           , 2008 Jun 12, # Term deposit 3 months
2008 Mar 12,        ESAVER,           ANZ,    50000.00,           , # Transfer to ANZ Account
2008 Mar 13,           ANZ,     PXUPA.ASX,    10982.95,    150.000, 32.95, # Initial Investment PXUPA, [[PXUPA Holding Statement 2008 Mar]], [[PXUPA Buy 2008 Mar]]
2008 Mar 13,           ANZ,     BENPA.ASX,    13270.45,    150.000, 32.95, # Initial Investment BENPA, [[BENPA Holding Statement 2008 Mar]], [[BENPA Buy 2008 Mar]]
2008 Mar 13,           ANZ,     SGBPD.ASX,    10537.95,    110.000, 32.95, # Initial Investment SGBPD, [[SGBPD Holding Statement 2008 Mar]], [[SGBPD Buy 2008 Mar]]
2008 Mar 13,           ANZ,     IAGPB.ASX,    14732.95,    150.000, 32.95, # Initial Investment IAGPB, [[IAGPB Holding Statement 2008 Mar]], [[IAGPB Buy 2008 Mar]]
2008 Mar 13,           ANZ,        ESAVER,      475.00,           , # Transfer to ESAVER
2008 Mar 18,       ANZ.INT,           ANZ,       50.29,           , # Interest (ANZ)
2008 Mar 18, I_TAX.BHP.ASX,      FRANKING,      284.15,           , # BHP.ASX Franking Credits
2008 Mar 18,   DIV.BHP.ASX,          CASH,      663.02,           , 2008 Feb 25, # BHP 356th Dividend, [[BHP Dividend 2008 Mar]]
2008 Mar 19,          CASH,        ESAVER,      663.00,           , # Transfer to ESAVER
2008 Mar 21, # CUA Bank Statements, [[CUA Statement 2008 Mar]]
2008 Mar 28,           ANZ,          CASH,       50.00,           , # Transfer to CUA
2008 Mar 29,          CASH,        ESAVER,       50.00,           , # Transfer to ESAVER
2008 Apr 01,       ESA.INT,        ESAVER,      337.05,           , # eSaver Interest
2008 Apr 04,     EMMA.WORK,          CASH,     7895.06,           , # Employer Contribution
2008 Apr 05,          CASH,        ESAVER,     7895.01,           , # Transfer to ESAVER
2008 Apr 09,   WILKINS.NON,           ANZ,     8382.00,           , # Contributions (WILKINS.
2008 Apr 09,           ANZ,       BHP.ASX,     8432.00,    200.000, 50.00, # BHP in specie transfer (200 units), [[BHP Holding Statement 2008 Apr]], [[BHP Buy 2008 Apr]]
2008 Apr 09,        ESAVER,           ANZ,       50.00,           , # Transfer to ANZ Account
2008 Apr 12,   WILKINS.NON,        ESAVER,    14000.00,           , # Contribution
2008 Apr 18,       ANZ.INT,           ANZ,        0.01,           , # Interest (ANZ)
2008 Apr 23,   WILKINS.SPC,        ESAVER,     3000.00,           , # Spouse contribution
2008 May 01,       ESA.INT,        ESAVER,      268.98,           , # Interest (ESaver)
2008 May 01,        ESAVER,          CASH,    48100.00,           , # Transfer to Cash
2008 May 01, I_TAX.BENPA.ASX,      FRANKING,      197.46,           , # BENPA.ASX Franking Credits
2008 May 01, DIV.BENPA.ASX,          CASH,      460.74,           , 2008 Apr 14, # BENPA Interim Dividend, [[BENPA Dividend 2008 May]]
2008 May 05,          CASH,      FARM.990,    48500.00,      1.000, # Down payment on Farm
2008 May 09,     EMMA.WORK,          CASH,    14183.06,           , # Employer Contribution
2008 May 10,          CASH,        ESAVER,    14200.00,           , # Transfer to ESAVER
2008 May 12,   WILKINS.NON,        ESAVER,    37500.00,           , # Contribution
2008 May 12,   WILKINS.CON,        ESAVER,    12500.00,           , # Contribution
2008 May 12,          CASH,        ESAVER,       23.88,           , # Transfer to ESAVER
2008 May 12,          CASH,        ESAVER,   318539.01,           , # Transfer to ESAVER
2008 May 12, EMMA.TAX-FREE,          CASH,     5000.00,           , # Emma's Rollin
2008 May 12,  EMMA.TAXABLE,          CASH,   313539.01,           , # Emma's Rollin
2008 May 20, I_TAX.SGBPD.ASX,      FRANKING,       73.31,           , # SGBPD.ASX Franking Credits
2008 May 20, DIV.SGBPD.ASX,          CASH,      171.06,           , 2008 May 04, # Dividends (SGBPD), [[SGBPD Dividend 2008 May]]
2008 May 21,          CASH,        ESAVER,      190.00,           , # Transfer to ESAVER
2008 May 31,       ESA.INT,        ESAVER,     1557.40,           , # Interest (ESaver)
2008 Jun 06,     EMMA.WORK,          CASH,    14022.36,           , # Employer Contribution
2008 Jun 07,          CASH,        ESAVER,    14023.00,           , # Transfer to ESAVER
2008 Jun 12,    TERM00.INT,        ESAVER,     1314.47,           , # Interest (CUA Term Deposit)
2008 Jun 12,    TERM00.CUA,        ESAVER,    70000.00,           , # Close term deposit
2008 Jun 16, I_TAX.IAGPB.ASX,      FRANKING,      145.36,           , # IAGPB.ASX Franking Credits
2008 Jun 16, DIV.IAGPB.ASX,          CASH,      339.18,           , 2008 May 28, # IAGPB Dividend, [[IAGPB Dividend 2008 Jun]]
2008 Jun 16,     IAGPB.ASX,          CASH,    15000.00,   -150.000, # Disposal of 150 units in IAGPB.ASX - No Brokerage, [[IAGPB Sell 2008 Jun]], [[IAGPB Holding Statement 2008 Jun]]
2008 Jun 16,          CASH,        ESAVER,    15339.00,           , # Transfer to ESAVER
2008 Jun 26,        ESAVER,          CASH,   436550.00,           , # Transfer from ESAVER
2008 Jun 26,          CASH,      FARM.990,   436401.85,          I, 2008 May 05, # Farm Final Installment
2008 Jun 26,          CASH,      FARM.990,        5.00,          I, 2008 May 05, # Bank Charges for Bank Cheque
2008 Jun 27,        ESAVER,          CASH,      600.00,           , # Transfer from ESAVER
2008 Jun 29,        ESAVER,          CASH,    26100.00,           , # Transfer to Cash
2008 Jun 30, FOR.PXUPA.ASX,          CASH,      726.63,           , # PXUPA distribution, [[PXUPA Distribution 2008 Jun]]
2008 Jun 30,      CASH.INT,          CASH,        3.44,           , # Cash A/C Interest
2008 Jun 30,     EMMA.WORK,          CASH,    12576.00,           , # Employer Contribution
2008 Jun 30,     EMMA.WORK,          CASH,     1607.06,           , # Employer Contribution
2008 Jun 30,       ESA.INT,        ESAVER,     2252.98,           , # Interest (ESaver)
```
Above is the listing for the _Example_ folder; the document have not been located (because the document folder doesn't exist!), and this is indicated
by them shown surrounded by double square brackets. If the files were found they are presented as a URI instead; as in this clip from another listing:
```
2014 Mar 20,        ESAVER,        DEBTOR,    15000.00,           , # ARG Buy SPP
2014 Mar 21,          CASH,        ESAVER,      995.00,           , # Transfer to ESaver
2014 Mar 21,           ANZ,          CASH,     1086.00,           , # Transfer to Cash
2014 Mar 21,           ANZ,       TAX.GST,        1.81,           , # , II, # GST BHP.ASX
2014 Mar 21,           ANZ,       BHP.ASX,     5854.90,    164.000, 18.14, # Buy 164 BHP, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/BHP%20Holding%20Statement%202014%20Mar.pdf, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/BHP%20Buy%202014%20Mar%2021.pdf
2014 Mar 24,          CASH,        ESAVER,     1086.00,           , # Transfer to ESaver
2014 Mar 26,        ESAVER,        DEBTOR,     4340.00,           , # TGG SPP, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/TGG%20Buy%202014%20Mar.pdf
2014 Mar 26, I_TAX.BHP.ASX,      FRANKING,      360.60,           , # BHP.ASX Franking Credits
2014 Mar 26,   DIV.BHP.ASX,          CASH,      841.41,           , 2014 Mar 03, # BHP Dividend, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/BHP%20Dividend%202014%20Mar.pdf
2014 Mar 26,       AGF.DUE,       AGF.ASX,      518.94,    678.000, # AMP China Dist, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/AGF%20Holding%20Statement%202014%20Mar.pdf, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/AGF%20Distribution%202014%20Mar.pdf
2014 Mar 27,          CASH,        ESAVER,      842.00,           , # Transfer to ESaver
2014 Mar 31,        ESAVER,          CASH,     4500.00,           , # Transfer to Cash
2014 Mar 31,       ESA.INT,        ESAVER,      152.15,           , # ESaver interest
2014 Mar 31, I_TAX.WAM.ASX,      FRANKING,      305.46,           , # WAM.ASX Franking Credits
2014 Mar 31,   DIV.WAM.ASX,       WAM.ASX,      712.73,    367.000, 2014 Mar 18, # WAM DRP, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/WAM%20Holding%20Statement%202014%20Mar.pdf, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/WAM%20Dividend%202014%20Mar.pdf
2014 Apr 01, UBS_INCOME.DUE,    UBS_INCOME,      517.79,    538.467, # Diversified Credit Dist, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/UBS%20Distribution%202014%20Apr.pdf
2014 Apr 01,   UBS_AUS.DUE,       UBS_AUS,      378.29,    345.976, # Australian Bond Dist, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/UBS%20Distribution%202014%20Apr.pdf
2014 Apr 01,  UBS_CASH.DUE,      UBS_CASH,      123.31,    118.079, # Cash Fund Dist, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/UBS%20Distribution%202014%20Apr.pdf
2014 Apr 01, UBS_FIXED.DUE,     UBS_FIXED,      497.58,    564.022, # Diversified Fixed Income Dist, file:///Users/me/CloudStation/Projects/Accounts/WTDSF%20Documents/UBS%20Distribution%202014%20Apr.pdf
```
