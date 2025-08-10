CARIAD SE Accrual Engine Optimization Project
Functional Specification 


Automated Roll-Forward of Revised POC in Final Service Period










Person Responsible:	Wendy Wu
Status:	in review

Version:	V1.0
Date:	28/07/2025
	
	
	
 

Version History:
Version	Date	Author	Comments
V1.0	28/07/2025	Wendy Wu	
			
			



 
1  Business Purpose
When a purchase order reaches its final service period, the accrual engine posts the accrual for 100 % of the contract value.
If the user manually changes the POC (Revised Percentage of Completion), the accrual for that period is posted according to the revised POC. However, the revised POC must still be maintained manually by each PO for every subsequent period; otherwise, in any month where it is not updated, the system will revert to accruing 100 % of the contract amount. 

A workaround has been introduced to eliminate this manual effort: develop program “Copy Revised POC.” For all POs whose:
•	service period has expired
•	goods delivery is not yet complete
•	and which already contain a revised POC record
the program copies the POC from the last accrual-engine run and replicates the same re-vised reason and comment.

This document describes the workaround solution.
2  Functional Specification
2.1  Selection Screen
Description	Field Name	Data 
Type	Length	Single/Multiple 
Selection	Optional/
Mandatory
Company Code	BUKRS	CHAR	4	Multiple	Optional
Last Day of Period	ZDATE	DATE	8	Single	Mandatory
Accrual Object	REF_KEY	CHAR	32	Multiple	Optional
  
 
 
2.2  Program Logic
2.2.1 Filter the revised POC records that meet the criteria.
Join table ACESOBJ_ITEM_PER to EKPO on:
•	EKPO- EBELN = ACESOBJ_ITEM_PER- REF_KEY
•	EKPO- EBELP = substring(ACESOBJ_ITEM_PER-REF_SUBKEY, 4, 2)
Then fetch EKPO-ELIKZ

Join table ACESOBJ_ITEM_PER to ACESOBJ_ITEM on:
•	ACESOBJ_ITEM-BUKRS = ACESOBJ_ITEM_PER- BUKRS
•	ACESOBJ_ITEM- REF_KEY = ACESOBJ_ITEM_PER- REF_KEY
•	ACESOBJ_ITEM- REF_SUBKEY= ACESOBJ_ITEM_PER- REF_SUBKEY 
•	ACESOBJ_ITEM- ITEMTYPE = ACESOBJ_ITEM_PER- ITEMTYPE
Then fetch ACESOBJ_ITEM-LIFE_END_DATE

Apply the following filters to the joined result set:
•	ACESOBJ_ITEM_PER- BUKRS= selection-screen BUKRS (if blank, include all company codes)
•	ACESOBJ_ITEM_PER- REF_KEY= selection-screen REF_KEY (if blank, in-clude all keys)
•	EKPO-ELIKZ is NULL
•	ACESOBJ_ITEM- LIFE_END_DATE < selection-screen ZDATE
•	ACESOBJ_ITEM_PER- ITEMTYPE = 'SCSTPLN'
•	ACESOBJ_ITEM_PER- ADJUSTED_PER_AMNT_WSL ≠ 0
For each unique combination of BUKRS, REF_KEY, and REF_SUBKEY, keep only the row with the most recent PERIOD_END_DATE. 
Then return: BUKRS, REF_KEY , REF_SUBKEY, ADJUSTED_PER_AMNT_WSL, ADJSTMNT_REASON, ADJSTMNT_COMMENT

2.2.2 Copy POC related fields to current month
Execute transaction FACRARVWCO to roll last month’s revision into the current peri-od.
Filter criteria (green fields from step 2.2.1):
•	Company Code = ACESOBJ_ITEM_PER- BUKRS
•	Accrual Object = ACESOBJ_ITEM_PER- REF_KEY
•	Accrual Subobject = ACESOBJ_ITEM_PER- REF_SUBKEY
•	Last Day of Period = selection-screen ZDATE
Populate the current-month revision (use blue fields from step 2.2.1):
•	Revised Cost in Transaction Currency = ACESOBJ_ITEM_PER- ADJUST-ED_PER_AMNT_WSL
•	Reason for adjusting accrual amount = ACESOBJ_ITEM_PER- ADJST-MNT_REASON
•	Comment = ACESOBJ_ITEM_PER- ADJSTMNT_COMMENT
Then save the result.


3  Authorization
NA

4  Others
