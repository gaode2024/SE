CARIAD SE CPM Project
Functional Specification 


Jira Interface 











Person Responsible:	Jimmy Xu
Status:	in review

Version:	V1.0
Date:	22/10/2025
	
	
	
 

Version History:
Version	Date	Author	Comments
V1.0	22/10/2025	Jimmy Xu	
			
			



 
1  Business Purpose
As described in “Fine Estimation” process, by starting EFES task in Jira, EFES task master data will be created automatically in SAP. SAP EFES Task status will be updated afterwards.
Planning status and figures in SAP will be synchronized to Jira to give an overview in Jira.
 

2  Functional Specification
2.1  Technic Prerequisite
Create customized table in SAP.
Field	Key	Data Type
Task ID	Yes	CHAR, 40
Task Name		CHAR, 80
Type		CHAR, 40
Jira Status		CHAR, 40
Task Status		CHAR, 40
Priority		CHAR, 40
Start Date		DATS, 8
End Date		DATS, 8
Assignee		CHAR, 40
Reporter		CHAR, 40
Description		
Labels		CHAR, 80
Jira2SAP Timestamp		
SAP2Jira Timestamp		
Last Update Timestamp		

2.2  Interface: Jira to SAP
2.2.1 Data Structure
Read delta tickets with following fields from Jira and transport to SAP.
Field	Key	Comment
Task ID	Yes	Only get newly created or updated tickets from last transport.
Summary		
Type		Set filter: Sub-task
Jira Status		
Priority		
Start Date		
End Date		
Assignee		
Reporter		
Description		
Labels		

2.2.2 Inbound Data Handling
Update entries by Task IDs in customized table by mapping following: 
Jira Field	SAP Field	Comment
Task ID	Task ID	Directly mapping
Summary	Task Name	Directly mapping
Type	Type	Directly mapping
Jira Status	Jira Status	Directly mapping
Priority	Priority	Directly mapping
Start Date	Start Date	Directly mapping
End Date	End Date	Directly mapping
Assignee	Assignee	Directly mapping
Reporter	Reporter	Directly mapping
Description	Description	Directly mapping
Labels	Labels	Directly mapping
	Jira2SAP Timestamp	Current Time

2.3  Interface: SAP to Jira
2.3.1 Outbound Data Handling
Read delta entries with following fields and transport to Jira.
Field	Key	Comment
Task ID	Yes	Only get entries which Last Update Timestamp > SAP2Jira Timestamp.
Task Status		

2.3.2 Data Structure
Update Jira tickets by Task IDs mapping following:
SAP Field	Jira Field	Comment
Task ID	Task ID	Directly mapping
Task Status	Comment	Create a new comment

2.3.3 Result Handling
If Jira tickets are successfully updated, update customized table as following.
Field	Comment
Task ID	
SAP2Jira	Current Time


3  Authorization
NA

4  Others
