CARIAD SE CPM Project
Technical Specification 



Skip Error Message on Assigning Items 










Person Responsible:	Baobao Gao
Status:	in review

Version:	V1.0
Date:	16/04/2025
	
	
	
 

Version History:
Version	Date	Author	Comments
V1.0	16/04/2025	Baobao Gao	
			
			



 
1  Technical Architecture


2  ABAP

2.1  Project Design Brief
             Able to assign same portfolio items under multi planning round initiatives.

2.2  Enhancement
                  
•	SE20 transaction to view Enhancements.
         
Enhancement Implementation	Short Description
ZCON_CPMIE_ITEM_ASSOCIATED	BAdI Impl. for Items can be associated more than once
ZCON_PFMIE_ITEM_ASSIGNED_FLAG	Associate multiple initiatives with a flag

•	SM30 transaction to maintain table ' ZUTILT_MULTICOMP '
       
Implementation Class	Method	Short Description
ZCL_CON_PFM_ITEM_ENHANCE	ENHANCE_ITEM_ASSO	Items can be asso-ciated more than once
ZCL_CON_PFM_ITEM_ENHANCE	ENHANCE_ITEM_ASSO_FLG	Associate multiple initiatives with a flag
           
            The configuration is as follows
             
2.2.1 ZCON_CPMIE_ITEM_ASSOCIATED – Enhancement

•	Additional Object Types ('ZPD')
 
•	Filter on retrieving project structure, only display WBS elements which is linked to current CPM master project with object type “0WBS”.
Step1 Copy standard class '/CPD/CL_PS_OBJECT_API ' to ' ZDE_CL_CON_CPM_PS_OBJECT_LINK'
Step2 Method '/CPD/IF_COMMON_ACCESS_API~GET_HIERARCHY' adjust-ments is as follows:

 
2.3  Authorization

        N/A
3  Other 

