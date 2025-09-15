# Quickstart Guide: SAP ABAP SY-UNAME Tracker

## Prerequisites
- SAP NetWeaver 7.40 or higher
- Developer authorization in target SAP system
- Access to SE80/SE38 transaction codes

## Installation

### 1. Transport Import
Import the provided transport request containing:
- ZSYUNAME_ANALYZER (Main program)
- ZCL_SYUNAME_* (Class implementations)
- ZIF_SYUNAME_* (Interface definitions)
- ZCX_SYUNAME_ERROR (Exception class)

### 2. Authorization Setup
Ensure users have authorization for:
- S_DEVELOP (Display authorization for ABAP programs)
- S_DATASET (File system access for CSV output)
- S_APPL_LOG (Application log writing)

## Basic Usage

### Scenario 1: Analyze Single Program
Find all places where sy-uname affects Z* tables in program ZREPORT_SALES:

```abap
" Transaction SE38
Program: ZSYUNAME_ANALYZER
Variant: (none)

" Selection Screen Input:
Program to Analyze: ZREPORT_SALES
Analysis Mode: L (Local Only)
Output File: /tmp/sales_analysis.csv
Debug Mode: (unchecked)

" Execute (F8)
```

**Expected Output:**
```
Analysis started for program ZREPORT_SALES
Found 3 includes to process
Processing main program...
Processing include ZREPORT_SALES_FORMS...
Analysis completed - 5 findings written to /tmp/sales_analysis.csv
```

**CSV Result Example:**
```csv
1,ZREPORT_SALES,42,INSERT,ZSALES_LOG,CREATED_BY,sy-uname,lv_user,ls_log-created_by
2,ZREPORT_SALES,89,UPDATE,ZSALES_HDR,CHANGED_BY,sy-uname,gv_username,SET changed_by
```

### Scenario 2: Deep Analysis with Call Following
Track sy-uname across function calls and performs:

```abap
" Selection Screen Input:
Program to Analyze: ZREPORT_MAIN
Analysis Mode: F (Follow Calls)
Output File: /tmp/deep_analysis.csv

" Execute (F8)
```

**Expected Behavior:**
- Analyzes ZREPORT_MAIN
- Follows PERFORM statements to forms
- Follows CALL FUNCTION to function modules
- Tracks tainted parameters across calls

### Scenario 3: Exclude Specific Programs
Analyze while skipping certain includes:

```abap
" Selection Screen Input:
Program to Analyze: ZREPORT_COMPLEX
Analysis Mode: L
Exclusions: ZOLD_INCLUDE    " In SELECT-OPTIONS
            ZTEST_*          " Wildcards supported
```

## Understanding Results

### Reading the CSV Output

Each row represents one finding where sy-uname data reaches a sink:

| Column | Meaning | Example |
|--------|---------|---------|
| B | Program/Include | ZREPORT_SALES |
| C | Line number | 42 |
| D | Operation | INSERT |
| E | Target | ZSALES_LOG |
| F | Field | CREATED_BY |
| G+ | Taint path | sy-uname → lv_user → ls_log-created_by |

### Confidence Levels in Results

**HIGH Confidence:**
- Direct assignment from sy-uname
- Static table names
- Clear data flow

**MEDIUM Confidence:**
- Indirect through structures
- Some type conversions
- Method parameters

**LOW Confidence:**
- Dynamic table names
- Field-symbols
- Complex transformations

### Special Markers

**OUT_OF_SCOPE:**
- Appears when LOCAL_ONLY mode encounters external calls
- Indicates potential taint path continues outside analysis

**DYNAMIC_TARGET:**
- Table name or destination determined at runtime
- Requires manual review

## Validation Tests

### Test Case 1: Direct Assignment
Create test program:
```abap
REPORT ztest_direct.
DATA: lv_user TYPE sy-uname.
lv_user = sy-uname.
INSERT INTO ztable VALUES lv_user.
```

Expected finding:
```csv
1,ZTEST_DIRECT,3,INSERT,ZTABLE,CLIENT,sy-uname,lv_user,INSERT
```

### Test Case 2: Structure Transfer
```abap
REPORT ztest_struct.
DATA: BEGIN OF ls_data,
        created_by TYPE sy-uname,
      END OF ls_data.
ls_data-created_by = sy-uname.
INSERT ztable FROM ls_data.
```

Expected finding:
```csv
1,ZTEST_STRUCT,5,INSERT,ZTABLE,CREATED_BY,sy-uname,ls_data-created_by,FROM ls_data
```

### Test Case 3: RFC Call
```abap
REPORT ztest_rfc.
DATA: lv_user TYPE sy-uname.
lv_user = sy-uname.
CALL FUNCTION 'Z_UPDATE_USER'
  EXPORTING
    iv_username = lv_user.
```

Expected finding:
```csv
1,ZTEST_RFC,4,CALL_FUNCTION,Z_UPDATE_USER,,sy-uname,lv_user,EXPORTING iv_username
```

## Troubleshooting

### No Findings Reported
- Check if program uses sy-uname at all
- Verify Z*/Y* tables are being used
- Enable debug mode for detailed trace

### Performance Issues
- Use LOCAL_ONLY mode for large programs
- Exclude unnecessary includes
- Check available work process memory

### Authorization Errors
```
E: Program ZREPORT not found or no authorization
```
Solution: Check S_DEVELOP authorization

### File Write Errors
```
E: Cannot write to /tmp/results.csv
```
Solution: Check file system permissions and S_DATASET authorization

## Advanced Features

### Debug Mode
Enable detailed logging:
```abap
Debug Mode: X (checked)
```

Provides:
- Statement-by-statement trace
- Scope entry/exit logging
- Taint propagation details

### Batch Processing
Create variant for repeated analysis:
1. SE38 → ZSYUNAME_ANALYZER
2. Goto → Variants → Create
3. Save parameters
4. Schedule as background job (SM36)

### Integration with Security Audits
Export results for security team review:
1. Run analysis to CSV
2. Import into Excel/security tools
3. Filter by confidence level
4. Prioritize HIGH confidence findings

## Best Practices

1. **Start with LOCAL_ONLY mode** - Faster, focused results
2. **Use exclusions** - Skip generated/standard includes
3. **Review HIGH confidence first** - Most reliable findings
4. **Validate sample findings** - Ensure accuracy before full scan
5. **Schedule regular scans** - Part of security review process

## Support

### Application Logs
View detailed logs in SLG1:
- Object: ZSYUNAME
- Subobject: ANALYSIS

### Common Messages
- `INFO: Analysis completed - N findings` - Success
- `WARNING: Dynamic table at line N` - Review manually
- `ERROR: Include not found` - Check authorization