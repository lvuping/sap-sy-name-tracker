# CLI Interface Contract: ZSYUNAME_ANALYZER

## Program Interface

### Selection Screen Parameters

```abap
PARAMETERS:
  p_prog   TYPE programm OBLIGATORY,  " Program/Report to analyze
  p_mode   TYPE char1 DEFAULT 'L',    " L=LOCAL_ONLY, F=FOLLOW_CALLS
  p_output TYPE string DEFAULT '/tmp/syuname_results.csv', " Output file
  p_debug  TYPE abap_bool DEFAULT ' '. " Debug mode

SELECT-OPTIONS:
  s_excl FOR sy-repid NO INTERVALS.   " Programs to exclude
```

### Input Validation
- `p_prog`: Must be valid ABAP program name (exists in REPOSRC)
- `p_mode`: Must be 'L' or 'F'
- `p_output`: Must be valid file path with write permissions
- `s_excl`: Optional list of programs to skip

### Return Codes
- 0: Success - analysis completed
- 4: Warning - analysis completed with issues
- 8: Error - analysis failed
- 12: Fatal - invalid parameters

## Command Line Usage

### Basic Analysis
```bash
# Analyze single program in local mode
ZSYUNAME_ANALYZER p_prog=ZREPORT_SALES

# Follow all calls
ZSYUNAME_ANALYZER p_prog=ZREPORT_SALES p_mode=F

# Custom output location
ZSYUNAME_ANALYZER p_prog=ZREPORT_SALES p_output=/usr/sap/analysis/results.csv
```

### Advanced Options
```bash
# Exclude certain programs
ZSYUNAME_ANALYZER p_prog=ZREPORT_MAIN s_excl=ZOLD_INCLUDE,ZTEST_*

# Debug mode with detailed logging
ZSYUNAME_ANALYZER p_prog=ZREPORT_SALES p_debug=X
```

## Output Contract

### CSV Format
Fixed columns with extensible taint path:

| Column | Name | Type | Description |
|--------|------|------|-------------|
| A | Sequence | INT | Row number (optional) |
| B | Program | CHAR(30) | Program/Include name |
| C | Line | INT | sy-uname source line |
| D | Operation | CHAR(20) | DML/RFC/PERFORM/METHOD |
| E | Target | CHAR(30) | Table/RFC/Routine name |
| F | Field | CHAR(30) | Field name (if applicable) |
| G+ | Path_Step_N | CHAR(50) | Each step in taint path |

### Example Output
```csv
1,ZREPORT,42,INSERT,ZTABLE01,CREATED_BY,sy-uname,lv_user,ls_rec-created_by
2,ZREPORT,89,UPDATE,ZTABLE02,CHANGED_BY,sy-uname,gv_username,CHANGING cv_user
3,ZINCLUDE1,156,CALL_FUNCTION,Z_RFC_CREATE,,sy-uname,ls_data-user,EXPORTING iv_creator
```

### Status Messages
Written to SAP application log (object: ZSYUNAME, subobject: ANALYSIS):

```
INFO: Analysis started for program ZREPORT
INFO: Found 15 includes to process
WARNING: Dynamic table name at line 234
INFO: Analysis completed - 8 findings
ERROR: Unable to read include ZMISSING
```

## Error Handling

### Input Errors
```
E001: Program & does not exist
E002: Invalid mode &, use L or F
E003: Cannot write to output path &
E004: No authorization for program &
```

### Runtime Errors
```
W001: Dynamic table name detected at line &
W002: Field-symbol assignment at line &
W003: Circular include detected: &
I001: Skipping out-of-scope call to &
```

### Output Errors
```
E101: Cannot create output file &
E102: Disk space insufficient
E103: Write permission denied for &
```

## Performance Contract

### Limits
- Maximum execution time: 10 minutes
- Maximum memory usage: 2GB
- Maximum output file size: 100MB
- Maximum includes depth: 50

### Expected Performance
- Small program (<1000 lines): <10 seconds
- Medium program (<10000 lines): <60 seconds
- Large program (<100000 lines): <10 minutes

### Progress Indication
For programs >5000 lines, display progress:
```
Processing ZREPORT_MAIN...
  Includes processed: 5/12
  Lines analyzed: 3500/8900
  Findings: 23
```

## Extension Points

### Custom Sinks
Future versions may support configuration file for additional sinks:
```json
{
  "custom_sinks": [
    {"type": "FUNCTION", "pattern": "Z_AUDIT_*"},
    {"type": "METHOD", "pattern": "*->LOG_USER"}
  ]
}
```

### Export Formats
Future versions may support additional output formats:
- JSON: `p_format=JSON`
- XML: `p_format=XML`
- SARIF: `p_format=SARIF`