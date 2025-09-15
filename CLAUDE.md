# Claude Code Context: SAP ABAP SY-UNAME Tracker

## Project Overview
Static analysis tool for SAP ABAP that tracks sy-uname (user ID) data flow through code to identify where user information impacts database tables, RFC calls, and other operations.

## Current Development Status
- **Phase**: Implementation Planning Complete
- **Branch**: 001-sap-abap-sy
- **Language**: ABAP (SAP NetWeaver 7.40+)

## Technical Stack
- **Core Language**: ABAP
- **Parser**: SCAN ABAP-SOURCE (SAP kernel tokenizer)
- **Testing**: ABAP Unit Test Framework
- **Output**: CSV format results
- **Logging**: SAP Application Log (SLG1)

## Key Components

### Main Program
- `ZSYUNAME_ANALYZER`: CLI entry point with selection screen

### Core Classes
- `ZCL_SYUNAME_SCANNER`: Source code reading and include resolution
- `ZCL_SYUNAME_TAINT`: Taint tracking and propagation engine
- `ZCL_SYUNAME_PARSER`: Statement parsing for DML/RFC/calls
- `ZCL_SYUNAME_REPORT`: CSV output and reporting

### Interfaces
- `ZIF_SYUNAME_SCANNER`: Scanner contract
- `ZIF_SYUNAME_TAINT`: Taint engine contract
- `ZIF_SYUNAME_PARSER`: Parser contract
- `ZIF_SYUNAME_REPORT`: Reporter contract

## Key Algorithms

### Taint Tracking
Forward data flow analysis tracking assignments from sy-uname through:
- Local variables
- Structure components
- Internal tables
- Method/function parameters
- Database operations

### Scope Management
Stack-based scope tracking for:
- FORM routines
- METHODS
- FUNCTION modules
- INCLUDE boundaries

## Testing Approach
1. **Contract Tests**: API interface validation
2. **Integration Tests**: Full flow with test programs
3. **Unit Tests**: Component-level testing
4. **Validation Tests**: Known taint paths verification

## Performance Targets
- Process 10,000+ lines/minute
- Handle programs up to 100,000 lines
- Track 1000+ taint paths

## Output Format
CSV with columns:
- A: Sequence number
- B: Program/Include name
- C: sy-uname line number
- D: Operation (INSERT/UPDATE/CALL_FUNCTION/etc)
- E: Target table/RFC name
- F: Field name
- G+: Taint path steps

## Development Commands

### Run Analysis
```abap
ZSYUNAME_ANALYZER p_prog=<program> p_mode=L p_output=/tmp/results.csv
```

### Test Execution
```abap
" Execute test class
SE80 → ZCL_SYUNAME_TEST → Execute → Unit Test
```

## Recent Changes
- Initial specification and planning complete
- Core architecture defined
- Contract interfaces established
- Test scenarios documented

## Next Steps
- [ ] Implement ZCL_SYUNAME_SCANNER
- [ ] Implement ZCL_SYUNAME_TAINT
- [ ] Implement ZCL_SYUNAME_PARSER
- [ ] Implement ZCL_SYUNAME_REPORT
- [ ] Create main program ZSYUNAME_ANALYZER
- [ ] Write unit tests
- [ ] Integration testing
- [ ] Performance optimization

## Known Issues
None yet - implementation pending

## Code Conventions
- Use Z* namespace for custom objects
- Follow SAP naming conventions
- Include detailed ABAP doc comments
- Maintain backwards compatibility to 7.40

## Error Handling
- Use ZCX_SYUNAME_ERROR for exceptions
- Log to application log (SLG1)
- Provide clear error messages
- Include context in error data