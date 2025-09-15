# Research: SAP ABAP SY-UNAME Tracker

## Overview
Research findings for implementing a static analysis tool that tracks sy-uname data flow in ABAP programs.

## Key Technical Decisions

### 1. ABAP Parsing Approach
**Decision**: Use SCAN ABAP-SOURCE statement
**Rationale**: Native SAP kernel tokenizer provides most reliable parsing, handles all ABAP syntax edge cases
**Alternatives considered**:
- Manual regex parsing - rejected due to ABAP syntax complexity
- External parser libraries - none available for ABAP

### 2. Taint Tracking Algorithm
**Decision**: Forward data flow analysis with symbol table
**Rationale**: Tracks assignments from source (sy-uname) to sinks (DML/RFC)
**Alternatives considered**:
- Backward analysis - more complex for multiple sources
- AST-based analysis - ABAP lacks accessible AST API

### 3. Scope Management
**Decision**: Stack-based scope tracking with FORM/METHOD boundaries
**Rationale**: Matches ABAP's execution model, handles nested includes
**Alternatives considered**:
- Flat scope model - loses precision with nested calls
- Full call graph - too complex for initial version

### 4. Output Format
**Decision**: CSV with columns as specified in spec clarification
**Rationale**: Easy to process, import into Excel/analysis tools
**Format**: A(sequence), B(program), C(line), D(operation), E(table), F(field), G+(flow path)
**Alternatives considered**:
- JSON - less readable for business users
- XML - verbose for simple tabular data

### 5. Performance Optimization
**Decision**: Single-pass analysis with lazy include loading
**Rationale**: Minimizes memory usage for large programs
**Alternatives considered**:
- Multi-pass analysis - better precision but slower
- Full program loading - memory issues with large codebases

## ABAP-Specific Considerations

### Statement Types to Track
1. **Direct assignments**: `lv_var = sy-uname`
2. **Structure assignments**: `ls_struct-field = sy-uname`
3. **Table operations**: `APPEND`, `INSERT`, `MODIFY` with tainted data
4. **Method calls**: `CALL METHOD` with tainted parameters
5. **Function calls**: `CALL FUNCTION` (especially RFC)
6. **Form calls**: `PERFORM` with tainted parameters
7. **Special constructs**:
   - `MOVE-CORRESPONDING` between structures
   - `FIELD-SYMBOLS` and `ASSIGN` statements
   - `CREATE DATA` and data references
   - Internal table operations with `TABLE`

### DML Operations
Track these database operations when they involve Z*/Y* tables:
- `INSERT` (single and mass)
- `UPDATE` (with SET and WHERE)
- `MODIFY` (insert or update)
- `DELETE` (with WHERE conditions)

### Confidence Levels
- **High**: Direct assignment paths, static table names
- **Medium**: Indirect via structures, static RFC destinations
- **Low**: Dynamic table names, field-symbols, data references

### Edge Cases Handled
1. **Circular includes**: Track visited includes to prevent loops
2. **Global variables**: Track across FORM/FUNCTION boundaries
3. **Parameter passing**: Track USING/CHANGING/TABLES parameters
4. **Local classes**: Track method parameters and attributes
5. **Macros**: Expand before analysis

## Implementation Architecture

### Component Breakdown
1. **ZCL_SYUNAME_SCANNER**
   - Wraps READ REPORT for source retrieval
   - Handles INCLUDE resolution
   - Manages source code cache

2. **ZCL_SYUNAME_TAINT**
   - Maintains tainted variable registry
   - Tracks assignment chains
   - Manages scope transitions

3. **ZCL_SYUNAME_PARSER**
   - Identifies DML statements
   - Extracts table names and fields
   - Detects RFC and method calls

4. **ZCL_SYUNAME_REPORT**
   - Formats results to CSV
   - Generates taint path strings
   - Calculates confidence scores

### Data Structures
```abap
TYPES: BEGIN OF ty_tainted_var,
         name TYPE string,
         scope TYPE string,
         line TYPE i,
         path TYPE string_table,
         confidence TYPE string,
       END OF ty_tainted_var.

TYPES: BEGIN OF ty_finding,
         program TYPE programm,
         line TYPE i,
         operation TYPE string,
         table_name TYPE tabname,
         field_name TYPE fieldname,
         taint_path TYPE string,
         confidence TYPE string,
       END OF ty_finding.
```

## Testing Strategy

### Test Data Sets
1. **Simple cases**: Direct sy-uname assignments
2. **Complex flows**: Multi-hop assignments through structures
3. **Edge cases**: Dynamic constructs, field-symbols
4. **Large programs**: Performance testing with 10K+ lines
5. **Real-world code**: Actual Z* programs from SAP systems

### Validation Approach
1. Manual verification of known taint paths
2. Comparison with security audit findings
3. False positive/negative rate analysis
4. Performance benchmarks against target goals

## Resolved Clarifications

### CSV Output Structure
Confirmed format from spec clarification:
- Column A: Sequence number (can be ignored)
- Column B: Program or include name
- Column C: sy-uname start line
- Column D: Operation type (DML/RFC/PERFORM)
- Column E: Table name
- Column F: Field name
- Columns G+: Flow path steps

### Performance Requirements
- Target: 10,000+ lines per minute
- Memory limit: Standard SAP work process limits
- Timeout handling: Progress indication for large programs

## Next Steps
With all technical decisions made and clarifications resolved, proceed to Phase 1 for detailed design and contract definition.