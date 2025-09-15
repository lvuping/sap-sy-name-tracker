# Data Model: SAP ABAP SY-UNAME Tracker

## Core Entities

### 1. TaintedVariable
Represents a variable or field that contains or has contained sy-uname data.

**Attributes**:
- `name` (string): Variable/field name
- `scope` (string): Current scope (GLOBAL/FORM/METHOD/FUNCTION)
- `scope_name` (string): Name of containing routine
- `program` (string): Program or include name
- `line_number` (integer): Line where tainted
- `taint_path` (string[]): Chain of assignments from sy-uname
- `confidence` (enum): HIGH/MEDIUM/LOW
- `is_active` (boolean): Still in scope

**Relationships**:
- Can propagate to multiple other TaintedVariables
- Associated with zero or more Findings

### 2. Finding
Represents a detected impact point where tainted data affects a sink.

**Attributes**:
- `id` (integer): Sequential finding number
- `program` (string): Program/include where found
- `line_number` (integer): Line of impact statement
- `operation_type` (enum): INSERT/UPDATE/MODIFY/DELETE/CALL_FUNCTION/PERFORM/CALL_METHOD
- `target_name` (string): Table name, RFC name, or routine name
- `target_field` (string): Specific field affected (if applicable)
- `taint_path` (string): Full path from sy-uname to sink
- `confidence` (enum): HIGH/MEDIUM/LOW
- `scope_info` (string): OUT_OF_SCOPE/IN_SCOPE/DYNAMIC_TARGET
- `statement_text` (string): Original ABAP statement

**Relationships**:
- Derived from one or more TaintedVariables
- Part of one AnalysisResult

### 3. AnalysisTarget
The ABAP program or include being analyzed.

**Attributes**:
- `name` (string): Program/include name
- `type` (enum): REPORT/INCLUDE/FUNCTION_GROUP/CLASS
- `lines_of_code` (integer): Total lines
- `includes` (string[]): List of included programs
- `analysis_mode` (enum): LOCAL_ONLY/FOLLOW_CALLS
- `start_time` (timestamp): Analysis start
- `end_time` (timestamp): Analysis end

**Relationships**:
- Contains multiple AnalysisScopes
- Produces one AnalysisResult

### 4. AnalysisScope
Represents a scope boundary in the code.

**Attributes**:
- `type` (enum): GLOBAL/FORM/FUNCTION/METHOD/CLASS
- `name` (string): Scope identifier
- `parent_scope` (string): Containing scope name
- `start_line` (integer): Scope start
- `end_line` (integer): Scope end
- `parameters` (Parameter[]): Input/output parameters
- `local_variables` (string[]): Declared variables

**Relationships**:
- Contains TaintedVariables
- Can have child AnalysisScopes (nested)

### 5. Parameter
Represents a parameter in a routine call.

**Attributes**:
- `name` (string): Parameter name
- `type` (enum): IMPORTING/EXPORTING/CHANGING/TABLES/RETURNING
- `is_tainted` (boolean): Contains tainted data
- `data_type` (string): ABAP data type

### 6. AnalysisResult
The complete output of an analysis run.

**Attributes**:
- `analysis_id` (string): Unique identifier
- `target_program` (string): Main program analyzed
- `total_findings` (integer): Count of findings
- `high_confidence` (integer): Count of HIGH confidence
- `medium_confidence` (integer): Count of MEDIUM confidence
- `low_confidence` (integer): Count of LOW confidence
- `execution_time` (decimal): Seconds to complete
- `lines_analyzed` (integer): Total lines processed
- `csv_output_path` (string): Result file location

**Relationships**:
- Contains multiple Findings
- Associated with one AnalysisTarget

## State Transitions

### TaintedVariable States
```
CREATED → ACTIVE → PROPAGATED → OUT_OF_SCOPE
                 ↘ CONSUMED (used in sink)
```

### Finding Confidence Levels
- **HIGH**: Direct assignment, static names, clear path
- **MEDIUM**: Indirect through structures, some inference
- **LOW**: Dynamic elements, field-symbols, unclear path

## Validation Rules

### TaintedVariable
- Name must be valid ABAP identifier
- Scope must match current parsing context
- Path must start with 'sy-uname'
- Confidence degrades with path length

### Finding
- Operation type must match statement type
- Target name required for all operations
- Field name required for DML operations
- Path must connect to sy-uname source

### AnalysisScope
- End line must be greater than start line
- Parent scope must exist (except GLOBAL)
- Parameters must have valid ABAP types

## Data Flow

```
sy-uname → TaintedVariable → (assignment/copy) → TaintedVariable
                           ↘ (structure field) → TaintedVariable
                           ↘ (table append) → TaintedVariable
                           ↘ (parameter) → TaintedVariable
                           ↘ (DML/RFC) → Finding
```

## Storage Format

### CSV Output Schema
```csv
A,B,C,D,E,F,G,H,I...
1,ZREPORT,42,INSERT,ZTABLE,CREATED_BY,sy-uname,lv_user,ls_data-created_by,INSERT
2,ZINCLUDE,156,CALL_FUNCTION,Z_RFC_UPDATE,,sy-uname,gv_user,EXPORTING iv_user
```

### Internal Working Format (Runtime)
```abap
* Tainted variables table
DATA: gt_tainted TYPE TABLE OF ty_tainted_var.

* Findings table
DATA: gt_findings TYPE TABLE OF ty_finding.

* Scope stack
DATA: gt_scope_stack TYPE TABLE OF ty_scope.
```

## Constraints

### Performance
- Maximum 10,000 tainted variables in memory
- Taint path limited to 20 steps
- Scope depth limited to 50 levels

### Accuracy
- Dynamic table names marked as LOW confidence
- Field-symbols tracked but with MEDIUM confidence
- External calls marked as OUT_OF_SCOPE in LOCAL_ONLY mode