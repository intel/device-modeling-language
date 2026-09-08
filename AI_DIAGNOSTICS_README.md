# AI-Friendly Diagnostics for DMLC

## Overview

The DMLC compiler now supports exporting compilation errors and warnings in a structured JSON format optimized for AI-assisted code generation and error correction.

## Usage

Use the `--ai-json` flag to export diagnostics:

```bash
# Basic usage
dmlc --ai-json errors.json my_device.dml

# With other flags
dmlc --ai-json diagnostics.json -I include_dir my_device.dml
```

## Output Format

The JSON output follows this structure:

```json
{
  "format_version": "1.0",
  "generator": "dmlc-ai-diagnostics",
  "compilation_summary": {
    "input_file": "my_device.dml",
    "dml_version": "1.4",
    "total_diagnostics": 5,
    "total_errors": 3,
    "total_warnings": 2,
    "error_categories": {
      "type_mismatch": 1,
      "undefined_symbol": 1,
      "duplicate_definition": 1,
      "other": 2
    },
    "success": false
  },
  "diagnostics": [
    {
      "type": "error",
      "severity": "error",
      "code": "EUNDEF",
      "message": "undefined symbol 'undefined_variable'",
      "category": "undefined_symbol",
      "location": {
        "file": "my_device.dml",
        "line": 12,
        "location_string": "my_device.dml:12:24"
      },
      "fix_suggestions": [
        "Check if the symbol is defined in imported files",
        "Verify the symbol name spelling",
        "Add necessary import statements if missing"
      ],
      "related_locations": [],
      "documentation_url": "https://intel.github.io/device-modeling-language/language.html#modules-and-imports"
    }
  ]
}
```

## Diagnostic Categories

Errors are automatically categorized to help AI understand fix strategies:

- **syntax**: Syntax errors in DML code
- **type_mismatch**: Type incompatibility errors
- **template_resolution**: Template inheritance and instantiation errors
- **undefined_symbol**: References to undefined symbols
- **duplicate_definition**: Multiple definitions of the same symbol
- **import_error**: Import and module resolution errors
- **semantic**: Other semantic errors
- **compatibility**: DML version compatibility issues
- **deprecation**: Use of deprecated features
- **other**: Miscellaneous warnings and info

## Fix Suggestions

Each diagnostic includes actionable fix suggestions based on the error type:

- **Template ambiguity (EAMBINH)**: Suggests adding `is` statements to clarify precedence
- **Undefined symbols**: Suggests checking imports and spelling
- **Type mismatches**: Suggests type conversions or casting
- **Circular dependencies**: Suggests refactoring strategies
- **Duplicate definitions**: Suggests removing or renaming duplicates
- **Syntax errors**: Suggests checking brackets, semicolons, and DML version

## Integration with AI Code Generators

The JSON format is designed to be easily consumed by AI models:

1. **Structured data**: All information in machine-readable JSON
2. **Error categorization**: Groups errors by fix strategy
3. **Actionable suggestions**: Provides specific fix recommendations
4. **Location tracking**: Precise file, line, and column information
5. **Related locations**: Links to other relevant code locations
6. **Documentation links**: Points to relevant DML manual sections

## Example Workflow

```bash
# 1. Generate DML code with AI
ai-generator > device.dml

# 2. Compile and capture diagnostics
dmlc --ai-json errors.json device.dml

# 3. Parse errors and generate fixes
ai-fixer --errors errors.json --output device_fixed.dml

# 4. Iterate until compilation succeeds
```

## Implementation Details

### Files Added

- `py/dml/ai_diagnostics.py`: Core AI diagnostics module
  - `AIDiagnostic`: Structures individual diagnostic messages
  - `AIFriendlyLogger`: Collects and exports diagnostics
  - `ErrorCategory`: Categorizes errors for AI understanding

### Files Modified

- `py/dml/logging.py`: Integrated AI logger with existing error reporting
- `py/dml/dmlc.py`: Added `--ai-json` CLI flag and output logic

### Integration Points

The AI logger integrates seamlessly with DMLC's existing error system:

1. All errors and warnings pass through `logging.report()`
2. When AI logging is enabled, messages are captured before normal output
3. At compilation end, captured diagnostics are exported to JSON
4. Normal stderr output is unchanged when AI logging is disabled

## Benefits

1. **Preserves all 199 error types**: Leverages DMLC's rich diagnostic system
2. **Minimal changes**: ~500 lines of new code, minimal modifications to existing code
3. **Backward compatible**: Existing tools and workflows are unaffected
4. **AI-optimized**: Structured for easy parsing and fix generation
5. **Extensible**: Easy to add more categories and suggestions

## Future Enhancements

Potential improvements:

- Code snippets: Include surrounding code context
- Fix templates: Provide code templates for common fixes
- Confidence scores: Rate the likelihood of suggested fixes
- Multi-file context: Track cross-file error relationships
- Diff suggestions: Provide specific code diffs for fixes
