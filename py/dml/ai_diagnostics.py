# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

"""
AI-friendly diagnostics module for DMLC.

This module provides JSON-structured error and warning output optimized for
AI-assisted code generation and error correction. It captures all the rich
context from DMLC's error system and formats it in a machine-readable way.
"""

import json
import sys
from typing import List, Dict, Optional, Any
from pathlib import Path

# Import will be available when this file is placed in dml/ directory
try:
    from . import logging as dml_logging
    from . import messages
except ImportError:
    # For standalone testing
    import logging as dml_logging
    messages = None


class ErrorCategory:
    """Categories of errors for AI to understand fix strategies."""
    SYNTAX = "syntax"
    TYPE_MISMATCH = "type_mismatch"
    TEMPLATE_RESOLUTION = "template_resolution"
    UNDEFINED_SYMBOL = "undefined_symbol"
    DUPLICATE_DEFINITION = "duplicate_definition"
    IMPORT_ERROR = "import_error"
    SEMANTIC = "semantic"
    COMPATIBILITY = "compatibility"
    DEPRECATION = "deprecation"
    OTHER = "other"


class AIDiagnostic:
    """Structured diagnostic for AI consumption."""
    
    def __init__(self, log_message: dml_logging.LogMessage):
        self.log_message = log_message
        self.tag = log_message.tag()
        self.message = log_message.msg
        self.site = log_message.site
        self.kind = self._determine_kind()
        self.category = self._categorize()
        self.severity = self._determine_severity()
        
    def _determine_kind(self) -> str:
        """Determine if this is an error or warning."""
        if isinstance(self.log_message, dml_logging.DMLError):
            return "error"
        elif isinstance(self.log_message, dml_logging.DMLWarning):
            return "warning"
        elif isinstance(self.log_message, dml_logging.ICE):
            return "internal_error"
        else:
            return "info"
    
    def _determine_severity(self) -> str:
        """Map to standard severity levels."""
        if isinstance(self.log_message, dml_logging.ICE):
            return "fatal"
        elif isinstance(self.log_message, dml_logging.DMLError):
            return "error"
        elif isinstance(self.log_message, dml_logging.DMLWarning):
            return "warning"
        else:
            return "info"
    
    def _categorize(self) -> str:
        """Categorize error for AI understanding."""
        tag = self.tag
        
        # Syntax errors
        if tag.startswith('ESYNTAX') or 'PARSE' in tag:
            return ErrorCategory.SYNTAX
        
        # Type errors
        if any(x in tag for x in ['TYPE', 'ECAST', 'EBITSLICE', 'EINT']):
            return ErrorCategory.TYPE_MISMATCH
        
        # Template errors
        if any(x in tag for x in ['TEMPLATE', 'EAMBINH', 'ECYCLICTEMPLATE', 
                                    'EABSTEMPLATE', 'ETMETH']):
            return ErrorCategory.TEMPLATE_RESOLUTION
        
        # Symbol resolution
        if any(x in tag for x in ['EUNDEF', 'EREF', 'ENVAR', 'ENOSYM']):
            return ErrorCategory.UNDEFINED_SYMBOL
        
        # Duplicate definitions
        if any(x in tag for x in ['EDUP', 'EREDEF', 'EAMBIG', 'ENAMECOLL']):
            return ErrorCategory.DUPLICATE_DEFINITION
        
        # Import errors
        if any(x in tag for x in ['IMPORT', 'ECYCLICIMP']):
            return ErrorCategory.IMPORT_ERROR
        
        # Compatibility
        if any(x in tag for x in ['ECOMPAT', 'EDML12']):
            return ErrorCategory.COMPATIBILITY
        
        # Deprecation
        if 'WDEPRECATED' in tag or 'WEXPERIMENTAL' in tag:
            return ErrorCategory.DEPRECATION
        
        # Semantic errors (everything else)
        if self.kind == "error":
            return ErrorCategory.SEMANTIC
        
        return ErrorCategory.OTHER
    
    def _get_location_dict(self) -> Optional[Dict[str, Any]]:
        """Extract location information from site."""
        if not self.site:
            return None
        
        try:
            loc_str = self.site.loc() if hasattr(self.site, 'loc') else str(self.site)
            filename = self.site.filename() if hasattr(self.site, 'filename') else None
            lineno = self.site.lineno if hasattr(self.site, 'lineno') else None
            
            location = {
                "file": filename,
                "line": lineno,
                "location_string": loc_str
            }
            
            return location
        except Exception:
            return {"raw": str(self.site)}
    
    def _generate_fix_suggestions(self) -> List[str]:
        """Generate actionable fix suggestions based on error type."""
        tag = self.tag
        suggestions = []
        
        # Debug: print tag to stderr
        import sys
        sys.stderr.write(f"DEBUG: Generating fix suggestions for tag: {tag}\n")
        
        # Name collision
        if tag == 'ENAMECOLL':
            suggestions.append("Rename one of the conflicting definitions to use a unique name")
            suggestions.append("Check if you intended to override a method instead of creating a duplicate")
            suggestions.append("If overriding is intended, ensure the original definition is in a parent template")
        
        # Template ambiguity
        elif tag == 'EAMBINH':
            suggestions.append("Add an 'is <template>' statement to specify template precedence")
            suggestions.append("Check template inheritance order in the object hierarchy")
        
        # Missing 'is' template for method override
        elif tag == 'WNOIS':
            # Extract method name from message if possible
            if self.message and 'implementation of' in self.message:
                # Message format: "implementation of X() without 'is X' is ignored by the standard library"
                import re
                match = re.search(r"implementation of (\w+)\(\)", self.message)
                if match:
                    method_name = match.group(1)
                    suggestions.append(f"Add 'is {method_name};' statement to the containing object to enable the {method_name}() method override")
            suggestions.append("Standard library method overrides require instantiating the corresponding template")
            suggestions.append("Use 'is <method_name>;' in the object definition to activate the method")
        
        # Missing short description
        elif tag == 'WNSHORTDESC':
            suggestions.append("Add a 'desc' parameter to the device with a short description string")
            suggestions.append("Example: param desc = \"Brief description of this device\";")
        
        # Undefined symbol
        elif 'EUNDEF' in tag or 'ENOSYM' in tag:
            suggestions.append("Check if the symbol is defined in imported files")
            suggestions.append("Verify the symbol name spelling")
            suggestions.append("Add necessary import statements if missing")
        
        # Type mismatch
        elif 'TYPE' in tag or 'ECAST' in tag:
            suggestions.append("Check if type conversion is needed")
            suggestions.append("Verify the types of all operands match expected types")
            suggestions.append("Consider explicit type casting if appropriate")
        
        # Circular dependency
        elif 'CYCLIC' in tag:
            suggestions.append("Review import chain to break circular dependency")
            suggestions.append("Consider refactoring shared code into a separate file")
        
        # Duplicate definition (generic)
        elif 'EDUP' in tag or 'EREDEF' in tag:
            suggestions.append("Remove or rename one of the duplicate definitions")
            suggestions.append("Check if definitions are unintentionally duplicated across templates")
        
        # Syntax errors
        elif 'SYNTAX' in tag:
            # Check if this is the specific 'device' declaration error
            if "syntax error at 'device'" in self.message.lower():
                suggestions.append("The 'device' statement must immediately follow 'dml 1.4;' with no other statements in between")
                suggestions.append("Move any statements to after the device declaration")
                suggestions.append("Example: dml 1.4;\ndevice my_device;")
            else:
                suggestions.append("Check for missing semicolons, braces, or parentheses")
                suggestions.append("Verify DML syntax matches the version specified (1.2 vs 1.4)")
        
        return suggestions
    
    def _get_related_locations(self) -> List[Dict[str, Any]]:
        """Extract related location information from error."""
        related = []
        
        # Check for common attributes that contain related sites
        if hasattr(self.log_message, 'other_site') and self.log_message.other_site:
            related.append({
                "file": getattr(self.log_message.other_site, 'filename', lambda: None)(),
                "line": getattr(self.log_message.other_site, 'lineno', None),
                "message": "related location"
            })
        
        if hasattr(self.log_message, 'other_sites'):
            for site in self.log_message.other_sites:
                if site:
                    related.append({
                        "file": getattr(site, 'filename', lambda: None)(),
                        "line": getattr(site, 'lineno', None),
                        "message": "related location"
                    })
        
        if hasattr(self.log_message, 'default_sites'):
            for site in self.log_message.default_sites:
                if site:
                    related.append({
                        "file": getattr(site, 'filename', lambda: None)(),
                        "line": getattr(site, 'lineno', None),
                        "message": "default method candidate"
                    })
        
        return related
    
    def _get_documentation_url(self) -> Optional[str]:
        """Get documentation URL for this error type."""
        base_url = "https://intel.github.io/device-modeling-language"
        
        # Map common error categories to documentation sections
        category_urls = {
            ErrorCategory.SYNTAX: f"{base_url}/language.html#syntax",
            ErrorCategory.TYPE_MISMATCH: f"{base_url}/language.html#types",
            ErrorCategory.TEMPLATE_RESOLUTION: f"{base_url}/language.html#templates",
            ErrorCategory.IMPORT_ERROR: f"{base_url}/language.html#modules-and-imports",
        }
        
        return category_urls.get(self.category)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        location = self._get_location_dict()
        
        diagnostic = {
            "type": self.kind,
            "severity": self.severity,
            "code": self.tag,
            "message": self.message,
            "category": self.category,
            "location": location,
            "fix_suggestions": self._generate_fix_suggestions(),
            "related_locations": self._get_related_locations()
        }
        
        # Add documentation URL if available
        doc_url = self._get_documentation_url()
        if doc_url:
            diagnostic["documentation_url"] = doc_url
        
        # Add error-specific context
        if hasattr(self.log_message, 'method'):
            diagnostic["context"] = {
                "method": getattr(self.log_message.method, 'name', str(self.log_message.method))
            }
        
        return diagnostic


class AIFriendlyLogger:
    """
    Collects diagnostics during compilation for AI-friendly JSON export.
    
    This logger captures all errors and warnings emitted during DMLC compilation
    and formats them into structured JSON that AI models can easily parse and
    use to suggest fixes.
    """
    
    def __init__(self):
        self.diagnostics: List[AIDiagnostic] = []
        self.input_file: Optional[str] = None
        self.dml_version: Optional[tuple] = None
        
    def set_compilation_context(self, input_file: str, dml_version: tuple):
        """Set context about the compilation."""
        self.input_file = input_file
        self.dml_version = dml_version
    
    def log_message(self, log_message: dml_logging.LogMessage):
        """Capture a log message for later export."""
        diagnostic = AIDiagnostic(log_message)
        self.diagnostics.append(diagnostic)
    
    def get_summary(self) -> Dict[str, Any]:
        """Get compilation summary statistics."""
        errors = [d for d in self.diagnostics if d.kind == "error"]
        warnings = [d for d in self.diagnostics if d.kind == "warning"]
        
        # Categorize errors
        categories = {}
        for diag in self.diagnostics:
            categories[diag.category] = categories.get(diag.category, 0) + 1
        
        # Try to infer DML version from diagnostics if not set
        dml_version_str = None
        if self.dml_version:
            dml_version_str = f"{self.dml_version[0]}.{self.dml_version[1]}"
        elif self.diagnostics:
            # Try to extract version from first diagnostic's site
            for diag in self.diagnostics:
                if hasattr(diag.log_message, 'site') and diag.log_message.site:
                    site = diag.log_message.site
                    if hasattr(site, 'dml_version') and callable(site.dml_version):
                        version = site.dml_version()
                        if version:
                            dml_version_str = f"{version[0]}.{version[1]}"
                            break
        
        return {
            "input_file": self.input_file,
            "dml_version": dml_version_str,
            "total_diagnostics": len(self.diagnostics),
            "total_errors": len(errors),
            "total_warnings": len(warnings),
            "error_categories": categories,
            "success": len(errors) == 0
        }
    
    def to_json(self, indent: int = 2) -> str:
        """Export all diagnostics as JSON string."""
        output = {
            "format_version": "1.0",
            "generator": "dmlc-ai-diagnostics",
            "compilation_summary": self.get_summary(),
            "diagnostics": [d.to_dict() for d in self.diagnostics]
        }
        
        return json.dumps(output, indent=indent, ensure_ascii=False)
    
    def write_json_file(self, output_path: Path):
        """Write diagnostics to a JSON file."""
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(self.to_json())
    
    def clear(self):
        """Clear all captured diagnostics."""
        self.diagnostics.clear()


# Global instance for the logger
_ai_logger: Optional[AIFriendlyLogger] = None


def enable_ai_logging():
    """Enable AI-friendly logging globally."""
    global _ai_logger
    _ai_logger = AIFriendlyLogger()
    return _ai_logger


def get_ai_logger() -> Optional[AIFriendlyLogger]:
    """Get the global AI logger instance."""
    return _ai_logger


def disable_ai_logging():
    """Disable AI-friendly logging."""
    global _ai_logger
    _ai_logger = None


def is_ai_logging_enabled() -> bool:
    """Check if AI logging is enabled."""
    return _ai_logger is not None
