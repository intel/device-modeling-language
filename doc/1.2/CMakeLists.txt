# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

set(dmlc_doc_name dml-1.2-reference-manual)
set(dmlc_python_dir ${SIMICS_HOST_BIN_DIR}/dml/python)
set(dmlc_doc_tools_dir ${CMAKE_CURRENT_SOURCE_DIR}/../1.4)
file(GLOB doc_sources CONFIGURE_DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/*.md ${CMAKE_CURRENT_SOURCE_DIR}/toc.json)
set(generated_files)

set(grammar ${CMAKE_CURRENT_BINARY_DIR}/grammar.md)
add_custom_command(
  OUTPUT ${grammar}
  COMMAND ${SIMICS_PYTHON_EXECUTABLE}
    ${dmlc_doc_tools_dir}/grammar_to_md.py ${dmlc_python_dir}
    ${dmlc_python_dir}/dml12_parser.out ${grammar}
  DEPENDS dmlc-files ${dmlc_doc_tools_dir}/grammar_to_md.py
    ${dmlc_python_dir}/dml12_parser.out
  VERBATIM)
list(APPEND generated_files ${grammar})

set(messages ${CMAKE_CURRENT_BINARY_DIR}/messages.md)
add_custom_command(
  OUTPUT ${messages}
  COMMAND ${SIMICS_PYTHON_EXECUTABLE}
    ${dmlc_doc_tools_dir}/messages_to_md.py ${dmlc_python_dir}
    1.2 ${messages}
  DEPENDS dmlc-files ${dmlc_doc_tools_dir}/messages_to_md.py
  VERBATIM)
list(APPEND generated_files ${messages})

foreach(kind deprecations provisional)
  set(output ${CMAKE_CURRENT_BINARY_DIR}/${kind}-auto.md)
  set(script ${dmlc_doc_tools_dir}/${kind}_to_md.py)
  set(header ${CMAKE_CURRENT_SOURCE_DIR}/${kind}-header.md)
  if(kind STREQUAL "provisional")
    add_custom_command(
      OUTPUT ${output}
      COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${script} ${dmlc_python_dir}
        ${header} ${output} 1.2
      DEPENDS dmlc-files ${script} ${header}
      VERBATIM)
  else()
    add_custom_command(
      OUTPUT ${output}
      COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${script} ${dmlc_python_dir}
        ${header} ${output}
      DEPENDS dmlc-files ${script} ${header}
      VERBATIM)
  endif()
  list(APPEND generated_files ${output})
endforeach()

set(link_validation_stamp ${CMAKE_CURRENT_BINARY_DIR}/links-validated)
add_custom_command(
  OUTPUT ${link_validation_stamp}
  COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/../../validate_md_links.py
    ${CMAKE_CURRENT_SOURCE_DIR}/toc.json ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E touch ${link_validation_stamp}
  DEPENDS ${doc_sources} ${generated_files}
    ${CMAKE_CURRENT_SOURCE_DIR}/../../validate_md_links.py
  VERBATIM)

simics_add_documentation(${dmlc_doc_name}
  DEPENDS ${doc_sources} ${generated_files} ${link_validation_stamp})
if(TARGET pkgs_dir)
  add_dependencies(doc-${dmlc_doc_name} pkgs_dir)
  add_dependencies(doc_${dmlc_doc_name} pkgs_dir)
endif()
