# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

set(dmlc_python_dir ${SIMICS_HOST_BIN_DIR}/dml/python)
set(dmlc_library_dir ${SIMICS_HOST_BIN_DIR}/dml)
option(DMLC_RUN_TESTS "Run dmlc unit tests as part of the dmlc build" ON)

function(dmlc_copy_file source output)
  get_filename_component(output_dir ${output} DIRECTORY)
  add_custom_command(
    OUTPUT ${output}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${output_dir}
    COMMAND ${CMAKE_COMMAND} -E copy ${source} ${output}
    DEPENDS ${source}
    VERBATIM)
endfunction()

file(GLOB dmlc_python_sources CONFIGURE_DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/py/dml/*.py
  ${CMAKE_CURRENT_SOURCE_DIR}/py/dml/*.yaml)
list(FILTER dmlc_python_sources EXCLUDE REGEX "_test\\.py$")
list(APPEND dmlc_python_sources ${CMAKE_CURRENT_SOURCE_DIR}/py/__main__.py)

set(dmlc_python_outputs)
foreach(source IN LISTS dmlc_python_sources)
  file(RELATIVE_PATH relative_source ${CMAKE_CURRENT_SOURCE_DIR}/py ${source})
  set(output ${dmlc_python_dir}/${relative_source})
  add_custom_command(
    OUTPUT ${output}
    COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${SIMICS_COPY_PYTHON_PY}
      ${source} ${output}
    DEPENDS ${source} ${SIMICS_COPY_PYTHON_PY}
    VERBATIM)
  list(APPEND dmlc_python_outputs ${output})
endforeach()

set(dmlc_generated_python_outputs)
set(dmlc_env ${dmlc_python_dir}/dml/env.py)
foreach(version 12 14)
  set(generated_parsetab ${CMAKE_CURRENT_BINARY_DIR}/dml${version}_parsetab.py)
  set(parsetab ${dmlc_python_dir}/dml/dml${version}_parsetab.py)
  set(parser_debug ${dmlc_python_dir}/dml${version}_parser.out)
  add_custom_command(
    OUTPUT ${parsetab} ${parser_debug}
    BYPRODUCTS ${generated_parsetab}
    COMMAND ${SIMICS_PYTHON_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/generate_parsetabs.py
      ${dmlc_python_dir} ${version} dml${version}_parsetab ${parser_debug}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      ${generated_parsetab} ${parsetab}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/generate_parsetabs.py
      ${dmlc_python_outputs} ${dmlc_env}
    VERBATIM)
  list(APPEND dmlc_generated_python_outputs ${parsetab} ${parser_debug})
endforeach()

add_custom_command(
  OUTPUT ${dmlc_env}
  COMMAND ${SIMICS_PYTHON_EXECUTABLE}
    ${CMAKE_CURRENT_SOURCE_DIR}/generate_env.py ${dmlc_env}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/generate_env.py
  VERBATIM)
list(APPEND dmlc_generated_python_outputs ${dmlc_env})

set(dmlc_header ${dmlc_library_dir}/include/simics/dmllib.h)
add_custom_command(
  OUTPUT ${dmlc_header}
  COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/copy_h.py
    ${CMAKE_CURRENT_SOURCE_DIR}/include/simics/dmllib.h ${dmlc_header}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/copy_h.py
    ${CMAKE_CURRENT_SOURCE_DIR}/include/simics/dmllib.h
  VERBATIM)

set(dmlc_script_outputs)
foreach(script port_dml.py dead_dml_methods.py)
  set(source ${CMAKE_CURRENT_SOURCE_DIR}/py/${script})
  set(output ${dmlc_python_dir}/${script})
  add_custom_command(
    OUTPUT ${output}
    COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${SIMICS_COPY_PYTHON_PY}
      ${source} ${output}
    DEPENDS ${source} ${SIMICS_COPY_PYTHON_PY}
    VERBATIM)
  list(APPEND dmlc_script_outputs ${output})
endforeach()

set(dmlc_data_outputs ${dmlc_python_dir}/LICENSE
  ${dmlc_library_dir}/include/simics/LICENSE)
foreach(version 1.2 1.4)
  list(APPEND dmlc_data_outputs ${dmlc_library_dir}/${version}/LICENSE)
endforeach()
foreach(output IN LISTS dmlc_data_outputs)
  if(output STREQUAL "${dmlc_python_dir}/LICENSE")
    set(source ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE)
  elseif(output STREQUAL "${dmlc_library_dir}/include/simics/LICENSE")
    set(source ${CMAKE_CURRENT_SOURCE_DIR}/include/simics/LICENSE)
  elseif(output MATCHES "/dml/([^/]+)/LICENSE$")
    set(source ${CMAKE_CURRENT_SOURCE_DIR}/lib/${CMAKE_MATCH_1}/LICENSE)
  else()
    message(FATAL_ERROR "Unknown dmlc license output: ${output}")
  endif()
  dmlc_copy_file(${source} ${output})
endforeach()

file(GLOB dmlc_library_sources CONFIGURE_DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/lib/*/*.dml)
set(dmlc_library_outputs)
foreach(source IN LISTS dmlc_library_sources)
  file(RELATIVE_PATH relative_source ${CMAKE_CURRENT_SOURCE_DIR}/lib ${source})
  set(output ${dmlc_library_dir}/${relative_source})
  dmlc_copy_file(${source} ${output})
  list(APPEND dmlc_library_outputs ${output})
endforeach()

add_custom_target(dmlc-files DEPENDS
  ${dmlc_python_outputs}
  ${dmlc_generated_python_outputs}
  ${dmlc_header}
  ${dmlc_script_outputs}
  ${dmlc_data_outputs}
  ${dmlc_library_outputs})

if(SIMICS_REPO_ROOT AND BUILD_MODE STREQUAL "Normal")
  set(dmlc_api_dir ${SIMICS_REPO_ROOT}/${SIMICS_HOST_TYPE}/bin/dml/api)
else()
  set(dmlc_api_dir ${SIMICS_BASE_DIR}/${SIMICS_HOST_TYPE}/bin/dml/api)
endif()
# TODO: hardcoded for now, 6 and 7 are the API versions for Simics 7, which is
# the only maintained branch. Deducing versions programmatically would require
# additional build deps.
set(dmlc_api_versions 6 7)

set(dmlc_local_preparse_outputs)
foreach(version 1.2 1.4)
  set(marker ${dmlc_library_dir}/${version}.d)
  set(output_dir ${dmlc_library_dir}/${version})
  add_custom_command(
    OUTPUT ${marker}
    COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/dmlast.py
      ${dmlc_python_dir} ${output_dir} ${output_dir} ${marker}
    DEPFILE ${marker}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/dmlast.py dmlc-files
    VERBATIM)
  list(APPEND dmlc_local_preparse_outputs ${marker})
endforeach()

set(dmlc_api_preparse_outputs)
foreach(api_version IN LISTS dmlc_api_versions)
  foreach(version 1.2 1.4)
    set(api_source_dir ${dmlc_api_dir}/${api_version}/${version})
    set(api_output_dir ${SIMICS_HOST_BIN_DIR}/dml/api/${api_version}/${version})
    set(marker ${SIMICS_HOST_BIN_DIR}/dml/api/${api_version}/${version}.stamp)
    set(api_dml_dependencies)
    if(SIMICS_REPO_ROOT AND BUILD_MODE STREQUAL "Normal")
      list(APPEND api_dml_dependencies ${api_source_dir}/.import-api)
    endif()
    add_custom_command(
      OUTPUT ${marker}
      COMMAND ${SIMICS_PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/dmlast.py
        ${dmlc_python_dir} ${api_output_dir} ${api_source_dir} ${marker}
      DEPFILE ${marker}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/dmlast.py dmlc-files
        ${api_dml_dependencies}
      VERBATIM)
    list(APPEND dmlc_api_preparse_outputs ${marker})
  endforeach()
endforeach()

add_custom_target(dmlc-local-preparse DEPENDS ${dmlc_local_preparse_outputs})
add_custom_target(dmlc-api-preparse DEPENDS ${dmlc_api_preparse_outputs})
add_custom_target(dmlc DEPENDS ${dmlc_local_preparse_outputs}
  ${dmlc_api_preparse_outputs})
add_dependencies(dmlc dmlc-files dmlc-local-preparse dmlc-api-preparse)

if(DMLC_RUN_TESTS)
  if(NOT CMAKE_C_COMPILER)
    message(FATAL_ERROR
      "DMLC_RUN_TESTS is ON, but no C compiler is enabled; configure with "
      "-DDMLC_RUN_TESTS=OFF to skip dmlc tests")
  endif()

  file(GLOB dmlc_test_sources CONFIGURE_DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/py/dml/*_test.py)
  get_filename_component(dmlc_host_dir ${SIMICS_HOST_BIN_DIR} DIRECTORY)
  set(dmlc_test_stamps)
  foreach(test_source IN LISTS dmlc_test_sources)
    get_filename_component(test_name ${test_source} NAME_WE)
    set(test_stamp ${CMAKE_CURRENT_BINARY_DIR}/${test_name}-passed)
    add_custom_command(
      OUTPUT ${test_stamp}
      COMMAND ${CMAKE_COMMAND} -E env
        "SIMICS_BASE=${SIMICS_BASE_DIR}"
        "CC=${CMAKE_C_COMPILER}"
        ${SIMICS_PYTHON_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/run_unit_tests.py
        ${dmlc_host_dir} ${test_source}
      COMMAND ${CMAKE_COMMAND} -E touch ${test_stamp}
      DEPENDS ${test_source}
        ${CMAKE_CURRENT_SOURCE_DIR}/run_unit_tests.py
        ${dmlc_python_outputs} ${dmlc_generated_python_outputs}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      VERBATIM)
    list(APPEND dmlc_test_stamps ${test_stamp})
  endforeach()
  add_custom_target(dmlc-unit-tests DEPENDS ${dmlc_test_stamps})
  add_dependencies(dmlc dmlc-unit-tests)
endif()

add_subdirectory(doc/1.2)
add_subdirectory(doc/1.4)

# TODO: this is a temporary compat hack to make files appear where the
# `dmlc-doc-wiki` target (in a different repo) expects them. It should be
# removed as soon as possible.
if(SIMICS_REPO_ROOT AND "${BUILD_MODE}" STREQUAL "Normal"
   AND NOT USE_COVERITY AND NOT SANITIZE_ALL_MODULES)
  set(dmlc_make_generated_md_dir
    ${SIMICS_REPO_ROOT}/${SIMICS_HOST_TYPE}/obj/modules/dmlc/generated-md-1.4)
  set(dmlc_wiki_generated_md
    grammar.md
    messages.md
    changes-auto.md
    dml-builtins.md
    utility.md
    deprecations-auto.md
    provisional-auto.md)
  set(dmlc_make_wiki_md)
  set(dmlc_make_wiki_sources)
  set(dmlc_make_wiki_copy_commands)
  foreach(md_file IN LISTS dmlc_wiki_generated_md)
    set(source ${CMAKE_CURRENT_BINARY_DIR}/doc/1.4/${md_file})
    set(output ${dmlc_make_generated_md_dir}/${md_file})
    list(APPEND dmlc_make_wiki_sources ${source})
    list(APPEND dmlc_make_wiki_md
      ${output})
    list(APPEND dmlc_make_wiki_copy_commands
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${source} ${output})
  endforeach()
  add_custom_command(
    OUTPUT ${dmlc_make_wiki_md}
    COMMAND ${CMAKE_COMMAND} -E make_directory
      ${dmlc_make_generated_md_dir}
    ${dmlc_make_wiki_copy_commands}
    DEPENDS ${dmlc_make_wiki_sources}
    VERBATIM)
  add_custom_target(dmlc-doc-wiki-inputs DEPENDS ${dmlc_make_wiki_md})
  add_dependencies(dmlc dmlc-doc-wiki-inputs)
endif()
