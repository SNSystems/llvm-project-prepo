configure_lit_site_cfg(
    ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
    ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
)

add_lit_testsuite(check-rld
    "Running the rld regression tests"
    ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS
        FileCheck
        count
        not
        llvm-readelf
        pstore-import
        repo-create-ticket
        rld
)
set_target_properties(check-rld PROPERTIES FOLDER "Tests")
