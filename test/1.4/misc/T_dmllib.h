/*
  © 2022-2023 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
*/

#define TEST_REPR(repr, indices, args, kind, msg)                       \
    ASSERT_MSG(_simple_event_data_representation(repr, indices, args)   \
               == _Simple_Event_Data_Format_ ## kind, msg)

#define TEST_REPR_FMT(repr, indices, args, kind, ...)                   \
    ASSERT_FMT(_simple_event_data_representation(repr, indices, args)   \
               == _Simple_Event_Data_Format_ ## kind, __VA_ARGS__)

void simple_event_data_representation_tests(void) {
    attr_value_t indices = SIM_make_attr_list(
        3,
        SIM_make_attr_uint64(2),
        SIM_make_attr_uint64(1),
        SIM_make_attr_uint64(4));

    attr_value_t args = SIM_make_attr_list(
        3,
        SIM_make_attr_boolean(true),
        SIM_make_attr_int64(-4),
        SIM_make_attr_floating(4.4));

    attr_value_t indices_args = SIM_make_attr_list(2, indices, args);

    attr_value_t entries[3];
    for (int i = 0; i < 3; ++i) {
        entries[i] = SIM_make_attr_list(2, SIM_make_attr_string("key"),
                                        SIM_make_attr_nil());
    }

    attr_value_t one_entry_dict = SIM_make_attr_list(1, entries[0]);
    attr_value_t two_entry_dict = SIM_make_attr_list(2, entries[1],
                                                     entries[2]);

    // Tests for Legacy representation or invalid representations
    TEST_REPR(indices, true, false, Legacy, "indices");
    // The integer list might just as well be arguments
    TEST_REPR(indices, false, true, Legacy, "indices/args");
    TEST_REPR(indices, true, true, Invalid, "missing args");
    TEST_REPR(indices, false, false, Invalid,
              "indices/args when not expected");


    TEST_REPR(args, false, true, Legacy, "args");
    TEST_REPR(args, true, false, Invalid, "args when not expected");
    TEST_REPR(args, true, true, Invalid, "missing indices");

    TEST_REPR(indices_args, true, true, Legacy, "indices & args");
    TEST_REPR(indices_args, true, false, Invalid,
              "indices & args when only indices expected");
    // [[index0, ...], [arg0, ..]] might just as well be as the serialized
    // representation of two arguments
    TEST_REPR(indices_args, false, true, Legacy,
              "indices & args when only args expected");
    TEST_REPR(indices_args, false, false, Invalid,
              "indices & args when neither expected");

    // Tests for pseudodict representation
    // has_params, has_indices should be irrelevant
    for (int i = 0; i < 4; ++i) {
        bool has_indices = i & 1;
        bool has_params  = i & 2;
        TEST_REPR_FMT(one_entry_dict, has_indices, has_params, Pseudodict,
                      "single-entry, indices=%d, params=%d",
                      has_indices, has_params);
        TEST_REPR_FMT(two_entry_dict, has_indices, has_params, Pseudodict,
                      "double-entry, indices=%d, params=%d",
                      has_indices, has_params);
    }

    SIM_attr_free(&indices_args);

    SIM_attr_free(&one_entry_dict);
    SIM_attr_free(&two_entry_dict);
}

void simple_event_data_deconstruct_pseudodict_tests(void) {
    attr_value_t *out_indices   = NULL;
    attr_value_t *out_arguments = NULL;
    attr_value_t *out_domains   = NULL;

    attr_value_t indices_entry = SIM_make_attr_list(
        2, SIM_make_attr_string("indices"),
        SIM_make_attr_string("indices_val"));
    attr_value_t args_entry = SIM_make_attr_list(
        2, SIM_make_attr_string("arguments"),
        SIM_make_attr_string("arguments_val"));
    attr_value_t domains_entry = SIM_make_attr_list(
        2, SIM_make_attr_string("domains"),
        SIM_make_attr_string("domains_val"));

    for (int i = 0; i < 1<<3; ++i) {
        int entry_index = 0;
        attr_value_t entries[3];
        if (i & 1)
            entries[entry_index++] = SIM_attr_copy(indices_entry);
        if (i & 2)
            entries[entry_index++] = SIM_attr_copy(args_entry);
        if (i & 4)
            entries[entry_index++] = SIM_attr_copy(domains_entry);

        attr_value_t pseudodict = SIM_alloc_attr_list(entry_index);
        attr_value_t *entry_list = SIM_attr_list(pseudodict);
        for (uint32 j = 0; j < entry_index; ++j) {
            entry_list[j] = entries[j];
        }

        set_error_t error = _simple_event_data_deconstruct_pseudodict(
            pseudodict, &out_indices, &out_arguments, &out_domains);

        ASSERT_FMT(error == Sim_Set_Ok,
            "combination test: errored with %d. indices=%d; args=%d; "
            "domains=%d", error, !!(i & 1), !!(i & 2), !!(i & 4));

        ASSERT_FMT(!out_indices == !(i & 1),
            "combination: bad indices decons. indices=%d; args=%d; "
            "domains=%d", !!(i & 1), !!(i & 2), !!(i & 4));
        ASSERT_FMT(!out_arguments == !(i & 2),
            "combination: bad args decons. indices=%d; args=%d; "
            "domains=%d", !!(i & 1), !!(i & 2), !!(i & 4));
        ASSERT_FMT(!out_domains == !(i & 4),
            "combination: bad domains decons. indices=%d; args=%d; "
            "domains=%d", !!(i & 1), !!(i & 2), !!(i & 4));
        out_indices = NULL;
        out_arguments = NULL;
        out_domains = NULL;
        SIM_attr_free(&pseudodict);
    }

    // Order is irrelevant
    attr_value_t pseudodict = SIM_make_attr_list(
        3, SIM_attr_copy(domains_entry), SIM_attr_copy(indices_entry),
        SIM_attr_copy(args_entry));
    attr_value_t *entries = SIM_attr_list(pseudodict);

    set_error_t error = _simple_event_data_deconstruct_pseudodict(
        pseudodict, &out_indices, &out_arguments, &out_domains);

    ASSERT_FMT(error == Sim_Set_Ok, "order test: errored with %d", error);
    ASSERT_MSG(out_indices == &SIM_attr_list(entries[1])[1],
        "order test: bad indices decons.");
    ASSERT_MSG(
        out_arguments == &SIM_attr_list(entries[2])[1],
        "order test: bad args decons");
    ASSERT_MSG(out_domains == &SIM_attr_list(entries[0])[1],
        "order test: bad domains decons");

    SIM_attr_free(&pseudodict);
    SIM_attr_free(&indices_entry);
    SIM_attr_free(&args_entry);
    SIM_attr_free(&domains_entry);
}

void deserialize_simple_event_indices_tests(void) {
    attr_value_t indexless = SIM_make_attr_list(0);
    attr_value_t one_index = SIM_make_attr_list(1,
                                                SIM_make_attr_uint64(4));
    attr_value_t indices = SIM_make_attr_list(
        3,
        SIM_make_attr_uint64(4),
        SIM_make_attr_uint64(2),
        SIM_make_attr_uint64(7));

    uint32 *out_indices = NULL;

    set_error_t error = _deserialize_simple_event_indices(
        NULL, 0, &indexless, &out_indices);
    ASSERT_FMT(error == Sim_Set_Ok, "indexless errored with %d", error);
    ASSERT_MSG(out_indices == NULL, "indexless: bad out_indices");
    MM_FREE(out_indices);

    error = _deserialize_simple_event_indices(
        (const uint32 []) {7}, 1, &one_index, &out_indices);
    ASSERT_FMT(error == Sim_Set_Ok, "one_index errored with %d", error);
    ASSERT_MSG(out_indices[0] == 4, "one_index: bad out_indices");
    MM_FREE(out_indices);

    error = _deserialize_simple_event_indices(
        (const uint32 []) {10, 10, 10}, 3, &indices, &out_indices);
    ASSERT_FMT(error == Sim_Set_Ok, "indices errored with %d", error);
    ASSERT_MSG(out_indices[0] == 4 && out_indices[1] == 2
               && out_indices[2] == 7, "indices: bad out_indices");

    MM_FREE(out_indices);
    SIM_attr_free(&indexless);
    SIM_attr_free(&one_index);
    SIM_attr_free(&indices);
}


set_error_t stub_deserializer(attr_value_t *val, void *dest) {
    ASSERT(SIM_attr_is_list(*val));
    if (!(SIM_attr_list_size(*val) == 1
          && SIM_attr_is_string(SIM_attr_list_item(*val, 0)))) {
        return Sim_Set_Illegal_Type;
    }
    strncpy(dest, SIM_attr_string(SIM_attr_list_item(*val, 0)), 511);
    ((char *)dest)[511] = '\0';
    return Sim_Set_Ok;
}

void deserialize_simple_event_arguments_tests(void) {
    // Minimal tests as _deserialize_simple_event_arguments is mostly
    // just a wrapper around a call to the serializer argument

    char *out_str = NULL;
    attr_value_t empty_list = SIM_make_attr_list(0);
    attr_value_t string_list = SIM_make_attr_list(1,
        SIM_make_attr_string("arg"));

    set_error_t error = _deserialize_simple_event_arguments(
        512, stub_deserializer, &empty_list, (void **) &out_str);
    ASSERT_FMT(
        error == Sim_Set_Illegal_Type, "empty_list (no)errored with %d",
        error);
    ASSERT_MSG(out_str == NULL, "empty_list: bad out_str");

    error = _deserialize_simple_event_arguments(
        512, stub_deserializer, &string_list, (void **) &out_str);
    ASSERT_FMT(error == Sim_Set_Ok, "empty_list errored with %d", error);
    ASSERT_MSG(strcmp("arg", out_str) == 0, "string_list: bad out_str");

    free(out_str);
    SIM_attr_free(&empty_list);
    SIM_attr_free(&string_list);
}

void deserialize_simple_event_domains_tests(void) {
    attr_value_t indexless = SIM_make_attr_list(0);
    attr_value_t one_index = SIM_make_attr_list(1,
                                                SIM_make_attr_uint64(4));
    attr_value_t indices = SIM_make_attr_list(
        3,
        SIM_make_attr_uint64(4),
        SIM_make_attr_uint64(2),
        SIM_make_attr_uint64(7));

    attr_value_t domain_a = SIM_make_attr_list(
        2, SIM_make_attr_string("a"), indexless);
    attr_value_t domain_b = SIM_make_attr_list(
        2, SIM_make_attr_string("b"), one_index);
    attr_value_t domain_c = SIM_make_attr_list(
        2, SIM_make_attr_string("c"), indices);

    attr_value_t domainless = SIM_make_attr_list(0);
    attr_value_t one_domain = SIM_make_attr_list(1,
                                                 SIM_attr_copy(domain_a));
    attr_value_t domains    = SIM_make_attr_list(
        3, domain_c, domain_a, domain_b);

    _id_info_t domain_a_info = {"a", NULL, 0, 1 };
    _id_info_t domain_b_info = {"b", (const uint32 []) {5}, 1, 2 };
    _id_info_t domain_c_info = {"b", (const uint32 []) {7, 4, 10}, 3, 3 };
    ht_str_table_t ht = HT_STR_NULL(false);
    ht_insert_str(&ht, "a", &domain_a_info);
    ht_insert_str(&ht, "b", &domain_b_info);
    ht_insert_str(&ht, "c", &domain_c_info);

    _identity_t *out_domains = NULL;
    uint64 out_no_domains = 0;

    set_error_t error = _deserialize_simple_event_domains(
        &ht, &domainless, &out_domains, &out_no_domains);
    ASSERT_FMT(error == Sim_Set_Ok, "domainless errored with %d", error);
    ASSERT_MSG(out_no_domains == 0, "domainless: bad out_no_domains");
    ASSERT_MSG(out_domains == NULL, "domainless: bad out_domains");

    error = _deserialize_simple_event_domains(
        &ht, &one_domain, &out_domains, &out_no_domains);
    ASSERT_FMT(error == Sim_Set_Ok, "one_domain errored with %d", error);
    ASSERT_MSG(out_no_domains == 1, "one_domain: bad out_no_domains");
    ASSERT_MSG(out_domains[0].id == 1 && out_domains[0].encoded_index == 0,
        "one_domain: bad out_domains");
    MM_FREE(out_domains);

    error = _deserialize_simple_event_domains(
        &ht, &domains, &out_domains, &out_no_domains);
    ASSERT_FMT(error == Sim_Set_Ok, "domains errored with %d", error);
    ASSERT_MSG(out_no_domains == 3, "domains: bad out_no_domains");
    uint32 c_flat_index = (4 * 4 + 2) * 10 + 7;
    ASSERT_MSG(
        out_domains[0].id == 3
        && out_domains[0].encoded_index == c_flat_index
        && out_domains[1].id == 1 && out_domains[1].encoded_index == 0
        && out_domains[2].id == 2 && out_domains[2].encoded_index == 4,
        "domains: bad out_domains");

    MM_FREE(out_domains);
    SIM_attr_free(&domainless);
    SIM_attr_free(&one_domain);
    SIM_attr_free(&domains);
    ht_delete_str_table(&ht, false);
}

void stub_serializer(const void *src, attr_value_t *dst) {
    if (!src) {
        *dst = SIM_make_attr_nil();
    } else {
        char buf[512] = {0};
        strncpy(buf, src, 511);
        *dst = SIM_make_attr_list(1, SIM_make_attr_string(buf));
    }
}

void round_trip_tests(void) {
    _id_info_t infos[3] = {
        {"a", NULL,                         0, 0},
        {"b", (const uint32 []) {4},        1, 1},
        {"c", (const uint32 []) {7, 4, 10}, 3, 2}
    };
    ht_str_table_t ht = HT_STR_NULL(false);
    for (int i = 0; i < 3; ++i) {
        ht_insert_str(&ht, infos[i].logname, &infos[i]);
    }

    _simple_event_data_t in_event_data = {
        .indices    = (uint32 []) { 2, 3, 5},
        .args       = (void *) "event_data",
        .domains    = (_identity_t []) {{2, 187}, {1, 2}, {0, 0}},
        .no_domains = 3
    };

    attr_value_t serialized = _serialize_simple_event_data(
        3, stub_serializer, infos, &in_event_data);

    _simple_event_data_t *out_event_data = NULL;

    set_error_t error = _deserialize_simple_event_data(
        (const uint32 []) {3, 5, 7}, 3, 512, stub_deserializer,
        &ht, serialized, &out_event_data);

    ASSERT_FMT(error == Sim_Set_Ok, "errored with %d", error);
    ASSERT_MSG(out_event_data != NULL, "out_event_data is NULL");
    ASSERT_MSG(
        memcmp(in_event_data.indices, out_event_data->indices,
               sizeof(uint32)*3) == 0, "indices mismatch");
    ASSERT_MSG(strncmp(in_event_data.args, out_event_data->args, 512) == 0,
        "args mismatch");
    ASSERT_MSG(out_event_data->no_domains == 3, "no_domains mismatch");
    ASSERT_MSG(
        memcmp(in_event_data.domains, out_event_data->domains,
               sizeof(_identity_t)*3) == 0, "domains mismatch");

    _free_simple_event_data(*out_event_data);
    MM_FREE(out_event_data);
    SIM_attr_free(&serialized);
}


void dmllib_test(void) {
    simple_event_data_representation_tests();
    simple_event_data_deconstruct_pseudodict_tests();
    deserialize_simple_event_indices_tests();
    deserialize_simple_event_arguments_tests();
    deserialize_simple_event_domains_tests();
    round_trip_tests();
}
