CREATE TABLE sys_model(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    app_id BIGINT(32)    COMMENT 'App ID' ,
    label_name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Label Name' ,
    model_name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Model Name' ,
    soft_delete TINYINT(1)   DEFAULT 0 COMMENT 'Enable Soft Delete' ,
    default_order VARCHAR(256)   DEFAULT '' COMMENT 'Default Order' ,
    display_name VARCHAR(255)   DEFAULT '' COMMENT 'Display Name' ,
    search_name VARCHAR(255)   DEFAULT '' COMMENT 'Search Name' ,
    table_name VARCHAR(64)   DEFAULT '' COMMENT 'Table Name' ,
    timeline TINYINT(1)   DEFAULT 0 COMMENT 'Is Timeline Model' ,
    id_strategy VARCHAR(64)   DEFAULT 'DbAutoID' COMMENT 'ID Strategy' ,
    storage_type VARCHAR(64)   DEFAULT 'RDBMS' COMMENT 'Storage Type' ,
    version_lock TINYINT(1)   DEFAULT 0 COMMENT 'Enable Version Lock' ,
    multi_tenant TINYINT(1)   DEFAULT 0 COMMENT 'Enable Multi-tenancy' ,
    partition_field VARCHAR(64)   DEFAULT '' COMMENT 'Partition Field' ,
    description VARCHAR(256)   DEFAULT '' COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    disabled TINYINT(1)   DEFAULT 0 COMMENT 'Disabled' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'System Model';


ALTER TABLE sys_model ADD UNIQUE INDEX uniq_modelname (model_name);

CREATE TABLE sys_model_trans(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    language_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Language Code' ,
    row_id BIGINT(32)    COMMENT 'Row ID' ,
    label_name VARCHAR(64)    COMMENT 'Label Name' ,
    description VARCHAR(256)    COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'System Model Translation';

CREATE TABLE sys_field(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    app_id BIGINT(32)    COMMENT 'App ID' ,
    label_name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Label Name' ,
    field_name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Field Name' ,
    column_name VARCHAR(64)   DEFAULT '' COMMENT 'Column Name' ,
    model_name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Model Name' ,
    model_id BIGINT(32)    COMMENT 'Model ID' ,
    description VARCHAR(256)   DEFAULT '' COMMENT 'Description' ,
    field_type VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Field Type' ,
    option_set_code VARCHAR(64)   DEFAULT '' COMMENT 'Option Set Code;Set when FieldType is Option or MultiOption' ,
    related_model VARCHAR(64)   DEFAULT '' COMMENT 'Related Model' ,
    related_field VARCHAR(64)   DEFAULT '' COMMENT 'Related Field' ,
    inverse_link_field VARCHAR(64)   DEFAULT '' COMMENT 'Inverse Link Field;Set when FieldType is ManyToMany' ,
    auto_bind_many TINYINT(1)   DEFAULT 0 COMMENT 'Auto Bind Many;Whether to read the XToMany field automatically' ,
    auto_expand_many TINYINT(1)   DEFAULT 0 COMMENT 'Auto Expand Many' ,
    cascaded_field VARCHAR(256)   DEFAULT '' COMMENT 'Cascaded Field' ,
    display_name VARCHAR(255)   DEFAULT '' COMMENT 'Display Name' ,
    filters VARCHAR(256)   DEFAULT '' COMMENT 'Filters;Filters for relational fields.' ,
    default_value VARCHAR(256)   DEFAULT '' COMMENT 'Default Value' ,
    length INT(11)   DEFAULT 0 COMMENT 'Length' ,
    scale TINYINT(4)   DEFAULT 0 COMMENT 'Scale' ,
    required TINYINT(1)   DEFAULT 0 COMMENT 'Is Required' ,
    readonly TINYINT(1)   DEFAULT 0 COMMENT 'Is Readonly' ,
    hidden TINYINT(1)   DEFAULT 0 COMMENT 'Hidden' ,
    translatable TINYINT(1)   DEFAULT 0 COMMENT 'Translatable' ,
    copyable TINYINT(1)   DEFAULT 0 COMMENT 'Copyable' ,
    unsearchable TINYINT(1)   DEFAULT 0 COMMENT 'Unsearchable' ,
    computed TINYINT(1)   DEFAULT 0 COMMENT 'Is Computed' ,
    expression TEXT(20000)    COMMENT 'Expression' ,
    dynamic TINYINT(1)   DEFAULT 0 COMMENT 'Dynamic Field' ,
    encrypted TINYINT(1)   DEFAULT 0 COMMENT 'Is Encrypted' ,
    masking_type VARCHAR(64)   DEFAULT '' COMMENT 'Masking Type' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    disabled TINYINT(1)   DEFAULT 0 COMMENT 'Disabled' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'System Field';


ALTER TABLE sys_field ADD UNIQUE INDEX uniq_modelname_fieldname (model_name,field_name);

CREATE TABLE sys_field_trans(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    language_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Language Code' ,
    row_id BIGINT(32)    COMMENT 'Row ID' ,
    label_name VARCHAR(64)    COMMENT 'Label Name' ,
    description VARCHAR(256)    COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'System Field Translation';

CREATE TABLE sys_option_set(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    app_id BIGINT(32)    COMMENT 'App ID' ,
    name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Option Set Name' ,
    option_set_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Option Set Code' ,
    description VARCHAR(256)   DEFAULT '' COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    disabled TINYINT(1)   DEFAULT 0 COMMENT 'Disabled' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'Option Set';

CREATE TABLE sys_option_set_trans(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    language_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Language Code' ,
    row_id BIGINT(32)    COMMENT 'Row ID' ,
    name VARCHAR(64)    COMMENT 'Option Set Name' ,
    description VARCHAR(256)    COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'Option Set Translation';

CREATE TABLE sys_option_item(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    app_id BIGINT(32)    COMMENT 'App ID' ,
    option_set_id BIGINT(32)    COMMENT 'Option Set ID' ,
    option_set_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Option Set Code' ,
    sequence INT(11) NOT NULL  DEFAULT 1 COMMENT 'Sequence' ,
    item_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Item Code' ,
    item_name VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Item Name' ,
    parent_item_code VARCHAR(64)   DEFAULT '' COMMENT 'Parent Item Code' ,
    item_color VARCHAR(64)   DEFAULT '' COMMENT 'Item Color' ,
    description VARCHAR(256)   DEFAULT '' COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    disabled TINYINT(1)   DEFAULT 0 COMMENT 'Disabled' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'Option Set Items';

CREATE TABLE sys_option_item_trans(
    id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
    language_code VARCHAR(64) NOT NULL  DEFAULT '' COMMENT 'Language Code' ,
    row_id BIGINT(32)    COMMENT 'Row ID' ,
    item_name VARCHAR(64)    COMMENT 'Item Name' ,
    description VARCHAR(256)    COMMENT 'Description' ,
    created_time DATETIME    COMMENT 'Created Time' ,
    created_by VARCHAR(32)    COMMENT 'Created By' ,
    created_id BIGINT(32)    COMMENT 'Created ID' ,
    updated_time DATETIME    COMMENT 'Updated Time' ,
    updated_id BIGINT(32)    COMMENT 'Updated ID' ,
    updated_by VARCHAR(32)    COMMENT 'Updated By' ,
    PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'Option Set Items Translation';

