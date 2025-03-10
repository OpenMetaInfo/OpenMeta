package info.openmeta.framework.orm.meta;

import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.enums.StorageType;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * MetaModel object
 */
@Data
public class MetaModel implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private Long appId;

    private String labelName;

    private String modelName;

    private String tableName;

    private IdStrategy idStrategy;

    private StorageType storageType;

    // Model level default orders, such as "name ASC"
    private String defaultOrder;

    // Display name fields
    private List<String> displayName;

    // Search name fields
    private List<String> searchName;

    private String description;

    private boolean timeline;

    private boolean softDelete;

    // Compatible with different soft delete field names of historical systems, such as "is_deleted"
    private String softDeleteField;

    private boolean enableActiveControl;

    private boolean versionLock;

    private boolean multiTenant;

    private String dataSource;

    private List<String> businessKey;

    private String partitionField;

    private List<MetaField> storedComputedFields = new ArrayList<>();

    private List<MetaField> storedCascadedFields = new ArrayList<>();

}