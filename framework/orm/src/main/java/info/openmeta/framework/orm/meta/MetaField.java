package info.openmeta.framework.orm.meta;

import info.openmeta.framework.orm.enums.MaskingType;
import info.openmeta.framework.orm.enums.FieldType;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * MetaField object
 */
@Data
public class MetaField implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private Long appId;

    private String labelName;

    private String fieldName;

    private String columnName;

    private String modelName;

    private Long modelId;

    private String description;

    private FieldType fieldType;

    private String optionSetCode;

    private String relatedModel;

    private String relatedField;

    private String inverseLinkField;

    private Boolean autoBindMany;

    private Boolean autoExpandMany;

    private String cascadedField;

    // Used when the current field is a relational field
    private List<String> displayName;

    // Field level filters
    private String filters;

    private String defaultValue;

    // Memory compute attribute: Instantiated object of the default value.
    private Object defaultValueObject;

    private Integer length;

    private Integer scale;

    private Boolean required;

    private Boolean readonly;

    private Boolean hidden;

    private Boolean translatable;

    private Boolean copyable;

    private Boolean searchable;

    private Boolean computed;

    private String expression;

    // Memory compute attribute: The dependent field of the expression.
    private List<String> dependentFields;

    private boolean dynamic;

    private Boolean encrypted;

    private MaskingType maskingType;

}