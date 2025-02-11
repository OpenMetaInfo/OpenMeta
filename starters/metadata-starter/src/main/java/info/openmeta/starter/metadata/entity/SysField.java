package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.enums.MaskingType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysField Model
 */
@Data
@Schema(name = "SysField")
@EqualsAndHashCode(callSuper = true)
public class SysField extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Label Name")
    private String labelName;

    @Schema(description = "Field Name")
    private String fieldName;

    @Schema(description = "Column Name")
    private String columnName;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Model ID")
    private Long modelId;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Field Type")
    private FieldType fieldType;

    @Schema(description = "Option Set Code")
    private String optionSetCode;

    @Schema(description = "Related Model")
    private String relatedModel;

    @Schema(description = "Middle Model")
    private String middleModel;

    @Schema(description = "Related Field")
    private String relatedField;

    @Schema(description = "Inverse Link Field")
    private String inverseLinkField;

    @Schema(description = "Cascaded Field")
    private String cascadedField;

    @Schema(description = "Filters")
    private String filters;

    @Schema(description = "Default Value")
    private String defaultValue;

    @Schema(description = "Length")
    private Integer length;

    @Schema(description = "Scale")
    private Integer scale;

    @Schema(description = "Is Required")
    private Boolean required;

    @Schema(description = "Is Readonly")
    private Boolean readonly;

    @Schema(description = "Hidden")
    private Boolean hidden;

    @Schema(description = "Translatable")
    private Boolean translatable;

    @Schema(description = "Noncopyable")
    private Boolean noncopyable;

    @Schema(description = "Unsearchable")
    private Boolean unsearchable;

    @Schema(description = "Is Computed")
    private Boolean computed;

    @Schema(description = "Expression")
    private String expression;

    @Schema(description = "Dynamic Field")
    private Boolean dynamic;

    @Schema(description = "Is Encrypted")
    private Boolean encrypted;

    @Schema(description = "Masking Type")
    private MaskingType maskingType;

    @Schema(description = "Disabled")
    private Boolean disabled;
}