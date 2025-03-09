package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * DesignModelOnchange Model
 */
@Data
@Schema(name = "DesignModelOnchange")
@EqualsAndHashCode(callSuper = true)
public class DesignModelOnchange extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "External ID")
    private Long externalId;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Env ID")
    private Long envId;

    @Schema(description = "Name")
    private String name;

    @Schema(description = "Code")
    private String code;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Onchange Fields")
    private List<String> onchangeFields;

    @Schema(description = "Expression")
    private String expression;

    @Schema(description = "Updated Fields")
    private List<String> updatedFields;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Deleted")
    private Boolean deleted;
}