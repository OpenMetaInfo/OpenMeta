package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * DesignModelValidation Model
 */
@Data
@Schema(name = "DesignModelValidation")
@EqualsAndHashCode(callSuper = true)
public class DesignModelValidation extends BaseModel {

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

    @Schema(description = "priority")
    private Integer priority;

    @Schema(description = "Expression")
    private String expression;

    @Schema(description = "Exception Message")
    private String exceptionMsg;

    @Schema(description = "Validated Fields")
    private List<String> validatedFields;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}