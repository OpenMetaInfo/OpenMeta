package info.openmeta.app.demo.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * ProjectInfo Model
 */
@Data
@Schema(name = "ProjectInfo")
@EqualsAndHashCode(callSuper = true)
public class ProjectInfo extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Name")
    private String name;

    @Schema(description = "Code")
    private String code;

    @Schema(description = "Employees")
    private List<Long> empIds;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}