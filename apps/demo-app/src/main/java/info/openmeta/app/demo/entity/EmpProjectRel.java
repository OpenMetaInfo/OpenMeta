package info.openmeta.app.demo.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * EmpProjectRel Model
 */
@Data
@Schema(name = "EmpProjectRel")
@EqualsAndHashCode(callSuper = true)
public class EmpProjectRel extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "员工ID")
    private Long empId;

    @Schema(description = "项目ID")
    private Long projectId;

    @Schema(description = "租户ID")
    private Long tenantId;

    @Schema(description = "Disabled")
    private Boolean disabled;
}