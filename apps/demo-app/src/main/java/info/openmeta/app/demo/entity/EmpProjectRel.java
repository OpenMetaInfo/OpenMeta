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

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Employee ID")
    private Long empId;

    @Schema(description = "Project ID")
    private Long projectId;

    @Schema(description = "Tenant ID")
    private String tenantId;
}