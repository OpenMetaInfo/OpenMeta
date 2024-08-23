package info.openmeta.app.demo.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * EmpInfo Model
 */
@Data
@Schema(name = "EmpInfo")
@EqualsAndHashCode(callSuper = true)
public class EmpInfo extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "员工姓名")
    private String name;

    @Schema(description = "员工编号")
    private String code;

    @Schema(description = "员工邮箱")
    private String email;

    @Schema(description = "员工部门")
    private Long deptId;

    @Schema(description = "员工参与项目")
    private List<Long> projectIds;

    @Schema(description = "描述信息")
    private String description;

    @Schema(description = "租户ID")
    private Long tenantId;

    @Schema(description = "Disabled")
    private Boolean disabled;
}