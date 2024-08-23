package info.openmeta.app.demo.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * DeptInfo Model
 */
@Data
@Schema(name = "DeptInfo")
@EqualsAndHashCode(callSuper = true)
public class DeptInfo extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "部门名称")
    private String name;

    @Schema(description = "部门编码")
    private String code;

    @Schema(description = "部门员工")
    private List<EmpInfo> empIds;

    @Schema(description = "版本")
    private Integer version;

    @Schema(description = "描述")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}