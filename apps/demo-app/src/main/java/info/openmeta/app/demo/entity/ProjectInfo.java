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

    @Schema(description = "项目名称")
    private String name;

    @Schema(description = "项目编码")
    private String code;

    @Schema(description = "项目员工")
    private List<Long> empIds;

    @Schema(description = "版本")
    private Integer version;

    @Schema(description = "描述")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}