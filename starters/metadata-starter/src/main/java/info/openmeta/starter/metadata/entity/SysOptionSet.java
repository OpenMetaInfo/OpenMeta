package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * SysOptionSet Model
 */
@Data
@Schema(name = "SysOptionSet")
@EqualsAndHashCode(callSuper = true)
public class SysOptionSet extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Option Set Name")
    private String name;

    @Schema(description = "Option Set Code")
    private String optionSetCode;

    @Schema(description = "Option Items")
    private List<SysOptionItem> optionItems;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Deleted")
    private Boolean deleted;
}