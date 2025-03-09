package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * DesignOptionSet Model
 */
@Data
@Schema(name = "DesignOptionSet")
@EqualsAndHashCode(callSuper = true)
public class DesignOptionSet extends BaseModel {

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

    @Schema(description = "Option Set Name")
    private String name;

    @Schema(description = "Option Set Code")
    private String optionSetCode;

    @Schema(description = "Option Items")
    private List<DesignOptionItem> optionItems;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Deleted")
    private Boolean deleted;
}