package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysOptionItem Model
 */
@Data
@Schema(name = "SysOptionItem")
@EqualsAndHashCode(callSuper = true)
public class SysOptionItem extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Option Set ID")
    private Long optionSetId;

    @Schema(description = "Option Set Code")
    private String optionSetCode;

    @Schema(description = "Sequence")
    private Integer sequence;

    @Schema(description = "Item Code")
    private String itemCode;

    @Schema(description = "Item Name")
    private String itemName;

    @Schema(description = "Parent Item Code")
    private String parentItemCode;

    @Schema(description = "Item Color")
    private String itemColor;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}